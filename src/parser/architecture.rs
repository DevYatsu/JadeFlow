use crate::{
    evaluation::EvaluationError,
    jadeflow_std::{load_std, StandardFunction},
};

use super::{
    class::Class,
    expression::Expression,
    functions::{errors::FunctionParsingError, Function, FunctionCall, MainFunctionData},
    types::{type_from_expression, TypeError, VariableType},
    vars::{Declaration, Reassignment},
};
use std::{collections::HashMap, fmt, num::ParseIntError};

#[derive(Debug, Clone)]
pub enum ASTNode {
    Program(Program),
    VariableDeclaration(Declaration),
    VariableReassignment(Reassignment),

    FunctionDeclaration(Function),
    // corresponds to both {} and => functions
    ClassDeclaration(Class),

    Return(Expression),
    FunctionCall(FunctionCall),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub symbol_table: SymbolTable,
}

pub fn program(
    Program {
        statements,
        symbol_table,
    }: Program,
) -> ASTNode {
    ASTNode::Program(Program {
        statements,
        symbol_table,
    })
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub node: ASTNode,
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    //struct to keep track of variables, fns and everything created
    pub variables: HashMap<String, Declaration>,
    pub functions: HashMap<String, Function>,
    pub registered_functions: HashMap<String, MainFunctionData>,
    pub classes: HashMap<String, Class>,
    pub std_functions: HashMap<String, StandardFunction>,
}
#[macro_export]
macro_rules! merge_symbol_tables {
    ( $($hashmap:expr),* $(,)? ) => {
        {
            let mut symbol_table = SymbolTable {
                variables: hashmap!{},
                functions: hashmap!{},
                registered_functions: hashmap!{},
                classes: hashmap!{},
            };

            $(
                symbol_table.variables.extend($hashmap.variables);
                symbol_table.functions.extend($hashmap.functions);
                symbol_table.registered_functions.extend($hashmap.registered_functions);
                symbol_table.classes.extend($hashmap.classes);
            )*

            symbol_table
        }
    };
}

custom_error::custom_error! {pub SymbolTableError
    TypeError{source: TypeError} = "{source}",
    ParseIntError{source: ParseIntError} = "{source}",
    EvaluationError{source: EvaluationError} = "{source}",

    InvalidDict{name: String} = "'{name}' is not a valid object",
    CannotIndexNotVector{indexed_expr: Expression, actual_type: VariableType} = "Cannot index '{indexed_expr}' as it is not a vector, it's of type '{actual_type}'",
    IndexOutOfRange{vec_name: String, index: usize, length: usize} = "Index out of range! Cannot index \"{vec_name}\" at index {index} when length is {length}",
    CannotIndexNegatively{vec_name: String, index: isize} = "Index out of range! Cannot index \"{vec_name}\" at index {index}",
    CannotDetermineObjPropTypeNotDefined{obj_name: String, prop: String} = "Cannot determine type of '{prop}' property on \"{obj_name}\" as it is not defined",
    CannotDetermineVarType{name: String} = "Cannot determine \"{name}\" type as it is not defined",
    InvalidTypeIndex{found_type: VariableType, full_expr: Expression} = "Index must be of type 'Number' while found type is '{found_type}' at '{full_expr}'"
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            variables: HashMap::new(),
            functions: HashMap::new(),
            registered_functions: HashMap::new(),
            classes: HashMap::new(),
            std_functions: HashMap::new(),
        }
    }
    pub fn table_init() -> SymbolTable {
        let std_functions = load_std();
        SymbolTable {
            variables: HashMap::new(),
            functions: HashMap::new(),
            registered_functions: HashMap::new(),
            classes: HashMap::new(),
            std_functions,
        }
    }

    pub fn insert_variable(&mut self, declaration: Declaration) {
        self.variables.insert(declaration.name.clone(), declaration);
    }
    pub fn insert_function(&mut self, f: &Function) {
        self.functions.insert(f.name.to_owned(), f.clone());
    }
    pub fn register_function(&mut self, f: MainFunctionData) {
        self.registered_functions.insert(f.name.to_owned(), f);
    }

    pub fn reassign_variable(
        &mut self,
        reassignement: Reassignment,
    ) -> Result<(), SymbolTableError> {
        if self.is_object_prop(&reassignement.name) {
            let parent_name = reassignement.name.rsplitn(2, '.').collect::<Vec<&str>>();
            let initial_var = self.get_variable(&parent_name[1])?;

            match initial_var.value {
                Expression::DictionaryExpression(mut hash) => {
                    hash.insert(parent_name[0].to_owned(), reassignement.value.clone());

                    self.insert_variable(Declaration {
                        value: Expression::DictionaryExpression(hash),
                        ..initial_var
                    });
                    let var_type = type_from_expression(&reassignement.value, self)?;
                    self.insert_variable(Declaration {
                        value: reassignement.value,
                        name: reassignement.name,
                        var_type,
                        is_mutable: true,
                        is_object_prop: true,
                    });
                }
                _ => {
                    return Err(SymbolTableError::InvalidDict {
                        name: parent_name[1].to_owned(),
                    })
                }
            }

            // let dec = Declaration {
            //     name: reassignement.name.clone(),
            //     var_type: type_from_expression(&reassignement.value, self, Some(tokens))?,
            //     value: reassignement.value,
            //     is_mutable: true,
            //     is_object_prop: true,
            // };
            // self.variables.insert(reassignement.name, dec);
        } else {
            let initial_var = self.get_variable(&reassignement.name)?;
            if reassignement.value != initial_var.value {
                self.variables.insert(
                    reassignement.name.clone(),
                    Declaration {
                        name: initial_var.name,
                        var_type: initial_var.var_type,
                        value: reassignement.value,
                        is_mutable: true,
                        is_object_prop: initial_var.is_object_prop,
                    },
                );
            }
        }
        Ok(())
    }

    pub fn get_variable(&self, name: &str) -> Result<Declaration, SymbolTableError> {
        let name_vec: Vec<&str> = name.split('.').collect();

        if name_vec.len() == 1 {
            let name_vec: Vec<&str> = name.split('[').map(|s| s.trim_matches(']')).collect();

            if name_vec.len() == 1 {
                return self
                    .variables
                    .get(name)
                    .map(|declaration| declaration.clone())
                    .ok_or_else(|| {
                        SymbolTableError::CannotDetermineVarType {
                            name: name.to_owned(),
                        }
                        .into()
                    });
            }

            // let mut var = self
            //     .variables
            //     .get(name_vec[0])
            //     .map(|declaration| declaration.value.clone())
            //     .ok_or_else(|| SymbolTableError::CannotDetermineVarType {
            //         name: name.to_owned(),
            //     })?;

            // for (i, element) in name_vec.iter().skip(1).enumerate() {
            //     match var {
            //         Expression::ArrayExpression(vec) => {
            //             let index = element.parse::<usize>()?;
            //             if index > vec.len() - 1 {
            //                 return Err(SymbolTableError::IndexOutOfRange {
            //                     vec_name: name_vec[0..i + 1]
            //                         .iter()
            //                         .enumerate()
            //                         .map(|(i, val)| {
            //                             if i != 0 {
            //                                 String::from(val.to_owned()) + "]"
            //                             } else {
            //                                 val.to_owned().to_owned()
            //                             }
            //                         })
            //                         .collect::<Vec<String>>()
            //                         .join("["),
            //                     index,
            //                     length: vec.len(),
            //                 }
            //                 .into());
            //             }

            //             var = vec[index].to_owned();
            //         }
            //         expr => {
            //             return Err(SymbolTableError::CannotIndexNotVector {
            //                 var_name: name_vec[0..i + 1]
            //                     .iter()
            //                     .enumerate()
            //                     .map(|(i, val)| {
            //                         if i != 0 {
            //                             String::from(val.to_owned()) + "]"
            //                         } else {
            //                             val.to_owned().to_owned()
            //                         }
            //                     })
            //                     .collect::<Vec<String>>()
            //                     .join("["),
            //                 actual_value: expr,
            //             })
            //         }
            //     }
            // }

            // return Ok(Declaration {
            //     name: name.to_owned(),
            //     var_type: type_from_expression(&var, self)?,
            //     value: var,
            //     is_mutable: true,
            //     is_object_prop: false,
            // });
        }

        let mut var = self
            .variables
            .get(name_vec[0])
            .map(|declaration| declaration.value.clone())
            .ok_or_else(|| SymbolTableError::CannotDetermineVarType {
                name: name.to_owned(),
            })?
            .clone();

        for (i, prop) in name_vec.iter().skip(1).enumerate() {
            match var {
                Expression::DictionaryExpression(expr) => {
                    if let Some(e) = expr.get(*prop) {
                        match e {
                            Expression::Variable(name) => {
                                var = self.get_variable(&name)?.value;
                                continue;
                            }
                            _ => {
                                var = e.clone();
                                continue;
                            }
                        }
                    } else {
                        return Err(SymbolTableError::CannotDetermineObjPropTypeNotDefined {
                            obj_name: name_vec[0..=i].join("."),
                            prop: name_vec[0..=i + 1].join("."),
                        }
                        .into());
                    }
                }
                expr => {
                    var = expr.clone();
                    continue;
                }
            }
        }

        let var_type = type_from_expression(&var, self)?;

        Ok(Declaration {
            name: name.to_owned(),
            var_type,
            value: var,
            is_mutable: true,
            is_object_prop: true,
        })
    }

    pub fn is_object_prop(&self, name: &str) -> bool {
        let name_vec = name.split('.').collect::<Vec<&str>>();

        if name_vec.len() == 1 {
            false
        } else {
            true
        }
    }

    pub fn is_fn_declared(&self, name: &str) -> bool {
        self.functions.get(name).is_some()
    }
    pub fn is_fn_std(&self, name: &str) -> bool {
        self.std_functions.get(name).is_some()
    }
    pub fn is_class_declared(&self, name: &str) -> bool {
        self.classes.get(name).is_some()
    }

    pub fn get_function(&self, name: &str) -> Result<MainFunctionData, FunctionParsingError> {
        let func = match self.registered_functions.get(name) {
            Some(v) => v.clone(),
            None => {
                let func = self.std_functions.get(name);
                if func.is_none() {
                    return Err(FunctionParsingError::NotDefinedFunction {
                        fn_name: name.to_owned(),
                    });
                }
                MainFunctionData::from(func.unwrap().clone())
            }
        };

        Ok(func)
    }

    pub fn get_full_function(&self, name: &str) -> Result<Function, FunctionParsingError> {
        let func = self.functions.get(name);

        if func.is_none() {
            return Err(FunctionParsingError::NotDefinedFunction {
                fn_name: name.to_owned(),
            });
        }

        Ok(func
            .cloned()
            .ok_or_else(|| FunctionParsingError::NotDefinedFunction {
                fn_name: name.to_owned(),
            })?)
    }
    pub fn get_std_function(&self, name: &str) -> Result<&StandardFunction, FunctionParsingError> {
        let func = self.std_functions.get(name);

        if func.is_none() {
            return Err(FunctionParsingError::NotDefinedFunctionInStd {
                fn_name: name.to_owned(),
            });
        }

        Ok(func.unwrap())
    }
}

impl fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "-- VARIABLES -- \n")?;
        if self.variables.is_empty() {
            write!(f, "None\n")?;
        } else {
            for var in &self.variables {
                write!(f, "{}\n", var.1.to_string())?;
            }
        }

        write!(f, "-- FUNCTIONS -- \n")?;
        if self.functions.is_empty() {
            write!(f, "None\n")?;
        } else {
            for var in &self.functions {
                write!(f, "- {}\n", var.1.to_string())?;
            }
        }

        write!(f, "-- Registered FUNCTIONS -- \n")?;
        if self.registered_functions.is_empty() {
            write!(f, "None\n")?;
        } else {
            for var in &self.registered_functions {
                write!(f, "- {}\n", var.1.to_string())?;
            }
        }

        write!(f, "-- CLASSES -- \n")?;
        if self.classes.is_empty() {
            write!(f, "None\n")?;
        } else {
            for var in &self.classes {
                write!(f, "- {}\n", var.1.to_string())?;
            }
        }

        write!(f, "-- STD FNs -- \n")?;
        if self.std_functions.is_empty() {
            write!(f, "None\n")?;
        } else {
            for var in &self.std_functions {
                write!(f, "- {}\n", var.1.to_string())?;
            }
        }

        Ok(())
    }
}
