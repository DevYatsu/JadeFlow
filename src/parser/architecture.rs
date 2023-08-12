use crate::token::Token;

use super::{
    class::Class,
    expression::Expression,
    functions::{errors::FunctionParsingError, Function, FunctionCall, MainFunctionData},
    types::type_from_expression,
    vars::{Declaration, Reassignment},
    ParsingError, TypeError,
};
use std::{collections::HashMap, fmt, iter::Peekable};

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
    variables: HashMap<String, Declaration>,
    functions: HashMap<String, Function>,
    registered_functions: HashMap<String, MainFunctionData>,
    classes: HashMap<String, Class>,
}
impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            variables: HashMap::new(),
            functions: HashMap::new(),
            registered_functions: HashMap::new(),
            classes: HashMap::new(),
        }
    }

    pub fn merge(first_table: &SymbolTable, second_table: SymbolTable) -> SymbolTable {
        let mut table = SymbolTable::new();

        table.variables.extend(first_table.variables.clone());
        table.functions.extend(first_table.functions.clone());
        table.variables.extend(second_table.variables);
        table.functions.extend(second_table.functions);

        table
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
        tokens: &mut Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<(), ParsingError> {
        if self.is_object_prop(&reassignement.name) {
            let parent_name = reassignement.name.rsplitn(2, '.').collect::<Vec<&str>>();
            let initial_var = self.get_variable(&parent_name[1], Some(tokens))?;

            match initial_var.value {
                Expression::DictionaryExpression(mut hash) => {
                    hash.insert(parent_name[0].to_owned(), reassignement.value.clone());

                    self.insert_variable(Declaration {
                        value: Expression::DictionaryExpression(hash),
                        ..initial_var
                    });
                    let var_type = type_from_expression(&reassignement.value, self, Some(tokens))?;
                    self.insert_variable(Declaration {
                        value: reassignement.value,
                        name: reassignement.name,
                        var_type,
                        is_mutable: true,
                        is_object_prop: true,
                    });
                }
                _ => {
                    return Err(ParsingError::InvalidDict {
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
            let initial_var = self.get_variable(&reassignement.name, Some(tokens))?;
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

    pub fn get_variable(
        &mut self,
        name: &str,
        tokens: Option<&mut Peekable<std::slice::Iter<'_, Token>>>,
    ) -> Result<Declaration, ParsingError> {
        let name_vec: Vec<&str> = name.split('.').collect();

        if name_vec.len() == 1 {
            let name_vec: Vec<&str> = name.split('[').collect();

            if name_vec.len() == 1 {
                return self
                    .variables
                    .get(name)
                    .map(|declaration| declaration.clone())
                    .ok_or_else(|| {
                        TypeError::CannotDetermineVarType {
                            name: name.to_owned(),
                        }
                        .into()
                    });
            }

            let mut var = self
                .variables
                .get(name_vec[0])
                .map(|declaration| declaration.value.clone())
                .ok_or_else(|| TypeError::CannotDetermineVarType {
                    name: name.to_owned(),
                })?;

            for (i, element) in name_vec.iter().skip(1).enumerate() {            
                println!("{:?}", var);
                match var {
                    Expression::ArrayExpression(vec) => {
                        let index = element.parse::<usize>()?;
                        if index > vec.len() - 1 {
                            return Err(TypeError::IndexOutOfRange {
                                vec_name: name_vec[0].to_owned(),
                                index,
                                length: vec.len(),
                            }
                            .into());
                        }

                        var = vec[index].to_owned();
                    }
                    expr => {
                        return Err(ParsingError::CannotIndexNotVector {
                            var_name: name_vec[0..i+1]
                                .iter()
                                .enumerate()
                                .map(|(i, val)| {
                                    if i != 0 {
                                        String::from(val.to_owned()) + "]"
                                    } else {
                                        val.to_owned().to_owned()
                                    }
                                })
                                .collect::<Vec<String>>()
                                .join("["),
                                actual_value: expr
                        })
                    }
                }
            }

            return Ok(Declaration {
                name: name.to_owned(),
                var_type: type_from_expression(&var, self, tokens)?,
                value: var,
                is_mutable: true,
                is_object_prop: false,
            });
        }

        let mut var = self
            .variables
            .get(name_vec[0])
            .map(|declaration| declaration.value.clone())
            .ok_or_else(|| TypeError::CannotDetermineVarType {
                name: name.to_owned(),
            })?
            .clone();

        for (i, prop) in name_vec.iter().skip(1).enumerate() {
            match var {
                Expression::DictionaryExpression(expr) => {
                    if let Some(e) = expr.get(*prop) {
                        match e {
                            Expression::Variable(name) => {
                                var = self.get_variable(&name, None)?.value;
                                continue;
                            }
                            _ => {
                                var = e.clone();
                                continue;
                            }
                        }
                    } else {
                        return Err(TypeError::CannotDetermineObjPropTypeNotDefined {
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

        let var_type = type_from_expression(&var, self, tokens)?;

        Ok(Declaration {
            name: name.to_owned(),
            var_type,
            value: var,
            is_mutable: true,
            is_object_prop: true,
        })
    }

    pub fn is_object_prop(&mut self, name: &str) -> bool {
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
    pub fn is_class_declared(&self, name: &str) -> bool {
        self.classes.get(name).is_some()
    }

    pub fn get_function(&mut self, name: &str) -> Result<MainFunctionData, ParsingError> {
        let func = self.registered_functions.get(name);

        if func.is_none() {
            return Err(FunctionParsingError::NotDefinedFunction {
                fn_name: name.to_owned(),
            }
            .into());
        }

        Ok(func
            .cloned()
            .ok_or_else(|| FunctionParsingError::NotDefinedFunction {
                fn_name: name.to_owned(),
            })?)
    }
}

impl fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "-- VARIABLES -- \n",)?;
        if self.variables.len() == 0 {
            write!(f, "None\n")?;
        } else {
            for var in &self.variables {
                write!(f, "{}\n", var.1.to_string())?;
            }
        }

        write!(f, "-- FUNCTIONS -- \n",)?;

        if self.functions.len() == 0 {
            {
                write!(f, "None\n")?;
            }
        } else {
            for var in &self.functions {
                write!(f, "- {}\n", var.1.to_string())?;
            }
        }

        write!(f, "-- Registered FUNCTIONS -- \n",)?;

        if self.registered_functions.len() == 0 {
            Ok({
                write!(f, "None\n")?;
            })
        } else {
            Ok(for var in &self.registered_functions {
                write!(f, "- {}\n", var.1.to_string())?;
            })
        }
    }
}
