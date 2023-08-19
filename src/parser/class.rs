use hashbrown::HashMap;
use std::fmt;

use crate::{
    parser::expression::parse_expression,
    print_warning,
    token::{Token, TokenType},
};

use super::{
    architecture::{ASTNode, Statement, SymbolTable},
    errors::ParsingError,
    expression::Expression,
    functions::{errors::FunctionParsingError, parse_fn_declaration, Function, MainFunctionData},
    ignore_until_statement, ignore_whitespace,
    types::TypeError,
    vars::Declaration,
};

custom_error::custom_error! {pub ClassError
    FunctionError{source: FunctionParsingError} = "{source}",
    TypeError{source: TypeError} = "{source}",
    Custom{msg: String} = "{msg}",

    ExpectedUppercaseInName{name: String} = "Expected a name starting with an uppercase for '{name}' class",
    ExpectedNameAfterClass = "Expected a class name after 'class'",
    NameAlreadyTaken{name:String} = "The class '{name}' already exists",
    ExpectedParenAfterName{name: String} = "Expected an '(' after class name in class '{name}' declaration",
    ExpectedBrace{cls_name: String} = "Expected '{{' after \"{cls_name}\" arguments declaration",

    NoVarInClassContext{var_name: String} = "Cannot instanciate variables in the class context: at '{var_name}'",
    CannotReassignExternalVar{var_name: String} = "Cannot reassign global variable in the class context: at '{var_name}'",
    NoMethodInGlobalCtxExceptInit{fn_name: String, class_name: String} = "Cannot create methods in the class global context except 'init': at '{fn_name}' in '{class_name}' declaration",
    NoIdInGlobalContext{identifier: String, class_name: String} = "No variable assignment in class global context: at '{identifier}' in '{class_name}' declaration",

    CannotReassignCtx{class_name: String} = "Cannot reassign '{class_name}' class 'ctx'",

    ClassMissingAnIniter{class_name: String} = "Class '{class_name}' is missing an 'init' method !",
    UnknownClassProp{prop: String} = "Use of an unknown class property '{prop}'"
}
pub fn cls(cls: Class) -> Statement {
    Statement {
        node: ASTNode::ClassDeclaration(cls),
    }
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub initer: Option<Function>,
    pub public_ctx: ClassCtx,
    pub private_ctx: ClassCtx,
}
impl Class {
    pub fn new() -> Class {
        Class {
            name: String::new(),
            public_ctx: ClassCtx::new(),
            private_ctx: ClassCtx::new(),
            initer: None,
        }
    }

    pub fn set_pub_prop(&mut self, prop: &str, value: Expression) -> Result<(), ClassError> {
        if prop == "ctx" {
            return Err(ClassError::CannotReassignCtx {
                class_name: self.name.to_owned(),
            });
        }
        let identifier_parts = prop.split('.').collect::<Vec<&str>>();

        if identifier_parts[0] != "ctx" {
            return Err(ClassError::CannotReassignExternalVar {
                var_name: prop.to_owned(),
            }
            .into());
        }
        let last_prop = identifier_parts.last().unwrap();

        let mut current_prop =
            &mut Expression::DictionaryExpression(self.public_ctx.properties.clone());
        for part in identifier_parts.iter().take(identifier_parts.len() - 1) {
            match current_prop {
                Expression::DictionaryExpression(d) => {
                    current_prop = d.entry(part.to_string()).or_insert(Expression::Null);
                }
                _ => {
                    return Err(ClassError::Custom {
                        msg: format!("Cannot access {} as it is not a dictionary!", part),
                    }
                    .into());
                }
            }
        }

        if let Expression::DictionaryExpression(d) = current_prop {
            d.insert(last_prop.to_string(), value);
        } else {
            return Err(ClassError::Custom {
                msg: format!(
                    "Cannot access {} as it is not a dictionary!",
                    identifier_parts.join(".")
                ),
            }
            .into());
        }

        Ok(())
    }

    pub fn set_priv_prop(&mut self, prop: &str, value: Expression) -> Result<(), ClassError> {
        if prop == "ctx" {
            return Err(ClassError::CannotReassignCtx {
                class_name: self.name.to_owned(),
            });
        }
        let identifier_parts = prop.split('.').collect::<Vec<&str>>();

        if identifier_parts[0] != "ctx" {
            return Err(ClassError::CannotReassignExternalVar {
                var_name: prop.to_owned(),
            }
            .into());
        }
        let last_prop = identifier_parts.last().unwrap();

        let mut current_prop =
            &mut Expression::DictionaryExpression(self.private_ctx.properties.clone());
        for part in identifier_parts.iter().take(identifier_parts.len() - 1) {
            match current_prop {
                Expression::DictionaryExpression(d) => {
                    current_prop = d.entry(part.to_string()).or_insert(Expression::Null);
                }
                _ => {
                    return Err(ClassError::Custom {
                        msg: format!("Cannot access {} as it is not a dictionary!", part),
                    }
                    .into());
                }
            }
        }

        if let Expression::DictionaryExpression(d) = current_prop {
            d.insert(last_prop.to_string(), value);
        } else {
            return Err(ClassError::Custom {
                msg: format!(
                    "Cannot access {} as it is not a dictionary!",
                    identifier_parts.join(".")
                ),
            }
            .into());
        }

        Ok(())
    }

    pub fn get_pub_prop(&self, prop: &str) -> Result<Expression, ClassError> {
        if prop == "ctx" {
            return Ok(Expression::DictionaryExpression(
                self.public_ctx.properties.clone(),
            ));
        }
        let mut identifier_parts = prop.split('.').collect::<Vec<&str>>();

        if identifier_parts[0] != "ctx" {
            return Err(ClassError::CannotReassignExternalVar {
                var_name: prop.to_owned(),
            }
            .into());
        }

        let mut ctx = self.get_pub_prop(identifier_parts.remove(0))?;

        while identifier_parts.len() > 0 {
            match ctx {
                Expression::DictionaryExpression(dict) => {
                    ctx = dict
                        .get(identifier_parts.remove(0))
                        .unwrap_or_else(|| &Expression::Null)
                        .to_owned();
                }
                _ => {
                    let whole_name = prop.split('.').collect::<Vec<&str>>();
                    return Err(ClassError::Custom {
                        msg: format!(
                            "Cannot access {} as it is not a dictionary!",
                            whole_name[0..whole_name.len() - identifier_parts.len()].join(".")
                        ),
                    });
                }
            }
        }

        Ok(ctx)
    }

    pub fn get_priv_prop(&self, prop: &str) -> Result<Expression, ClassError> {
        if prop == "ctx" {
            return Ok(Expression::DictionaryExpression(
                self.private_ctx.properties.clone(),
            ));
        }

        let mut identifier_parts = prop.split('.').collect::<Vec<&str>>();

        if identifier_parts[0] != "ctx" {
            return Err(ClassError::CannotReassignExternalVar {
                var_name: prop.to_owned(),
            }
            .into());
        }

        let index_0 = identifier_parts.remove(0);
        let mut ctx = self.get_pub_prop(index_0)?;
        let priv_ctx = match self.get_priv_prop(index_0)? {
            Expression::DictionaryExpression(d) => d,
            _ => unreachable!(),
        };
        ctx = match ctx {
            Expression::DictionaryExpression(mut d) => {
                d.extend(priv_ctx);
                Expression::DictionaryExpression(d)
            }
            _ => unreachable!(),
        };

        while identifier_parts.len() > 0 {
            match ctx {
                Expression::DictionaryExpression(dict) => {
                    ctx = dict
                        .get(identifier_parts.remove(0))
                        .unwrap_or_else(|| &Expression::Null)
                        .to_owned();
                }
                _ => {
                    let whole_name = prop.split('.').collect::<Vec<&str>>();
                    return Err(ClassError::Custom {
                        msg: format!(
                            "Cannot access {} as it is not a dictionary!",
                            whole_name[0..whole_name.len() - identifier_parts.len()].join(".")
                        ),
                    });
                }
            }
        }

        Ok(ctx)
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "class {}({})",
            self.name,
            match self.initer.clone().unwrap() {
                Function::StandardFunction { .. } => unreachable!(),
                Function::DefinedFunction { arguments, .. } => {
                    arguments
                        .iter()
                        .map(|arg| format!("{}: {}", arg.name, arg.var_type.as_assignment()))
                        .collect::<Vec<String>>()
                        .join(", ")
                }
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct ClassCtx {
    pub methods: HashMap<String, Function>,
    pub properties: HashMap<String, Expression>,
}
impl ClassCtx {
    pub fn new() -> ClassCtx {
        ClassCtx {
            methods: HashMap::new(),
            properties: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MainClassCtx {
    pub methods: HashMap<String, MainFunctionData>,
    pub properties: HashMap<String, Expression>,
}
impl From<ClassCtx> for MainClassCtx {
    fn from(value: ClassCtx) -> Self {
        MainClassCtx {
            methods: value
                .methods
                .iter()
                .map(|(name, fn_data)| (name.to_owned(), MainFunctionData::from(fn_data.clone())))
                .collect(),
            properties: value.properties,
        }
    }
}

pub fn parse_class_declaration(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &mut SymbolTable,
) -> Result<Class, ParsingError> {
    let mut cls = Class::new();

    parse_class_header(tokens, symbol_table, &mut cls)?;

    parse_class_content(tokens, symbol_table, &mut cls)?;

    println!("{}", cls);
    println!("{:?}", cls.initer);

    Ok(cls)
}

#[derive(Debug, Clone)]
pub struct ClassHeader {
    pub name: String,
    pub arguments: Vec<Declaration>,
}
impl fmt::Display for ClassHeader {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "class {}({})",
            self.name,
            self.arguments
                .iter()
                .map(|arg| format!("{}: {}", arg.name, arg.var_type.as_assignment()))
                .collect::<Vec<String>>()
                .join(", "),
        )
    }
}

pub fn parse_class_header(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &mut SymbolTable,
    cls: &mut Class,
) -> Result<(), ClassError> {
    // Expect an identifier token (function name)
    if let Some(Token {
        token_type: TokenType::Identifier,
        value,
    }) = tokens.next()
    {
        let name = value;

        if !name.chars().next().unwrap().is_uppercase() {
            print_warning!(ClassError::ExpectedUppercaseInName {
                name: name.to_owned()
            }
            .to_string());
        }

        if symbol_table.is_class_declared(name) {
            return Err(ClassError::NameAlreadyTaken {
                name: name.to_owned(),
            });
        }

        cls.name = name.to_owned();

        return Ok(());
    } else {
        return Err(ClassError::ExpectedNameAfterClass);
    }
}

fn parse_class_content(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &mut SymbolTable,
    cls: &mut Class,
) -> Result<(), ParsingError> {
    while let Some(token) = tokens.next() {
        match token.token_type {
            TokenType::ClassPublic => {
                // parse public content
            }
            TokenType::ClassPrivate => {
                // parse private content
            }
            TokenType::Identifier => {
                return Err(ClassError::NoIdInGlobalContext {
                    identifier: token.value.to_owned(),
                    class_name: cls.name.to_owned(),
                }
                .into());
            }
            TokenType::Separator => {}
            TokenType::Var => {
                let mut err_content = token.value.to_owned();
                ignore_whitespace(tokens);

                if let Some(token) = tokens.next() {
                    if token.token_type == TokenType::Identifier {
                        err_content.push(' ');

                        err_content.push_str(&token.value);
                    }
                };
                return Err(ClassError::NoVarInClassContext {
                    var_name: err_content,
                }
                .into());
            }
            TokenType::Function => {
                let ctx = cls.get_pub_prop("ctx")?;
                let fn_data = parse_fn_declaration(tokens, symbol_table, Some(ctx))?;

                match &fn_data {
                    Function::DefinedFunction { name, .. } => {
                        if name != "init" {
                            return Err(ClassError::NoMethodInGlobalCtxExceptInit {
                                fn_name: name.to_owned(),
                                class_name: cls.name.to_owned(),
                            }
                            .into());
                        }
                    }
                    Function::StandardFunction { .. } => unreachable!(),
                };

                cls.initer = Some(fn_data);
            }
            TokenType::CloseBrace => {
                break;
            }
            _ => {
                ignore_until_statement(tokens)?;
            }
        }
    }

    if cls.initer.is_none() {
        return Err(ClassError::ClassMissingAnIniter {
            class_name: cls.name.to_owned(),
        }
        .into());
    }

    Ok(())
}

fn parse_ctx_assignment(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    prop_name: &str,
    symbol_table: &mut SymbolTable,
    cls: &mut Class,
    is_pub: bool,
) -> Result<(), ParsingError> {
    let prop = if !is_pub {
        cls.get_priv_prop(&prop_name)?
    } else {
        cls.get_pub_prop(&prop_name)?
    };
    let is_prop_null = match prop {
        Expression::Null => true,
        _ => false,
    };

    if let Some(Token {
        token_type: TokenType::AssignmentOperator,
        value: operator,
    }) = tokens.next()
    {
        if is_prop_null && operator != "=" {
            return Err(ClassError::Custom {
                msg: format!(
                    "Cannot reassign with '{}' operator '{}' as it was not assigned yet",
                    operator, prop_name
                ),
            }
            .into());
        }
        symbol_table.variables.insert(
            "ctx".to_owned(),
            Declaration {
                name: "ctx".to_owned(),
                var_type: crate::parser::types::VariableType::Dictionary,
                value: if is_pub {
                    cls.get_pub_prop("ctx")?
                } else {
                    cls.get_priv_prop("ctx")?
                },
                is_mutable: false,
                is_object_prop: false,
            },
        );

        let expr = parse_expression(tokens, symbol_table)?;
        symbol_table.variables.remove("ctx");

        Ok(if is_pub {
            cls.set_pub_prop(prop_name, expr)?
        } else {
            cls.set_priv_prop(prop_name, expr)?
        })
    } else {
        return Err(ParsingError::ExpectedReassignment {
            var_name: prop_name.to_owned(),
        });
    }
}
