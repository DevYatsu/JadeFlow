use std::{collections::HashMap, fmt};

use crate::{
    ignore_tokens_until,
    parser::vars::parse_var_reassignment,
    token::{Token, TokenType},
};

use super::{
    architecture::SymbolTable,
    errors::ParsingError,
    expression::Expression,
    functions::{errors::FunctionParsingError, parse_fn_args, Function, MainFunctionData},
    ignore_until_statement, ignore_whitespace,
    types::TypeError,
    vars::Declaration,
};

custom_error::custom_error! {pub ClassError
    FunctionError{source: FunctionParsingError} = "{source}",
    TypeError{source: TypeError} = "{source}",
    ExpectedUppercaseInName{name: String} = "Expected a class name starting with an uppercase: {name}",
    ExpectedNameAfterClass = "Expected a class name after 'class'",
    NameAlreadyTaken{name:String} = "The class '{name}' already exists",
    ExpectedParenAfterName{name: String} = "Expected an '(' after class name in class '{name}' declaration",
    ExpectedBrace{cls_name: String} = "Expected '{{' after \"{cls_name}\" arguments declaration",

    NoVarInClassContext{var_name: String} = "Cannot instanciate variables in the class context: at '{var_name}'",
    CannotReassignExternalVar{var_name: String} = "Cannot reassign global variable in the class context: at '{var_name}'",
    NoMethodInGlobalCtx{var_name: String} = "Cannot create methods in the class global context: at '{var_name}'",

    UnknownClassProp{prop: String} = "Use of an unknown class property '{prop}'"
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub arguments: Vec<Declaration>,
    pub global_properties: HashMap<String, Expression>,
    pub public_ctx: ClassCtx,
    pub private_ctx: ClassCtx,
}
impl Class {
    pub fn new() -> Class {
        Class {
            name: String::new(),
            arguments: Vec::new(),
            global_properties: HashMap::new(),
            public_ctx: ClassCtx::new(),
            private_ctx: ClassCtx::new(),
        }
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
impl MainClassCtx {
    pub fn from_class_ctx(ctx: &ClassCtx) -> MainClassCtx {
        MainClassCtx {
            methods: ctx
                .methods
                .iter()
                .map(|(name, fn_data)| (name.to_owned(), MainFunctionData::from_function(fn_data)))
                .collect(),
            properties: ctx.properties.clone(),
        }
    }
}

pub fn parse_class_declaration(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &mut SymbolTable,
) -> Result<Class, ParsingError> {
    let mut cls = Class::new();

    parse_class_header(tokens, symbol_table, &mut cls)?;
    if let Some(Token { token_type: TokenType::OpenBrace ,..}) = tokens.next() {
        parse_class_content(tokens, symbol_table, &mut cls)?;
    }else {
        return Err(ClassError::ExpectedBrace { cls_name: cls.name }.into())
    }

    println!("{:?}", cls);

    todo!()
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
            "class {} ({})",
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

        if name[0..0] != name[0..0].to_uppercase() {
            return Err(ClassError::ExpectedUppercaseInName {
                name: name.to_string(),
            });
        }

        if symbol_table.is_class_declared(name) {
            return Err(ClassError::NameAlreadyTaken {
                name: name.to_owned(),
            });
        }

        // Check if the next token is an open parenthesis
        if let Some(Token {
            token_type: TokenType::OpenParen,
            ..
        }) = tokens.next()
        {
            let arguments = parse_fn_args(tokens)?;

            cls.name.push_str(name);
            cls.arguments = arguments;

            return Ok(());
        } else {
            return Err(FunctionParsingError::ExpectedOpenParen {
                name: name.to_owned(),
            }
            .into());
        }
    } else {
        return Err(ClassError::ExpectedNameAfterClass);
    }
}

fn parse_class_content(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &mut SymbolTable,
    cls: &mut Class,
) -> Result<(), ParsingError> {
    while let Some(token) = tokens.peek() {
        match token.token_type {
            TokenType::ClassPublic => {
                // parse public content
            }
            TokenType::ClassPrivate => {
                // parse private content
            }
            TokenType::Identifier => {
                if token.value.starts_with("ctx.") {
                    let identifier_parts = &token.value.split('.').collect::<Vec<&str>>();
                    let prop_name = identifier_parts[1];
                    
                    let assignement =
                        parse_var_reassignment(tokens, None, symbol_table, prop_name)?;

                    if identifier_parts.len() == 2 {
                        cls.global_properties
                            .insert(assignement.name, assignement.value.clone());

                        continue;
                    }

                    // manage the case of objects
                    let prop = cls.global_properties.get(prop_name);
                    if prop.is_some() {
                        let prop = prop.unwrap();
                        match prop {
                            Expression::DictionaryExpression(d) => {
                                let mut new_hash = d.clone();
                                new_hash.insert(identifier_parts[2].to_owned(), assignement.value);
                                cls.global_properties.insert(
                                    prop_name.to_owned(),
                                    Expression::DictionaryExpression(new_hash),
                                );
                            }
                            _ => {
                                return Err(ParsingError::InvalidDict {
                                    name: prop_name.to_owned(),
                                })
                            }
                        }
                    } else {
                        return Err(ClassError::UnknownClassProp {
                            prop: prop_name.to_owned(),
                        }
                        .into());
                    }
                } else {
                    return Err(ClassError::CannotReassignExternalVar {
                        var_name: token.value.to_owned(),
                    }
                    .into());
                }
            }
            TokenType::Separator => {
                // dont do anything
            }
            TokenType::Var => {
                let mut err_content = String::from(&token.value);
                ignore_whitespace(tokens);
                if let Some(token) = tokens.next() {
                    if token.token_type == TokenType::Identifier {
                        err_content.push(' ');

                        err_content.push_str(&token.value);
                    }
                }
                return Err(ClassError::NoVarInClassContext {
                    var_name: err_content,
                }
                .into());
            }
            TokenType::Function => {
                let mut err_content = String::from(&token.value);
                ignore_whitespace(tokens);
                if let Some(token) = tokens.next() {
                    if token.token_type == TokenType::Identifier {
                        err_content.push(' ');
                        err_content.push_str(&token.value);
                    }
                }
                return Err(ClassError::NoMethodInGlobalCtx {
                    var_name: err_content,
                }
                .into());
            }
            TokenType::CloseBrace => {
                break;
            }
            TokenType::Class => {
                return Err(ParsingError::Custom {
                    data: "Cannot instanciate a class in the context of another class!".to_owned(),
                })
            }
            TokenType::Return => {
                return Err(ParsingError::Custom {
                    data: "Invalid return statement! Cannot return a value in the class context"
                        .to_owned(),
                })
            }
            _ => {
                println!("{:?}",token.value);
                ignore_tokens_until!(TokenType::Return, TokenType::Class, TokenType::CloseBrace, TokenType::Identifier in tokens);
            }
        }
        tokens.next();
    }
    println!("{:?}", tokens);

    Ok(())
}
