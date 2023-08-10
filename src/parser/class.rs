use std::{collections::HashMap, fmt};

use crate::token::{Token, TokenType};

use super::{
    architecture::{SymbolTable},
    functions::{errors::FunctionParsingError, parse_fn_args, Function, MainFunctionData},
    ignore_whitespace,
    vars::Declaration,
    ParsingError, expression::Expression,
};

custom_error::custom_error! {pub ClassError
    FunctionError{source: FunctionParsingError} = "{source}",
    ExpectedUppercaseInName{name: String} = "Expected a class name starting with an uppercase: {name}",
    ExpectedNameAfterClass = "Expected a class name after 'class'",
    NameAlreadyTaken{name:String} = "The class '{name}' already exists",
    ExpectedParenAfterName{name: String} = "Expected an open parenthesis after class name in class '{name}' declaration",
}

#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub arguments: Vec<Declaration>,
    pub global_properties: HashMap<String, Expression>,
    pub public_ctx: ClassCtx,
    pub private_ctx: ClassCtx,
}
#[derive(Debug, Clone)]
pub struct ClassCtx {
    pub methods: HashMap<String, Function>,
    pub properties: HashMap<String, Expression>,
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

#[derive(Debug, Clone)]
pub struct MainClassData {
    pub name: String,
    pub arguments: Vec<Declaration>,
    pub global_properties: HashMap<String, Expression>,
    pub public_ctx: MainClassCtx,
    pub private_ctx: MainClassCtx,
}
impl MainClassData {
    pub fn from_class(cls: &Class) -> MainClassData {
        MainClassData {
            name: cls.name.to_owned(),
            arguments: cls.arguments.clone(),
            global_properties: cls.global_properties.clone(),
            public_ctx: MainClassCtx::from_class_ctx(&cls.public_ctx),
            private_ctx: MainClassCtx::from_class_ctx(&cls.private_ctx),
        }
    }
}
impl fmt::Display for MainClassData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "class {} ({}): \n {}",
            self.name,
            self.arguments
                .iter()
                .map(|arg| format!("{}: {}", arg.name, arg.var_type.as_assignment()))
                .collect::<Vec<String>>()
                .join(", "),
            self,
        )
    }
}


pub fn parse_class_declaration(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &mut SymbolTable,
) -> Result<Class, ParsingError> {
    todo!()
}

pub fn parse_main_class_data(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &mut SymbolTable,
) -> Result<MainClassData, ClassError> {
    let header = parse_class_header(tokens, symbol_table)?;

    todo!()
}

struct ClassHeader {
    pub name: String,
    pub arguments: Vec<Declaration>,
}

fn parse_class_header(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &mut SymbolTable,
) -> Result<ClassHeader, ClassError> {
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

        if symbol_table.is_cls_declared(name) {
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

            ignore_whitespace(tokens);

            let cls_header = ClassHeader {
                name: name.to_owned(),
                arguments,
            };

            return Ok(cls_header);
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
