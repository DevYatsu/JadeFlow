use crate::token::{Token, TokenType};

use super::{
    architecture::{Class, MainClassData, SymbolTable},
    functions::{errors::FunctionParsingError, parse_fn_args},
    ignore_whitespace,
    vars::Declaration,
    ParsingError,
};

custom_error::custom_error! {pub ClassError
    FunctionError{source: FunctionParsingError} = "{source}",
    ExpectedUppercaseInName{name: String} = "Expected a class name starting with an uppercase: {name}",
    ExpectedNameAfterClass = "Expected a class name after 'class'",
    NameAlreadyTaken{name:String} = "The class '{name}' already exists",
    ExpectedParenAfterName{name: String} = "Expected an open parenthesis after class name in class '{name}' declaration",
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
