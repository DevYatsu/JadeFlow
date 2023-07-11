use crate::errors::{find_line, SyntaxError, TokenisationError};

#[derive(Debug)]
pub enum TokenType {
    Identifier,
    AssignmentOperator,
    Int,
    Float,
    String,
    Boolean,
    Null,
    Undefined,
    Object,
    Function,
    Array,
    Class,
    OpenParen,
    CloseParen,
    BinaryOperator,
}

#[derive(Debug)]
pub struct Token {
    value: String,
    token_type: TokenType,
}

fn token(value: &str, token_type: TokenType) -> Token {
    Token {
        value: value.to_string(),
        token_type,
    }
}

pub fn tokenize(source_code: &str) -> Result<Vec<Token>, String> {
    let mut tokens: Vec<Token> = vec![];
    let mut src: Vec<&str> = formatted_src.split(" ").filter(|x| x != &"").collect();


    Ok(tokens)
}
