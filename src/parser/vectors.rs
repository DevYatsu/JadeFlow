use super::{
    architecture::{Declaration, Expression, SymbolTable},
    expression::parse_expression,
    ParsingError,
};
use crate::token::{Token, TokenType};

pub fn parse_array_expression(
    tokens: &mut std::slice::Iter<'_, Token>,
    symbol_table: &SymbolTable,
) -> Result<Expression, ParsingError> {
    let mut vec_expressions = Vec::new();

    while let Some(token) = tokens.next() {
        match &token.token_type {
            TokenType::CloseBracket => {
                break;
            }
            TokenType::Comma => {
                continue;
            }
            _ => {
                let value = parse_expression(tokens, symbol_table)?;
                check_and_insert_expression(&value, symbol_table, &mut vec_expressions)?;
            }
        }
    }

    Ok(Expression::ArrayExpression(vec_expressions))
}

fn check_and_insert_expression(
    expression: &Expression,
    symbol_table: &SymbolTable,
    vec_expressions: &mut Vec<Expression>,
) -> Result<(), ParsingError> {
    match expression {
        Expression::Variable(name) => {
            symbol_table.get_variable(name)?; // error if var not defined
        }
        _ => (),
    }
    vec_expressions.push(expression.clone());
    Ok(())
}

pub fn parse_array_indexing(
    tokens: &mut std::slice::Iter<'_, Token>,
    var: Declaration,
) -> Result<Expression, ParsingError> {
    let next = tokens.next();

    if let Some(Token {
        token_type: TokenType::Number,
        value,
    }) = next
    {
        let index: usize = value.parse()?;

        if let Some(token) = tokens.next() {
            if token.token_type == TokenType::CloseBracket {
                return Ok(Expression::Variable(format!("{}[{}", var.name, index)));
            } else {
                return Err(ParsingError::ExpectedBracketAfterVectorIndex {
                    found: token.value.to_owned(),
                });
            }
        } else {
            return Err(ParsingError::UnexpectedEndOfInput);
        }
    } else if let Some(Token { value, .. }) = next {
        return Err(ParsingError::ExpectedValidVectorIndex {
            found: value.to_owned(),
        });
    } else {
        return Err(ParsingError::UnexpectedEndOfInput);
    }
}
