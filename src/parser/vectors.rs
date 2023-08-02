use super::{
    architecture::{Expression, SymbolTable},
    expression::parse_expression,
    ParsingError,
};
use crate::token::{Token, TokenType};

pub fn parse_array_expression(
    tokens: &[Token],
    position: &mut usize,
    symbol_table: &SymbolTable,
) -> Result<Expression, ParsingError> {
    let mut vec_expressions = Vec::new();
    *position += 1;

    while let Some(token) = tokens.get(*position) {
        match &token.token_type {
            TokenType::CloseBracket => {
                *position += 1;
                break;
            }
            TokenType::Comma => {
                *position += 1;
            }
            _ => {
                let value = parse_expression(tokens, position, symbol_table)?;
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
