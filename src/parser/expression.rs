use super::{
    architecture::{BinaryOperator, Expression, SymbolTable},
    dictionary::parse_dictionary_expression,
    vectors::parse_array_expression,
    ParsingError,
};
use crate::token::{Token, TokenType};

pub fn parse_expression(
    tokens: &[Token],
    position: &mut usize,
    symbol_table: &SymbolTable,
) -> Result<Expression, ParsingError> {
    let expression = parse_primary_expression(tokens, position, symbol_table)?;

    if let Some(token) = tokens.get(*position) {
        match &token.token_type {
            TokenType::BinaryOperator => {
                *position += 1;
                let operator = match token.value.as_str() {
                    "+" => BinaryOperator::Plus,
                    "-" => BinaryOperator::Minus,
                    "*" => BinaryOperator::Multiply,
                    "/" => BinaryOperator::Divide,
                    _ => {
                        return Err(ParsingError::InvalidExpression {
                            value: token.value.clone(),
                        });
                    }
                };

                let right_expr = parse_expression(tokens, position, symbol_table)?;
                Ok(Expression::BinaryOperation {
                    left: Box::new(expression),
                    operator,
                    right: Box::new(right_expr),
                })
            }
            _ => Ok(expression),
        }
    } else {
        Ok(expression)
    }
}

fn parse_primary_expression(
    tokens: &[Token],
    position: &mut usize,
    symbol_table: &SymbolTable,
) -> Result<Expression, ParsingError> {
    if let Some(token) = tokens.get(*position) {
        match &token.token_type {
            TokenType::Identifier => {
                *position += 1;
                Ok(Expression::Variable(token.value.clone()))
            }
            TokenType::Number => {
                *position += 1;
                token
                    .value
                    .parse::<f64>()
                    .map(Expression::Number)
                    .map_err(|_| ParsingError::InvalidNumber {
                        value: token.value.clone(),
                    })
            }
            TokenType::String => {
                *position += 1;
                Ok(Expression::String(token.value.clone()))
            }
            TokenType::Null => {
                *position += 1;
                Ok(Expression::Null)
            }
            TokenType::Boolean => {
                *position += 1;
                let b = token.value == "true";
                Ok(Expression::Boolean(b))
            }
            TokenType::OpenBracket => parse_array_expression(tokens, position, symbol_table),
            TokenType::OpenBrace => parse_dictionary_expression(tokens, position, symbol_table),
            TokenType::OpenParen => {
                *position += 1;
                let expr = parse_expression(tokens, position, symbol_table)?;
                if let Some(close_paren) = tokens.get(*position) {
                    if close_paren.token_type == TokenType::CloseParen {
                        *position += 1;
                        Ok(expr)
                    } else {
                        Err(ParsingError::UnexpectedToken {
                            expected: ")".to_string(),
                            found: close_paren.value.clone(),
                        })
                    }
                } else {
                    Err(ParsingError::UnexpectedEndOfInput)
                }
            }
            _ => Err(ParsingError::InvalidExpression {
                value: token.value.clone(),
            }),
        }
    } else {
        Err(ParsingError::UnexpectedEndOfInput)
    }
}

fn parse_binary_operation(
    tokens: &[Token],
    position: &mut usize,
    symbol_table: &SymbolTable,
) -> Result<Expression, ParsingError> {
    let mut left_expr = parse_primary_expression(tokens, position, symbol_table)?;

    while let Some(token) = tokens.get(*position) {
        match &token.token_type {
            TokenType::BinaryOperator => {
                *position += 1;
                let operator = match token.value.as_str() {
                    "+" => BinaryOperator::Plus,
                    "-" => BinaryOperator::Minus,
                    "*" => BinaryOperator::Multiply,
                    "/" => BinaryOperator::Divide,
                    _ => {
                        return Err(ParsingError::InvalidExpression {
                            value: token.value.clone(),
                        });
                    }
                };

                let right_expr = parse_primary_expression(tokens, position, symbol_table)?;
                left_expr = Expression::BinaryOperation {
                    left: Box::new(left_expr),
                    operator,
                    right: Box::new(right_expr),
                };
            }
            _ => break, // Break the loop if the next token is not a binary operator
        }
    }

    Ok(left_expr)
}
