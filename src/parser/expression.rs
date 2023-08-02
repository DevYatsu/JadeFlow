use super::{
    architecture::{BinaryOperator, Expression, FormattedSegment, SymbolTable, VariableType},
    dictionary::parse_dictionary_expression,
    ignore_whitespace,
    vectors::{parse_array_expression, parse_array_indexing},
    ParsingError,
};
use crate::token::{Token, TokenType};

pub fn parse_expression(
    tokens: &[Token],
    position: &mut usize,
    symbol_table: &SymbolTable,
) -> Result<Expression, ParsingError> {
    parse_expression_with_precedence(tokens, position, symbol_table, 0)
}

pub fn parse_with_operator(operator: &str, expr: Expression, initial_var_name: &str) -> Expression {
    // we assume the fn is used for reassigment with these operators:
    // +=, -=, *=, %= or **=

    let operator = match operator {
        "=" => return expr.clone(),
        "+=" => BinaryOperator::Plus,
        "-=" => BinaryOperator::Minus,
        "*=" => BinaryOperator::Multiply,
        "**=" => BinaryOperator::Exponential,
        "/=" => BinaryOperator::Divide,
        "%=" => BinaryOperator::Modulo,
        _ => unreachable!(), // no other operator
    };

    Expression::BinaryOperation {
        left: Box::new(Expression::Variable(initial_var_name.to_string())),
        operator,
        right: Box::new(expr),
    }
}

fn parse_expression_with_precedence(
    tokens: &[Token],
    position: &mut usize,
    symbol_table: &SymbolTable,
    precedence: u8,
) -> Result<Expression, ParsingError> {
    let mut expression = parse_primary_expression(tokens, position, symbol_table)?;

    while let Some(token) = tokens.get(*position) {
        match &token.token_type {
            TokenType::BinaryOperator => {
                let operator_precedence = match token.value.as_str() {
                    "**" => 3,            // Highest precedence for exponentiation
                    "*" | "/" | "%" => 2, // Multiplication, Division, and Modulo
                    "+" | "-" => 1,       // Addition and Subtraction
                    _ => {
                        return Err(ParsingError::InvalidExpression {
                            value: token.value.clone(),
                        })
                    }
                };

                if operator_precedence < precedence {
                    break; // Exit the loop if the operator has lower precedence
                }

                *position += 1;
                let operator = match token.value.as_str() {
                    "+" => BinaryOperator::Plus,
                    "-" => BinaryOperator::Minus,
                    "*" => BinaryOperator::Multiply,
                    "/" => BinaryOperator::Divide,
                    "%" => BinaryOperator::Modulo,
                    "**" => BinaryOperator::Exponential,
                    _ => unreachable!(), // This should never happen due to the match above
                };

                let right_expr = parse_expression_with_precedence(
                    tokens,
                    position,
                    symbol_table,
                    operator_precedence + 1,
                )?;
                expression = Expression::BinaryOperation {
                    left: Box::new(expression),
                    operator,
                    right: Box::new(right_expr),
                };
            }
            TokenType::CloseParen
            | TokenType::CloseBracket
            | TokenType::Colon
            | TokenType::Comma
            | TokenType::Separator => break,

            TokenType::Var
            | TokenType::For
            | TokenType::Function
            | TokenType::CloseBrace
            | TokenType::While
            | TokenType::Match
            | TokenType::Identifier => {
                *position -= 1;
                break;
            }
            _ => {
                return Err(ParsingError::UnexpectedToken {
                    expected: ";".to_string(),
                    found: token.value.to_string(),
                })
            }
        }
    }

    Ok(expression)
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
                let var = symbol_table.get_variable(&token.value)?;
                if var.var_type != VariableType::Vector {
                    Ok(Expression::Variable(token.value.clone()))
                } else {
                    if let Some(Token {
                        token_type: TokenType::OpenBracket,
                        ..
                    }) = tokens.get(*position)
                    {
                        Ok(parse_array_indexing(tokens, position, var)?)
                    } else {
                        Ok(Expression::Variable(token.value.clone()))
                    }
                }
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
            TokenType::FormatedString => {
                *position += 1;
                Ok(Expression::FormattedString(FormattedSegment::from_str(
                    &token.value,
                    symbol_table,
                )?))
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
                ignore_whitespace(tokens, position);

                let expr = parse_expression(tokens, position, symbol_table)?;
                ignore_whitespace(tokens, position);

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
            TokenType::Separator => Err(ParsingError::ExpectedSomething),
            _ => Err(ParsingError::InvalidExpression {
                value: token.value.clone(),
            }),
        }
    } else {
        Err(ParsingError::UnexpectedEndOfInput)
    }
}
