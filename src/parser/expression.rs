use super::{
    architecture::{BinaryOperator, Expression, FormattedSegment, SymbolTable, VariableType},
    dictionary::parse_dictionary_expression,
    functions::parse_fn_call,
    ignore_whitespace,
    vectors::{parse_array_expression, parse_array_indexing},
    ParsingError,
};
use crate::token::{Token, TokenType};

pub fn parse_expression(
    tokens: &mut std::slice::Iter<'_, Token>,
    symbol_table: &SymbolTable,
) -> Result<Expression, ParsingError> {
    parse_expression_with_precedence(tokens, symbol_table, 0)
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
        left: Box::new(Expression::Variable(initial_var_name.to_owned())),
        operator,
        right: Box::new(expr),
    }
}

fn parse_expression_with_precedence(
    tokens: &mut std::slice::Iter<'_, Token>,
    symbol_table: &SymbolTable,
    precedence: u8,
) -> Result<Expression, ParsingError> {
    let mut expression = parse_primary_expression(tokens, symbol_table)?;

    let mut peekable = tokens.clone().peekable();
    if let Some(token) = peekable.peek() {
        match token.token_type {
            TokenType::Var
            | TokenType::For
            | TokenType::Function
            | TokenType::CloseBrace
            | TokenType::While
            | TokenType::Match
            | TokenType::Comma
            | TokenType::Identifier => return Ok(expression),
            _ => (),
        }
    }

    while let Some(token) = tokens.next() {
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
            | TokenType::Separator => break,
            _ => {
                return Err(ParsingError::UnexpectedToken {
                    expected: ";".to_owned(),
                    found: token.value.to_owned(),
                })
            }
        }
    }

    Ok(expression)
}

fn parse_primary_expression(
    tokens: &mut std::slice::Iter<'_, Token>,
    symbol_table: &SymbolTable,
) -> Result<Expression, ParsingError> {
    if let Some(token) = tokens.next() {
        match &token.token_type {
            TokenType::Identifier => {
                let next = tokens.next();
                if let Some(Token {
                    token_type: TokenType::OpenParen,
                    ..
                }) = next
                {
                    let function = symbol_table.get_function(&token.value)?;

                    let assignment = parse_fn_call(tokens, &symbol_table, &function)?;
                }

                let var = symbol_table.get_variable(&token.value)?;
                if var.var_type != VariableType::Vector {
                    Ok(Expression::Variable(token.value.clone()))
                } else {
                    if let Some(Token {
                        token_type: TokenType::OpenBracket,
                        ..
                    }) = next
                    {
                        Ok(parse_array_indexing(tokens, var)?)
                    } else {
                        Ok(Expression::Variable(token.value.clone()))
                    }
                }
            }
            TokenType::Number => token
                .value
                .parse::<f64>()
                .map(Expression::Number)
                .map_err(|_| ParsingError::InvalidNumber {
                    value: token.value.clone(),
                }),
            TokenType::String => Ok(Expression::String(token.value.clone())),
            TokenType::FormatedString => Ok(Expression::FormattedString(
                FormattedSegment::from_str(&token.value, symbol_table)?,
            )),
            TokenType::Null => Ok(Expression::Null),
            TokenType::Boolean => {
                return Ok(Expression::Boolean(token.value == "true"));
            }
            TokenType::OpenBracket => parse_array_expression(tokens, symbol_table),
            TokenType::OpenBrace => parse_dictionary_expression(tokens, symbol_table),
            TokenType::OpenParen => {
                ignore_whitespace(tokens);

                let expr = parse_expression(tokens, symbol_table)?;
                ignore_whitespace(tokens);

                if let Some(close_paren) = tokens.next() {
                    if close_paren.token_type == TokenType::CloseParen {
                        Ok(expr)
                    } else {
                        Err(ParsingError::UnexpectedToken {
                            expected: ")".to_owned(),
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
