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
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &SymbolTable,
) -> Result<Expression, ParsingError> {
    let expression = parse_expression_with_precedence(tokens, symbol_table, 0)?;
    Ok(expression)
}

// still need to make the expression parsing work for complex calculs

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
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &SymbolTable,
    precedence: u8,
) -> Result<Expression, ParsingError> {
    let mut expression = parse_primary_expression(tokens, symbol_table)?;

    while let Some(token) = tokens.peek() {
        match token.token_type {
            TokenType::CloseBrace | TokenType::CloseBracket | TokenType::Comma => {
                return Ok(expression)
            }
            TokenType::Separator => {
                if token.value == ";" {
                    return Ok(expression);
                } else {
                    let mut clone_iter = tokens.clone().peekable();
                    clone_iter.next();

                    if let Some(token) = clone_iter.peek() {
                        match token.token_type {
                            TokenType::Var
                            | TokenType::For
                            | TokenType::Function
                            | TokenType::CloseBrace
                            | TokenType::While
                            | TokenType::Match
                            | TokenType::Comma
                            | TokenType::Return => return Ok(expression),
                            _ => (),
                        }
                    }
                }
            }
            _ => (),
        }

        if let Some(token) = tokens.next() {
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
                TokenType::CloseParen | TokenType::CloseBracket | TokenType::Colon => break,
                TokenType::Separator => {
                    if token.value == ";" {
                        return Ok(expression);
                    }
                }
                _ => {
                    return Err(ParsingError::UnexpectedToken {
                        expected: ";".to_owned(),
                        found: token.value.to_owned(),
                    })
                }
            }
        }

        println!("{expression}");
    }

    Ok(expression)
}

fn parse_primary_expression(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &SymbolTable,
) -> Result<Expression, ParsingError> {
    ignore_whitespace(tokens);
    if let Some(token) = tokens.next() {
        match &token.token_type {
            TokenType::Identifier => {
                let next = tokens.peek();
                if let Some(Token {
                    token_type: TokenType::OpenParen,
                    ..
                }) = next
                {
                    let fn_call = parse_fn_call(tokens, &token.value, symbol_table)?;

                    return Ok(Expression::FunctionCall(fn_call));
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
                        tokens.next();
                        Ok(parse_array_indexing(tokens, var)?)
                    } else {
                        Ok(Expression::Variable(token.value.clone()))
                    }
                }
            }
            TokenType::Number => {
                let number = token
                    .value
                    .parse::<f64>()
                    .map(Expression::Number)
                    .map_err(|_| ParsingError::InvalidNumber {
                        value: token.value.clone(),
                    })?;

                let mut clone_iter = tokens.clone();
                if let Some(next_token) = clone_iter.peek() {
                    if next_token.token_type == TokenType::BinaryOperator {
                        tokens.next();

                        let operator_precedence = match next_token.value.as_str() {
                            "**" => 3,
                            "*" | "/" | "%" => 2,
                            "+" | "-" => 1,
                            _ => {
                                return Err(ParsingError::InvalidExpression {
                                    value: next_token.value.clone(),
                                })
                            }
                        };

                        // Parse the right-hand side expression with higher precedence
                        let right_expr = parse_expression_with_precedence(
                            tokens,
                            symbol_table,
                            operator_precedence,
                        )?;

                        // Build the expression tree with the binary operator and operands
                        let operator = match next_token.value.as_str() {
                            "+" => BinaryOperator::Plus,
                            "-" => BinaryOperator::Minus,
                            "*" => BinaryOperator::Multiply,
                            "/" => BinaryOperator::Divide,
                            "%" => BinaryOperator::Modulo,
                            "**" => BinaryOperator::Exponential,
                            _ => unreachable!(),
                        };

                        return Ok(Expression::BinaryOperation {
                            left: Box::new(number),
                            operator,
                            right: Box::new(right_expr),
                        });
                    }
                }

                return Ok(number);
            }
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
