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
    symbol_table: &mut SymbolTable,
) -> Result<Expression, ParsingError> {
    let expression = parse_expression_with_precedence(tokens, symbol_table)?;
    Ok(expression)
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
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &mut SymbolTable,
) -> Result<Expression, ParsingError> {
    let mut operand_stack: Vec<Expression> = Vec::new();
    let mut operator_stack: Vec<BinaryOperator> = Vec::new();

    let mut expression = parse_primary_expression(tokens, symbol_table)?;
    operand_stack.push(expression.clone());

    while let Some(token) = tokens.peek() {
        match token.token_type {
            TokenType::BinaryOperator => {
                while let Some(operator) = operator_stack.last() {
                    let operator_precedence = get_precedence(&operator);

                    if operator_precedence >= get_precedence(&str_as_operator(&token.value)?) {
                        apply_operator(&mut operand_stack, operator_stack.pop().unwrap());
                    } else {
                        break;
                    }
                }

                operator_stack.push(str_as_operator(&token.value)?);
            }
            TokenType::Separator => {
                if token.value == ";" {
                    break;
                } else {
                    let mut clone_iter = tokens.clone().peekable();
                    clone_iter.next();

                    if let Some(token) = clone_iter.peek() {
                        match token.token_type {
                            TokenType::BinaryOperator | TokenType::Separator => {
                                tokens.next();
                                continue;
                            }
                            _ => break,
                        }
                    }
                }
            }
            TokenType::CloseParen
            | TokenType::CloseBracket
            | TokenType::Colon
            | TokenType::Comma => {
                // If a closing parenthesis, bracket, or colon is encountered, stop parsing
                break;
            }
            _ => return Ok(expression),
        }

        tokens.next();
        if let Some(_) = tokens.peek() {
            expression = parse_primary_expression(tokens, symbol_table)?;
            operand_stack.push(expression.clone());
        }
    }

    while let Some(operator) = operator_stack.pop() {
        apply_operator(&mut operand_stack, operator);
    }

    // The final result will be the last remaining expression on the operand stack
    let result_expression = operand_stack
        .pop()
        .ok_or(ParsingError::UnexpectedEndOfInput)?;
    println!("resulting expr {:?}", operand_stack);

    Ok(result_expression)
}

fn parse_primary_expression(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &mut SymbolTable,
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

                let var = symbol_table.get_variable(&token.value, None)?;
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

fn get_precedence(operator: &BinaryOperator) -> u8 {
    match operator {
        BinaryOperator::Exponential => 3,
        BinaryOperator::Multiply | BinaryOperator::Divide | BinaryOperator::Modulo => 2,
        BinaryOperator::Plus | BinaryOperator::Minus => 1,
    }
}
fn str_as_operator(string: &str) -> Result<BinaryOperator, ParsingError> {
    Ok(match string {
        "+" => BinaryOperator::Plus,
        "-" => BinaryOperator::Minus,
        "*" => BinaryOperator::Multiply,
        "/" => BinaryOperator::Divide,
        "%" => BinaryOperator::Modulo,
        "**" => BinaryOperator::Exponential,
        _ => {
            return Err(ParsingError::InvalidExpression {
                value: string.to_owned(),
            })
        }
    })
}

fn apply_operator(operands: &mut Vec<Expression>, operator: BinaryOperator) {
    let right_expr = operands.pop().unwrap();
    let left_expr = operands.pop().unwrap();
    let binary_expr = Expression::BinaryOperation {
        left: Box::new(left_expr),
        operator,
        right: Box::new(right_expr),
    };

    operands.push(binary_expr);
}
