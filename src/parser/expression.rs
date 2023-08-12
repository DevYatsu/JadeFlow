pub mod operation;
use std::{collections::HashMap, fmt};

use self::operation::BinaryOperator;

use super::{
    architecture::SymbolTable,
    dictionary::parse_dictionary_expression,
    functions::{parse_fn_call, FunctionCall},
    ignore_whitespace,
    vectors::{parse_array_expression, parse_array_indexing},
    ParsingError,
};
use crate::token::{tokenize, Token, TokenType};

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Variable(String),
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
    ArrayExpression(Vec<Expression>),
    DictionaryExpression(HashMap<String, Expression>),
    BinaryOperation {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    FormattedString(Vec<FormattedSegment>),
    FunctionCall(FunctionCall),
}
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Variable(val) => write!(f, "{}", val),
            Expression::Number(val) => write!(f, "{}", val),
            Expression::String(val) => write!(f, "\"{}\"", val),
            Expression::Boolean(val) => write!(f, "{}", val),
            Expression::Null => write!(f, "null"),
            Expression::ArrayExpression(values) => {
                write!(f, "[")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, "]")
            }
            Expression::DictionaryExpression(entries) => {
                write!(f, "{{")?;
                for (i, (key, value)) in entries.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "\"{}\": {}", key, value)?;
                }
                write!(f, "}}")
            }
            Expression::BinaryOperation {
                left,
                operator,
                right,
            } => {
                write!(f, "({} {} {})", left, operator, right)
            }
            Expression::FormattedString(segments) => {
                write!(f, "\"")?;
                for segment in segments {
                    match segment {
                        FormattedSegment::Literal(text) => write!(f, "{}", text)?,
                        FormattedSegment::Expression(expr) => write!(f, "{}", expr)?,
                    }
                }
                write!(f, "\"")
            }
            Expression::FunctionCall(call) => {
                write!(f, "fn ")?;
                write!(f, "{} ", call.function_name)?;
                write!(f, "(")?;

                for argument in &call.arguments {
                    write!(f, "{}, ", argument)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum FormattedSegment {
    // exemple "hey #{2 + 3} how r u ?"
    Literal(String),        // (ex: "hey " and " how r u ?")
    Expression(Expression), // (ex: 2 + 3 -> 5)
}
impl FormattedSegment {
    pub fn from_str(
        input: &str,
        symbol_table: &mut SymbolTable,
    ) -> Result<Vec<FormattedSegment>, ParsingError> {
        let mut result: Vec<FormattedSegment> = Vec::new();
        let mut current_part = String::new();
        let mut inside_expression = false;
        let mut expression = String::new();

        for (i, c) in input.chars().enumerate() {
            if inside_expression {
                if c == '}' {
                    // Finished parsing the expression, add it to the result
                    inside_expression = false;
                    let t: Vec<Token> = match tokenize(expression[1..].as_bytes()) {
                        Ok(t) => t.into(),
                        Err(_) => {
                            return Err(ParsingError::ExpectedValidExpressionInFormattedString)
                        }
                    };

                    let expr = FormattedSegment::Expression(parse_expression(
                        &mut t.iter().peekable(),
                        symbol_table,
                    )?);
                    result.push(expr);
                    expression.clear();
                } else {
                    expression.push(c);
                }
            } else {
                if c == '#' {
                    // Check if this is the start of an expression
                    let next_char = input.chars().nth(i + 1);

                    if let Some('{') = next_char {
                        // This is the start of an expression
                        inside_expression = true;

                        // Add the current string part to the result
                        if !current_part.is_empty() {
                            result.push(FormattedSegment::Literal(current_part.clone()));
                        }
                        current_part.clear();
                    } else {
                        // Just a regular '#' character, add it to the current part
                        current_part.push(c);
                    }
                } else {
                    // Add the character to the current part
                    current_part.push(c);
                }
            }
        }

        // Add the remaining string part to the result
        if !current_part.is_empty() {
            result.push(FormattedSegment::Literal(current_part));
        }

        Ok(result)
    }
}

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

                let var = symbol_table.get_variable(&token.value)?;
                    if let Some(Token {
                        token_type: TokenType::OpenBracket,
                        ..
                    }) = next
                    {
                        tokens.next();
                        Ok(parse_array_indexing(tokens, &var.name, symbol_table)?)
                    } else {
                        Ok(Expression::Variable(token.value.clone()))
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
