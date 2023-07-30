mod architecture;
mod dictionary;
mod vars;

use std::io::Error;

use custom_error::custom_error;

use crate::token::{Token, TokenType};

use self::{
    architecture::{
        variable, ASTNode, BinaryOperator, Expression, FormattedSegment, Statement, SymbolTable,
        VariableType,
    },
    dictionary::parse_dictionary_expression,
    vars::parse_var_declaration,
};

custom_error! {pub ParsingError
    Io{source: Error} = "{source}",
    Default = "Failed to parse tokens",
    UnexpectedEndOfInput = "No more token left to parse",
    ExpectedVarInitialization{var_value: String} = "Expected valid variable name after \"{var_value}\"",
    ExpectedType{value: String} = "Expected valid type after \"{value}\"",
    UnknownVariableType{var_name: String} = "Impossible to guess {var_name} type",
    CannotReassignIfNotAssigned{operator: String, var_name: String} = "Must first assign value to \"{var_name}\" with '=' before using '{operator}'",
    CannotReassignConst{var_name: String} = "Cannot reassign \"{var_name}\" as it is a 'const'",
    InvalidNumber{value: String} = "\"{value}\" is an invalid number!",
    InvalidExpression{value: String} = "\"{value}\" is an invalid expression!",
    MissingClosingParenthesis = "Expected a closing parenthesis!",
    ExpectedSeparator{value: String} = "Expected new line or semicolon after \"{value}\"",
    MissingKeyValueDict = "Dictionary is missing a key value",
    MissingValueDict{key: String} = "Dictionary is missing a value for key \"{key}\"",
    InvalidKeyDict{key_type: Expression} = "\"{key_type}\" is not a handled key variant",
    ExpectedStringAsDictKey = "Expected string as dictionary key",
    AssignedTypeNotFound{assigned_t: VariableType, found_t: VariableType} = "Assigned type '{assigned_t}' is different from found type '{found_t}'",
    InvalidStringDictKey{key: String} = "Expected a string or number as dictionary key, not '{key}'",
    UseOfUndefinedVariabl{name: String} = "\"{name}\" is not defined",
}

pub fn parse(tokens: &mut Vec<Token>) -> Result<ASTNode, ParsingError> {
    let mut position = 0;
    let mut statements = Vec::new();
    let mut symbol_table = SymbolTable::new();

    while position < tokens.len() {
        let statement = parse_statement(&tokens, &mut position, &mut symbol_table)?;
        statements.push(statement);

        if let Some(Token {
            token_type: TokenType::Separator,
            ..
        }) = tokens.get(position)
        {
            position += 1;
            continue;
        }

        match &statements[statements.len() - 1].node {
            ASTNode::VariableDeclaration(declaration) => {
                if let Some(Token {
                    token_type: TokenType::Comma,
                    ..
                }) = tokens.get(position)
                {
                    let var_keyword = if declaration.is_mutable {
                        "mut"
                    } else {
                        "const"
                    };
                    tokens.insert(
                        position + 1,
                        Token {
                            token_type: TokenType::Var,
                            value: var_keyword.to_string(),
                        },
                    );
                    position += 1;
                    continue;
                }
            }
            ASTNode::FunctionDeclaration(_) => {
                position += 1;
                continue;
            }
            ASTNode::ClassDeclaration(_) => {
                position += 1;
                continue;
            }
            _ => (),
        }

        if tokens.len() == position {
            break;
        }

        if let Some(token) = tokens.get(position) {
            return Err(ParsingError::ExpectedSeparator {
                value: token.value.to_string(),
            });
        } else {
            return Err(ParsingError::UnexpectedEndOfInput);
        }
    }
    Ok(ASTNode::Program(statements))
}

fn formatted_string_segments(input: &str) -> Vec<FormattedSegment> {
    let mut segments = Vec::new();
    let mut current_literal = String::new();
    let mut inside_expression = false;
    let mut after_hashtag = false;
    let mut expression = String::new();
    let mut open_par_num = 0;

    for c in input.chars() {
        if c == '#' {
            after_hashtag = true;
        } else if c == '{' {
            if after_hashtag {
                current_literal.pop();
                segments.push(FormattedSegment::Literal(current_literal.to_string()));
                current_literal.clear();

                inside_expression = true;
                open_par_num += 1;
            }
        } else if c == '}' {
            if inside_expression {
                open_par_num -= 1;
                if open_par_num == 0 {
                    //      segments.push(FormattedSegment::Expression(Expression::parse(&expression)));
                    expression.clear();
                    inside_expression = false;
                }
            }
        } else {
            if inside_expression {
                expression.push(c);
            } else {
                current_literal.push(c);
            }
        }
    }

    if !current_literal.is_empty() {
        segments.push(FormattedSegment::Literal(current_literal));
    }

    segments
}

fn parse_statement(
    tokens: &[Token],
    position: &mut usize,
    symbol_table: &mut SymbolTable,
) -> Result<Statement, ParsingError> {
    while let Some(Token {
        token_type: TokenType::Separator,
        ..
    }) = tokens.get(*position)
    {
        *position += 1;
    }

    if let Some(Token {
        token_type: TokenType::Var,
        value,
    }) = tokens.get(*position)
    {
        let declaration = parse_var_declaration(tokens, position, value, symbol_table)?;
        symbol_table.insert_variable(declaration.clone());
        return Ok(variable(declaration));
    }

    Ok(Statement {
        node: ASTNode::Expression(Expression::Null),
    })
}

fn type_from_expression(expr: &Expression, symbol_table: &SymbolTable) -> Option<VariableType> {
    match expr {
        Expression::Number(_) => Some(VariableType::Number),
        Expression::String(_) => Some(VariableType::String),
        Expression::ArrayExpression(_) => Some(VariableType::Vector),
        Expression::FormatedString(_) => Some(VariableType::String),
        Expression::Boolean(_) => Some(VariableType::Boolean),
        Expression::Null => None,
        Expression::DictionaryExpression(_) => Some(VariableType::Dictionary),
        Expression::Variable(var_name) => {
            symbol_table.get_variable(var_name).map(|var| var.var_type)
        }
        Expression::BinaryOperation {
            left,
            operator,
            right,
        } => type_from_expression(&*left, symbol_table),
    }
}

fn parse_expression(tokens: &[Token], position: &mut usize) -> Result<Expression, ParsingError> {
    tokens
        .get(*position)
        .ok_or(ParsingError::UnexpectedEndOfInput)
        .and_then(|token| {
            *position += 1;

            let expression =
                match &token.token_type {
                    TokenType::Identifier => Ok(Expression::Variable(token.value.clone())),
                    TokenType::Number => token
                        .value
                        .parse::<f64>()
                        .map(Expression::Number)
                        .map_err(|_| ParsingError::InvalidNumber {
                            value: token.value.clone(),
                        }),
                    TokenType::String => Ok(Expression::String(token.value.clone())),
                    TokenType::Null => Ok(Expression::Null),
                    TokenType::Boolean => {
                        let b = token.value == "true";
                        Ok(Expression::Boolean(b))
                    }
                    TokenType::OpenParen => parse_parenthesized_expression(tokens, position),
                    TokenType::OpenBracket => parse_array_expression(tokens, position),
                    TokenType::OpenBrace => parse_dictionary_expression(tokens, position),
                    TokenType::BinaryOperator => {
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

                        let left = parse_expression(tokens, position)?;
                        let right = parse_expression(tokens, position)?;

                        Ok(Expression::BinaryOperation {
                            left: Box::new(left),
                            operator,
                            right: Box::new(right),
                        })
                    }
                    _ => Err(ParsingError::InvalidExpression {
                        value: token.value.clone(),
                    }),
                };

            expression
        })
}

fn parse_array_expression(
    tokens: &[Token],
    position: &mut usize,
) -> Result<Expression, ParsingError> {
    let mut vec_expressions = vec![];

    for token in tokens.iter().skip(*position) {
        match &token.token_type {
            TokenType::CloseBracket => {
                *position += 1;
                break;
            }
            TokenType::Comma => {
                *position += 1;
                continue;
            }
            _ => vec_expressions.push(parse_expression(tokens, position)?),
        }
    }

    Ok(Expression::ArrayExpression(vec_expressions))
}

fn parse_parenthesized_expression(
    tokens: &[Token],
    position: &mut usize,
) -> Result<Expression, ParsingError> {
    let expression = parse_expression(tokens, position)?;

    if let Some(Token {
        token_type: TokenType::CloseParen,
        ..
    }) = tokens.get(*position)
    {
        *position += 1;

        Ok(expression)
    } else {
        Err(ParsingError::MissingClosingParenthesis)
    }
}

fn parse_type(tokens: &[Token], position: &mut usize) -> Result<VariableType, ParsingError> {
    // fn to use after encountering ':'
    if let Some(Token {
        token_type: TokenType::TypeBool,
        ..
    }) = tokens.get(*position)
    {
        *position += 1;
        Ok(VariableType::Boolean)
    } else if let Some(Token {
        token_type: TokenType::TypeDict,
        ..
    }) = tokens.get(*position)
    {
        *position += 1;
        Ok(VariableType::Dictionary)
    } else if let Some(Token {
        token_type: TokenType::TypeNumber,
        ..
    }) = tokens.get(*position)
    {
        *position += 1;
        Ok(VariableType::Number)
    } else if let Some(Token {
        token_type: TokenType::TypeString,
        ..
    }) = tokens.get(*position)
    {
        *position += 1;
        Ok(VariableType::String)
    } else if let Some(Token {
        token_type: TokenType::TypeVec,
        ..
    }) = tokens.get(*position)
    {
        *position += 1;
        Ok(VariableType::Vector)
    } else if let Some(Token { value, .. }) = tokens.get(*position) {
        return Err(ParsingError::ExpectedType {
            value: value.to_string(),
        });
    } else {
        return Err(ParsingError::UnexpectedEndOfInput);
    }
}
