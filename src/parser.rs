mod architecture;
mod dictionary;
mod expression;
mod vars;
mod vectors;

use std::io::Error;

use custom_error::custom_error;

use crate::token::{Token, TokenType};

use self::{
    architecture::{
        reassignment, variable, ASTNode, BinaryOperator, Expression, FormattedSegment, Statement,
        SymbolTable, VariableType,
    },
    vars::{parse_var_declaration, parse_var_reassignment},
};

custom_error! {pub ParsingError
    Io{source: Error} = "{source}",
    Default = "Failed to parse tokens",
    UnexpectedEndOfInput = "No more token left to parse",
    ExpectedVarInitialization{var_value: String} = "Expected identifier after \"{var_value}\"",
    ExpectedReassignment{var_name: String} = "Expected assignment of new value to \"{var_name}\"",
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
    UseOfUndefinedVariable{name: String} = "\"{name}\" is not defined",
    UnexpectedToken{expected: String, found: String} = "Expected '{expected}', found '{found}'",
    CannotReassignVar{name: String} = "Cannot reassign \"{name}\" as it is not defined",
    CannotChangeAssignedType{assigned_t: VariableType, found_t: VariableType, var_name: String, at: String} = "Expected type '{assigned_t}', found '{found_t}' for '{var_name}' at: {at}",
    CannotOperationTypeWithType{operator: String, expr: String, first_type: VariableType, second_type: VariableType} = "Failed to {operator} \"{first_type}\" with \"{second_type}\" at: {expr}",

    ExpectedIdentifier{after: String} = "Expected identifier after: '{after}'",
    MissingInitializer{keyword: String, name: String} = "Missing initializer for {keyword} {name}",
    IncompleteDeclaration{keyword: String, name: String} = "Incomplete declaration: {keyword} {name} requires a value assignment",
    IncompleteReassagnment{keyword: String, name: String} = "Incomplete reassignment: {keyword} {name} requires a value assignment",

    ExpectedSomething = "Expected a value, found nothing"
}

// add support for formatted string, and errors when we expect a token and it is not present

pub fn parse(tokens: &mut Vec<Token>) -> Result<ASTNode, ParsingError> {
    let mut position = 0;
    let mut statements = Vec::new();
    let mut symbol_table = SymbolTable::new();

    while position < tokens.len() {
        let statement = parse_statement(&tokens, &mut position, &mut symbol_table)?;
        statements.push(statement);

        if tokens.len() == position {
            break;
        }

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
                // in case several vars are declaring one after another
                // with the comma notation: const x: num, y: num

                if let Some(Token {
                    token_type: TokenType::Comma,
                    ..
                }) = tokens.get(position)
                {
                    position += 1;
                    let var_keyword = if declaration.is_mutable {
                        "mut"
                    } else {
                        "const"
                    };

                    ignore_whitespace(tokens, &mut position);

                    if let Some(Token { token_type, .. }) = tokens.get(position) {
                        if token_type == &TokenType::Identifier {
                            tokens.insert(
                                position,
                                Token {
                                    token_type: TokenType::Var,
                                    value: var_keyword.to_string(),
                                },
                            );
                        } else {
                            return Err(ParsingError::ExpectedIdentifier {
                                after: format!("{},", declaration.to_string()),
                            });
                        }
                    }

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
            _ => {
                if let Some(token) = tokens.get(position) {
                    return Err(ParsingError::ExpectedSeparator {
                        value: token.value.to_string(),
                    });
                } else {
                    return Err(ParsingError::UnexpectedEndOfInput);
                }
            }
        }
    }

    println!("symbol {:?}", symbol_table);
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

    if let Some(Token {
        token_type: TokenType::Identifier,
        value,
    }) = tokens.get(*position)
    {
        if let Some(next_token) = tokens.get(*position + 1) {
            if !(next_token.token_type == TokenType::Colon
                || next_token.token_type == TokenType::AssignmentOperator)
            {
                while let Some(Token { token_type, .. }) = tokens.get(*position) {
                    if token_type != &TokenType::Separator {
                        *position += 1;
                    } else {
                        break;
                    }
                }
                return Ok(Statement {
                    node: ASTNode::Expression(Expression::Null),
                });
            }
        }

        if let Some(var) = symbol_table.get_variable(value) {
            if !var.is_mutable {
                return Err(ParsingError::CannotReassignConst {
                    var_name: value.to_string(),
                });
            }

            let assignment = parse_var_reassignment(tokens, position, value, symbol_table)?;
            symbol_table.reassign_variable(assignment.clone());

            return Ok(reassignment(assignment));
        } else {
            return Err(ParsingError::CannotReassignVar {
                name: value.to_string(),
            });
        }
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
        } => {
            let left_type = type_from_expression(left, symbol_table);
            let right_type = type_from_expression(right, symbol_table);

            match operator {
                BinaryOperator::Plus => {
                    match (left_type, right_type) {
                        (Some(VariableType::Vector), _) => Some(VariableType::Vector),
                        (_, Some(VariableType::Vector)) => Some(VariableType::Vector),

                        (Some(VariableType::Boolean), Some(VariableType::Boolean)) => {
                            Some(VariableType::Boolean)
                        }

                        (Some(VariableType::Dictionary), Some(VariableType::Dictionary)) => {
                            Some(VariableType::Dictionary)
                        }

                        (Some(VariableType::String), Some(VariableType::String)) => {
                            Some(VariableType::String)
                        }
                        (Some(VariableType::Number), Some(VariableType::Number)) => {
                            Some(VariableType::Number)
                        }
                        _ => None, // Invalid binary operation, return None for other cases.
                    }
                }
                _ => {
                    match (left_type, right_type) {
                        (Some(VariableType::Number), Some(VariableType::Number)) => {
                            Some(VariableType::Number)
                        }
                        _ => None,
                        // return None for every other operation cause we can only use these operations on numbers
                    }
                }
            }
        }
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

pub fn ignore_whitespace(tokens: &[Token], position: &mut usize) {
    while let Some(Token {
        token_type: TokenType::Separator,
        ..
    }) = tokens.get(*position)
    {
        *position += 1;
    }
}
