mod architecture;
mod dictionary;
mod expression;
mod operations;
mod vars;
mod vectors;

use std::io::Error;

use custom_error::custom_error;

use crate::token::{Token, TokenType};

use self::{
    architecture::{
        variable, ASTNode, BinaryOperator, Expression, FormattedSegment, Statement, SymbolTable,
        VariableType,
    },
    dictionary::parse_dictionary_expression,
    vars::{parse_var_declaration, parse_var_reassignment},
    vectors::parse_array_expression,
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
    UseOfUndefinedVariable{name: String} = "\"{name}\" is not defined",
    UnexpectedToken{expected: String, found: String} = "Expected '{expected}, found {found}'",
    CannotReassignVar{name: String} = "Cannot reassign \"{name}\" as it is not defined",
    CannotChangeAssignedType{assigned_t: VariableType, found_t: VariableType} = "Expected type '{assigned_t}', found '{found_t}'",
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
        if let Some(var) = symbol_table.get_variable(value) {
            if !var.is_mutable {
                return Err(ParsingError::CannotReassignConst {
                    var_name: value.to_string(),
                });
            }

            let declaration = parse_var_reassignment(tokens, position, value, symbol_table)?;
            symbol_table.insert_variable(declaration.clone());

            return Ok(variable(declaration));
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
        } => type_from_expression(&*left, symbol_table),
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
