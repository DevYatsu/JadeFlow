use super::{
    architecture::{Expression, SymbolTable},
    expression::parse_expression,
    ParsingError,
};
use crate::token::{Token, TokenType};
use std::collections::HashMap;

pub fn parse_dictionary_expression(
    tokens: &[Token],
    position: &mut usize,
    symbol_table: &SymbolTable,
) -> Result<Expression, ParsingError> {
    let mut expressions: HashMap<String, Expression> = HashMap::new();
    let mut temp_key: Option<String> = None;
    *position += 1;

    while let Some(token) = tokens.get(*position) {
        match token.token_type {
            TokenType::Separator => {
                *position += 1;
            }
            TokenType::Comma => {
                if let Some(key) = temp_key.take() {
                    symbol_table
                        .get_variable(&key)
                        .map(|_| expressions.insert(key.clone(), Expression::Variable(key)));
                }
                handle_missing_value_dict(&temp_key)?;
                *position += 1;
            }
            TokenType::Colon => {
                handle_missing_value_dict(&temp_key)?;
                *position += 1;
            }
            TokenType::CloseBrace => {
                handle_missing_value_dict(&temp_key)?;
                *position += 1;
                break;
            }
            _ => {
                if temp_key.is_some() {
                    let value = parse_expression(tokens, position, symbol_table)?;

                    match &value {
                        Expression::Variable(name) => {
                            if symbol_table.get_variable(name).is_none() {
                                return Err(ParsingError::UseOfUndefinedVariable {
                                    name: name.to_string(),
                                });
                            }
                        }
                        _ => (),
                    }

                    expressions.insert(temp_key.take().unwrap(), value);
                    *position += 1;
                } else {
                    temp_key = Some(
                        parse_expression(tokens, position, symbol_table)?
                            .string_from_expression()?,
                    );
                    handle_invalid_string_dict_key(&temp_key, &token.value)?;
                    skip_to_colon(tokens, position, &temp_key)?;
                }
            }
        }
    }

    Ok(Expression::DictionaryExpression(expressions))
}

fn handle_missing_value_dict(temp_key: &Option<String>) -> Result<(), ParsingError> {
    match temp_key {
        Some(key) => Err(ParsingError::MissingValueDict { key: key.clone() }),
        None => Ok(()),
    }
}

fn handle_invalid_string_dict_key(
    temp_key: &Option<String>,
    token_value: &str,
) -> Result<(), ParsingError> {
    if let Some(key) = temp_key {
        if key.contains(" ") {
            let invalid_key = format!("{} {}", key, token_value);
            return Err(ParsingError::InvalidStringDictKey { key: invalid_key });
        }
    }
    Ok(())
}

fn skip_to_colon(
    tokens: &[Token],
    position: &mut usize,
    temp_key: &Option<String>,
) -> Result<(), ParsingError> {
    while let Some(token) = tokens.get(*position) {
        *position += 1;
        match token.token_type {
            TokenType::Separator => {
                continue;
            }
            TokenType::Colon => {
                break;
            }
            TokenType::Comma => {
                *position -= 1;
                break;
            }
            _ => {
                handle_invalid_string_dict_key(temp_key, &token.value)?;
            }
        }
    }
    Ok(())
}
