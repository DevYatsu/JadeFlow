use super::{architecture::Expression, parse_expression, ParsingError};
use crate::token::{Token, TokenType};
use std::collections::HashMap;

pub fn parse_dictionary_expression(
    tokens: &[Token],
    position: &mut usize,
) -> Result<Expression, ParsingError> {
    let mut expressions: HashMap<String, Expression> = HashMap::new();
    let mut temp_key: Option<String> = None;

    while let Some(token) = tokens.get(*position) {
        match token.token_type {
            TokenType::Separator => {
                *position += 1;
            }
            TokenType::Comma => {
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
                    let value = parse_expression(tokens, position)?;
                    expressions.insert(temp_key.take().unwrap(), value);
                    *position += 1;
                } else {
                    temp_key = Some(parse_expression(tokens, position)?.string_from_expression()?);
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
            _ => {
                handle_invalid_string_dict_key(temp_key, &token.value)?;
            }
        }
    }
    Ok(())
}
