use super::{
    architecture::{Expression, SymbolTable},
    expression::parse_expression,
    ParsingError,
};
use crate::token::{Token, TokenType};
use std::collections::HashMap;

pub fn parse_dictionary_expression(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &SymbolTable,
) -> Result<Expression, ParsingError> {
    let mut expressions: HashMap<String, Expression> = HashMap::new();
    let mut temp_key: Option<String> = None;

    while let Some(token) = tokens.next() {
        match token.token_type {
            TokenType::Separator => {
                continue;
            }
            TokenType::Comma | TokenType::CloseBrace => {
                if temp_key.is_some() {
                    symbol_table
                        .get_variable(&temp_key.clone().unwrap())
                        .and_then(|dec| {
                            Ok(expressions.insert(
                                dec.name.clone(),
                                Expression::Variable(temp_key.take().unwrap().to_string()),
                            ))
                        })?;
                }

                handle_missing_value_dict(&temp_key)?;

                if token.token_type == TokenType::CloseBrace {
                    break;
                } else {
                    continue;
                }
            }
            TokenType::Colon => {
                if temp_key.is_some() {
                    let value = parse_expression(tokens, symbol_table)?;
                    match &value {
                        Expression::Variable(name) => {
                            symbol_table.get_variable(name)?;
                        }
                        _ => (),
                    }

                    expressions.insert(temp_key.take().unwrap(), value);
                    continue;
                } else {
                    return Err(ParsingError::InvalidStringDictKey {
                        key: temp_key.unwrap().to_string(),
                    });
                }
            }
            _ => {
                if temp_key.is_some() {
                    return Err(ParsingError::MissingValueDict {
                        key: temp_key.unwrap().to_string(),
                    });
                } else {
                    if token.token_type == TokenType::Identifier
                        || token.token_type == TokenType::String
                        || token.token_type == TokenType::Number
                    {
                        temp_key = Some(token.value.clone());
                    } else {
                        return Err(ParsingError::InvalidStringDictKey {
                            key: token.value.clone(),
                        });
                    }

                    handle_invalid_string_dict_key(&temp_key, &token.value)?;

                    skip_to_colon(tokens, &temp_key)?;
                }
            }
        }
    }

    Ok(Expression::DictionaryExpression(expressions))
}

fn handle_missing_value_dict(temp_key: &Option<String>) -> Result<(), ParsingError> {
    match temp_key {
        // if temp key is Some() it means that we have not found a value pair
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
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    temp_key: &Option<String>,
) -> Result<(), ParsingError> {
    while let Some(token) = tokens.peek() {
        match token.token_type {
            TokenType::Colon | TokenType::Comma | TokenType::CloseBrace | TokenType::Separator => {
                break;
            }
            _ => {
                handle_invalid_string_dict_key(temp_key, &token.value)?;
            }
        }
    }
    Ok(())
}
