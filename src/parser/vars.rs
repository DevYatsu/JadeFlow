use crate::token::{Token, TokenType};

use super::{
    architecture::{Declaration, Expression, SymbolTable},
    parse_expression, parse_type, type_from_expression, ParsingError,
};

pub fn parse_var_declaration(
    tokens: &[Token],
    position: &mut usize,
    value: &str,
    symbole_table: &SymbolTable,
) -> Result<Declaration, ParsingError> {
    *position += 1;

    let is_mutable = value == "mut";

    if let Some(Token {
        token_type: TokenType::Identifier,
        value,
    }) = tokens.get(*position)
    {
        *position += 1;
        let name = value;

        if let Some(Token {
            token_type: TokenType::Colon,
            ..
        }) = tokens.get(*position)
        {
            *position += 1;
            let var_type = parse_type(tokens, position)?;

            if let Some(Token {
                token_type: TokenType::AssignmentOperator,
                value,
            }) = tokens.get(*position)
            {
                if value != "=" {
                    return Err(ParsingError::CannotReassignIfNotAssigned {
                        operator: value.to_string(),
                        var_name: name.to_string(),
                    });
                }

                *position += 1;

                let expression = parse_expression(tokens, position)?;

                match &expression {
                    Expression::Variable(var_name) => {
                        if let Some(t) = type_from_expression(&expression, symbole_table) {
                            if var_type != t {
                                return Err(ParsingError::AssignedTypeNotFound {
                                    assigned_t: var_type,
                                    found_t: t,
                                });
                            }
                        } else {
                            return Err(ParsingError::UseOfUndefinedVariabl {
                                name: var_name.to_string(),
                            });
                        }
                    }
                    _ => {
                        if let Some(t) = type_from_expression(&expression, symbole_table) {
                            if var_type != t {
                                return Err(ParsingError::AssignedTypeNotFound {
                                    assigned_t: var_type,
                                    found_t: t,
                                });
                            }
                        }
                    }
                }

                return Ok(Declaration {
                    name: name.to_string(),
                    var_type,
                    value: expression,
                    is_mutable,
                });
            } else {
                let value = Expression::Null;

                return Ok(Declaration {
                    name: name.to_string(),
                    var_type,
                    value,
                    is_mutable,
                });
            }
        } else if let Some(Token {
            token_type: TokenType::AssignmentOperator,
            value,
        }) = tokens.get(*position)
        {
            if value != "=" {
                return Err(ParsingError::CannotReassignIfNotAssigned {
                    operator: value.to_string(),
                    var_name: name.to_string(),
                });
            }

            *position += 1;

            let expression = parse_expression(tokens, position)?;

            match &expression {
                Expression::Variable(var_name) => {
                    if let Some(_) = type_from_expression(&expression, &symbole_table) {
                        return Ok(Declaration {
                            name: name.to_string(),
                            is_mutable,
                            ..symbole_table.get_variable(&var_name).unwrap()
                        });
                    } else {
                        return Err(ParsingError::UseOfUndefinedVariabl {
                            name: var_name.to_string(),
                        });
                    }
                }
                _ => {
                    if let Some(var_type) = type_from_expression(&expression, &symbole_table) {
                        return Ok(Declaration {
                            name: name.to_string(),
                            var_type,
                            value: expression,
                            is_mutable,
                        });
                    } else {
                        return Err(ParsingError::ExpectedType {
                            value: name.to_string(),
                        });
                    }
                }
            }
        } else {
            return Err(ParsingError::UnknownVariableType {
                var_name: name.to_string(),
            });
        }
    } else {
        return Err(ParsingError::ExpectedVarInitialization {
            var_value: value.to_string(),
        });
    }
}
