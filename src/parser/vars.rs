use crate::{
    parser::expression::parse_expression,
    token::{Token, TokenType},
};

use super::{
    architecture::{Declaration, Expression, SymbolTable},
    parse_type, type_from_expression, ParsingError,
};

pub fn parse_var_declaration(
    tokens: &[Token],
    position: &mut usize,
    value: &str,
    symbol_table: &SymbolTable,
) -> Result<Declaration, ParsingError> {
    *position += 1;

    let is_mutable = value == "mut";

    // Expect an identifier token (variable name)
    if let Some(Token {
        token_type: TokenType::Identifier,
        value,
    }) = tokens.get(*position)
    {
        *position += 1;
        let name = value;

        // Check if the next token is a colon (':')
        if let Some(Token {
            token_type: TokenType::Colon,
            ..
        }) = tokens.get(*position)
        {
            *position += 1;
            let var_type = parse_type(tokens, position)?;

            // Check if the next token is an assignment operator ('=')
            if let Some(Token {
                token_type: TokenType::AssignmentOperator,
                value,
            }) = tokens.get(*position)
            {
                // Ensure the assignment operator is '='
                if value != "=" {
                    return Err(ParsingError::CannotReassignIfNotAssigned {
                        operator: value.to_string(),
                        var_name: name.to_string(),
                    });
                }

                *position += 1;

                // Parse the expression following the assignment operator
                let expression = parse_expression(tokens, position, symbol_table)?;

                // Check if the expression type matches the declared variable type
                match &expression {
                    Expression::Variable(var_name) => {
                        if let Some(t) = type_from_expression(&expression, symbol_table) {
                            if var_type != t {
                                return Err(ParsingError::AssignedTypeNotFound {
                                    assigned_t: var_type,
                                    found_t: t,
                                });
                            }
                        } else {
                            return Err(ParsingError::UseOfUndefinedVariable {
                                name: var_name.to_string(),
                            });
                        }
                    }
                    _ => {
                        if let Some(t) = type_from_expression(&expression, symbol_table) {
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
                // If no assignment operator is found, the variable is declared but not assigned a value.
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
            // If there is an assignment operator without a preceding colon, it's an error.
            if value != "=" {
                return Err(ParsingError::CannotReassignIfNotAssigned {
                    operator: value.to_string(),
                    var_name: name.to_string(),
                });
            }

            *position += 1;

            // Parse the expression following the assignment operator
            let expression = parse_expression(tokens, position, symbol_table)?;

            match &expression {
                Expression::Variable(var_name) => {
                    // If the expression is a variable, check if it's already declared in the symbol table
                    if let Some(_) = type_from_expression(&expression, symbol_table) {
                        return Ok(Declaration {
                            name: name.to_string(),
                            is_mutable,
                            ..symbol_table.get_variable(&var_name).unwrap()
                        });
                    } else {
                        return Err(ParsingError::UseOfUndefinedVariable {
                            name: var_name.to_string(),
                        });
                    }
                }
                _ => {
                    // For non-variable expressions, get the type from the expression and create the declaration.
                    if let Some(var_type) = type_from_expression(&expression, symbol_table) {
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
            // If there's no colon or assignment operator, it's an error.
            return Err(ParsingError::UnknownVariableType {
                var_name: name.to_string(),
            });
        }
    } else {
        // If no identifier is found, it's an error.
        return Err(ParsingError::ExpectedVarInitialization {
            var_value: value.to_string(),
        });
    }
}

// ADD SUPPORT FOR different operators (+=, -=, *=, /=)
// prevent concatenation of objects {} + 2 but allow it for vectors

pub fn parse_var_reassignment(
    tokens: &[Token],
    position: &mut usize,
    name: &str,
    symbol_table: &SymbolTable,
) -> Result<Declaration, ParsingError> {
    *position += 1;
    let initial_var = symbol_table.get_variable(name).unwrap();

    // Check if the next token is a colon (':')
    if let Some(Token {
        token_type: TokenType::Colon,
        ..
    }) = tokens.get(*position)
    {
        *position += 1;
        let var_type = parse_type(tokens, position)?;

        if var_type != initial_var.var_type {
            return Err(ParsingError::CannotChangeAssignedType {
                assigned_t: initial_var.var_type,
                found_t: var_type,
            });
        }

        if let Some(Token {
            token_type: TokenType::AssignmentOperator,
            value,
        }) = tokens.get(*position)
        {
            *position += 1;

            // Parse the expression following the assignment operator
            let expression = parse_expression(tokens, position, symbol_table)?;

            // Check if the expression type matches the declared variable type
            match &expression {
                Expression::Variable(var_name) => {
                    if let Some(t) = type_from_expression(&expression, symbol_table) {
                        if var_type != t {
                            return Err(ParsingError::AssignedTypeNotFound {
                                assigned_t: var_type,
                                found_t: t,
                            });
                        }
                    } else {
                        return Err(ParsingError::UseOfUndefinedVariable {
                            name: var_name.to_string(),
                        });
                    }
                }
                _ => {
                    if let Some(t) = type_from_expression(&expression, symbol_table) {
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
                value: expression,
                ..initial_var
            });
        } else {
            // If no assignment operator is found, the variable is declared but not assigned a value.
            let value = Expression::Null;

            return Ok(Declaration {
                value: value.clone(),
                ..initial_var
            });
        }
    } else if let Some(Token {
        token_type: TokenType::AssignmentOperator,
        value,
    }) = tokens.get(*position)
    {
        //manage the different operators
        *position += 1;

        // Parse the expression following the assignment operator
        let expression = parse_expression(tokens, position, symbol_table)?;

        match &expression {
            Expression::Variable(var_name) => {
                // If the expression is a variable, check if it's already declared in the symbol table
                if let Some(var_type) = type_from_expression(&expression, symbol_table) {
                    if var_type != initial_var.var_type {
                        return Err(ParsingError::CannotChangeAssignedType {
                            assigned_t: initial_var.var_type,
                            found_t: var_type,
                        });
                    }
                    return Ok(Declaration {
                        value: Expression::Variable(var_name.to_string()),
                        ..initial_var
                    });
                } else {
                    return Err(ParsingError::UseOfUndefinedVariable {
                        name: var_name.to_string(),
                    });
                }
            }
            expression => {
                if let Some(var_type) = type_from_expression(&expression, symbol_table) {
                    if var_type != initial_var.var_type {
                        return Err(ParsingError::CannotChangeAssignedType {
                            assigned_t: initial_var.var_type,
                            found_t: var_type,
                        });
                    }

                    return Ok(Declaration {
                        value: expression.clone(),
                        ..initial_var
                    });
                } else {
                    return Ok(Declaration {
                        value: expression.clone(),
                        ..initial_var
                    });
                }
            }
        }
    } else {        
        // If there's no colon or assignment operator, it's an error.
        return Err(ParsingError::UnknownVariableType {
            var_name: name.to_string(),
        });
    }
}
