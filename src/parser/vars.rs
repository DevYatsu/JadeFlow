use crate::{
    parser::expression::parse_expression,
    token::{Token, TokenType},
};

use super::{
    architecture::{Declaration, Expression, Reassignment, SymbolTable},
    expression::parse_with_operator,
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
                let expression = match parse_expression(tokens, position, symbol_table) {
                    Ok(r) => r,
                    Err(e) => match e {
                        ParsingError::ExpectedSomething => {
                            let keyword = if is_mutable {
                                "mut".to_string()
                            } else {
                                "const".to_string()
                            };
                            return Err(ParsingError::IncompleteDeclaration {
                                keyword,
                                name: name.to_string(),
                            });
                        }
                        _ => return Err(e),
                    },
                };

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
                let keyword = if is_mutable {
                    "mut".to_string()
                } else {
                    "const".to_string()
                };
                return Err(ParsingError::MissingInitializer {
                    keyword,
                    name: name.to_string(),
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
            let expression = match parse_expression(tokens, position, symbol_table) {
                Ok(r) => r,
                Err(e) => match e {
                    ParsingError::ExpectedSomething => {
                        let keyword = if is_mutable {
                            "mut".to_string()
                        } else {
                            "const".to_string()
                        };
                        return Err(ParsingError::IncompleteDeclaration {
                            keyword,
                            name: name.to_string(),
                        });
                    }
                    _ => return Err(e),
                },
            };

            match &expression {
                Expression::Variable(var_name) => {
                    // If the expression is a variable, check if it's already declared in the symbol table
                    if let Some(_) = type_from_expression(&expression, symbol_table) {
                        return Ok(Declaration {
                            name: name.to_string(),
                            is_mutable,
                            value: Expression::Variable(var_name.to_string()),
                            ..symbol_table.get_variable(&var_name).unwrap()
                        });
                    } else {
                        return Err(ParsingError::UseOfUndefinedVariable {
                            name: var_name.to_string(),
                        });
                    }
                }
                Expression::Null => {
                    return Err(ParsingError::UnknownVariableType {
                        var_name: name.to_string(),
                    })
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
                        return Err(type_expression_error(&expression, symbol_table, None).unwrap());
                    }
                }
            }
        } else {
            // If there's no colon or assignment operator, it's an error.
            let keyword = if is_mutable {
                "mut".to_string()
            } else {
                "const".to_string()
            };
            return Err(ParsingError::MissingInitializer {
                keyword,
                name: name.to_string(),
            });
        }
    } else {
        // If no identifier is found, it's an error.
        return Err(ParsingError::ExpectedVarInitialization {
            var_value: value.to_string(),
        });
    }
}

// prevent concatenation of objects {} + 2 but allow it for vectors

pub fn parse_var_reassignment(
    tokens: &[Token],
    position: &mut usize,
    name: &str,
    symbol_table: &SymbolTable,
) -> Result<Reassignment, ParsingError> {
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

        if let Some(Token {
            token_type: TokenType::AssignmentOperator,
            value,
        }) = tokens.get(*position)
        {
            let operator = value;
            *position += 1;

            let after_assignment_expression = match parse_expression(tokens, position, symbol_table)
            {
                Ok(r) => r,
                Err(e) => match e {
                    ParsingError::ExpectedSomething => {
                        let keyword = if initial_var.is_mutable {
                            "mut".to_string()
                        } else {
                            "const".to_string()
                        };
                        return Err(ParsingError::IncompleteReassagnment {
                            keyword,
                            name: name.to_string(),
                        });
                    }
                    _ => return Err(e),
                },
            };

            if var_type != initial_var.var_type {
                return Err(ParsingError::CannotChangeAssignedType {
                    assigned_t: initial_var.var_type,
                    at: format!(
                        "{}: {} {operator} {}",
                        name,
                        var_type.to_string(),
                        after_assignment_expression
                    ),
                    found_t: var_type,
                    var_name: name.to_string(),
                });
            }

            // Parse the expression following the assignment operator
            let expression = parse_with_operator(operator, after_assignment_expression, name);

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

            return Ok(Reassignment {
                value: expression,
                name: initial_var.name,
            });
        } else {
            if var_type != initial_var.var_type {
                return Err(ParsingError::CannotChangeAssignedType {
                    assigned_t: initial_var.var_type,
                    at: format!("{}: {}", name, var_type.to_string()),
                    found_t: var_type,
                    var_name: name.to_string(),
                });
            }

            return Err(ParsingError::ExpectedReassignment {
                var_name: name.to_string(),
            });
        }
    } else if let Some(Token {
        token_type: TokenType::AssignmentOperator,
        value,
    }) = tokens.get(*position)
    {
        *position += 1;
        let operator = value;

        let after_operator_expr = match parse_expression(tokens, position, symbol_table) {
            Ok(r) => r,
            Err(e) => match e {
                ParsingError::ExpectedSomething => {
                    let keyword = if initial_var.is_mutable {
                        "mut".to_string()
                    } else {
                        "const".to_string()
                    };
                    return Err(ParsingError::IncompleteReassagnment {
                        keyword,
                        name: name.to_string(),
                    });
                }
                _ => return Err(e),
            },
        };

        // Parse the expression following the assignment operator
        let expression = parse_with_operator(operator, after_operator_expr.clone(), name);

        match &expression {
            Expression::Variable(var_name) => {
                // If the expression is a variable, check if it's already declared in the symbol table
                if let Some(var_type) = type_from_expression(&expression, symbol_table) {
                    if var_type != initial_var.var_type {
                        return Err(ParsingError::CannotChangeAssignedType {
                            assigned_t: initial_var.var_type,
                            found_t: var_type,
                            var_name: name.to_string(),
                            at: format!("{} {operator} {}", name, after_operator_expr),
                        });
                    }
                    return Ok(Reassignment {
                        value: Expression::Variable(var_name.to_string()),
                        name: initial_var.name,
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
                            var_name: name.to_string(),
                            at: format!("{} {operator} {}", name, after_operator_expr),
                        });
                    }

                    return Ok(Reassignment {
                        value: expression.clone(),
                        name: initial_var.name,
                    });
                } else {
                    return Err(type_expression_error(
                        &after_operator_expr,
                        symbol_table,
                        Some(format!("{} {operator} {}", name, after_operator_expr)),
                    )
                    .unwrap_or_else(|| {
                        type_expression_error(
                            expression,
                            symbol_table,
                            Some(format!(
                                "{}: {} {operator} {}",
                                name,
                                initial_var.var_type.as_assignment(),
                                after_operator_expr
                            )),
                        )
                        .unwrap()
                    }));
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

fn type_expression_error(
    expr: &Expression,
    symbol_table: &SymbolTable,
    error_expr: Option<String>,
) -> Option<ParsingError> {
    // we assume there is an error
    // only for a type error when assigning/reassigning value to a variable
    // and expression is a binary operation

    match expr.clone() {
        Expression::BinaryOperation { left, .. } => {
            let mut final_expr: Box<Expression> = left;
            let mut not_left: bool = false;

            while let Some(_) = type_from_expression(&final_expr, symbol_table) {
                final_expr = if not_left {
                    match *final_expr {
                        Expression::BinaryOperation { right, .. } => Box::new(*right.clone()),
                        _ => return None,
                    }
                } else {
                    match *final_expr {
                        Expression::BinaryOperation { left, .. } => Box::new(*left.clone()),
                        _ => {
                            not_left = true;
                            Box::new(expr.clone())
                        }
                    }
                }
            }

            match *final_expr.clone() {
                Expression::BinaryOperation {
                    left,
                    operator,
                    right,
                } => Some(ParsingError::CannotOperationTypeWithType {
                    expr: error_expr.unwrap_or((*final_expr).to_string()),
                    first_type: type_from_expression(&left, symbol_table).unwrap(),
                    second_type: type_from_expression(&right, symbol_table).unwrap(),
                    operator: operator.operator_as_verb(),
                }),
                _ => None,
            }
        }
        _ => None,
    }
}
