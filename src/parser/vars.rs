use crate::{
    parser::expression::parse_expression,
    token::{Token, TokenType},
};

use super::{
    architecture::{Declaration, Expression, Reassignment, SymbolTable},
    expression::parse_with_operator,
    types::{parse_type, type_from_expression},
    ParsingError,
};

pub fn parse_var_declaration(
    tokens: &[Token],
    position: &mut usize,
    var_keyword: &str,
    symbol_table: &SymbolTable,
) -> Result<Declaration, ParsingError> {
    *position += 1;

    let is_mutable = var_keyword == "mut";

    if tokens.get(*position).is_none() {
        return Err(ParsingError::UnexpectedEndOfInput);
    }

    // Expect an identifier token (variable name)
    let name = tokens.get(*position).map_or_else(
        || Err(ParsingError::UnexpectedEndOfInput),
        |token| match &token.token_type {
            TokenType::Identifier => Ok(token.value.clone()),
            _ => Err(ParsingError::ExpectedVarInitialization {
                var_value: var_keyword.to_owned(),
                found: token.value.clone(),
            }),
        },
    )?;

    *position += 1;

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
                    operator: value.to_owned(),
                    var_name: name,
                });
            }

            *position += 1;
            let expression = parse_expression(tokens, position, symbol_table)?;

            if let Expression::Null = &expression {
                return Ok(Declaration {
                    name,
                    var_type,
                    value: expression,
                    is_mutable,
                });
            }

            let var_type_from_expression = type_from_expression(&expression, symbol_table)?;
            if var_type != var_type_from_expression {
                return Err(ParsingError::AssignedTypeNotFound {
                    assigned_t: var_type,
                    found_t: var_type_from_expression,
                });
            }

            return Ok(Declaration {
                name,
                var_type,
                value: expression,
                is_mutable,
            });
        } else {
            return Err(ParsingError::MissingInitializer {
                keyword: if is_mutable { "mut" } else { "const" }.to_owned(),
                name,
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
                operator: value.to_owned(),
                var_name: name,
            });
        }

        *position += 1;
        let expression = parse_expression(tokens, position, symbol_table)?;

        match &expression {
            Expression::Null => return Err(ParsingError::UnknownVariableType { var_name: name }),
            _ => {
                let var_type = type_from_expression(&expression, symbol_table)?;
                return Ok(Declaration {
                    name,
                    var_type,
                    value: expression,
                    is_mutable,
                });
            }
        }
    } else {
        // If there's no colon or assignment operator, it's an error.
        return Err(ParsingError::MissingInitializer {
            keyword: if is_mutable { "mut" } else { "const" }.to_owned(),
            name,
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
    let initial_var = symbol_table.get_variable(name)?;

    if tokens.get(*position).is_none() {
        return Err(ParsingError::UnexpectedEndOfInput);
    }

    if let Some(Token {
        token_type: TokenType::AssignmentOperator,
        value: operator,
    }) = tokens.get(*position)
    {
        *position += 1;

        // Parse the expression following the assignment operator
        let after_assignment_expression = parse_expression(tokens, position, symbol_table)?;

        let after_assignment_expression_string = after_assignment_expression.to_owned();

        // Parse the expression with the operator
        let expression = parse_with_operator(&operator, after_assignment_expression, name);

        // Check if the expression type matches the declared variable type
        let t = type_from_expression(&expression, symbol_table)?;

        if initial_var.var_type != t {
            return Err(ParsingError::CannotChangeAssignedType {
                assigned_t: initial_var.var_type,
                found_t: t,
                var_name: name.to_owned(),
                at: format!(
                    "{} {} {}",
                    name, operator, after_assignment_expression_string
                ),
            });
        }

        return Ok(Reassignment {
            value: expression,
            name: initial_var.name,
        });
    } else {
        // If there's no assignment operator, it's an error.
        return Err(ParsingError::ExpectedReassignment {
            var_name: name.to_owned(),
        });
    }
}
