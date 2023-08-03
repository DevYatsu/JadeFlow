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
    tokens: &mut std::slice::Iter<'_, Token>,
    var_keyword: &str,
    symbol_table: &SymbolTable,
) -> Result<Declaration, ParsingError> {
    let is_mutable = var_keyword == "mut";

    // Expect an identifier token (variable name)
    let name = tokens.next().map_or_else(
        || Err(ParsingError::UnexpectedEndOfInput),
        |token| match &token.token_type {
            TokenType::Identifier => Ok(token.value.clone()),
            _ => Err(ParsingError::ExpectedVarInitialization {
                var_value: var_keyword.to_owned(),
                found: token.value.clone(),
            }),
        },
    )?;
    let next = tokens.next();

    if let Some(Token {
        token_type: TokenType::Colon,
        ..
    }) = next
    {
        let var_type = parse_type(tokens)?;

        if let Some(Token {
            token_type: TokenType::AssignmentOperator,
            value,
        }) = tokens.next()
        {
            if value != "=" {
                return Err(ParsingError::CannotReassignIfNotAssigned {
                    operator: value.to_owned(),
                    var_name: name,
                });
            }
            let expression = parse_expression(tokens, symbol_table)?;
        println!("var: {name} = {expression}");

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
    }) = next
    {
        // If there is an assignment operator without a preceding colon, it's an error.
        if value != "=" {
            return Err(ParsingError::CannotReassignIfNotAssigned {
                operator: value.to_owned(),
                var_name: name,
            });
        }

        let expression = parse_expression(tokens, symbol_table)?;
        println!("var: {name} = {expression}");

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
    tokens: &mut std::slice::Iter<'_, Token>,
    initial_var: &Declaration,
    symbol_table: &SymbolTable,
) -> Result<Reassignment, ParsingError> {
    let next = tokens.next();
    if next.is_none() {
        return Err(ParsingError::UnexpectedEndOfInput);
    }

    if let Some(Token {
        token_type: TokenType::AssignmentOperator,
        value: operator,
    }) = next
    {
        let after_assignment_expression = parse_expression(tokens, symbol_table)?;

        let after_assignment_expression_string = after_assignment_expression.to_owned();

        let expression =
            parse_with_operator(&operator, after_assignment_expression, &initial_var.name);

        let t = type_from_expression(&expression, symbol_table)?;

        if initial_var.var_type != t {
            return Err(ParsingError::CannotChangeAssignedType {
                assigned_t: initial_var.var_type.to_owned(),
                found_t: t,
                var_name: initial_var.name.to_owned(),
                at: format!(
                    "{} {} {}",
                    initial_var.name, operator, after_assignment_expression_string
                ),
            });
        }

        return Ok(Reassignment {
            value: expression,
            name: initial_var.name.to_owned(),
        });
    } else {
        // If there's no assignment operator, it's an error.
        return Err(ParsingError::ExpectedReassignment {
            var_name: initial_var.name.to_owned(),
        });
    }
}
