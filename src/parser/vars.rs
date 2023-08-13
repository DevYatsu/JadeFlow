use std::{collections::HashMap, fmt};

use crate::{
    evaluation::evaluate_expression,
    parser::expression::parse_expression,
    token::{tokenize, Token, TokenType},
};

use super::{
    architecture::{ASTNode, Statement, SymbolTable},
    expression::{parse_with_operator, Expression},
    ignore_whitespace,
    types::{parse_type, type_from_expression, VariableType},
    ParsingError,
};

#[derive(Debug, Clone)]
pub struct Declaration {
    pub name: String,
    pub var_type: VariableType,
    pub value: Expression,
    pub is_mutable: bool,
    pub is_object_prop: bool,
}
pub fn variable(declaration: Declaration) -> Statement {
    Statement {
        node: ASTNode::VariableDeclaration(declaration),
    }
}
impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let var = self.get_var_keyword();
        write!(
            f,
            "{} {}: {} = {}",
            var,
            self.name,
            self.var_type.as_assignment(),
            self.value
        )
    }
}
impl Declaration {
    pub fn equivalent_tokens(&self) -> Vec<Token> {
        let keyword = self.get_var_keyword();
        let value = match &self.value {
            Expression::Null => match self.var_type {
                VariableType::String => Expression::String("".to_owned()),
                VariableType::Number => Expression::Number(0.0),
                VariableType::Boolean => Expression::Boolean(false),
                VariableType::Vector => Expression::ArrayExpression(Vec::new()),
                VariableType::Dictionary => Expression::DictionaryExpression(HashMap::new()),
            },
            _ => self.value.to_owned(),
        };
        let source_code = format!(
            "{} {}: {} = {};",
            keyword,
            self.name,
            self.var_type.as_assignment(),
            value
        );
        tokenize(source_code.as_bytes()).unwrap().into()
    }
    fn get_var_keyword(&self) -> &str {
        if self.is_mutable {
            "mut"
        } else {
            "const"
        }
    }
}

#[derive(Debug, Clone)]
pub struct Reassignment {
    pub name: String,
    pub value: Expression,
}
pub fn reassignment(reassignement: Reassignment) -> Statement {
    Statement {
        node: ASTNode::VariableReassignment(reassignement),
    }
}

pub fn parse_var_declaration(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    var_keyword: &str,
    symbol_table: &mut SymbolTable,
) -> Result<Declaration, ParsingError> {
    let is_mutable = var_keyword == "mut";
    ignore_whitespace(tokens);

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
    ignore_whitespace(tokens);

    let next = tokens.next();

    if let Some(Token {
        token_type: TokenType::Colon,
        ..
    }) = next
    {
        let var_type = parse_type(tokens.next())?;
        ignore_whitespace(tokens);

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
            ignore_whitespace(tokens);

            let mut expression = parse_expression(tokens, symbol_table)?;

            match expression {
                Expression::Null => {
                    return Ok(Declaration {
                        name,
                        var_type,
                        value: expression,
                        is_mutable,
                        is_object_prop: false,
                    })
                }
                Expression::FunctionCall(_) => (),
                _ => expression = evaluate_expression(expression, symbol_table)?,
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
                is_object_prop: false,
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
        ignore_whitespace(tokens);

        let mut expression = parse_expression(tokens, symbol_table)?;

        match &expression {
            Expression::Null => return Err(ParsingError::UnknownVariableType { var_name: name }),
            Expression::FunctionCall(_) => (),
            _ => expression = evaluate_expression(expression, symbol_table)?,
        }

        let var_type = type_from_expression(&expression, symbol_table)?;

        return Ok(Declaration {
            name,
            var_type,
            value: expression,
            is_mutable,
            is_object_prop: false,
        });
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
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    initial_var: Option<&Declaration>,
    symbol_table: &mut SymbolTable,
    identifier: &str,
) -> Result<Reassignment, ParsingError> {
    ignore_whitespace(tokens);

    // initial_var is none if it's an object prop reassignment

    let next = tokens.next();
    if next.is_none() {
        return Err(ParsingError::UnexpectedEndOfInput);
    }

    if let Some(Token {
        token_type: TokenType::AssignmentOperator,
        value: operator,
    }) = next
    {
        ignore_whitespace(tokens);

        let after_assignment_expression = parse_expression(tokens, symbol_table)?;

        let after_assignment_expression_string = after_assignment_expression.to_owned();

        if operator != "=" && initial_var.is_none() {
            return Err(ParsingError::CannotReassignNotDefinedDictProp {
                operator: operator.to_owned(),
                prop: identifier.to_owned(),
            });
        }

        let expression = parse_with_operator(
            &operator,
            after_assignment_expression,
            &identifier.to_owned(),
        );

        if let Some(initial_var) = initial_var {
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
        }

        return Ok(Reassignment {
            value: expression,
            name: identifier.to_owned(),
        });
    } else {
        // If there's no assignment operator, it's an error.
        return Err(ParsingError::ExpectedReassignment {
            var_name: identifier.to_owned(),
        });
    }
}
