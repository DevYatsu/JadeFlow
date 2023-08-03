pub mod architecture;
mod dictionary;
mod expression;
mod functions;
mod returns;
mod types;
mod vars;
mod vectors;

use std::{io::Error, num::ParseIntError};

use custom_error::custom_error;

use crate::{
    parser::architecture::{program, Program},
    token::{Token, TokenType},
};

use self::{
    architecture::{
        reassignment, variable, ASTNode, Expression, Statement, SymbolTable, VariableType,
    },
    expression::parse_expression,
    functions::{parse_fn_declaration, FunctionParsingError},
    returns::parse_return_statement,
    types::TypeError,
    vars::{parse_var_declaration, parse_var_reassignment},
};

custom_error! {pub ParsingError
    Io{source: Error} = "{source}",
    ParsingTypyError{source: TypeError} = "{source}",
    ParseInt{source: ParseIntError} = "{source}",
    FunctionParsingError{source: FunctionParsingError} = "{source}",
    Default = "Failed to parse tokens",
    UnexpectedEndOfInput = "No more token left to parse",
    ExpectedVarInitialization{var_value: String, found: String} = "Expected identifier after \"{var_value}\", found '{found}'",
    ExpectedReassignment{var_name: String} = "Expected assignment of new value to \"{var_name}\"",
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
    UnexpectedToken{expected: String, found: String} = "Expected '{expected}', found '{found}'",
    CannotReassignVar{name: String} = "Cannot reassign \"{name}\" as it is not defined",
    CannotChangeAssignedType{assigned_t: VariableType, found_t: VariableType, var_name: String, at: String} = "Expected type '{assigned_t}', found '{found_t}' for '{var_name}' at: {at}",

    ExpectedIdentifier{after: String} = "Expected identifier after: '{after}'",
    MissingInitializer{keyword: String, name: String} = "Missing initializer for {keyword} {name}",
    IncompleteDeclaration{keyword: String, name: String} = "Incomplete declaration: {keyword} {name} requires a value assignment",
    IncompleteReassagnment{keyword: String, name: String} = "Incomplete reassignment: {keyword} {name} requires a value assignment",

    ExpectedSomething = "Expected an expression, found nothing",
    ExpectedValidExpressionInFormattedString = "Expected a valid expression in formatted string argument",

    TypeInvalidExpressionElement{expr: Expression} = "Cannot use a type as an expression value: {expr}",

    ExpectedValidVectorIndex{found: String} = "Expected valid vector index, found {found}",
    ExpectedBracketAfterVectorIndex{found: String} = "Expected ']' after vector index, found {found}",

    UnwantedColon = "Type annotation only allowed on variable initialization"
}

// add support for formatted string, and errors when we expect a token and it is not present

pub fn parse(tokens: &mut Vec<Token>) -> Result<ASTNode, ParsingError> {
    let mut statements = Vec::new();
    let mut symbol_table = SymbolTable::new();
    let mut tokens_iter = tokens.iter();

    while 0 != tokens_iter.clone().count() {
        if let Some(token) = tokens_iter.next() {
            match &token.token_type {
                TokenType::Separator => continue,
                TokenType::Var => {
                    let declaration =
                        parse_var_declaration(&mut tokens_iter, &token.value, &mut symbol_table)?;
                    symbol_table.insert_variable(declaration.clone());
                    statements.push(variable(declaration));
                }
                TokenType::Identifier => {
                    let mut peek_iter = tokens_iter.clone().peekable();
                    let next_token = peek_iter.peek();
                    if let Some(Token {
                        token_type: TokenType::AssignmentOperator,
                        ..
                    }) = next_token
                    {
                        let var = symbol_table.get_variable(&token.value)?;
                        if !var.is_mutable {
                            return Err(ParsingError::CannotReassignConst {
                                var_name: token.value.clone(),
                            });
                        }

                        let assignment =
                            parse_var_reassignment(&mut tokens_iter, &var, &mut symbol_table)?;

                        symbol_table.reassign_variable(assignment.clone());
                        statements.push(reassignment(assignment));
                    } else {
                        if let Some(Token {
                            token_type: TokenType::Colon,
                            ..
                        }) = next_token
                        {
                            return Err(ParsingError::UnwantedColon);
                        }

                        parse_expression(&mut tokens_iter, &mut symbol_table)?;
                        statements.push(Statement {
                            node: ASTNode::Expression(Expression::Null),
                        });
                    }
                }
                TokenType::Function => {
                    statements.push(parse_fn_declaration(&mut tokens_iter, &mut symbol_table)?);
                }
                TokenType::Return => {
                    statements.push(parse_return_statement(&mut tokens_iter, &mut symbol_table)?);
                }
                TokenType::Comma => {
                    if statements.len() == 0 {
                        return Err(ParsingError::InvalidExpression {
                            value: ",".to_string(),
                        });
                    }
                    match &statements[statements.len() - 1] {
                        Statement {
                            node: ASTNode::VariableDeclaration(dec),
                        } => {
                            let keyword = if dec.is_mutable { "mut" } else { "const" };
                            let declaration = parse_var_declaration(
                                &mut tokens_iter,
                                &keyword,
                                &mut symbol_table,
                            )?;
                            symbol_table.insert_variable(declaration.clone());
                            statements.push(variable(declaration));
                        }
                        _ => {
                            return Err(ParsingError::InvalidExpression {
                                value: ",".to_string(),
                            })
                        }
                    }
                }
                _ => {
                    return Err(ParsingError::UnexpectedToken {
                        expected: ";".to_owned(),
                        found: token.value.clone(),
                    });
                }
            }
        }
    }

    Ok(program(Program {
        statements,
        symbol_table,
    }))
}

pub fn ignore_whitespace(tokens: &mut std::slice::Iter<'_, Token>) {
    let mut clone_iter = tokens.clone().peekable();

    while let Some(Token {
        token_type: TokenType::Separator,
        ..
    }) = clone_iter.peek()
    {
        tokens.next();
    }
}
