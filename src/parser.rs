pub mod architecture;
mod dictionary;
mod expression;
mod functions;
mod returns;
mod types;
mod vars;
mod vectors;

use std::{io::Error, iter::Peekable, num::ParseIntError, slice::Iter};

use custom_error::custom_error;

use crate::{
    parser::architecture::{program, Program},
    token::{Token, TokenType},
};

use self::{
    architecture::{
        function_call, reassignment, variable, ASTNode, Expression, Statement, SymbolTable,
        VariableType,
    },
    functions::{parse_fn_call, parse_fn_declaration, FunctionParsingError},
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
    SpecialStringOnlyAtFileStart = "Special strings ('') can only appear at the start of the file",
    InvalidSpecialStringContent{invalid_content: String} = "Special strings ('') content is invalid. Supported values: 'strict' and 'soft', found '{invalid_content}'",
    RequiredNewLineAfterSpecialString = "Special strings ('') require to be followed by another special string or nothing",

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

    UnwantedColon = "Type annotation only allowed on variable initialization",
}

// need to resolve the UnexpectedEndOfInput error that appears randomly for instance in fn.jf

pub fn parse(
    mut tokens_iter: Peekable<Iter<'_, Token>>,
    optional_symbol_table: Option<&SymbolTable>,
) -> Result<ASTNode, ParsingError> {
    let mut statements = Vec::new();
    let mut symbol_table = if let Some(table) = optional_symbol_table {
        table.clone()
    } else {
        SymbolTable::new()
    };

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
                        let var = symbol_table.get_variable(&token.value, None)?;
                        if !var.is_mutable {
                            return Err(ParsingError::CannotReassignConst {
                                var_name: token.value.clone(),
                            });
                        }

                        let assignment =
                            parse_var_reassignment(&mut tokens_iter, &var, &mut symbol_table)?;

                        symbol_table.reassign_variable(assignment.clone(), &mut tokens_iter);
                        statements.push(reassignment(assignment));
                    } else if let Some(Token {
                        token_type: TokenType::OpenParen,
                        ..
                    }) = next_token
                    {
                        let call =
                            parse_fn_call(&mut tokens_iter, &token.value, &mut symbol_table)?;

                        statements.push(function_call(call));
                    } else if let Some(Token {
                        token_type: TokenType::Colon,
                        ..
                    }) = next_token
                    {
                        return Err(ParsingError::UnwantedColon);
                    } else {
                        ignore_until_statement(&mut tokens_iter)?;
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
                    ignore_until_statement(&mut tokens_iter)?;
                }
            }
        }
    }

    Ok(program(Program {
        statements,
        symbol_table,
    }))
}

pub fn ignore_whitespace(tokens: &mut Peekable<std::slice::Iter<'_, Token>>) {
    while let Some(Token {
        token_type: TokenType::Separator,
        value,
    }) = tokens.peek()
    {
        if value == "\n" {
            tokens.next();
        } else {
            break;
        }
    }
}

pub fn ignore_until_statement(
    tokens: &mut Peekable<std::slice::Iter<'_, Token>>,
) -> Result<(), ParsingError> {
    let mut last_val: Option<&str> = None;

    while let Some(Token { token_type, value }) = tokens.peek() {
        match token_type {
            TokenType::Separator => {
                tokens.next();
                if let Some(Token {
                    token_type,
                    value: next_val,
                }) = tokens.peek()
                {
                    match token_type {
                        TokenType::Var
                        | TokenType::For
                        | TokenType::Function
                        | TokenType::If
                        | TokenType::Return
                        | TokenType::While
                        | TokenType::Class
                        | TokenType::Match => break,
                        _ => {
                            last_val = Some(next_val);

                            continue;
                        }
                    }
                }
            }
            TokenType::Var
            | TokenType::For
            | TokenType::Function
            | TokenType::If
            | TokenType::Return
            | TokenType::While
            | TokenType::Class
            | TokenType::Match => {
                return Err(ParsingError::ExpectedSeparator {
                    value: last_val.unwrap_or(value).to_owned(),
                })
            }
            _ => {
                tokens.next();
                last_val = Some(value);
                continue;
            }
        }
    }

    Ok(())
}
