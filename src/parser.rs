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

    ExpectedSomething = "Expected a value, found nothing",
    ExpectedValidExpressionInFormattedString = "Expected a valid expression in formatted string argument",

    TypeInvalidExpressionElement{expr: Expression} = "Cannot use a type as an expression value: {expr}",

    ExpectedValidVectorIndex{found: String} = "Expected valid vector index, found {found}",
    ExpectedBracketAfterVectorIndex{found: String} = "Expected ']' after vector index, found {found}",

    UnwantedColon = "Type annotation only allowed on variable initialization"
}

// add support for formatted string, and errors when we expect a token and it is not present

pub fn parse(tokens: &mut Vec<Token>) -> Result<ASTNode, ParsingError> {
    let mut position = 0;
    let mut statements = Vec::new();
    let mut symbol_table = SymbolTable::new();

    while position < tokens.len() {
        if let Some(Token { token_type, .. }) = tokens.get(position) {
            if token_type == &TokenType::Separator
                || ((token_type != &TokenType::Var)
                    && (token_type != &TokenType::Function)
                    && (token_type != &TokenType::For)
                    && (token_type != &TokenType::While)
                    && (token_type != &TokenType::Match)
                    && (token_type != &TokenType::Class)
                    && (token_type != &TokenType::Identifier)
                    && (token_type != &TokenType::Return))
            {
                position += 1;
                continue;
            }
        }

        let statement = parse_statement(tokens, &mut position, &mut symbol_table)?;

        statements.push(statement);

        if let Some(Token { token_type, .. }) = tokens.get(position) {
            if token_type == &TokenType::Separator {
                position += 1;
                continue;
            }
        }

        if tokens.len() == position {
            break;
        }

        match &statements[statements.len() - 1].node {
            ASTNode::VariableDeclaration(declaration) => {
                // in case several vars are declaring one after another
                // with the comma notation: const x: num, y: num

                if let Some(Token {
                    token_type: TokenType::Comma,
                    ..
                }) = tokens.get(position)
                {
                    position += 1;
                    let var_keyword = if declaration.is_mutable {
                        "mut"
                    } else {
                        "const"
                    };

                    ignore_whitespace(tokens, &mut position);

                    if let Some(Token { token_type, .. }) = tokens.get(position) {
                        if token_type == &TokenType::Identifier {
                            tokens.insert(
                                position,
                                Token {
                                    token_type: TokenType::Var,
                                    value: var_keyword.to_string(),
                                },
                            );
                        } else {
                            return Err(ParsingError::ExpectedIdentifier {
                                after: format!("{},", declaration.to_string()),
                            });
                        }
                    }

                    continue;
                }
            }
            ASTNode::FunctionDeclaration(_) => {
                position += 1;
                continue;
            }
            ASTNode::ClassDeclaration(_) => {
                position += 1;
                continue;
            }
            _ => {
                if let Some(token) = tokens.get(position) {
                    return Err(ParsingError::ExpectedSeparator {
                        value: token.value.to_string(),
                    });
                } else {
                    return Err(ParsingError::UnexpectedEndOfInput);
                }
            }
        }
    }

    println!("symbol {:?}", symbol_table);
    Ok(program(Program {
        statements,
        symbol_table,
    }))
}

fn parse_statement(
    tokens: &mut Vec<Token>,
    position: &mut usize,
    symbol_table: &mut SymbolTable,
) -> Result<Statement, ParsingError> {
    while let Some(Token {
        token_type: TokenType::Separator,
        ..
    }) = tokens.get(*position)
    {
        *position += 1;
    }

    if let Some(Token {
        token_type: TokenType::Var,
        value,
    }) = tokens.get(*position)
    {
        let declaration = parse_var_declaration(tokens, position, value, symbol_table)?;
        symbol_table.insert_variable(declaration.clone());

        return Ok(variable(declaration));
    }

    if let Some(Token {
        token_type: TokenType::Identifier,
        value,
    }) = tokens.get(*position)
    {
        if let Some(Token {
            token_type: TokenType::AssignmentOperator,
            ..
        }) = tokens.get(*position + 1)
        {
            let var = symbol_table.get_variable(value)?;
            if !var.is_mutable {
                return Err(ParsingError::CannotReassignConst {
                    var_name: value.to_owned(),
                });
            }

            let assignment = parse_var_reassignment(tokens, position, value, symbol_table)?;
            symbol_table.reassign_variable(assignment.clone());

            return Ok(reassignment(assignment));
        } else {
            if let Some(Token {
                token_type: TokenType::Colon,
                ..
            }) = tokens.get(*position + 1)
            {
                return Err(ParsingError::UnwantedColon);
            }

            parse_expression(tokens, position, symbol_table)?;
            return Ok(Statement {
                node: ASTNode::Expression(Expression::Null),
            });
        }
    }

    if let Some(Token {
        token_type: TokenType::Function,
        ..
    }) = tokens.get(*position)
    {
        return Ok(parse_fn_declaration(tokens, position, symbol_table)?);
    }

    if let Some(Token {
        token_type: TokenType::Return,
        ..
    }) = tokens.get(*position)
    {
        return Ok(parse_return_statement(tokens, position, symbol_table)?);
    }

    Ok(Statement {
        node: ASTNode::Expression(Expression::Null),
    })
}

pub fn ignore_whitespace(tokens: &[Token], position: &mut usize) {
    while let Some(Token {
        token_type: TokenType::Separator,
        ..
    }) = tokens.get(*position)
    {
        *position += 1;
    }
}