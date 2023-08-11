pub mod architecture;
pub mod errors;
mod class;
mod dictionary;
mod expression;
mod functions;
mod returns;
mod types;
mod vars;
mod vectors;

use std::{iter::Peekable, slice::Iter};

use crate::{
    parser::{
        architecture::{program, Program},
        class::{parse_class_header, ClassError},
        functions::parse_fn_header,
    },
    token::{Token, TokenType},
};

use self::{
    architecture::{ASTNode, Statement, SymbolTable},
    class::parse_class_declaration,
    errors::ParsingError,
    functions::{function, function_call, parse_fn_call, parse_fn_declaration},
    returns::parse_return_statement,
    types::TypeError,
    vars::{parse_var_declaration, parse_var_reassignment, reassignment, variable},
};

pub fn parse(
    mut tokens_iter: Peekable<Iter<'_, Token>>,
    optional_symbol_table: Option<&SymbolTable>,
) -> Result<ASTNode, ParsingError> {
    let mut statements = Vec::new();
    let mut symbol_table = if let Some(table) = optional_symbol_table {
        table.clone()
    } else {
        parse_all_fns_dec(tokens_iter.clone())?
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
                        let assignment = if symbol_table.is_object_prop(&token.value) {
                            parse_var_reassignment(
                                &mut tokens_iter,
                                None,
                                &mut symbol_table,
                                &token.value,
                            )?
                        } else {
                            let var = symbol_table.get_variable(&token.value, None)?;
                            if !var.is_mutable {
                                return Err(ParsingError::CannotReassignConst {
                                    var_name: token.value.clone(),
                                });
                            }

                            parse_var_reassignment(
                                &mut tokens_iter,
                                Some(&var),
                                &mut symbol_table,
                                &token.value,
                            )?
                        };

                        symbol_table.reassign_variable(assignment.clone(), &mut tokens_iter)?;
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
                    let f = parse_fn_declaration(&mut tokens_iter, &mut symbol_table)?;
                    symbol_table.insert_function(&f);
                    statements.push(function(f));
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
                TokenType::Class => {
                    let cls = parse_class_declaration(&mut tokens_iter, &mut symbol_table)?;
                    todo!("cls implementation coming soon! {:?}", cls)
                }
                TokenType::If => {
                    todo!("'if' implementation coming soon!")
                }
                TokenType::Else => {
                    todo!("'if' implementation coming soon!")
                }
                TokenType::Match => {
                    todo!("'match' implementation coming soon!")
                }
                TokenType::For => {
                    todo!("'for' implementation coming soon!")
                }
                TokenType::IncrementOperator => {
                    todo!("'++' operator implementation coming soon!")
                }
                TokenType::DecrementOperator => {
                    todo!("'--' operator implementation coming soon!")
                }
                TokenType::While => {
                    todo!("'while' operator implementation coming soon!")
                }
                TokenType::LogicalOperator => {
                    todo!(
                        "'{}' logical operator implementation coming soon!",
                        token.value
                    )
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

pub fn parse_all_fns_dec(
    mut tokens_iter: Peekable<Iter<'_, Token>>,
) -> Result<SymbolTable, ParsingError> {
    let mut symbol_table = SymbolTable::new();

    while let Some(token) = tokens_iter.peek() {
        match token.token_type {
            crate::token::TokenType::Function => {
                tokens_iter.next();
                let f = parse_fn_header(&mut tokens_iter, &mut symbol_table)?;
                symbol_table.register_function(f);
            }
            crate::token::TokenType::Class => {
                tokens_iter.next();
                let cls_header = parse_class_header(&mut tokens_iter, &mut symbol_table)?;
                let mut open_brace_count = 0;

                if let Some(token) = tokens_iter.peek() {
                    if token.token_type == TokenType::OpenBrace {
                        while let Some(token) = tokens_iter.peek() {
                            if token.token_type == TokenType::OpenBrace {
                                open_brace_count += 1;
                            } else if token.token_type == TokenType::CloseBrace {
                                open_brace_count -= 1;
                            }

                            if open_brace_count == 0 {
                                break;
                            }
                            tokens_iter.next();
                        }
                    } else {
                        return Err(ClassError::ExpectedBrace { header: cls_header }.into());
                    }
                } else {
                    return Err(ParsingError::UnexpectedEndOfInput);
                }
            }
            _ => {
                tokens_iter.next();
            }
        }
    }

    Ok(symbol_table)
}
