use crate::{
    parser::{architecture::function, parse},
    token::{Token, TokenType},
};

use super::{
    architecture::{Declaration, Expression, Function, Statement, SymbolTable, VariableType},
    ParsingError,
};

use custom_error::custom_error;

custom_error! {pub FunctionParsingError
    ExpectedIdentifier = "Expected function name after 'fn' keyword",
    ExpectedParen{name: String} = "Expected parenthesis after {name}",

    ExpectedArgName = "Expected a valid function argument name",
    ExpectedArgColon{arg_name: String} = "Expected '{arg_name}: type'",
    ExpectedArgType{arg_name: String} = "Missing type for argument '{arg_name}'",
    ExpectedCommaBetweenArgs{arg_name: String, arg_type: String} = "Expected ',' after '{arg_name}: {arg_type}'",

    ExpectedReturnType{fn_name: String} = "Expected {fn_name} function return type",
    ExpectedValidReturnType{fn_name: String} = "Expected VALID {fn_name} function return type",

    ExpectedBrace{fn_name: String} = "Expected {fn_name} function context after arguments",

    NameAlreadyTaken{name: String} = "Function {name} is already used",
}
// update to support return statement in parse
pub fn parse_fn_declaration(
    tokens: &mut Vec<Token>,
    position: &mut usize,
    symbol_table: &mut SymbolTable,
) -> Result<Statement, ParsingError> {
    *position += 1;

    // Expect an identifier token (function name)
    if let Some(Token {
        token_type: TokenType::Identifier,
        value,
    }) = tokens.get(*position)
    {
        *position += 1;
        let name = value;

        if let Some(_) = symbol_table.get_function(name) {
            return Err(FunctionParsingError::NameAlreadyTaken {
                name: name.to_string(),
            }
            .into());
        }

        // Check if the next token is an open parenthesis
        if let Some(Token {
            token_type: TokenType::OpenParen,
            ..
        }) = tokens.get(*position)
        {
            *position += 1;
            let arguments = parse_args(tokens, position)?;
            println!("args {:?}", arguments);

            // Check if the next token is a colon
            // determine function output type
            // if no colon -> no output
            if let Some(Token {
                token_type: TokenType::Colon,
                ..
            }) = tokens.get(*position)
            {
                *position += 1;

                let return_type: Option<VariableType> = parse_return_type(tokens, position, &name)?;

                if let Some(Token {
                    token_type: TokenType::OpenBrace,
                    ..
                }) = tokens.get(*position)
                {
                    *position += 1;

                    let mut ctx_tokens: Vec<Token> = vec![];
                    arguments
                        .iter()
                        .for_each(|dec| ctx_tokens.extend(dec.equivalent_tokens()));

                    println!("{:?}", ctx_tokens);

                    let mut brace_count = 1;

                    while let Some(token) = tokens.get(*position) {
                        match token.token_type {
                            TokenType::OpenBrace => brace_count += 1,
                            TokenType::CloseBrace => {
                                brace_count -= 1;
                                if brace_count == 0 {
                                    *position += 1;
                                    break; // End of function context, exit the loop
                                }
                            }
                            _ => (),
                        }
                        ctx_tokens.push(token.clone());
                        *position += 1;
                    }

                    // still need to add the args as declarations at the start of the program
                    let function_context = parse(&mut ctx_tokens)?;
                    println!("ctx {:?}", function_context);
                    let f = Function {
                        name: name.to_string(),
                        arguments,
                        context: Box::new(function_context),
                        return_type,
                    };

                    symbol_table.insert_function(&f);

                    return Ok(function(f));
                } else {
                    return Err(FunctionParsingError::ExpectedBrace {
                        fn_name: name.to_string(),
                    }
                    .into());
                }
            } else {
                let return_type: Option<VariableType> = None;

                if let Some(Token {
                    token_type: TokenType::OpenBrace,
                    ..
                }) = tokens.get(*position)
                {
                    let mut ctx_tokens = Vec::new();
                    let mut brace_count = 1;

                    while let Some(token) = tokens.get(*position) {
                        match token.token_type {
                            TokenType::OpenBrace => brace_count += 1,
                            TokenType::CloseBrace => {
                                brace_count -= 1;
                                if brace_count == 0 {
                                    *position += 1;
                                    break; // End of function context, exit the loop
                                }
                            }
                            _ => (),
                        }
                        ctx_tokens.push(token.clone());
                        *position += 1;
                    }

                    // still need to add the args as declarations at the start of the program
                    let function_context = parse(&mut ctx_tokens)?;
                    let f = Function {
                        name: name.to_string(),
                        arguments,
                        context: Box::new(function_context),
                        return_type,
                    };

                    symbol_table.insert_function(&f);

                    return Ok(function(f));
                } else {
                    return Err(FunctionParsingError::ExpectedBrace {
                        fn_name: name.to_string(),
                    }
                    .into());
                }
            }
        } else {
            return Err(FunctionParsingError::ExpectedParen {
                name: name.to_string(),
            }
            .into());
        }
    } else {
        return Err(FunctionParsingError::ExpectedIdentifier.into());
    }
}

fn parse_args(
    tokens: &[Token],
    position: &mut usize,
) -> Result<Vec<Declaration>, FunctionParsingError> {
    let mut arguments: Vec<Declaration> = Vec::new();

    while let Some(initial_token) = tokens.get(*position) {
        if initial_token.token_type == TokenType::CloseParen {
            *position += 1;
            break;
        }

        if initial_token.token_type == TokenType::Identifier {
            *position += 1;

            if let Some(Token {
                token_type: TokenType::Colon,
                ..
            }) = tokens.get(*position)
            {
                *position += 1;

                if let Some(Token { value, .. }) = tokens.get(*position) {
                    *position += 1;

                    let arg_type = match VariableType::from_assignment(&value) {
                        Some(t) => t,
                        None => {
                            return Err(FunctionParsingError::ExpectedArgType {
                                arg_name: initial_token.value.to_string(),
                            })
                        }
                    };
                    arguments.push(Declaration {
                        name: initial_token.value.to_string(),
                        var_type: arg_type.clone(),
                        value: Expression::Null,
                        is_mutable: true,
                    });

                    if let Some(Token {
                        token_type: TokenType::Comma,
                        ..
                    }) = tokens.get(*position)
                    {
                        *position += 1;
                    } else if let Some(Token {
                        token_type: TokenType::CloseParen,
                        ..
                    }) = tokens.get(*position)
                    {
                        *position += 1;
                        break;
                    } else {
                        return Err(FunctionParsingError::ExpectedCommaBetweenArgs {
                            arg_name: initial_token.value.to_string(),
                            arg_type: arg_type.to_string(),
                        });
                    }
                } else {
                    return Err(FunctionParsingError::ExpectedArgType {
                        arg_name: initial_token.value.to_string(),
                    });
                }
            } else {
                return Err(FunctionParsingError::ExpectedArgColon {
                    arg_name: initial_token.value.to_string(),
                });
            }
        } else {
            return Err(FunctionParsingError::ExpectedArgName);
        }
    }

    Ok(arguments)
}

fn parse_return_type(
    tokens: &[Token],
    position: &mut usize,
    fn_name: &str,
) -> Result<Option<VariableType>, FunctionParsingError> {
    if let Some(Token { value, .. }) = tokens.get(*position) {
        *position += 1;
        match VariableType::from_assignment(&value) {
            Some(t) => Ok(Some(t)),
            None => {
                return Err(FunctionParsingError::ExpectedValidReturnType {
                    fn_name: fn_name.to_string(),
                })
            }
        }
    } else {
        return Err(FunctionParsingError::ExpectedReturnType {
            fn_name: fn_name.to_string(),
        });
    }
}
