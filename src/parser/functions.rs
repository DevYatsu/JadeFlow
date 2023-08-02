use crate::{
    parser::{
        architecture::{function, program},
        parse,
    },
    token::{Token, TokenType},
};

use super::{
    architecture::{
        ASTNode, Declaration, Expression, Function, MainFunctionData, Program, Statement,
        SymbolTable, VariableType,
    },
    types::type_from_expression,
    ParsingError,
};

use custom_error::custom_error;

custom_error! {pub FunctionParsingError
    ExpectedIdentifier = "Expected a function name after the 'fn' keyword",
    ExpectedOpenParen{name: String} = "Expected parentheses after '{name}' in the function declaration",

    ExpectedArgName = "Expected a valid function argument name",
    ExpectedArgColon{arg_name: String} = "Expected a type declaration after '{arg_name}'",
    ExpectedArgType{arg_name: String} = "Missing type declaration for argument '{arg_name}'",
    ExpectedCommaBetweenArgs{arg_name: String, arg_type: String, fn_name: String} = "Expected ',' after '{arg_name}: {arg_type}' in the '{fn_name}' function arguments",
    ExpectedCloseParen{arg_name: String, arg_type: String, fn_name: String} = "Expected ')' after '{arg_name}: {arg_type}' in the '{fn_name}' function arguments",

    ExpectedReturnType{fn_name: String} = "Expected a return type for the '{fn_name}' function",
    ExpectedValidReturnType{fn_name: String} = "Expected a valid return type for the '{fn_name}' function",

    ExpectedBrace{fn_name: String} = "Expected the function body after the function arguments in the '{fn_name}' function",

    NameAlreadyTaken{name: String} = "The function name '{name}' is already in use",

    ExpectingExpressionAfterArrow{fn_name: String} = "Expecting expression after '=>' in \"{fn_name}\" declaration",

    MissingReturnType{fn_name: String} = "Missing a return type in \"{fn_name}\" function declaration",
    MissingReturnStatement{fn_name: String} = "Missing a return statement in \"{fn_name}\" function declaration",
    ReturnTypeInvalid{fn_name: String, return_type: String, found: String} = "Return Type '{return_type}' of function \"{fn_name}\" does not correspond to returned type '{found}'",

    NotDefinedFunction{fn_name: String} = "\"{fn_name}\" does not correspond to any defined function",
}
// update to support return statement in parse
pub fn parse_fn_declaration(
    tokens: &[Token],
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

        if symbol_table.get_function(name).is_ok() {
            return Err(FunctionParsingError::NameAlreadyTaken {
                name: name.to_owned(),
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

            let mut return_type: Option<VariableType> = None;

            // Check if the next token is a colon
            // determine function output type
            // if no colon -> no output
            if let Some(Token {
                token_type: TokenType::Colon,
                ..
            }) = tokens.get(*position)
            {
                *position += 1;

                return_type = parse_return_type(tokens, position, &name)?;
            }

            let function_context = match tokens.get(*position) {
                Some(Token {
                    token_type: TokenType::OpenBrace,
                    ..
                }) => {
                    *position += 1;
                    let mut ctx_tokens: Vec<Token> = arguments
                        .iter()
                        .flat_map(|dec| dec.equivalent_tokens())
                        .collect();

                    ctx_tokens.extend(parse_fn_block(tokens, position, &name)?);

                    parse(&mut ctx_tokens)?
                }
                Some(Token {
                    token_type: TokenType::FunctionArrow,
                    ..
                }) => {
                    *position += 1;
                    let mut ctx_tokens: Vec<Token> = arguments
                        .iter()
                        .flat_map(|dec| dec.equivalent_tokens())
                        .collect();

                    ctx_tokens.push(Token {
                        value: "return".to_owned(),
                        token_type: TokenType::Return,
                    });
                    let before_expr_length = ctx_tokens.len();

                    while let Some(token) = tokens.get(*position) {
                        match token.token_type {
                            TokenType::Separator => break,
                            _ => (),
                        }
                        ctx_tokens.push(token.clone());
                        *position += 1;
                    }

                    if ctx_tokens.len() == before_expr_length {
                        return Err(FunctionParsingError::ExpectingExpressionAfterArrow {
                            fn_name: name.to_owned(),
                        }
                        .into());
                    }

                    parse(&mut ctx_tokens)?
                }
                _ => {
                    return Err(FunctionParsingError::ExpectedBrace {
                        fn_name: name.to_owned(),
                    }
                    .into());
                }
            };

            let function_context = keep_useful_content(function_context);

            let returned_type = returned_type(&function_context);

            if return_type != returned_type {
                if let Some(return_type) = return_type {
                    if let Some(returned_type) = returned_type {
                        return Err(FunctionParsingError::ReturnTypeInvalid {
                            fn_name: name.to_owned(),
                            return_type: return_type.to_string(),
                            found: returned_type.to_string(),
                        }
                        .into());
                    } else {
                        return Err(FunctionParsingError::MissingReturnStatement {
                            fn_name: name.to_owned(),
                        }
                        .into());
                    }
                } else {
                    return Err(FunctionParsingError::MissingReturnStatement {
                        fn_name: name.to_owned(),
                    }
                    .into());
                }
            }

            let f = Function {
                name: name.to_owned(),
                arguments,
                context: Box::new(program(function_context)),
                return_type,
            };

            symbol_table.insert_function(&f);

            return Ok(function(f));
        } else {
            return Err(FunctionParsingError::ExpectedOpenParen {
                name: name.to_owned(),
            }
            .into());
        }
    } else {
        return Err(FunctionParsingError::ExpectedIdentifier.into());
    }
}

pub fn parse_fn_call(
    tokens: &[Token],
    position: &mut usize,
    symbol_table: &SymbolTable,
    f: &MainFunctionData,
) -> Result<Expression, FunctionParsingError> {
    todo!()
}

fn parse_fn_block(
    tokens: &[Token],
    position: &mut usize,
    fn_name: &str,
) -> Result<Vec<Token>, ParsingError> {
    let mut ctx_tokens: Vec<Token> = Vec::new();
    let mut brace_count = 1;

    while brace_count > 0 {
        if let Some(token) = tokens.get(*position) {
            match token.token_type {
                TokenType::OpenBrace => brace_count += 1,
                TokenType::CloseBrace => brace_count -= 1,
                _ => (),
            }
            ctx_tokens.push(token.clone());
            *position += 1;
        } else {
            return Err(FunctionParsingError::ExpectedBrace {
                fn_name: fn_name.to_owned(), // You can customize the error message here.
            }
            .into());
        }
    }

    Ok(ctx_tokens)
}

fn keep_useful_content(function_context: ASTNode) -> Program {
    match function_context {
        ASTNode::Program(Program {
            statements,
            symbol_table,
        }) => {
            if let Some(return_index) = statements
                .iter()
                .position(|s| matches!(s.node, ASTNode::Return(_)))
            {
                Program {
                    statements: statements[0..=return_index].to_vec(),
                    symbol_table,
                }
            } else {
                Program {
                    statements,
                    symbol_table,
                }
            }
        }
        _ => unreachable!(),
    }
}

fn returned_type(program: &Program) -> Option<VariableType> {
    if let Some(Statement {
        node: ASTNode::Return(returned),
    }) = program.statements.last()
    {
        type_from_expression(returned, &program.symbol_table).ok()
    } else {
        None
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
                                arg_name: initial_token.value.to_owned(),
                            })
                        }
                    };
                    arguments.push(Declaration {
                        name: initial_token.value.to_owned(),
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
                    } else if let Some(Token {
                        token_type: TokenType::Identifier,
                        ..
                    }) = tokens.get(*position)
                    {
                        return Err(FunctionParsingError::ExpectedCommaBetweenArgs {
                            arg_name: initial_token.value.to_owned(),
                            arg_type: arg_type.as_assignment().to_owned(),
                            fn_name: initial_token.value.to_owned(),
                        });
                    } else {
                        return Err(FunctionParsingError::ExpectedCloseParen {
                            arg_name: initial_token.value.to_owned(),
                            arg_type: arg_type.as_assignment().to_owned(),
                            fn_name: initial_token.value.to_owned(),
                        });
                    }
                } else {
                    return Err(FunctionParsingError::ExpectedArgType {
                        arg_name: initial_token.value.to_owned(),
                    });
                }
            } else {
                return Err(FunctionParsingError::ExpectedArgColon {
                    arg_name: initial_token.value.to_owned(),
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
                    fn_name: fn_name.to_owned(),
                })
            }
        }
    } else {
        return Err(FunctionParsingError::ExpectedReturnType {
            fn_name: fn_name.to_owned(),
        });
    }
}