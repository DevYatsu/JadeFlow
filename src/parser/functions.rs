use crate::{
    parser::{
        architecture::{function, MainFunctionData},
        expression::parse_expression,
        types::TypeError,
        vectors::check_and_insert_expression,
    },
    token::{Token, TokenType},
};

use super::{
    architecture::{
        Declaration, Expression, Function, FunctionCall, Statement, SymbolTable, VariableType,
    },
    ignore_whitespace,
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

    MissingClosingParenInFnCall{fn_name: String} = "Missing a closing parenthesis in \"{fn_name}\" call",
    InvalidFnCallArg{fn_name: String, err: String} = "Invalid \"{fn_name}\" call arguments: {err}",
    InvalidFnCallArgType{fn_name: String, arg_name: String, required_t: VariableType, found_t: VariableType} = "In \"{fn_name}\" call, '{arg_name}' must be of type {required_t} whereas a value of type {found_t} was provided!",
    InvalidFnCallArgNumber{fn_name: String, required_num: usize, found_num: usize} = "\"{fn_name}\" requires {required_num} arguments whereas {found_num} were provided!",
}

pub fn parse_fn_declaration(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &mut SymbolTable,
) -> Result<Statement, ParsingError> {
    let fn_data = parse_fn_header(tokens, symbol_table)?;

    let function_context = match tokens.next() {
        Some(Token {
            token_type: TokenType::OpenBrace,
            ..
        }) => parse_fn_block(tokens, &fn_data.name)?,
        Some(Token {
            token_type: TokenType::FunctionArrow,
            ..
        }) => {
            let mut ctx_tokens: Vec<Token> = Vec::new();

            ctx_tokens.push(Token {
                value: "return".to_owned(),
                token_type: TokenType::Return,
            });
            let before_expr_length = ctx_tokens.len();

            while let Some(token) = tokens.next() {
                match token.token_type {
                    TokenType::Separator => break,
                    _ => (),
                }
                ctx_tokens.push(token.clone());
            }

            if ctx_tokens.len() == before_expr_length {
                return Err(FunctionParsingError::ExpectingExpressionAfterArrow {
                    fn_name: fn_data.name.to_owned(),
                }
                .into());
            }

            ctx_tokens
        }
        _ => {
            return Err(FunctionParsingError::ExpectedBrace {
                fn_name: fn_data.name.to_owned(),
            }
            .into());
        }
    };

    let f = Function {
        name: fn_data.name.to_owned(),
        arguments: fn_data.arguments,
        context: function_context,
        return_type: fn_data.return_type,
    };

    symbol_table.insert_function(&f);

    return Ok(function(f));
}

pub fn parse_fn_header(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &mut SymbolTable,
) -> Result<MainFunctionData, ParsingError> {
    // Expect an identifier token (function name)
    if let Some(Token {
        token_type: TokenType::Identifier,
        value,
    }) = tokens.next()
    {
        let name = value;

        if symbol_table.is_fn_declared(name) {
            return Err(FunctionParsingError::NameAlreadyTaken {
                name: name.to_owned(),
            }
            .into());
        }

        // Check if the next token is an open parenthesis
        if let Some(Token {
            token_type: TokenType::OpenParen,
            ..
        }) = tokens.next()
        {
            let arguments = parse_fn_args(tokens)?;

            let mut return_type: Option<VariableType> = None;

            // Check if the next token is a colon
            // determine function output type
            // if no colon -> no output

            ignore_whitespace(tokens);

            if let Some(Token {
                token_type: TokenType::Colon,
                ..
            }) = tokens.peek()
            {
                tokens.next();
                return_type = parse_return_type(tokens, &name)?;
            }
            ignore_whitespace(tokens);

            let fn_data = MainFunctionData {
                name: name.to_owned(),
                arguments,
                return_type,
            };

            return Ok(fn_data);
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
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    function_name: &str,
    symbol_table: &mut SymbolTable,
) -> Result<FunctionCall, ParsingError> {
    // jump the '('
    tokens.next();
    println!("st: \n {}", symbol_table);
    println!("t: {:?}", tokens);
    let fn_data = symbol_table.get_function(function_name, tokens)?;
    let arguments = fn_data.arguments;

    if arguments.len() == 0 {
        if let Some(token) = tokens.next() {
            match token.token_type {
                TokenType::CloseParen => {
                    return Ok(FunctionCall {
                        function_name: function_name.to_owned(),
                        arguments: vec![],
                    })
                }
                _ => {
                    return Err(FunctionParsingError::MissingClosingParenInFnCall {
                        fn_name: function_name.to_owned(),
                    }
                    .into())
                }
            }
        } else {
            // unexpected end of input
            return Err(FunctionParsingError::MissingClosingParenInFnCall {
                fn_name: function_name.to_owned(),
            }
            .into());
        }
    }

    let call_args: Vec<Expression> = parse_call_args(tokens, function_name, symbol_table)?;
    let args_types = call_args
        .iter()
        .map(|expr| type_from_expression(expr, symbol_table, Some(tokens)))
        .collect::<Vec<Result<VariableType, TypeError>>>();

    let required_num = arguments.len();
    let found_num = call_args.len();

    if required_num != required_num {
        return Err(FunctionParsingError::InvalidFnCallArgNumber {
            fn_name: function_name.to_string(),
            required_num,
            found_num,
        }
        .into());
    }

    for (i, arg) in args_types.iter().enumerate() {
        match arg {
            Ok(t) => {
                let required_t = &arguments[i].var_type;
                if t != required_t {
                    return Err(FunctionParsingError::InvalidFnCallArgType {
                        fn_name: function_name.to_owned(),
                        arg_name: arguments[i].name.to_owned(),
                        found_t: t.clone(),
                        required_t: required_t.clone(),
                    }
                    .into());
                }
            }
            Err(e) => {
                return Err(FunctionParsingError::InvalidFnCallArg {
                    fn_name: function_name.to_owned(),
                    err: e.to_string(),
                }
                .into())
            }
        }
    }

    Ok(FunctionCall {
        function_name: function_name.to_owned(),
        arguments: call_args,
    })
}

fn parse_call_args(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    fn_name: &str,
    symbol_table: &mut SymbolTable,
) -> Result<Vec<Expression>, FunctionParsingError> {
    let mut args = Vec::new();
    ignore_whitespace(tokens);

    while let Some(token) = tokens.peek() {
        match token.token_type {
            TokenType::CloseParen => {
                tokens.next();
                break;
            }
            TokenType::Comma | TokenType::Separator => {
                tokens.next();
                continue;
            }
            _ => {
                let value = match parse_expression(tokens, symbol_table) {
                    Ok(v) => v,
                    Err(e) => {
                        return Err(FunctionParsingError::InvalidFnCallArg {
                            fn_name: fn_name.to_owned(),
                            err: e.to_string(),
                        })
                    }
                };

                match check_and_insert_expression(value, symbol_table, &mut args) {
                    Ok(_) => (),
                    Err(e) => {
                        return Err(FunctionParsingError::InvalidFnCallArg {
                            fn_name: fn_name.to_owned(),
                            err: e.to_string(),
                        })
                    }
                };
            }
        }
    }

    Ok(args)
}

fn parse_fn_block(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    fn_name: &str,
) -> Result<Vec<Token>, ParsingError> {
    let mut ctx_tokens: Vec<Token> = Vec::new();
    let mut brace_count = 1;

    while brace_count > 0 {
        if let Some(token) = tokens.next() {
            match token.token_type {
                TokenType::OpenBrace => brace_count += 1,
                TokenType::CloseBrace => {
                    brace_count -= 1;
                    if brace_count == 0 {
                        break;
                    }
                }
                _ => (),
            }
            ctx_tokens.push(token.clone());
        } else {
            return Err(FunctionParsingError::ExpectedBrace {
                fn_name: fn_name.to_owned(), // You can customize the error message here.
            }
            .into());
        }
    }

    Ok(ctx_tokens)
}

fn parse_fn_args(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
) -> Result<Vec<Declaration>, FunctionParsingError> {
    let mut arguments: Vec<Declaration> = Vec::new();

    while let Some(initial_token) = tokens.next() {
        if initial_token.token_type == TokenType::CloseParen {
            break;
        }

        if initial_token.token_type == TokenType::Identifier {
            if let Some(Token {
                token_type: TokenType::Colon,
                ..
            }) = tokens.next()
            {
                if let Some(Token { value, .. }) = tokens.next() {
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
                        is_object_prop: false,
                    });

                    let next = tokens.next();
                    if let Some(Token {
                        token_type: TokenType::Comma,
                        ..
                    }) = next
                    {
                        continue;
                    } else if let Some(Token {
                        token_type: TokenType::CloseParen,
                        ..
                    }) = next
                    {
                        break;
                    } else if let Some(Token {
                        token_type: TokenType::Identifier,
                        ..
                    }) = next
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
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    fn_name: &str,
) -> Result<Option<VariableType>, FunctionParsingError> {
    if let Some(Token { value, .. }) = tokens.next() {
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
