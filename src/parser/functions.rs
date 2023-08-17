pub mod errors;
use std::fmt;

use crate::{
    evaluation::{evaluate_expression, EvaluationError},
    jadeflow_std::StandardFunction,
    parser::{expression::parse_expression, vectors::check_and_insert_expression},
    token::{Token, TokenType},
};

use self::errors::FunctionParsingError;

pub trait RunnableFunction {
    fn run_with_args(
        &self,
        args: &Vec<Expression>,
        symbol_table: &SymbolTable,
    ) -> Result<Expression, EvaluationError>;
}

use super::{
    architecture::{ASTNode, Program, Statement, SymbolTable, SymbolTableError},
    expression::Expression,
    ignore_whitespace, parse,
    types::{err_on_fn_call_args_invalid, type_from_expression, VariableType},
    vars::Declaration,
    ParsingError,
};

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub arguments: Vec<Declaration>,
    pub context: Vec<Statement>,
    pub return_type: Option<VariableType>,
    pub table: SymbolTable,
}
impl Function {
    pub fn new(
        name: &str,
        arguments: Vec<Declaration>,
        context: Vec<Statement>,
        return_type: Option<VariableType>,
        table: SymbolTable,
    ) -> Function {
        Function {
            name: name.to_string(),
            arguments,
            context,
            return_type,
            table,
        }
    }
    pub fn argument(name: &str, var_type: VariableType) -> Declaration {
        Declaration::new(name, var_type, Expression::Null, true, false)
    }

    fn get_returned_expr(&self) -> Expression {
        for statement in self.context.iter() {
            match &statement.node {
                ASTNode::Return { value, .. } => return value.to_owned(),
                _ => (),
            }
        }

        Expression::Null
    }

    pub fn get_returned_type(program: &mut Program) -> Option<VariableType> {
        if let Some(Statement {
            node: ASTNode::Return {
                value: returned, ..
            },
        }) = program.statements.last()
        {
            type_from_expression(returned, &mut program.symbol_table).ok()
        } else {
            None
        }
    }
}

impl RunnableFunction for Function {
    fn run_with_args(
        &self,
        args: &Vec<Expression>,
        symbol_table: &SymbolTable,
    ) -> Result<Expression, EvaluationError> {
        err_on_fn_call_args_invalid(&self.name, &self.arguments, args, symbol_table)?;

        let new_args = self
            .arguments
            .iter()
            .enumerate()
            .map(|(i, arg)| {
                let mut new_arg = arg.clone();
                new_arg.value = args[i].clone();
                new_arg
            })
            .collect::<Vec<Declaration>>();

        let tokens = new_args
            .iter()
            .flat_map(|dec| dec.equivalent_tokens())
            .collect::<Vec<Token>>();

        let tokens_iter = tokens.iter().peekable();

        let fn_parsing = match parse(tokens_iter.clone(), Some(symbol_table.clone())) {
            Ok(r) => r,
            Err(e) => {
                return Err(EvaluationError::Custom {
                    message: e.to_string(),
                })
            }
        };
        let mut program = match fn_parsing {
            ASTNode::Program(p) => p,
            _ => unreachable!(),
        };

        if self.return_type.is_none() {
            return Ok(Expression::Null);
        }

        Ok(evaluate_expression(
            self.get_returned_expr(),
            &mut program.symbol_table,
        )?)
    }
}
impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "fn {}({}) => {}",
            self.name,
            self.arguments
                .iter()
                .map(|arg| format!("{}: {}", arg.name, arg.var_type.as_assignment()))
                .collect::<Vec<String>>()
                .join(", "),
            self.return_type
                .as_ref()
                .map_or("null", |output_t| output_t.as_assignment()),
        )
    }
}

#[derive(Debug, Clone)]
pub struct MainFunctionData {
    pub name: String,
    pub arguments: Vec<Declaration>,
    pub return_type: Option<VariableType>,
    pub is_std: bool,
}
impl MainFunctionData {
    pub fn args_as_tokens(&self) -> Vec<Token> {
        self.arguments
            .iter()
            .flat_map(|dec| dec.equivalent_tokens())
            .collect()
    }
}
impl From<Function> for MainFunctionData {
    fn from(f: Function) -> Self {
        MainFunctionData {
            name: f.name,
            arguments: f.arguments,
            return_type: f.return_type,
            is_std: false,
        }
    }
}
impl From<StandardFunction> for MainFunctionData {
    fn from(f: StandardFunction) -> Self {
        MainFunctionData {
            name: f.name,
            arguments: f.arguments,
            return_type: f.return_type,
            is_std: true,
        }
    }
}
impl fmt::Display for MainFunctionData {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "fn {}({}) => {}",
            self.name,
            self.arguments
                .iter()
                .map(|arg| format!("{}: {}", arg.name, arg.var_type.as_assignment()))
                .collect::<Vec<String>>()
                .join(", "),
            self.return_type
                .as_ref()
                .map_or("null", |output_t| output_t.as_assignment()),
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub function_name: String,
    pub arguments: Vec<Expression>,
}

pub fn function(f: Function) -> Statement {
    Statement {
        node: ASTNode::FunctionDeclaration(f),
    }
}
pub fn function_call(call: FunctionCall) -> Statement {
    Statement {
        node: ASTNode::FunctionCall {
            function_name: call.function_name,
            arguments: call.arguments,
        },
    }
}

pub fn parse_fn_declaration(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &mut SymbolTable,
    ctx: Option<Expression>,
) -> Result<Function, ParsingError> {
    let fn_data = parse_fn_header(tokens, symbol_table)?;

    let is_method = ctx.is_some();

    let function_context = match tokens.next() {
        Some(Token {
            token_type: TokenType::OpenBrace,
            ..
        }) => {
            let mut ctx_tokens = Vec::new();
            ctx_tokens.extend(fn_data.args_as_tokens());
            ctx_tokens.extend(parse_fn_block(tokens, &fn_data.name)?);
            let mut table = symbol_table.clone();
            if is_method {
                table.variables.insert(
                    "ctx".to_owned(),
                    Declaration {
                        name: "ctx".to_owned(),
                        var_type: crate::parser::types::VariableType::Dictionary,
                        value: ctx.unwrap(),
                        is_mutable: false,
                        is_object_prop: false,
                    },
                );
            }
            parse(ctx_tokens.iter().peekable(), Some(table))
        }
        Some(Token {
            token_type: TokenType::FunctionArrow,
            ..
        }) => Ok({
            let mut ctx_tokens: Vec<Token> = Vec::new();

            ctx_tokens.extend(fn_data.args_as_tokens());

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
            let mut table = symbol_table.clone();
            if is_method {
                table.variables.insert(
                    "ctx".to_owned(),
                    Declaration {
                        name: "ctx".to_owned(),
                        var_type: crate::parser::types::VariableType::Dictionary,
                        value: ctx.unwrap(),
                        is_mutable: false,
                        is_object_prop: false,
                    },
                );
            }
            parse(ctx_tokens.iter().peekable(), Some(table))?
        }),
        _ => {
            return Err(FunctionParsingError::ExpectedBrace {
                fn_name: fn_data.name.to_owned(),
            }
            .into());
        }
    };

    let mut program = match function_context? {
        super::architecture::ASTNode::Program(p) => p,
        _ => unreachable!(),
    };

    err_if_fn_type_issue(&fn_data, &mut program)?;

    let function_context = program.statements;

    let f = Function {
        name: fn_data.name.to_owned(),
        arguments: fn_data.arguments,
        context: function_context,
        return_type: fn_data.return_type,
        table: symbol_table.clone(),
    };

    return Ok(f);
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
        } else if symbol_table.is_fn_std(name) {
            return Err(FunctionParsingError::NameAlreadyTakenByStd {
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
                is_std: false,
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
    let fn_data = symbol_table.get_function(function_name)?;
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
        .map(|expr| type_from_expression(expr, symbol_table))
        .collect::<Vec<Result<VariableType, SymbolTableError>>>();

    let required_num = arguments.len();
    let found_num = call_args.len();

    if required_num != found_num {
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

pub fn parse_fn_args(
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

fn err_if_fn_type_issue(
    fn_data: &MainFunctionData,
    program: &mut Program,
) -> Result<(), ParsingError> {
    let found = Function::get_returned_type(program);

    let return_type = &fn_data.return_type;

    if return_type != &found {
        if let Some(return_type) = return_type {
            if let Some(found) = found {
                return Err(FunctionParsingError::ReturnTypeInvalid {
                    fn_name: fn_data.name.to_owned(),
                    return_type: return_type.to_string(),
                    found: found.to_string(),
                }
                .into());
            } else {
                return Err(FunctionParsingError::MissingReturnStatement {
                    fn_name: fn_data.name.to_owned(),
                }
                .into());
            }
        } else {
            return Err(FunctionParsingError::MissingReturnStatement {
                fn_name: fn_data.name.to_owned(),
            }
            .into());
        }
    }

    Ok(())
}
