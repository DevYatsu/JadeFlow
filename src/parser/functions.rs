pub mod errors;
use self::errors::FunctionParsingError;
use super::{
    architecture::{ASTNode, Program, Statement, SymbolTable, SymbolTableError},
    expression::Expression,
    ignore_whitespace, parse,
    types::{err_on_fn_call_args_invalid, type_from_expression, VariableType},
    vars::Declaration,
    ParsingError,
};
use crate::{
    evaluation::{evaluate_expression, evaluate_program, EvaluationError},
    jadeflow_std::load_std,
    parser::{expression::parse_expression, vectors::check_and_insert_expression},
    token::{tokenize, Token, TokenType},
};
use hashbrown::HashMap;
use once_cell::sync::OnceCell;
use std::fmt;

pub enum Function {
    DefinedFunction {
        name: String,
        arguments: Vec<Argument>,
        context: Vec<Statement>,
        return_type: Option<VariableType>,
        table: SymbolTable,
    },
    StandardFunction {
        name: String,
        arguments: Vec<Argument>,
        return_type: Option<VariableType>,
        code_to_run: OnceCell<Box<dyn Fn(Vec<Expression>) -> Expression>>,
    },
}
impl Clone for Function {
    fn clone(&self) -> Self {
        match self {
            Function::DefinedFunction {
                name,
                arguments,
                context,
                return_type,
                table,
            } => Function::DefinedFunction {
                name: name.clone(),
                arguments: arguments.clone(),
                context: context.clone(),
                return_type: return_type.clone(),
                table: table.clone(),
            },
            Function::StandardFunction {
                name,
                arguments,
                return_type,
                ..
            } => Function::StandardFunction {
                name: name.clone(),
                arguments: arguments.clone(),
                return_type: return_type.clone(),
                code_to_run: match load_std().remove(name).unwrap() {
                    Function::DefinedFunction { .. } => unreachable!(),
                    Function::StandardFunction { code_to_run, .. } => code_to_run,
                },
            },
        }
    }
}
impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Function::DefinedFunction {
                name,
                arguments,
                return_type,
                ..
            } => write!(
                f,
                "StandardFunction {{ name: {}, arguments: {:?}, return_type: {:?} }}",
                name, arguments, return_type
            ),
            Function::StandardFunction {
                name,
                arguments,
                return_type,
                ..
            } => write!(
                f,
                "StandardFunction {{ name: {}, arguments: {:?}, return_type: {:?} }}",
                name, arguments, return_type
            ),
        }
    }
}

#[macro_export]
macro_rules! function {
    (
        $name:expr,
        arguments: $arguments:expr,
        context: $context:expr,
        return_type: $return_type:expr,
        table: $table:expr
    ) => {
        Function::DefinedFunction {
            name: $name.to_string(),
            arguments: $arguments,
            context: $context,
            return_type: $return_type,
            table: $table,
        }
    };
    (
        $name:expr,
        arguments: $arguments:expr,
        return_type: $return_type:expr,
        code: $code:expr
    ) => {{
        let boxed_closure: Box<dyn Fn(Vec<Expression>) -> Expression> = Box::new($code);
        let once_cell = OnceCell::from(boxed_closure);

        Function::StandardFunction {
            name: $name.to_string(),
            arguments: $arguments,
            return_type: $return_type,
            code_to_run: once_cell,
        }
    }};
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: String,
    pub var_type: VariableType,
    pub is_mutable: bool,
}
impl Argument {
    pub fn new(name: String, var_type: VariableType, is_mutable: bool) -> Argument {
        Argument {
            name,
            var_type,
            is_mutable,
        }
    }
    pub fn equivalent_tokens(&self) -> Vec<Token> {
        let keyword = if self.is_mutable { "mut" } else { "const" };
        let value = match self.var_type {
            VariableType::String => Expression::String("".to_owned()),
            VariableType::Number => Expression::Number(0.0),
            VariableType::Boolean => Expression::Boolean(false),
            VariableType::Vector => Expression::ArrayExpression(Vec::new()),
            VariableType::Dictionary => Expression::DictionaryExpression(HashMap::new()),
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
}

impl Function {
    pub fn run_with_args(
        self,
        args: &Vec<Expression>,
        symbol_table: &SymbolTable,
    ) -> Result<Expression, EvaluationError> {
        match self {
            Function::DefinedFunction {
                name,
                arguments,
                return_type,
                context: statements,
                ..
            } => {
                err_on_fn_call_args_invalid(&name, &arguments, args, symbol_table)?;
                let mut table = symbol_table.clone();

                arguments.iter().enumerate().for_each(|(i, arg)| {
                    let mut new_arg = Declaration::from(arg.clone());
                    new_arg.value = args[i].clone();
                    table.insert_variable(new_arg)
                });

                let returned_expr = Function::get_returned_expr(&statements);

                let mut table = evaluate_program(Program {
                    statements,
                    symbol_table: table,
                })?;

                if return_type.is_none() {
                    return Ok(Expression::Null);
                }

                Ok(evaluate_expression(returned_expr, &mut table)?)
            }
            Function::StandardFunction {
                name,
                arguments,
                return_type,
                code_to_run,
            } => {
                err_on_fn_call_args_invalid(&name, &arguments, args, symbol_table)?;
                let std_func = code_to_run.get().expect("Code not initialized");

                let returned_expr = std_func(
                    args.to_owned()
                        .into_iter()
                        .map(|arg| -> Result<Expression, EvaluationError> {
                            evaluate_expression(arg.clone(), symbol_table)
                        })
                        .collect::<Result<Vec<Expression>, EvaluationError>>()?,
                );
                if return_type.is_none() {
                    return Ok(Expression::Null);
                }

                Ok(evaluate_expression(returned_expr, symbol_table)?)
            }
        }
    }

    fn get_returned_expr(statements: &Vec<Statement>) -> Expression {
        for statement in statements.iter() {
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

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Function::DefinedFunction {
                name,
                arguments,
                return_type,
                ..
            } => write!(
                f,
                "fn {}({}) => {}",
                name,
                arguments
                    .iter()
                    .map(|arg| format!("{}: {}", arg.name, arg.var_type.as_assignment()))
                    .collect::<Vec<String>>()
                    .join(", "),
                return_type
                    .as_ref()
                    .map_or("null", |output_t| output_t.as_assignment()),
            ),
            Function::StandardFunction {
                name,
                arguments,
                return_type,
                ..
            } => write!(
                f,
                "fn {}({}) => {}",
                name,
                arguments
                    .iter()
                    .map(|arg| format!("{}: {}", arg.name, arg.var_type.as_assignment()))
                    .collect::<Vec<String>>()
                    .join(", "),
                return_type
                    .as_ref()
                    .map_or("null", |output_t| output_t.as_assignment()),
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub struct MainFunctionData {
    pub name: String,
    pub arguments: Vec<Argument>,
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
        match f {
            Function::DefinedFunction {
                name,
                arguments,
                return_type,
                ..
            } => MainFunctionData {
                name,
                arguments,
                return_type,
                is_std: false,
            },
            Function::StandardFunction {
                name,
                arguments,
                return_type,
                ..
            } => MainFunctionData {
                name,
                arguments,
                return_type,
                is_std: true,
            },
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
    let arguments = fn_data.clone().arguments;

    let is_method = ctx.is_some();

    let function_context = match tokens.next() {
        Some(Token {
            token_type: TokenType::OpenBrace,
            ..
        }) => {
            let mut ctx_tokens = Vec::new();
            ctx_tokens.extend(parse_fn_block(tokens, &fn_data.name)?);
            let mut table = symbol_table.clone();
            fn_data.arguments.iter().for_each(|arg| {
                table.insert_variable(arg.clone().into());
            });
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
    let f = function!(
        fn_data.name.to_owned(),
        arguments: arguments,
        context: function_context,
        return_type: fn_data.return_type,
        table: program.symbol_table
    );

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

        if symbol_table.is_fn_std(name) {
            return Err(FunctionParsingError::NameAlreadyTakenByStd {
                name: name.to_owned(),
            }
            .into());
        } else if symbol_table.is_fn_declared(name) {
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
) -> Result<Vec<Argument>, FunctionParsingError> {
    let mut arguments: Vec<Argument> = Vec::new();

    while let Some(mut initial_token) = tokens.next() {
        if initial_token.token_type == TokenType::CloseParen {
            break;
        }
        let is_mutable = if &initial_token.value == "mut" {
            initial_token = tokens.next().ok_or_else(|| FunctionParsingError::Custom {
                msg: ParsingError::UnexpectedEndOfInput.to_string(),
            })?;
            true
        } else {
            false
        };
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
                    arguments.push(Argument {
                        name: initial_token.value.to_owned(),
                        var_type: arg_type.clone(),
                        is_mutable,
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
