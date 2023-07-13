use crate::token::{Token, TokenType};

macro_rules! implement {
    ($name: expr) => {
        VariableImpl { name: $name }
    };
    ($arg1:expr, $arg2:expr) => {{
        let token = $arg2;
        if let Some(value) = $arg1.as_ref().downcast_ref::<f64>() {
            NumberImpl { value, token }
        } else if let Some(value) = $arg1.as_ref().downcast_ref::<String>() {
            StringImpl { value, token }
        } else {
            let target = $arg1;
            let value = $arg2;
            AssignmentImpl { target, value }
        }
    }};
    ($lhs:expr, $operation:expr, $rhs:expr) => {
        BinaryOpImpl {
            lhs: $lhs,
            operation: $operation,
            rhs: $rhs,
        }
    };
}

pub struct AssignmentImpl {
    target: VariableImpl,
    value: Box<Expr>,
}
fn assignment(target: VariableImpl, value: Box<Expr>) -> AssignmentImpl {
    AssignmentImpl { target, value }
}

pub struct VariableImpl {
    name: Token,
}
fn var(name: Token) -> VariableImpl {
    VariableImpl { name }
}

pub struct BinaryOpImpl {
    lhs: Box<Expr>,
    operation: Token,
    rhs: Box<Expr>,
}
fn operation(lhs: Box<Expr>, operation: Token, rhs: Box<Expr>) -> BinaryOpImpl {
    BinaryOpImpl {
        lhs,
        operation,
        rhs,
    }
}

pub struct NumberImpl {
    value: f64,
    token: Token,
}
fn num(value: f64, token: Token) -> NumberImpl {
    NumberImpl { value, token }
}

pub struct StringImpl {
    value: String,
    token: Token,
}
fn str(value: String, token: Token) -> StringImpl {
    StringImpl { value, token }
}

pub enum Expr {
    Assignment(AssignmentImpl),
    BinaryOperation(BinaryOpImpl),
    Variable(VariableImpl),
    Number(NumberImpl),
    String(StringImpl),
}

pub fn parse(mut tokens: Vec<Token>) -> Vec<Expr> {
    tokens.reverse();

    let mut result = Vec::new();

    while tokens.len() > 1 {
        let expr: Expr = parse_expr(&mut tokens);

        expect(TokenType::Separator, &mut tokens);

        result.push(expr)
    }

    result
}

fn parse_expr(tokens: &mut Vec<Token>) -> Expr {
    parse_assignment(tokens)
}
fn parse_assignment(tokens: &mut Vec<Token>) -> Expr {
    let next_token: &Token = &tokens[tokens.len() - 2];
    if next_token.token_type == TokenType::AssignmentOperator {
        let variable: VariableImpl = parse_variable(tokens);
        expect(TokenType::AssignmentOperator, tokens);
        let value: Expr = parse_expr(tokens);

        Expr::Assignment(assignment(variable, Box::new(value)))
    } else {
        todo!()
    }
}
fn parse_variable(tokens: &mut Vec<Token>) -> VariableImpl {
    todo!()
}

fn expect(expected: TokenType, tokens: &mut Vec<Token>) {
    match tokens.pop() {
        None => (),
        Some(token) => {
            if token.token_type != expected {
                panic!("Expected {:?}, found {:?}", expected, token.token_type);
            }
        }
    }
}
