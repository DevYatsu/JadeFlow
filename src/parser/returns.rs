use crate::{parser::expression::parse_expression, token::Token};

use super::{
    architecture::{ASTNode, Statement, SymbolTable},
    expression::Expression,
    ParsingError,
};

pub fn return_statement(expr: Expression) -> Statement {
    Statement {
        node: ASTNode::Return(expr),
    }
}

pub fn parse_return_statement(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &mut SymbolTable,
) -> Result<Statement, ParsingError> {
    let expr = parse_expression(tokens, symbol_table)?;

    Ok(return_statement(expr))
}
