use crate::{parser::expression::parse_expression, token::Token};

use super::{
    architecture::{ASTNode, Statement, SymbolTable},
    expression::Expression,
    ParsingError,
};

pub fn return_statement(value: Expression, keyword: String) -> Statement {
    Statement {
        node: ASTNode::Return { value, keyword },
    }
}

pub fn parse_return_statement(
    keyword: String,
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &mut SymbolTable,
) -> Result<Statement, ParsingError> {
    let expr = parse_expression(tokens, symbol_table)?;

    Ok(return_statement(expr, keyword))
}
