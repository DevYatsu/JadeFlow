use crate::{parser::expression::parse_expression, token::Token};

use super::{
    architecture::{return_statement, Statement, SymbolTable},
    ParsingError,
};

pub fn parse_return_statement(
    tokens: &[Token],
    position: &mut usize,
    symbol_table: &mut SymbolTable,
) -> Result<Statement, ParsingError> {
    *position += 1;

    let expr = parse_expression(tokens, position, symbol_table)?;

    Ok(return_statement(expr))
}
