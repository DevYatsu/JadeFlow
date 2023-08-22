use super::{
    architecture::SymbolTable,
    expression::{parse_expression, Expression},
    ignore_whitespace, ParsingError,
};
use crate::token::{Token, TokenType};

pub fn parse_array_expression(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    symbol_table: &mut SymbolTable,
) -> Result<Expression, ParsingError> {
    let mut vec_expressions = Vec::new();
    ignore_whitespace(tokens);

    while let Some(token) = tokens.peek() {
        match token.token_type {
            TokenType::CloseBracket => {
                tokens.next();

                if let Some(Token {
                    token_type: TokenType::OpenBracket,
                    ..
                }) = tokens.peek()
                {
                    tokens.next();
                    return Ok(parse_array_indexing(
                        tokens,
                        vec![Expression::ArrayExpression(vec_expressions)],
                        symbol_table,
                    )?);
                } else {
                    break;
                }
            }
            TokenType::Comma | TokenType::Separator => {
                tokens.next();
                continue; // still an issue if there is an expr, then two /n and an expr again
            }
            _ => {
                let value = parse_expression(tokens, symbol_table)?;

                check_and_insert_expression(value, symbol_table, &mut vec_expressions)?;
            }
        }
    }

    Ok(Expression::ArrayExpression(vec_expressions))
}

pub fn check_and_insert_expression(
    expression: Expression,
    symbol_table: &mut SymbolTable,
    vec_expressions: &mut Vec<Expression>,
) -> Result<(), ParsingError> {
    match &expression {
        Expression::Variable(name) => {
            symbol_table.get_variable(name)?; // error if var not defined
        }
        _ => (),
    }
    vec_expressions.push(expression);
    Ok(())
}

pub fn parse_array_indexing(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    indexed_expr: Vec<Expression>,
    symbol_table: &mut SymbolTable,
) -> Result<Expression, ParsingError> {
    let mut expressions_vec = Vec::from(indexed_expr);
    expressions_vec.push(parse_expression(tokens, symbol_table)?);
    ignore_whitespace(tokens);

    if let Some(token) = tokens.next() {
        if token.token_type == TokenType::CloseBracket {
            while let Some(t) = tokens.peek() {
                match t.token_type {
                    TokenType::OpenBracket => {
                        tokens.next();
                        return Ok(parse_array_indexing(tokens, expressions_vec, symbol_table)?);
                    }
                    _ => break,
                }
            }
            return Ok(Expression::ArrayIndexing(expressions_vec));
        } else {
            return Err(ParsingError::ExpectedBracketAfterVectorIndex {
                found: token.value.to_owned(),
            });
        }
    } else {
        return Err(ParsingError::UnexpectedEndOfInput);
    }
}
