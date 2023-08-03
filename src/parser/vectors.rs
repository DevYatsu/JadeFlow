use super::{
    architecture::{Declaration, Expression, SymbolTable},
    expression::parse_expression,
    ignore_whitespace, ParsingError,
};
use crate::token::{Token, TokenType};

pub fn parse_array_expression(
    tokens: &mut std::slice::Iter<'_, Token>,
    symbol_table: &SymbolTable,
) -> Result<Expression, ParsingError> {
    let mut vec_expressions = Vec::new();
    ignore_whitespace(tokens);
    println!("tokens in vec: {:?}", tokens);
    let mut peekable = tokens.clone().peekable();
    while let Some(token) = peekable.next() {
        println!("{:?}", tokens);
        match token.token_type {
            TokenType::CloseBracket => {
                tokens.next();
                break;
            }
            TokenType::Comma | TokenType::Separator => {
                tokens.next();
                continue; // still an issue if there is an expr, then two /n and an expr again
            }
            _ => {
                let value = parse_expression(tokens, symbol_table)?;

                match value {
                    Expression::ArrayExpression(_) | Expression::DictionaryExpression(_) => {
                        peekable = tokens.clone().peekable();
                    }
                    _ => (),
                };
                check_and_insert_expression(value, symbol_table, &mut vec_expressions)?;
            }
        }
    }

    Ok(Expression::ArrayExpression(vec_expressions))
}

fn check_and_insert_expression(
    expression: Expression,
    symbol_table: &SymbolTable,
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
    tokens: &mut std::slice::Iter<'_, Token>,
    var: Declaration,
) -> Result<Expression, ParsingError> {
    let next = tokens.next();

    if let Some(Token {
        token_type: TokenType::Number,
        value,
    }) = next
    {
        let index: usize = value.parse()?;

        if let Some(token) = tokens.next() {
            if token.token_type == TokenType::CloseBracket {
                return Ok(Expression::Variable(format!("{}[{}", var.name, index)));
            } else {
                return Err(ParsingError::ExpectedBracketAfterVectorIndex {
                    found: token.value.to_owned(),
                });
            }
        } else {
            return Err(ParsingError::UnexpectedEndOfInput);
        }
    } else if let Some(Token { value, .. }) = next {
        return Err(ParsingError::ExpectedValidVectorIndex {
            found: value.to_owned(),
        });
    } else {
        return Err(ParsingError::UnexpectedEndOfInput);
    }
}
