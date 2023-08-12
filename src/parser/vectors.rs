use super::{
    architecture::SymbolTable,
    expression::{parse_expression, Expression},
    ignore_whitespace,
    vars::Declaration,
    ParsingError,
};
use crate::{
    parser::types::VariableType,
    print_info,
    token::{Token, TokenType},
};

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
                break;
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
            symbol_table.get_variable(name, None)?; // error if var not defined
        }
        _ => (),
    }
    vec_expressions.push(expression);
    Ok(())
}

pub fn parse_array_indexing(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
    var_name: &str,
    symbol_table: &mut SymbolTable,
) -> Result<Expression, ParsingError> {
    let next = tokens.next();
    println!("next {:?}", next);

    let index = if let Some(Token {
        token_type: TokenType::Number,
        value,
    }) = next
    {
        let index: i128 = value.parse()?;
        let index_as_f64: f64 = index as f64;
        index_as_f64
    } else if let Some(Token {
        token_type: TokenType::Identifier,
        value: name,
    }) = next
    {
        let var = symbol_table.get_variable(&name, Some(tokens))?;

        if var.var_type != VariableType::Number {
            return Err(ParsingError::VariableAsVectorIndexInvalid {
                var_name: var.name.to_owned(),
            });
        }

        match &var.value {
            Expression::Variable(_) => {
                print_info!("Variables pointing to another variable as array index not yet supported! at {}", var.value);
                todo!()
            }
            Expression::Number(n) => n.clone(),
            Expression::BinaryOperation { .. } => {
                print_info!(
                    "Operations as array index not yet supported! at {}",
                    var.value
                );
                todo!()
            }
            _ => unreachable!(),
        }
    } else if let Some(Token { value, .. }) = next {
        return Err(ParsingError::ExpectedValidVectorIndex {
            found: value.to_owned(),
        });
    } else {
        return Err(ParsingError::UnexpectedEndOfInput);
    };

    if let Some(token) = tokens.next() {
        if token.token_type == TokenType::CloseBracket {
            while let Some(t) = tokens.peek() {
                match t.token_type {
                    TokenType::OpenBracket => {
                        tokens.next();
                        return Ok(parse_array_indexing(
                            tokens,
                            &format!("{}[{}", var_name, index),
                            symbol_table,
                        )?);
                    }
                    _ => break,
                }
            }
            return Ok(Expression::Variable(format!("{}[{}", var_name, index)));
        } else {
            return Err(ParsingError::ExpectedBracketAfterVectorIndex {
                found: token.value.to_owned(),
            });
        }
    } else {
        return Err(ParsingError::UnexpectedEndOfInput);
    }
}
