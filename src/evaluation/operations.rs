use crate::{
    parser::{
        architecture::SymbolTable,
        expression::{operation::BinaryOperator, Expression},
    },
    print_info,
};

use super::{evaluate_expression, EvaluationError};

pub fn evaluate_binary_operation(
    op: Expression,
    symbol_table: &mut SymbolTable,
) -> Result<Expression, EvaluationError> {
    match op {
        Expression::BinaryOperation {
            mut left,
            operator,
            mut right,
        } => {
            while let Expression::BinaryOperation { .. } = *left {
                left = Box::new(evaluate_expression(*left, symbol_table)?);
            }
            while let Expression::BinaryOperation { .. } = *right {
                right = Box::new(evaluate_expression(*right, symbol_table)?);
            }

            let expr = *left;
            match expr {
                Expression::Variable(_) => {
                    let left = Box::new(evaluate_expression(expr, symbol_table)?);
                    let right = Box::new(evaluate_expression(*right, symbol_table)?);
                    return Ok(evaluate_binary_operation(
                        Expression::BinaryOperation {
                            left,
                            operator,
                            right,
                        },
                        symbol_table,
                    )?);
                }
                Expression::Number(x) => match *right {
                    Expression::Number(y) => match operator {
                        BinaryOperator::Plus => return Ok(Expression::Number(x + y)),
                        BinaryOperator::Minus => return Ok(Expression::Number(x - y)),
                        BinaryOperator::Multiply => return Ok(Expression::Number(x * y)),
                        BinaryOperator::Divide => return Ok(Expression::Number(x / y)),
                        BinaryOperator::Modulo => return Ok(Expression::Number(x % y)),
                        BinaryOperator::Exponential => return Ok(Expression::Number(x.powf(y))),
                    },
                    Expression::ArrayExpression(mut arr) => {
                        arr.push(expr);
                        return Ok(Expression::ArrayExpression(arr));
                    }
                    _ => unreachable!(),
                },
                Expression::String(mut s) => {
                    let right_s = match evaluate_expression(*right, symbol_table)? {
                        Expression::String(s) => s,
                        _ => unreachable!(),
                    };
                    s.push_str(&right_s);
                    return Ok(Expression::String(s));
                }
                Expression::ArrayExpression(mut arr) => {
                    // we know that everything can be add with an array

                    match *right {
                        Expression::ArrayExpression(other_arr) => arr.extend(other_arr),
                        _ => arr.push(*right),
                    };

                    return Ok(Expression::ArrayExpression(arr));
                }
                Expression::DictionaryExpression(mut dict) => {
                    // we know that only a dict can be add with a dict
                    let right = match *right {
                        Expression::DictionaryExpression(d) => d,
                        _ => unreachable!(),
                    };
                    dict.extend(right);
                    return Ok(Expression::DictionaryExpression(dict));
                }
                Expression::FormattedString(f) => {
                    let mut s =
                        match evaluate_expression(Expression::FormattedString(f), symbol_table)? {
                            Expression::String(s) => s,
                            _ => unreachable!(),
                        };
                    let right_s = match evaluate_expression(*right, symbol_table)? {
                        Expression::String(s) => s,
                        _ => unreachable!(),
                    };
                    s.push_str(&right_s);
                    return Ok(Expression::String(s));
                }
                Expression::FunctionCall(_) => {
                    print_info!("Function calls evaluation not yet implemented");
                    todo!()
                }
                _ => unreachable!(),
            }
        }
        _ => unreachable!(),
    }
}
