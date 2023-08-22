use std::{fmt, num::ParseIntError};

use crate::{
    evaluation::{evaluate_expression, EvaluationError},
    token::{Token, TokenType},
};

use super::{
    architecture::{SymbolTable, SymbolTableError},
    expression::{operation::BinaryOperator, Expression},
    functions::{errors::FunctionParsingError, Argument},
    ParsingError,
};
use custom_error::custom_error;

custom_error! {pub TypeError
    ExpectedType{value: String} = "Expected valid type after \"{value}\"",
    FunctionParsingError{source: FunctionParsingError} = "{source}",
    ExpressionNull = "Cannot determine type of null expression",
    CannotOperation{type1: VariableType, type2: VariableType, action: String} = "Cannot {action} '' with ''",
    CannotOperationTypeWithType{operator: String, expr: String, first_type: VariableType, second_type: VariableType} = "Failed to {operator} \"{first_type}\" with \"{second_type}\" at: {expr}",

    // FailedToDetermineObjPropType{obj_name: String, prop: String} = "Failed to determine type of '{prop}' property on \"{obj_name}\""

    ParseInt{source: ParseIntError} = "{source}",
    Custom{data:String} = "{data}"
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableType {
    String,
    Number,
    Boolean,
    Vector,
    Dictionary,
    Class(String),
}
impl fmt::Display for VariableType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VariableType::String => write!(f, "String"),
            VariableType::Number => write!(f, "Number"),
            VariableType::Boolean => write!(f, "Boolean"),
            VariableType::Vector => write!(f, "Vector"),
            VariableType::Dictionary => write!(f, "Dictionary"),
            VariableType::Class(name) => write!(f, "Class {}", name),
        }
    }
}

impl VariableType {
    pub fn as_assignment(&self) -> &str {
        match self {
            VariableType::String => "str",
            VariableType::Number => "num",
            VariableType::Boolean => "bool",
            VariableType::Vector => "vec",
            VariableType::Dictionary => "dict",
            VariableType::Class(name) => &name,
        }
    }
    pub fn from_assignment(input: &str, symbol_table: &SymbolTable) -> Option<VariableType> {
        match input {
            "str" => Some(VariableType::String),
            "num" => Some(VariableType::Number),
            "bool" => Some(VariableType::Boolean),
            "vec" => Some(VariableType::Vector),
            "dict" => Some(VariableType::Dictionary),
            cls_name if symbol_table.is_class_declared(input) => {
                Some(VariableType::Class(cls_name.to_owned()))
            }
            _ => None,
        }
    }
}

pub fn type_from_expression(
    expr: &Expression,
    symbol_table: &SymbolTable,
) -> Result<VariableType, SymbolTableError> {
    match expr.clone() {
        Expression::Number(_) => Ok(VariableType::Number),
        Expression::String(_) => Ok(VariableType::String),
        Expression::ArrayExpression(_) => Ok(VariableType::Vector),
        Expression::FormattedString(_) => Ok(VariableType::String),
        Expression::Boolean(_) => Ok(VariableType::Boolean),
        Expression::Null => Err(TypeError::ExpressionNull.into()),
        Expression::DictionaryExpression(_) => Ok(VariableType::Dictionary),
        Expression::Variable(var_name) => {
            symbol_table.get_variable(&var_name).map(|var| var.var_type)
        }
        Expression::BinaryOperation {
            left,
            operator,
            right,
        } => {
            let left_type = type_from_expression(&left, symbol_table)?;
            let right_type = type_from_expression(&right, symbol_table)?;

            match operator {
                BinaryOperator::Plus => {
                    match (left_type.clone(), right_type.clone()) {
                        (VariableType::Vector, _) => Ok(VariableType::Vector),
                        (_, VariableType::Vector) => Ok(VariableType::Vector),

                        (VariableType::Boolean, VariableType::Boolean) => Ok(VariableType::Boolean),

                        (VariableType::Dictionary, VariableType::Dictionary) => {
                            Ok(VariableType::Dictionary)
                        }

                        (VariableType::String, VariableType::String) => Ok(VariableType::String),
                        (VariableType::Number, VariableType::Number) => Ok(VariableType::Number),
                        _ => Err(TypeError::CannotOperationTypeWithType {
                            operator: BinaryOperator::Plus.operator_as_verb(),
                            expr: expr.to_string(),
                            first_type: left_type,
                            second_type: right_type,
                        }
                        .into()), // Invalid binary operation, return None for other cases.
                    }
                }
                operator => {
                    match (left_type.clone(), right_type.clone()) {
                        (VariableType::Number, VariableType::Number) => Ok(VariableType::Number),
                        _ => Err(TypeError::CannotOperationTypeWithType {
                            operator: operator.operator_as_verb(),
                            expr: expr.to_string(),
                            first_type: left_type,
                            second_type: right_type,
                        }
                        .into()),
                        // return None for every other operation cause we can only use these operations on numbers
                    }
                }
            }
        }
        Expression::FunctionCall(call) => {
            match symbol_table
                .get_function(&call.function_name)
                .map(|var| var.return_type)
            {
                Ok(r) => {
                    if r.is_some() {
                        Ok(r.unwrap())
                    } else {
                        Err(TypeError::ExpressionNull.into())
                    }
                }
                Err(e) => Err(TypeError::Custom {
                    data: e.to_string(),
                }
                .into()),
            }
        }
        Expression::ArrayIndexing(mut indexing) => {
            let initial_expr = indexing.remove(0);

            let t = type_from_expression(&initial_expr, symbol_table)?;

            // vec is an array expression corresponding to the one that is indexed
            let mut vec = match t {
                VariableType::Vector => match &initial_expr {
                    Expression::Variable(name) => match symbol_table.get_variable(name)?.value {
                        Expression::ArrayExpression(arr) => arr,
                        _ => unreachable!(),
                    },
                    Expression::ArrayExpression(arr) => arr.to_owned(),
                    _ => unreachable!(),
                },
                _ => {
                    return Err(SymbolTableError::CannotIndexNotVector {
                        indexed_expr: Expression::ArrayIndexing(vec![
                            initial_expr,
                            indexing[0].to_owned(),
                        ]),
                        actual_type: t,
                    })
                }
            };

            for (i, val) in indexing.iter().enumerate() {
                let t = type_from_expression(&val, symbol_table)?;

                let index = match evaluate_expression(val.clone(), symbol_table)? {
                    Expression::Number(n) => n,
                    _ => {
                        return Err(SymbolTableError::InvalidTypeIndex {
                            found_type: type_from_expression(val, symbol_table)?,
                            full_expr: Expression::ArrayIndexing(
                                vec![initial_expr]
                                    .into_iter()
                                    .chain(indexing.iter().take(i + 1).cloned())
                                    .collect(),
                            ),
                        })
                    }
                } as isize;

                if vec.len() == 0 {
                    let vec_name = vec![initial_expr]
                        .into_iter()
                        .chain(indexing.iter().take(i + 1).cloned())
                        .collect();

                    return Err(SymbolTableError::CannotIndexEmptyVec {
                        vec_name: Expression::ArrayIndexing(vec_name).to_string(),
                    });
                }

                if index < 0 {
                    let vec_name = vec![initial_expr]
                        .into_iter()
                        .chain(indexing.iter().take(i + 1).cloned())
                        .collect();

                    return Err(SymbolTableError::CannotIndexNegatively {
                        vec_name: Expression::ArrayIndexing(vec_name).to_string(),
                        index,
                    });
                }
                let index = index as usize;

                if index > &vec.len() - 1 {
                    let vec_name = vec![initial_expr]
                        .into_iter()
                        .chain(indexing.iter().take(i + 1).cloned())
                        .collect();

                    return Err(SymbolTableError::IndexOutOfRange {
                        vec_name: Expression::ArrayIndexing(vec_name).to_string(),
                        index,
                        length: vec.len(),
                    });
                }
                match t {
                    VariableType::Number => {
                        if i == indexing.len() - 1 {
                            return Ok(type_from_expression(&vec[index], symbol_table)?);
                        } else {
                            vec = match &vec[index] {
                                Expression::ArrayExpression(arr) => arr.to_owned(),
                                _ => {
                                    return Err(SymbolTableError::CannotIndexNotVector {
                                        indexed_expr: Expression::ArrayIndexing(
                                            vec![initial_expr]
                                                .into_iter()
                                                .chain(indexing.iter().take(i + 1).cloned())
                                                .collect(),
                                        ),
                                        actual_type: t,
                                    })
                                }
                            };
                        }
                    }
                    _ => {
                        return Err(SymbolTableError::InvalidTypeIndex {
                            found_type: t,
                            full_expr: Expression::ArrayIndexing(
                                vec![initial_expr]
                                    .into_iter()
                                    .chain(indexing.iter().take(i + 1).cloned())
                                    .collect(),
                            ),
                        })
                    }
                }
            }

            // cannot arrive here
            Ok(t)
        }
    }
}

pub fn parse_type(token: Option<&Token>) -> Result<VariableType, ParsingError> {
    // fn to use after encountering ':'
    if let Some(Token {
        token_type: TokenType::TypeBool,
        ..
    }) = token
    {
        Ok(VariableType::Boolean)
    } else if let Some(Token {
        token_type: TokenType::TypeDict,
        ..
    }) = token
    {
        Ok(VariableType::Dictionary)
    } else if let Some(Token {
        token_type: TokenType::TypeNumber,
        ..
    }) = token
    {
        Ok(VariableType::Number)
    } else if let Some(Token {
        token_type: TokenType::TypeString,
        ..
    }) = token
    {
        Ok(VariableType::String)
    } else if let Some(Token {
        token_type: TokenType::TypeVec,
        ..
    }) = token
    {
        Ok(VariableType::Vector)
    } else if let Some(Token { value, .. }) = token {
        return Err(TypeError::ExpectedType {
            value: value.to_owned(),
        }
        .into());
    } else {
        return Err(ParsingError::UnexpectedEndOfInput);
    }
}

pub fn err_on_fn_call_args_invalid(
    fn_name: &str,
    fn_arguments: &Vec<Argument>,
    call_args: &Vec<Expression>,
    symbol_table: &SymbolTable,
) -> Result<(), EvaluationError> {
    let fn_types_vec = fn_arguments
        .iter()
        .map(|dec| dec.var_type.clone())
        .collect::<Vec<VariableType>>();

    for (i, arg) in call_args.iter().enumerate() {
        let actual_arg_type = match type_from_expression(arg, symbol_table) {
            Ok(r) => r,
            Err(e) => {
                return Err(EvaluationError::Custom {
                    message: e.to_string(),
                })
            }
        };

        if fn_types_vec[i] != actual_arg_type {
            return Err(FunctionParsingError::InvalidFnCallArgType {
                fn_name: fn_name.to_owned(),
                arg_name: fn_arguments[i].name.to_owned(),
                required_t: fn_types_vec[i].clone(),
                found_t: actual_arg_type,
            }
            .into());
        }
    }

    Ok(())
}
