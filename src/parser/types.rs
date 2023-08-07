use std::num::ParseIntError;

use crate::{
    parser::Expression,
    token::{Token, TokenType},
};

use super::{
    architecture::{BinaryOperator, SymbolTable, VariableType},
    functions::FunctionParsingError,
    ParsingError,
};
use custom_error::custom_error;

custom_error! {pub TypeError
    ExpectedType{value: String} = "Expected valid type after \"{value}\"",
    FunctionParsingError{source: FunctionParsingError} = "{source}",
    ExpressionNull = "Cannot determine type of null expression",
    CannotOperation{type1: VariableType, type2: VariableType, action: String} = "Cannot {action} '' with ''",
    CannotOperationTypeWithType{operator: String, expr: String, first_type: VariableType, second_type: VariableType} = "Failed to {operator} \"{first_type}\" with \"{second_type}\" at: {expr}",

    CannotDetermineVarType{name: String} = "Cannot determine \"{name}\" type as it is not defined",
    CannotDetermineObjPropTypeNotDefined{obj_name: String, prop: String} = "Cannot determine type of '{prop}' property on \"{obj_name}\" as it is not defined",
    // FailedToDetermineObjPropType{obj_name: String, prop: String} = "Failed to determine type of '{prop}' property on \"{obj_name}\""

    ParseInt{source: ParseIntError} = "{source}",
    IndexOutOfRange{vec_name: String, index: usize, length: usize} = "Index out of range! Cannot index \"{vec_name}\" at index {index} when length is {length}"
    ,Custom{data:String} = "{data}"
}

pub fn type_from_expression(
    expr: &Expression,
    symbol_table: &mut SymbolTable,
    tokens: Option<&mut std::iter::Peekable<std::slice::Iter<'_, Token>>>,
) -> Result<VariableType, TypeError> {
    match expr {
        Expression::Number(_) => Ok(VariableType::Number),
        Expression::String(_) => Ok(VariableType::String),
        Expression::ArrayExpression(_) => Ok(VariableType::Vector),
        Expression::FormattedString(_) => Ok(VariableType::String),
        Expression::Boolean(_) => Ok(VariableType::Boolean),
        Expression::Null => Err(TypeError::ExpressionNull),
        Expression::DictionaryExpression(_) => Ok(VariableType::Dictionary),
        Expression::Variable(var_name) => symbol_table
            .get_variable(var_name, None)
            .map(|var| var.var_type),
        Expression::BinaryOperation {
            left,
            operator,
            right,
        } => {
            let mut t = tokens.cloned();
            let left_type = type_from_expression(left, symbol_table, t.as_mut())?;
            let right_type = type_from_expression(right, symbol_table, t.as_mut())?;

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
                        }), // Invalid binary operation, return None for other cases.
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
                        }),
                        // return None for every other operation cause we can only use these operations on numbers
                    }
                }
            }
        }
        Expression::FunctionCall(call) => {
            match symbol_table
                .get_function(&call.function_name, tokens.unwrap())
                .map(|var| var)
            {
                Ok(f) => {
                    symbol_table.insert_function_in_advance(&f);
                    if f.return_type.is_some() {
                        Ok(f.return_type.unwrap())
                    } else {
                        Err(TypeError::ExpressionNull)
                    }
                }
                Err(e) => Err(TypeError::Custom {
                    data: e.to_string(),
                }),
            }
        }
    }
}

pub fn parse_type(
    tokens: &mut std::iter::Peekable<std::slice::Iter<'_, Token>>,
) -> Result<VariableType, ParsingError> {
    let next = tokens.next();
    // fn to use after encountering ':'
    if let Some(Token {
        token_type: TokenType::TypeBool,
        ..
    }) = next
    {
        Ok(VariableType::Boolean)
    } else if let Some(Token {
        token_type: TokenType::TypeDict,
        ..
    }) = next
    {
        Ok(VariableType::Dictionary)
    } else if let Some(Token {
        token_type: TokenType::TypeNumber,
        ..
    }) = next
    {
        Ok(VariableType::Number)
    } else if let Some(Token {
        token_type: TokenType::TypeString,
        ..
    }) = next
    {
        Ok(VariableType::String)
    } else if let Some(Token {
        token_type: TokenType::TypeVec,
        ..
    }) = next
    {
        Ok(VariableType::Vector)
    } else if let Some(Token { value, .. }) = next {
        return Err(TypeError::ExpectedType {
            value: value.to_owned(),
        }
        .into());
    } else {
        return Err(ParsingError::UnexpectedEndOfInput);
    }
}
