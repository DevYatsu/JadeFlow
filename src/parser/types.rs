use std::{fmt, num::ParseIntError};

use crate::token::{Token, TokenType};

use super::{
    architecture::{SymbolTable, SymbolTableError},
    expression::{operation::BinaryOperator, Expression},
    functions::errors::FunctionParsingError,
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
}
impl fmt::Display for VariableType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VariableType::String => write!(f, "String"),
            VariableType::Number => write!(f, "Number"),
            VariableType::Boolean => write!(f, "Boolean"),
            VariableType::Vector => write!(f, "Vector"),
            VariableType::Dictionary => write!(f, "Dictionary"),
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
        }
    }
    pub fn from_assignment(input: &str) -> Option<VariableType> {
        match input {
            "str" => Some(VariableType::String),
            "num" => Some(VariableType::Number),
            "bool" => Some(VariableType::Boolean),
            "vec" => Some(VariableType::Vector),
            "dict" => Some(VariableType::Dictionary),
            _ => None,
        }
    }
}

pub fn type_from_expression(
    expr: &Expression,
    symbol_table: &mut SymbolTable,
) -> Result<VariableType, SymbolTableError> {
    match expr {
        Expression::Number(_) => Ok(VariableType::Number),
        Expression::String(_) => Ok(VariableType::String),
        Expression::ArrayExpression(_) => Ok(VariableType::Vector),
        Expression::FormattedString(_) => Ok(VariableType::String),
        Expression::Boolean(_) => Ok(VariableType::Boolean),
        Expression::Null => Err(TypeError::ExpressionNull.into()),
        Expression::DictionaryExpression(_) => Ok(VariableType::Dictionary),
        Expression::Variable(var_name) => {
            symbol_table.get_variable(var_name).map(|var| var.var_type)
        }
        Expression::BinaryOperation {
            left,
            operator,
            right,
        } => {
            let left_type = type_from_expression(left, symbol_table)?;
            let right_type = type_from_expression(right, symbol_table)?;

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
