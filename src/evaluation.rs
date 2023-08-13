use crate::parser::{
    architecture::{SymbolTable, SymbolTableError},
    expression::{Expression, FormattedSegment},
    functions::{errors::FunctionParsingError, Function},
};

use self::operations::evaluate_binary_operation;

pub mod operations;

// evaluate expressions

custom_error::custom_error! {pub EvaluationError
    SymbolTable{source: SymbolTableError} = "{source}",
    FunctionEr{source: FunctionParsingError} = "{source}",
    InvalidExpression{expression: Expression} = "Invalid expression `{expression}`",

    Custom{message: String} = "{message}"
}

pub fn evaluate_expression(
    expr: Expression,
    symbol_table: &mut SymbolTable,
) -> Result<Expression, EvaluationError> {
    match expr {
        Expression::Variable(var_name) => {
            let var = symbol_table.get_variable(&var_name)?;
            Ok(evaluate_expression(var.value, symbol_table)?)
        }
        Expression::BinaryOperation { right, operator, left } => {
            Ok(evaluate_binary_operation(Expression::BinaryOperation {right, operator, left }, symbol_table)?)
        }
        Expression::FormattedString(formatted) => {
            let mut final_str = String::new();

            for s in formatted {
                match s {
                    FormattedSegment::Literal(l) => {
                        final_str.push_str(&l);
                    }
                    FormattedSegment::Expression(expr) => {
                        final_str.push_str(&evaluate_expression(expr, symbol_table)?.to_string());
                    }
                }
            }

            Ok(Expression::String(final_str))
        }
        Expression::FunctionCall(fn_call) => {
            let mut f = symbol_table.get_full_function(&fn_call.function_name)?;

            let e = Function::with_args(&mut f, &fn_call.arguments, symbol_table)?;

            Ok(e)
        }
        _ => Ok(expr),
    }
}
