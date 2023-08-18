use crate::{
    jadeflow_std::load_std,
    parser::{
        architecture::{ASTNode, Program, SymbolTable},
        expression::{Expression, FormattedSegment},
        functions::errors::FunctionParsingError,
        types::type_from_expression,
        vars::Declaration,
    },
};

use self::operations::evaluate_binary_operation;

pub mod operations;

// evaluate expressions

custom_error::custom_error! {pub EvaluationError
    FunctionEr{source: FunctionParsingError} = "{source}",
    InvalidExpression{expression: Expression} = "Invalid expression `{expression}`",

    Custom{message: String} = "{message}"
}

pub fn evaluate_expression(
    expr: Expression,
    symbol_table: &SymbolTable,
) -> Result<Expression, EvaluationError> {
    match expr {
        Expression::Variable(var_name) => {
            let var = match symbol_table.get_variable(&var_name) {
                Ok(result) => result,
                Err(e) => {
                    return Err(EvaluationError::Custom {
                        message: e.to_string(),
                    })
                }
            };
            Ok(evaluate_expression(var.value, symbol_table)?)
        }
        Expression::BinaryOperation { .. } => Ok(evaluate_binary_operation(expr, symbol_table)?),
        Expression::FormattedString(formatted) => {
            let mut final_str = String::new();

            for s in formatted {
                match s {
                    FormattedSegment::Literal(l) => {
                        final_str.push_str(&l);
                    }
                    FormattedSegment::Expression(expr) => {
                        final_str.push_str(
                            &evaluate_expression(expr, symbol_table)?.str_for_formatted(),
                        );
                    }
                }
            }

            Ok(Expression::String(final_str))
        }
        Expression::FunctionCall(fn_call) => {
            let f = symbol_table.get_full_function(&fn_call.function_name)?;
            let ex = f.run_with_args(&fn_call.arguments, symbol_table)?;
            Ok(ex)
        }
        Expression::ArrayIndexing(mut index_vec) => {
            // throw an error if there is a type issue in the array indexing,
            // for example if we try to index sth other than a vector
            match type_from_expression(&Expression::ArrayIndexing(index_vec.clone()), symbol_table)
            {
                Ok(_) => (),
                Err(e) => {
                    return Err(EvaluationError::Custom {
                        message: e.to_string(),
                    })
                }
            };
            let initial_value = index_vec.remove(0);

            let mut actual_arr = match evaluate_expression(initial_value, symbol_table)? {
                Expression::ArrayExpression(arr) => arr,
                _ => unreachable!(), // as we used the type_from_expression fn in the start we can rest assured that the rest is unreachable
            };

            while index_vec.len() > 1 {
                let value = index_vec.remove(0);
                match evaluate_expression(value, symbol_table)? {
                    Expression::Number(num) => {
                        let num = num as usize;
                        actual_arr =
                            match evaluate_expression(actual_arr[num].clone(), symbol_table)? {
                                Expression::ArrayExpression(arr) => arr,
                                _ => unreachable!(), // as we used the type_from_expression fn in the start we can rest assured that the rest is unreachable
                            };
                    }
                    _ => unreachable!(), // thx to the type_from_expression
                };
            }

            let last_index = match evaluate_expression(index_vec.remove(0), symbol_table)? {
                Expression::Number(num) => {
                    let num = num as usize;
                    num
                }
                _ => unreachable!(),
            };

            Ok(evaluate_expression(
                actual_arr.remove(last_index),
                symbol_table,
            )?)
        }
        _ => Ok(expr),
    }
}

pub fn evaluate_program(mut program: Program) -> Result<SymbolTable, EvaluationError> {
    let mut rerun_table = program.symbol_table;
    rerun_table.functions.extend(load_std());

    for statement in program.statements.iter_mut() {
        match &statement.node {
            ASTNode::VariableDeclaration {
                name,
                var_type,
                value,
                is_mutable,
                is_object_prop,
            } => {
                let value = evaluate_expression(value.clone(), &mut rerun_table)?;

                rerun_table.insert_variable(Declaration {
                    name: name.to_owned(),
                    var_type: var_type.to_owned(),
                    value,
                    is_mutable: *is_mutable,
                    is_object_prop: *is_object_prop,
                });
            }
            ASTNode::VariableReassignment { name, value } => {
                let value = evaluate_expression(value.to_owned(), &mut rerun_table)?;
                rerun_table
                    .reassign_variable(name.to_owned(), value)
                    .map_err(|e| EvaluationError::Custom {
                        message: e.to_string(),
                    })?;
            }
            ASTNode::FunctionCall {
                function_name,
                arguments,
            } => {
                rerun_table.run_fn(&function_name, &arguments)?;
            }
            ASTNode::FunctionDeclaration(_) | ASTNode::ClassDeclaration(_) => {}
            ASTNode::Return { .. } => {}
            _ => unreachable!(),
        }
    }

    Ok(rerun_table)
}
