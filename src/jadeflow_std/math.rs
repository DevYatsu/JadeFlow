use crate::{
    create_function_map,
    jadeflow_std::StandardFunction,
    parser::{expression::Expression, functions::Function, types::VariableType},
};
use std::collections::HashMap;
pub fn load_std_math() -> HashMap<String, StandardFunction> {
    let sqrt = StandardFunction::new(
        "sqrt",
        vec![Function::argument("number", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(num) = &args[0] {
                Expression::Number(num.sqrt())
            } else {
                unreachable!()
            }
        }),
    );
    let abs = StandardFunction::new(
        "abs",
        vec![Function::argument("number", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(num) = &args[0] {
                Expression::Number(num.abs())
            } else {
                unreachable!()
            }
        }),
    );
    let sin = StandardFunction::new(
        "sin",
        vec![Function::argument("angle", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(angle) = &args[0] {
                Expression::Number(angle.sin())
            } else {
                unreachable!()
            }
        }),
    );

    let cos = StandardFunction::new(
        "cos",
        vec![Function::argument("angle", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(angle) = &args[0] {
                Expression::Number(angle.cos())
            } else {
                unreachable!()
            }
        }),
    );

    let tan = StandardFunction::new(
        "tan",
        vec![Function::argument("angle", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(angle) = &args[0] {
                Expression::Number(angle.tan())
            } else {
                unreachable!()
            }
        }),
    );

    let log = StandardFunction::new(
        "log",
        vec![Function::argument("value", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(value) = &args[0] {
                Expression::Number(value.ln())
            } else {
                unreachable!()
            }
        }),
    );

    let round = StandardFunction::new(
        "round",
        vec![Function::argument("number", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(num) = &args[0] {
                Expression::Number(num.round())
            } else {
                unreachable!()
            }
        }),
    );

    let floor = StandardFunction::new(
        "floor",
        vec![Function::argument("number", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(num) = &args[0] {
                Expression::Number(num.floor())
            } else {
                unreachable!()
            }
        }),
    );

    let ceil = StandardFunction::new(
        "ceil",
        vec![Function::argument("number", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(num) = &args[0] {
                Expression::Number(num.ceil())
            } else {
                unreachable!()
            }
        }),
    );

    create_function_map!(sqrt, abs, sin, cos, tan, log, floor, round, ceil)
}
