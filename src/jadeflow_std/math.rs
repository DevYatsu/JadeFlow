use crate::{
    create_function_map, function,
    parser::{
        expression::Expression,
        functions::{Argument, Function},
        types::VariableType,
    },
};
use hashbrown::HashMap;
use once_cell::sync::OnceCell;

pub fn load_jadeflow_math() -> HashMap<String, Function> {
    let sqrt = function!(
        "sqrt",
        arguments: vec![Argument::new(
            "number".to_owned(),
            VariableType::Number,
            false,
        )],
        return_type: Some(VariableType::Number),
        code: |args: Vec<Expression>| -> Expression {
            if let Expression::Number(num) = &args[0] {
                Expression::Number(num.sqrt())
            } else {
                unreachable!()
            }
        }
    );
    let abs = function!(
        "abs",
        arguments: vec![Argument::new(
            "number".to_owned(),
            VariableType::Number,
            false,
        )],
        return_type: Some(VariableType::Number),
        code: |args: Vec<Expression>| -> Expression {
            if let Expression::Number(num) = &args[0] {
                Expression::Number(num.abs())
            } else {
                unreachable!()
            }
        }
    );
    let sin = function!(
        "sin",
        arguments: vec![Argument::new(
                "angle".to_owned(),
                VariableType::Number,
                false,
            )],
        return_type: Some(VariableType::Number),
        code: |args: Vec<Expression>| -> Expression {
                if let Expression::Number(angle) = &args[0] {
                    Expression::Number(angle.sin())
                } else {
                    unreachable!()
                }
            }
    );
    let cos = function!(
        "cos",
        arguments: vec![Argument::new(
                "angle".to_owned(),
                VariableType::Number,
                false,
            )],
        return_type: Some(VariableType::Number),
        code: |args: Vec<Expression>| -> Expression {
                if let Expression::Number(angle) = &args[0] {
                    Expression::Number(angle.cos())
                } else {
                    unreachable!()
                }
            }
    );

    let tan = function!(
        "tan",
        arguments: vec![Argument::new(
                "angle".to_owned(),
                VariableType::Number,
                false,
            )],
        return_type: Some(VariableType::Number),
        code: |args: Vec<Expression>| -> Expression {
                if let Expression::Number(angle) = &args[0] {
                    Expression::Number(angle.tan())
                } else {
                    unreachable!()
                }
            }
    );

    let log = function!(
        "log",
        arguments: vec![Argument::new(
                "value".to_owned(),
                VariableType::Number,
                false,
            )],
        return_type: Some(VariableType::Number),
        code: |args: Vec<Expression>| -> Expression {
                if let Expression::Number(value) = &args[0] {
                    Expression::Number(value.ln())
                } else {
                    unreachable!()
                }
            }
    );

    let round = function!(
        "round",
        arguments: vec![Argument::new(
                "number".to_owned(),
                VariableType::Number,
                false,
            )],
        return_type: Some(VariableType::Number),
        code: |args: Vec<Expression>| -> Expression {
                if let Expression::Number(num) = &args[0] {
                    Expression::Number(num.round())
                } else {
                    unreachable!()
                }
            }
    );

    let floor = function!(
        "floor",
        arguments: vec![Argument::new(
                "number".to_owned(),
                VariableType::Number,
                false,
            )],
        return_type: Some(VariableType::Number),
        code: |args: Vec<Expression>| -> Expression {
                if let Expression::Number(num) = &args[0] {
                    Expression::Number(num.floor())
                } else {
                    unreachable!()
                }
            }
    );

    let ceil = function!(
        "ceil",
        arguments: vec![Argument::new(
                "number".to_owned(),
                VariableType::Number,
                false,
            )],
        return_type: Some(VariableType::Number),
        code: |args: Vec<Expression>| -> Expression {
                if let Expression::Number(num) = &args[0] {
                    Expression::Number(num.ceil())
                } else {
                    unreachable!()
                }
            }
    );

    create_function_map!(sqrt, abs, sin, cos, tan, log, floor, round, ceil)
}
