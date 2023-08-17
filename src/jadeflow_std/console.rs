use crate::{
    create_function_map, function,
    parser::{
        expression::Expression,
        functions::{Argument, Function},
        types::VariableType,
    },
};
use once_cell::sync::OnceCell;
use std::{
    collections::HashMap,
    io::{self, Write},
};

pub fn load_std_console() -> HashMap<String, Function> {
    let println = function!(
        "println",
        arguments: vec![Argument::new(
                "message".to_owned(),
                VariableType::String,
                false,
            )],
        return_type: None,
        code: |args: Vec<Expression>| -> Expression {
                if let Expression::String(msg) = &args[0] {
                    println!("{msg}");
                    Expression::Null
                } else {
                    unreachable!()
                }
            }
    );

    let input = function!(
        "input",
        arguments: vec![Argument::new(
                "prompt".to_owned(),
                VariableType::String,
                false,
            )],
        return_type: Some(VariableType::String),
        code: |args: Vec<Expression>| -> Expression {
                let mut input = String::new();

                  if let Expression::String(msg) = &args[0] {
                      print!("{msg}");
                      io::stdout().flush().expect("Failed to flush stdout");
                      std::io::stdin()
                          .read_line(&mut input)
                          .expect("Failed to read input");
                      Expression::String(input.trim().to_string())
                } else {
                    unreachable!()
                }
            }
    );

    let print = function!(
        "print",
        arguments: vec![Argument::new(
                "message".to_owned(),
                VariableType::String,
                false,
            )],
        return_type: None,
        code: |args: Vec<Expression>| -> Expression {
                if let Expression::String(text) = &args[0] {
                    print!("{text}");
                    Expression::Null
                } else {
                    unreachable!()
                }
            }
    );

    create_function_map!(print, input, println)
}
