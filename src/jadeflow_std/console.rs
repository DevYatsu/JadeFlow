use crate::{
    create_function_map,
    jadeflow_std::StandardFunction,
    parser::{expression::Expression, functions::Function, types::VariableType},
};
use std::collections::HashMap;

pub fn load_std_console() -> HashMap<String, StandardFunction> {
    let print = StandardFunction::new(
        "print",
        vec![Function::argument("message", VariableType::String)],
        None,
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::String(msg) = &args[0] {
                println!("{msg}");
                Expression::Null
            } else {
                unreachable!()
            }
        }),
    );

    let input = StandardFunction::new(
        "input",
        vec![Function::argument("prompt", VariableType::String)],
        Some(VariableType::String),
        Box::new(|args: Vec<Expression>| -> Expression {
            let mut input = String::new();

            if let Expression::String(msg) = &args[0] {
                print!("{} ", msg);
                std::io::stdin()
                    .read_line(&mut input)
                    .expect("Failed to read input");
                Expression::String(input.trim().to_string())
            } else {
                unreachable!()
            }
        }),
    );

    // You can also create a function similar to 'print' that prints to standard error
    let eprint = StandardFunction::new(
        "eprint",
        vec![Function::argument("message", VariableType::String)],
        None,
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::String(msg) = &args[0] {
                eprintln!("{msg}");
                Expression::Null
            } else {
                unreachable!()
            }
        }),
    );

    let print_no_newline = StandardFunction::new(
        "print_no_newline",
        vec![Function::argument("message", VariableType::String)],
        None,
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::String(text) = &args[0] {
                print!("{text}");
                Expression::Null
            } else {
                unreachable!()
            }
        }),
    );

    create_function_map!(print, input, eprint, print_no_newline)
}
