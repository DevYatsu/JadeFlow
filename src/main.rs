mod evaluation;
mod jadeflow_std;
mod parser;
mod tests;
mod token;

use crate::{
    evaluation::evaluate_program,
    parser::{architecture::ASTNode, parse},
    tests::run_tests,
    token::tokenize,
};
use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        print_error!("At least one argument is required!");
        return;
    }

    if &args[1] == "tests" {
        return run_tests();
    }

    let file_name = &args[1];

    return run_script(file_name);
}

fn run_script(file_name: &str) {
    let content = match fs::read(file_name) {
        Ok(c) => c,
        Err(e) => {
            print_error!(e.to_string());
            return;
        }
    };
    let tokens = tokenize(&content);

    if tokens.is_err() {
        print_error!(tokens.unwrap_err().to_string());
        return;
    }

    let program = parse(tokens.unwrap().iter().peekable(), None);

    if program.is_err() {
        print_error!(program.unwrap_err().to_string());
        return;
    }

    let program = match program.unwrap() {
        ASTNode::Program(p) => p,
        _ => unreachable!(),
    };

    let final_table = evaluate_program(program);

    if final_table.is_err() {
        print_error!(final_table.unwrap_err().to_string());
        return;
    }
}
