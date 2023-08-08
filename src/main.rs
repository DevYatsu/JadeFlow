mod parser;
mod select_test;
mod token;

use crate::{
    parser::{architecture::ASTNode, parse},
    select_test::run_file,
    token::tokenize,
};
use std::{fs, time::Instant};

fn main() {
    let file_name = match run_file() {
        Ok(f) => f,
        Err(e) => {
            println!("ERROR: {}", e.to_string());
            return;
        }
    };
    let content = match fs::read(file_name) {
        Ok(c) => c,
        Err(e) => {
            println!("ERROR: {}", e.to_string());
            return;
        }
    };

    let start = Instant::now();
    let tokens = match tokenize(&content) {
        Ok(t) => t,
        Err(e) => {
            println!("ERROR: {}", e.to_string());
            return;
        }
    };
    let end = Instant::now();
    let tokens_iter = tokens.iter().peekable();

    for token in &tokens {
        println!("{:?}", token)
    }

    let first_timer = (end - start).as_secs_f64();
    println!("tokenization: {}s", first_timer);

    let start = Instant::now();
    let program = match parse(tokens_iter, None) {
        Ok(p) => p,
        Err(e) => {
            println!("ERROR: {}", e.to_string());
            return;
        }
    };
    let end = Instant::now();

    match program {
        ASTNode::Program(p) => {
            println!("symbol table: \n {}", p.symbol_table);
            println!("statements number: {}", p.statements.len());
        }
        _ => unreachable!(),
    }

    let second_timer = (end - start).as_secs_f64();
    println!("parsing: {}s", second_timer);

    println!("total time: {}s", first_timer + second_timer);
}
