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
    let contents: String = match fs::read_to_string(file_name) {
        Ok(c) => c,
        Err(e) => {
            println!("ERROR: {}", e.to_string());
            return;
        }
    };

    let start = Instant::now();
    let tokens = match tokenize(&contents) {
        Ok(t) => t,
        Err(e) => {
            println!("ERROR: {}", e.to_string());
            return;
        }
    };
    let tokens_iter = tokens.iter().peekable();
    let end = Instant::now();

    for token in &tokens {
        println!("{:?}", token)
    }

    let first_timer = (end - start).as_secs_f64();
    println!("{} seconds to execute tokenisation", first_timer);

    let start = Instant::now();
    let program = match parse(tokens_iter) {
        Ok(p) => p,
        Err(e) => {
            println!("ERROR: {}", e.to_string());
            return;
        }
    };
    let end = Instant::now();

    match program {
        ASTNode::Program(p) => {
            for s in &p.statements {
                println!("{:?}", s);
            }
            println!("statements number: {}", p.statements.len());
        }
        _ => unreachable!(),
    }

    let second_timer = (end - start).as_secs_f64();
    println!("{} seconds to execute parsing", second_timer);

    println!("total time: {}", first_timer + second_timer);
}
