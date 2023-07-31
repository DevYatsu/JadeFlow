mod parser;
mod select_test;
mod token;

use std::{fs, time::Instant};

use token::Token;

use crate::{parser::parse, select_test::run_file, token::tokenize};

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
    let mut tokens: Vec<Token> = match tokenize(&contents) {
        Ok(t) => t,
        Err(e) => {
            println!("ERROR: {}", e.to_string());
            return;
        }
    };
    let end = Instant::now();

    for token in &tokens {
        println!("{:?}", token)
    }

    let first_timer = (end - start).as_secs_f64();
    println!("{} seconds to execute tokenisation", first_timer);

    let start = Instant::now();
    let program = match parse(&mut tokens) {
        Ok(p) => p,
        Err(e) => {
            println!("ERROR: {}", e.to_string());
            return;
        }
    };
    let end = Instant::now();

    println!("{:?}", program);
    let second_timer = (end - start).as_secs_f64();
    println!("{} seconds to execute parsing", second_timer);

    println!("total time: {}", first_timer + second_timer);
}
