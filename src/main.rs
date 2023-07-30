mod errors;
mod parser;
mod select_test;
mod token;

use std::{error::Error, fs, time::Instant};

use token::Token;

use crate::{parser::parse, select_test::run_file, token::tokenize};

fn main() -> Result<(), Box<dyn Error>> {
    let contents: String = fs::read_to_string(run_file()?)?;

    let start = Instant::now();
    let mut tokens: Vec<Token> = tokenize(&contents)?;
    let end = Instant::now();

    for token in &tokens {
        println!("{:?}", token)
    }

    println!(
        "{} seconds to execute tokenisation",
        (end - start).as_secs_f64()
    );

    let start = Instant::now();
    let program = parse(&mut tokens)?;
    let end = Instant::now();

    println!("{:?}", program);

    println!("{} seconds to execute parsing", (end - start).as_secs_f64());

    Ok(())
}
