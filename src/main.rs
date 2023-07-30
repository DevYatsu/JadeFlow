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

    let first_timer = (end - start).as_secs_f64();
    println!(
        "{} seconds to execute tokenisation",
        first_timer
    );

    let start = Instant::now();
    let program = parse(&mut tokens)?;
    let end = Instant::now();

    println!("{:?}", program);

    let second_timer = (end - start).as_secs_f64();
    println!("{} seconds to execute parsing", second_timer);

    println!("total time {}", first_timer + second_timer);
    Ok(())
}
