mod errors;
mod token;

use std::{error::Error, fs, time::Instant};

use token::Token;

use crate::token::tokenize;

const FILE_PATH: &str = "./tests/vars.jf";

fn main() -> Result<(), Box<dyn Error>> {
    let start = Instant::now();
    let contents: String = fs::read_to_string(FILE_PATH)?;

    let tokens: Vec<Token> = tokenize(&contents)?;

    for token in tokens {

    }

    let end = Instant::now();

    println!("{} seconds to execute", (end - start).as_secs_f64());

    Ok(())
}