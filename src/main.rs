mod errors;
mod token;

use std::{error::Error, fs, time::Instant};

use token::Token;

use crate::token::tokenize;

const FILE_PATH: &str = "./tests/vars.jf";

fn main() -> Result<(), Box<dyn Error>> {
    let contents: String = fs::read_to_string(FILE_PATH)?;
    
    let start = Instant::now();
    let tokens: Vec<Token> = tokenize(&contents)?;
    let end = Instant::now();

    for token in tokens {
        println!("{:?}", token)
    }
    

    println!("{} seconds to execute", (end - start).as_secs_f64());

    Ok(())
}