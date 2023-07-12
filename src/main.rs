mod errors;
mod token;

use std::{error::Error, fs, time::Instant};

use token::Token;

use crate::token::tokenize;

const FILE_PATH: &str = "./tests/vars.jf";

fn main() -> Result<(), Box<dyn Error>> {
    let contents: String = fs::read_to_string(FILE_PATH)?;

    println!("{:?}", time(&contents, tokenize)?);
    Ok(())
}

fn time(
    content: &str,
    func: impl Fn(&str) -> Result<Vec<Token>, String>,
) -> Result<Vec<Token>, String> {
    let start = Instant::now();
    let result = func(content);
    let end = Instant::now();

    println!("{} seconds to execute", (end - start).as_secs_f64());
    result
}
