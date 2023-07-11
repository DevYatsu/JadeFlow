mod errors;
mod token;

use std::{error::Error, fs};

use crate::token::tokenize;

const FILE_PATH: &str = "./tests/vars.jf";

fn main() -> Result<(), Box<dyn Error>> {
    let contents: String = fs::read_to_string(FILE_PATH)?;

    println!("{:?}", tokenize(&contents)?);
    Ok(())
}
