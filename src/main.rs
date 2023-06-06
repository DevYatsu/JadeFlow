mod token;
mod errors;

use std::{fs, error::Error};

use crate::token::tokenize;

const FILE_PATH: &str = "./tests/vars.jf";

fn main() -> Result<(), Box<dyn Error>> {
    let contents: String = fs::read_to_string(FILE_PATH)?;

    println!("{:?}", tokenize(contents)?);
    Ok(())
}
