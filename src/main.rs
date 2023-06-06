mod token;

use std::fs;

use crate::token::tokenize;

const FILE_PATH: &str = "./tests/vars.jf";

fn main() {
    let contents = fs::read_to_string(FILE_PATH)
        .expect("Should have been able to read the file");


    println!("{:?}", tokenize(contents).unwrap())
}
