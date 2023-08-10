mod parser;
mod select_test;
mod token;

use crate::{
    parser::{architecture::ASTNode, parse, ParsingError},
    select_test::run_file,
    token::{errors::SyntaxError, tokenize, Token},
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
    let content = match fs::read(file_name) {
        Ok(c) => c,
        Err(e) => {
            println!("ERROR: {}", e.to_string());
            return;
        }
    };

    let tokenization = time_execution("tokenization", || -> Result<Vec<Token>, SyntaxError> {
        let tokens = tokenize(&content)?;

        Ok(tokens)
    });

    if tokenization.data.is_err() {
        println!("{}", tokenization.data.unwrap_err().to_string());
        return;
    }

    let parsing = time_execution("parsing", || -> Result<ASTNode, ParsingError> {
        let program = parse(tokenization.data.unwrap().iter().peekable(), None)?;

        match &program {
            ASTNode::Program(p) => {
                println!("symbol table: \n {}", p.symbol_table);
                println!("statements number: {}", p.statements.len());
            }
            _ => unreachable!(),
        }

        Ok(program)
    });

    if parsing.data.is_err() {
        println!("{}", parsing.data.unwrap_err().to_string());
        return;
    }

    println!("total time: {}s", tokenization.duration + parsing.duration);
}

fn time_execution<F, R>(name: &str, code: F) -> TimerData<R>
where
    F: FnOnce() -> R,
{
    let start = Instant::now();
    let returned_val = code();
    let end = Instant::now();
    let duration = (end - start).as_secs_f64();
    println!("{}: {}s", name, duration);

    TimerData::new(returned_val, duration)
}

#[derive(Clone, Debug)]
struct TimerData<D> {
    pub duration: f64,
    pub data: D,
}

impl<D> TimerData<D> {
    pub fn new(data: D, duration: f64) -> TimerData<D> {
        TimerData { duration, data }
    }
}
