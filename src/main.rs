mod evaluation;
mod jadeflow_std;
mod parser;
mod select_test;
mod token;

use crate::{
    evaluation::{evaluate_program, EvaluationError},
    parser::{architecture::ASTNode, errors::ParsingError, parse},
    select_test::run_file,
    token::{errors::SyntaxError, tokenize, Token},
};
use std::{fs, time::Instant};

fn main() {
    let file_name = match run_file() {
        Ok(f) => f,
        Err(e) => {
            print_error!(e.to_string());
            return;
        }
    };
    let content = match fs::read(file_name) {
        Ok(c) => c,
        Err(e) => {
            print_error!(e.to_string());
            return;
        }
    };

    let tokenization = time_execution("tokenization", || -> Result<Vec<Token>, SyntaxError> {
        let tokens = tokenize(&content)?;

        Ok(tokens)
    });

    if tokenization.data.is_err() {
        print_error!(tokenization.data.unwrap_err().to_string());
        return;
    }

    let parsing = time_execution("parsing", || -> Result<ASTNode, ParsingError> {
        let program = parse(tokenization.data.unwrap().iter().peekable(), None)?;

        Ok(program)
    });

    if parsing.data.is_err() {
        print_error!(parsing.data.unwrap_err().to_string());
        return;
    }

    let program = match parsing.data.unwrap() {
        ASTNode::Program(p) => p,
        _ => unreachable!(),
    };

    let evaluation = time_execution("evaluation", || -> Result<(), EvaluationError> {
        let final_table = evaluate_program(program)?;

        println!("{}", final_table);

        Ok(())
    });

    if evaluation.data.is_err() {
        print_error!(evaluation.data.unwrap_err().to_string());
        return;
    }

    print_info!(
        "total time: {}s",
        tokenization.duration + parsing.duration + evaluation.duration
    );
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
