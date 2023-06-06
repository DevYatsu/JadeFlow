extern crate custom_error;
use custom_error::custom_error;

custom_error!{pub TokenisationErrors
    SyntaxError{line: usize, at: String} = "Syntax Error at line {line}: '{at}'",
}