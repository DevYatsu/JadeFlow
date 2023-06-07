extern crate custom_error;
use custom_error::custom_error;

custom_error! {pub TokenisationError
    TypeAssertionError{line: usize, at: String} = "Type Error: '{at}' is not a valid type [at line: {line}]",
    TypeInferenceError{line:usize, at:String, assigned: String} = "Type Error: the assigned type '{assigned}' does not correspond to the type of '{at}' [at line: {line}]",
    MissingTypeError{line: usize} = "Type Error: ':' assignement operator is not followed by a valid type [at line {line}]"
}

custom_error! {pub SyntaxError
    Default{line:usize, at: String} = "Syntax Error: '{at}' is not defined [at line: {line}]",
    StartCollon{line:usize, at:String} = "Syntax Error: expression '{at}' cannot start with ':' [at line: {line}]",
    MiddleCollon{line:usize, at:String} = "Syntax Error: expression '{at}' cannot contain ':' [at line: {line}]",
    ClassInstance{line:usize, at:String} = "Syntax Error: expression '{at}' is not a valid class instance initialization [at line: {line}]"
}

pub fn find_line(src: &str, x: &str) -> usize {
    src.find(x).unwrap()
}
