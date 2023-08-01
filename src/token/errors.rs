use custom_error::custom_error;

custom_error! {pub TokenisationError
    TypeAssertionError{line: usize, at: String} = "Type Error: '{at}' is not a valid type [at line: {line}]",
    TypeInferenceError{line:usize, at:String, assigned: String} = "Type Error: the assigned type '{assigned}' does not correspond to the type of '{at}' [at line: {line}]",
    MissingTypeError{line: usize} = "Type Error: ':' assignement operator is not followed by a valid type [at line {line}]"
}

custom_error! {pub SyntaxError
    UndefinedVar{line:usize, at: String} = "Syntax Error: '{at}' is not defined [at line: {line}]",
    StartCollon{line:usize, at:String} = "Syntax Error: expression '{at}' cannot start with ':' [at line: {line}]",
    MiddleCollon{line:usize, at:String} = "Syntax Error: expression '{at}' cannot contain ':' [at line: {line}]",
    ClassInstance{line:usize, at:String} = "Syntax Error: expression '{at}' is not a valid class instance initialization [at line: {line}]",
    InvalidNumber{line:usize, at:String} = "Syntax Error: Invalid number '{at}' [at line: {line}]",
    Comment{line:usize, message:String} = "Syntax Error: {message} [at line: {line}]",
    InvalidArray{line:usize, at:String} = "Syntax Error: Invalid Array '{at}' [at line: {line}]",
    NonAlphabeticCharacter = "Character must be alphabetic",
    UnclosedString = "Unclosed string literal"
}
