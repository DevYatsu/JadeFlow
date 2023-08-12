use std::{io::Error, num::ParseIntError};

use super::{
    class::ClassError,
    expression::Expression,
    functions::errors::FunctionParsingError,
    types::{TypeError, VariableType},
};
#[macro_export]
macro_rules! print_error {
    ( $msg:expr ) => {
        println!("\x1b[31mError:\x1b[0m {}", $msg);
    };
    ( $( $arg:expr ),* ) => {
        println!("\x1b[31mError:\x1b[0m {}", format!($( $arg ),*));
    };
}
#[macro_export]
macro_rules! print_warning {
    ( $msg:expr ) => {
        println!("\x1b[33mWarning:\x1b[0m {}", $msg);
    };
    ( $( $arg:expr ),* ) => {
        println!("\x1b[33mWarning:\x1b[0m {}", format!($( $arg ),*));
    };
}
#[macro_export]
macro_rules! print_info {
    ( $msg:expr ) => {
        println!("\x1b[35mInfo:\x1b[0m {}", $msg);
    };    
    ( $( $arg:expr ),* ) => {
        println!("\x1b[35mInfo:\x1b[0m {}", format!($( $arg ),*))
    };
}

custom_error::custom_error! {pub ParsingError
    Io{source: Error} = "{source}",
    ParsingTypyError{source: TypeError} = "{source}",
    ParseInt{source: ParseIntError} = "{source}",
    FunctionParsingError{source: FunctionParsingError} = "{source}",
    ClassError{source: ClassError} = "{source}",
    Custom{data: String} = "{data}",

    Default = "Failed to parse tokens",
    SpecialStringOnlyAtFileStart = "Special strings ('') can only appear at the start of the file",
    InvalidSpecialStringContent{invalid_content: String} = "Special strings ('') content is invalid. Supported values: 'strict' and 'soft', found '{invalid_content}'",
    RequiredNewLineAfterSpecialString = "Special strings ('') require to be followed by another special string or nothing",

    UnexpectedEndOfInput = "No more token left to parse",
    ExpectedVarInitialization{var_value: String, found: String} = "Expected identifier after \"{var_value}\", found '{found}'",
    ExpectedReassignment{var_name: String} = "Expected assignment of new value to \"{var_name}\"",
    UnknownVariableType{var_name: String} = "Impossible to guess {var_name} type",
    CannotReassignIfNotAssigned{operator: String, var_name: String} = "Must first assign value to \"{var_name}\" with '=' before using '{operator}'",
    CannotReassignConst{var_name: String} = "Cannot reassign \"{var_name}\" as it is a 'const'",
    InvalidNumber{value: String} = "\"{value}\" is an invalid number!",
    InvalidExpression{value: String} = "\"{value}\" is an invalid expression!",
    MissingClosingParenthesis = "Expected a closing parenthesis!",
    ExpectedSeparator{value: String} = "Expected new line or semicolon after \"{value}\"",
    MissingKeyValueDict = "Dictionary is missing a key value",
    MissingValueDict{key: String} = "Dictionary is missing a value for key \"{key}\"",
    InvalidKeyDict{key_type: Expression} = "\"{key_type}\" is not a handled key variant",
    ExpectedStringAsDictKey = "Expected string as dictionary key",
    AssignedTypeNotFound{assigned_t: VariableType, found_t: VariableType} = "Assigned type '{assigned_t}' is different from found type '{found_t}'",
    InvalidStringDictKey{key: String} = "Expected a string or number as dictionary key, not '{key}'",
    UseOfUndefinedVariable{name: String} = "\"{name}\" is not defined",
    UnexpectedToken{expected: String, found: String} = "Expected '{expected}', found '{found}'",
    CannotReassignVar{name: String} = "Cannot reassign \"{name}\" as it is not defined",
    CannotChangeAssignedType{assigned_t: VariableType, found_t: VariableType, var_name: String, at: String} = "Expected type '{assigned_t}', found '{found_t}' for '{var_name}' at: {at}",

    ExpectedIdentifier{after: String} = "Expected identifier after: '{after}'",
    MissingInitializer{keyword: String, name: String} = "Missing initializer for {keyword} {name}",
    IncompleteDeclaration{keyword: String, name: String} = "Incomplete declaration: {keyword} {name} requires a value assignment",
    IncompleteReassagnment{keyword: String, name: String} = "Incomplete reassignment: {keyword} {name} requires a value assignment",

    ExpectedSomething = "Expected an expression, found nothing",
    ExpectedValidExpressionInFormattedString = "Expected a valid expression in formatted string argument",

    TypeInvalidExpressionElement{expr: Expression} = "Cannot use a type as an expression value: {expr}",

    ExpectedValidVectorIndex{found: String} = "Expected valid vector index, found {found}",
    ExpectedBracketAfterVectorIndex{found: String} = "Expected ']' after vector index, found {found}",

    UnwantedColon = "Type annotation only allowed on variable initialization",

    CannotReassignNotDefinedDictProp{operator: String, prop: String} = "Cannot reassign with {operator} operator {prop} property",

    InvalidDict{name: String} = "'{name}' is not a valid object"
}
