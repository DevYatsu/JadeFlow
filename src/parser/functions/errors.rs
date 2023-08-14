use custom_error::custom_error;

use crate::parser::types::VariableType;

custom_error! {pub FunctionParsingError
    ExpectedIdentifier = "Expected a function name after the 'fn' keyword",
    ExpectedOpenParen{name: String} = "Expected parentheses after '{name}' in the function declaration",

    ExpectedArgName = "Expected a valid function argument name",
    ExpectedArgColon{arg_name: String} = "Expected a type declaration after '{arg_name}'",
    ExpectedArgType{arg_name: String} = "Missing type declaration for argument '{arg_name}'",
    ExpectedCommaBetweenArgs{arg_name: String, arg_type: String, fn_name: String} = "Expected ',' after '{arg_name}: {arg_type}' in the '{fn_name}' function arguments",
    ExpectedCloseParen{arg_name: String, arg_type: String, fn_name: String} = "Expected ')' after '{arg_name}: {arg_type}' in the '{fn_name}' function arguments",

    ExpectedReturnType{fn_name: String} = "Expected a return type for the '{fn_name}' function",
    ExpectedValidReturnType{fn_name: String} = "Expected a valid return type for the '{fn_name}' function",

    ExpectedBrace{fn_name: String} = "Expected the function body after the function arguments in the '{fn_name}' function",

    NameAlreadyTaken{name: String} = "The function name '{name}' is already in use",
NameAlreadyTakenByStd{name: String} = "The function name '{name}' is already in use by a standard library function",
    ExpectingExpressionAfterArrow{fn_name: String} = "Expecting expression after '=>' in \"{fn_name}\" declaration",

    MissingReturnType{fn_name: String} = "Missing a return type in \"{fn_name}\" function declaration",
    MissingReturnStatement{fn_name: String} = "Missing a return statement in \"{fn_name}\" function declaration",
    ReturnTypeInvalid{fn_name: String, return_type: String, found: String} = "Return Type '{return_type}' of function \"{fn_name}\" does not correspond to returned type '{found}'",

    NotDefinedFunction{fn_name: String} = "\"{fn_name}\" does not correspond to any defined function",

    MissingClosingParenInFnCall{fn_name: String} = "Missing a closing parenthesis in \"{fn_name}\" call",
    InvalidFnCallArg{fn_name: String, err: String} = "Invalid \"{fn_name}\" call arguments: {err}",
    InvalidFnCallArgType{fn_name: String, arg_name: String, required_t: VariableType, found_t: VariableType} = "In \"{fn_name}\" call, '{arg_name}' must be of type {required_t} whereas a value of type {found_t} was provided!",
    InvalidFnCallArgNumber{fn_name: String, required_num: usize, found_num: usize} = "\"{fn_name}\" requires {required_num} arguments whereas {found_num} were provided!",
}
