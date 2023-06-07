use crate::errors::{find_line, SyntaxError, TokenisationError};

const TYPE_ASSERTIONS: [&str; 8] = [
    "string",
    "boolean",
    "int",
    "float",
    "null",
    "undefined",
    "{}",
    "[]",
];

#[derive(Debug)]
pub enum TokenType {
    Let,
    MutLet,
    Identifier,
    TypeAssertion,
    AssignmentOperator,
    Int,
    Float,
    String,
    Boolean,
    Null,
    Undefined,
    Object,
    Function,
    Array,
    Class,
    OpenParen,
    CloseParen,
    BinaryOperator,
}

#[derive(Debug)]
pub struct Token {
    value: String,
    type_: TokenType,
}

fn token(value: &str, type_: TokenType) -> Token {
    Token {
        value: value.to_string(),
        type_,
    }
}

fn is_bool(str: &str) -> bool {
    if str == "false" || str == "true" {
        true
    } else {
        false
    }
}
fn is_float(str: &str) -> bool {
    str.parse::<f64>().ok().is_some()
}
fn is_int(str: &str) -> bool {
    str.parse::<i64>().ok().is_some()
}
fn is_string(str: &str) -> bool {
    if str.starts_with("\"") || str.starts_with("String(") || str.starts_with("new String(") {
        true
    } else {
        false
    }
}
fn is_object(str: &str) -> bool {
    if str.starts_with("{") || str.starts_with("Object(") || str.starts_with("new Object(") {
        true
    } else {
        false
    }
}
fn is_array(str: &str) -> bool {
    if str.starts_with("[") || str.starts_with("Array(") || str.starts_with("new Array(") {
        true
    } else {
        false
    }
}
fn is_fn_declaration(str: &str) -> bool {
    if str.starts_with("fn") {
        true
    } else {
        false
    }
}
fn is_class_init(str: &str) -> bool {
    if str.starts_with("new") || str.starts_with(" new") || str.starts_with("new ") {
        true
    } else {
        false
    }
}

fn is_type_coherent(type_assertion: &str, value: &str) -> bool {
    match type_assertion {
        "" => true,
        "string" => is_string(value),
        "int" => is_int(value),
        "float" => is_float(value),
        "boolean" => is_bool(value),
        "{}" => {
            if is_object(value) {
                return true;
            }
            if is_class_init(value) {
                return true;
            }
            false
        }
        "[]" => is_array(value),
        "null" => value == "null",
        "undefined" => value == "undefined",
        _ => false,
    }
}

pub fn tokenize(source_code: String) -> Result<Vec<Token>, String> {
    let mut tokens: Vec<Token> = vec![];
    let formatted_src: String = source_code
        .replace("\n", "")
        .replace(";", " ")
        .replace("  ", " ");
    let mut src: Vec<&str> = formatted_src.split(" ").filter(|x| x != &"").collect();

    let mut is_after_let: bool = false;
    let mut is_after_fn: bool = false;
    let mut is_a_type: bool = false;
    let mut is_after_new: bool = false;

    let mut type_assertion: &str = "";

    while !src.is_empty() {
        let val: &str = src.remove(0);

        match val {
            "(" => tokens.push(token(val, TokenType::OpenParen)),
            ")" => tokens.push(token(val, TokenType::CloseParen)),
            "new" => is_after_new = true,
            "let" | "nl" => {
                if is_after_new {
                    return Err(SyntaxError::ClassInstance {
                        line: find_line(&source_code, val),
                        at: format!("new {}", val.replace("\"", "")),
                    }
                    .to_string());
                }
                tokens.push(token(val, TokenType::Let));
                is_after_let = true
            }
            "letmut" | "ml" => {
                if is_after_new {
                    return Err(SyntaxError::ClassInstance {
                        line: find_line(&source_code, val),
                        at: format!("new {}", val.replace("\"", "")),
                    }
                    .to_string());
                }
                tokens.push(token(val, TokenType::MutLet));
                is_after_let = true
            }
            "+" | "-" | "*" | "/" | "%" => {
                if is_after_new {
                    return Err(SyntaxError::ClassInstance {
                        line: find_line(&source_code, val),
                        at: format!("new {}", val.replace("\"", "")),
                    }
                    .to_string());
                };
                tokens.push(token(val, TokenType::BinaryOperator))
            }
            "=" | "+=" | "-=" | "*=" | "/=" | "%=" => {
                if is_a_type {
                    return Err(TokenisationError::MissingTypeError {
                        line: find_line(&source_code, val),
                    }
                    .to_string());
                }
                if is_after_new {
                    return Err(SyntaxError::ClassInstance {
                        line: find_line(&source_code, val),
                        at: format!("new {}", val.replace("\"", "")),
                    }
                    .to_string());
                }

                tokens.push(token(val, TokenType::AssignmentOperator))
            }
            "true" | "false" => {
                if is_after_new {
                    return Err(SyntaxError::ClassInstance {
                        line: find_line(&source_code, val),
                        at: format!("new {}", val.replace("\"", "")),
                    }
                    .to_string());
                }

                tokens.push(token(val, TokenType::Boolean));
                if !is_type_coherent(type_assertion, val) {
                    return Err(TokenisationError::TypeAssertionError {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
                    }
                    .to_string());
                }
                type_assertion = "";
            }
            "null" => {
                if is_after_new {
                    return Err(SyntaxError::ClassInstance {
                        line: find_line(&source_code, val),
                        at: format!("new {}", val.replace("\"", "")),
                    }
                    .to_string());
                }

                tokens.push(token(val, TokenType::Null));
                if !is_type_coherent(type_assertion, val) {
                    return Err(TokenisationError::TypeAssertionError {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
                    }
                    .to_string());
                }
                type_assertion = "";
            }
            "undefined" => {
                if is_after_new {
                    return Err(SyntaxError::ClassInstance {
                        line: find_line(&source_code, val),
                        at: format!("new {}", val.replace("\"", "")),
                    }
                    .to_string());
                }

                tokens.push(token(val, TokenType::Undefined));
                if !is_type_coherent(type_assertion, val) {
                    return Err(TokenisationError::TypeAssertionError {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
                    }
                    .to_string());
                }
                type_assertion = "";
            }
            ":" | ": " | " :" => {
                if is_after_new {
                    return Err(SyntaxError::ClassInstance {
                        line: find_line(&source_code, val),
                        at: format!("new {}", val.replace("\"", "")),
                    }
                    .to_string());
                };
                is_a_type = true;
            }
            x => {
                if is_a_type {
                    //retrieves the type assigned by the user to the variable
                    if !TYPE_ASSERTIONS.contains(&val) {
                        return Err(TokenisationError::TypeAssertionError {
                            line: find_line(&source_code, x),
                            at: x.to_string(),
                        }
                        .to_string());
                    }

                    type_assertion = val;
                    tokens.push(token(val, TokenType::TypeAssertion));
                    is_a_type = false;
                } else if is_after_new {
                    if !x.contains("(") || !x.contains(")") {
                        return Err(SyntaxError::ClassInstance {
                            line: find_line(&source_code, x),
                            at: format!("new {}", x.replace("\"", "'")),
                        }
                        .to_string());
                    }

                    tokens.push(token(val, TokenType::Class));

                    if !is_type_coherent(type_assertion, "new") {
                        return Err(TokenisationError::TypeInferenceError {
                            line: find_line(&source_code, val),
                            at: val.to_string(),
                            assigned: type_assertion.to_string(),
                        }
                        .to_string());
                    }
                    type_assertion = "";
                    is_after_new = false;
                } else if is_int(x) {
                    tokens.push(token(val, TokenType::Int));
                    if !is_type_coherent(type_assertion, val) {
                        return Err(TokenisationError::TypeInferenceError {
                            line: find_line(&source_code, val),
                            at: val.to_string(),
                            assigned: type_assertion.to_string(),
                        }
                        .to_string());
                    }
                    type_assertion = "";
                } else if is_float(x) {
                    tokens.push(token(val, TokenType::Float));

                    if !is_type_coherent(type_assertion, val) {
                        return Err(TokenisationError::TypeInferenceError {
                            line: find_line(&source_code, val),
                            at: val.to_string(),
                            assigned: type_assertion.to_string(),
                        }
                        .to_string());
                    }
                    type_assertion = "";
                } else if is_after_let || is_after_fn {
                    if is_after_let && x.starts_with(":") {
                        return Err(SyntaxError::StartCollon {
                            line: find_line(&source_code, x),
                            at: x.to_string(),
                        }
                        .to_string());
                    } else if is_after_let && x.contains(":") {
                        let vec: Vec<&str> = x.split(":").collect();

                        if vec.len() > 2 {
                            return Err(SyntaxError::MiddleCollon {
                                line: find_line(&source_code, x),
                                at: x.to_string(),
                            }
                            .to_string());
                        }

                        if x.ends_with(":") {
                            is_a_type = true;
                            tokens.push(token(&val[..val.len() - 1], TokenType::Identifier));
                            is_after_let = false;
                            continue;
                        }

                        if !TYPE_ASSERTIONS.contains(&vec[1]) {
                            return Err(TokenisationError::TypeAssertionError {
                                line: find_line(&source_code, vec[1]),
                                at: vec[1].to_string(),
                            }
                            .to_string());
                        }

                        tokens.push(token(vec[0], TokenType::Identifier));
                        tokens.push(token(vec[1], TokenType::TypeAssertion));

                        type_assertion = vec[1];
                    } else {
                        tokens.push(token(val, TokenType::Identifier));
                    }

                    is_after_fn = false;
                    is_after_let = false;
                } else if is_string(x) {
                    tokens.push(token(val, TokenType::String));

                    if !is_type_coherent(type_assertion, val) {
                        return Err(TokenisationError::TypeInferenceError {
                            line: find_line(&source_code, val),
                            at: val.to_string(),
                            assigned: type_assertion.to_string(),
                        }
                        .to_string());
                    }
                    type_assertion = "";
                } else if is_object(x) {
                    tokens.push(token(val, TokenType::Object));

                    if !is_type_coherent(type_assertion, val) {
                        return Err(TokenisationError::TypeInferenceError {
                            line: find_line(&source_code, val),
                            at: val.to_string(),
                            assigned: type_assertion.to_string(),
                        }
                        .to_string());
                    }

                    type_assertion = "";
                } else if is_array(x) {
                    tokens.push(token(val, TokenType::Array));

                    if !is_type_coherent(type_assertion, val) {
                        return Err(TokenisationError::TypeInferenceError {
                            line: find_line(&source_code, val),
                            at: val.to_string(),
                            assigned: type_assertion.to_string(),
                        }
                        .to_string());
                    }
                    type_assertion = "";
                } else if is_fn_declaration(x) {
                    tokens.push(token(val, TokenType::Function));
                    is_after_fn = true;
                } else {
                    return Err(SyntaxError::Default {
                        line: find_line(&source_code, x),
                        at: x.to_string(),
                    }
                    .to_string());
                }
            }
        }
    }

    Ok(tokens)
}
