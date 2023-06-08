use crate::errors::{find_line, SyntaxError, TokenisationError};
use regex::Regex;

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

#[derive(Debug, Clone, Copy, PartialEq)]
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
    Separator,
}

#[derive(Debug, Clone)]
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
    str == "false" || str == "true"
}
fn is_float(str: &str) -> bool {
    str.parse::<f64>().is_ok()
}
fn is_int(str: &str) -> bool {
    str.parse::<i64>().is_ok()
}
fn is_string(str: &str) -> bool {
    str.starts_with('\"') || str.starts_with("String(") || str.starts_with("new String(")
}
fn is_object(str: &str) -> bool {
    str.starts_with('{') || str.starts_with("Object(") || str.starts_with("new Object(")
}
fn is_array(str: &str) -> bool {
    str.starts_with('[') || str.starts_with("Array(") || str.starts_with("new Array(")
}
fn is_fn_declaration(str: &str) -> bool {
    str.starts_with("fn")
}
fn is_class_init(str: &str) -> bool {
    str.starts_with("new") || str.starts_with(" new") || str.starts_with("new ")
}

fn is_type_coherent(type_assertion: &str, value: &str) -> bool {
    match type_assertion {
        "" => true,
        "string" => is_string(value),
        "int" => is_int(value),
        "float" => is_float(value),
        "boolean" => is_bool(value),
        "{}" => {
            if is_object(value) || is_class_init(value) {
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

fn replace_sequence_outside_quotes(input: &str, sequence: &str, replacement: &str) -> String {
    let mut output = String::new();
    let mut inside_quotes = false;

    let sequence_chars: Vec<char> = sequence.chars().collect();
    let sequence_len = sequence_chars.len();

    for (i, c) in input.chars().enumerate() {
        if c == '"' {
            inside_quotes = !inside_quotes;
        }

        if i + sequence_len <= input.len() {
            let current_sequence: String = input.chars().skip(i).take(sequence_len).collect();
            if current_sequence == sequence && !inside_quotes {
                output.push_str(replacement);
                continue; // Skip the sequence if not inside quotes
            }
        }
        println!("{}",c);
        output.push(c);
    }

    output
}


pub fn tokenize(source_code: String) -> Result<Vec<Token>, String> {
    let reg = Regex::new(r#"(?:"[^"]*[";])|\b(\+=|\-=|\*=|/=|%=|=[^;=])\b"#).unwrap();

    // println!("{}", replace_sequence_outside_quotes(&source_code, "=", " = "));

    let formatted_src: String = reg.replace_all(&source_code, |caps: &regex::Captures<'_>| {
        caps[0].to_string().replace("=", " = ")
    })
        .replace(";", " ; ").replace("  ", " ").to_string();

    let mut src: Vec<&str> = formatted_src
        .split_whitespace()
        .filter(|x| !x.is_empty())
        .collect();

    let mut tokens: Vec<Token> = vec![];

    let mut is_after_let: bool = false;
    let mut is_after_fn: bool = false;
    let mut is_a_type: bool = false;
    let mut is_after_new: bool = false;
    let mut is_after_semicollon: bool = false;
    let mut is_after_identifier: bool = false;

    let mut last_variable_type: Option<TokenType> = None;
    let mut is_after_collon: bool = false;

    let mut type_assertion: &str = "";

    while !src.is_empty() {
        let val: &str = src.remove(0);

        match val {
            ";" => {
                if is_after_let {
                    return Err(SyntaxError::MissingVariableName {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
                    }
                    .to_string());
                }

                if is_after_new {
                    return Err(SyntaxError::ClassInstance {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
                    }
                    .to_string());
                }

                if is_after_semicollon {
                    return Err(SyntaxError::SeveralSemiCollon {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
                    }
                    .to_string());
                }

                if is_after_identifier {
                    tokens.push(token("=", TokenType::AssignmentOperator));
                    tokens.push(token("undefined", TokenType::Undefined));
                }

                tokens.push(token(val, TokenType::Separator));
                is_after_semicollon = true;
                last_variable_type = None;
            }
            "(" => {
                if is_after_let {
                    return Err(SyntaxError::ForbiddenIdentifierName {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
                    }
                    .to_string());
                }
                tokens.push(token(val, TokenType::OpenParen));
            }
            ")" => {
                if is_after_let {
                    return Err(SyntaxError::ForbiddenIdentifierName {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
                    }
                    .to_string());
                }
                tokens.push(token(val, TokenType::CloseParen));
            }
            "new" => {
                if is_after_let {
                    return Err(SyntaxError::ForbiddenIdentifierName {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
                    }
                    .to_string());
                };
                is_after_new = true
            }
            "let" | "nl" => {
                if is_after_let {
                    return Err(SyntaxError::ForbiddenIdentifierName {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
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
                tokens.push(token(val, TokenType::Let));
                is_after_let = true;
                is_after_semicollon = false;
                last_variable_type = Some(TokenType::Let);
            }
            "letmut" | "ml" => {
                if is_after_let {
                    return Err(SyntaxError::ForbiddenIdentifierName {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
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
                tokens.push(token(val, TokenType::MutLet));
                is_after_let = true;
                is_after_semicollon = false;
                last_variable_type = Some(TokenType::MutLet);
            }
            "+" | "-" | "*" | "/" | "%" => {
                if is_after_let {
                    return Err(SyntaxError::ForbiddenIdentifierName {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
                    }
                    .to_string());
                }

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
                if is_after_let {
                    return Err(SyntaxError::MissingVariableName {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
                    }
                    .to_string());
                }

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

                tokens.push(token(val, TokenType::AssignmentOperator));
                is_after_identifier = false;
            }
            "true" | "false" | "true," | "false," => {
                if is_after_let {
                    return Err(SyntaxError::ForbiddenIdentifierName {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
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

                if val.ends_with(",") {
                    tokens.push(token(&val[0..val.len() - 1], TokenType::Boolean));
                    is_after_collon = true;
                } else {
                    tokens.push(token(val, TokenType::Boolean));
                }
                if !is_type_coherent(type_assertion, val) {
                    return Err(TokenisationError::TypeAssertionError {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
                    }
                    .to_string());
                }
                type_assertion = "";
            }
            "null" | "null," => {
                if is_after_let {
                    return Err(SyntaxError::ForbiddenIdentifierName {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
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

                if val.ends_with(",") {
                    tokens.push(token(&val[0..val.len() - 1], TokenType::Null));
                    is_after_collon = true;
                } else {
                    tokens.push(token(val, TokenType::Null));
                }

                if !is_type_coherent(type_assertion, val) {
                    return Err(TokenisationError::TypeAssertionError {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
                    }
                    .to_string());
                }
                type_assertion = "";
            }
            "undefined" | "undefined," => {
                if is_after_let {
                    return Err(SyntaxError::ForbiddenIdentifierName {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
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

                if val.ends_with(",") {
                    tokens.push(token(&val[0..val.len() - 1], TokenType::Undefined));
                    is_after_collon = true;
                } else {
                    tokens.push(token(val, TokenType::Undefined));
                }
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
                if is_after_let {
                    return Err(SyntaxError::ForbiddenIdentifierName {
                        line: find_line(&source_code, val),
                        at: val.to_string(),
                    }
                    .to_string());
                }

                if is_after_new {
                    return Err(SyntaxError::ClassInstance {
                        line: find_line(&source_code, val),
                        at: format!("new {}", val.replace("\"", "")),
                    }
                    .to_string());
                };
                is_a_type = true;
            }
            mut x => {
                if is_after_collon {
                    if let Some(x) = &last_variable_type {
                        tokens.push(token(";", TokenType::Separator));
                        tokens.push(token(
                            if x == &TokenType::Let {
                                "let"
                            } else {
                                "letmut"
                            },
                            *x,
                        ));
                        is_after_let = true;
                        is_after_collon = false;
                    }
                    //next things are managed
                }

                if x.ends_with(",") && !is_a_type {
                    x = &x[0..x.len() - 1];
                    is_after_collon = true;
                }

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
                    if is_after_let {
                        return Err(SyntaxError::ForbiddenIdentifierName {
                            line: find_line(&source_code, val),
                            at: val.to_string(),
                        }
                        .to_string());
                    }

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
                    if is_after_let {
                        return Err(SyntaxError::ForbiddenIdentifierName {
                            line: find_line(&source_code, val),
                            at: val.to_string(),
                        }
                        .to_string());
                    }

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
                            is_after_identifier = true;
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
                    } else if is_after_let {
                        tokens.push(token(val, TokenType::Identifier));
                        is_after_identifier = true;
                    } else {
                        tokens.push(token(val, TokenType::Identifier));
                        is_after_identifier = true;
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
                    if is_after_let {
                        return Err(SyntaxError::ForbiddenIdentifierName {
                            line: find_line(&source_code, val),
                            at: val.to_string(),
                        }
                        .to_string());
                    }

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
                    if is_after_let {
                        return Err(SyntaxError::ForbiddenIdentifierName {
                            line: find_line(&source_code, val),
                            at: val.to_string(),
                        }
                        .to_string());
                    }

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
