use crate::errors::{SyntaxError, TokenisationError};

#[derive(Debug, Clone, Copy)]
pub enum TokenType {
    Identifier,
    AssignmentOperator,
    Number,
    String,
    FormatedString, //format with #{} inside `` quote
    Boolean,
    Null,
    Object,
    Function,
    Array,
    Class,
    OpenParen,
    CloseParen,
    BinaryOperator,
    ComparisonOperator,
    LineComment,
    BlockComment,
}

#[derive(Debug, Clone)]
pub struct Token {
    value: String,
    token_type: TokenType,
}

fn token(value: &str, token_type: TokenType) -> Token {
    Token {
        value: value.to_string(),
        token_type,
    }
}

pub fn tokenize(source_code: &str) -> Result<Vec<Token>, String> {
    let mut tokens: Vec<Token> = vec![];
    let mut position = 0;

    while position < source_code.len() {
        let character: char = source_code.chars().nth(position).unwrap();

        match character {
            ' ' | '\n' | '\t' | ';' => (),
            '+' | '-' | '/' | '*' | '%' => {
                // for binary and assignement operators
                let mut operator_lexeme = character.to_string();

                if source_code.as_bytes().get(position + 1) == Some(&(b'=' as u8))
                    || (operator_lexeme == "+"
                        && source_code.as_bytes().get(position + 1) == Some(&(b'+' as u8)))
                    || (operator_lexeme == "-"
                        && source_code.as_bytes().get(position + 1) == Some(&(b'-' as u8)))
                {
                    position += 1;
                    operator_lexeme.push(source_code.as_bytes()[position] as char);
                    tokens.push(token(&operator_lexeme, TokenType::AssignmentOperator));
                } else {
                    tokens.push(token(&operator_lexeme, TokenType::BinaryOperator));
                }
            }
            '>' | '<' | '!' => {
                // for binary and assignement operators
                let mut operator_lexeme = character.to_string();

                if source_code.as_bytes().get(position + 1) == Some(&(b'=' as u8)) {
                    position += 1;
                    operator_lexeme.push('=');
                }

                tokens.push(token(&operator_lexeme, TokenType::ComparisonOperator));
            }
            '=' => {
                // for equality and value assignement
                let mut equal_lexeme = character.to_string();

                if source_code.as_bytes().get(position + 1) == Some(&(b'=' as u8))
                    || source_code.as_bytes().get(position + 1) == Some(&(b'>' as u8))
                    || source_code.as_bytes().get(position + 1) == Some(&(b'<' as u8))
                {
                    position += 1;
                    equal_lexeme.push(source_code.as_bytes()[position] as char);
                    tokens.push(token(&equal_lexeme, TokenType::ComparisonOperator));
                } else {
                    tokens.push(token(&equal_lexeme, TokenType::AssignmentOperator));
                }
            }
            c if character.is_digit(10) => {
                // for numbers
                let mut number_lexeme = c.to_string();
                position += 1;

                while let Some(next_char) = source_code.chars().nth(position) {
                    match next_char {
                        _n if next_char.is_digit(10) || next_char == '.' => {
                            position += 1;
                            number_lexeme.push(next_char);
                        }
                        ' ' | '\n' | ')' | ';' | '+' | '-' | '*' | '/' | '%' | '=' => break,
                        _ => {
                            return Err(SyntaxError::InvalidNumber {
                                line: 9999,
                                at: format!("{}{}", number_lexeme, next_char),
                            }
                            .to_string())
                        }
                    }
                }

                if number_lexeme.ends_with(".") {
                    return Err(SyntaxError::InvalidNumber {
                        line: 9999,
                        at: number_lexeme.clone(),
                    }
                    .to_string());
                }
                tokens.push(token(&number_lexeme, TokenType::Number));
            }
            '#' => {
                // for comments
                let mut comment_lexeme = character.to_string();
                position += 1;

                if source_code.as_bytes().get(position) == Some(&(b'#' as u8))
                    && source_code.as_bytes().get(position + 1) == Some(&(b'#' as u8))
                // if "###" then block comment
                {
                    position += 2;
                    comment_lexeme.push_str("##");
                    //jumping next chars and adding the ## cause we know the next two chars are #

                    while position < source_code.len() {
                        comment_lexeme.push(source_code.as_bytes()[position] as char);

                        if comment_lexeme.ends_with("###") {
                            position += 2; //to get past the two #
                            break;
                        }

                        position += 1;
                    }

                    tokens.push(token(&comment_lexeme, TokenType::BlockComment));
                } else {
                    comment_lexeme.push(source_code.as_bytes()[position] as char);

                    while position < source_code.len() {
                        comment_lexeme.push(source_code.as_bytes()[position + 1] as char);
                        position += 1;

                        if comment_lexeme.ends_with('#') {
                            return Err(SyntaxError::Comment {
                                line: 000,
                                message: format!(
                                    "Cannot start a comment in another comment: '{comment_lexeme}'"
                                ),
                            }
                            .to_string());
                        }

                        if comment_lexeme.ends_with('\n') {
                            break;
                        }
                    }

                    tokens.push(token(&comment_lexeme, TokenType::LineComment));
                }
            }
            '"' => {
                // for strings
                let mut string_lexeme = String::new();

                position += 1;

                while position < source_code.len() {
                    let c = source_code.as_bytes()[position] as char;

                    if c == '"' {
                        break;
                    }

                    string_lexeme.push(c);
                    position += 1;
                }

                tokens.push(token(&string_lexeme, TokenType::String));
            }
            '`' => todo!(),
            '(' => tokens.push(token(&character.to_string(), TokenType::OpenParen)),
            ')' => tokens.push(token(&character.to_string(), TokenType::CloseParen)),
            '[' => {
                // for arrays
                let mut array_lexeme: String = String::new();
                position += 1;

                while position < source_code.len() {
                    let c: char = source_code.as_bytes()[position] as char;

                    if c == ']' {
                        break;
                    }

                    array_lexeme.push(c);
                    position += 1;
                }

                if position >= source_code.len() {
                    return Err("Missing closing ']' for the array".to_string());
                }

                /*
                let arr_vec: Vec<Token> = array_lexeme.split(',').map(|el| {
                    let tokens: Vec<Token> = match tokenize(el) {
                        Ok(t) => t,
                        Err(e) => panic!("{}", e)
                    };

                    if tokens.len() > 1 {
                        panic!("Invalid array elements")
                    }
                    tokens[0].clone()
                }).collect::<Vec<Token>>();

                println!("{:?}", arr_vec);

                tockenize values of an array
                */

                tokens.push(token(&array_lexeme, TokenType::Array));
            }
            't' | 'f' | 'n' => {
                // for booleans and null values
                let mut value_lexeme: String = character.to_string();

                position += 1;

                while position < source_code.len() {
                    let c = source_code.as_bytes()[position] as char;

                    match c {
                        ' ' | '\n' | ';' | '+' | '-' | '*' | '/' | '%' | '=' | '"' | '#' | '`' | '(' | ')' | '[' => break,
                        _ => value_lexeme.push(c),
                    }

                    position += 1;
                }

                let token_type = match value_lexeme.as_str() {
                    "true" | "false" => TokenType::Boolean,
                    "null" => TokenType::Null,
                    _ => TokenType::Identifier,
                };

                tokens.push(token(&value_lexeme, token_type));
            }
            _ => {
                let mut value_lexeme: String = character.to_string();

                position += 1;

                while position < source_code.len() {
                    let c = source_code.as_bytes()[position] as char;

                    match c {
                        ' ' | '\n' | ')' | ';' | '+' | '-' | '*' | '/' | '%' | '=' => break,
                        _ => value_lexeme.push(c),
                    }

                    position += 1;
                }

                tokens.push(token(&value_lexeme, TokenType::Identifier));
            }
        };

        position += 1;
    }

    Ok(tokens)
}
