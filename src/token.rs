use crate::errors::{SyntaxError, TokenisationError};

#[derive(Debug)]
pub enum TokenType {
    Identifier,
    AssignmentOperator,
    Number,
    String,
    FormatedString, //format with #{} inside `` quote 
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
    ComparisonOperator,
    LineComment,
    BlockComment,
}

#[derive(Debug)]
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
        println!("position {position}");
        let character: char = source_code.chars().nth(position).unwrap();
        println!("{}", character);

        match character {
            '+' | '-' | '/' | '*' | '%' => {
                let mut operator_lexeme = character.to_string();

                if source_code.as_bytes().get(position + 1) == Some(&(b'=' as u8)) {
                    position += 1;
                    operator_lexeme.push('=');
                    tokens.push(token(&operator_lexeme, TokenType::AssignmentOperator));
                } else {
                    tokens.push(token(&operator_lexeme, TokenType::BinaryOperator));
                }
                println!("{operator_lexeme}");
            }
            '=' => {
                let mut equal_lexeme = character.to_string();

                if source_code.as_bytes().get(position + 1) == Some(&(b'=' as u8)) {
                    position += 1;
                    equal_lexeme.push('=');
                    tokens.push(token(&equal_lexeme, TokenType::ComparisonOperator));
                } else {
                    tokens.push(token(&equal_lexeme, TokenType::AssignmentOperator));
                }
            }
            c if character.is_digit(10) => {
                let mut number_lexeme = c.to_string();

                while let Some(next_char) = source_code.chars().nth(position + 1) {
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
            },
            '"' => {
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
            },
            '`' => {},
            _ => (),
        };
        position += 1;
    }

    Ok(tokens)
}
