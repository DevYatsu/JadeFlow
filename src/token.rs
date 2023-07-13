use crate::errors::{SyntaxError, TokenisationError};

#[derive(Debug, Clone, Copy)]
pub enum TokenType {
    Identifier,
    Number,
    String,
    FormatedString, //format with #{} inside `` quote
    Boolean,
    Null,
    Function,
    Class,
    OpenParen,
    OpenBrace,
    OpenBracket,
    CloseParen,
    CloseBrace,
    CloseBracket,
    BlockComment,
    LineComment,
    BinaryOperator,
    ComparisonOperator,
    AssignmentOperator,
    LogicalOperator,

    Separator, 
    Comma, // specifically to simply arrays analysing

    Return,
    If,
    Else,
    While,
    Match,

    Range
}

#[derive(Debug, Clone)]
pub struct Token {
    pub value: String,
    pub token_type: TokenType,
}

fn token(value: String, token_type: TokenType) -> Token {
    Token { value, token_type }
}

pub fn tokenize(source_code: &str) -> Result<Vec<Token>, String> {
    let mut tokens: Vec<Token> = vec![];
    let mut position: usize = 0;

    while position < source_code.len() {
        let character: char = source_code.as_bytes()[position] as char;

        match character {
            ' ' | '\t'  => (),
            '\n' | ';' => tokens.push(token(character.to_string(), TokenType::Separator)),
            ',' => tokens.push(token(character.to_string(), TokenType::Comma)),
            '+' | '/' | '*' | '%' => {
                // for binary and assignement operators
                let operator_lexeme = match character {
                    '+' if source_code.as_bytes().get(position + 1) == Some(&(b'+' as u8)) => {
                        "++".to_string()
                    }
                    '+' if source_code.as_bytes().get(position + 1) == Some(&(b'=' as u8)) => {
                        "+=".to_string()
                    }
                    _ => character.to_string(),
                };

                if operator_lexeme.len() == 2 {
                    position += 1;
                    tokens.push(token(operator_lexeme, TokenType::AssignmentOperator));
                } else {
                    tokens.push(token(operator_lexeme, TokenType::BinaryOperator));
                }
            }
            '-' => {
                // for equality and value assignement
                match character {
                    '-' if source_code.as_bytes().get(position + 1) == Some(&(b'-' as u8)) => {
                        position += 1;
                        tokens.push(token("--".to_string(), TokenType::AssignmentOperator));
                    },
                    '-' if source_code.as_bytes().get(position + 1) == Some(&(b'>' as u8)) => {
                        position += 1;
                        tokens.push(token("->".to_string(), TokenType::Range));
                    }
                    _ => {
                        tokens.push(token(character.to_string(), TokenType::BinaryOperator));
                    }
                };
            }
            '<' => {
                // for binary and assignement operators
                let mut operator_lexeme = character.to_string();

                if source_code.as_bytes().get(position + 1) == Some(&(b'=' as u8)) {
                    position += 1;
                    operator_lexeme.push('=');
                }

                tokens.push(token(operator_lexeme, TokenType::ComparisonOperator));
            }
            '>' => {
                let mut operator_lexeme = character.to_string();
                position += 1;

                if let Some(next_char) = source_code.as_bytes().get(position) {
                    if *next_char == b'=' {
                        operator_lexeme.push('=');
                    } else if *next_char == b'>' {
                        position += 1; //position increased here cause we continue in the while and what comes next is jumped
                        operator_lexeme.push('>');
                        tokens.push(token(operator_lexeme, TokenType::Return));
                        continue; // continue in the while
                    }
                }

                tokens.push(token(operator_lexeme, TokenType::ComparisonOperator));
            }
            '!' => {
                let mut operator_lexeme = character.to_string();

                if source_code.as_bytes().get(position + 1) == Some(&(b'=' as u8)) {
                    position += 1;
                    operator_lexeme.push('=');
                    tokens.push(token(operator_lexeme, TokenType::ComparisonOperator));
                } else {
                    tokens.push(token(operator_lexeme, TokenType::LogicalOperator))
                }
            }
            'a' | 'o' => {
                // for 'and' and 'or' logical operators
                let mut value_lexeme: String = character.to_string();

                position += 1;

                while position < source_code.len() {
                    let c = source_code.as_bytes()[position] as char;

                    match c {
                        ' ' | '\n' | ';' | '+' | '-' | '*' | '/' | '%' | '=' | '"' | '#' | '`'
                        | '(' | ')' | '[' => break,
                        _ => value_lexeme.push(c),
                    }

                    position += 1;
                }

                let token_type = match value_lexeme.as_str() {
                    "and" | "or" => TokenType::LogicalOperator,
                    _ => TokenType::Identifier,
                };

                tokens.push(token(value_lexeme, token_type));
            }
            '=' => {
                // for equality and value assignement
                let mut equal_lexeme = character.to_string();

                if source_code.as_bytes().get(position + 1) == Some(&(b'=' as u8)) {
                    position += 1;
                    equal_lexeme.push(source_code.as_bytes()[position] as char);
                    tokens.push(token(equal_lexeme, TokenType::ComparisonOperator));
                } else if source_code.as_bytes().get(position + 1) == Some(&(b'>' as u8)) {
                    position += 1;
                    equal_lexeme.push(source_code.as_bytes()[position] as char);
                    tokens.push(token(equal_lexeme, TokenType::Range));
                } else {
                    tokens.push(token(equal_lexeme, TokenType::AssignmentOperator));
                }
            }
            character if character.is_digit(10) => {
                // for numbers
                let mut number_lexeme = character.to_string();

                while let Some(&next_char) = source_code.as_bytes().get(position + 1) {
                    let next_char: char = next_char as char;

                    match next_char {
                        _n if next_char.is_digit(10) || next_char == '.' => {
                            position += 1;
                            number_lexeme.push(next_char);
                        }
                        ' ' | '\n' | ')' | ';' | '+' | '-' | '*' | '/' | '%' | '=' | ',' | ']' => {
                            break
                        }
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
                tokens.push(token(number_lexeme, TokenType::Number));
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

                    tokens.push(token(comment_lexeme, TokenType::BlockComment));
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

                    tokens.push(token(comment_lexeme, TokenType::LineComment));
                }
            }
            '"' => {
                // for strings
                let mut string_lexeme = String::new();

                position += 1;

                while position < source_code.len() {
                    let c: char = source_code.as_bytes()[position] as char;

                    if c == '"' {
                        break;
                    }

                    string_lexeme.push(c);
                    position += 1;
                }

                tokens.push(token(string_lexeme, TokenType::String));
            }
            '`' => {
                // for strings
                let mut string_lexeme = String::new();

                position += 1;

                while position < source_code.len() {
                    let c: char = source_code.as_bytes()[position] as char;

                    if c == '`' {
                        break;
                    }

                    string_lexeme.push(c);
                    position += 1;
                }

                tokens.push(token(string_lexeme, TokenType::FormatedString));
            }
            '(' => tokens.push(token(character.to_string(), TokenType::OpenParen)),
            ')' => tokens.push(token(character.to_string(), TokenType::CloseParen)),
            '{' => tokens.push(token(character.to_string(), TokenType::OpenBrace)),
            '}' => tokens.push(token(character.to_string(), TokenType::CloseBrace)),
            '[' => {
                tokens.push(token(character.to_string(), TokenType::OpenBracket))
                // for arrays

                /*
                let mut array_lexeme: String = String::new();
                position += 1;

                while position < source_code.len() {
                    let c: char = source_code.as_bytes()[position] as char;

                    match c {
                        ']' => break,
                        '\n' | ';' => {
                            return Err(SyntaxError::InvalidArray {
                                line: 000,
                                at: format!("[{array_lexeme}]"),
                            }
                            .to_string())
                        }
                        _ => array_lexeme.push(c),
                    }

                    position += 1;
                }

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

                tokens.push(token(array_lexeme, TokenType::Array));
                */
            }
            ']' => tokens.push(token(character.to_string(), TokenType::CloseBracket)),
            't' | 'f' | 'n' | 'c' | 'r' | 'i' | 'e' | 'w' | 'm' => {
                // for booleans and null values
                let mut value_lexeme: String = character.to_string();

                position += 1;

                while position < source_code.len() {
                    let c = source_code.as_bytes()[position] as char;

                    match c {
                        ' ' | '\n' | ';' | '+' | '-' | '*' | '/' | '%' | '=' | '"' | '#' | '`'
                        | '(' | ')' | '[' => break,
                        _ => value_lexeme.push(c),
                    }

                    position += 1;
                }

                let token_type = match value_lexeme.as_str() {
                    "true" | "false" => TokenType::Boolean,
                    "null" => TokenType::Null,
                    "fn" => TokenType::Function,
                    "class" => TokenType::Class,
                    "return" => TokenType::Return,
                    "if" => TokenType::If,
                    "else" => TokenType::Else,
                    "while" => TokenType::While,
                    "match" => TokenType::Match,
                    _ => TokenType::Identifier,
                };

                tokens.push(token(value_lexeme, token_type));
            }
            character => {
                let mut value_lexeme: String = character.to_string();

                if !character.is_alphabetic() {
                    return Err("Character must be alphabetic".to_string());
                }

                position += 1;

                while position < source_code.len() {
                    let c = source_code.as_bytes()[position] as char;

                    match c {
                        c if !c.is_alphabetic() && !c.is_ascii_digit() => {
                            position -= 1;
                            break;
                        }
                        _ => value_lexeme.push(c),
                    }

                    position += 1;
                }

                tokens.push(token(value_lexeme, TokenType::Identifier));
            }
        };

        position += 1;
    }

    Ok(tokens)
}
