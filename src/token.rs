use self::{errors::SyntaxError, line::get_line};

mod errors;
mod line;

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum TokenType {
    Var,

    Identifier, // variables and also object props
    Number,

    String,
    FormatedString, //format with #{} inside `` quote
    SpecialString, // string to specify certain things for the file, for instance 'strict' at the start of the file

    Boolean,
    Null,
    Function,
    OptionArgFunction,
    Class,
    OpenParen,
    OpenBrace,
    OpenBracket,
    CloseParen,
    CloseBrace,
    CloseBracket,
    BinaryOperator,
    ComparisonOperator,
    AssignmentOperator,
    LogicalOperator,
    DecrementOperator,
    IncrementOperator,

    Separator,
    Comma,

    Return,
    If,
    Else,
    While,

    Match,
    QuestionMarkMatch,

    For,
    In,

    Range,
    FunctionArrow,

    Colon,
    TypeNumber,
    TypeBool,
    TypeString,
    TypeVec,
    TypeDict,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub value: String,
    pub token_type: TokenType,
}

fn token(value: String, token_type: TokenType) -> Token {
    Token { value, token_type }
}

pub fn tokenize(source_code: &str) -> Result<Vec<Token>, SyntaxError> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut position: usize = 0;

    while position < source_code.len() {
        let character: char = source_code.as_bytes()[position] as char;
        // most efficient way to retrieve a char at a given index

        match character {
            ' ' | '\t' => (),
            '\n' | ';' => tokens.push(token(character.to_string(), TokenType::Separator)),
            ',' => tokens.push(token(character.to_string(), TokenType::Comma)),
            '.' => match character {
                _ if source_code.as_bytes().get(position + 1) == Some(&(b'.' as u8)) => {
                    position += 1;
                    tokens.push(token("..".to_string(), TokenType::Range));
                }
                _ if source_code.as_bytes().get(position + 1) == Some(&(b'.' as u8))
                    && source_code.as_bytes().get(position + 2) == Some(&(b'=' as u8)) =>
                {
                    position += 1;
                    tokens.push(token("..".to_string(), TokenType::Range));
                }
                _ => {
                    return Err(SyntaxError::UnexpectedToken {
                        token: ".".to_string(),
                        line: get_line(position, source_code),
                    })
                }
            },
            '*' => {
                let operator_lexeme = match character {
                    '*' if source_code.as_bytes().get(position + 1) == Some(&(b'*' as u8))
                        && source_code.as_bytes().get(position + 2) == Some(&(b'=' as u8)) =>
                    {
                        position += 3;
                        tokens.push(token("**=".to_string(), TokenType::AssignmentOperator));
                        continue;
                    }
                    '*' if source_code.as_bytes().get(position + 1) == Some(&(b'*' as u8)) => {
                        position += 1;
                        "**".to_string()
                    }
                    '*' if source_code.as_bytes().get(position + 1) == Some(&(b'=' as u8)) => {
                        position += 2;
                        tokens.push(token("*=".to_string(), TokenType::AssignmentOperator));
                        continue;
                    }
                    _ => character.to_string(),
                };
                tokens.push(token(operator_lexeme, TokenType::BinaryOperator));
            }
            '+' | '%' => {
                // for binary and assignment operators
                let operator_lexeme = match character {
                    '+' if source_code.as_bytes().get(position + 1) == Some(&(b'+' as u8)) => {
                        position += 1;
                        tokens.push(token("++".to_string(), TokenType::IncrementOperator));
                        continue;
                    }
                    '+' if source_code.as_bytes().get(position + 1) == Some(&(b'=' as u8)) => {
                        "+=".to_string()
                    }
                    '*' if source_code.as_bytes().get(position + 1) == Some(&(b'=' as u8)) => {
                        "*=".to_string()
                    }
                    '%' if source_code.as_bytes().get(position + 1) == Some(&(b'=' as u8)) => {
                        "%=".to_string()
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
            ':' => tokens.push(token(":".to_string(), TokenType::Colon)),
            '-' => {
                // for equality and value assignment
                match character {
                    '-' if source_code.as_bytes().get(position + 1) == Some(&(b'-' as u8)) => {
                        position += 1;
                        tokens.push(token("--".to_string(), TokenType::DecrementOperator));
                    }
                    '-' if source_code.as_bytes().get(position + 1) == Some(&(b'=' as u8)) => {
                        position += 1;
                        tokens.push(token("-=".to_string(), TokenType::AssignmentOperator));
                    }
                    _ => {
                        tokens.push(token(character.to_string(), TokenType::BinaryOperator));
                    }
                };
            }
            '<' => {
                // for binary and assignment operators
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
            '?' => {
                let mut question_lexeme = character.to_string();
                position += 1;

                if let Some(next_char) = source_code.as_bytes().get(position) {
                    if *next_char == b'=' {
                        question_lexeme.push('=');
                    } else if *next_char == b'>' {
                        question_lexeme.push('>');
                    } else if *next_char == b'<' {
                        question_lexeme.push('<');
                    } else {
                        tokens.push(token(question_lexeme, TokenType::QuestionMarkMatch));
                        continue;
                    }
                }

                tokens.push(token(question_lexeme, TokenType::ComparisonOperator));
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
            '=' => {
                // for equality and value assignment
                let mut equal_lexeme = character.to_string();

                if source_code.as_bytes().get(position + 1) == Some(&(b'=' as u8)) {
                    position += 1;
                    equal_lexeme.push(source_code.as_bytes()[position] as char);
                    tokens.push(token(equal_lexeme, TokenType::ComparisonOperator));
                } else if source_code.as_bytes().get(position + 1) == Some(&(b'>' as u8)) {
                    position += 1;
                    equal_lexeme.push(source_code.as_bytes()[position] as char);
                    tokens.push(token(equal_lexeme, TokenType::FunctionArrow));
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
                        ' ' | '\n' | ';' | '+' | '-' | '*' | '/' | '%' | '=' | '"' | '#' | '`'
                        | ')' | ':' | '?' | ',' | '}' | ']' => break,
                        _ => {
                            return Err(SyntaxError::InvalidNumber {
                                line: get_line(position, source_code),
                                at: format!("{}{}", number_lexeme, next_char),
                            })
                        }
                    }
                }

                if number_lexeme.ends_with(".") {
                    return Err(SyntaxError::InvalidNumber {
                        line: get_line(position, source_code),
                        at: number_lexeme.clone(),
                    });
                }
                tokens.push(token(number_lexeme, TokenType::Number));
            }
            '/' => {
                // for comments
                let mut slash_lexeme = character.to_string();
                position += 1;

                if source_code.as_bytes().get(position) == Some(&(b'*' as u8))
                // if "/*" then block comment
                {
                    position += 1;
                    slash_lexeme.push_str("*");
                    //jumping next chars and adding the / cause we know the next char is /

                    while position < source_code.len() {
                        slash_lexeme.push(source_code.as_bytes()[position] as char);

                        if slash_lexeme.ends_with("*/") {
                            position += 1; //to get past the two #
                            break;
                        }

                        position += 1;
                    }

                    tokens.push(token(slash_lexeme, TokenType::Separator));
                } else if source_code.as_bytes().get(position) == Some(&(b'/' as u8)) {
                    slash_lexeme.push_str("/");
                    position += 1;

                    while position < source_code.len() {
                        slash_lexeme.push(source_code.as_bytes()[position] as char);

                        if slash_lexeme.ends_with('\n') {
                            break;
                        }
                        position += 1;
                    }
                    // no need to push as there is nothing to analyse
                    tokens.push(token(slash_lexeme, TokenType::Separator));
                } else {
                    if source_code.as_bytes().get(position) == Some(&(b'=' as u8)) {
                        slash_lexeme.push('=');
                        tokens.push(token(slash_lexeme, TokenType::AssignmentOperator));
                    } else {
                        tokens.push(token(slash_lexeme, TokenType::BinaryOperator));
                    }
                }
            }
            '"' | '\'' => {
                // for strings
                let mut string_lexeme = String::new();

                position += 1;

                while position < source_code.len() {
                    let c: char = source_code.as_bytes()[position] as char;

                    if c == character {
                        break;
                    }
                    if position == source_code.len() - 1 {
                        return Err(SyntaxError::UnclosedString);
                    }

                    string_lexeme.push(c);
                    position += 1;
                }

                if character == '"' {
                    tokens.push(token(string_lexeme, TokenType::String));
                } else {
                    tokens.push(token(string_lexeme, TokenType::SpecialString));
                }
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
                    if position == source_code.len() - 1 {
                        return Err(SyntaxError::UnclosedString);
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
            character if character.is_alphabetic() => {
                // for booleans and null values
                let mut value_lexeme: String = character.to_string();

                position += 1;

                while position < source_code.len() {
                    let c = source_code.as_bytes()[position] as char;

                    match c {
                        ' ' | '\n' | ';' | '+' | '-' | '*' | '/' | '%' | '=' | '"' | '#' | '`'
                        | '(' | ')' | '[' | ']' | ':' | '?' | ',' | '{' | '}' => {
                            position -= 1;
                            break;
                        }
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
                    "for" => TokenType::For,
                    "in" => TokenType::In,
                    "mut" => TokenType::Var,
                    "const" => TokenType::Var,
                    "bool" => TokenType::TypeBool,
                    "num" => TokenType::TypeNumber,
                    "str" => TokenType::TypeString,
                    "vec" => TokenType::TypeVec,
                    "dict" => TokenType::TypeDict,
                    "and" | "or" => TokenType::LogicalOperator,
                    "let" => return Err(SyntaxError::ExpectedMutNotLet),
                    "opt" => TokenType::OptionArgFunction,
                    val if value_lexeme.ends_with('.') => {
                        return Err(SyntaxError::ExpectingSomethingAfterDot {
                            id: val.to_string(),
                        })
                    }
                    _ => TokenType::Identifier,
                };

                tokens.push(token(value_lexeme, token_type));
            }
            _ => {
                return Err(SyntaxError::NonAlphabeticCharacter);
            }
        };

        position += 1;
    }

    while let Some(Token {
        token_type: TokenType::Separator,
        ..
    }) = tokens.first()
    {
        tokens.remove(0);
    }

    while let Some(Token {
        token_type: TokenType::Separator,
        ..
    }) = tokens.last()
    {
        tokens.pop();
    }

    Ok(tokens)
}
