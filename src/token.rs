use hashbrown::HashMap;
use lazy_static::lazy_static;

use self::{errors::SyntaxError, line::get_line};

pub mod errors;
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

    Class,
    ClassPublic,
    ClassPrivate,

    Import,
    Export,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub value: String,
    pub token_type: TokenType,
}

fn token(value: String, token_type: TokenType) -> Token {
    Token { value, token_type }
}

pub fn tokenize(source_code: &[u8]) -> Result<Vec<Token>, SyntaxError> {
    let mut tokens: Vec<Token> = Vec::with_capacity(5000);
    let mut position: usize = 0;

    while position < source_code.len() {
        let character: char = source_code[position] as char;
        // most efficient way to retrieve a char at a given index

        match character {
            ' ' | '\t' => (),
            '\n' | ';' => tokens.push(token(character.to_string(), TokenType::Separator)),
            ',' => tokens.push(token(character.to_string(), TokenType::Comma)),
            '.' => match character {
                _ if source_code.get(position + 1) == Some(&(b'.' as u8)) => {
                    position += 1;
                    tokens.push(token("..".to_string(), TokenType::Range));
                }
                _ if source_code.get(position + 1) == Some(&(b'.' as u8))
                    && source_code.get(position + 2) == Some(&(b'=' as u8)) =>
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
                    '*' if source_code.get(position + 1) == Some(&(b'*' as u8))
                        && source_code.get(position + 2) == Some(&(b'=' as u8)) =>
                    {
                        position += 3;
                        tokens.push(token("**=".to_string(), TokenType::AssignmentOperator));
                        continue;
                    }
                    '*' if source_code.get(position + 1) == Some(&(b'*' as u8)) => {
                        position += 1;
                        "**".to_string()
                    }
                    '*' if source_code.get(position + 1) == Some(&(b'=' as u8)) => {
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
                    '+' if source_code.get(position + 1) == Some(&(b'+' as u8)) => {
                        position += 1;
                        tokens.push(token("++".to_string(), TokenType::IncrementOperator));
                        continue;
                    }
                    '+' if source_code.get(position + 1) == Some(&(b'=' as u8)) => "+=".to_string(),
                    '*' if source_code.get(position + 1) == Some(&(b'=' as u8)) => "*=".to_string(),
                    '%' if source_code.get(position + 1) == Some(&(b'=' as u8)) => "%=".to_string(),
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
                // for equality and value assignment
                match character {
                    '-' if source_code.get(position + 1) == Some(&(b'-' as u8)) => {
                        position += 1;
                        tokens.push(token("--".to_string(), TokenType::DecrementOperator));
                    }
                    '-' if source_code.get(position + 1) == Some(&(b'=' as u8)) => {
                        position += 1;
                        tokens.push(token("-=".to_string(), TokenType::AssignmentOperator));
                    }
                    _ => {
                        let mut lexeme = character.to_string();
                        while let Some(number) = source_code.get(position + 1) {
                            if !number.is_ascii_digit() {
                                break;
                            }
                            lexeme.push(*number as char);

                            position += 1;
                        }
                        if lexeme.len() == 1 {
                            tokens.push(token(lexeme, TokenType::BinaryOperator));
                        } else {
                            tokens.push(token(lexeme, TokenType::Number));
                        }
                    }
                };
            }

            ':' => tokens.push(token(":".to_string(), TokenType::Colon)),
            '<' => {
                // for binary and assignment operators
                let mut operator_lexeme = character.to_string();

                if source_code.get(position + 1) == Some(&(b'=' as u8)) {
                    position += 1;
                    operator_lexeme.push('=');
                }

                tokens.push(token(operator_lexeme, TokenType::ComparisonOperator));
            }
            '>' => {
                let mut operator_lexeme = character.to_string();
                position += 1;

                if let Some(next_char) = source_code.get(position) {
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

                if let Some(next_char) = source_code.get(position) {
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

                if source_code.get(position + 1) == Some(&(b'=' as u8)) {
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

                if source_code.get(position + 1) == Some(&(b'=' as u8)) {
                    position += 1;
                    equal_lexeme.push(source_code[position] as char);
                    tokens.push(token(equal_lexeme, TokenType::ComparisonOperator));
                } else if source_code.get(position + 1) == Some(&(b'>' as u8)) {
                    position += 1;
                    equal_lexeme.push(source_code[position] as char);
                    tokens.push(token(equal_lexeme, TokenType::FunctionArrow));
                } else {
                    tokens.push(token(equal_lexeme, TokenType::AssignmentOperator));
                }
            }
            character if character.is_digit(10) => {
                // for numbers
                let mut number_lexeme = character.to_string();

                let mut is_minus_operation = false;

                'global_loop: while let Some(&next_char) = source_code.get(position + 1) {
                    let next_char: char = next_char as char;

                    match next_char {
                        _n if next_char.is_digit(10) || next_char == '.' => {
                            position += 1;
                            number_lexeme.push(next_char);
                        }
                        ';' | '+' | '*' | '/' | '%' | '=' | '"' | '#' | '`' | ')' | ':' | '?'
                        | ',' | '}' | ']' => break,
                        ' ' | '\n' | '-' => {
                            let initial_pos = position;
                            position += 1;
                            while let Some(&next_char) = source_code.get(position) {
                                let next_char = next_char as char;
                                match next_char {
                                    '-' => {
                                        is_minus_operation = true;
                                        break 'global_loop;
                                    }
                                    ' ' | '\n' => {
                                        position += 1;
                                        continue;
                                    }
                                    _ => {
                                        position = initial_pos;
                                        break 'global_loop;
                                    }
                                }
                            }
                        }
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

                if is_minus_operation {
                    tokens.push(token("-".to_string(), TokenType::BinaryOperator))
                }
            }
            '/' => {
                // for comments
                let mut slash_lexeme = character.to_string();

                if source_code.get(position + 1) == Some(&(b'*' as u8))
                // if "/*" then block comment
                {
                    position += 2;
                    slash_lexeme.push_str("*");
                    //jumping next chars and adding the / cause we know the next char is /

                    while position < source_code.len() {
                        slash_lexeme.push(source_code[position] as char);

                        if slash_lexeme.ends_with("*/") {
                            position += 1; //to get past the two #
                            break;
                        }

                        position += 1;
                    }

                    tokens.push(token(slash_lexeme, TokenType::Separator));
                } else if source_code.get(position + 1) == Some(&(b'/' as u8)) {
                    slash_lexeme.push_str("/");
                    position += 2;

                    while position < source_code.len() {
                        slash_lexeme.push(source_code[position] as char);

                        if slash_lexeme.ends_with('\n') {
                            break;
                        }
                        position += 1;
                    }
                    // no need to push as there is nothing to analyse
                    tokens.push(token(slash_lexeme, TokenType::Separator));
                } else {
                    if source_code.get(position + 1) == Some(&(b'=' as u8)) {
                        position += 1;
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
                    let c: char = source_code[position] as char;

                    if c == character {
                        break;
                    }
                    if position == source_code.len() - 1 {
                        return Err(SyntaxError::UnclosedString);
                    }
                    if c == '\\' {
                        position += 1;
                        string_lexeme.push(source_code[position] as char);
                        position += 1;
                        continue;
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
                    let c: char = source_code[position] as char;

                    if c == '`' {
                        break;
                    }
                    if position == source_code.len() - 1 {
                        return Err(SyntaxError::UnclosedString);
                    }
                    if c == '\\' {
                        position += 1;
                        string_lexeme.push(source_code[position] as char);
                        position += 1;
                        continue;
                    }

                    string_lexeme.push(c);
                    position += 1;
                }

                tokens.push(token(string_lexeme, TokenType::FormatedString));
            }
            '(' => tokens.push(token(character.to_string(), TokenType::OpenParen)),
            ')' => {
                let mut is_minus_operation = false;
                let initial_position = position;
                'global_loop: while let Some(next_char) = source_code.get(position + 1) {
                    let next_char = *next_char as char;
                    match next_char {
                        ' ' | '\n' | '-' => {
                            let initial_pos = position;
                            position += 1;
                            while let Some(&next_char) = source_code.get(position) {
                                let next_char = next_char as char;
                                match next_char {
                                    '-' => {
                                        is_minus_operation = true;
                                        break 'global_loop;
                                    }
                                    ' ' | '\n' => {
                                        position += 1;
                                        continue;
                                    }
                                    _ => {
                                        position = initial_pos;
                                        break 'global_loop;
                                    }
                                }
                            }
                        }
                        _ => {
                            position = initial_position;
                            break 'global_loop;
                        }
                    }
                }

                tokens.push(token(character.to_string(), TokenType::CloseParen));
                if is_minus_operation {
                    tokens.push(token("-".to_string(), TokenType::BinaryOperator))
                }
            }
            '{' => tokens.push(token(character.to_string(), TokenType::OpenBrace)),
            '}' => tokens.push(token(character.to_string(), TokenType::CloseBrace)),
            '[' => {
                tokens.push(token(character.to_string(), TokenType::OpenBracket))
                // for arrays

                /*
                let mut array_lexeme: String = String::new();
                position += 1;

                while position < source_code.len() {
                    let c: char = source_code[position] as char;

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
                let mut is_minus_operation = false;

                position += 1;

                'global_loop: while position < source_code.len() {
                    let c = source_code[position] as char;
                    match c {
                        ';' | '+' | '*' | '/' | '%' | '=' | '"' | '#' | '`' | '(' | ')' | '['
                        | ']' | ':' | '?' | ',' | '{' | '}' => {
                            position -= 1;
                            break;
                        }
                        ' ' | '\n' | '-' => {
                            let initial_pos = position - 1;
                            position += 1;
                            while let Some(&next_char) = source_code.get(position) {
                                let next_char = next_char as char;
                                match next_char {
                                    '-' => {
                                        is_minus_operation = true;
                                        break 'global_loop;
                                    }
                                    ' ' | '\n' => {
                                        position += 1;
                                        continue;
                                    }
                                    _ => {
                                        position = initial_pos;
                                        break 'global_loop;
                                    }
                                }
                            }
                        }
                        _ => value_lexeme.push(c),
                    }

                    position += 1;
                }

                if value_lexeme.ends_with('.') {
                    return Err(SyntaxError::ExpectingSomethingAfterDot {
                        id: value_lexeme.to_owned(),
                    });
                }

                let token_type = KEYWORDS
                    .get(value_lexeme.as_str())
                    .unwrap_or_else(|| &TokenType::Identifier)
                    .to_owned();

                tokens.push(token(value_lexeme, token_type));

                if is_minus_operation {
                    tokens.push(token("-".to_string(), TokenType::BinaryOperator));
                }
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

lazy_static! {
    static ref KEYWORDS: HashMap<&'static str, TokenType> = {
        let mut keywords: HashMap<&str, TokenType> = HashMap::new();
        keywords.insert("true", TokenType::Boolean);
        keywords.insert("false", TokenType::Boolean);
        keywords.insert("null", TokenType::Null);
        keywords.insert("fn", TokenType::Function);
        keywords.insert("class", TokenType::Class);
        keywords.insert("return", TokenType::Return);
        keywords.insert("if", TokenType::If);
        keywords.insert("else", TokenType::Else);
        keywords.insert("while", TokenType::While);
        keywords.insert("match", TokenType::Match);
        keywords.insert("for", TokenType::For);
        keywords.insert("in", TokenType::In);
        keywords.insert("mut", TokenType::Var);
        keywords.insert("const", TokenType::Var);
        keywords.insert("bool", TokenType::TypeBool);
        keywords.insert("num", TokenType::TypeNumber);
        keywords.insert("str", TokenType::TypeString);
        keywords.insert("vec", TokenType::TypeVec);
        keywords.insert("dict", TokenType::TypeDict);
        keywords.insert("and", TokenType::LogicalOperator);
        keywords.insert("or", TokenType::LogicalOperator);
        keywords.insert("opt", TokenType::OptionArgFunction);
        keywords.insert("pub", TokenType::ClassPublic);
        keywords.insert("priv", TokenType::ClassPrivate);
        keywords.insert("import", TokenType::Import);
        keywords.insert("export", TokenType::Export);

        keywords
    };
}
