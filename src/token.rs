use std::error::Error;

#[derive(Debug)]
pub enum TokenType {
    Let, MutLet,
    Identifier,
    AssignmentOperator, 
    Int, Float, String, Boolean, Null, Undefined, Object, Function, Array, Class, 
    OpenParen, CloseParen,
    BinaryOperator,
}

#[derive(Debug)]
pub struct Token {
    value: String,
    type_: TokenType
}

fn token(value:&str, type_: TokenType) -> Token {
    Token { value: value.to_string(), type_ }
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
    }else {
        false
    }
}
fn is_object(str: &str) -> bool {
    if str.starts_with("{") || str.starts_with("Object(") || str.starts_with("new Object(") {
        true
    }else {
        false
    }
}
fn is_array(str: &str) -> bool {
    if str.starts_with("[") || str.starts_with("Array(") || str.starts_with("new Array(") {
        true
    }else {
        false
    }
}
fn is_fn_declaration(str: &str) -> bool {
    if str.starts_with("fn") {
        true
    }else{
        false
    }
}
fn is_class_init(str: &str) -> bool {
    if str.starts_with("new") {
        true
    }else{
        false
    }
}
pub fn tokenize(source_code: String) -> Result<Vec<Token>, Box<dyn Error>> {
    let mut tokens: Vec<Token> = vec![];
    let formatted_src: String = source_code.replace("\n", "").replace(";", " ").replace("  ", " ");
    let mut src: Vec<&str> = formatted_src.split(" ").filter(|x| x != &"").collect();

    let mut is_after_let:bool = false;
    let mut is_after_fn:bool = false;
    let mut is_a_type:bool = false;

    while !src.is_empty() {
        let val: &str = src.remove(0);

        match val {
            "(" => {tokens.push(token(val, TokenType::OpenParen))},
            ")" => {tokens.push(token(val, TokenType::CloseParen))},
            "let"|"nl" => {tokens.push(token(val, TokenType::Let)); is_after_let = true},
            "letmut"|"ml" => {tokens.push(token(val, TokenType::MutLet)); is_after_let = true},
            "+"|"-"|"*"|"/"|"%" => {tokens.push(token(val, TokenType::BinaryOperator))},
            "="|"+="|"-="|"*="|"/="|"%=" => {tokens.push(token(val, TokenType::AssignmentOperator))},
            "true"|"false" => {tokens.push(token(val, TokenType::Boolean))},
            "null" => {tokens.push(token(val, TokenType::Null))},
            "undefined" => {tokens.push(token(val, TokenType::Undefined))},
            ":"| ": "|" :" => {is_a_type = true},
            x => {
                
                if is_a_type {
                    //retrieves the type assigned by the user to the variable

                    is_a_type = false;
                }else if is_int(x) {
                    tokens.push(token(val, TokenType::Int));
                }else if is_float(x) {
                    tokens.push(token(val, TokenType::Float));
                } else if is_after_let || is_after_fn {
                    if is_after_let && x.ends_with(":"){
                        is_a_type = true;
                    }

                    tokens.push(token(val, TokenType::Identifier));
                    is_after_fn = false;
                    is_after_let = false;
                }else if is_string(x) {
                    tokens.push(token(val, TokenType::String));
                }else if is_object(x) {
                    tokens.push(token(val, TokenType::Object));
                }else if is_array(x) {
                    tokens.push(token(val, TokenType::Array));
                }else if is_fn_declaration(x) {
                    tokens.push(token(val, TokenType::Function));   
                    is_after_fn = true;              
                }else if is_class_init(x) {
                    tokens.push(token(val, TokenType::Class));
                }else {
                    println!("{}",x);
                    return Err("Error in the provided code".into())
                }
            },
        }
    }

    Ok(tokens)
}