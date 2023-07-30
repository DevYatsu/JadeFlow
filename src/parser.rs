use std::io::Error;

use custom_error::custom_error;

use crate::token::{Token, TokenType};

custom_error! {pub ParsingError
    Io{source: Error} = "{source}",
    Default = "Failed to parse tokens",
    UnexpectedEndOfInput = "No more token left to parse",
    ExpectedVarInitialization{var_value: String} = "Expected valid variable name after \"{var_value}\"",
    ExpectedType{value: String} = "Expected valid type after \"{value}\"",
    UnknownVariableType{var_name: String} = "Impossible to guess {var_name} type",
    CannotReassignIfNotAssigned{operator: String, var_name: String} = "Must first assign value to \"{var_name}\" with '=' before using '{operator}'",
    CannotReassignConst{var_name: String} = "Cannot reassign \"{var_name}\" as it is a 'const'",
    InvalidNumber{value: String} = "\"{value}\" is an invalid number!",
    InvalidExpression{value: String} = "\"{value}\" is an invalid expression!",
    MissingClosingParenthesis = "Expected a closing parenthesis!",
    ExpectedSeparator{value: String} = "Expected new line or semicolon after \"{value}\""
}

#[derive(Debug, Clone)]
pub enum ASTNode {
    Program(Vec<Statement>),
    VariableDeclaration(Declaration),
    FunctionDeclaration(Function),
    ClassDeclaration(Class),
    Expression(Expression),
}
#[derive(Debug, Clone)]
pub struct Statement {
    pub node: ASTNode,
}
fn statement(node: ASTNode) -> Statement {
    Statement { node }
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub name: String,
    pub var_type: VariableType,
    pub value: Expression,
    pub is_mutable: bool,
}
#[derive(Debug, Clone)]
pub enum Expression {
    Variable(String),
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
    ArrayExpression(Vec<Expression>),
    DictionaryExpression(Vec<(Expression, Expression)>),
    BinaryOperation {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    FormatedString(Vec<FormattedSegment>),
}
#[derive(Debug, Clone)]
pub enum FormattedSegment {
    // exemple "hey #{2 + 3} how r u ?"
    Literal(String),        // (ex: "hey " and " how r u ?")
    Expression(Expression), // (ex: 2 + 3 -> 5)
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub arguments: Vec<(String, VariableType)>,
    pub context: Vec<Statement>,
    pub return_type: VariableType,
}
#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub arguments: Vec<(String, VariableType)>,
    pub context: Vec<Vec<Statement>>, // methods and declarations
}

#[derive(Debug, Clone, PartialEq)]
pub enum VariableType {
    String,
    Number,
    Boolean,
    Vector,
    Dictionary,
}

fn formatted_string_segments(input: &str) -> Vec<FormattedSegment> {
    let mut segments = Vec::new();
    let mut current_literal = String::new();
    let mut inside_expression = false;
    let mut after_hashtag = false;
    let mut expression = String::new();
    let mut open_par_num = 0;

    for c in input.chars() {
        if c == '#' {
            after_hashtag = true;
        } else if c == '{' {
            if after_hashtag {
                current_literal.pop();
                segments.push(FormattedSegment::Literal(current_literal.to_string()));
                current_literal.clear();

                inside_expression = true;
                open_par_num += 1;
            }
        } else if c == '}' {
            if inside_expression {
                open_par_num -= 1;
                if open_par_num == 0 {
                    //      segments.push(FormattedSegment::Expression(Expression::parse(&expression)));
                    expression.clear();
                    inside_expression = false;
                }
            }
        } else {
            if inside_expression {
                expression.push(c);
            } else {
                current_literal.push(c);
            }
        }
    }

    if !current_literal.is_empty() {
        segments.push(FormattedSegment::Literal(current_literal));
    }

    segments
}

pub fn parse(tokens: &mut Vec<Token>) -> Result<ASTNode, ParsingError> {
    let mut position = 0;
    let mut statements = Vec::new();

    while position < tokens.len() {
        let statement = parse_statement(&tokens, &mut position)?;
        println!("statement {:?}", statement);
        statements.push(statement);

        if let Some(Token {
            token_type: TokenType::Separator,
            ..
        }) = tokens.get(position)
        {
            position += 1;
            continue;
        }

        match &statements[statements.len() - 1].node {
            ASTNode::VariableDeclaration(declaration) => {
                if let Some(Token {
                    token_type: TokenType::Comma,
                    ..
                }) = tokens.get(position)
                {
                    let var_keyword = if declaration.is_mutable {
                        "mut"
                    } else {
                        "const"
                    };
                    tokens.insert(
                        position + 1,
                        Token {
                            token_type: TokenType::Var,
                            value: var_keyword.to_string(),
                        },
                    );
                    position += 1;
                    continue;
                }
            }
            ASTNode::FunctionDeclaration(_) => {
                position += 1;
                continue;
            }
            ASTNode::ClassDeclaration(_)  => {
                position += 1;
                continue;
            }
            _ => (),
        }

        if tokens.len() == position {
            break;
        }
    
        if let Some(token) = tokens.get(position) {
            return Err(ParsingError::ExpectedSeparator { value: token.value.to_string() })
        }else {
            return Err(ParsingError::UnexpectedEndOfInput)
        }
    }

    Ok(ASTNode::Program(statements))
}

fn parse_statement(tokens: &[Token], position: &mut usize) -> Result<Statement, ParsingError> {
    if let Some(Token {
        token_type: TokenType::Var,
        value,
    }) = tokens.get(*position)
    {
        *position += 1;
        
        let is_mutable = if value == "mut" { true } else { false };
        
        if let Some(Token {
            token_type: TokenType::Identifier,
            value,
        }) = tokens.get(*position)
        {
            *position += 1;
            let name = value;
            
            if let Some(Token {
                token_type: TokenType::Colon,
                ..
            }) = tokens.get(*position)
            {
                *position += 1;
                let var_type = parse_type(tokens, position)?;

                if let Some(Token {
                    token_type: TokenType::AssignmentOperator,
                    value,
                }) = tokens.get(*position)
                {
                    if value != "=" {
                        return Err(ParsingError::CannotReassignIfNotAssigned {
                            operator: value.to_string(),
                            var_name: name.to_string(),
                        });
                    }

                    *position += 1;

                    let expression = parse_expression(tokens, position)?;

                    return Ok(statement(ASTNode::VariableDeclaration(Declaration { name: name.to_string(), var_type, value: expression, is_mutable })))   
                } else {
                    let value = Expression::Null;

                    return Ok(statement(ASTNode::VariableDeclaration(Declaration {
                        name: name.to_string(),
                        var_type,
                        value,
                        is_mutable,
                    })));
                }
            } else if let Some(Token {
                token_type: TokenType::AssignmentOperator,
                value
            }) = tokens.get(*position)
            {                                       
                if value != "=" {
                    return Err(ParsingError::CannotReassignIfNotAssigned {
                        operator: value.to_string(),
                        var_name: name.to_string(),
                    });
                }

                *position += 1;

                let expression = parse_expression(tokens, position)?;
                if let Some(var_type) = type_from_expression(&expression) {
                    return Ok(statement(ASTNode::VariableDeclaration(Declaration { name: name.to_string(), var_type, value: expression, is_mutable })))   
                }else {
                    return Err(ParsingError::ExpectedType { value: name.to_string() })
                }

            } else {
                return Err(ParsingError::UnknownVariableType {
                    var_name: name.to_string(),
                });
            }
        } else {
            return Err(ParsingError::ExpectedVarInitialization {
                var_value: value.to_string(),
            });
        }
    }

    *position += 1;
    Ok(Statement {
        node: ASTNode::Expression(Expression::Null),
    })
}

fn type_from_expression(expr: &Expression) -> Option<VariableType> {
    match expr {
        Expression::Number(_) => Some(VariableType::Number),
        Expression::String(_) => Some(VariableType::String),
        Expression::ArrayExpression(_) => Some(VariableType::Vector),
        Expression::FormatedString(_) => Some(VariableType::String),
        Expression::Boolean(_) => Some(VariableType::Boolean),
        Expression::Null => None,
        Expression::DictionaryExpression(_) => Some(VariableType::Dictionary),
        Expression::Variable(var_name) => {
            // retrieve variable type from name
            None
        }
        Expression::BinaryOperation { left, operator, right } => {
            type_from_expression(&*left)
        }
    }
}

fn parse_expression(tokens: &[Token], position: &mut usize) -> Result<Expression, ParsingError> {
    if let Some(token) = tokens.get(*position) {
        match &token.token_type {
            TokenType::Identifier => {
                let variable_name = token.value.clone();
                *position += 1;
                Ok(Expression::Variable(variable_name))
            }
            TokenType::Number => {
                if let Ok(number) = token.value.parse::<f64>() {
                    *position += 1;
                    Ok(Expression::Number(number))
                } else {
                    Err(ParsingError::InvalidNumber {
                        value: token.value.clone(),
                    })
                }
            }
            TokenType::String => {
                let string_value = token.value.clone();
                *position += 1;
                Ok(Expression::String(string_value))
            }
            TokenType::Null => {
                *position += 1;
                Ok(Expression::Null)
            }
            TokenType::Boolean => {
                *position += 1;
                let b = if token.value == "true" {true}else{false};
                Ok(Expression::Boolean(b))
            }
            TokenType::OpenParen => parse_parenthesized_expression(tokens, position),
            TokenType::OpenBracket => parse_array_expression(tokens, position),
            TokenType::OpenBrace => parse_dictionary_expression(tokens, position),
            //TokenType::FormatedString => parse_formatted_string(tokens, position),
            _ => Err(ParsingError::InvalidExpression {
                value: token.value.clone(),
            }),
        }
    } else {
        Err(ParsingError::UnexpectedEndOfInput)
    }
}

fn parse_dictionary_expression(
    tokens: &[Token],
    position: &mut usize,
) -> Result<Expression, ParsingError> {
    *position += 1;
    let mut vec_expressions: Vec<(Expression, Expression)> = vec![];
    let mut temp_key: Option<Expression> = None;

    while let Some(token) = tokens.get(*position) {
        if token.token_type == TokenType::CloseBrace {
            if let Some(key) = temp_key {
                vec_expressions.push((key, Expression::Null));
            }
            break;
        }

        if temp_key.is_some() {
            let value = parse_expression(tokens, position)?;
            vec_expressions.push((temp_key.take().unwrap(), value));
        } else {
            temp_key = Some(parse_expression(tokens, position)?);
        }
    }

    Ok(Expression::DictionaryExpression(vec_expressions))
}

fn parse_array_expression(
    tokens: &[Token],
    position: &mut usize,
) -> Result<Expression, ParsingError> {
    *position += 1;
    let mut vec_expressions = vec![];

    while let Some(token) = tokens.get(*position) {
        if token.token_type == TokenType::CloseBracket {
            break;
        }

        vec_expressions.push(parse_expression(tokens, position)?)
    }

    Ok(Expression::ArrayExpression(vec_expressions))
}

fn parse_parenthesized_expression(
    tokens: &[Token],
    position: &mut usize,
) -> Result<Expression, ParsingError> {
    *position += 1;

    let expression = parse_expression(tokens, position)?;

    if let Some(Token {
        token_type: TokenType::CloseParen,
        ..
    }) = tokens.get(*position)
    {
        *position += 1;

        Ok(expression)
    } else {
        Err(ParsingError::MissingClosingParenthesis)
    }
}

fn parse_type(tokens: &[Token], position: &mut usize) -> Result<VariableType, ParsingError> {
    // fn to use after encountering ':'
    if let Some(Token {
        token_type: TokenType::TypeBool,
        ..
    }) = tokens.get(*position)
    {
        *position += 1;
        Ok(VariableType::Boolean)
    } else if let Some(Token {
        token_type: TokenType::TypeDict,
        ..
    }) = tokens.get(*position)
    {
        *position += 1;
        Ok(VariableType::Dictionary)
    } else if let Some(Token {
        token_type: TokenType::TypeNumber,
        ..
    }) = tokens.get(*position)
    {
        *position += 1;
        Ok(VariableType::Number)
    } else if let Some(Token {
        token_type: TokenType::TypeString,
        ..
    }) = tokens.get(*position)
    {
        *position += 1;
        Ok(VariableType::String)
    } else if let Some(Token {
        token_type: TokenType::TypeVec,
        ..
    }) = tokens.get(*position)
    {
        *position += 1;
        Ok(VariableType::Vector)
    } else if let Some(Token { value, .. }) = tokens.get(*position) {
        return Err(ParsingError::ExpectedType {
            value: value.to_string(),
        });
    } else {
        return Err(ParsingError::UnexpectedEndOfInput);
    }
}
