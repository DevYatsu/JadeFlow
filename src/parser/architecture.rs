use crate::token::{tokenize, Token};

use super::{expression::parse_expression, types::type_from_expression, ParsingError, TypeError};
use std::{collections::HashMap, fmt};

#[derive(Debug, Clone)]
pub enum ASTNode {
    Program(Program),
    VariableDeclaration(Declaration),
    VariableReassignment(Reassignment),

    FunctionDeclaration(Function),
    // corresponds to both {} and => functions
    ClassDeclaration(Class),
    Expression(Expression),

    Return(Expression),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Statement>,
    pub symbol_table: SymbolTable,
}

pub fn program(
    Program {
        statements,
        symbol_table,
    }: Program,
) -> ASTNode {
    ASTNode::Program(Program {
        statements,
        symbol_table,
    })
}

#[derive(Debug, Clone)]
pub struct Statement {
    pub node: ASTNode,
}
pub fn variable(declaration: Declaration) -> Statement {
    Statement {
        node: ASTNode::VariableDeclaration(declaration),
    }
}
pub fn reassignment(reassignement: Reassignment) -> Statement {
    Statement {
        node: ASTNode::VariableReassignment(reassignement),
    }
}
pub fn function(f: Function) -> Statement {
    Statement {
        node: ASTNode::FunctionDeclaration(f),
    }
}
pub fn return_statement(expr: Expression) -> Statement {
    Statement {
        node: ASTNode::Return(expr),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Variable(String),
    Number(f64),
    String(String),
    Boolean(bool),
    Null,
    ArrayExpression(Vec<Expression>),
    DictionaryExpression(HashMap<String, Expression>),
    BinaryOperation {
        left: Box<Expression>,
        operator: BinaryOperator,
        right: Box<Expression>,
    },
    FormattedString(Vec<FormattedSegment>),
}
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Variable(val) => write!(f, "{}", val),
            Expression::Number(val) => write!(f, "{}", val),
            Expression::String(val) => write!(f, "\"{}\"", val),
            Expression::Boolean(val) => write!(f, "{}", val),
            Expression::Null => write!(f, "null"),
            Expression::ArrayExpression(values) => {
                write!(f, "[")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, "]")
            }
            Expression::DictionaryExpression(entries) => {
                write!(f, "{{")?;
                for (i, (key, value)) in entries.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "\"{}\": {}", key, value)?;
                }
                write!(f, "}}")
            }
            Expression::BinaryOperation {
                left,
                operator,
                right,
            } => {
                write!(f, "({} {} {})", left, operator, right)
            }
            Expression::FormattedString(segments) => {
                write!(f, "\"")?;
                for segment in segments {
                    match segment {
                        FormattedSegment::Literal(text) => write!(f, "{}", text)?,
                        FormattedSegment::Expression(expr) => write!(f, "{}", expr)?,
                    }
                }
                write!(f, "\"")
            }
        }
    }
}
impl Expression {
    pub fn string_from_expression(self) -> Result<String, ParsingError> {
        match self {
            Expression::Variable(val) => Ok(val.to_string()),
            Expression::Number(val) => {
                let val = val as i64;
                Ok(val.to_string())
            }
            Expression::String(val) => Ok(val.to_string()),
            expression => {
                return Err(ParsingError::InvalidKeyDict {
                    key_type: expression,
                })
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Declaration {
    pub name: String,
    pub var_type: VariableType,
    pub value: Expression,
    pub is_mutable: bool,
}
impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let var = self.get_var_keyword();
        write!(
            f,
            "{} {}: {} = {}",
            var,
            self.name,
            self.var_type.as_assignment(),
            self.value
        )
    }
}
impl Declaration {
    pub fn equivalent_tokens(&self) -> Vec<Token> {
        let keyword = self.get_var_keyword();
        let source_code = format!(
            "{} {}: {} = {};",
            keyword,
            self.name,
            self.var_type.as_assignment(),
            self.value
        );
        tokenize(&source_code).unwrap()
    }
    fn get_var_keyword(&self) -> &str {
        if self.is_mutable {
            "mut"
        } else {
            "const"
        }
    }
}

#[derive(Debug, Clone)]
pub struct Reassignment {
    pub name: String,
    pub value: Expression,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FormattedSegment {
    // exemple "hey #{2 + 3} how r u ?"
    Literal(String),        // (ex: "hey " and " how r u ?")
    Expression(Expression), // (ex: 2 + 3 -> 5)
}
impl FormattedSegment {
    pub fn from_str(
        input: &str,
        symbol_table: &SymbolTable,
    ) -> Result<Vec<FormattedSegment>, ParsingError> {
        let mut result: Vec<FormattedSegment> = Vec::new();
        let mut current_part = String::new();
        let mut inside_expression = false;
        let mut expression = String::new();

        for (i, c) in input.chars().enumerate() {
            if inside_expression {
                if c == '}' {
                    // Finished parsing the expression, add it to the result
                    inside_expression = false;
                    let t = match tokenize(&expression[1..]) {
                        Ok(t) => t,
                        Err(_) => {
                            return Err(ParsingError::ExpectedValidExpressionInFormattedString)
                        }
                    };

                    let expr =
                        FormattedSegment::Expression(parse_expression(&t, &mut 0, symbol_table)?);
                    result.push(expr);
                    expression.clear();
                } else {
                    expression.push(c);
                }
            } else {
                if c == '#' {
                    // Check if this is the start of an expression
                    let next_char = input.chars().nth(i + 1);

                    if let Some('{') = next_char {
                        // This is the start of an expression
                        inside_expression = true;

                        // Add the current string part to the result
                        if !current_part.is_empty() {
                            result.push(FormattedSegment::Literal(current_part.to_string()));
                        }
                        current_part.clear();
                    } else {
                        // Just a regular '#' character, add it to the current part
                        current_part.push(c);
                    }
                } else {
                    // Add the character to the current part
                    current_part.push(c);
                }
            }
        }

        // Add the remaining string part to the result
        if !current_part.is_empty() {
            result.push(FormattedSegment::Literal(current_part));
        }

        Ok(result)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Exponential,
}
impl fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BinaryOperator::Plus => write!(f, "+"),
            BinaryOperator::Minus => write!(f, "-"),
            BinaryOperator::Multiply => write!(f, "*"),
            BinaryOperator::Divide => write!(f, "/"),
            BinaryOperator::Modulo => write!(f, "%"),
            BinaryOperator::Exponential => write!(f, "**"),
        }
    }
}
impl BinaryOperator {
    pub fn operator_as_verb(&self) -> String {
        match self {
            BinaryOperator::Plus => "add".to_string(),
            BinaryOperator::Minus => "substract".to_string(),
            BinaryOperator::Multiply => "multiply".to_string(),
            BinaryOperator::Divide => "divide".to_string(),
            BinaryOperator::Modulo => "modulate".to_string(),
            BinaryOperator::Exponential => "exponentiate".to_string(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub arguments: Vec<Declaration>,
    pub context: Box<ASTNode>,
    pub return_type: Option<VariableType>,
}
#[derive(Debug, Clone)]
pub struct MainFunctionData {
    pub name: String,
    pub arguments: Vec<Declaration>,
    pub return_type: Option<VariableType>,
}
impl MainFunctionData {
    pub fn from_function(f: &Function) -> MainFunctionData {
        MainFunctionData {
            name: f.name.to_string(),
            arguments: f.arguments.clone(),
            return_type: f.return_type.clone(),
        }
    }
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
impl fmt::Display for VariableType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            VariableType::String => write!(f, "String"),
            VariableType::Number => write!(f, "Number"),
            VariableType::Boolean => write!(f, "Boolean"),
            VariableType::Vector => write!(f, "Vector"),
            VariableType::Dictionary => write!(f, "Dictionary"),
        }
    }
}

impl VariableType {
    pub fn as_assignment(&self) -> &str {
        match self {
            VariableType::String => "str",
            VariableType::Number => "num",
            VariableType::Boolean => "bool",
            VariableType::Vector => "vec",
            VariableType::Dictionary => "dict",
        }
    }
    pub fn from_assignment(input: &str) -> Option<VariableType> {
        match input {
            "str" => Some(VariableType::String),
            "num" => Some(VariableType::Number),
            "bool" => Some(VariableType::Boolean),
            "vec" => Some(VariableType::Vector),
            "dict" => Some(VariableType::Dictionary),
            _ => None,
        }
    }
    pub fn is_assignment_type(input: &str) -> bool {
        match input {
            "str" | "num" | "bool" | "vec" | "dict" => true,
            _ => false,
        }
    }
    pub fn contains_assignment_type(input: &Expression) -> Option<VariableType> {
        match input {
            Expression::Variable(v) => {
                if VariableType::is_assignment_type(v) {
                    return Some(VariableType::from_assignment(v).unwrap());
                }
                None
            }
            Expression::Number(_) => None,
            Expression::String(_) => None,
            Expression::Boolean(_) => None,
            Expression::Null => None,
            Expression::ArrayExpression(v) => {
                for el in v {
                    if let Some(t) = VariableType::contains_assignment_type(el) {
                        return Some(t);
                    }
                }
                None
            }
            Expression::DictionaryExpression(h) => {
                for (_, v) in h.iter() {
                    if let Some(t) = VariableType::contains_assignment_type(v) {
                        return Some(t);
                    }
                }
                None
            }
            Expression::BinaryOperation { left, right, .. } => {
                if let Some(x) = VariableType::contains_assignment_type(left) {
                    return Some(x);
                }
                if let Some(x) = VariableType::contains_assignment_type(right) {
                    return Some(x);
                }
                None
            }
            Expression::FormattedString(_) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    //struct to keep track of variables, fns and everything created
    variables: HashMap<String, Declaration>,
    functions: HashMap<String, MainFunctionData>,
}
impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn insert_variable(&mut self, declaration: Declaration) {
        self.variables
            .insert(declaration.name.to_string(), declaration);
    }
    pub fn insert_function(&mut self, f: &Function) {
        self.functions
            .insert(f.name.to_string(), MainFunctionData::from_function(f));
    }
    pub fn reassign_variable(&mut self, reassignement: Reassignment) {
        let initial_var = self.get_variable(&reassignement.name).unwrap();

        self.variables.insert(
            reassignement.name.to_string(),
            Declaration {
                name: initial_var.name,
                var_type: initial_var.var_type,
                value: reassignement.value,
                is_mutable: true,
            },
        );
    }

    pub fn get_variable(&self, name: &str) -> Result<Declaration, TypeError> {
        let name_vec: Vec<&str> = name.split('.').collect();

        if name_vec.len() == 1 {
            let name_vec: Vec<&str> = name.split('[').collect();

            if name_vec.len() == 1 {
                return self
                    .variables
                    .get(name)
                    .map(|declaration| declaration.clone())
                    .ok_or_else(|| TypeError::CannotDetermineVarType {
                        name: name.to_string(),
                    });
            }

            let var = self
                .variables
                .get(name_vec[0])
                .map(|declaration| declaration.value.clone())
                .ok_or_else(|| TypeError::CannotDetermineVarType {
                    name: name.to_string(),
                })?;

            match var {
                Expression::ArrayExpression(vec) => {
                    let index = name_vec[1].parse::<usize>()?;
                    if index > vec.len() - 1 {
                        return Err(TypeError::IndexOutOfRange {
                            vec_name: name_vec[0].to_string(),
                            index,
                            length: vec.len(),
                        });
                    }
                    return Ok(Declaration {
                        name: name_vec[0].to_string(),
                        var_type: type_from_expression(&vec[index], &self)?,
                        value: vec[index].clone(),
                        is_mutable: true,
                    });
                }
                _ => unreachable!(),
            }
        }

        let mut var = self
            .variables
            .get(name_vec[0])
            .map(|declaration| declaration.value.clone())
            .ok_or_else(|| TypeError::CannotDetermineVarType {
                name: name.to_string(),
            })?;

        for (i, prop) in name_vec.iter().skip(1).enumerate() {
            match var {
                Expression::DictionaryExpression(expr) => {
                    if let Some(e) = expr.get(*prop) {
                        match e {
                            Expression::Variable(name) => {
                                var = self.get_variable(&name)?.value;
                                continue;
                            }
                            _ => {
                                var = e.to_owned();
                                continue;
                            }
                        }
                    } else {
                        return Err(TypeError::CannotDetermineObjPropTypeNotDefined {
                            obj_name: name_vec[0..=i].join("."),
                            prop: name_vec[0..=i + 1].join("."),
                        });
                    }
                }
                expr => {
                    println!("exprrrr: {:?}", expr);

                    var = expr.to_owned();
                    continue;
                }
            }
        }

        let var_type = type_from_expression(&var, &self)?;

        Ok(Declaration {
            name: name_vec.last().unwrap().to_string(),
            var_type,
            value: var,
            is_mutable: true,
        })
    }
    pub fn get_function(&self, name: &str) -> Option<MainFunctionData> {
        self.functions.get(name).cloned()
    }
}
