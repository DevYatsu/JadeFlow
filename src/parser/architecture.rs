use super::ParsingError;
use std::{collections::HashMap, fmt};

#[derive(Debug, Clone)]
pub enum ASTNode {
    Program(Vec<Statement>),
    VariableDeclaration(Declaration),
    VariableReassignment(Reassignment),
    FunctionDeclaration(Function),
    ClassDeclaration(Class),
    Expression(Expression),
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

#[derive(Debug, Clone)]
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
    FormatedString(Vec<FormattedSegment>),
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
            Expression::FormatedString(segments) => {
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
        let var = if self.is_mutable {"mut"}else{"const"};
        write!(f, "{} {}: {} = {}", var, self.name, self.var_type.as_assignment(), self.value)
    }
}

#[derive(Debug, Clone)]
pub struct Reassignment {
    pub name: String,
    pub value: Expression,
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
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    //struct to keep track of variables, fns and everything created
    variables: HashMap<String, Declaration>,
}
impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            variables: HashMap::new(),
        }
    }

    pub fn insert_variable(&mut self, declaration: Declaration) {
        self.variables
            .insert(declaration.name.to_string(), declaration);
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

    pub fn get_variable(&self, name: &str) -> Option<Declaration> {
        self.variables.get(name).cloned()
    }
}
