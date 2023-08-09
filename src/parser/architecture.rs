use crate::token::{tokenize, Token};

use super::{
    expression::parse_expression,
    functions::{parse_fn_header, FunctionParsingError},
    parse,
    types::type_from_expression,
    ParsingError, TypeError,
};
use std::{collections::HashMap, fmt, iter::Peekable};

#[derive(Debug, Clone)]
pub enum ASTNode {
    Program(Program),
    VariableDeclaration(Declaration),
    VariableReassignment(Reassignment),

    FunctionDeclaration(Function),
    // corresponds to both {} and => functions
    ClassDeclaration(Class),

    Return(Expression),
    FunctionCall(FunctionCall),
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
pub fn function_call(call: FunctionCall) -> Statement {
    Statement {
        node: ASTNode::FunctionCall(call),
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
    FunctionCall(FunctionCall),
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
            Expression::FunctionCall(call) => {
                write!(f, "fn ")?;
                write!(f, "{} ", call.function_name)?;
                write!(f, "(")?;

                for argument in &call.arguments {
                    write!(f, "{}, ", argument)?;
                }
                write!(f, ")")
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
    pub is_object_prop: bool,
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
        tokenize(source_code.as_bytes()).unwrap().into()
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

#[derive(Debug, Clone)]
pub struct Class {
    pub name: String,
    pub arguments: Vec<Declaration>,
    pub global_properties: HashMap<String, Expression>,
    pub public_ctx: ClassCtx,
    pub private_ctx: ClassCtx,
}
#[derive(Debug, Clone)]
pub struct ClassCtx {
    pub methods: HashMap<String, Function>,
    pub properties: HashMap<String, Expression>,
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
        symbol_table: &mut SymbolTable,
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
                    let t: Vec<Token> = match tokenize(expression[1..].as_bytes()) {
                        Ok(t) => t.into(),
                        Err(_) => {
                            return Err(ParsingError::ExpectedValidExpressionInFormattedString)
                        }
                    };

                    let expr = FormattedSegment::Expression(parse_expression(
                        &mut t.iter().peekable(),
                        symbol_table,
                    )?);
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
                            result.push(FormattedSegment::Literal(current_part.clone()));
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
            BinaryOperator::Plus => "add".to_owned(),
            BinaryOperator::Minus => "substract".to_owned(),
            BinaryOperator::Multiply => "multiply".to_owned(),
            BinaryOperator::Divide => "divide".to_owned(),
            BinaryOperator::Modulo => "modulate".to_owned(),
            BinaryOperator::Exponential => "exponentiate".to_owned(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub arguments: Vec<Declaration>,
    pub context: Vec<Token>,
    pub return_type: Option<VariableType>,
}
impl Function {
    pub fn with_args(
        &mut self,
        args: &Vec<Expression>,
        symbol_table: &mut SymbolTable,
    ) -> Result<Expression, ParsingError> {
        let types_vec = self.args_types();

        for (i, arg) in args.iter().enumerate() {
            let actual_arg_type = type_from_expression(arg, symbol_table, None)?;

            if types_vec[i] != actual_arg_type {
                return Err(FunctionParsingError::InvalidFnCallArgType {
                    fn_name: self.name.to_owned(),
                    arg_name: self.arguments[i].name.to_owned(),
                    required_t: types_vec[i].clone(),
                    found_t: actual_arg_type,
                }
                .into());
            }
        }

        let new_args = self
            .arguments
            .iter()
            .enumerate()
            .map(|(i, arg)| {
                let mut new_arg = arg.clone();
                new_arg.value = args[i].clone();
                new_arg
            })
            .collect::<Vec<Declaration>>();

        self.arguments = new_args;

        let mut tokens = self.args_as_tokens();
        tokens.append(&mut self.context);

        let tokens_iter = tokens.iter().peekable();

        let mut program = match parse(tokens_iter.clone(), Some(symbol_table))? {
            ASTNode::Program(p) => p,
            _ => unreachable!(),
        };

        self.compare_types(&mut program)?; // return an error if returned type is different from expected

        Ok(Expression::Null)
    }

    fn args_as_tokens(&self) -> Vec<Token> {
        self.arguments
            .iter()
            .flat_map(|dec| dec.equivalent_tokens())
            .collect()
    }

    fn args_types(&self) -> Vec<VariableType> {
        self.arguments
            .iter()
            .map(|dec| dec.var_type.clone())
            .collect()
    }

    fn get_returned_type(program: &mut Program) -> Option<VariableType> {
        if let Some(Statement {
            node: ASTNode::Return(returned),
        }) = program.statements.last()
        {
            type_from_expression(returned, &mut program.symbol_table, None).ok()
        } else {
            None
        }
    }

    fn compare_types(&self, program: &mut Program) -> Result<(), ParsingError> {
        let return_type = self.return_type.clone();

        let found = Function::get_returned_type(program).clone();

        if return_type != found {
            if let Some(return_type) = return_type {
                if let Some(found) = found {
                    return Err(FunctionParsingError::ReturnTypeInvalid {
                        fn_name: self.name.to_owned(),
                        return_type: return_type.to_string(),
                        found: found.to_string(),
                    }
                    .into());
                } else {
                    return Err(FunctionParsingError::MissingReturnStatement {
                        fn_name: self.name.to_owned(),
                    }
                    .into());
                }
            } else {
                return Err(FunctionParsingError::MissingReturnStatement {
                    fn_name: self.name.to_owned(),
                }
                .into());
            }
        }

        Ok(())
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "fn {}({}) => {}",
            self.name,
            self.arguments
                .iter()
                .map(|arg| format!("{}: {}", arg.name, arg.var_type.as_assignment()))
                .collect::<Vec<String>>()
                .join(", "),
            if let Some(output_t) = &self.return_type {
                output_t.as_assignment()
            } else {
                "null"
            },
        )
    }
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
            name: f.name.clone(),
            arguments: f.arguments.clone(),
            return_type: f.return_type.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub function_name: String,
    pub arguments: Vec<Expression>,
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
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    //struct to keep track of variables, fns and everything created
    variables: HashMap<String, Declaration>,
    functions: HashMap<String, Function>,
    registered_functions: HashMap<String, MainFunctionData>,
}
impl SymbolTable {
    pub fn new() -> Self {
        SymbolTable {
            variables: HashMap::new(),
            functions: HashMap::new(),
            registered_functions: HashMap::new(),
        }
    }

    pub fn merge(first_table: &SymbolTable, second_table: SymbolTable) -> SymbolTable {
        let mut table = SymbolTable::new();

        table.variables.extend(first_table.variables.clone());
        table.functions.extend(first_table.functions.clone());
        table.variables.extend(second_table.variables);
        table.functions.extend(second_table.functions);

        table
    }

    pub fn insert_variable(&mut self, declaration: Declaration) {
        self.variables.insert(declaration.name.clone(), declaration);
    }
    pub fn insert_function(&mut self, f: &Function) {
        self.functions.insert(f.name.to_owned(), f.clone());
    }
    pub fn register_function(&mut self, f: &Function) {
        self.registered_functions
            .insert(f.name.to_owned(), MainFunctionData::from_function(&f));
    }

    pub fn reassign_variable(
        &mut self,
        reassignement: Reassignment,
        tokens: &mut Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<(), ParsingError> {
        if self.is_object_prop(&reassignement.name) {
            let parent_name = reassignement.name.rsplitn(2, '.').collect::<Vec<&str>>();
            let initial_var = self.get_variable(&parent_name[1], Some(tokens))?;

            match initial_var.value {
                Expression::DictionaryExpression(mut hash) => {
                    hash.insert(parent_name[0].to_owned(), reassignement.value.clone());

                    self.insert_variable(Declaration {
                        value: Expression::DictionaryExpression(hash),
                        ..initial_var
                    });
                    let var_type = type_from_expression(&reassignement.value, self, Some(tokens))?;
                    self.insert_variable(Declaration {
                        value: reassignement.value,
                        name: reassignement.name,
                        var_type,
                        is_mutable: true,
                        is_object_prop: true,
                    });
                }
                _ => {
                    return Err(ParsingError::Custom {
                        data: format!("'{}' is not a valid object", parent_name[1]),
                    })
                }
            }

            // let dec = Declaration {
            //     name: reassignement.name.clone(),
            //     var_type: type_from_expression(&reassignement.value, self, Some(tokens))?,
            //     value: reassignement.value,
            //     is_mutable: true,
            //     is_object_prop: true,
            // };
            // self.variables.insert(reassignement.name, dec);
        } else {
            let initial_var = self.get_variable(&reassignement.name, Some(tokens))?;
            if reassignement.value != initial_var.value {
                self.variables.insert(
                    reassignement.name.clone(),
                    Declaration {
                        name: initial_var.name,
                        var_type: initial_var.var_type,
                        value: reassignement.value,
                        is_mutable: true,
                        is_object_prop: initial_var.is_object_prop,
                    },
                );
            }
        }
        Ok(())
    }

    pub fn get_variable(
        &mut self,
        name: &str,
        tokens: Option<&mut Peekable<std::slice::Iter<'_, Token>>>,
    ) -> Result<Declaration, TypeError> {
        let name_vec: Vec<&str> = name.split('.').collect();

        if name_vec.len() == 1 {
            let name_vec: Vec<&str> = name.split('[').collect();

            if name_vec.len() == 1 {
                return self
                    .variables
                    .get(name)
                    .map(|declaration| declaration.clone())
                    .ok_or_else(|| TypeError::CannotDetermineVarType {
                        name: name.to_owned(),
                    });
            }

            let var = self
                .variables
                .get(name_vec[0])
                .map(|declaration| declaration.value.clone())
                .ok_or_else(|| TypeError::CannotDetermineVarType {
                    name: name.to_owned(),
                })?;

            match var {
                Expression::ArrayExpression(vec) => {
                    let index = name_vec[1].parse::<usize>()?;
                    if index > vec.len() - 1 {
                        return Err(TypeError::IndexOutOfRange {
                            vec_name: name_vec[0].to_owned(),
                            index,
                            length: vec.len(),
                        });
                    }
                    return Ok(Declaration {
                        name: name_vec[0].to_owned(),
                        var_type: type_from_expression(&vec[index], self, tokens)?,
                        value: vec[index].clone(),
                        is_mutable: true,
                        is_object_prop: false,
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
                name: name.to_owned(),
            })?
            .clone();

        for (i, prop) in name_vec.iter().skip(1).enumerate() {
            match var {
                Expression::DictionaryExpression(expr) => {
                    if let Some(e) = expr.get(*prop) {
                        match e {
                            Expression::Variable(name) => {
                                var = self.get_variable(&name, None)?.value;
                                continue;
                            }
                            _ => {
                                var = e.clone();
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
                    var = expr.clone();
                    continue;
                }
            }
        }

        let var_type = type_from_expression(&var, self, tokens)?;

        Ok(Declaration {
            name: name_vec.last().unwrap().to_string(),
            var_type,
            value: var,
            is_mutable: true,
            is_object_prop: true,
        })
    }

    pub fn is_object_prop(&mut self, name: &str) -> bool {
        let name_vec = name.split('.').collect::<Vec<&str>>();

        if name_vec.len() == 1 {
            false
        } else {
            true
        }
    }

    pub fn is_fn_declared(&self, name: &str) -> bool {
        self.functions.get(name).is_some()
    }

    pub fn get_function(
        &mut self,
        name: &str,
        tokens: &mut Peekable<std::slice::Iter<'_, Token>>,
    ) -> Result<MainFunctionData, ParsingError> {
        let func = self.functions.get(name);

        if func.is_none() {
            return Err(FunctionParsingError::NotDefinedFunction {
                fn_name: name.to_owned(),
            }
            .into());
        }

        Ok(func
            .cloned()
            .map(|f| MainFunctionData::from_function(&f))
            .ok_or_else(|| FunctionParsingError::NotDefinedFunction {
                fn_name: name.to_owned(),
            })?)
    }
}

impl fmt::Display for SymbolTable {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "-- VARIABLES -- \n",)?;
        if self.variables.len() == 0 {
            write!(f, "None\n")?;
        } else {
            for var in &self.variables {
                write!(f, "{}\n", var.1.to_string())?;
            }
        }

        write!(f, "-- FUNCTIONS -- \n",)?;

        if self.functions.len() == 0 {
            {
                write!(f, "None\n")?;
            }
        } else {
            for var in &self.functions {
                write!(f, "- {}\n", var.1.to_string())?;
            }
        }

        write!(f, "-- Registered FUNCTIONS -- \n",)?;

        if self.registered_functions.len() == 0 {
            Ok({
                write!(f, "None\n")?;
            })
        } else {
            Ok(for var in &self.functions {
                write!(f, "- {}\n", var.1.to_string())?;
            })
        }
    }
}
