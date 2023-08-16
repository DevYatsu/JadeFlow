// functions and constants in the standard lib
mod console;
mod math;
use std::collections::HashMap;
use std::fmt;

use crate::{
    evaluation::{evaluate_expression, EvaluationError},
    parser::{
        architecture::SymbolTable,
        expression::Expression,
        functions::RunnableFunction,
        types::{err_on_fn_call_args_invalid, VariableType},
        vars::Declaration,
    },
};
use once_cell::sync::OnceCell;

use self::{console::load_std_console, math::load_std_math};

pub struct StandardFunction {
    pub name: String,
    pub arguments: Vec<Declaration>,
    pub return_type: Option<VariableType>,
    pub code_to_run: OnceCell<Box<dyn Fn(Vec<Expression>) -> Expression>>,
}

impl Clone for StandardFunction {
    fn clone(&self) -> Self {
        StandardFunction {
            name: self.name.clone(),
            arguments: self.arguments.clone(),
            return_type: self.return_type.clone(),
            code_to_run: OnceCell::new(),
        }
    }
}

impl fmt::Debug for StandardFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "StandardFunction {{ name: {}, arguments: {:?}, return_type: {:?} }}",
            self.name, self.arguments, self.return_type
        )
    }
}
impl fmt::Display for StandardFunction {
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
            self.return_type
                .as_ref()
                .map_or("null", |output_t| output_t.as_assignment()),
        )
    }
}

impl StandardFunction {
    pub fn new(
        name: &str,
        arguments: Vec<Declaration>,
        return_type: Option<VariableType>,
        code: Box<dyn Fn(Vec<Expression>) -> Expression>,
    ) -> StandardFunction {
        let code_to_run = OnceCell::from(code);
        StandardFunction {
            name: name.to_string(),
            arguments,
            return_type,
            code_to_run,
        }
    }
}
impl RunnableFunction for StandardFunction {
    fn run_with_args(
        &self,
        args: &Vec<Expression>,
        symbol_table: &SymbolTable,
    ) -> Result<Expression, EvaluationError> {
        err_on_fn_call_args_invalid(&self.name, &self.arguments, args, symbol_table)?;
        let std_func = self.code_to_run.get().expect("Code not initialized");

        let returned_expr = std_func(
            args.to_owned()
                .into_iter()
                .map(|arg| -> Result<Expression, EvaluationError> {
                    evaluate_expression(arg.clone(), symbol_table)
                })
                .collect::<Result<Vec<Expression>, EvaluationError>>()?,
        );
        if self.return_type.is_none() {
            return Ok(Expression::Null);
        }

        Ok(evaluate_expression(returned_expr, symbol_table)?)
    }
}

#[macro_export]
macro_rules! create_function_map {
    ( $( $func:expr ),* ) => {{
        use std::collections::HashMap;

        let mut function_map = HashMap::new();

        $(
            function_map.insert($func.name.clone(), $func);
        )*

        function_map
    }};
}

macro_rules! merge_fns_modules {
    ( $( $module:expr ),* ) => {
        {
            let mut modules = HashMap::new();
            $(
                modules.extend($module);
            )*
            modules
        }
    };
}

pub fn load_std() -> HashMap<String, StandardFunction> {
    let maths_fns = load_std_math();
    let console_fns = load_std_console();
    // initialize all standard fns

    merge_fns_modules!(maths_fns, console_fns)
}
