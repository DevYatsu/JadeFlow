// functions and constants in the standard lib

use std::collections::HashMap;
use std::fmt;

use crate::{
    evaluation::{evaluate_expression, EvaluationError},
    parser::{
        architecture::SymbolTable,
        expression::Expression,
        functions::Function,
        types::{err_on_fn_call_args_invalid, VariableType},
        vars::Declaration,
    },
};
use once_cell::sync::OnceCell;

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
            if let Some(output_t) = &self.return_type {
                output_t.as_assignment()
            } else {
                "null"
            },
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

    pub fn run_with_args(
        &self,
        args: &Vec<Expression>,
        symbol_table: &SymbolTable,
    ) -> Result<Expression, EvaluationError> {
        err_on_fn_call_args_invalid(&self.name, &self.arguments, args, symbol_table)?;
        let returned_expr = self.code_to_run.get().expect("Code not initialized");

        if self.return_type.is_none() {
            return Ok(Expression::Null);
        }

        Ok(evaluate_expression(
            returned_expr(
                args.to_owned()
                    .into_iter()
                    .map(|arg| -> Result<Expression, EvaluationError> {
                        evaluate_expression(arg.clone(), symbol_table)
                    })
                    .collect::<Result<Vec<Expression>, EvaluationError>>()?,
            ),
            symbol_table,
        )?)
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

fn load_std_math() -> HashMap<String, StandardFunction> {
    let sqrt = StandardFunction::new(
        "sqrt",
        vec![Function::argument("number", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(num) = &args[0] {
                Expression::Number(num.sqrt())
            } else {
                unreachable!()
            }
        }),
    );
    let abs = StandardFunction::new(
        "abs",
        vec![Function::argument("number", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(num) = &args[0] {
                Expression::Number(num.abs())
            } else {
                unreachable!()
            }
        }),
    );
    let sin = StandardFunction::new(
        "sin",
        vec![Function::argument("angle", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(angle) = &args[0] {
                Expression::Number(angle.sin())
            } else {
                unreachable!()
            }
        }),
    );

    let cos = StandardFunction::new(
        "cos",
        vec![Function::argument("angle", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(angle) = &args[0] {
                Expression::Number(angle.cos())
            } else {
                unreachable!()
            }
        }),
    );

    let tan = StandardFunction::new(
        "tan",
        vec![Function::argument("angle", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(angle) = &args[0] {
                Expression::Number(angle.tan())
            } else {
                unreachable!()
            }
        }),
    );

    let log = StandardFunction::new(
        "log",
        vec![Function::argument("value", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(value) = &args[0] {
                Expression::Number(value.ln())
            } else {
                unreachable!()
            }
        }),
    );

    let round = StandardFunction::new(
        "round",
        vec![Function::argument("number", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(num) = &args[0] {
                Expression::Number(num.round())
            } else {
                unreachable!()
            }
        }),
    );

    let floor = StandardFunction::new(
        "floor",
        vec![Function::argument("number", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(num) = &args[0] {
                Expression::Number(num.floor())
            } else {
                unreachable!()
            }
        }),
    );

    let ceil = StandardFunction::new(
        "ceil",
        vec![Function::argument("number", VariableType::Number)],
        Some(VariableType::Number),
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::Number(num) = &args[0] {
                Expression::Number(num.ceil())
            } else {
                unreachable!()
            }
        }),
    );

    create_function_map!(sqrt, abs, sin, cos, tan, log, floor, round, ceil)
}

fn load_std_console() -> HashMap<String, StandardFunction> {
    let print = StandardFunction::new(
        "print",
        vec![Function::argument("message", VariableType::String)],
        None,
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::String(msg) = &args[0] {
                println!("{msg}");
                Expression::Null
            } else {
                unreachable!()
            }
        }),
    );

    let input = StandardFunction::new(
        "input",
        vec![Function::argument("prompt", VariableType::String)],
        Some(VariableType::String),
        Box::new(|args: Vec<Expression>| -> Expression {
            let mut input = String::new();

            if let Expression::String(msg) = &args[0] {
                print!("{} ", msg);
                std::io::stdin()
                    .read_line(&mut input)
                    .expect("Failed to read input");
                Expression::String(input.trim().to_string())
            } else {
                unreachable!()
            }
        }),
    );

    // You can also create a function similar to 'print' that prints to standard error
    let eprint = StandardFunction::new(
        "eprint",
        vec![Function::argument("message", VariableType::String)],
        None,
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::String(msg) = &args[0] {
                eprintln!("{msg}");
                Expression::Null
            } else {
                unreachable!()
            }
        }),
    );

    let print_no_newline = StandardFunction::new(
        "print_no_newline",
        vec![Function::argument("text", VariableType::String)],
        None,
        Box::new(|args: Vec<Expression>| -> Expression {
            if let Expression::String(text) = &args[0] {
                print!("{}", text);
                Expression::Null
            } else {
                unreachable!()
            }
        }),
    );

    create_function_map!(print, input, eprint, print_no_newline)
}
