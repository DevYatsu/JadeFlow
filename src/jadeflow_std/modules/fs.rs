use crate::{
    generate_module,
    jadeflow_std::{modules::ModuleComponent, StandardFunction},
    parser::{class::Class, expression::Expression, functions::Argument, types::VariableType},
};

use super::Module;

pub fn load_fs_module() -> Module {
    todo!();
    // need to implement classes correctly and make them into possible types
    let file: Class = todo!(); // create a file class

    let open = StandardFunction::new(
        "open",
        vec![Argument::new(
            "file path".to_owned(),
            VariableType::String,
            false,
        )],
        Some(VariableType::Dictionary), // return File
        Box::new(|args: Vec<Expression>| -> Expression { todo!() }),
    );

    let rename = StandardFunction::new(
        "open",
        vec![
            Argument::new("file path".to_owned(), VariableType::String, false),
            Argument::new("new name".to_owned(), VariableType::String, false),
        ],
        None, // return File
        Box::new(|args: Vec<Expression>| -> Expression { todo!() }),
    );

    generate_module!(in "fs";
        file,
        open,
        rename
    )
}
