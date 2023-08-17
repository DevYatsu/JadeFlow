use crate::{
    function, generate_module,
    jadeflow_std::modules::ModuleComponent,
    parser::{
        class::Class,
        expression::Expression,
        functions::{Argument, Function},
        types::VariableType,
    },
};
use once_cell::sync::OnceCell;

use super::Module;

pub fn load_fs_module() -> Module {
    todo!();
    // need to implement classes correctly and make them into possible types
    let file: Class = todo!(); // create a file class

    let open = function!(
        "open",
        arguments: vec![Argument::new(
            "file path".to_owned(),
            VariableType::String,
            false,
        )],
        return_type: Some(VariableType::Dictionary),
        code: |args: Vec<Expression>| -> Expression { todo!() }
    );

    let rename = function!(
        "rename",
        arguments: vec![
            Argument::new("file path".to_owned(), VariableType::String, false),
            Argument::new("new name".to_owned(), VariableType::String, false),
        ],
        return_type: None, // return File
        code: |args: Vec<Expression>| -> Expression { todo!() }
    );

    generate_module!(in "fs";
        Class file,
        Function open,
        Function rename,
    )
}
