// functions and constants in the standard lib
mod console;
mod math;
mod modules;
use crate::parser::functions::Function;
use hashbrown::HashMap;

use self::{console::load_jadeflow_console, math::load_jadeflow_math};

#[macro_export]
macro_rules! create_function_map {
    ( $( $func:expr ),* ) => {{
    use hashbrown::HashMap;

        let mut function_map = HashMap::new();

        $(
            match &$func {
                Function::StandardFunction {name,..} => {
                    function_map.insert(name.clone(), $func);
                }
                Function::DefinedFunction {name, ..} => {
                    function_map.insert(name.clone(), $func);
                }
            }
        )*

        function_map
    }};
}

macro_rules! merge_hash_maps {
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

pub fn load_jadeflow_fns() -> HashMap<String, Function> {
    let maths_fns = load_jadeflow_math();
    let console_fns = load_jadeflow_console();
    // initialize all language fns

    merge_hash_maps!(maths_fns, console_fns)
}
