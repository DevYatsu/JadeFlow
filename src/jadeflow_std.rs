// functions and constants in the standard lib
mod console;
mod math;
mod modules;
use hashbrown::HashMap;

use crate::parser::functions::Function;

use self::{console::load_std_console, math::load_std_math};

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

pub fn load_std() -> HashMap<String, Function> {
    let maths_fns = load_std_math();
    let console_fns = load_std_console();
    // initialize all standard fns

    merge_hash_maps!(maths_fns, console_fns)
}
