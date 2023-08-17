use std::collections::HashMap;

use crate::parser::{class::Class, functions::Function, vars::Declaration};

use super::StandardFunction;

pub mod fs;

pub struct Module {
    pub name: String,
    pub elements: HashMap<String, ModuleComponent>,
}

pub enum ModuleComponent {
    Function(Function),
    // need to put classical Function with StandardFunction
    Variable(Declaration),
    Class(Class),
    StandardFunction(StandardFunction),
}

impl Module {
    pub fn new(name: &str) -> Module {
        Module {
            name: name.to_owned(),
            elements: HashMap::new(),
        }
    }
}
impl From<Function> for ModuleComponent {
    fn from(value: Function) -> Self {
        ModuleComponent::Function(value)
    }
}
impl From<Declaration> for ModuleComponent {
    fn from(value: Declaration) -> Self {
        ModuleComponent::Variable(value)
    }
}
impl From<Class> for ModuleComponent {
    fn from(value: Class) -> Self {
        ModuleComponent::Class(value)
    }
}
impl From<StandardFunction> for ModuleComponent {
    fn from(value: StandardFunction) -> Self {
        ModuleComponent::StandardFunction(value)
    }
}

#[macro_export]
macro_rules! generate_module {
    (in $module:expr; $($args:expr),* $(,)?) => {
        {
            let mut module = Module::new($module);
            $(
                module.elements.insert($args.name.clone(), ModuleComponent::from($args));
            )*
            module
        }
    };
}
