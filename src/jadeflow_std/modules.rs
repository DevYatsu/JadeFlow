use crate::parser::{class::Class, functions::Function, vars::Declaration};
use std::collections::HashMap;
pub mod fs;

pub struct Module {
    pub name: String,
    pub elements: HashMap<String, ModuleComponent>,
}

pub enum ModuleComponent {
    Variable(Declaration),
    Class(Class),
    Function(Function),
}

impl Module {
    pub fn new(name: &str) -> Module {
        Module {
            name: name.to_owned(),
            elements: HashMap::new(),
        }
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
impl From<Function> for ModuleComponent {
    fn from(value: Function) -> Self {
        ModuleComponent::Function(value)
    }
}

#[macro_export]
macro_rules! generate_module {
    (in $module:expr;
     $(Class $class:expr),* $(,)*,
     $(Function $function:expr),* $(,)*,
     $(Const $constant:expr),* $(,)*
    ) => {
        {
            let mut module = Module::new($module);

            $(
                module.elements.insert($class.name.clone(), ModuleComponent::from($class));
            )*

            $(
                match $function {
                    Function::StandardFunction {name, ..} => {
                        module.elements.insert(name.clone(), ModuleComponent::from($function));
                    }
                    Function::DefinedFunction {name, ..} => {
                        module.elements.insert(name.clone(), ModuleComponent::from($function));
                    }
                }
            )*

            $(
                module.elements.insert($constant.name.clone(), ModuleComponent::from($constant));
            )*

            module
        }
    };
}
