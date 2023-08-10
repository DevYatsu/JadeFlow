use std::fmt;

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

