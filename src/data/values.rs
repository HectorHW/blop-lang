use std::fmt::{Display, Formatter};

pub type Value = StackObject;

#[derive(Copy, Clone, Debug)]
pub enum StackObject {
    Int(i64),
    Function { chunk_id: usize },
}

impl Display for StackObject {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StackObject::Int(n) => {
                write!(f, "{}", n)
            }
            StackObject::Function { .. } => {
                write!(f, "{}", "function")
            }
        }
    }
}

impl StackObject {
    pub fn unwrap_int(&self) -> Option<i64> {
        match self {
            StackObject::Int(n) => Some(*n),
            _ => None,
        }
    }

    pub fn type_string(&self) -> String {
        match self {
            StackObject::Int(_) => "int".to_string(),
            StackObject::Function { .. } => "function".to_string(),
        }
    }
}

impl PartialEq for StackObject {
    fn eq(&self, other: &Self) -> bool {
        use StackObject::*;
        match (self, other) {
            (Int(a), Int(b)) => a == b,
            (Function { chunk_id: a }, Function { chunk_id: b }) => a == b,
            _ => false,
        }
    }
}
