use indexmap::IndexMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::{cmp::Ordering, collections::HashMap};

use crate::execution::arity::Arity;
use crate::parsing::lexer::Token;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Nothing,
    String,
    StructDescriptor(StructDescriptorType),
    StructInstance(StructInstanceType),
    EnumDescriptor(EnumType),

    Callable(Callable),

    Union(HashSet<Type>),

    Unspecified,
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for Type {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Type::Int
            | Type::Float
            | Type::Bool
            | Type::Nothing
            | Type::String
            | Type::Unspecified => {}

            Type::StructDescriptor(s) => s.hash(state),
            Type::StructInstance(i) => i.hash(state),
            Type::EnumDescriptor(e) => e.hash(state),
            Type::Callable(c) => c.hash(state),
            Type::Union(u) => {
                for item in u {
                    item.hash(state);
                }
            }
        }
    }
}

#[derive(Clone, Debug, Eq)]
pub struct StructDescriptorType {
    fields: IndexMap<String, Type>,
    methods: HashMap<String, Callable>,
}

impl PartialEq for StructDescriptorType {
    fn eq(&self, other: &Self) -> bool {
        self.fields == other.fields
    }
}

impl Hash for StructDescriptorType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for f in &self.fields {
            f.0.hash(state);
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct StructInstanceType {
    descriptor: Token,
}

#[derive(Clone, Debug, Eq)]
pub struct EnumType {
    variants: HashMap<String, StructDescriptorType>,
}

impl PartialEq for EnumType {
    fn eq(&self, other: &Self) -> bool {
        self.variants == other.variants
    }
}

impl Hash for EnumType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        for (variant_name, var_desc) in &self.variants {
            variant_name.hash(state);
            var_desc.hash(state);
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Callable {
    pub arguments: Vec<Type>,
    pub vararg: Option<Box<Type>>,
    pub return_type: Box<Type>,
}

impl Type {
    pub fn can_set_field(&self) -> bool {
        matches!(self, Type::StructInstance(_) | Type::Unspecified)
    }

    pub fn get_field(&self, field_name: &str) -> Option<Type> {
        match self {
            Type::Int => todo!(),
            Type::Float => todo!(),
            Type::Bool => todo!(),
            Type::Nothing => todo!(),
            Type::String => todo!(),
            Type::StructDescriptor(_) => todo!(),
            Type::StructInstance(_) => todo!(),
            Type::EnumDescriptor(_) => todo!(),
            Type::Callable(_) => todo!(),
            Type::Union(_) => todo!(),
            Type::Unspecified => Some(Type::Unspecified),
        }
    }

    pub fn build_union(left: Type, right: Type) -> Type {
        if left == right {
            return left;
        }

        match (left, right) {
            (left, right) if left.is_unspecified() || right.is_unspecified() => Default::default(),
            (Type::Union(mut u1), Type::Union(u2)) => {
                u1.extend(u2.into_iter());
                Type::Union(u1)
            }
            (Type::Union(mut u1), right) => {
                u1.insert(right);
                Type::Union(u1)
            }
            (left, Type::Union(mut u)) => {
                u.insert(left);
                Type::Union(u)
            }
            (other1, other2) => {
                let mut union = HashSet::new();
                union.insert(other1);
                union.insert(other2);
                Type::Union(union)
            }
        }
    }

    pub fn build_function(args: Vec<Type>, vararg: Option<Type>, ret: Type) -> Type {
        Self::Callable(Callable {
            arguments: args,
            vararg: vararg.map(Box::new),
            return_type: Box::new(ret),
        })
    }

    pub fn is_unspecified(&self) -> bool {
        matches!(self, Type::Unspecified)
    }

    pub fn is_union(&self) -> bool {
        matches!(self, Type::Union(..))
    }

    pub fn get_arity(&self) -> Option<Arity> {
        match self {
            Type::StructDescriptor(s) => Some(Arity::Exact(s.fields.len())),
            Type::Callable(c) => Some(match &c.vararg {
                Some(_) => Arity::AtLeast(c.arguments.len()),
                None => Arity::Exact(c.arguments.len()),
            }),
            Type::Union(items) => {
                let arities = items
                    .iter()
                    .map(|i| i.get_arity())
                    .collect::<Option<Vec<_>>>()?;

                let (head, tail) = arities.split_first().unwrap();

                let sample = *head;
                if tail.iter().all(|a| a == &sample) {
                    Some(sample)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Type::Unspecified
    }
}

impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self == other {
            return Some(Ordering::Equal);
        }

        if self.is_unspecified() {
            return Some(Ordering::Equal);
        }
        if other.is_unspecified() {
            return Some(Ordering::Equal);
        }

        None
    }
}
