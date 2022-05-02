use indexmap::IndexMap;
use std::collections::HashSet;
use std::fmt::format;
use std::hash::Hash;
use std::{cmp::Ordering, collections::HashMap};

use crate::compile::checks::Annotations;
use crate::execution::arity::Arity;
use crate::parsing::lexer::Token;

use super::typechecker::{Checker, SomewhereTypeError, TypeError, Typemap};

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

    Union(TypeUnion),

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
                for item in &u.0 {
                    item.hash(state);
                }
            }
        }
    }
}

#[derive(Clone, Debug, Eq)]
pub struct StructDescriptorType {
    pub name: Token,
    pub fields: IndexMap<String, Type>,
    pub methods: HashMap<String, Callable>,
}

impl StructDescriptorType {
    pub fn get_field(&self, name: &str) -> Option<Type> {
        self.fields.get(name).cloned()
    }

    pub fn get_method(&self, name: &str) -> Option<Callable> {
        self.methods.get(name).cloned()
    }

    pub fn get_field_idx(&self, idx: usize) -> Option<Type> {
        self.fields.get_index(idx).map(|(k, v)| v.clone())
    }
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
    pub descriptor: Token,
}

#[derive(Clone, Debug, Eq)]
pub struct EnumType {
    pub variants: HashMap<String, StructDescriptorType>,
    pub methods: HashMap<String, Callable>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct TypeUnion(HashSet<Type>);

impl TypeUnion {
    pub fn project<F, T, E>(&self, op: F) -> Result<Type, E>
    where
        F: Fn(Type) -> Result<Type, E>,
    {
        let items = self
            .0
            .iter()
            .cloned()
            .map(op)
            .collect::<Result<Vec<_>, _>>()?;
        Ok(items
            .into_iter()
            .fold(Type::Union(TypeUnion(Default::default())), |a, i| {
                Type::build_union(a, i)
            }))
    }
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

impl Callable {
    pub fn as_bound_method(&self) -> Callable {
        let args = self.arguments.split_first().unwrap().1.to_owned();
        Callable {
            arguments: args,
            vararg: self.vararg.clone(),
            return_type: self.return_type.clone(),
        }
    }
}

impl Type {
    pub fn build_union(left: Type, right: Type) -> Type {
        if left == right {
            return left;
        }

        match (left, right) {
            (left, right) if left.is_unspecified() || right.is_unspecified() => Default::default(),
            (Type::Union(mut u1), Type::Union(u2)) => {
                u1.0.extend(u2.0.into_iter());
                Type::Union(u1)
            }
            (Type::Union(mut u1), right) => {
                u1.0.insert(right);
                Type::Union(u1)
            }
            (left, Type::Union(mut u)) => {
                u.0.insert(left);
                Type::Union(u)
            }
            (other1, other2) => {
                let mut union = HashSet::new();
                union.insert(other1);
                union.insert(other2);
                Type::Union(TypeUnion(union))
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
                    .0
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

    fn check_fn_less(first: &Type, second: &Type) -> bool {
        match (first, second) {
            (Type::Callable(c1), Type::Callable(c2)) => {
                if c1.arguments.len() != c2.arguments.len()
                    || c1.vararg.is_some() != c2.vararg.is_some()
                {
                    return false;
                }
                c1.arguments
                    .iter()
                    .zip(c2.arguments.iter())
                    .all(|(a1, a2)| PartialOrd::le(a1, a2))
                    && c1
                        .vararg
                        .as_ref()
                        .into_iter()
                        .zip(c2.vararg.as_ref().into_iter())
                        .map(|(v1, v2)| PartialOrd::le(v1, v2))
                        .last()
                        .unwrap_or(true)
                    && PartialOrd::le(&c1.return_type, &c2.return_type)
            }
            _ => false,
        }
    }

    pub(crate) fn perform_lookup(
        target: Type,
        property: &Token,
        type_map: &Typemap,
        _annotations: &Annotations,
    ) -> Result<Type, TypeError> {
        match &target {
            Type::StructInstance(i) => {
                let descriptor = type_map.type_of((&i.descriptor).into());

                match descriptor {
                    Type::StructDescriptor(s) => {
                        match crate::compile::compiler::Compiler::try_parse_special_field_access(
                            property,
                        ) {
                            Ok(k) => {
                                let v = if let Some(prop_idx) = k {
                                    s.get_field_idx(prop_idx as usize).ok_or_else(|| {
                                        SomewhereTypeError::OperationUnsupported {
                                            target,
                                            message: format!(
                                                "index too big: no field with index {}",
                                                prop_idx,
                                            ),
                                        }
                                        .at(property.position)
                                    })?
                                } else {
                                    s.get_method(property.get_string().unwrap())
                                        .map(|c| {
                                            //bind method
                                            Type::Callable(c.as_bound_method())
                                        })
                                        .or_else(|| s.get_field(property.get_string().unwrap()))
                                        .ok_or_else(|| {
                                            SomewhereTypeError::AttributeError {
                                                target_type: target,
                                                field: property.get_string().unwrap().to_string(),
                                            }
                                            .at(property.position)
                                        })?
                                };
                                Ok(v)
                            }
                            Err(e) => Err(SomewhereTypeError::OperationUnsupported {
                                target,
                                message: e,
                            }
                            .at(property.position))?,
                        }
                    }
                    Type::Unspecified => Ok(Type::Unspecified),
                    _ => unreachable!(),
                }
            }
            Type::Union(_) => {
                Ok(Type::Unspecified)
                //TODO : union ops
            }
            Type::EnumDescriptor(e) => {
                Ok(Type::Unspecified)
                //TODO: get desc
            }
            Type::Unspecified => Ok(Type::Unspecified),
            _ => Err(SomewhereTypeError::AttributeError {
                target_type: target,
                field: property.get_string().unwrap().to_string(),
            }
            .at(property.position))?,
        }
    }

    pub fn perform_set(
        target: Type,
        property: &Token,
        set: Type,
        type_map: &Typemap,
        _annotations: &Annotations,
    ) -> Result<(), TypeError> {
        match &target {
            Type::StructInstance(i) => {
                let descriptor = type_map.type_of((&i.descriptor).into());

                match descriptor {
                    Type::StructDescriptor(s) => {
                        match crate::compile::compiler::Compiler::try_parse_special_field_access(
                            property,
                        ) {
                            Ok(k) => {
                                let v = if let Some(prop_idx) = k {
                                    s.get_field_idx(prop_idx as usize).ok_or_else(|| {
                                        SomewhereTypeError::OperationUnsupported {
                                            target,
                                            message: format!(
                                                "index too big: no field with index {}",
                                                prop_idx,
                                            ),
                                        }
                                        .at(property.position)
                                    })?
                                } else {
                                    s.get_field(property.get_string().unwrap()).ok_or_else(
                                        || {
                                            SomewhereTypeError::AttributeError {
                                                target_type: target,
                                                field: property.get_string().unwrap().to_string(),
                                            }
                                            .at(property.position)
                                        },
                                    )?
                                };

                                Checker::check_expectation(&set, &v)
                                    .map_err(|e| e.at(property.position))?;

                                Ok(())
                            }
                            Err(e) => Err(SomewhereTypeError::OperationUnsupported {
                                target,
                                message: e,
                            }
                            .at(property.position))?,
                        }
                    }
                    Type::Unspecified => Ok(()),
                    _ => unreachable!(),
                }
            }
            Type::Union(_) => {
                Ok(())
                //TODO : union ops
            }
            Type::Unspecified => Ok(()),
            _ => Err(SomewhereTypeError::AttributeError {
                target_type: target,
                field: property.get_string().unwrap().to_string(),
            }
            .at(property.position))?,
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Type::Unspecified
    }
}
///partialcmp that is only used through le and threated like type match
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

        match (self, other) {
            (Type::Callable(_), Type::Callable(_)) => {
                if Self::check_fn_less(self, other) || Self::check_fn_less(other, self) {
                    Some(Ordering::Equal)
                } else {
                    None
                }
            }
            _ => None,
        }
    }
}
