use std::fmt::Display;

///
/// contract: all builtin functions may change vm state, but they should never touch VM's buitin_map as it may be aliased
use crate::data::objects::{StackObject, VVec, Value};
use indexmap::IndexMap;

use super::{arity::Arity, vm::VM};

type BuiltinFuction = fn(Vec<Value>, &mut VM) -> Result;

type BuiltinMethod = fn(Value, Vec<Value>, &mut VM) -> Result;

#[derive(Default)]
pub struct BuiltinMap {
    functions: IndexMap<String, (Arity, BuiltinFuction)>,
    methods: IndexMap<String, IndexMap<String, (Arity, BuiltinMethod)>>,
}

pub enum BuiltinError {
    ArityMismatch { provided: usize, expected: Arity },
    Other(String),
}

impl Display for BuiltinError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BuiltinError::ArityMismatch { provided, expected } =>
                    format!("expected {} args but got {}", expected, provided),
                BuiltinError::Other(e) => e.clone(),
            }
        )
    }
}

impl From<String> for BuiltinError {
    fn from(s: String) -> Self {
        BuiltinError::Other(s)
    }
}

type Result = std::result::Result<Value, BuiltinError>;

impl BuiltinMap {
    pub(self) fn new() -> Self {
        Default::default()
    }

    pub(self) fn add_builtin(&mut self, name: &'static str, arity: Arity, f: BuiltinFuction) {
        self.functions.insert(name.to_owned(), (arity, f));
    }

    pub(self) fn add_method(
        &mut self,
        classname: &str,
        method_name: &str,
        arity: Arity,
        f: BuiltinMethod,
    ) {
        self.methods
            .entry(classname.to_string())
            .or_default()
            .insert(method_name.to_string(), (arity, f));
    }

    pub fn apply_builtin(&self, idx: usize, args: VVec, vm: &mut VM) -> Result {
        match self.functions.get_index(idx) {
            Some((_, (arity, builtin))) => {
                check_arity(*arity, args.len())?;
                builtin(args, vm)
            }

            None => Err(format!("could not find builtin with index {}", idx).into()),
        }
    }

    pub fn get_method_full(
        &self,
        class_idx: u32,
        method_idx: u32,
    ) -> Option<(&'_ str, &'_ str, Arity, &'_ BuiltinMethod)> {
        self.methods
            .get_index(class_idx as usize)
            .and_then(|(class_name, method_map)| {
                method_map
                    .get_index(method_idx as usize)
                    .map(|(method_name, method)| {
                        (
                            class_name.as_str(),
                            method_name.as_str(),
                            method.0,
                            &method.1,
                        )
                    })
            })
    }

    pub fn apply_method(
        &self,
        class_idx: u32,
        method_idx: u32,
        self_ref: Value,
        args: VVec,
        vm: &mut VM,
    ) -> Result {
        match self.get_method_full(class_idx, method_idx) {
            Some((_, _, arity, method)) => {
                check_arity(arity - 1, args.len())?;
                method(self_ref, args, vm)
            }

            None => Err(format!(
                "failed to find method with class idx {} and method idx {}",
                class_idx, method_idx
            )
            .into()),
        }
    }

    pub fn get_builtin(&self, name: &str) -> Option<Value> {
        self.functions
            .get_full(name)
            .map(|(idx, _, _)| Value::Builtin(idx))
    }

    pub fn get_method(&self, class_name: &str, method_name: &str) -> Option<Value> {
        self.methods
            .get_full(class_name)
            .and_then(|(class_idx, _, class_methods)| {
                class_methods
                    .get_full(method_name)
                    .map(|(method_idx, _, _)| Value::BuiltinMethod {
                        class_idx: class_idx as u32,
                        method_idx: method_idx as u32,
                    })
            })
    }

    pub fn get_builtin_name(&self, idx: usize) -> Option<&str> {
        self.functions.get_index(idx).map(|(k, _v)| k.as_str())
    }

    pub fn get_method_name(&self, class_idx: u32, method_idx: u32) -> Option<String> {
        self.get_method_full(class_idx, method_idx)
            .map(|(class_name, method_name, ..)| format!("{}.{}", class_name, method_name))
    }

    pub fn get_builtin_arity(&self, idx: usize) -> Option<Arity> {
        self.functions.get_index(idx).map(|(_k, v)| v.0)
    }

    pub fn get_method_arity(&self, class_idx: u32, method_idx: u32) -> Option<Arity> {
        self.get_method_full(class_idx, method_idx)
            .map(|(_, _, arity, _)| arity)
    }
}

fn check_arity(arity: Arity, provided: usize) -> std::result::Result<(), BuiltinError> {
    if !arity.accepts(provided) {
        Err(BuiltinError::ArityMismatch {
            provided,
            expected: arity,
        })
    } else {
        Ok(())
    }
}

pub fn builtin_factory() -> BuiltinMap {
    use Arity::*;

    let mut map: BuiltinMap = BuiltinMap::new();

    macro_rules! builtin {
        ($name:expr, $arity:expr, $function: expr) => {
            map.add_builtin($name, $arity, $function)
        };
    }

    macro_rules! methods {
        ($classname:expr, $($method_name:expr => $arity:expr => $function: expr);* $(;)? ) => {
            {
                $(
                    map.add_method($classname, $method_name, $arity + 1, $function);
                )*
            }
        }
    }

    builtin!("sum", AtLeast(0), |args, _vm| {
        if let Some((idx, obj)) = args[0]
            .unwrap_vector()
            .unwrap()
            .iter()
            .enumerate()
            .find(|(_idx, v)| v.unwrap_int().is_none())
        {
            return Err(format!(
                "expected all args of type int, got {} arg of {}",
                idx,
                obj.type_string()
            )
            .into());
        }

        Ok(Value::Int(
            args[0]
                .unwrap_vector()
                .unwrap()
                .iter()
                .map(|v| v.unwrap_int().unwrap())
                .sum(),
        ))
    });

    builtin!("int", Exact(1), |args, _vm| {
        if args[0].unwrap_any_str().is_none() {
            return Err("expected string-like in int".to_string().into());
        }
        Ok(StackObject::Int(
            args[0]
                .unwrap_any_str()
                .unwrap()
                .parse::<i64>()
                .map_err(|_e| BuiltinError::Other(format!("failed to parse {}", args[0])))?,
        ))
    });

    builtin!("list", AtLeast(0), |mut args, _vm| {
        Ok(args.pop().unwrap())
    });

    builtin!("arity", Exact(1), |args, vm| {
        args[0]
            .get_arity(vm)
            .map(|arity| StackObject::Int(usize::from(arity) as i64))
            .ok_or_else(|| {
                BuiltinError::Other(format!("expected callable, got {}", args[0].type_string()))
            })
    });

    #[cfg(test)]
    builtin!("set_stack_limit", Exact(1), |args, vm| {
        vm.override_stack_limit(args[0].unwrap_int().unwrap() as usize);
        Ok(Value::Int(0))
    });

    builtin!("is_vararg", Exact(1), |args, vm| {
        let v = args
            .get(0)
            .unwrap()
            .get_arity(vm)
            .unwrap_or(Arity::Exact(0))
            .is_vararg();

        Ok(Value::Int(if v { 1 } else { 0 }))
    });

    builtin!("print", AtLeast(0), |args, vm| {
        let values: &mut VVec = args[0].unwrap_vector().unwrap();

        let s = values
            .iter()
            .map(|value| crate::data::objects::pretty_format(value, vm))
            .collect::<Vec<String>>()
            .join(" ");

        println!("{}", s);

        Ok(Value::Int(0))
    });

    methods!("Int",
        "abs" => Exact(0) => |obj, _args, _context| {
            Ok(Value::Int(obj.unwrap_int().unwrap().abs()))
        };
        "_mod" => Exact(1) => |obj, args, _context| {
            match args[0] {
                Value::Int(b) => {
                    Ok(Value::Int(obj.unwrap_int().unwrap() % b ))
                }
                _ => Err("expected int".to_string().into())
            }
        }

    );

    map
}
