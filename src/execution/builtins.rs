///
/// contract: all builtin functions may change vm state, but they should never touch VM's buitin_map as it may be aliased
use crate::data::objects::{StackObject, VVec, Value};
use std::collections::HashMap;

use super::vm::VM;

type BuiltinFuction = fn(Vec<Value>, &mut VM) -> BuiltinResult;

pub struct BuiltinMap(HashMap<Box<str>, BuiltinFuction>);

type BuiltinResult = std::result::Result<Value, String>;

impl BuiltinMap {
    pub(self) fn new() -> Self {
        BuiltinMap(Default::default())
    }

    pub(self) fn add_builtin(&mut self, name: &'static str, f: BuiltinFuction) {
        self.0.insert(name.to_owned().into_boxed_str(), f);
    }

    pub fn apply_builtin(&self, name: &str, args: VVec, vm: &mut VM) -> BuiltinResult {
        match self.0.get(name) {
            Some(builtin) => builtin(args, vm),

            None => Err(format!("could not find builtin with name {}", name)),
        }
    }

    pub fn get_builtin(&self, name: &str) -> Option<Value> {
        if self.0.get(name).is_some() {
            Some(Value::Builtin(name.to_owned().into_boxed_str()))
        } else {
            None
        }
    }
}

pub fn builtin_factory() -> BuiltinMap {
    let mut map: BuiltinMap = BuiltinMap::new();

    macro_rules! require_arity {
        ($checked:expr, $n:expr) => {
            if $checked.len() != $n {
                return Err(format!(
                    "arity mismatch: expected {} but got {}",
                    $n,
                    $checked.len()
                ));
            }
        };
    }

    macro_rules! builtin {
        ($name:expr, $combinator: expr) => {
            map.add_builtin($name, $combinator)
        };
    }

    builtin!("sum", |args, _vm| {
        match args
            .iter()
            .enumerate()
            .find(|(_idx, v)| v.unwrap_int().is_none())
        {
            Some((idx, obj)) => {
                return Err(format!(
                    "expected all args of type int, got {} arg of {}",
                    idx,
                    obj.type_string()
                ));
            }
            None => {}
        };

        Ok(Value::Int(
            args.into_iter().map(|v| v.unwrap_int().unwrap()).sum(),
        ))
    });

    builtin!("int", |args, _vm| {
        require_arity!(args, 1);
        if args[0].unwrap_any_str().is_none() {
            return Err("expected string-like in int".to_string());
        }
        Ok(StackObject::Int(
            args[0]
                .unwrap_any_str()
                .unwrap()
                .parse::<i64>()
                .map_err(|_e| format!("failed to parse {}", args[0]))?,
        ))
    });

    #[cfg(test)]
    builtin!("set_stack_limit", |args, vm| {
        vm.override_stack_limit(args[0].unwrap_int().unwrap() as usize);
        Ok(Value::Int(0))
    });

    map
}
