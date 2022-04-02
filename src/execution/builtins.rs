///
/// contract: all builtin functions may change vm state, but they should never touch VM's buitin_map as it may be aliased
use crate::data::objects::{StackObject, VVec, Value};
use indexmap::IndexMap;

use super::vm::VM;

type BuiltinFuction = fn(Vec<Value>, &mut VM) -> BuiltinResult;

type BuiltinMethod = fn(Value, Vec<Value>, &mut VM) -> BuiltinResult;

use crate::data::objects::BuiltinMethod as MethodInstance;

#[derive(Default)]
pub struct BuiltinMap {
    functions: IndexMap<String, BuiltinFuction>,
    methods: IndexMap<String, IndexMap<String, BuiltinMethod>>,
}

type BuiltinResult = std::result::Result<Value, String>;

impl BuiltinMap {
    pub(self) fn new() -> Self {
        Default::default()
    }

    pub(self) fn add_builtin(&mut self, name: &'static str, f: BuiltinFuction) {
        self.functions.insert(name.to_owned(), f);
    }

    pub(self) fn add_method(&mut self, classname: &str, method_name: &str, f: BuiltinMethod) {
        self.methods
            .entry(classname.to_string())
            .or_default()
            .insert(method_name.to_string(), f);
    }

    pub fn apply_builtin(&self, idx: usize, args: VVec, vm: &mut VM) -> BuiltinResult {
        match self.functions.get_index(idx) {
            Some((_, builtin)) => builtin(args, vm),

            None => Err(format!("could not find builtin with index {}", idx)),
        }
    }

    pub fn get_method(
        &self,
        class_idx: usize,
        method_idx: usize,
    ) -> Option<(&'_ str, &'_ str, &'_ BuiltinMethod)> {
        self.methods
            .get_index(class_idx)
            .and_then(|(class_name, method_map)| {
                method_map
                    .get_index(method_idx)
                    .map(|(method_name, method)| {
                        (class_name.as_str(), method_name.as_str(), method)
                    })
            })
    }

    pub fn apply_method(
        &self,
        class_idx: usize,
        method_idx: usize,
        self_ref: Value,
        args: VVec,
        vm: &mut VM,
    ) -> BuiltinResult {
        match self.get_method(class_idx, method_idx) {
            Some((_, _, method)) => method(self_ref, args, vm),

            None => Err(format!(
                "failed to find method with class idx {} and method idx {}",
                class_idx, method_idx
            )),
        }
    }

    pub fn get_builtin(&self, name: &str) -> Option<Value> {
        self.functions
            .get_full(name)
            .map(|(idx, _, _)| Value::Builtin(idx))
    }

    pub fn bind_method(&self, object: Value, method_name: &str, context: &mut VM) -> Option<Value> {
        self.methods
            .get_full(object.type_string())
            .and_then(|(class_idx, _, class_methods)| {
                class_methods
                    .get_full(method_name)
                    .map(|(method_idx, _, _)| {
                        context.gc.store(MethodInstance {
                            self_object: object,
                            class_id: class_idx,
                            method_id: method_idx,
                        })
                    })
            })
    }

    pub fn get_builtin_name(&self, idx: usize) -> Option<&str> {
        self.functions.get_index(idx).map(|(k, _v)| k.as_str())
    }

    pub fn get_method_name(&self, class_idx: usize, method_idx: usize) -> Option<String> {
        self.get_method(class_idx, method_idx)
            .map(|(class_name, method_name, _)| format!("{}.{}", class_name, method_name))
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
        ($name:expr, $function: expr) => {
            map.add_builtin($name, $function)
        };
    }

    macro_rules! methods {
        ($classname:expr, $($method_name:expr => $function: expr);* $(;)? ) => {
            {
                $(
                    map.add_method($classname, $method_name, $function);
                )*
            }
        }
    }

    builtin!("sum", |args, _vm| {
        if let Some((idx, obj)) = args
            .iter()
            .enumerate()
            .find(|(_idx, v)| v.unwrap_int().is_none())
        {
            return Err(format!(
                "expected all args of type int, got {} arg of {}",
                idx,
                obj.type_string()
            ));
        }

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

    methods!("Int",
        "abs" => |obj, args, _context| {
            require_arity!(args, 0);
            Ok(Value::Int(obj.unwrap_int().unwrap().abs()))
        };
        "_mod" => |obj, args, _context| {
            require_arity!(args, 1);
            match args[0] {
                Value::Int(b) => {
                    Ok(Value::Int(obj.unwrap_int().unwrap() % b ))
                }
                _ => Err("expected int".to_string())
            }
        }

    );

    map
}
