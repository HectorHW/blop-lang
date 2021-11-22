use crate::data::objects::{StackObject, Value};

pub fn apply_builtin(builtin_name: &str, args: &[Value]) -> Result<Value, String> {
    macro_rules! require_arity {
        ($n:expr) => {
            if args.len() != $n {
                return Err(format!(
                    "arity mismatch: expected {} but got {}",
                    $n,
                    args.len()
                ));
            }
        };
    }
    match builtin_name {
        "int" => {
            require_arity!(1);
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
        }

        _any_other => return Err(format!("unknown builtin {}", _any_other)),
    }
}

pub fn get_builtin(builtin_name: &str) -> Option<StackObject> {
    match builtin_name {
        "int" => Some(StackObject::Builtin("int")),
        _ => None,
    }
}
