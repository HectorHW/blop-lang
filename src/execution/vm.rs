use crate::data::gc::GC;
use crate::data::objects::{Closure, StackObject, Value, ValueBox};
use crate::execution::builtins::{apply_builtin, get_builtin};
use crate::execution::chunk::{Chunk, Opcode};
use std::cmp::Ordering;
use std::collections::HashMap;

const DEFAULT_MAX_STACK_SIZE: usize = 4 * 1024 * 1024 / std::mem::size_of::<StackObject>();
//4MB

pub struct VM {
    pub(super) stack: Vec<Value>,
    pub(super) call_stack: Vec<CallStackValue>,
    pub(super) globals: HashMap<String, Value>,
    locals_offset: usize,
    stack_max_size: usize,
    pub gc: GC,
}

pub struct CallStackValue {
    return_chunk: usize,
    return_ip: usize,
    return_locals_offset: usize,
    return_stack_size: usize,
}

type Result<T> = std::result::Result<T, InterpretError>;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct InterpretError {
    pub opcode_index: usize,
    pub chunk_index: usize,
    pub kind: InterpretErrorKind,
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
pub enum InterpretErrorKind {
    StackUnderflow,
    ZeroDivision,
    OperandIndexing,
    JumpBounds,
    AssertionFailure,
    StackOverflow,
    TypeError { message: String },
    MissedReturn,
    NameError { name: String },
    NativeError { message: String },
}

impl VM {
    pub fn new() -> VM {
        VM {
            stack: Vec::new(),
            call_stack: Vec::new(),
            globals: HashMap::new(),
            locals_offset: 0,
            gc: GC::default_gc(),
            stack_max_size: DEFAULT_MAX_STACK_SIZE,
        }
    }

    pub fn override_stack_limit(&mut self, new_limit: usize) -> usize {
        let old_stack_size = self.stack_max_size;
        self.stack_max_size = new_limit;
        old_stack_size
    }

    pub fn run(&mut self, program: &[Chunk]) -> Result<()> {
        use InterpretErrorKind::*;
        let mut ip = 0;
        let mut current_chunk_id = 0;
        let mut current_chunk = program.get(current_chunk_id).unwrap();

        macro_rules! checked_stack_pop {
            () => {{
                self.stack.pop().ok_or(InterpretError {
                    opcode_index: ip,
                    chunk_index: current_chunk_id,
                    kind: StackUnderflow,
                })
            }};
        }

        macro_rules! runtime_error {
            ($e:expr) => {
                InterpretError {
                    opcode_index: ip,
                    chunk_index: current_chunk_id,
                    kind: $e,
                }
            };
        }

        macro_rules! as_int {
            ($value:expr) => {
                Ok($value).and_then(|u| {
                    u.clone().unwrap_int().ok_or_else(|| {
                        runtime_error!(TypeError {
                            message: format!(
                                "expected {} but got {}",
                                Value::Int(0).type_string(),
                                u.type_string()
                            )
                        })
                    })
                })
            };
        }

        macro_rules! as_closure {
            ($value:expr) => {
                Ok($value).and_then(|closure| match &closure {
                    Value::Closure(_gc_ptr, closure_ptr) => Ok(closure_ptr.unwrap_ref()),
                    other => Err(runtime_error!(TypeError {
                        message: format!("expected {} but got {}", "function", other.type_string())
                    })),
                })
            };
        }

        macro_rules! comparison_operator {
            ($pat:pat) => {{
                let second_operand = checked_stack_pop!()?;
                let first_operand = checked_stack_pop!()?;
                let value = match first_operand.partial_cmp(&second_operand) {
                    $pat => Ok(1),
                    Some(_) => Ok(0),
                    None => Err(runtime_error!(InterpretErrorKind::TypeError {
                        message: format!(
                            "got unsupported argument types ({} and {})",
                            first_operand.type_string(),
                            second_operand.type_string()
                        )
                    })),
                }?;
                self.stack.push(Value::Int(value));
                ip += 1;
            }};
        }

        while ip < current_chunk.code.len() {
            #[cfg(feature = "print-execution")]
            print!("{} => ", current_chunk.code[ip]);
            match current_chunk.code[ip] {
                Opcode::Print => {
                    let result = checked_stack_pop!()?;
                    println!("{}", result);
                    ip += 1;
                }
                Opcode::LoadConst(idx) => {
                    let idx = idx as usize;
                    let value = current_chunk
                        .constants
                        .get(idx)
                        .cloned()
                        .ok_or(runtime_error!(OperandIndexing))?;
                    self.stack.push(value);
                    ip += 1;
                }
                Opcode::LoadImmediateInt(i) => {
                    self.stack.push(Value::Int(i as i64));
                    ip += 1;
                }

                Opcode::Add => {
                    //let second_operand = checked_stack_pop!()
                    let second_operand = checked_stack_pop!()?;
                    let first_operand = checked_stack_pop!()?;

                    match (&first_operand, &second_operand) {
                        (s1, s2)
                            if s1.unwrap_any_str().is_some() && s2.unwrap_any_str().is_some() =>
                        {
                            let resulting_string = self
                                .gc
                                .try_inplace_string_concat(first_operand, second_operand)
                                .map_err(|o| {
                                    runtime_error!(InterpretErrorKind::TypeError { message: o })
                                })?;
                            self.stack.push(resulting_string);
                        }

                        (StackObject::Int(n1), StackObject::Int(n2)) => {
                            self.stack.push(Value::Int(*n1 + *n2));
                        }
                        (other_1, other_2) => {
                            return Err(runtime_error!(InterpretErrorKind::TypeError {
                                message: format!(
                                    "uncompatible types in Add (got {} and {})",
                                    other_1.type_string(),
                                    other_2.type_string()
                                )
                            }));
                        }
                    }

                    ip += 1;
                }

                Opcode::Sub => {
                    let second_operand = as_int!(checked_stack_pop!()?)?;
                    let first_operand = as_int!(checked_stack_pop!()?)?;
                    self.stack.push(Value::Int(first_operand - second_operand));
                    ip += 1;
                }

                Opcode::Mul => {
                    let second_operand = as_int!(checked_stack_pop!()?)?;
                    let first_operand = as_int!(checked_stack_pop!()?)?;
                    self.stack.push(Value::Int(first_operand * second_operand));
                    ip += 1;
                }

                Opcode::Div => {
                    let second_operand = as_int!(checked_stack_pop!()?)?;
                    let first_operand = as_int!(checked_stack_pop!()?)?;

                    let value = first_operand
                        .checked_div(second_operand)
                        .ok_or(runtime_error!(ZeroDivision))?;
                    self.stack.push(Value::Int(value));
                    ip += 1;
                }

                Opcode::Mod => {
                    let second_operand = as_int!(checked_stack_pop!()?)?;
                    let first_operand = as_int!(checked_stack_pop!()?)?;

                    let value = first_operand
                        .checked_rem(second_operand)
                        .ok_or(runtime_error!(ZeroDivision))?;
                    self.stack.push(Value::Int(value));
                    ip += 1;
                }

                Opcode::Power => {
                    let second_operand = as_int!(checked_stack_pop!()?)?;
                    let first_operand = as_int!(checked_stack_pop!()?)?;
                    let value = first_operand.pow(second_operand as u64 as u32);
                    self.stack.push(Value::Int(value));
                    ip += 1;
                }

                Opcode::LoadGlobal(idx) => {
                    let key = current_chunk
                        .global_names
                        .get(idx as usize)
                        .ok_or(runtime_error!(OperandIndexing))?;
                    let value = self
                        .globals
                        .get(key)
                        .cloned()
                        .or_else(|| get_builtin(key))
                        .ok_or(runtime_error!(InterpretErrorKind::NameError {
                            name: key.clone()
                        }))?;

                    self.stack.push(value);
                    ip += 1;
                }

                Opcode::LoadLocal(idx) => {
                    let absolute_pos = self.locals_offset + idx as usize;
                    let value = self
                        .stack
                        .get(absolute_pos)
                        .cloned()
                        .ok_or(runtime_error!(OperandIndexing))?;
                    self.stack.push(value);
                    ip += 1;
                }

                Opcode::StoreLocal(idx) => {
                    let value = self.stack.pop().ok_or(runtime_error!(StackUnderflow))?;

                    let absolute_pos = self.locals_offset + idx as usize;

                    let addr = self
                        .stack
                        .get_mut(absolute_pos)
                        .ok_or(runtime_error!(OperandIndexing))?;
                    *addr = value;
                    ip += 1;
                }
                Opcode::TestEquals => {
                    let second_operand = checked_stack_pop!()?;
                    let first_operand = checked_stack_pop!()?;
                    let value = if second_operand == first_operand {
                        1
                    } else {
                        0
                    };
                    self.stack.push(Value::Int(value));
                    ip += 1;
                }

                Opcode::TestNotEquals => {
                    let second_operand = checked_stack_pop!()?;
                    let first_operand = checked_stack_pop!()?;
                    let value = if second_operand != first_operand {
                        1
                    } else {
                        0
                    };
                    self.stack.push(Value::Int(value));
                    ip += 1;
                }

                Opcode::TestGreater => {
                    comparison_operator!(Some(Ordering::Greater))
                }

                Opcode::TestGreaterEqual => {
                    comparison_operator!(Some(Ordering::Equal | Ordering::Greater))
                }

                Opcode::TestLess => comparison_operator!(Some(Ordering::Less)),

                Opcode::TestLessEqual => {
                    comparison_operator!(Some(Ordering::Equal | Ordering::Less))
                }

                Opcode::JumpIfFalse(delta) => {
                    let value_to_test = as_int!(checked_stack_pop!()?)?;
                    if value_to_test == 0 {
                        let new_ip = ip + delta as usize;
                        if new_ip >= current_chunk.code.len() {
                            return Err(runtime_error!(JumpBounds));
                        }
                        ip = new_ip;
                    } else {
                        ip += 1;
                    }
                }

                Opcode::JumpIfTrue(delta) => {
                    let value_to_test = as_int!(checked_stack_pop!()?)?;
                    if value_to_test == 1 {
                        let new_ip = ip + delta as usize;
                        if new_ip >= current_chunk.code.len() {
                            return Err(runtime_error!(JumpBounds));
                        }
                        ip = new_ip;
                    } else {
                        ip += 1;
                    }
                    self.stack.push(StackObject::Int(value_to_test));
                }

                Opcode::JumpRelative(delta) => {
                    let new_ip = ip + delta as usize;
                    if new_ip >= current_chunk.code.len() {
                        return Err(runtime_error!(JumpBounds));
                    }
                    ip = new_ip;
                }

                Opcode::JumpAbsolute(idx) => {
                    let new_ip = idx as usize;
                    if new_ip >= current_chunk.code.len() {
                        return Err(runtime_error!(JumpBounds));
                    }
                    ip = new_ip;
                }

                Opcode::Pop(n) => {
                    if self.stack.len() < n as usize {
                        return Err(runtime_error!(StackUnderflow));
                    }
                    let new_stack_size = self.stack.len() - n as usize;
                    self.stack.truncate(new_stack_size);
                    ip += 1;
                }
                Opcode::Nop => {
                    ip += 1;
                }
                Opcode::Assert => {
                    let value = as_int!(checked_stack_pop!()?)?;
                    if value == 0 {
                        return Err(runtime_error!(AssertionFailure));
                    }
                    ip += 1;
                }
                Opcode::Call(arity) => {
                    //check stack
                    let arity = arity as usize;
                    if self.stack.len() < arity + 1 {
                        return Err(runtime_error!(StackUnderflow));
                    }

                    let object = self
                        .stack
                        .get(self.stack.len() - 1 - arity)
                        .cloned()
                        .unwrap();

                    if let Value::Builtin(name) = object {
                        let final_length = self.stack.len().saturating_sub(arity);
                        let args = self.stack.split_off(final_length);
                        self.stack.pop(); //remove builtin
                        let result = apply_builtin(name, &args);
                        let result = result.map_err(|e| {
                            runtime_error!(InterpretErrorKind::NativeError { message: e })
                        })?;
                        self.stack.push(result);
                        ip += 1;
                    } else {
                        let chunk_id = match object {
                            Value::Function { chunk_id } => Ok(chunk_id),
                            Value::Closure(_gc, ptr) => Ok(ptr.unwrap_ref().chunk_id),
                            _ => Err(runtime_error!(TypeError {
                                message: format!(
                                    "expected function but got {}",
                                    object.type_string()
                                )
                            })),
                        }?;

                        self.call_stack.push(CallStackValue {
                            return_chunk: current_chunk_id,
                            return_ip: ip + 1,
                            return_locals_offset: self.locals_offset,
                            return_stack_size: self.stack.len() - 1 - arity,
                        });

                        self.locals_offset = self.stack.len() - 1 - arity;

                        current_chunk_id = chunk_id;
                        ip = 0;
                        current_chunk = program.get(current_chunk_id).unwrap();

                        if arity as usize != current_chunk.arity {
                            return Err(runtime_error!(TypeError {
                                message: format!(
                                    "mismatched arguments: expected {} but got {}",
                                    current_chunk.arity, arity
                                )
                            }));
                        }
                    }
                }

                Opcode::Return => {
                    if self.call_stack.is_empty() {
                        return Ok(());
                    }
                    let return_info = self.call_stack.pop().unwrap();

                    ip = return_info.return_ip;
                    current_chunk_id = return_info.return_chunk;
                    current_chunk = program.get(current_chunk_id).unwrap();
                    self.locals_offset = return_info.return_locals_offset;

                    let ret_value = checked_stack_pop!()?;
                    if self.stack.len() < return_info.return_stack_size {
                        return Err(runtime_error!(StackUnderflow));
                    }

                    let new_stack_size = return_info.return_stack_size;
                    self.stack.truncate(new_stack_size);
                    self.stack.push(ret_value);
                }
                Opcode::NewBox => {
                    let box_ref = self.gc.allocate_new::<ValueBox>();
                    self.stack.push(box_ref);
                    ip += 1;
                }
                Opcode::LoadBox => {
                    match checked_stack_pop!()? {
                        Value::Box(_gc, _obj) => {
                            let box_obj = _obj.unwrap_ref_mut();
                            self.stack.push(box_obj.0.clone());
                        }
                        _any_other => {
                            return Err(runtime_error!(TypeError {
                                message: format!(
                                    "expected {} but got {}",
                                    "box",
                                    _any_other.type_string()
                                ),
                            }))
                        }
                    };
                    ip += 1;
                }

                Opcode::StoreBox => {
                    let value = checked_stack_pop!()?;
                    let addr = checked_stack_pop!()?;

                    match addr {
                        Value::Box(_gc, _obj) => {
                            let box_obj = _obj.unwrap_ref_mut();
                            box_obj.0 = value;
                        }
                        _any_other => {
                            return Err(runtime_error!(TypeError {
                                message: format!(
                                    "expected {} but got {}",
                                    "box",
                                    _any_other.type_string()
                                ),
                            }))
                        }
                    };
                    ip += 1;
                }

                Opcode::NewClosure => {
                    let value = checked_stack_pop!()?;
                    let chunk_id = match value {
                        Value::Function { chunk_id } => chunk_id,
                        other => {
                            return Err(runtime_error!(TypeError {
                                message: format!(
                                    "expected {} but got {}",
                                    "function",
                                    other.type_string()
                                )
                            }))
                        }
                    };

                    let ptr = self.gc.store(Closure {
                        closed_values: vec![],
                        chunk_id,
                    });
                    self.stack.push(ptr);
                    ip += 1;
                }

                Opcode::AddClosedValue => {
                    let value = checked_stack_pop!()?;
                    let closure = checked_stack_pop!()?;

                    match &closure {
                        Value::Closure(_gc_ptr, closure_ptr) => {
                            closure_ptr.unwrap_ref_mut().closed_values.push(value);
                        }
                        other => {
                            return Err(runtime_error!(TypeError {
                                message: format!(
                                    "expected {} but got {}",
                                    "function",
                                    other.type_string()
                                )
                            }))
                        }
                    }
                    self.stack.push(closure);
                    ip += 1;
                }
                Opcode::LoadClosureValue(idx) => {
                    let closure = as_closure!(self.stack.get(self.locals_offset).unwrap())?;
                    let value = closure
                        .closed_values
                        .get(idx as usize)
                        .cloned()
                        .ok_or(runtime_error!(OperandIndexing))?;
                    self.stack.push(value);
                    ip += 1;
                }
                Opcode::Duplicate => {
                    let value = checked_stack_pop!()?;
                    self.stack.push(value.clone());
                    self.stack.push(value);
                    ip += 1;
                }
            }
            #[cfg(feature = "print-execution")]
            {
                print!("[");
                for item in &self.stack[..self.locals_offset] {
                    print!("{} ", item);
                }
                print!("| ");
                for item in &self.stack[self.locals_offset..] {
                    print!("{} ", item);
                }
                println!("]");
            }

            if self.stack.len() > self.stack_max_size || self.call_stack.len() > self.stack_max_size
            {
                return Err(runtime_error!(StackOverflow));
                //TODO include last stack frame?
            }

            unsafe {
                self.gc.mark_and_sweep(self.stack.iter(), program);
            }
        }

        if ip == current_chunk.code.len() {
            return Err(InterpretError {
                opcode_index: ip - 1,
                chunk_index: current_chunk_id,
                kind: InterpretErrorKind::MissedReturn,
            });
        }

        Ok(())
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}
