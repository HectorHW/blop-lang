use crate::data::gc::GC;
use crate::data::objects::{Closure, StackObject, VVec, Value, ValueBox};
use crate::data::value_ops::{self, cast_binary, numeric_cast, NumberCastResult};
use crate::execution::chunk::Opcode;
use std::cmp::Ordering;
use std::collections::HashMap;

use super::arity::Arity;
use super::builtins::BuiltinMap;
use super::module::Module;

const DEFAULT_MAX_STACK_SIZE: usize = 4 * 1024 * 1024 / std::mem::size_of::<StackObject>();
//4MB

pub struct VM<'gc, 'builtins> {
    pub(super) stack: Vec<Value>,
    pub(super) call_stack: Vec<CallStackValue>,
    pub(super) loaded_modules: HashMap<Module, HashMap<String, Value>>,
    locals_offset: usize,
    stack_max_size: usize,
    pub gc: &'gc mut GC,
    pub(crate) builtins: &'builtins BuiltinMap,
}

pub struct CallStackValue {
    pub return_chunk: StackObject, //actually HeapObject -> Function
    return_ip: usize,
    return_locals_offset: usize,
    return_stack_size: usize,
}

type Result<T> = std::result::Result<T, InterpretError>;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct InterpretError {
    pub opcode_index: usize,
    pub chunk: StackObject,
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
    AttributeError { object: Value, missed_field: String },
    IndexAttributeError { object: Value, missed_idx: usize },
}

enum InstructionExecution {
    NextInstruction,
    LocalJump(usize),
    EnterChunk(StackObject),
    CrossChunkJump {
        new_chunk_id: StackObject,
        new_ip: usize,
    },
    Termination,
}

impl<'gc, 'builtins> VM<'gc, 'builtins> {
    pub fn new(gc: &'gc mut GC, builtins: &'builtins BuiltinMap) -> VM<'gc, 'builtins> {
        VM {
            stack: Vec::new(),
            call_stack: Vec::new(),
            loaded_modules: Default::default(),
            locals_offset: 0,
            gc,
            stack_max_size: DEFAULT_MAX_STACK_SIZE,
            builtins,
        }
    }

    #[cfg(test)]
    pub fn override_stack_limit(&mut self, new_limit: usize) -> usize {
        let old_stack_size = self.stack_max_size;
        self.stack_max_size = new_limit;
        old_stack_size
    }

    pub fn reset_stacks(&mut self) {
        self.call_stack.clear();
        self.stack.clear();
        self.locals_offset = 0;
    }

    pub fn maybe_create_module(&mut self, module: &Module) {
        self.loaded_modules.entry(module.clone()).or_default();
    }

    pub fn run(&mut self, entry_point: StackObject) -> Result<StackObject> {
        use InterpretErrorKind::*;
        self.reset_stacks();
        let mut ip = 0;
        let mut current_chunk = entry_point;

        self.call_stack.push(CallStackValue {
            return_chunk: current_chunk.clone(),
            return_ip: 0,
            return_locals_offset: 0,
            return_stack_size: usize::MAX,
        });

        macro_rules! runtime_error {
            ($e:expr) => {
                InterpretError {
                    opcode_index: ip,
                    chunk: current_chunk,
                    kind: $e,
                }
            };
        }

        macro_rules! checked_stack_pop {
            () => {{
                self.stack.pop().ok_or(InterpretError {
                    opcode_index: ip,
                    chunk: current_chunk.clone(),
                    kind: StackUnderflow,
                })
            }};
        }

        while ip < current_chunk.unwrap_function().unwrap().code.len() {
            #[cfg(feature = "print-execution")]
            print!("{} => ", current_chunk.unwrap_function().unwrap().code[ip]);

            let jump = self.execute_instruction(ip, &current_chunk)?;

            #[cfg(feature = "print-execution")]
            {
                use crate::data::objects::pretty_format;
                print!("[");
                for item in &self.stack[..self.locals_offset] {
                    print!("{} ", pretty_format(item, self));
                }
                print!("| ");
                for item in &self.stack[self.locals_offset..] {
                    print!("{} ", pretty_format(item, self));
                }
                println!("]");
            }

            match jump {
                InstructionExecution::NextInstruction => {
                    ip += 1;
                }

                InstructionExecution::LocalJump(idx) => {
                    ip = idx;
                }

                InstructionExecution::EnterChunk(chunk) => {
                    ip = 0;
                    current_chunk = chunk;
                }

                InstructionExecution::CrossChunkJump {
                    new_chunk_id,
                    new_ip,
                } => {
                    ip = new_ip;
                    current_chunk = new_chunk_id;
                }

                InstructionExecution::Termination => {
                    let value = checked_stack_pop!()?;
                    //return immediately, without possibly triggering gc
                    return Ok(value);
                }
            }

            if self.stack.len() > self.stack_max_size || self.call_stack.len() > self.stack_max_size
            {
                return Err(runtime_error!(StackOverflow));
                //TODO include last stack frame?
            }
            if self.gc.needs_collection() {
                unsafe {
                    self.gc.mark_and_sweep(
                        self.stack
                            .iter()
                            .chain(self.loaded_modules.values().flat_map(|v| v.values())),
                        &*self.call_stack,
                    );
                }
            }
        }

        if ip == current_chunk.unwrap_function().unwrap().code.len() {
            return Err(InterpretError {
                opcode_index: ip - 1,
                chunk: current_chunk,
                kind: InterpretErrorKind::MissedReturn,
            });
        }
        //function will always terminate through InstructionExecution::Termination
        unreachable!()
    }

    #[inline(always)]
    fn execute_instruction(
        &mut self,
        ip: usize,
        current_chunk: &StackObject,
    ) -> Result<InstructionExecution> {
        use InterpretErrorKind::*;

        macro_rules! checked_stack_pop {
            () => {{
                self.stack.pop().ok_or(InterpretError {
                    opcode_index: ip,
                    chunk: current_chunk.clone(),
                    kind: StackUnderflow,
                })
            }};
        }

        macro_rules! runtime_error {
            ($e:expr) => {
                InterpretError {
                    opcode_index: ip,
                    chunk: current_chunk.clone(),
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

        macro_rules! comparison_operator {
            ($pat:pat) => {{
                let second_operand = checked_stack_pop!()?;
                let first_operand = checked_stack_pop!()?;
                let value = match crate::data::value_ops::comparison_operator!(
                    &first_operand,
                    &second_operand,
                    $pat
                ) {
                    Some(v) => Ok(v.into()),
                    None => Err(runtime_error!(InterpretErrorKind::TypeError {
                        message: format!(
                            "got unsupported values in comparison ({} and {})",
                            first_operand.type_string(),
                            second_operand.type_string()
                        )
                    })),
                }?;
                self.stack.push(value);
                InstructionExecution::NextInstruction
            }};
        }

        let chunk = current_chunk.unwrap_function().unwrap();

        macro_rules! checked_get_name {
            ($idx:expr) => {
                chunk
                    .global_names
                    .get($idx as usize)
                    .ok_or(runtime_error!(OperandIndexing))
            };
        }

        macro_rules! get_from_top {
            () => {
                self.stack
                    .last()
                    .ok_or_else(|| runtime_error!(StackUnderflow))
            };

            ($idx:expr) => {
                self.stack
                    .get($idx)
                    .ok_or_else(|| runtime_error!(StackUnderflow))
            };
        }

        let jump = match chunk.code[ip] {
            Opcode::LoadConst(idx) => {
                let idx = idx as usize;
                let value = chunk
                    .constants
                    .get(idx)
                    .cloned()
                    .ok_or(runtime_error!(OperandIndexing))?;
                self.stack.push(value);
                InstructionExecution::NextInstruction
            }
            Opcode::LoadImmediateInt(i) => {
                self.stack.push(Value::Int(i as i64));
                InstructionExecution::NextInstruction
            }

            Opcode::Add => {
                let second_operand = checked_stack_pop!()?;
                let first_operand = checked_stack_pop!()?;

                let value = match (&first_operand, &second_operand) {
                    (s1, s2) if s1.unwrap_any_str().is_some() && s2.unwrap_any_str().is_some() => {
                        self.gc
                            .try_inplace_string_concat(first_operand, second_operand)
                            .unwrap()
                    }

                    (_, _) => {
                        cast_binary!(&first_operand, +, &second_operand).ok_or_else(|| {
                            runtime_error!(InterpretErrorKind::TypeError {
                                message: format!(
                                    "uncompatible types in Add (got {} and {})",
                                    first_operand.type_string(),
                                    second_operand.type_string()
                                )
                            })
                        })?
                    }
                };
                self.stack.push(value);

                InstructionExecution::NextInstruction
            }

            Opcode::Sub => {
                let second_operand = checked_stack_pop!()?;
                let first_operand = checked_stack_pop!()?;
                self.stack
                    .push(
                        cast_binary!(&first_operand, -, &second_operand).ok_or_else(|| {
                            runtime_error!(InterpretErrorKind::TypeError {
                                message: format!(
                                    "uncompatible types in Sub (got {} and {})",
                                    first_operand.type_string(),
                                    second_operand.type_string()
                                )
                            })
                        })?,
                    );
                InstructionExecution::NextInstruction
            }

            Opcode::Mul => {
                let second_operand = checked_stack_pop!()?;
                let first_operand = checked_stack_pop!()?;
                self.stack
                    .push(
                        cast_binary!(&first_operand, *, &second_operand).ok_or_else(|| {
                            runtime_error!(InterpretErrorKind::TypeError {
                                message: format!(
                                    "uncompatible types in Multiply (got {} and {})",
                                    first_operand.type_string(),
                                    second_operand.type_string()
                                )
                            })
                        })?,
                    );
                InstructionExecution::NextInstruction
            }

            Opcode::Div => {
                let second_operand = checked_stack_pop!()?;
                let first_operand = checked_stack_pop!()?;

                if second_operand.unwrap_int().map(|i| i == 0i64) == Some(true)
                    || second_operand.unwrap_float().map(|f| f.abs() == 0f64) == Some(true)
                {
                    return Err(runtime_error!(ZeroDivision));
                }

                self.stack
                    .push(
                        cast_binary!(&first_operand, /, &second_operand).ok_or_else(|| {
                            runtime_error!(InterpretErrorKind::TypeError {
                                message: format!(
                                    "uncompatible types in Divide (got {} and {})",
                                    first_operand.type_string(),
                                    second_operand.type_string()
                                )
                            })
                        })?,
                    );

                InstructionExecution::NextInstruction
            }

            Opcode::Mod => {
                let second_operand = as_int!(checked_stack_pop!()?)?;
                let first_operand = as_int!(checked_stack_pop!()?)?;

                let value = first_operand
                    .checked_rem(second_operand)
                    .ok_or(runtime_error!(ZeroDivision))?;
                self.stack.push(Value::Int(value));
                InstructionExecution::NextInstruction
            }

            Opcode::Power => {
                let second_operand = checked_stack_pop!()?;
                let first_operand = checked_stack_pop!()?;

                let value: Value =
                    match (numeric_cast(&first_operand), numeric_cast(&second_operand)) {
                        (Some(NumberCastResult::Int(a)), Some(NumberCastResult::Int(b))) => {
                            if b < 0 || b > u32::MAX as i64 {
                                return Err(runtime_error!(InterpretErrorKind::NativeError {
                                    message: format!("unsupported operand {} in pow", b)
                                }));
                            }
                            a.pow(b as u64 as u32).into()
                        }

                        (Some(first), Some(second)) => {
                            (first.downgrade().powf(second.downgrade())).into()
                        }
                        _ => {
                            return Err(runtime_error!(InterpretErrorKind::TypeError {
                                message: format!(
                                    "expected numbers in pow, got {} and {}",
                                    first_operand.type_string(),
                                    second_operand.type_string()
                                )
                            }));
                        }
                    };

                self.stack.push(value);
                InstructionExecution::NextInstruction
            }

            Opcode::LoadGlobal(idx) => {
                let key = checked_get_name!(idx)?;
                let value = self
                    .loaded_modules
                    .get(&chunk.module)
                    .unwrap()
                    .get(key)
                    .cloned()
                    .or_else(|| self.builtins.get_builtin(key))
                    .ok_or(runtime_error!(InterpretErrorKind::NameError {
                        name: key.clone()
                    }))?;

                self.stack.push(value);
                InstructionExecution::NextInstruction
            }

            Opcode::LoadField(idx) => {
                let pointer = checked_stack_pop!()?;
                let key = checked_get_name!(idx)?;

                match pointer.lookup(key, self) {
                    Some(obj) => {
                        self.stack.push(obj);
                    }
                    None => {
                        return Err(runtime_error!(InterpretErrorKind::AttributeError {
                            object: pointer,
                            missed_field: key.to_string()
                        }))
                    }
                }

                InstructionExecution::NextInstruction
            }

            Opcode::StoreField(idx) => {
                let value = checked_stack_pop!()?;
                let pointer = checked_stack_pop!()?;
                let key = checked_get_name!(idx)?;

                match pointer.set_field(key, value, self) {
                    Ok(()) => {}
                    Err(()) => {
                        return Err(runtime_error!(InterpretErrorKind::AttributeError {
                            object: pointer,
                            missed_field: key.to_string()
                        }))
                    }
                }

                InstructionExecution::NextInstruction
            }

            Opcode::LoadFieldByIndex(idx) => {
                let pointer = checked_stack_pop!()?;

                match VM::get_property_idx_mut(&pointer, idx as usize) {
                    Some(field) => {
                        self.stack.push(field.clone());
                    }
                    None => {
                        return Err(runtime_error!(InterpretErrorKind::IndexAttributeError {
                            object: pointer,
                            missed_idx: idx as usize
                        }))
                    }
                }

                InstructionExecution::NextInstruction
            }

            Opcode::StoreFieldByIndex(idx) => {
                let value = checked_stack_pop!()?;
                let pointer = checked_stack_pop!()?;
                match VM::get_property_idx_mut(&pointer, idx as usize) {
                    Some(field) => {
                        *field = value;
                    }
                    None => {
                        return Err(runtime_error!(InterpretErrorKind::IndexAttributeError {
                            object: pointer,
                            missed_idx: idx as usize
                        }))
                    }
                }

                InstructionExecution::NextInstruction
            }

            Opcode::StoreGLobal(idx) => {
                let key = checked_get_name!(idx)?;
                let value = checked_stack_pop!()?;

                self.loaded_modules
                    .get_mut(&chunk.module)
                    .unwrap()
                    .insert(key.to_string(), value);
                InstructionExecution::NextInstruction
            }

            Opcode::LoadLocal(idx) => {
                let absolute_pos = self.locals_offset + idx as usize;
                let value = self
                    .stack
                    .get(absolute_pos)
                    .cloned()
                    .ok_or(runtime_error!(OperandIndexing))?;
                self.stack.push(value);
                InstructionExecution::NextInstruction
            }

            Opcode::StoreLocal(idx) => {
                let value = self.stack.pop().ok_or(runtime_error!(StackUnderflow))?;

                let absolute_pos = self.locals_offset + idx as usize;

                let addr = self
                    .stack
                    .get_mut(absolute_pos)
                    .ok_or(runtime_error!(OperandIndexing))?;
                *addr = value;
                InstructionExecution::NextInstruction
            }
            Opcode::TestEquals => {
                let second_operand = checked_stack_pop!()?;
                let first_operand = checked_stack_pop!()?;
                self.stack
                    .push(value_ops::equality_operator(&first_operand, &second_operand).into());
                InstructionExecution::NextInstruction
            }

            Opcode::TestNotEquals => {
                let second_operand = checked_stack_pop!()?;
                let first_operand = checked_stack_pop!()?;
                self.stack
                    .push((!value_ops::equality_operator(&first_operand, &second_operand)).into());
                InstructionExecution::NextInstruction
            }

            Opcode::TestGreater => {
                comparison_operator!(Ordering::Greater)
            }

            Opcode::TestGreaterEqual => {
                comparison_operator!(Ordering::Equal | Ordering::Greater)
            }

            Opcode::TestLess => comparison_operator!(Ordering::Less),

            Opcode::TestLessEqual => {
                comparison_operator!(Ordering::Equal | Ordering::Less)
            }

            Opcode::TestProperty(idx) => {
                let key = chunk
                    .global_names
                    .get(idx as usize)
                    .ok_or(runtime_error!(OperandIndexing))?;
                let pointer = checked_stack_pop!()?;
                let value = pointer.lookup(key, self).is_some().into();
                self.stack.push(value);
                InstructionExecution::NextInstruction
            }

            Opcode::JumpIfFalseOrPop(delta) => {
                let value = checked_stack_pop!()?;

                if !value.as_bool() {
                    let new_ip = ip + delta as usize;
                    if new_ip >= chunk.code.len() {
                        return Err(runtime_error!(JumpBounds));
                    }
                    self.stack.push(value);
                    InstructionExecution::LocalJump(new_ip)
                } else {
                    InstructionExecution::NextInstruction
                }
            }

            Opcode::JumpIfTrueOrPop(delta) => {
                let value = checked_stack_pop!()?;

                if value.as_bool() {
                    self.stack.push(value);
                    let new_ip = ip + delta as usize;
                    if new_ip >= chunk.code.len() {
                        return Err(runtime_error!(JumpBounds));
                    }
                    InstructionExecution::LocalJump(new_ip)
                } else {
                    InstructionExecution::NextInstruction
                }
            }

            Opcode::JumpRelative(delta) => {
                let new_ip = ip + delta as usize;
                if new_ip >= chunk.code.len() {
                    return Err(runtime_error!(JumpBounds));
                }
                InstructionExecution::LocalJump(new_ip)
            }

            Opcode::JumpAbsolute(idx) => {
                let new_ip = idx as usize;
                if new_ip >= chunk.code.len() {
                    return Err(runtime_error!(JumpBounds));
                }
                InstructionExecution::LocalJump(new_ip)
            }

            Opcode::Pop(n) => {
                if self.stack.len() < n as usize {
                    return Err(runtime_error!(StackUnderflow));
                }
                let new_stack_size = self.stack.len() - n as usize;
                self.stack.truncate(new_stack_size);
                InstructionExecution::NextInstruction
            }

            Opcode::LogicalNot => {
                let value = checked_stack_pop!()?;
                self.stack.push((!value.as_bool()).into());
                InstructionExecution::NextInstruction
            }

            Opcode::Nop => InstructionExecution::NextInstruction,
            Opcode::Assert => {
                let value = checked_stack_pop!()?;

                match value {
                    Value::Bool(true) => {}
                    Value::Bool(false) => {
                        return Err(runtime_error!(AssertionFailure));
                    }
                    other => {
                        return Err(runtime_error!(TypeError {
                            message: format!("expected Bool but got {}", other.type_string())
                        }))
                    }
                }
                InstructionExecution::NextInstruction
            }
            Opcode::Call(arity) => {
                //check stack
                let mut arity = arity as usize;

                self.check_underflow(arity + 1)
                    .map_err(|_e| runtime_error!(StackUnderflow))?;

                let mut object = self
                    .stack
                    .get(self.stack.len() - 1 - arity)
                    .cloned()
                    .unwrap();

                if object.unwrap_partial().is_some() {
                    let final_length = self.stack.len().saturating_sub(arity);
                    let args = self.stack.split_off(final_length);
                    let target = checked_stack_pop!()?;

                    //check that all blanks are filled for fully defined call
                    if !target
                        .unwrap_partial()
                        .unwrap()
                        .get_arity()
                        .accepts(args.len())
                    {
                        return Err(runtime_error!(TypeError {
                            message: format!(
                                "expected {} when calling partial but got {}",
                                target.unwrap_partial().unwrap().get_arity(),
                                args.len()
                            )
                        }));
                    }
                    let mut supplied_partial = target.unwrap_partial().unwrap().substitute(args);
                    //setup stack for defined call
                    self.stack.push(supplied_partial.target);
                    //we can no longer use arity from Call[N] as it applies to partial ant not
                    //underlying function
                    //
                    //instead, get new arity from created functional args
                    //(they are checked at CallPartial)
                    arity = supplied_partial.args.len();
                    self.stack.append(&mut supplied_partial.args);

                    //rewrite reference to called object to newly pushed callable
                    object = self
                        .stack
                        .get(self.stack.len() - 1 - arity)
                        .cloned()
                        .unwrap();
                }

                let call_arity = object.get_arity(self).ok_or_else(|| {
                    runtime_error!(TypeError {
                        message: format!("expected callable but got {}", object.type_string())
                    })
                })?;

                if !call_arity.accepts(arity) {
                    return Err(runtime_error!(InterpretErrorKind::TypeError {
                        message: format!("expected {call_arity} but got {arity}")
                    }));
                }

                if call_arity.is_vararg() {
                    let final_length = self.stack.len().saturating_sub(arity);
                    let args = self.stack.split_off(final_length);

                    let mut args = self.wrap_by_arity(args, call_arity).ok_or_else(|| {
                        runtime_error!(InterpretErrorKind::TypeError {
                            message: format!("expected {call_arity} but got {arity}")
                        })
                    })?;
                    arity = args.len();
                    self.stack.append(&mut args);
                }

                match &object {
                    Value::Builtin(name) => {
                        let final_length = self.stack.len().saturating_sub(arity);
                        let args = self.stack.split_off(final_length);
                        self.stack.pop(); //remove builtin

                        let builtins = self.builtins;

                        let result = builtins.apply_builtin(*name, args, self);
                        let result = result.map_err(|e| {
                            runtime_error!(InterpretErrorKind::NativeError {
                                message: e.to_string()
                            })
                        })?;
                        self.stack.push(result);
                        InstructionExecution::NextInstruction
                    }

                    &Value::BuiltinMethod {
                        class_idx,
                        method_idx,
                    } => {
                        let final_length = self.stack.len().saturating_sub(arity);
                        let args = self.stack.split_off(final_length);

                        let mut all_args = args;
                        let args = all_args.split_off(1);

                        let self_ptr = all_args.pop().unwrap();

                        //pop called builtin method off the stack
                        self.stack.pop();

                        let builtins = self.builtins;

                        let result =
                            builtins.apply_method(class_idx, method_idx, self_ptr, args, self);

                        let result = result.map_err(|e| {
                            runtime_error!(InterpretErrorKind::NativeError {
                                message: e.to_string()
                            })
                        })?;

                        self.stack.push(result);
                        InstructionExecution::NextInstruction
                    }

                    obj @ Value::HeapObject(..) if obj.unwrap_struct_descriptor().is_some() => {
                        let final_length = self.stack.len().saturating_sub(arity);
                        let args = self.stack.split_off(final_length);
                        self.stack.pop(); //remove descriptor

                        let descriptor = obj.unwrap_struct_descriptor().unwrap();

                        let instance = descriptor.make_instance(obj.clone(), args).map_err(
                            |(expected, got)| {
                                runtime_error!(TypeError {
                                    message: format!(
                                        "struct field mismatch: expected {}, got {}",
                                        expected, got
                                    )
                                })
                            },
                        )?;
                        let ptr = self.gc.store(instance);
                        self.stack.push(ptr);
                        InstructionExecution::NextInstruction
                    }

                    _ => {
                        let new_chunk = VM::get_chunk(object.clone()).ok_or_else(|| {
                            runtime_error!(TypeError {
                                message: format!(
                                    "expected function but got {}",
                                    object.type_string()
                                )
                            })
                        })?;

                        self.call_stack.push(CallStackValue {
                            return_chunk: current_chunk.clone(),
                            return_ip: ip + 1,
                            return_locals_offset: self.locals_offset,
                            return_stack_size: self.stack.len() - 1 - arity,
                        });

                        self.locals_offset = self.stack.len() - 1 - arity;

                        InstructionExecution::EnterChunk(new_chunk)
                    }
                }
            }

            Opcode::CallPartial(arity) => {
                let arity = arity as usize;

                self.check_underflow(arity + 1)
                    .map_err(|_e| runtime_error!(StackUnderflow))?;
                /*right now we have on stack:
                    1. some (probably) callable object
                    2. arguments mixed with at least one blank, total of `arity`
                */
                let final_length = self.stack.len().saturating_sub(arity);
                let args = self.stack.split_off(final_length);
                let target = checked_stack_pop!()?;
                if !VM::is_callable(&target) {
                    return Err(runtime_error!(TypeError {
                        message: format!(
                            "expected callable when performing partial call but got {}",
                            target.type_string()
                        )
                    }));
                }

                let target_arity = target.get_arity(self).unwrap();

                if !target_arity.accepts(arity as usize) {
                    return Err(runtime_error!(TypeError {
                        message: format!("expected {} but got {} args", target_arity, arity)
                    }));
                }

                let value = if let Some(partial) = target.unwrap_partial() {
                    let subs_partial = partial.substitute(args);
                    self.gc.store(subs_partial)
                } else {
                    self.gc.new_partial(target, target_arity, args)
                };

                self.stack.push(value);
                InstructionExecution::NextInstruction
            }

            Opcode::Return => {
                if self.call_stack.len() <= 1 {
                    return Ok(InstructionExecution::Termination);
                }
                let return_info = self.call_stack.pop().unwrap();

                self.locals_offset = return_info.return_locals_offset;

                let ret_value = checked_stack_pop!()?;
                if self.stack.len() < return_info.return_stack_size {
                    return Err(runtime_error!(StackUnderflow));
                }

                let new_stack_size = return_info.return_stack_size;
                self.stack.truncate(new_stack_size);
                self.stack.push(ret_value);
                InstructionExecution::CrossChunkJump {
                    new_chunk_id: return_info.return_chunk,
                    new_ip: return_info.return_ip,
                }
            }
            Opcode::NewBox => {
                let box_ref = self.gc.allocate_new::<ValueBox>();
                self.stack.push(box_ref);
                InstructionExecution::NextInstruction
            }
            Opcode::LoadBox => {
                let addr = checked_stack_pop!()?;
                addr.unwrap_box()
                    .map(|box_obj| {
                        self.stack.push(box_obj.0.clone());
                    })
                    .ok_or_else(|| {
                        runtime_error!(TypeError {
                            message: format!("expected {} but got {}", "box", addr.type_string()),
                        })
                    })?;

                InstructionExecution::NextInstruction
            }

            Opcode::StoreBox => {
                let value = checked_stack_pop!()?;
                let addr = checked_stack_pop!()?;
                addr.unwrap_box()
                    .map(|box_obj| {
                        box_obj.0 = value;
                    })
                    .ok_or_else(|| {
                        runtime_error!(TypeError {
                            message: format!("expected {} but got {}", "box", addr.type_string()),
                        })
                    })?;
                InstructionExecution::NextInstruction
            }

            Opcode::NewClosure => {
                let value = checked_stack_pop!()?;
                if value.unwrap_function().is_none() {
                    return Err(runtime_error!(TypeError {
                        message: format!("expected {} but got {}", "function", value.type_string())
                    }));
                }

                let ptr = self.gc.store(Closure {
                    closed_values: vec![],
                    underlying: value,
                });
                self.stack.push(ptr);
                InstructionExecution::NextInstruction
            }

            Opcode::AddClosedValue => {
                let value = checked_stack_pop!()?;
                let closure = checked_stack_pop!()?;

                match closure.unwrap_closure() {
                    Some(closure_ptr) => {
                        closure_ptr.closed_values.push(value);
                    }
                    None => {
                        return Err(runtime_error!(TypeError {
                            message: format!(
                                "expected {} but got {}",
                                "closure",
                                closure.type_string()
                            )
                        }))
                    }
                }
                self.stack.push(closure);
                InstructionExecution::NextInstruction
            }

            Opcode::LoadClosureValue(idx) => {
                let maybe_closure = self.stack.get(self.locals_offset).unwrap();
                let closure = maybe_closure.unwrap_closure().ok_or_else(|| {
                    runtime_error!(TypeError {
                        message: format!(
                            "expected {} but got {}",
                            "function",
                            maybe_closure.type_string()
                        )
                    })
                })?;
                let value = closure
                    .closed_values
                    .get(idx as usize)
                    .cloned()
                    .ok_or(runtime_error!(OperandIndexing))?;
                self.stack.push(value);
                InstructionExecution::NextInstruction
            }
            Opcode::Duplicate => {
                let value = get_from_top!()?.clone();
                self.stack.push(value);
                InstructionExecution::NextInstruction
            }
            Opcode::LoadBlank => {
                self.stack.push(StackObject::Blank);
                InstructionExecution::NextInstruction
            }

            Opcode::LoadNothing => {
                self.stack.push(StackObject::Nothing);
                InstructionExecution::NextInstruction
            }

            Opcode::MakeList(size) => {
                let size = size as usize;
                self.check_underflow(size + 1)
                    .map_err(|_e| runtime_error!(StackUnderflow))?;

                let new_length = self.stack.len().saturating_sub(size);

                let items = self.stack.split_off(new_length);

                self.stack.push(self.gc.store(items));

                InstructionExecution::NextInstruction
            }
        };
        Ok(jump)
    }

    fn check_underflow(&self, needed_args: usize) -> std::result::Result<(), ()> {
        if self.stack.len() < needed_args {
            return Err(());
        }
        Ok(())
    }

    fn get_property_idx_mut(pointer: &Value, index: usize) -> Option<&mut Value> {
        match pointer {
            obj @ StackObject::HeapObject(..) if obj.unwrap_struct_instance().is_some() => {
                let instance = obj.unwrap_struct_instance().unwrap();
                instance.fields.get_index_mut(index).map(|(_k, v)| v)
            }

            _other => None,
        }
    }

    fn wrap_by_arity(&mut self, mut args: VVec, arity: Arity) -> Option<VVec> {
        if arity.accepts(args.len()) {
            match arity {
                Arity::Exact(_) => Some(args),
                Arity::AtLeast(expected) => {
                    let vararg_part = args.split_off(expected);

                    let vararg_ptr = self.gc.store(vararg_part);
                    args.push(vararg_ptr);
                    Some(args)
                }
            }
        } else {
            None
        }
    }

    fn is_callable(value: &StackObject) -> bool {
        matches!(
            value,
            StackObject::Builtin(_) | StackObject::BuiltinMethod { .. }
        ) || value.unwrap_closure().is_some()
            || value.unwrap_partial().is_some()
            || value.unwrap_function().is_some()
    }

    fn get_chunk(value: StackObject) -> Option<StackObject> {
        if value.unwrap_function().is_some() {
            return Some(value);
        }
        if value.unwrap_closure().is_some() {
            return Some(value.unwrap_closure().unwrap().underlying.clone());
        }
        if value.unwrap_partial().is_some() {
            return VM::get_chunk(value.unwrap_partial().unwrap().target.clone());
        }
        None
    }
}
