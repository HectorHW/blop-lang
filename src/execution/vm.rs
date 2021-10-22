use crate::data::values::Value;
use crate::execution::chunk::{Chunk, Opcode};

pub struct VM {
    pub(super) stack: Vec<Value>,
    pub(super) call_stack: Vec<CallStackValue>,
    locals_offset: usize,
}

pub struct CallStackValue {
    return_chunk: usize,
    return_ip: usize,
    return_locals_offset: usize,
    function_arguments: usize,
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
    TypeError { message: String },
}

impl VM {
    pub fn new() -> VM {
        VM {
            stack: Vec::new(),
            call_stack: Vec::new(),
            locals_offset: 0,
        }
    }

    pub fn run(&mut self, program: &Vec<Chunk>) -> Result<()> {
        use InterpretErrorKind::*;
        let mut ip = 0;
        let mut current_chunk_id = 0;
        let mut current_chunk = program.get(current_chunk_id).unwrap();
        let mut arg_1_register = 0;

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
                    u.unwrap_int().ok_or_else(|| {
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

        while ip < current_chunk.code.len() {
            #[cfg(debug_assertions)]
            print!("{} => ", current_chunk.code[ip]);
            match current_chunk.code[ip] {
                Opcode::Print => {
                    let result = checked_stack_pop!()?;
                    println!("{}", result);
                    ip += 1;
                }
                Opcode::LoadConst(idx) => {
                    arg_1_register <<= 16;
                    arg_1_register += idx as usize;
                    let value = *current_chunk
                        .constants
                        .get(arg_1_register)
                        .ok_or(runtime_error!(OperandIndexing))?;
                    self.stack.push(value);
                    ip += 1;
                }
                Opcode::LoadImmediateInt(i) => {
                    self.stack.push(Value::Int(i as i64));
                    ip += 1;
                }

                Opcode::Add => {
                    //let second_operand = checked_stack_pop!();
                    let second_operand = as_int!(checked_stack_pop!()?)?;

                    let first_operand = as_int!(checked_stack_pop!()?)?;
                    self.stack.push(Value::Int(first_operand + second_operand));
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
                Opcode::LoadGlobal(idx) => {
                    let value = *self
                        .stack
                        .get(idx as usize)
                        .ok_or(runtime_error!(OperandIndexing))?;

                    self.stack.push(value);
                    ip += 1;
                }

                Opcode::LoadLocal(idx) => {
                    let absolute_pos = self.locals_offset + idx as usize;
                    let value = *self
                        .stack
                        .get(absolute_pos)
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
                Opcode::Jump(delta) => {
                    let new_ip = ip + delta as usize;
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

                    let object = *self.stack.get(self.stack.len() - 1 - arity).unwrap();
                    let chunk_id = match object {
                        Value::Function { chunk_id } => Ok(chunk_id),
                        _ => Err(runtime_error!(TypeError {
                            message: format!("expected function but got {}", object.type_string())
                        })),
                    }?;

                    self.call_stack.push(CallStackValue {
                        return_chunk: current_chunk_id,
                        return_ip: ip + 1,
                        function_arguments: arity,
                        return_locals_offset: self.locals_offset,
                    });

                    self.locals_offset = self.stack.len() - 1 - arity;

                    current_chunk_id = chunk_id;
                    ip = 0;
                    current_chunk = program.get(current_chunk_id).unwrap();
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
                    let new_stack_size = self
                        .stack
                        .len()
                        .checked_sub(1 + return_info.function_arguments)
                        .ok_or(runtime_error!(StackUnderflow))?;
                    self.stack.truncate(new_stack_size);
                    self.stack.push(ret_value);
                }
            }
            #[cfg(debug_assertions)]
            println!("{:?}", self.stack);
        }

        Ok(())
    }
}
