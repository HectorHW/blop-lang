use crate::data::values::Value;
use crate::execution::chunk::{Chunk, Opcode};

pub struct VM {
    pub(super) stack: Vec<Value>,
}

type Result<T> = std::result::Result<T, InterpretError>;

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub struct InterpretError {
    pub opcode_index: usize,
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
}

impl VM {
    pub fn new() -> VM {
        VM { stack: Vec::new() }
    }

    pub fn run(&mut self, program: &Chunk) -> Result<()> {
        use InterpretErrorKind::*;
        let mut ip = 0;
        let mut arg_1_register = 0;

        macro_rules! checked_stack_pop {
            () => {{
                self.stack.pop().ok_or(InterpretError {
                    opcode_index: ip,
                    kind: StackUnderflow,
                })
            }};
        }

        macro_rules! runtime_error {
            ($e:expr) => {
                InterpretError {
                    opcode_index: ip,
                    kind: $e,
                }
            };
        }

        let code_bounds = program.code.len();

        while ip < code_bounds {
            print!("{} => ", program.code[ip]);
            match program.code[ip] {
                Opcode::Print => {
                    let result = checked_stack_pop!()?;
                    println!("{}", result);
                    ip += 1;
                }
                Opcode::LoadConst(idx) => {
                    arg_1_register <<= 16;
                    arg_1_register += idx as usize;
                    let value = *program
                        .constants
                        .get(arg_1_register)
                        .ok_or(runtime_error!(OperandIndexing))?;
                    self.stack.push(value);
                    ip += 1;
                }
                Opcode::LoadImmediateInt(i) => {
                    self.stack.push(i as i64 as Value);
                    ip += 1;
                }

                Opcode::Add => {
                    let second_operand = checked_stack_pop!()? as i64;
                    let first_operand = checked_stack_pop!()? as i64;
                    self.stack.push((first_operand + second_operand) as Value);
                    ip += 1;
                }

                Opcode::Sub => {
                    let second_operand = checked_stack_pop!()? as i64;
                    let first_operand = checked_stack_pop!()? as i64;
                    self.stack.push((first_operand - second_operand) as Value);
                    ip += 1;
                }

                Opcode::Mul => {
                    let second_operand = checked_stack_pop!()? as i64;
                    let first_operand = checked_stack_pop!()? as i64;
                    self.stack.push((first_operand * second_operand) as Value);
                    ip += 1;
                }

                Opcode::Div => {
                    let second_operand = checked_stack_pop!()? as i64;
                    let first_operand = checked_stack_pop!()? as i64;

                    let value = first_operand
                        .checked_div(second_operand)
                        .ok_or(runtime_error!(ZeroDivision))?;
                    self.stack.push(value);
                    ip += 1;
                }
                Opcode::Load(idx) => {
                    let value = *self
                        .stack
                        .get(idx as usize)
                        .ok_or(runtime_error!(OperandIndexing))?;

                    self.stack.push(value);
                    ip += 1;
                }
                Opcode::Store(idx) => {
                    let value = self.stack.pop().ok_or(runtime_error!(StackUnderflow))?;

                    let addr = self
                        .stack
                        .get_mut(idx as usize)
                        .ok_or(runtime_error!(OperandIndexing))?;
                    *addr = value;
                    ip += 1;
                }
                Opcode::TestEquals => {
                    let second_operand = checked_stack_pop!()? as i64;
                    let first_operand = checked_stack_pop!()? as i64;
                    let value = if second_operand == first_operand {
                        1
                    } else {
                        0
                    };
                    self.stack.push(value);
                    ip += 1;
                }
                Opcode::JumpIfFalse(delta) => {
                    let value_to_test = checked_stack_pop!()? as i64;
                    if value_to_test == 0 {
                        let new_ip = ip + delta as usize;
                        if new_ip >= code_bounds {
                            return Err(runtime_error!(JumpBounds));
                        }
                        ip = new_ip;
                    } else {
                        ip += 1;
                    }
                }
                Opcode::Jump(delta) => {
                    let new_ip = ip + delta as usize;
                    if new_ip >= code_bounds {
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
                    let value = checked_stack_pop!()? as i64;
                    if value == 0 {
                        return Err(runtime_error!(AssertionFailure));
                    }
                    ip += 1;
                }
            }
            println!("{:?}", self.stack);
        }

        Ok(())
    }
}
