use crate::data::values::Value;
use std::fmt::{Display, Formatter};
use std::ops::AddAssign;

#[derive(Debug, Clone)]
pub struct Chunk {
    pub constants: Vec<Value>,
    pub code: Vec<Opcode>,
}

#[derive(Debug, Clone, Copy)]
pub enum Opcode {
    Print,
    LoadConst(u16),
    Load(u16),
    Store(u16),
    LoadImmediateInt(i16),
    Add,
    Sub,
    Div,
    Mul,

    TestEquals,

    JumpIfFalse(u16),
    Jump(u16),

    Pop(u16),

    Nop, //SwapStack(u8, u8),
         //ExtendArg1(u16),
         //ExtendDouble(u8, u8)
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Opcode::*;
        write!(
            f,
            "{}",
            match self {
                Print => "Print".to_string(),
                LoadConst(a) => format!("LoadConst[{}]", a),
                LoadImmediateInt(i) => format!("LoadImmediateInt[{}]", i),
                Load(a) => format!("LoadStack[{}]", a),
                Store(a) => format!("StoreStack[{}]", a),
                Add => "Add".to_string(),
                Sub => "Sub".to_string(),
                Div => "Div".to_string(),
                Mul => "Mul".to_string(), //Opcode::SwapStack(a, b) => format!("SwapStack[{}, {}]", a, b),
                //Opcode::ExtendArg1(e) => format!("Extend[{}]", e),
                //Opcode::ExtendDouble(a, b) => format!("Extend[{}, {}]", a, b)
                TestEquals => "TestEquals".to_string(),
                JumpIfFalse(delta) => format!("JumpIfFalse[{}]", delta),
                Jump(delta) => format!("Jump[{}]", delta),
                Pop(n) => format!("Pop[{}]", n),
                Nop => "Nop".to_string(),
            }
        )
    }
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            constants: Vec::new(),
            code: Vec::new(),
        }
    }

    pub fn append(&mut self, values: Vec<Opcode>) {
        let mut values = values;
        self.code.append(&mut values)
    }
}

impl AddAssign<Opcode> for Chunk {
    fn add_assign(&mut self, rhs: Opcode) {
        self.code.push(rhs)
    }
}
