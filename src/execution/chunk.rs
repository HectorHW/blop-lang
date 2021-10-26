use crate::data::objects::Value;
use std::fmt::{Display, Formatter};
use std::ops::AddAssign;

#[derive(Debug, Clone)]
pub struct Chunk {
    pub constants: Vec<Value>,
    pub code: Vec<Opcode>,
    pub name: String,
    pub arity: usize,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Opcode {
    Print,
    LoadConst(u16),
    LoadGlobal(u16),
    LoadLocal(u16),
    StoreLocal(u16),

    NewBox,
    LoadBox,
    StoreBox,

    NewClosure,
    AddClosedValue,
    LoadClosureValue(u16),

    Duplicate,

    LoadImmediateInt(i16),
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    Power,

    TestEquals,

    JumpIfFalse(u16),
    JumpRelative(u16),
    JumpAbsolute(u16),
    Pop(u16),

    Call(u16),
    Return,

    Nop,
    Assert, //SwapStack(u8, u8),
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
                LoadGlobal(a) => format!("LoadGlobal[{}]", a),
                StoreLocal(a) => format!("StoreLocal[{}]", a),
                Add => "Add".to_string(),
                Sub => "Sub".to_string(),
                Div => "Div".to_string(),
                Mul => "Mul".to_string(), //Opcode::SwapStack(a, b) => format!("SwapStack[{}, {}]", a, b),
                //Opcode::ExtendArg1(e) => format!("Extend[{}]", e),
                //Opcode::ExtendDouble(a, b) => format!("Extend[{}, {}]", a, b)
                TestEquals => "TestEquals".to_string(),
                JumpIfFalse(delta) => format!("JumpIfFalse[{}]", delta),
                JumpRelative(delta) => format!("Jump[{}]", delta),
                Pop(n) => format!("Pop[{}]", n),
                Nop => "Nop".to_string(),
                Assert => "Assert".to_string(),
                Call(arity) => format!("Call[{}]", arity),
                Return => "Return".to_string(),
                LoadLocal(idx) => format!("LoadLocal[{}]", idx),
                NewBox => "NewBox".to_string(),
                LoadBox => "LoadBox".to_string(),
                StoreBox => "StoreBox".to_string(),
                NewClosure => "NewClosure".to_string(),
                AddClosedValue => "AddClosedValue".to_string(),
                LoadClosureValue(idx) => format!("LoadClosureValue[{}]", idx),
                Duplicate => "Duplicate".to_string(),
                JumpAbsolute(idx) => format!("JumpAbsolute[{}]", idx),
                Mod => "Mod".to_string(),
                Power => "Power".to_string(),
            }
        )
    }
}

impl Chunk {
    pub fn new(name: String, arity: usize) -> Chunk {
        Chunk {
            constants: Vec::new(),
            code: Vec::new(),
            name,
            arity,
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

impl Display for Chunk {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{:-^40}", format!("chunk {}", self.name))?;
        writeln!(f, "constants: {:?}", self.constants)?;
        let strings = chunk_pretty_printer::draw_chunk(self);
        for s in &strings {
            writeln!(f, "{}", s)?
        }
        Ok(())
    }
}

mod chunk_pretty_printer {
    use crate::execution::chunk::{Chunk, Opcode};

    pub fn draw_chunk(chunk: &Chunk) -> Vec<String> {
        let mut strings = draw_instructions(chunk);
        extend_canvas(&mut strings);
        draw_arrows(chunk, &mut strings);
        strings
    }

    fn draw_instructions(chunk: &Chunk) -> Vec<String> {
        use std::fmt::Write;
        let mut res = vec![];
        for (i, opcode) in chunk.code.iter().enumerate() {
            let mut s = String::new();
            let _ = write!(
                s,
                "{:<5} {}",
                i,
                match opcode {
                    Opcode::LoadConst(idx) => {
                        format!(
                            "{:<21} (value {})",
                            format!("{}", Opcode::LoadConst(*idx)),
                            (chunk.constants[(*idx) as usize])
                        )
                    }

                    Opcode::LoadImmediateInt(n) => {
                        format!("{:<21} (value {})", "LoadImmediateInt", n)
                    }

                    Opcode::JumpIfFalse(delta) => {
                        format!(
                            "{:<21} ({})",
                            format!("{}", Opcode::JumpIfFalse(*delta)),
                            i + *delta as usize
                        )
                    }
                    Opcode::JumpRelative(delta) => {
                        format!(
                            "{:<21} ({})",
                            format!("{}", Opcode::JumpRelative(*delta)),
                            i + *delta as usize
                        )
                    }

                    Opcode::JumpAbsolute(idx) => {
                        format!(
                            "{:<21} ({})",
                            format!("{}", Opcode::JumpAbsolute(*idx)),
                            *idx as usize
                        )
                    }

                    any_other => {
                        format!("{}", any_other)
                    }
                }
            );
            res.push(s);
        }
        res
    }

    fn extend_canvas(lines: &mut Vec<String>) {
        for item in lines {
            let mut new_str = String::new();
            new_str.push_str("  ");
            new_str.push_str(item);
            *item = new_str;
        }
    }

    fn draw_arrows(chunk: &Chunk, lines: &mut Vec<String>) {
        let arrows = chunk
            .code
            .iter()
            .enumerate()
            .filter_map(|(pos, opcode)| match opcode {
                Opcode::JumpIfFalse(delta) | Opcode::JumpRelative(delta) => {
                    let start = pos;
                    let end = start + *delta as usize;
                    Some((start, end))
                }
                Opcode::JumpAbsolute(idx) => {
                    let start = pos;
                    let end = *idx as usize;
                    Some((start, end))
                }
                _ => None,
            })
            .collect::<Vec<_>>();

        for (arrow_start, arrow_end) in arrows {
            if !check_canvas_range(lines, (arrow_start, arrow_end)) {
                extend_canvas(lines);
            }

            //draw exit

            let (mut arrow_start, mut arrow_end) = (arrow_start, arrow_end);

            draw_at_char(&mut lines[arrow_start], 0, b'*');
            draw_at_char(&mut lines[arrow_end], 0, b'>');

            if arrow_start > arrow_end {
                std::mem::swap(&mut arrow_start, &mut arrow_end);
            }
            for i in arrow_start + 1..arrow_end {
                draw_at_char(&mut lines[i], 0, b'|');
            }
        }
    }

    fn check_canvas_range(lines: &[String], range: (usize, usize)) -> bool {
        let mut range = range;
        if range.0 > range.1 {
            std::mem::swap(&mut range.0, &mut range.1);
        }

        for line in &lines[range.0..=range.1] {
            if !line.starts_with(' ') {
                return false;
            }
        }
        true
    }

    fn draw_at_char(s: &mut String, idx: usize, c: u8) {
        if !c.is_ascii() {
            panic!()
        }
        let raw_str_ref = unsafe { s.as_bytes_mut() };
        raw_str_ref[idx] = c;
    }
}
