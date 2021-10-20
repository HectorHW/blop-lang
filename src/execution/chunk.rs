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

impl Display for Chunk {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
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
                    Opcode::Jump(delta) => {
                        format!(
                            "{:<21} ({})",
                            format!("{}", Opcode::Jump(*delta)),
                            i + *delta as usize
                        )
                    }

                    any_other => {
                        format!("{}", any_other)
                    }
                }
            );
            res.push(s);
        }
        return res;
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
                Opcode::JumpIfFalse(delta) | Opcode::Jump(delta) => {
                    let mut start = pos;
                    let mut end = start + *delta as usize;
                    if start > end {
                        std::mem::swap(&mut start, &mut end);
                    }
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

            draw_at_char(&mut lines[arrow_start], 0, b'*');
            for i in arrow_start + 1..arrow_end {
                draw_at_char(&mut lines[i], 0, b'|');
            }
            draw_at_char(&mut lines[arrow_end], 0, b'>');
        }
    }

    fn check_canvas_range(lines: &Vec<String>, range: (usize, usize)) -> bool {
        for i in range.0..=range.1 {
            if !lines[i].starts_with(' ') {
                return false;
            }
        }
        return true;
    }

    fn draw_at_char(s: &mut String, idx: usize, c: u8) {
        if !c.is_ascii() {
            panic!()
        }
        let raw_str_ref = unsafe { s.as_bytes_mut() };
        raw_str_ref[idx] = c;
    }
}
