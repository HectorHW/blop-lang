use crate::compile::code_blob::AnnotatedCodeBlob;
use crate::data::objects::Value;
use crate::parsing::lexer::{Token, TokenKind};
use std::fmt::{Display, Formatter};

use super::arity::Arity;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Chunk {
    pub constants: Vec<Value>,
    pub global_names: Vec<String>,
    pub code: Vec<Opcode>,
    pub name: Token,
    pub arity: Arity,
    pub opcode_to_line: Vec<usize>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Opcode {
    LoadConst(u16),
    LoadGlobal(u16),
    LoadLocal(u16),
    StoreLocal(u16),
    StoreGLobal(u16),

    LoadField(u16),
    StoreField(u16),
    LoadFieldByIndex(u16),
    StoreFieldByIndex(u16),

    NewBox,
    LoadBox,
    StoreBox,

    NewClosure,
    AddClosedValue,
    LoadClosureValue(u16),

    LoadBlank,
    LoadNothing,
    CallPartial(u16),

    Duplicate,

    LoadImmediateInt(i16),
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    Power,

    TestEquals,
    TestNotEquals,

    TestGreater,
    TestGreaterEqual,
    TestLess,
    TestLessEqual,

    TestProperty(u16),

    LogicalNot,

    JumpIfFalseOrPop(u16),
    JumpIfTrueOrPop(u16),
    JumpRelative(u16),
    JumpAbsolute(u16),
    Pop(u16),

    Call(u16),

    MakeList(u16),
    Return,

    Nop,
    Assert, //SwapStack(u8, u8),
            //ExtendArg1(u16),
            //ExtendDouble(u8, u8)
}

impl Display for Opcode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Chunk {
    pub fn new(name: Token, arity: Arity) -> Chunk {
        Chunk {
            constants: Vec::new(),
            global_names: Vec::new(),
            code: Vec::new(),
            name,
            arity,
            opcode_to_line: vec![],
        }
    }

    pub fn append(&mut self, mut blob: AnnotatedCodeBlob) {
        self.code.append(&mut blob.code);
        self.opcode_to_line.append(&mut blob.indices);
    }
}

impl Display for Chunk {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{:-^40}",
            format!(
                "chunk '{}' at {}",
                match &self.name.kind {
                    TokenKind::Name(..) => self.name.get_string().unwrap(),
                    TokenKind::Arrow => "<anon function>",
                    _ => panic!(),
                },
                self.name.position
            )
        )?;
        writeln!(
            f,
            "constants:\n{}",
            self.constants
                .iter()
                .enumerate()
                .map(|(i, v)| { format!("  {i}: {v:?}") })
                .collect::<Vec<String>>()
                .join("\n")
        )?;
        writeln!(f, "code:")?;
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

            macro_rules! pretty_argument {
                ($arg:expr) => {
                    format!("{:<21} ({})", format!("{}", opcode), $arg)
                };
            }

            macro_rules! pretty_with_global {
                ($idx:expr) => {
                    format!(
                        "{:<21} ({})",
                        format!("{}", opcode),
                        format!("value {}", (chunk.global_names[($idx) as usize]))
                    )
                };
            }

            let _ = write!(
                s,
                "{:<5} {}",
                i,
                match opcode {
                    Opcode::LoadConst(idx) =>
                        pretty_argument!(format!("value {}", (chunk.constants[(*idx) as usize]))),

                    Opcode::LoadGlobal(idx) => pretty_with_global!(*idx),

                    Opcode::LoadField(idx) => pretty_with_global!(*idx),

                    Opcode::StoreField(idx) => pretty_with_global!(*idx),

                    Opcode::TestProperty(idx) => pretty_with_global!(*idx),

                    Opcode::StoreGLobal(idx) => pretty_with_global!(*idx),

                    Opcode::LoadImmediateInt(n) => pretty_argument!(format!("value {}", n)),

                    op @ Opcode::LoadLocal(idx) => {
                        if chunk.name.get_string().unwrap() != "<script>" {
                            //inside some function
                            if *idx == 0 {
                                pretty_argument!("current function")
                            } else if *idx as usize <= chunk.arity.into() {
                                pretty_argument!(format!("argument {}", idx - 1))
                            } else {
                                format!("{}", op)
                            }
                        } else {
                            format!("{}", op)
                        }
                    }

                    Opcode::JumpIfFalseOrPop(delta) => pretty_argument!(i + *delta as usize),

                    Opcode::JumpIfTrueOrPop(delta) => pretty_argument!(i + *delta as usize),

                    Opcode::JumpRelative(delta) => pretty_argument!(i + *delta as usize),

                    Opcode::JumpAbsolute(idx) => pretty_argument!(*idx as usize),

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
                Opcode::JumpIfFalseOrPop(delta)
                | Opcode::JumpIfTrueOrPop(delta)
                | Opcode::JumpRelative(delta) => {
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

            for line in &mut lines[arrow_start + 1..arrow_end] {
                draw_at_char(line, 0, b'|');
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

    fn draw_at_char(s: &mut str, idx: usize, c: u8) {
        if !c.is_ascii() {
            panic!()
        }
        let raw_str_ref = unsafe { s.as_bytes_mut() };
        raw_str_ref[idx] = c;
    }
}
