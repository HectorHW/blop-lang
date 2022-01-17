use crate::data::objects::Value;
use crate::execution::chunk::{Chunk, Opcode};
use crate::parsing::lexer::Token;
use std::collections::HashMap;
use std::ops::{Add, AddAssign};

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
enum Relativity {
    Absolute,
    Relative,
}

#[derive(Clone, Default, Debug)]
pub struct AnnotatedCodeBlob {
    pub code: Vec<Opcode>,
    pub indices: Vec<usize>,
    pub constants: Vec<Value>,
    pub global_names: Vec<String>,
    relativity: Vec<Relativity>,
}

impl AnnotatedCodeBlob {
    pub fn new() -> Self {
        Self::default()
    }

    fn _append(&mut self, mut other: Self) {
        self.indices.append(&mut other.indices);
        self.code.append(&mut other.code);
        self.relativity.append(&mut other.relativity);
    }

    pub fn append(&mut self, mut other: Self) {
        let mut one = AnnotatedCodeBlob::new();
        std::mem::swap(self, &mut one);
        one = one.fuse(other);
        std::mem::swap(self, &mut one);
    }

    pub fn push(&mut self, code: Opcode, index: usize) {
        self.code.push(code);
        self.indices.push(index);
        self.relativity.push(Relativity::Relative);
    }

    pub fn make_last_absolute(&mut self) {
        if let Some(r) = self.relativity.last_mut() {
            *r = Relativity::Absolute;
        }
    }

    pub fn last_index(&self) -> Option<usize> {
        self.indices.last().cloned()
    }

    pub fn get_or_create_name(&mut self, name: &str) -> usize {
        for (i, item) in self.global_names.iter().enumerate() {
            if item == name {
                return i;
            }
        }
        self.global_names.push(name.to_string());
        self.global_names.len() - 1
    }

    pub fn get_or_create_constant(&mut self, constant: Value) -> usize {
        for (i, item) in self.constants.iter().enumerate() {
            if item == &constant {
                return i;
            }
        }
        self.constants.push(constant);
        self.constants.len() - 1
    }

    fn shift_names(mut self, transformation: HashMap<u16, u16>) -> AnnotatedCodeBlob {
        for code in &mut self.code {
            match code {
                Opcode::LoadGlobal(idx) => {
                    *idx = *transformation.get(idx).unwrap();
                }
                _ => {}
            }
        }
        self
    }

    fn shift_constants(mut self, transformation: HashMap<u16, u16>) -> AnnotatedCodeBlob {
        for code in &mut self.code {
            match code {
                Opcode::LoadConst(idx) => {
                    *idx = *transformation.get(idx).unwrap();
                }
                _ => {}
            }
        }
        self
    }

    fn shift_positioned_code(mut self, offset: usize) -> AnnotatedCodeBlob {
        for (code, relativity) in self.code.iter_mut().zip(self.relativity.iter()) {
            match code {
                Opcode::JumpAbsolute(target) if matches!(relativity, Relativity::Relative) => {
                    let new_target = *target as usize + offset;
                    if new_target > u16::MAX as usize {
                        panic!("index overflow over u16::MAX when trying to shift positioned code");
                    }
                    *target = new_target as u16;
                }
                _ => {}
            }
        }
        self
    }

    pub fn fuse(mut self, mut other: AnnotatedCodeBlob) -> AnnotatedCodeBlob {
        let mut naming_transform_map: HashMap<u16, u16> = HashMap::new();
        for (i, name) in other.global_names.iter().enumerate() {
            let index_in_first = self.get_or_create_name(name);
            naming_transform_map.insert(i as u16, index_in_first as u16);
        }

        let mut constant_transform_map: HashMap<u16, u16> = HashMap::new();
        let mut other_constants = vec![];
        std::mem::swap(&mut other_constants, &mut other.constants);
        for (i, constant) in other_constants.into_iter().enumerate() {
            let index_in_first = self.get_or_create_constant(constant);
            constant_transform_map.insert(i as u16, index_in_first as u16);
        }

        let instruction_offset = self.code.len();

        self._append(
            other
                .shift_constants(constant_transform_map)
                .shift_names(naming_transform_map)
                .shift_positioned_code(instruction_offset),
        );

        self
    }

    pub fn into_chunk(self, name: Token, arity: usize) -> Chunk {
        Chunk {
            constants: self.constants,
            global_names: self.global_names,
            code: self.code,
            name,
            arity,
            opcode_to_line: self.indices,
        }
    }
}

impl AddAssign<(Opcode, usize)> for AnnotatedCodeBlob {
    fn add_assign(&mut self, rhs: (Opcode, usize)) {
        self.push(rhs.0, rhs.1);
    }
}

impl Add<AnnotatedCodeBlob> for AnnotatedCodeBlob {
    type Output = AnnotatedCodeBlob;

    fn add(self, rhs: AnnotatedCodeBlob) -> Self::Output {
        self.fuse(rhs)
    }
}
