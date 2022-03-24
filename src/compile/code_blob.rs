use crate::execution::chunk::Opcode;
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

    pub fn append(&mut self, other: Self) {
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

    pub fn fuse(mut self, other: AnnotatedCodeBlob) -> AnnotatedCodeBlob {
        let instruction_offset = self.code.len();

        self._append(other.shift_positioned_code(instruction_offset));

        self
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
