use crate::execution::chunk::{Chunk, Opcode};
use crate::parsing::ast::{Expr, Op, Program, Stmt};
use crate::parsing::lexer::TokenKind;
use std::collections::HashMap;

pub struct Compiler {
    names: HashMap<String, usize>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            names: HashMap::new(),
        }
    }

    pub fn compile(&mut self, program: &Program) -> Result<Chunk, String> {
        let mut chunk = Chunk::new();
        for stmt in program {
            self.visit_stmt(&mut chunk, stmt)?;
        }
        Ok(chunk)
    }

    fn visit_stmt(&mut self, current_chunk: &mut Chunk, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Print(e) => {
                self.visit_expr(current_chunk, e)?;
                *current_chunk += Opcode::Print;
            }
            Stmt::VarDeclaration(n, e) => {
                let varname = match &n.kind {
                    TokenKind::Name(n) => n,
                    _ => panic!(),
                };
                if self.names.contains_key(varname) {
                    Err(format!("redefinition of variable {}", varname))?
                }
                let this_varname_index = self.names.len();
                self.names.insert(varname.clone(), this_varname_index);

                if e.is_none() {
                    *current_chunk += Opcode::LoadImmediateInt(0);
                } else {
                    self.visit_expr(current_chunk, e.as_ref().unwrap())?;
                }
            }
            Stmt::Assignment(target, expr) => {
                let varname = target.get_string().unwrap();
                let var_idx = *self.names.get(varname).ok_or(format!(
                    "assignment to undefined variable {} at {}",
                    varname, target.position
                ))?;
                self.visit_expr(current_chunk, expr)?;

                *current_chunk += Opcode::Store(var_idx as u16); //TODO extension
            }
        }
        Ok(())
    }

    fn visit_expr(&mut self, current_chunk: &mut Chunk, expr: &Expr) -> Result<(), String> {
        match expr {
            Expr::Number(n) => {
                let n = *n;
                if n >= (i16::MIN as i64) && n <= (i16::MAX as i64) {
                    *current_chunk += Opcode::LoadImmediateInt(n as i16);
                } else {
                    let constant_index = current_chunk.constants.len();
                    current_chunk.constants.push(n);
                    *current_chunk += Opcode::LoadConst(constant_index as u16); //TODO extension
                }
            }
            Expr::Binary(op, a, b) => {
                self.visit_expr(current_chunk, a)?;
                self.visit_expr(current_chunk, b)?;
                *current_chunk += match op {
                    Op::Mul => Opcode::Mul,
                    Op::Div => Opcode::Div,
                    Op::Add => Opcode::Add,
                    Op::Sub => Opcode::Sub,
                }
            }

            Expr::Name(n) => match self.names.get(n.get_string().unwrap()) {
                Some(var_idx) => *current_chunk += Opcode::Load(*var_idx as u16), //TODO extension
                None => return Err(format!("undeclared variable {}", n.get_string().unwrap())),
            },
        }
        Ok(())
    }
}
