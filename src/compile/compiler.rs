use crate::execution::chunk::{Chunk, Opcode};
use crate::parsing::ast::{Expr, Op, Program, Stmt};
use std::collections::HashMap;

pub struct Compiler {
    names: Vec<HashMap<String, usize>>,
    _needs_value: Vec<bool>,
    chunks: Vec<Chunk>,
    total_variables: usize,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            names: vec![],
            _needs_value: vec![],
            chunks: vec![],
            total_variables: 0,
        }
    }

    pub fn compile(program: &Program) -> Result<Vec<Chunk>, String> {
        let mut compiler = Compiler::new();

        compiler.chunks = vec![Chunk::new()];
        compiler.new_scope();
        compiler
            .chunks
            .last_mut()
            .unwrap()
            .code
            .push(Opcode::LoadImmediateInt(0)); // _ variable

        compiler.require_nothing();
        for stmt in program {
            let code = compiler.visit_stmt(stmt)?;
            compiler.chunks.last_mut().unwrap().append(code);
        }
        compiler.pop_requirement();
        Ok(compiler.chunks)
    }

    fn require_value(&mut self) {
        self._needs_value.push(true)
    }

    fn require_nothing(&mut self) {
        self._needs_value.push(false)
    }

    fn pop_requirement(&mut self) {
        self._needs_value.pop();
    }

    fn needs_value(&self) -> bool {
        *self._needs_value.last().unwrap_or(&false)
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        self.chunks.last_mut().unwrap()
    }

    fn lookup_local(&mut self, name: &str) -> Option<usize> {
        for scope in self.names.iter().rev() {
            if scope.contains_key(name) {
                return Some(*scope.get(name).unwrap());
            }
        }
        None
    }

    fn new_variable_slot(&mut self, variable_name: &str) -> Option<usize> {
        if self.names.last().unwrap().contains_key(variable_name) {
            return None;
        }
        let var_index = self.total_variables;
        self.total_variables += 1;
        self.names
            .last_mut()
            .unwrap()
            .insert(variable_name.to_string(), var_index);
        Some(var_index)
    }

    fn new_scope(&mut self) {
        self.names.push(HashMap::new());
        /*self.names
        .last_mut()
        .unwrap()
        .insert("_".to_string(), self.total_variables);*/
        self.new_variable_slot("_");
        //self.total_variables += 1;
    }

    fn pop_scope(&mut self) -> usize {
        let scope = self.names.pop().unwrap();
        let items_in_scope = scope.len();
        drop(scope);
        self.total_variables -= items_in_scope;
        items_in_scope
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<Vec<Opcode>, String> {
        let mut result = vec![];
        match stmt {
            Stmt::Print(e) => {
                self.require_value();
                let mut compiled_expression = self.visit_expr(e)?;
                self.pop_requirement();
                result.append(&mut compiled_expression);
                result.push(Opcode::Print);

                if self.needs_value() {
                    result.push(Opcode::LoadImmediateInt(0));
                }
            }

            Stmt::VarDeclaration(n, e) => {
                if e.is_none() {
                    result.push(Opcode::LoadImmediateInt(0));
                } else {
                    self.require_value();
                    let mut assignment_body = self.visit_expr(e.as_ref().unwrap())?;
                    self.pop_requirement();
                    result.append(&mut assignment_body);

                    let varname = n.get_string().unwrap();
                    let _ = self
                        .new_variable_slot(varname)
                        .ok_or(format!("redefinition of variable {}", varname))?;
                }

                if self.needs_value() {
                    result.push(Opcode::LoadImmediateInt(0)) //TODO dup?
                }
            }

            Stmt::Assignment(target, expr) => {
                let varname = target.get_string().unwrap();

                let var_idx = self.lookup_local(varname).ok_or(format!(
                    "assignment to undefined variable {} at {}",
                    varname, target.position
                ))?;

                self.require_value();
                let mut expr_body = self.visit_expr(expr)?;
                self.pop_requirement();

                result.append(&mut expr_body);

                result.push(Opcode::Store(var_idx as u16)); //TODO extension

                if self.needs_value() {
                    result.push(Opcode::LoadImmediateInt(0));
                }
            }
            Stmt::Expression(e) => {
                let mut body = self.visit_expr(e)?;
                result.append(&mut body);
            }
            Stmt::Assert(_token, expr) => {
                self.require_value();
                let mut body = self.visit_expr(expr)?;
                self.pop_requirement();
                result.append(&mut body);
                result.push(Opcode::Assert);
            }
        }
        Ok(result)
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<Vec<Opcode>, String> {
        let mut result = vec![];
        match expr {
            Expr::Number(n) => {
                let n = *n;
                if n >= (i16::MIN as i64) && n <= (i16::MAX as i64) {
                    result.push(Opcode::LoadImmediateInt(n as i16));
                } else {
                    let constant_index = self.current_chunk().constants.len();
                    self.current_chunk().constants.push(n);
                    result.push(Opcode::LoadConst(constant_index as u16)); //TODO extension
                }
                if !self.needs_value() {
                    result.push(Opcode::Pop(1));
                }
            }

            Expr::Binary(op, a, b) => {
                self.require_value();
                let mut a = self.visit_expr(a)?;
                let mut b = self.visit_expr(b)?;
                self.pop_requirement();

                result.append(&mut a);
                result.append(&mut b);

                result.push(match op {
                    Op::Mul => Opcode::Mul,
                    Op::Div => Opcode::Div,
                    Op::Add => Opcode::Add,
                    Op::Sub => Opcode::Sub,
                    Op::TestEquals => Opcode::TestEquals,
                });
                if !self.needs_value() {
                    result.push(Opcode::Pop(1));
                }
            }

            Expr::Name(n) => match self.lookup_local(n.get_string().unwrap()) {
                Some(var_idx) => {
                    result.push(Opcode::Load(var_idx as u16)); //TODO extension
                    if !self.needs_value() {
                        result.push(Opcode::Pop(1));
                    }
                }
                None => return Err(format!("undeclared variable {}", n.get_string().unwrap())),
            },

            Expr::IfExpr(cond, then_body, else_body) => {
                self.require_value();
                let mut condition = self.visit_expr(cond)?;
                self.pop_requirement();

                let mut then_body = self.visit_expr(then_body)?;

                let mut else_body = else_body
                    .as_ref()
                    .map(|x| self.visit_expr(x.as_ref()))
                    .unwrap_or_else(|| {
                        Ok(if self.needs_value() {
                            vec![Opcode::LoadImmediateInt(0)]
                        } else {
                            vec![Opcode::Nop]
                        })
                    })?;

                result.append(&mut condition);

                let then_body_size = then_body.len();
                let else_body_size = else_body.len();

                result.push(Opcode::JumpIfFalse((then_body_size + 1 + 1) as u16));
                //instruction AFTER then_body and jump

                result.append(&mut then_body);
                result.push(Opcode::Jump((else_body_size + 1) as u16));

                result.append(&mut else_body);
                result.push(Opcode::Nop);
            }
            Expr::Block(b) => {
                let mut body = self.visit_block(b)?;
                result.append(&mut body);
            }
        }
        Ok(result)
    }

    fn visit_block(&mut self, block: &[Stmt]) -> Result<Vec<Opcode>, String> {
        /*
        block that does not return value:
            stmt(return=false)
            ...
            stmt(return=false)
            pop(n) //locals
        */

        /*
        block that returns value:
            stmt(return=false)
            ...
            stmt(return=true)
            _ = pop(1)
            pop(n)
            push(_)
         */

        let mut result = vec![];

        self.new_scope();
        result.push(Opcode::LoadImmediateInt(0)); // _ variable

        let (last_statement, other_statements) = block.split_last().ok_or("got empty block")?;

        self.require_nothing();
        for item in other_statements {
            let mut stmt_code = self.visit_stmt(item)?;
            result.append(&mut stmt_code);
        }
        self.pop_requirement();

        let mut last_statement = self.visit_stmt(last_statement)?;
        result.append(&mut last_statement);
        if self.needs_value() {
            let fictional_slot = self.lookup_local("_").unwrap();
            result.push(Opcode::Store(fictional_slot as u16));
            let scope_variable_count = self.pop_scope();
            result.push(Opcode::Pop((scope_variable_count - 1) as u16));
        } else {
            let scope_variable_count = self.pop_scope();
            result.push(Opcode::Pop(scope_variable_count as u16));
        }

        Ok(result)
    }
}
