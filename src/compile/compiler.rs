use crate::data::objects::Value;
use crate::execution::chunk::{Chunk, Opcode};
use crate::parsing::ast::{Expr, Program, Stmt};
use crate::parsing::lexer::{Token, TokenKind};
use std::collections::HashMap;

pub struct Compiler {
    names: Vec<HashMap<String, usize>>,
    _needs_value: Vec<bool>,
    chunks: Vec<Chunk>,
    total_variables: usize,
    current_chunk_idx: usize,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            names: vec![],
            _needs_value: vec![],
            chunks: vec![],
            total_variables: 0,
            current_chunk_idx: 0,
        }
    }

    pub fn compile(program: &Program) -> Result<Vec<Chunk>, String> {
        let mut compiler = Compiler::new();

        compiler.chunks = vec![Chunk::new("<script>".to_string())];
        compiler.new_scope();

        compiler.require_nothing();
        for stmt in program {
            let code = compiler.visit_stmt(stmt)?;
            compiler.current_chunk().append(code);
        }
        compiler.pop_requirement();
        *compiler.current_chunk() += Opcode::Return;
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
        self.chunks.get_mut(self.current_chunk_idx).unwrap()
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
    }

    fn pop_scope(&mut self) -> usize {
        let scope = self.names.pop().unwrap();
        let items_in_scope = scope.len();
        drop(scope);
        self.total_variables -= items_in_scope;
        items_in_scope
    }

    fn compile_function(
        &mut self,
        name: &Token,
        args: &[Token],
        body: &Expr,
    ) -> Result<usize, String> {
        //save current compiler
        let mut scope_stack = vec![];
        std::mem::swap(&mut self.names, &mut scope_stack);

        let new_chunk_idx = self.chunks.len();
        self.chunks.push(Chunk::new(format!(
            "{} [{}]",
            name.get_string().unwrap(),
            name.position
        )));

        let previous_chunk_idx = self.current_chunk_idx;
        let previous_total_variables = self.total_variables;
        self.current_chunk_idx = new_chunk_idx;
        self.total_variables = 0;
        //compile body

        self.new_scope();

        self.new_variable_slot(name.get_string().unwrap()).unwrap();
        //define function inside itself

        for arg_name in args {
            match self.new_variable_slot(arg_name.get_string().unwrap()) {
                Some(_) => {}
                None => {
                    return Err(format!(
                        "arguement {} repeats in function {}",
                        arg_name.get_string().unwrap(),
                        name.get_string().unwrap()
                    ));
                }
            }
        }
        //self.new_variable_slot("_").unwrap(); // to store return value
        self.require_value();
        let code = self.visit_expr(body)?;
        self.pop_requirement();
        self.current_chunk().append(code);
        *self.current_chunk() += Opcode::Return;

        //load current compiler
        std::mem::swap(&mut self.names, &mut scope_stack);
        self.current_chunk_idx = previous_chunk_idx;
        self.total_variables = previous_total_variables;

        Ok(new_chunk_idx)
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

                result.push(Opcode::StoreLocal(var_idx as u16)); //TODO extension

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
                if self.needs_value() {
                    result.push(Opcode::LoadImmediateInt(0));
                }
            }
            Stmt::FunctionDeclaration { name, args, body } => {
                let new_chunk_idx = self.compile_function(name, args, body)?;

                self.new_variable_slot(name.get_string().unwrap())
                    .ok_or_else(|| {
                        format!("redifinition of name {}", name.get_string().unwrap())
                    })?;

                let const_idx = self.current_chunk().constants.len();
                self.current_chunk().constants.push(Value::Function {
                    chunk_id: new_chunk_idx,
                });

                result.push(Opcode::LoadConst(const_idx as u16)); //define name
                if self.needs_value() {
                    result.push(Opcode::LoadImmediateInt(0));
                }
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
                    self.current_chunk().constants.push(Value::Int(n));
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

                result.push(match &op.kind {
                    TokenKind::Star => Opcode::Mul,
                    TokenKind::Slash => Opcode::Div,
                    TokenKind::Plus => Opcode::Add,
                    TokenKind::Minus => Opcode::Sub,
                    TokenKind::TestEquals => Opcode::TestEquals,
                    other => panic!("unimplemented binary operator {} [{}]", other, op.position),
                });
                if !self.needs_value() {
                    result.push(Opcode::Pop(1));
                }
            }

            Expr::Name(n) => match self.lookup_local(n.get_string().unwrap()) {
                Some(var_idx) => {
                    result.push(Opcode::LoadLocal(var_idx as u16)); //TODO extension
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
                let body = self.visit_block(b)?;
                result = body;
            }
            Expr::Call(target, args) => {
                self.require_value();
                let mut target = self.visit_expr(target)?;
                result.append(&mut target);
                for arg in args {
                    result.append(&mut self.visit_expr(arg)?);
                }

                self.pop_requirement();
                result.push(Opcode::Call(args.len() as u16));

                if !self.needs_value() {
                    result.push(Opcode::Pop(1));
                }
            }
            Expr::SingleStatement(s) => {
                self.require_value();
                let body = self.visit_stmt(s)?;

                self.pop_requirement();
                result = body;
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
         */

        /*
        block with single statement
            stmt(return=return)
        */

        self.new_scope();

        let mut result = vec![];

        if self.needs_value() {
            self.new_variable_slot("_").unwrap();
            result.push(Opcode::LoadImmediateInt(0)); // _ variable
        }

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
            result.push(Opcode::StoreLocal(fictional_slot as u16));
            let scope_variable_count = self.pop_scope();
            result.push(Opcode::Pop((scope_variable_count - 1) as u16));
        } else {
            let scope_variable_count = self.pop_scope();
            result.push(Opcode::Pop(scope_variable_count as u16));
        }

        Ok(result)
    }
}
