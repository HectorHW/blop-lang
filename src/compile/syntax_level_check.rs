use crate::parsing::ast::{Expr, Op, Program, Stmt};
use crate::parsing::lexer::Token;
use std::collections::HashSet;

struct Checker {
    names: Vec<HashSet<String>>,
    total_variables: usize,
}

pub fn check(program: &Program) -> Result<(), String> {
    let mut checker = Checker::new();
    checker.new_scope();
    for stmt in program {
        checker.visit_stmt(stmt)?;
    }
    Ok(())
}

impl Checker {
    fn new() -> Checker {
        Checker {
            names: vec![],
            total_variables: 0,
        }
    }

    fn lookup_local(&mut self, name: &Token) -> Result<(), String> {
        for scope in self.names.iter().rev() {
            if scope.contains(name.get_string().unwrap()) {
                return Ok(());
            }
        }

        Err(format!(
            "no variable {} found in scope [{}]",
            name.get_string().unwrap(),
            name.position
        ))
    }

    fn new_variable_slot(&mut self, variable_name: &Token) -> Result<(), String> {
        if self
            .names
            .last()
            .unwrap()
            .contains(variable_name.get_string().unwrap())
        {
            return Err(format!(
                "name {} already exists in current scope [{}]",
                variable_name.get_string().unwrap(),
                variable_name.position
            ));
        }
        self.total_variables += 1;
        self.names
            .last_mut()
            .unwrap()
            .insert(variable_name.get_string().unwrap().clone());
        Ok(())
    }

    fn new_scope(&mut self) {
        self.names.push(HashSet::new());
    }

    fn pop_scope(&mut self) {
        let scope = self.names.pop().unwrap();
        let items_in_scope = scope.len();
        drop(scope);
        self.total_variables -= items_in_scope;
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Print(e) => self.visit_expr(e),
            Stmt::VarDeclaration(name, body) => {
                body.as_ref()
                    .map(|e| self.visit_expr(e))
                    .unwrap_or(Ok(()))?;
                self.new_variable_slot(name)
            }
            Stmt::Assignment(target, expr) => {
                self.lookup_local(target)?;
                self.visit_expr(expr)
            }
            Stmt::Expression(e) => self.visit_expr(e),
            Stmt::Assert(_kw, e) => self.visit_expr(e),
            Stmt::FunctionDeclaration { name, args, body } => {
                self.check_function(name, args, body)?;
                self.new_variable_slot(name)
            }
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match expr {
            Expr::Number(_) => Ok(()),

            Expr::Name(n) => self.lookup_local(n),

            Expr::Binary(op, a, b) => {
                self.visit_expr(a)?;
                self.visit_expr(b)?;
                match op {
                    Op::Mul | Op::Div | Op::Add | Op::Sub | Op::TestEquals => Ok(()),
                    _ => Err(format!("cannot compile operator {:?}", op)),
                }
            }
            Expr::IfExpr(cond, then_body, else_body) => {
                self.visit_expr(cond)?;
                self.visit_expr(then_body)?;
                else_body
                    .as_ref()
                    .map(|x| self.visit_expr(x.as_ref()))
                    .unwrap_or(Ok(()))
            }
            Expr::Block(b) => self.visit_block(b),
            Expr::Call(target, args) => {
                self.visit_expr(target)?;
                for arg in args {
                    self.visit_expr(arg)?;
                }
                Ok(())
            }
            Expr::SingleStatement(s) => self.visit_stmt(s),
        }
    }

    fn check_function(&mut self, name: &Token, args: &[Token], body: &Expr) -> Result<(), String> {
        let mut scope_stack = vec![];
        std::mem::swap(&mut self.names, &mut scope_stack);
        let previous_total_variables = self.total_variables;
        self.total_variables = 0;

        self.new_scope();

        self.new_variable_slot(name)?; //define function inside itself
        for arg_name in args {
            self.new_variable_slot(arg_name)?;
        }
        self.visit_expr(body)?;

        std::mem::swap(&mut self.names, &mut scope_stack);
        self.total_variables = previous_total_variables;

        Ok(())
    }

    fn visit_block(&mut self, block: &[Stmt]) -> Result<(), String> {
        self.new_scope();

        for item in block {
            self.visit_stmt(item)?;
        }
        self.pop_scope();
        Ok(())
    }
}
