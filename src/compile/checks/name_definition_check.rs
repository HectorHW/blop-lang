use crate::compile::checks::tree_visitor::Visitor;
use crate::parsing::ast::{Program, Stmt};
use crate::parsing::lexer::Token;
use crate::Expr;
use std::collections::HashMap;

/// checks that names and arguments do not repeat in same scope
pub struct NameRedefinitionChecker {
    scope: Vec<HashMap<String, Token>>,
}

impl NameRedefinitionChecker {
    pub fn check(ast: Program) -> Result<Program, String> {
        let mut checker = NameRedefinitionChecker { scope: vec![] };
        checker.visit_expr(ast)
    }

    fn new_scope(&mut self) {
        self.scope.push(HashMap::new())
    }

    fn pop_scope(&mut self) {
        self.scope.pop();
    }

    fn declare_name(&mut self, name: &Token) -> Result<(), Token> {
        let previous_def = self
            .scope
            .last_mut()
            .unwrap()
            .insert(name.get_string().unwrap().to_string(), name.clone());
        if let Some(token) = previous_def {
            Err(token)
        } else {
            Ok(())
        }
    }
}

impl Visitor<String> for NameRedefinitionChecker {
    fn visit_var_stmt(&mut self, name: Token, rhs: Option<Expr>) -> Result<Stmt, String> {
        let rhs = if let Some(rhs) = rhs {
            Some(self.visit_expr(rhs)?)
        } else {
            None
        };

        self.declare_name(&name).map_err(|e| {
            format!(
                "name {} [{}] is redefined in block, previous definition at [{}]",
                name.get_string().unwrap(),
                name.position,
                e.position
            )
        })?;

        Ok(Stmt::VarDeclaration(name, rhs))
    }

    fn visit_function_declaration_statement(
        &mut self,
        name: Token,
        args: Vec<Token>,
        body: Expr,
    ) -> Result<Stmt, String> {
        self.declare_name(&name).map_err(|e| {
            format!(
                "name {} [{}] is redefined in block, previous definition at [{}]",
                name.get_string().unwrap(),
                name.position,
                e.position
            )
        })?;

        self.new_scope();
        for arg_name in &args {
            self.declare_name(arg_name).map_err(|_e| {
                format!(
                    "argument {} repeats in function {} at [{}]",
                    arg_name.get_string().unwrap(),
                    name.get_string().unwrap(),
                    name.position
                )
            })?;
        }
        let body = self.visit_expr(body)?;
        self.pop_scope();
        Ok(Stmt::FunctionDeclaration { name, args, body })
    }

    fn visit_block(
        &mut self,
        start_token: Token,
        end_token: Token,
        containing_statements: Vec<Stmt>,
    ) -> Result<Expr, String> {
        let mut statements = vec![];
        self.new_scope();
        for stmt in containing_statements {
            statements.push(self.visit_stmt(stmt)?);
        }
        self.pop_scope();
        Ok(Expr::Block(start_token, end_token, statements))
    }

    fn visit_anon_function_expr(
        &mut self,
        args: Vec<Token>,
        arrow: Token,
        body: Box<Expr>,
    ) -> Result<Expr, String> {
        self.new_scope();

        for arg_name in &args {
            self.declare_name(arg_name).map_err(|_e| {
                format!(
                    "argument {} repeats in anonymous function at [{}]",
                    arg_name.get_string().unwrap(),
                    arrow.position
                )
            })?;
        }
        let body = Box::new(self.visit_expr(*body)?);
        Ok(Expr::AnonFunction(args, arrow, body))
    }
}
