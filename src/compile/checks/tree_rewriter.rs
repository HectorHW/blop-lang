#![allow(clippy::boxed_local)]

use crate::parsing::ast::{EnumVariant, Stmt};
use crate::parsing::lexer::Token;
use crate::Expr;

pub(super) trait Rewriter<E> {
    fn visit_stmt(&mut self, stmt: Stmt) -> Result<Stmt, E> {
        match stmt {
            Stmt::VarDeclaration(a, b) => self.visit_var_stmt(a, b),
            Stmt::Assignment(target, value) => self.visit_assignment_stmt(target, value),
            Stmt::Expression(e) => self.visit_expr_stmt(e),
            Stmt::Assert(keyword, value) => self.visit_assert_statement(keyword, value),
            Stmt::Pass(keyword) => self.visit_pass_stmt(keyword),
            Stmt::FunctionDeclaration {
                name,
                args,
                vararg,
                body,
            } => self.visit_function_declaration_statement(name, args, vararg, body),

            Stmt::StructDeclaration { name, fields } => {
                self.visit_struct_declaration_statement(name, fields)
            }

            Stmt::EnumDeclaration { name, variants } => self.visit_enum_declaration(name, variants),

            Stmt::PropertyAssignment(target, value) => {
                self.visit_property_assignment(target, value)
            }

            Stmt::ImplBlock {
                name,
                implementations,
            } => self.visit_impl_block(name, implementations),
        }
    }

    fn visit_var_stmt(&mut self, name: Token, rhs: Option<Expr>) -> Result<Stmt, E> {
        Ok(Stmt::VarDeclaration(
            name,
            if rhs.is_some() {
                Some(self.visit_expr(rhs.unwrap())?)
            } else {
                None
            },
        ))
    }

    fn visit_assignment_stmt(&mut self, target: Token, value: Expr) -> Result<Stmt, E> {
        Ok(Stmt::Assignment(target, self.visit_expr(value)?))
    }

    fn visit_expr_stmt(&mut self, expr: Expr) -> Result<Stmt, E> {
        Ok(Stmt::Expression(self.visit_expr(expr)?))
    }

    fn visit_assert_statement(&mut self, keyword: Token, expr: Expr) -> Result<Stmt, E> {
        Ok(Stmt::Assert(keyword, self.visit_expr(expr)?))
    }

    fn visit_pass_stmt(&mut self, keyword: Token) -> Result<Stmt, E> {
        Ok(Stmt::Pass(keyword))
    }

    fn visit_function_declaration_statement(
        &mut self,
        name: Token,
        args: Vec<Token>,
        vararg: Option<Token>,
        body: Expr,
    ) -> Result<Stmt, E> {
        Ok(Stmt::FunctionDeclaration {
            name,
            args,
            vararg,
            body: self.visit_expr(body)?,
        })
    }

    fn visit_struct_declaration_statement(
        &mut self,
        name: Token,
        fields: Vec<Token>,
    ) -> Result<Stmt, E> {
        Ok(Stmt::StructDeclaration { name, fields })
    }

    fn visit_enum_declaration(
        &mut self,
        name: Token,
        variants: Vec<EnumVariant>,
    ) -> Result<Stmt, E> {
        Ok(Stmt::EnumDeclaration { name, variants })
    }

    fn visit_property_assignment(&mut self, target: Expr, value: Expr) -> Result<Stmt, E> {
        Ok(Stmt::PropertyAssignment(
            self.visit_expr(target)?,
            self.visit_expr(value)?,
        ))
    }

    fn visit_impl_block(&mut self, name: Token, functions: Vec<Stmt>) -> Result<Stmt, E> {
        let functions = functions
            .into_iter()
            .map(|f| self.visit_stmt(f))
            .collect::<Result<Vec<Stmt>, E>>()?;

        Ok(Stmt::ImplBlock {
            name,
            implementations: functions,
        })
    }

    fn visit_expr(&mut self, expr: Expr) -> Result<Expr, E> {
        match expr {
            Expr::Number(n) => self.visit_number_expr(n),
            Expr::Name(n) => self.visit_variable_expr(n),
            Expr::ConstString(s) => self.visit_string_expr(s),
            Expr::Binary(op, a, b) => self.visit_binary_expr(op, a, b),
            Expr::Unary(op, a) => self.visit_unary_expr(op, a),
            Expr::If(cond, then_branch, else_branch) => {
                self.visit_cond_expr(cond, then_branch, else_branch)
            }
            Expr::Block(start, end, containing) => self.visit_block(start, end, containing),
            Expr::SingleStatement(s) => self.visit_single_statement_expr(s),
            Expr::Call(target, args) => self.visit_call_expr(target, args),
            Expr::PartialCall(target, args) => self.visit_partial_call_expr(target, args),
            Expr::AnonFunction(args, vararg, arrow, body) => {
                self.visit_anon_function_expr(args, vararg, arrow, body)
            }
            Expr::PropertyAccess(target, prop) => self.visit_property_access(target, prop),
            Expr::PropertyTest(target, prop) => self.visit_property_check(target, prop),
        }
    }

    fn visit_number_expr(&mut self, token: Token) -> Result<Expr, E> {
        Ok(Expr::Number(token))
    }

    fn visit_variable_expr(&mut self, variable_name: Token) -> Result<Expr, E> {
        Ok(Expr::Name(variable_name))
    }

    fn visit_string_expr(&mut self, string_literal: Token) -> Result<Expr, E> {
        Ok(Expr::ConstString(string_literal))
    }

    fn visit_binary_expr(
        &mut self,
        op: Token,
        left: Box<Expr>,
        right: Box<Expr>,
    ) -> Result<Expr, E> {
        let left = self.visit_expr(*left)?;
        let right = self.visit_expr(*right)?;
        Ok(Expr::Binary(op, Box::new(left), Box::new(right)))
    }

    fn visit_unary_expr(&mut self, op: Token, arg: Box<Expr>) -> Result<Expr, E> {
        Ok(Expr::Unary(op, Box::new(self.visit_expr(*arg)?)))
    }

    fn visit_cond_expr(
        &mut self,
        condition: Box<Expr>,
        then_branch: Box<Expr>,
        else_branch: Option<Box<Expr>>,
    ) -> Result<Expr, E> {
        let condition = Box::new(self.visit_expr(*condition)?);
        let then_branch = Box::new(self.visit_expr(*then_branch)?);
        let else_branch = if else_branch.is_some() {
            Some(Box::new(self.visit_expr(*else_branch.unwrap())?))
        } else {
            None
        };

        Ok(Expr::If(condition, then_branch, else_branch))
    }

    fn visit_block(
        &mut self,
        start_token: Token,
        end_token: Token,
        containing_statements: Vec<Stmt>,
    ) -> Result<Expr, E> {
        let mut res = vec![];
        for stmt in containing_statements {
            res.push(self.visit_stmt(stmt)?);
        }
        Ok(Expr::Block(start_token, end_token, res))
    }

    fn visit_single_statement_expr(&mut self, stmt: Box<Stmt>) -> Result<Expr, E> {
        let containing = self.visit_stmt(*stmt)?;
        Ok(Expr::SingleStatement(Box::new(containing)))
    }

    fn visit_call_expr(&mut self, target: Box<Expr>, args: Vec<Expr>) -> Result<Expr, E> {
        let target = Box::new(self.visit_expr(*target)?);
        let mut processed_args = vec![];
        for arg in args {
            processed_args.push(self.visit_expr(arg)?);
        }
        Ok(Expr::Call(target, processed_args))
    }

    fn visit_partial_call_expr(
        &mut self,
        target: Box<Expr>,
        args: Vec<Option<Expr>>,
    ) -> Result<Expr, E> {
        let target = Box::new(self.visit_expr(*target)?);
        let mut processed_args = vec![];
        for arg in args {
            processed_args.push(if arg.is_some() {
                Some(self.visit_expr(arg.unwrap())?)
            } else {
                None
            });
        }
        Ok(Expr::PartialCall(target, processed_args))
    }

    fn visit_property_access(&mut self, target: Box<Expr>, property: Token) -> Result<Expr, E> {
        let target = Box::new(self.visit_expr(*target)?);
        Ok(Expr::PropertyAccess(target, property))
    }

    fn visit_property_check(&mut self, target: Box<Expr>, property: Token) -> Result<Expr, E> {
        let target = Box::new(self.visit_expr(*target)?);
        Ok(Expr::PropertyTest(target, property))
    }

    fn visit_anon_function_expr(
        &mut self,
        args: Vec<Token>,
        vararg: Option<Token>,
        arrow: Token,
        body: Box<Expr>,
    ) -> Result<Expr, E> {
        let body = Box::new(self.visit_expr(*body)?);
        Ok(Expr::AnonFunction(args, vararg, arrow, body))
    }
}
