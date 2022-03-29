use crate::parsing::ast::Stmt;
use crate::parsing::lexer::Token;
use crate::Expr;

#[allow(unused)]
pub(super) trait Visitor<E> {
    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), E> {
        match stmt {
            Stmt::Print(a, b) => self.visit_print_stmt(a, b),
            Stmt::VarDeclaration(a, b) => self.visit_var_stmt(a, b.as_ref()),
            Stmt::Assignment(target, value) => self.visit_assignment_stmt(target, value),
            Stmt::Expression(e) => self.visit_expr_stmt(e),
            Stmt::Assert(keyword, value) => self.visit_assert_statement(keyword, value),
            Stmt::Pass(keyword) => self.visit_pass_stmt(keyword),
            Stmt::FunctionDeclaration { name, args, body } => {
                self.visit_function_declaration_statement(name, args, body)
            }
            Stmt::StructDeclaration { name, fields } => {
                self.visit_struct_declaration_statement(name, fields)
            }
            Stmt::PropertyAssignment(target, value) => {
                self.visit_property_assignment(target, value)
            }

            Stmt::ImplBlock {
                name,
                implementations,
            } => self.visit_impl_block(name, implementations),
        }
    }

    fn visit_print_stmt(&mut self, _print_keyword: &Token, e: &Expr) -> Result<(), E> {
        self.visit_expr(e)
    }

    fn visit_var_stmt(&mut self, _variable_name: &Token, rhs: Option<&Expr>) -> Result<(), E> {
        if rhs.is_some() {
            self.visit_expr(rhs.unwrap())?;
        }
        Ok(())
    }

    fn visit_assignment_stmt(&mut self, _target: &Token, value: &Expr) -> Result<(), E> {
        self.visit_expr(value)
    }

    fn visit_expr_stmt(&mut self, expr: &Expr) -> Result<(), E> {
        self.visit_expr(expr)
    }

    fn visit_assert_statement(&mut self, _keyword: &Token, expr: &Expr) -> Result<(), E> {
        self.visit_expr(expr)
    }

    fn visit_pass_stmt(&mut self, _keyword: &Token) -> Result<(), E> {
        Ok(())
    }

    fn visit_function_declaration_statement(
        &mut self,
        name: &Token,
        args: &[Token],
        body: &Expr,
    ) -> Result<(), E> {
        self.visit_expr(body)
    }

    fn visit_method(&mut self, name: &Token, args: &[Token], body: &Expr) -> Result<(), E> {
        self.visit_function_declaration_statement(name, args, body)
    }

    fn visit_struct_declaration_statement(
        &mut self,
        name: &Token,
        fields: &[Token],
    ) -> Result<(), E> {
        Ok(())
    }

    fn visit_property_assignment(&mut self, target: &Expr, value: &Expr) -> Result<(), E> {
        self.visit_expr(target)?;
        self.visit_expr(value)
    }

    fn visit_impl_block(&mut self, name: &Token, implementations: &[Stmt]) -> Result<(), E> {
        implementations
            .iter()
            .try_for_each(|f| self.visit_stmt(f))?;

        Ok(())
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<(), E> {
        match expr {
            Expr::Number(n) => self.visit_number_expr(n),
            Expr::Name(n) => self.visit_variable_expr(n),
            Expr::ConstString(s) => self.visit_string_expr(s),
            Expr::Binary(op, a, b) => self.visit_binary_expr(op, a, b),
            Expr::Unary(op, a) => self.visit_unary_expr(op, a),
            Expr::If(cond, then_branch, else_branch) => {
                self.visit_cond_expr(cond, then_branch, else_branch.as_deref())
            }

            Expr::Block(start, end, containing) => self.visit_block(start, end, containing),
            Expr::SingleStatement(s) => self.visit_single_statement_expr(s),
            Expr::Call(target, args) => self.visit_call_expr(target, args),
            Expr::PartialCall(target, args) => self.visit_partial_call_expr(target, args),
            Expr::AnonFunction(args, arrow, body) => {
                self.visit_anon_function_expr(args, arrow, body)
            }
            Expr::PropertyAccess(target, prop) => self.visit_property_access(target.as_ref(), prop),
            Expr::PropertyTest(target, prop) => self.visit_property_check(target.as_ref(), prop),
        }
    }

    fn visit_number_expr(&mut self, token: &Token) -> Result<(), E> {
        Ok(())
    }

    fn visit_variable_expr(&mut self, variable_name: &Token) -> Result<(), E> {
        Ok(())
    }

    fn visit_string_expr(&mut self, string_literal: &Token) -> Result<(), E> {
        Ok(())
    }

    fn visit_binary_expr(&mut self, op: &Token, left: &Expr, right: &Expr) -> Result<(), E> {
        self.visit_expr(left)?;
        self.visit_expr(right)
    }

    fn visit_unary_expr(&mut self, op: &Token, arg: &Expr) -> Result<(), E> {
        self.visit_expr(arg)
    }

    fn visit_cond_expr(
        &mut self,
        condition: &Expr,
        then_branch: &Expr,
        else_branch: Option<&Expr>,
    ) -> Result<(), E> {
        self.visit_expr(condition)?;
        self.visit_expr(then_branch)?;
        if else_branch.is_some() {
            self.visit_expr(else_branch.unwrap())?;
        }
        Ok(())
    }

    fn visit_block(
        &mut self,
        start_token: &Token,
        end_token: &Token,
        containing_statements: &[Stmt],
    ) -> Result<(), E> {
        let mut res = vec![];
        for stmt in containing_statements {
            res.push(self.visit_stmt(stmt)?);
        }
        Ok(())
    }

    fn visit_single_statement_expr(&mut self, stmt: &Stmt) -> Result<(), E> {
        self.visit_stmt(stmt)
    }

    fn visit_call_expr(&mut self, target: &Expr, args: &[Expr]) -> Result<(), E> {
        self.visit_expr(target)?;
        for arg in args {
            self.visit_expr(arg)?;
        }
        Ok(())
    }

    fn visit_partial_call_expr(&mut self, target: &Expr, args: &[Option<Expr>]) -> Result<(), E> {
        self.visit_expr(target)?;
        for arg in args {
            if arg.is_some() {
                self.visit_expr(arg.as_ref().unwrap())?;
            }
        }
        Ok(())
    }

    fn visit_anon_function_expr(
        &mut self,
        args: &[Token],
        arrow: &Token,
        body: &Expr,
    ) -> Result<(), E> {
        self.visit_expr(body)?;
        Ok(())
    }

    fn visit_property_access(&mut self, target: &Expr, property: &Token) -> Result<(), E> {
        self.visit_expr(target)?;
        Ok(())
    }

    fn visit_property_check(&mut self, target: &Expr, property: &Token) -> Result<(), E> {
        self.visit_expr(target)?;
        Ok(())
    }
}
