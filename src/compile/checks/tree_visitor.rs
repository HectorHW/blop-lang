use crate::parsing::ast::{EnumVariant, Stmt, TypeMention, TypedName};
use crate::parsing::lexer::Token;
use crate::Expr;

#[allow(unused)]
pub(crate) trait Visitor<'ast, T, E>
where
    T: Default,
{
    fn visit_stmt(&mut self, stmt: &'ast Stmt) -> Result<T, E> {
        let obj = match stmt {
            Stmt::VarDeclaration(a, b) => self.visit_var_stmt(a, b.as_ref()),
            Stmt::Assignment(target, value) => self.visit_assignment_stmt(target, value),
            Stmt::Expression(e) => self.visit_expr_stmt(e),
            Stmt::Assert(keyword, value) => self.visit_assert_statement(keyword, value),
            Stmt::Pass(keyword) => self.visit_pass_stmt(keyword),
            Stmt::FunctionDeclaration {
                name,
                args,
                vararg,
                body,
                returns,
            } => self.visit_function_declaration_statement(
                name,
                args,
                vararg.as_ref(),
                body,
                returns.as_ref(),
            ),
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
            Stmt::Import {
                module,
                name,
                rename,
            } => self.visit_import_stmt(module, name, rename.as_ref()),
        }?;

        self.after_stmt(stmt, obj)
    }

    fn after_stmt(&mut self, stmt: &'ast Stmt, value: T) -> Result<T, E> {
        Ok(value)
    }

    fn visit_var_stmt(
        &mut self,
        _variable_name: &'ast TypedName,
        rhs: Option<&'ast Expr>,
    ) -> Result<T, E> {
        if rhs.is_some() {
            self.visit_expr(rhs.unwrap())?;
        }
        Ok(Default::default())
    }

    fn visit_assignment_stmt(&mut self, _target: &'ast Token, value: &'ast Expr) -> Result<T, E> {
        self.visit_expr(value)
    }

    fn visit_expr_stmt(&mut self, expr: &'ast Expr) -> Result<T, E> {
        self.visit_expr(expr)
    }

    fn visit_assert_statement(&mut self, _keyword: &'ast Token, expr: &'ast Expr) -> Result<T, E> {
        self.visit_expr(expr)
    }

    fn visit_pass_stmt(&mut self, _keyword: &'ast Token) -> Result<T, E> {
        Ok(Default::default())
    }

    fn visit_function_declaration_statement(
        &mut self,
        name: &'ast Token,
        args: &'ast [TypedName],
        vararg: Option<&'ast TypedName>,
        body: &'ast Expr,
        returns: Option<&'ast TypeMention>,
    ) -> Result<T, E> {
        self.visit_expr(body)
    }

    fn visit_method(
        &mut self,
        definiton_context: &'ast Token,
        name: &'ast Token,
        args: &'ast [TypedName],
        vararg: Option<&'ast TypedName>,
        body: &'ast Expr,
        returns: Option<&'ast TypeMention>,
    ) -> Result<T, E> {
        self.visit_function_declaration_statement(name, args, vararg, body, returns)
    }

    fn visit_struct_declaration_statement(
        &mut self,
        name: &'ast Token,
        fields: &[TypedName],
    ) -> Result<T, E> {
        Ok(Default::default())
    }

    fn visit_enum_declaration(
        &mut self,
        name: &'ast Token,
        variants: &'ast [EnumVariant],
    ) -> Result<T, E> {
        Ok(Default::default())
    }

    fn visit_property_assignment(&mut self, target: &'ast Expr, value: &'ast Expr) -> Result<T, E> {
        self.visit_expr(target)?;
        self.visit_expr(value)
    }

    fn visit_impl_block(
        &mut self,
        name: &'ast Token,
        implementations: &'ast [Stmt],
    ) -> Result<T, E> {
        implementations.iter().try_for_each(|f| match f {
            Stmt::FunctionDeclaration {
                name: f_name,
                args,
                vararg,
                body,
                returns,
            } => {
                self.visit_method(name, f_name, args, vararg.as_ref(), body, returns.as_ref())?;
                Ok(())
            }
            _ => unreachable!(),
        })?;

        Ok(Default::default())
    }

    fn visit_import_stmt(
        &mut self,
        module: &'ast [Token],
        name: &'ast Token,
        rename: Option<&'ast Token>,
    ) -> Result<T, E> {
        Ok(Default::default())
    }

    fn visit_expr(&mut self, expr: &'ast Expr) -> Result<T, E> {
        let value = match expr {
            Expr::Number(n) => self.visit_number_expr(n),
            Expr::FloatNumber(n) => self.visit_float_number_expr(n),
            Expr::Bool(b) => self.visit_bool_expr(b),
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
            Expr::AnonFunction(args, vararg, arrow, body) => {
                self.visit_anon_function_expr(args, vararg.as_ref(), arrow, body)
            }
            Expr::PropertyAccess(target, prop) => self.visit_property_access(target.as_ref(), prop),
            Expr::PropertyTest(target, prop) => self.visit_property_check(target.as_ref(), prop),
        }?;
        self.after_expr(expr, value)
    }

    fn after_expr(&mut self, expr: &'ast Expr, value: T) -> Result<T, E> {
        Ok(value)
    }

    fn visit_bool_expr(&mut self, token: &'ast Token) -> Result<T, E> {
        Ok(Default::default())
    }

    fn visit_number_expr(&mut self, token: &'ast Token) -> Result<T, E> {
        Ok(Default::default())
    }

    fn visit_float_number_expr(&mut self, token: &'ast Token) -> Result<T, E> {
        Ok(Default::default())
    }

    fn visit_variable_expr(&mut self, variable_name: &'ast Token) -> Result<T, E> {
        Ok(Default::default())
    }

    fn visit_string_expr(&mut self, string_literal: &'ast Token) -> Result<T, E> {
        Ok(Default::default())
    }

    fn visit_binary_expr(
        &mut self,
        op: &'ast Token,
        left: &'ast Expr,
        right: &'ast Expr,
    ) -> Result<T, E> {
        self.visit_expr(left)?;
        self.visit_expr(right)
    }

    fn visit_unary_expr(&mut self, op: &'ast Token, arg: &'ast Expr) -> Result<T, E> {
        self.visit_expr(arg)
    }

    fn visit_cond_expr(
        &mut self,
        condition: &'ast Expr,
        then_branch: &'ast Expr,
        else_branch: Option<&'ast Expr>,
    ) -> Result<T, E> {
        self.visit_expr(condition)?;
        self.visit_expr(then_branch)?;
        if else_branch.is_some() {
            self.visit_expr(else_branch.unwrap())?;
        }
        Ok(Default::default())
    }

    fn visit_block(
        &mut self,
        start_token: &'ast Token,
        end_token: &'ast Token,
        containing_statements: &'ast [Stmt],
    ) -> Result<T, E> {
        let mut res = vec![];
        for stmt in containing_statements {
            res.push(self.visit_stmt(stmt)?);
        }
        Ok(Default::default())
    }

    fn visit_single_statement_expr(&mut self, stmt: &'ast Stmt) -> Result<T, E> {
        self.visit_stmt(stmt)
    }

    fn visit_call_expr(&mut self, target: &'ast Expr, args: &'ast [Expr]) -> Result<T, E> {
        self.visit_expr(target)?;
        for arg in args {
            self.visit_expr(arg)?;
        }
        Ok(Default::default())
    }

    fn visit_partial_call_expr(
        &mut self,
        target: &'ast Expr,
        args: &'ast [Option<Expr>],
    ) -> Result<T, E> {
        self.visit_expr(target)?;
        for arg in args {
            if arg.is_some() {
                self.visit_expr(arg.as_ref().unwrap())?;
            }
        }
        Ok(Default::default())
    }

    fn visit_anon_function_expr(
        &mut self,
        args: &'ast [TypedName],
        vararg: Option<&'ast TypedName>,
        arrow: &'ast Token,
        body: &'ast Expr,
    ) -> Result<T, E> {
        self.visit_expr(body)?;
        Ok(Default::default())
    }

    fn visit_property_access(&mut self, target: &'ast Expr, property: &'ast Token) -> Result<T, E> {
        self.visit_expr(target)?;
        Ok(Default::default())
    }

    fn visit_property_check(&mut self, target: &'ast Expr, property: &'ast Token) -> Result<T, E> {
        self.visit_expr(target)?;
        Ok(Default::default())
    }
}
