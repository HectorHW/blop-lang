use crate::compile::checks::tree_rewriter::Rewriter;
use crate::parsing::ast::{Program, Stmt};
use crate::parsing::lexer::Token;
use crate::Expr;

pub struct ExpressionLifter {}

impl ExpressionLifter {
    pub fn optimize(ast: Program) -> Result<Program, String> {
        let mut lifter = ExpressionLifter {};
        ast.into_iter().map(|s| lifter.visit_stmt(s)).collect()
    }
}

impl Rewriter<String> for ExpressionLifter {
    fn visit_expr_stmt(&mut self, expr: Expr) -> Result<Stmt, String> {
        match expr {
            Expr::SingleStatement(s) => self.visit_stmt(*s),
            any_other => Ok(Stmt::Expression(self.visit_expr(any_other)?)),
        }
    }

    fn visit_block(
        &mut self,
        start_token: Token,
        end_token: Token,
        mut containing_statements: Vec<Stmt>,
    ) -> Result<Expr, String> {
        if containing_statements.len() == 1 {
            let statement = containing_statements.remove(0);
            let e = Expr::SingleStatement(Box::new(statement));
            self.visit_expr(e)
        } else {
            let mut statements = vec![];
            for stmt in containing_statements {
                statements.push(self.visit_stmt(stmt)?);
            }

            Ok(Expr::Block(start_token, end_token, statements))
        }
    }

    fn visit_single_statement_expr(&mut self, stmt: Box<Stmt>) -> Result<Expr, String> {
        Ok(match *stmt {
            //singleStatement is artificial node representing block with single statement
            // do not alter declarations as they may lead to global assignments
            Stmt::Expression(e) => self.visit_expr(e)?,

            other => Expr::SingleStatement(Box::new(other)),
        })
    }
}
