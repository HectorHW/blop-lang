use crate::compile::checks::tree_visitor::Visitor;
use crate::parsing::ast::{Program, Stmt};
use crate::parsing::lexer::{Token, TokenKind};
use crate::Expr;

pub struct ExpressionLifter {}

impl ExpressionLifter {
    pub fn optimize(ast: Program) -> Result<Program, String> {
        let mut lifter = ExpressionLifter {};
        lifter.visit_expr(ast)
    }
}

impl Visitor<String> for ExpressionLifter {
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
            Stmt::Print(t, p) => Expr::SingleStatement(Box::new(Stmt::Print(t, p))),

            Stmt::VarDeclaration(_name, maybe_body) => {
                /*
                thing like

                if ...
                    var a = 2
                ...

                variable is not used anywhere else, can be substituted with expr
                */
                maybe_body.unwrap_or(Expr::Number(Token {
                    position: _name.position,
                    kind: TokenKind::Number(0),
                }))
            }

            a @ Stmt::Assignment(..) => Expr::SingleStatement(Box::new(a)),

            Stmt::Expression(e) => self.visit_expr(e)?,

            a @ Stmt::Assert(..) => Expr::SingleStatement(Box::new(a)),

            Stmt::FunctionDeclaration { name, .. } => {
                /*
                thing like
                if ...
                    def ... =
                        ...
                ...

                as def value is not used, it can be exluded from resulting program
                */
                Expr::Number(Token {
                    position: name.position,
                    kind: TokenKind::Number(0),
                })
                //Expr::SingleStatement(a)
            }

            p @ Stmt::Pass(..) => Expr::SingleStatement(Box::new(p)),
        })
    }
}
