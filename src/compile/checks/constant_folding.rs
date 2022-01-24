use super::tree_visitor::Visitor;
use crate::parsing::ast::{Program, Stmt};
use crate::parsing::lexer::{Token, TokenKind};
use crate::Expr;

pub(super) struct Folder {}

impl Folder {
    pub fn fold_constants(program: Program) -> Result<Program, String> {
        let mut f = Folder {};
        f.visit_expr(program)
    }
}

impl Visitor<String> for Folder {
    fn visit_assert_statement(&mut self, keyword: Token, expr: Expr) -> Result<Stmt, String> {
        //do not touch asserts
        Ok(Stmt::Assert(keyword, expr))
    }

    fn visit_binary_expr(
        &mut self,
        op: Token,
        left: Box<Expr>,
        right: Box<Expr>,
    ) -> Result<Expr, String> {
        let left = self.visit_expr(*left)?;
        let right = self.visit_expr(*right)?;

        let res: Expr = match (&left, &right) {
            (Expr::Number(a), Expr::Number(b)) => {
                let na = a.get_number().unwrap();
                let nb = b.get_number().unwrap();

                enum FoldResult {
                    Ok(i64),
                    Warning(String),
                    Error(String),
                }

                let result = match &op.kind {
                    TokenKind::Star => FoldResult::Ok(na*nb),

                    TokenKind::Slash => match na.checked_div(nb) {
                        Some(result) => FoldResult::Ok(result),
                        None => {
                            FoldResult::Warning(format!("encountered zero division while folding constants, assuming it is intended [{}]", op.position))
                        }
                    },

                    TokenKind::Mod => match na.checked_rem(nb) {
                        Some(result) => FoldResult::Ok(result),
                        None => {
                            FoldResult::Warning( format!("encountered modulo 0 while folding constants, assuming it is intended [{}]", op.position))
                        }
                    },

                    TokenKind::Plus => FoldResult::Ok(na + nb),

                    TokenKind::Minus => FoldResult::Ok(na - nb),

                    TokenKind::CompareEquals => FoldResult::Ok(if na == nb { 1 } else { 0 }),

                    TokenKind::Power => match na.checked_pow(nb as u64 as u32) {
                        Some(value) => FoldResult::Ok(value),
                        None => FoldResult::Warning(format!("overflow when folding power at [{}]", op.position))
                    },

                    _any_other => FoldResult::Error(format!("unexpected binary operator {}", _any_other)),
                };

                match result {
                    FoldResult::Ok(number) => Expr::Number(Token {
                        position: a.position,
                        kind: TokenKind::Number(number),
                    }),
                    FoldResult::Warning(w) => {
                        eprintln!("{}", w);
                        Expr::Binary(op, Box::new(left), Box::new(right))
                    }
                    FoldResult::Error(e) => {
                        return Err(e);
                    }
                }
            }

            (Expr::ConstString(s1), Expr::ConstString(s2)) if op.kind == TokenKind::Plus => {
                let s = [s1.get_string().unwrap(), s2.get_string().unwrap()].join("");
                Expr::ConstString(Token {
                    position: s1.position,
                    kind: TokenKind::ConstString(s),
                })
            }

            _other_cases => Expr::Binary(op, Box::new(left), Box::new(right)),
        };

        Ok(res)
    }
}
