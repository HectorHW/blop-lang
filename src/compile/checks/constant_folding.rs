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

                match op.kind {
                    TokenKind::Star => Expr::Number(Token {
                        position: a.position,
                        kind: TokenKind::Number(na * nb),
                    }),
                    TokenKind::Slash => match na.checked_div(nb) {
                        Some(result) => Expr::Number(Token {
                            position: a.position,
                            kind: TokenKind::Number(result),
                        }),
                        None => {
                            eprintln!("encountered zero division while folding constants, assuming it is intended [{}]", op.position);
                            Expr::Binary(op, Box::new(left), Box::new(right))
                        }
                    },

                    TokenKind::Mod => match na.checked_rem(nb) {
                        Some(result) => Expr::Number(Token {
                            position: a.position,
                            kind: TokenKind::Number(result),
                        }),
                        None => {
                            eprintln!("encountered modulo 0 while folding constants, assuming it is intended [{}]", op.position);
                            Expr::Binary(op, Box::new(left), Box::new(right))
                        }
                    },

                    TokenKind::Plus => Expr::Number(Token {
                        position: a.position,
                        kind: TokenKind::Number(na + nb),
                    }),
                    TokenKind::Minus => Expr::Number(Token {
                        position: a.position,
                        kind: TokenKind::Number(na - nb),
                    }),
                    TokenKind::CompareEquals => Expr::Number(Token {
                        position: a.position,
                        kind: TokenKind::Number(if na == nb { 1 } else { 0 }),
                    }),
                    TokenKind::Power => Expr::Number(Token {
                        position: a.position,
                        kind: TokenKind::Number(na.pow(nb as u64 as u32)),
                    }),
                    _any_other => panic!("unexpected binary operator {}", _any_other),
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
