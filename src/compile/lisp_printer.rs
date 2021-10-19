use crate::parsing::ast::{Expr, Op, Stmt};
use crate::parsing::lexer::TokenKind;


pub fn print_as_sexp(s: &Stmt) -> String {
    visit_stmt(s)
}

fn visit_stmt(s: &Stmt) -> String {
    match s {
        Stmt::PrintStmt(e) => {
            format!("(print {})", visit_expr(e))
        }
        Stmt::VarDeclaration(name, expr) => {
            format!(
                "(var_decl {} {})",
                name.kind,
                expr.as_ref()
                    .map(|e| { visit_expr(e) })
                    .unwrap_or_else(|| "nil".to_string())
            )
        }
    }
}

fn visit_expr(e: &Expr) -> String {
    match e {
        Expr::Number(n) => {
            format!("{}", n)
        }
        Expr::Binary(op, a, b) => {
            let s1 = visit_expr(a);
            let s2 = visit_expr(b);
            format!(
                "({} {} {})",
                match op {
                    Op::Mul => {
                        "*"
                    }
                    Op::Div => {
                        "/"
                    }
                    Op::Add => {
                        "+"
                    }
                    Op::Sub => {
                        "-"
                    }
                },
                s1,
                s2
            )
        }
        Expr::Name(n) => match &n.kind {
            TokenKind::Name(n) => n.clone(),
            _ => panic!(),
        },
    }
}
