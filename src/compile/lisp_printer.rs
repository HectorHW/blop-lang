use crate::parsing::ast::{Expr, Op, Stmt};
use crate::parsing::lexer::TokenKind;

pub fn print_as_sexp(s: &Stmt) -> String {
    visit_stmt(s)
}

fn visit_stmt(s: &Stmt) -> String {
    match s {
        Stmt::Print(e) => {
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
        Stmt::Assignment(target, expr) => {
            format!("(= {} {})", target.get_string().unwrap(), visit_expr(expr))
        }
        Stmt::Expression(e) => {
            format!("({}, pop)", visit_expr(e))
        }
    }
}

fn visit_block(block: &[Stmt]) -> String {
    let mut res = String::new();
    for item in block {
        res.push_str(&visit_stmt(item));
    }
    res
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
                    Op::TestEquals => {
                        "?="
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
        Expr::IfExpr(cond, then_body, else_body) => {
            format!(
                "(if {} {} {})",
                visit_expr(cond),
                visit_expr(then_body),
                else_body
                    .as_ref()
                    .map(|x| visit_expr(x.as_ref()))
                    .unwrap_or("0".to_string())
            )
        }
        Expr::Block(b) => visit_block(b),
    }
}
