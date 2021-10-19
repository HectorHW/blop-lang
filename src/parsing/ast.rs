use crate::parsing::lexer::Token;

#[derive(Clone, Debug)]
pub enum Stmt {
    Print(Box<Expr>),
    VarDeclaration(Token, Option<Box<Expr>>),
    Assignment(Token, Box<Expr>),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Number(i64),
    Name(Token),
    Binary(Op, Box<Expr>, Box<Expr>),
}

#[derive(Copy, Clone, Debug)]
pub enum Op {
    Mul,
    Div,
    Add,
    Sub,
}

pub type Program = Vec<Stmt>;
