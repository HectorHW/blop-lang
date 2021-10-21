use crate::parsing::lexer::Token;

#[derive(Clone, Debug)]
pub enum Stmt {
    Print(Box<Expr>),
    VarDeclaration(Token, Option<Box<Expr>>),
    Assignment(Token, Box<Expr>),
    Expression(Box<Expr>),
    Assert(Token, Box<Expr>),
    FunctionDeclaration {
        name: Token,
        args: Vec<Token>,
        body: Box<Expr>,
    },
}

#[derive(Clone, Debug)]
pub enum Expr {
    Number(i64),
    Name(Token),
    Binary(Op, Box<Expr>, Box<Expr>),
    IfExpr(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Block(Vec<Stmt>),
    Call(Box<Expr>, Vec<Box<Expr>>),
}

#[derive(Copy, Clone, Debug)]
pub enum Op {
    Mul,
    Div,
    Add,
    Sub,

    TestEquals,
}

pub type Program = Vec<Stmt>;
