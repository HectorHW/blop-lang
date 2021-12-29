use crate::parsing::lexer::Token;

#[derive(Clone, Debug)]
pub enum Stmt {
    Print(Token, Box<Expr>),
    VarDeclaration(Token, Option<Box<Expr>>),
    Assignment(Token, Box<Expr>),
    Expression(Box<Expr>),
    Assert(Token, Box<Expr>),
    Pass(Token),
    FunctionDeclaration {
        name: Token,
        args: Vec<Token>,
        body: Box<Expr>,
    },
}

#[derive(Clone, Debug)]
pub enum Expr {
    Number(Token),
    Name(Token),
    ConstString(Token),
    Binary(Token, Box<Expr>, Box<Expr>),
    Unary(Token, Box<Expr>),
    IfExpr(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Block(Token, Token, Vec<Stmt>),
    SingleStatement(Stmt),
    Call(Box<Expr>, Vec<Box<Expr>>),
    AnonFunction(Vec<Token>, Token, Box<Expr>),
}

pub type Program = Box<Expr>;
