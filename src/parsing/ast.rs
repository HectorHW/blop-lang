use crate::parsing::lexer::Token;

#[derive(Clone, Debug)]
pub enum Stmt {
    Print(Token, Expr),
    VarDeclaration(Token, Option<Expr>),
    Assignment(Token, Expr),
    Expression(Expr),
    Assert(Token, Expr),
    Pass(Token),
    FunctionDeclaration {
        name: Token,
        args: Vec<Token>,
        body: Expr,
    },
}

#[derive(Clone, Debug)]
pub enum Expr {
    Number(Token),
    Name(Token),
    ConstString(Token),
    Binary(Token, Box<Expr>, Box<Expr>),
    Unary(Token, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Block(Token, Token, Vec<Stmt>),
    SingleStatement(Box<Stmt>),
    Call(Box<Expr>, Vec<Expr>),
    PartialCall(Box<Expr>, Vec<Option<Expr>>),
    AnonFunction(Vec<Token>, Token, Box<Expr>),
}

pub type Program = Expr;
