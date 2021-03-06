use crate::parsing::lexer::Token;

#[derive(Clone, Debug)]
pub struct EnumVariant {
    pub name: Token,
    pub fields: Vec<Token>,
}

#[derive(Clone, Debug)]
pub enum Stmt {
    VarDeclaration(Token, Option<Expr>),
    Assignment(Token, Expr),
    PropertyAssignment(Expr, Expr),
    Expression(Expr),
    Assert(Token, Expr),
    Pass(Token),
    FunctionDeclaration {
        name: Token,
        args: Vec<Token>,
        vararg: Option<Token>,
        body: Expr,
    },
    StructDeclaration {
        name: Token,
        fields: Vec<Token>,
    },

    EnumDeclaration {
        name: Token,
        variants: Vec<EnumVariant>,
    },

    ImplBlock {
        name: Token,
        implementations: Vec<Stmt>,
    },

    Import {
        module: Vec<Token>,
        name: Token,
        rename: Option<Token>,
    },
}

#[derive(Clone, Debug)]
pub enum Expr {
    Number(Token),
    FloatNumber(Token),
    Bool(Token),
    Name(Token),
    ConstString(Token),
    Binary(Token, Box<Expr>, Box<Expr>),
    Unary(Token, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Block(Token, Token, Vec<Stmt>),
    SingleStatement(Box<Stmt>),
    Call(Box<Expr>, Vec<Expr>),
    PartialCall(Box<Expr>, Vec<Option<Expr>>),
    AnonFunction(Vec<Token>, Option<Token>, Token, Box<Expr>),
    PropertyAccess(Box<Expr>, Token),
    PropertyTest(Box<Expr>, Token),
}

pub type Program = Vec<Stmt>;
