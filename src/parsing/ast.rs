use crate::parsing::lexer::Token;
use std::hash::Hash;

use super::lexer::Index;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: Token,
    pub fields: Vec<TypedName>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypedName {
    pub name: Token,
    pub type_name: Option<TypeMention>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeMention(pub Token);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Stmt {
    VarDeclaration(TypedName, Option<Expr>),
    Assignment(Token, Expr),
    PropertyAssignment(Expr, Expr),
    Expression(Expr),
    Assert(Token, Expr),
    Pass(Token),
    FunctionDeclaration {
        name: Token,
        args: Vec<TypedName>,
        vararg: Option<TypedName>,
        body: Expr,
        returns: Option<TypeMention>,
    },
    StructDeclaration {
        name: Token,
        fields: Vec<TypedName>,
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

#[derive(Clone, Debug, PartialEq, Eq)]
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
    AnonFunction(Vec<TypedName>, Option<TypedName>, Token, Box<Expr>),
    PropertyAccess(Box<Expr>, Token),
    PropertyTest(Box<Expr>, Token),
}

impl Expr {
    pub fn get_pos(&self) -> Index {
        match self {
            Expr::Number(n)
            | Expr::FloatNumber(n)
            | Expr::Bool(n)
            | Expr::Name(n)
            | Expr::ConstString(n) => n.position,

            Expr::Binary(op, _, _) => op.position,
            Expr::Unary(op, _) => op.position,
            Expr::If(cond, _, _) => cond.get_pos(),
            Expr::Block(bb, _, _) => bb.position,
            Expr::SingleStatement(s) => s.get_pos(),
            Expr::Call(trg, _) => trg.get_pos(),
            Expr::PartialCall(trg, _) => trg.get_pos(),
            Expr::AnonFunction(_, _, arrow, _) => arrow.position,
            Expr::PropertyAccess(_, prop) => prop.position,
            Expr::PropertyTest(_, prop) => prop.position,
        }
    }
}

impl Stmt {
    pub fn get_pos(&self) -> Index {
        match self {
            Stmt::VarDeclaration(n, _) => n.name.position,
            Stmt::Assignment(n, _) => n.position,
            Stmt::PropertyAssignment(pr, _) => pr.get_pos(),
            Stmt::Expression(e) => e.get_pos(),
            Stmt::Assert(kw, _) => kw.position,
            Stmt::Pass(p) => p.position,
            Stmt::FunctionDeclaration { name, .. } => name.position,
            Stmt::StructDeclaration { name, fields } => name.position,
            Stmt::EnumDeclaration { name, variants } => name.position,
            Stmt::ImplBlock {
                name,
                implementations,
            } => name.position,
            Stmt::Import {
                module,
                name,
                rename,
            } => name.position,
        }
    }
}

pub type Program = Vec<Stmt>;

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for Expr {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Expr::Number(n)
            | Expr::FloatNumber(n)
            | Expr::Bool(n)
            | Expr::Name(n)
            | Expr::ConstString(n)
            | Expr::Binary(n, ..)
            | Expr::Unary(n, ..) => n.position.hash(state),

            Expr::Block(b, ..) => b.position.hash(state),

            Expr::If(cond, then, _) => {
                cond.hash(state);
                then.hash(state)
            }
            Expr::SingleStatement(s) => s.hash(state),
            Expr::Call(trg, args) => {
                trg.hash(state);
                for arg in args {
                    arg.hash(state)
                }
            }
            Expr::PartialCall(trg, ..) => trg.hash(state),
            Expr::AnonFunction(_, _, arrow, _) => arrow.hash(state),
            Expr::PropertyAccess(_, prop) => prop.hash(state),
            Expr::PropertyTest(_, prop) => prop.hash(state),
        }
    }
}

#[allow(clippy::derive_hash_xor_eq)]
impl Hash for Stmt {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            Stmt::VarDeclaration(v, _) => v.name.hash(state),
            Stmt::Assignment(trg, _) => trg.hash(state),
            Stmt::PropertyAssignment(trg, ..) => trg.hash(state),
            Stmt::Expression(e) => e.hash(state),
            Stmt::Assert(kw, _) => kw.hash(state),
            Stmt::Pass(p) => p.hash(state),
            Stmt::FunctionDeclaration { name, .. } => name.hash(state),
            Stmt::StructDeclaration { name, .. } => name.hash(state),
            Stmt::EnumDeclaration { name, .. } => name.hash(state),
            Stmt::ImplBlock { name, .. } => name.hash(state),
            Stmt::Import { name, .. } => name.hash(state),
        }
    }
}
