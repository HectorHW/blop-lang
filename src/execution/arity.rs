use std::{
    fmt::{Display, Formatter},
    ops::{Add, Sub},
};

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Arity {
    /// normal function/method that accepts `n` args
    Exact(usize),
    /// vararg function/method that accepts at least `n` args,
    /// additional args will be put inside vararg
    AtLeast(usize),
}

impl Arity {
    pub fn accepts(&self, args: usize) -> bool {
        match *self {
            Arity::Exact(i) => i == args,
            Arity::AtLeast(i) => args >= i,
        }
    }

    pub fn is_vararg(&self) -> bool {
        matches!(self, Arity::AtLeast(..))
    }

    pub fn get_number(&self) -> usize {
        match *self {
            Arity::Exact(n) => n,
            Arity::AtLeast(n) => n,
        }
    }
}

impl From<Arity> for usize {
    fn from(a: Arity) -> Self {
        match a {
            Arity::Exact(arity) => arity,
            Arity::AtLeast(arity) => arity,
        }
    }
}

impl Display for Arity {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Arity::Exact(e) => format!("{e} args"),
                Arity::AtLeast(args) => format!("at least {args} args"),
            }
        )
    }
}

impl Add<usize> for Arity {
    type Output = Arity;

    fn add(self, rhs: usize) -> Self::Output {
        match self {
            Arity::Exact(i) => Arity::Exact(i + rhs),
            Arity::AtLeast(i) => Arity::AtLeast(i + rhs),
        }
    }
}

impl Sub<usize> for Arity {
    type Output = Arity;

    fn sub(self, rhs: usize) -> Self::Output {
        match self {
            Arity::Exact(i) => Arity::Exact(i - rhs),
            Arity::AtLeast(i) => Arity::AtLeast(i - rhs),
        }
    }
}
