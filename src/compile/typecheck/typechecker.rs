use super::types::Type;
use crate::compile::checks::tree_visitor::Visitor;
use crate::compile::checks::Annotations;
use crate::execution::arity::Arity;
use crate::parsing::ast::{Expr, Program, Stmt};
use crate::parsing::lexer::{Index, Token, TokenKind};
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug)]
pub enum SomewhereTypeError {
    TypeMismatch { expected: Type, got: Type },
    UnspecifiedBinary { left: Type, op: Token, right: Type },

    ArityMismatch { expected: Arity, got: usize },

    AttributeGetError { target_type: Type, field: String },
}

#[derive(Debug)]
pub struct LocalizedError {
    error: SomewhereTypeError,
    position: Index,
}

impl From<LocalizedError> for TypeError {
    fn from(e: LocalizedError) -> Self {
        TypeError::LocalizedError(e)
    }
}

impl From<SomewhereTypeError> for TypeError {
    fn from(e: SomewhereTypeError) -> Self {
        TypeError::Somewhere(e)
    }
}

impl SomewhereTypeError {
    pub fn at(self, position: Index) -> LocalizedError {
        LocalizedError {
            error: self,
            position,
        }
    }
}

#[derive(Debug)]
pub enum TypeError {
    Somewhere(SomewhereTypeError),
    LocalizedError(LocalizedError),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Typable<'a> {
    Expr(&'a Expr),
    Stmt(&'a Stmt),
}

impl<'a> From<&'a Expr> for Typable<'a> {
    fn from(e: &'a Expr) -> Self {
        Typable::Expr(e)
    }
}

impl<'a> From<&'a Stmt> for Typable<'a> {
    fn from(s: &'a Stmt) -> Self {
        Typable::Stmt(s)
    }
}

#[derive(Clone, Debug, Default)]
pub struct Typemap<'a>(HashMap<Typable<'a>, Type>);

impl<'a> Typemap<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn type_of(&self, obj: Typable) -> Type {
        self.0.get(&obj).cloned().unwrap_or_default()
    }

    pub(super) fn add_expr(&mut self, expr: &'a Expr, expr_type: Type) {
        self.0.insert(Typable::Expr(expr), expr_type);
    }

    pub(super) fn add_stmt(&mut self, stmt: &'a Stmt, stmt_type: Type) {
        self.0.insert(Typable::Stmt(stmt), stmt_type);
    }
}

pub struct Checker<'an, 'ast> {
    annotations: &'an Annotations,
    scopes: Vec<HashMap<String, Type>>,
    type_map: Typemap<'ast>,
}

impl<'a, 'ast> Checker<'a, 'ast> {
    pub fn typecheck(
        program: &'ast Program,
        annotations: &'a Annotations,
    ) -> Result<Typemap<'ast>, TypeError> {
        let mut checker = Checker::new(annotations);

        for stmt in program {
            checker.visit_stmt(stmt)?;
        }

        Ok(checker.type_map)
    }

    pub fn new(annotations: &'a Annotations) -> Checker<'a, 'ast> {
        Self {
            annotations,
            scopes: Default::default(),
            type_map: Default::default(),
        }
    }
}

impl<'a, 'ast> Visitor<'ast, Type, TypeError> for Checker<'a, 'ast> {
    fn after_expr(&mut self, expr: &'ast Expr, value: Type) -> Result<Type, TypeError> {
        self.type_map.add_expr(expr, value.clone());
        Ok(value)
    }

    fn after_stmt(&mut self, stmt: &'ast Stmt, value: Type) -> Result<Type, TypeError> {
        self.type_map.add_stmt(stmt, value.clone());
        Ok(value)
    }

    fn visit_float_number_expr(&mut self, _token: &Token) -> Result<Type, TypeError> {
        Ok(Type::Float)
    }

    fn visit_number_expr(&mut self, _token: &Token) -> Result<Type, TypeError> {
        Ok(Type::Int)
    }

    fn visit_bool_expr(&mut self, _token: &Token) -> Result<Type, TypeError> {
        Ok(Type::Bool)
    }

    fn visit_string_expr(&mut self, string_literal: &'ast Token) -> Result<Type, TypeError> {
        Ok(Type::String)
    }

    fn visit_cond_expr(
        &mut self,
        condition: &'ast Expr,
        then_branch: &'ast Expr,
        else_branch: Option<&'ast Expr>,
    ) -> Result<Type, TypeError> {
        let condition_t = self.visit_expr(condition)?;
        if !&Type::Bool.le(&condition_t) {
            return Err(SomewhereTypeError::TypeMismatch {
                expected: Type::Bool,
                got: condition_t,
            }
            .at(condition.get_pos())
            .into());
        }

        let left = self.visit_expr(then_branch)?;
        let right = if let Some(else_branch) = else_branch {
            self.visit_expr(else_branch)?
        } else {
            Type::Nothing
        };

        Ok(Type::build_union(left, right))
    }

    fn visit_binary_expr(
        &mut self,
        op: &Token,
        left: &'ast Expr,
        right: &'ast Expr,
    ) -> Result<Type, TypeError> {
        let left = self.visit_expr(left)?;
        let right = self.visit_expr(right)?;

        use TokenKind::*;

        macro_rules! num {
            ($e: pat) => {
                (_, $e, _)
            };
        }
        match (&left, &op.kind, &right) {
            num!(CompareEquals) | num!(CompareNotEquals) => Ok(Type::Bool), //always

            _ if left.is_unspecified() => Ok(Type::Unspecified),
            _ if right.is_unspecified() => Ok(Type::Unspecified),
            (Type::Int, Mod, Type::Int) => Ok(Type::Int),
            (Type::Bool, Or, Type::Bool) => Ok(Type::Bool),
            (Type::Bool, And, Type::Bool) => Ok(Type::Bool),
            num!(Minus) | num!(Star) | num!(Slash) | num!(Power) => {
                number_upcast_binary_op(&left, &right).map_err(|e| e.at(op.position).into())
            }

            num!(Plus) => {
                if left == Type::String && right == Type::String {
                    Ok(Type::String)
                } else {
                    number_upcast_binary_op(&left, &right).map_err(|e| e.at(op.position).into())
                }
            }

            num!(CompareGreater)
            | num!(CompareGreaterEqual)
            | num!(CompareLess)
            | num!(CompareLessEqual) => {
                if left == Type::String && right == Type::String {
                    Ok(Type::Bool)
                } else {
                    number_upcast_binary_op(&left, &right)
                        .map_err(|e| e.at(op.position).into())
                        .map(|_| Type::Bool)
                }
            }

            (left, Or | And, right) => Ok(Type::build_union(left.clone(), right.clone())),

            (left, _op, right) => Err(SomewhereTypeError::UnspecifiedBinary {
                left: left.clone(),
                op: op.clone(),
                right: right.clone(),
            }
            .at(op.position)
            .into()),
        }
    }

    fn visit_block(
        &mut self,
        _start_token: &Token,
        _end_token: &Token,
        containing_statements: &'ast [Stmt],
    ) -> Result<Type, TypeError> {
        let (last, rest) = containing_statements.split_last().unwrap();

        for stmt in rest {
            let _ = self.visit_stmt(stmt)?;
        }

        self.visit_stmt(last)
    }

    fn visit_expr_stmt(&mut self, expr: &'ast Expr) -> Result<Type, TypeError> {
        self.visit_expr(expr)
    }
}

fn number_upcast_binary_op(left: &Type, right: &Type) -> Result<Type, SomewhereTypeError> {
    if !matches!(left, Type::Int | Type::Float) {
        return Err(SomewhereTypeError::TypeMismatch {
            expected: Type::build_union(Type::Int, Type::Float),
            got: left.clone(),
        });
    }
    if !matches!(right, Type::Int | Type::Float) {
        return Err(SomewhereTypeError::TypeMismatch {
            expected: Type::build_union(Type::Int, Type::Float),
            got: right.clone(),
        });
    }
    if matches!(left, Type::Int) && matches!(right, Type::Int) {
        Ok(Type::Int)
    } else {
        Ok(Type::Float)
    }
}

#[cfg(test)]
mod tests {

    use crate::{
        compile::{checks::Annotations, typecheck::types::Type},
        parsing::{
            ast::{Expr, Stmt},
            lexer::{Index, Token, TokenKind},
        },
    };

    lazy_static! {
        static ref EMPTY_ANNOTATIONS: Annotations = Default::default();
    }

    fn make_expr(content: &str) -> Expr {
        use crate::parsing::lexer::tokenize;
        use crate::parsing::parser::program_parser;

        let tokens = tokenize(content).unwrap();

        program_parser::expr(tokens.iter().collect::<Vec<_>>().as_slice()).unwrap()
    }

    fn wrap_in_stmt(expr: Expr) -> Stmt {
        Stmt::Expression(expr)
    }

    use super::Checker;

    fn type_expected_expr(content: &str, expected_type: Type) {
        let ast = make_expr(content);

        let program = wrap_in_stmt(ast.clone());
        let program = &vec![program];
        let types = Checker::typecheck(program, &EMPTY_ANNOTATIONS).unwrap();

        assert_eq!(types.type_of((&ast).into()), expected_type);
    }

    fn error_expected_expr(content: &str) {
        let ast = make_expr(content);

        let program = wrap_in_stmt(ast);
        Checker::typecheck(&vec![program], &EMPTY_ANNOTATIONS).unwrap_err();
    }

    #[test]
    fn number_should_have_type_int() {
        type_expected_expr("1", Type::Int)
    }

    #[test]
    fn addition_of_ints_should_be_int() {
        type_expected_expr("1+2", Type::Int)
    }

    #[test]
    fn addition_of_int_and_float_should_be_float() {
        type_expected_expr("1+2.0", Type::Float);
        type_expected_expr("1.0+2", Type::Float);
        type_expected_expr("1.0 + 2.0", Type::Float);
    }

    #[test]
    fn addition_of_int_and_other_should_error() {
        error_expected_expr("1+true");
        error_expected_expr("true+1");
    }

    #[test]
    fn mod_should_allow_only_ints() {
        type_expected_expr("1 mod 2", Type::Int);
        error_expected_expr("1 mod true");
        error_expected_expr("1.0 mod 1");
    }
    #[test]
    fn comparisons() {
        type_expected_expr("1==2", Type::Bool);
        type_expected_expr("\"abc\"==1", Type::Bool);

        type_expected_expr("1!=2", Type::Bool);
        type_expected_expr("1!=\"abc\"", Type::Bool);

        type_expected_expr("1>2", Type::Bool);
        type_expected_expr("1>=2", Type::Bool);
        type_expected_expr("1<2", Type::Bool);
        type_expected_expr("1<=2", Type::Bool);

        error_expected_expr("true > false");
    }

    #[test]
    fn if_typecheck() {
        error_expected_expr("if 1 2 else 3"); // only bool
        type_expected_expr("if 1==2 2 else 3", Type::Int);
        type_expected_expr("if 1==2 2", Type::build_union(Type::Int, Type::Nothing));
        type_expected_expr(
            "if 1==2 2 else true",
            Type::build_union(Type::Int, Type::Bool),
        );
    }
}
