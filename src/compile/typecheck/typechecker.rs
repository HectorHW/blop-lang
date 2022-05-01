use super::types::Type;
use crate::compile::checks::tree_visitor::Visitor;
use crate::compile::checks::Annotations;
use crate::execution::arity::Arity;
use crate::parsing::ast::{Expr, Program, Stmt, TypeMention, TypedName};
use crate::parsing::lexer::{Index, Token, TokenKind};
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug)]
pub enum SomewhereTypeError {
    TypeMismatch { expected: Type, got: Type },
    UnspecifiedBinary { left: Type, op: Token, right: Type },
    UnknownType { value: TypeMention },

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
    Definition(&'a Token),
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

impl<'a> From<&'a Token> for Typable<'a> {
    fn from(t: &'a Token) -> Self {
        Typable::Definition(t)
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

    pub(super) fn add_definition(&mut self, name: &'a Token, def_type: Type) {
        self.0.insert(name.into(), def_type);
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

        checker.perform_block_predef(program)?;

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

    fn check_expectation(provided: &Type, expected: &Type) -> Result<(), SomewhereTypeError> {
        if PartialOrd::le(provided, expected) {
            Ok(())
        } else {
            Err(SomewhereTypeError::TypeMismatch {
                expected: expected.clone(),
                got: provided.clone(),
            })
        }
    }

    fn lookup_type(&self, type_name: &TypeMention) -> Result<Type, TypeError> {
        let def_place = self.annotations.get_definiton(&type_name.0);

        let def_place = if let Some(def_place) = def_place {
            def_place
        } else {
            return match type_name.0.get_string().unwrap() {
                "Int" => Ok(Type::Int),
                "Bool" => Ok(Type::Bool),
                "Nothing" => Ok(Type::Nothing),
                _ => Err(SomewhereTypeError::UnknownType {
                    value: type_name.clone(),
                }
                .at(type_name.0.position))?,
            };
        };

        Ok(self.type_map.type_of(def_place.into()))
    }

    fn lookup_type_of(&self, name: &TypedName) -> Result<Type, TypeError> {
        name.type_name
            .as_ref()
            .map(|t| self.lookup_type(t))
            .unwrap_or(Ok(Type::Unspecified))
    }

    fn perform_block_predef(&mut self, statements: &'ast [Stmt]) -> Result<(), TypeError> {
        for stmt in statements {
            match stmt {
                Stmt::VarDeclaration(v, _) => {
                    let var_type = if let Some(type_name) = &v.type_name {
                        self.lookup_type(type_name)?
                    } else {
                        Type::Unspecified
                    };

                    self.type_map.add_definition(&v.name, var_type);
                }
                Stmt::FunctionDeclaration {
                    name,
                    args,
                    vararg,
                    body: _,
                    returns,
                } => {
                    let arg_type = args
                        .iter()
                        .map(|arg| self.lookup_type_of(arg))
                        .collect::<Result<Vec<_>, _>>()?;
                    let vararg = vararg
                        .as_ref()
                        .map(|v| self.lookup_type_of(v))
                        .transpose()?;

                    let return_type = returns
                        .as_ref()
                        .map(|ret| self.lookup_type(ret))
                        .transpose()?
                        .unwrap_or(Type::Unspecified);

                    let function_signature = Type::build_function(arg_type, vararg, return_type);
                    self.type_map.add_definition(name, function_signature);
                }
                Stmt::StructDeclaration { name, fields } => {}
                Stmt::EnumDeclaration { name, variants } => {}
                Stmt::ImplBlock { .. } => {
                    //TODO impl binding
                }
                Stmt::Import { .. } => {
                    //nothing for now
                }
                _ => {}
            }
        }
        Ok(())
    }
}

impl<'a, 'ast> Visitor<'ast, Type, TypeError> for Checker<'a, 'ast> {
    fn after_stmt(&mut self, stmt: &'ast Stmt, value: Type) -> Result<Type, TypeError> {
        self.type_map.add_stmt(stmt, value.clone());
        Ok(value)
    }

    fn visit_var_stmt(
        &mut self,
        _variable_name: &'ast TypedName,
        rhs: Option<&'ast Expr>,
    ) -> Result<Type, TypeError> {
        if rhs.is_some() {
            self.visit_expr(rhs.unwrap())?;
        }
        Ok(Type::Nothing)
    }

    fn visit_assignment_stmt(
        &mut self,
        target: &'ast Token,
        value: &'ast Expr,
    ) -> Result<Type, TypeError> {
        let value = self.visit_expr(value)?;
        let definition_type = self
            .annotations
            .get_definiton(target)
            .map(|def| self.type_map.type_of(def.into()))
            .unwrap_or(Type::Unspecified);

        Self::check_expectation(&value, &definition_type)?;

        Ok(Type::Nothing)
    }

    fn visit_expr_stmt(&mut self, expr: &'ast Expr) -> Result<Type, TypeError> {
        self.visit_expr(expr)
    }

    fn visit_assert_statement(
        &mut self,
        keyword: &'ast Token,
        expr: &'ast Expr,
    ) -> Result<Type, TypeError> {
        let inner = self.visit_expr(expr)?;
        Self::check_expectation(&inner, &Type::Bool).map_err(|e| e.at(keyword.position))?;
        Ok(Type::Nothing)
    }

    fn visit_pass_stmt(&mut self, _keyword: &'ast Token) -> Result<Type, TypeError> {
        Ok(Type::Nothing)
    }

    fn visit_function_declaration_statement(
        &mut self,
        name: &'ast Token,
        args: &'ast [TypedName],
        vararg: Option<&'ast TypedName>,
        body: &'ast Expr,
        returns: Option<&'ast TypeMention>,
    ) -> Result<Type, TypeError> {
        self.visit_expr(body)?;
        Ok(Type::Nothing)
    }

    fn visit_method(
        &mut self,
        name: &'ast Token,
        args: &'ast [TypedName],
        vararg: Option<&'ast TypedName>,
        body: &'ast Expr,
        returns: Option<&'ast TypeMention>,
    ) -> Result<Type, TypeError> {
        self.visit_function_declaration_statement(name, args, vararg, body, returns)
    }

    fn visit_struct_declaration_statement(
        &mut self,
        name: &'ast Token,
        fields: &[TypedName],
    ) -> Result<Type, TypeError> {
        Ok(Type::Nothing)
    }

    fn visit_enum_declaration(
        &mut self,
        name: &'ast Token,
        variants: &'ast [crate::parsing::ast::EnumVariant],
    ) -> Result<Type, TypeError> {
        Ok(Type::Nothing)
    }

    fn visit_property_assignment(
        &mut self,
        target: &'ast Expr,
        value: &'ast Expr,
    ) -> Result<Type, TypeError> {
        self.visit_expr(target)?;
        self.visit_expr(value)?;
        Ok(Type::Nothing)
    }

    fn visit_impl_block(
        &mut self,
        name: &'ast Token,
        implementations: &'ast [Stmt],
    ) -> Result<Type, TypeError> {
        implementations
            .iter()
            .try_for_each(|f| self.visit_stmt(f).map(|_| ()))?;

        Ok(Type::Nothing)
    }

    fn visit_import_stmt(
        &mut self,
        module: &'ast [Token],
        name: &'ast Token,
        rename: Option<&'ast Token>,
    ) -> Result<Type, TypeError> {
        Ok(Type::Nothing)
    }

    fn after_expr(&mut self, expr: &'ast Expr, value: Type) -> Result<Type, TypeError> {
        self.type_map.add_expr(expr, value.clone());
        Ok(value)
    }

    fn visit_bool_expr(&mut self, _token: &Token) -> Result<Type, TypeError> {
        Ok(Type::Bool)
    }

    fn visit_number_expr(&mut self, _token: &Token) -> Result<Type, TypeError> {
        Ok(Type::Int)
    }

    fn visit_float_number_expr(&mut self, _token: &Token) -> Result<Type, TypeError> {
        Ok(Type::Float)
    }

    fn visit_variable_expr(&mut self, variable_name: &'ast Token) -> Result<Type, TypeError> {
        Ok(Default::default())
    }

    fn visit_string_expr(&mut self, string_literal: &'ast Token) -> Result<Type, TypeError> {
        Ok(Type::String)
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

    fn visit_unary_expr(&mut self, op: &'ast Token, arg: &'ast Expr) -> Result<Type, TypeError> {
        self.visit_expr(arg)
    }

    fn visit_cond_expr(
        &mut self,
        condition: &'ast Expr,
        then_branch: &'ast Expr,
        else_branch: Option<&'ast Expr>,
    ) -> Result<Type, TypeError> {
        let condition_t = self.visit_expr(condition)?;

        Self::check_expectation(&condition_t, &Type::Bool)
            .map_err(|e| e.at(condition.get_pos()))?;

        let left = self.visit_expr(then_branch)?;
        let right = if let Some(else_branch) = else_branch {
            self.visit_expr(else_branch)?
        } else {
            Type::Nothing
        };

        Ok(Type::build_union(left, right))
    }

    fn visit_block(
        &mut self,
        _start_token: &Token,
        _end_token: &Token,
        containing_statements: &'ast [Stmt],
    ) -> Result<Type, TypeError> {
        self.perform_block_predef(containing_statements)?;

        let (last, rest) = containing_statements.split_last().unwrap();

        for stmt in rest {
            let _ = self.visit_stmt(stmt)?;
        }

        self.visit_stmt(last)
    }

    fn visit_single_statement_expr(&mut self, stmt: &'ast Stmt) -> Result<Type, TypeError> {
        self.visit_stmt(stmt)
    }

    fn visit_call_expr(
        &mut self,
        target: &'ast Expr,
        args: &'ast [Expr],
    ) -> Result<Type, TypeError> {
        self.visit_expr(target)?;
        for arg in args {
            self.visit_expr(arg)?;
        }
        Ok(Default::default())
    }

    fn visit_partial_call_expr(
        &mut self,
        target: &'ast Expr,
        args: &'ast [Option<Expr>],
    ) -> Result<Type, TypeError> {
        self.visit_expr(target)?;
        for arg in args {
            if arg.is_some() {
                self.visit_expr(arg.as_ref().unwrap())?;
            }
        }
        Ok(Default::default())
    }

    fn visit_anon_function_expr(
        &mut self,
        args: &'ast [TypedName],
        vararg: Option<&'ast TypedName>,
        arrow: &'ast Token,
        body: &'ast Expr,
    ) -> Result<Type, TypeError> {
        self.visit_expr(body)?;
        Ok(Default::default())
    }

    fn visit_property_access(
        &mut self,
        target: &'ast Expr,
        property: &'ast Token,
    ) -> Result<Type, TypeError> {
        self.visit_expr(target)?;
        Ok(Default::default())
    }

    fn visit_property_check(
        &mut self,
        target: &'ast Expr,
        property: &'ast Token,
    ) -> Result<Type, TypeError> {
        self.visit_expr(target)?;
        Ok(Type::Bool)
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
        parsing::ast::{Expr, Stmt},
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
