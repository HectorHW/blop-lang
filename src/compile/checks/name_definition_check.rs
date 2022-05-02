use crate::compile::checks::tree_visitor::Visitor;
use crate::parsing::ast::{EnumVariant, Program, Stmt, TypeMention, TypedName};
use crate::parsing::lexer::Token;
use crate::Expr;
use std::collections::HashMap;

/// checks that names and arguments do not repeat in same scope
pub struct NameRedefinitionChecker {
    scope: Vec<HashMap<String, Token>>,
}

impl<'ast> NameRedefinitionChecker {
    pub fn check(ast: &'ast Program) -> Result<(), String> {
        let mut checker = NameRedefinitionChecker { scope: vec![] };
        checker.new_scope();
        ast.iter().try_for_each(|s| checker.visit_stmt(s))
    }

    fn new_scope(&mut self) {
        self.scope.push(HashMap::new())
    }

    fn pop_scope(&mut self) {
        self.scope.pop();
    }

    fn declare_name(&mut self, name: &Token) -> Result<(), Token> {
        let previous_def = self
            .scope
            .last_mut()
            .unwrap()
            .insert(name.get_string().unwrap().to_string(), name.clone());
        if let Some(token) = previous_def {
            Err(token)
        } else {
            Ok(())
        }
    }
}

impl<'ast> Visitor<'ast, (), String> for NameRedefinitionChecker {
    fn visit_var_stmt(
        &mut self,
        name: &'ast TypedName,
        rhs: Option<&'ast Expr>,
    ) -> Result<(), String> {
        if let Some(rhs) = rhs {
            self.visit_expr(rhs)?
        };

        self.declare_name(&name.name).map_err(|e| {
            format!(
                "name {} [{}] is redefined in block, previous definition at [{}]",
                name.name.get_string().unwrap(),
                name.name.position,
                e.position
            )
        })?;

        Ok(())
    }

    fn visit_function_declaration_statement(
        &mut self,
        name: &Token,
        args: &[TypedName],
        vararg: Option<&TypedName>,
        body: &Expr,
        _returns: Option<&TypeMention>,
    ) -> Result<(), String> {
        self.declare_name(name).map_err(|e| {
            format!(
                "name {} [{}] is redefined in block, previous definition at [{}]",
                name.get_string().unwrap(),
                name.position,
                e.position
            )
        })?;

        self.new_scope();
        for arg_name in args.iter().chain(vararg.into_iter()) {
            self.declare_name(&arg_name.name).map_err(|_e| {
                format!(
                    "argument {} repeats in function {} at [{}]",
                    arg_name.name.get_string().unwrap(),
                    name.get_string().unwrap(),
                    name.position
                )
            })?;
        }
        self.visit_expr(body)?;
        self.pop_scope();
        Ok(())
    }

    fn visit_block(
        &mut self,
        _start_token: &Token,
        _end_token: &Token,
        containing_statements: &[Stmt],
    ) -> Result<(), String> {
        self.new_scope();
        for stmt in containing_statements {
            self.visit_stmt(stmt)?;
        }
        self.pop_scope();
        Ok(())
    }

    fn visit_anon_function_expr(
        &mut self,
        args: &[TypedName],
        vararg: Option<&TypedName>,
        arrow: &Token,
        body: &Expr,
    ) -> Result<(), String> {
        self.new_scope();

        for arg_name in args.iter().chain(vararg.into_iter()) {
            self.declare_name(&arg_name.name).map_err(|_e| {
                format!(
                    "argument {} repeats in anonymous function at [{}]",
                    arg_name.name.get_string().unwrap(),
                    arrow.position
                )
            })?;
        }
        self.visit_expr(body)?;
        self.pop_scope();
        Ok(())
    }

    fn visit_struct_declaration_statement(
        &mut self,
        name: &Token,
        fields: &[TypedName],
    ) -> Result<(), String> {
        self.declare_name(name).map_err(|e| {
            format!(
                "name {} [{}] is redefined in block, previous definition at [{}]",
                name.get_string().unwrap(),
                name.position,
                e.position
            )
        })?;

        self.new_scope();

        for field in fields {
            self.declare_name(&field.name).map_err(|e| {
                format!(
                    "field {} [{}] is redefined in struct/enum, previous definition at [{}]",
                    field.name.get_string().unwrap(),
                    field.name.position,
                    e.position
                )
            })?;
        }

        self.pop_scope();

        Ok(())
    }

    fn visit_enum_declaration(
        &mut self,
        name: &Token,
        variants: &[EnumVariant],
    ) -> Result<(), String> {
        self.declare_name(name).map_err(|e| {
            format!(
                "name {} [{}] is redefined in block, previous definition at [{}]",
                name.get_string().unwrap(),
                name.position,
                e.position
            )
        })?;

        self.new_scope();

        for variant in variants {
            self.visit_struct_declaration_statement(&variant.name, &variant.fields)?;
        }
        self.pop_scope();
        Ok(())
    }

    fn visit_impl_block(
        &mut self,
        item_name: &Token,
        implementations: &[Stmt],
    ) -> Result<(), String> {
        self.new_scope();

        for f in implementations {
            match f {
                Stmt::FunctionDeclaration {
                    name,
                    args,
                    vararg,
                    body,
                    returns,
                } => {
                    self.visit_method(
                        item_name,
                        name,
                        args,
                        vararg.as_ref(),
                        body,
                        returns.as_ref(),
                    )?;
                }
                _ => unreachable!(),
            }
        }

        self.pop_scope();
        Ok(())
    }

    fn visit_import_stmt(
        &mut self,
        _module: &[Token],
        name: &Token,
        rename: Option<&Token>,
    ) -> Result<(), String> {
        let import_name = rename.unwrap_or(name);

        self.declare_name(import_name).map_err(|e| {
            format!(
                "name {} [{}] is redefined in block, previous definition at [{}]",
                import_name.get_string().unwrap(),
                import_name.position,
                e.position
            )
        })
    }

    fn visit_method(
        &mut self,
        _definiton_context: &Token,
        name: &Token,
        args: &[TypedName],
        vararg: Option<&TypedName>,
        body: &Expr,
        returns: Option<&TypeMention>,
    ) -> Result<(), String> {
        if args.is_empty() {
            return Err(format!(
                "method {} [{}] should have at least one argument",
                name.get_string().unwrap(),
                name.position
            ));
        }

        self.visit_function_declaration_statement(name, args, vararg, body, returns)
    }
}
