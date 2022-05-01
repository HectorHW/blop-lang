use crate::compile::checks::tree_visitor::Visitor;
use crate::compile::checks::{Annotations, VariableType};
use crate::parsing::ast::{Program, Stmt, TypeMention, TypedName};
use crate::parsing::lexer::Token;
use crate::Expr;
use std::collections::HashMap;

type ScopeStack = Vec<(ScopeType, Token, HashMap<String, (bool, Token)>)>;

pub struct AnnotationGenerator<'a> {
    annotations: &'a mut Annotations,
    scopes: ScopeStack,
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum ScopeType {
    Block,
    TopLevel,
    Function,
}

impl<'a> AnnotationGenerator<'a> {
    pub fn generate_annotations(
        ast: &Program,
        annotations: &'a mut Annotations,
    ) -> Result<(), String> {
        let mut annotator = AnnotationGenerator {
            annotations,
            scopes: Default::default(),
        };

        annotator.new_scope(ScopeType::TopLevel, &crate::compile::compiler::SCRIPT_TOKEN);

        ast.iter().try_for_each(|s| annotator.visit_stmt(s))
    }

    fn declare_name(&mut self, variable_name: &Token) {
        self.scopes.last_mut().unwrap().2.insert(
            variable_name.get_string().unwrap().to_string(),
            (false, variable_name.clone()),
        );

        self.annotations
            .get_or_create_block_scope(&self.scopes.last_mut().unwrap().1)
            .insert(
                variable_name.get_string().unwrap().to_string(),
                if self.scopes.last().unwrap().0 == ScopeType::TopLevel {
                    VariableType::Global
                } else {
                    VariableType::Normal
                },
            );
    }

    fn define_name(&mut self, variable_name: &Token) {
        self.scopes.last_mut().unwrap().2.insert(
            variable_name.get_string().unwrap().to_string(),
            (true, variable_name.clone()),
        );
    }

    fn lookup_local(&mut self, variable: &Token) -> bool {
        //try to lookup initialized value
        let variable_name = variable.get_string().unwrap();
        for (scope_type, _scope_identifier, scope_map) in self.scopes.iter().rev() {
            if let Some((true, definition)) = scope_map.get(variable_name) {
                self.annotations
                    .variable_bindings
                    .insert(variable.clone(), definition.clone());
                return true;
            }

            if *scope_type == ScopeType::Function {
                break;
            }
        }
        false
    }

    fn lookup_outer(&self, variable: &Token) -> Option<VariableType> {
        let mut passed_function_scope = false;
        let variable_name = variable.get_string().unwrap();
        for (scope_type, _scope_identifier, scope_map) in self.scopes.iter().rev() {
            if passed_function_scope {
                if scope_map.contains_key(variable_name) {
                    if *scope_type == ScopeType::TopLevel {
                        return Some(VariableType::Global);
                    } else {
                        return Some(VariableType::Closed);
                    }
                }
            } else if *scope_type == ScopeType::Function {
                passed_function_scope = true;
            }
        }
        None
    }

    fn lookup_name(&mut self, variable: &Token) {
        if self.lookup_local(variable) {
            return;
        }

        let outer_type = self.lookup_outer(variable);

        if outer_type.is_none() {
            //global undeclared
            return;
        }

        let outer_type = outer_type.unwrap();

        let variable_name = variable.get_string().unwrap();

        let definition = self.find_closed(variable).clone();

        self.annotations
            .variable_bindings
            .insert(variable.clone(), definition.clone());

        if outer_type == VariableType::Global {
            return;
        }

        let mut passed_function_scope = false;
        for (scope_type, scope_identifier, scope_map) in self.scopes.iter().rev() {
            if !passed_function_scope && *scope_type == ScopeType::Function {
                passed_function_scope = true;
                self.annotations
                    .get_or_create_closure_scope(scope_identifier)
                    .insert(definition.clone());
            } else {
                if scope_map.contains_key(variable_name) {
                    self.annotations
                        .get_or_create_block_scope(scope_identifier)
                        .insert(variable_name.to_string(), VariableType::Boxed);
                    return;
                }
                if *scope_type == ScopeType::Function {
                    self.annotations
                        .get_or_create_closure_scope(scope_identifier)
                        .insert(definition.clone());
                }
            }
        }
    }

    fn find_closed(&self, variable: &Token) -> &Token {
        let mut passed_function_scope = false;
        let variable_name = variable.get_string().unwrap();
        for (scope_type, _scope_identifier, scope_map) in self.scopes.iter().rev() {
            if !passed_function_scope && *scope_type == ScopeType::Function {
                passed_function_scope = true;
            } else if scope_map.contains_key(variable_name) {
                let (_, definition) = scope_map.get(variable_name).unwrap();
                return definition;
            }
        }
        unreachable!("should've found closed variable");
    }

    fn new_scope(&mut self, scope_type: ScopeType, token: &Token) {
        self.scopes
            .push((scope_type, token.clone(), Default::default()));
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn lookup_type(&mut self, name: &TypedName) {
        name.type_name.as_ref().map(|t| self.lookup_local(&t.0));
    }
}

impl<'a, 'ast> Visitor<'ast, (), String> for AnnotationGenerator<'a> {
    fn visit_var_stmt(&mut self, name: &TypedName, rhs: Option<&Expr>) -> Result<(), String> {
        self.lookup_type(name);
        if let Some(value) = rhs {
            self.visit_expr(value)?;
        }

        self.define_name(&name.name);

        Ok(())
    }

    fn visit_struct_declaration_statement(
        &mut self,
        name: &'ast Token,
        fields: &[TypedName],
    ) -> Result<(), String> {
        for field in fields {
            self.lookup_type(field);
        }
        self.define_name(name);
        Ok(())
    }

    fn visit_enum_declaration(
        &mut self,
        name: &'ast Token,
        variants: &'ast [crate::parsing::ast::EnumVariant],
    ) -> Result<(), String> {
        for variant in variants {
            for field in &variant.fields {
                self.lookup_type(field);
            }
        }
        self.define_name(name);
        Ok(())
    }

    fn visit_function_declaration_statement(
        &mut self,
        name: &Token,
        args: &[TypedName],
        vararg: Option<&TypedName>,
        body: &Expr,
        returns: Option<&TypeMention>,
    ) -> Result<(), String> {
        self.new_scope(ScopeType::Function, name);
        self.annotations.get_or_create_closure_scope(name);
        for arg_name in args.iter().chain(vararg.into_iter()) {
            self.lookup_type(arg_name);
            self.declare_name(&arg_name.name);
            self.define_name(&arg_name.name);
        }
        if let Some(t) = returns {
            self.lookup_name(&t.0)
        }
        self.define_name(name);
        self.visit_expr(body)?;
        self.pop_scope();

        self.define_name(name);

        Ok(())
    }

    fn visit_variable_expr(&mut self, variable_name: &Token) -> Result<(), String> {
        self.lookup_name(variable_name);
        Ok(())
    }

    fn visit_assignment_stmt(
        &mut self,
        target: &'ast Token,
        value: &'ast Expr,
    ) -> Result<(), String> {
        self.visit_expr(value)?;
        self.lookup_name(target);
        Ok(())
    }

    fn visit_block(
        &mut self,
        start_token: &Token,
        _end_token: &Token,
        containing_statements: &[Stmt],
    ) -> Result<(), String> {
        self.new_scope(ScopeType::Block, start_token);
        self.annotations.get_or_create_block_scope(start_token);

        //declare variables
        for statement in containing_statements {
            match statement {
                Stmt::VarDeclaration(name, _) => {
                    self.declare_name(&name.name);
                }
                Stmt::FunctionDeclaration { name, .. } => {
                    self.declare_name(name);
                }

                Stmt::StructDeclaration { name, .. } => {
                    self.declare_name(name);
                }

                Stmt::EnumDeclaration { name, .. } => {
                    self.declare_name(name);
                }

                Stmt::Import { name, rename, .. } => {
                    let import_name = rename.as_ref().unwrap_or(name);
                    self.declare_name(import_name);
                }

                _ => {}
            }
        }

        for item in containing_statements {
            self.visit_stmt(item)?;
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
        self.new_scope(ScopeType::Function, arrow);
        self.annotations.get_or_create_closure_scope(arrow);
        for arg_name in args.iter().chain(vararg.into_iter()) {
            self.lookup_type(arg_name);
            self.declare_name(&arg_name.name);
            self.define_name(&arg_name.name);
        }

        self.visit_expr(body)?;
        self.pop_scope();
        Ok(())
    }

    fn visit_impl_block(&mut self, name: &Token, implementations: &[Stmt]) -> Result<(), String> {
        self.lookup_name(name);

        for f in implementations {
            self.visit_stmt(f)?;
        }

        Ok(())
    }

    fn visit_import_stmt(
        &mut self,
        _module: &[Token],
        name: &Token,
        rename: Option<&Token>,
    ) -> Result<(), String> {
        self.define_name(rename.unwrap_or(name));
        Ok(())
    }
}
