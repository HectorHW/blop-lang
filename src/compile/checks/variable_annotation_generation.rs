use crate::compile::checks::tree_visitor::Visitor;
use crate::compile::checks::{Annotations, VariableType};
use crate::parsing::ast::{Program, Stmt, TypeMention, TypedName};
use crate::parsing::lexer::Token;
use crate::Expr;
use std::collections::HashMap;

struct Scope {
    scope_type: ScopeType,
    indentifying_token: Token,
    ///mapping of variables defined in scope
    variables: HashMap<String, (bool, Token)>,
}

type ScopeStack = Vec<Scope>;

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
        annotator.process_toplevel(ast);

        ast.iter().try_for_each(|s| annotator.visit_stmt(s))
    }

    ///perform predefinitions necessary for closure boxing and recursive types
    /// expects scope to be already constructed
    fn predefine_block(&mut self, statements: &[Stmt]) {
        //declare variables
        for statement in statements {
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
    }

    fn process_toplevel(&mut self, statements: &[Stmt]) {
        self.predefine_block(statements);
    }

    fn declare_name(&mut self, variable_name: &Token) {
        self.scopes.last_mut().unwrap().variables.insert(
            variable_name.get_string().unwrap().to_string(),
            (false, variable_name.clone()),
        );

        self.annotations
            .get_or_create_block_scope(&self.scopes.last_mut().unwrap().indentifying_token)
            .insert(
                variable_name.get_string().unwrap().to_string(),
                if self.scopes.last().unwrap().scope_type == ScopeType::TopLevel {
                    VariableType::Global
                } else {
                    VariableType::Normal
                },
            );
    }

    fn define_name(&mut self, variable_name: &Token) {
        self.scopes.last_mut().unwrap().variables.insert(
            variable_name.get_string().unwrap().to_string(),
            (true, variable_name.clone()),
        );
    }

    fn lookup_local(&mut self, variable: &Token) -> bool {
        //try to lookup initialized value
        let variable_name = variable.get_string().unwrap();
        for scope in self.scopes.iter().rev() {
            if let Some((true, definition)) = scope.variables.get(variable_name) {
                self.annotations
                    .variable_bindings
                    .insert(variable.clone(), definition.clone());
                return true;
            }

            if scope.scope_type == ScopeType::Function {
                break;
            }
        }
        false
    }

    fn lookup_outer(&self, variable: &Token) -> Option<VariableType> {
        let mut passed_function_scope = false;
        let variable_name = variable.get_string().unwrap();
        for scope in self.scopes.iter().rev() {
            if passed_function_scope {
                if scope.variables.contains_key(variable_name) {
                    if scope.scope_type == ScopeType::TopLevel {
                        return Some(VariableType::Global);
                    } else {
                        return Some(VariableType::Closed);
                    }
                }
            } else if scope.scope_type == ScopeType::Function {
                passed_function_scope = true;
            }
        }
        None
    }

    ///simply set mapping 'mention --> definition'
    fn bind_name(&mut self, variable: &Token) -> Option<(VariableType, Token)> {
        if self.lookup_local(variable) {
            return None;
        }

        let outer_type = self.lookup_outer(variable)?;

        let definition = self.find_closed(variable).clone();

        self.annotations
            .variable_bindings
            .insert(variable.clone(), definition.clone());

        Some((outer_type, definition))
    }

    fn lookup_name(&mut self, variable: &Token) {
        let outer_type = self.bind_name(variable);

        let variable_name = variable.get_string().unwrap();

        if outer_type.is_none() {
            return; // global undeclared
        }

        let (outer_type, definition) = outer_type.unwrap();

        if outer_type == VariableType::Global {
            return;
        }

        let mut passed_function_scope = false;
        for scope in self.scopes.iter().rev() {
            if !passed_function_scope && scope.scope_type == ScopeType::Function {
                passed_function_scope = true;
                self.annotations
                    .get_or_create_closure_scope(&scope.indentifying_token)
                    .insert(definition.clone());
            } else {
                if scope.variables.contains_key(variable_name) {
                    self.annotations
                        .get_or_create_block_scope(&scope.indentifying_token)
                        .insert(variable_name.to_string(), VariableType::Boxed);
                    return;
                }
                if scope.scope_type == ScopeType::Function {
                    self.annotations
                        .get_or_create_closure_scope(&scope.indentifying_token)
                        .insert(definition.clone());
                }
            }
        }
    }

    fn find_closed(&self, variable: &Token) -> &Token {
        let mut passed_function_scope = false;
        let variable_name = variable.get_string().unwrap();
        for scope in self.scopes.iter().rev() {
            if !passed_function_scope && scope.scope_type == ScopeType::Function {
                passed_function_scope = true;
            } else if scope.variables.contains_key(variable_name) {
                let (_, definition) = scope.variables.get(variable_name).unwrap();
                return definition;
            }
        }
        unreachable!("should've found closed variable");
    }

    fn new_scope(&mut self, scope_type: ScopeType, token: &Token) {
        self.scopes.push(Scope {
            scope_type,
            indentifying_token: token.clone(),
            variables: Default::default(),
        });
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn lookup_type_of(&mut self, name: &TypedName) {
        if let Some(t) = name.type_name.as_ref() {
            self.lookup_type(t)
        }
    }

    fn lookup_type(&mut self, type_def: &TypeMention) {
        match type_def {
            TypeMention::Simple(t) => {
                let _ = self.bind_name(t);
            }
            TypeMention::Function {
                kw: _,
                args,
                vararg,
                return_type,
            } => {
                for t in args
                    .iter()
                    .chain(vararg.as_ref().map(|v| v.as_ref()).into_iter())
                    .chain(std::iter::once(return_type.as_ref()))
                {
                    self.lookup_type(t);
                }
            }
        }
    }
}

impl<'a, 'ast> Visitor<'ast, (), String> for AnnotationGenerator<'a> {
    fn visit_var_stmt(&mut self, name: &TypedName, rhs: Option<&Expr>) -> Result<(), String> {
        self.lookup_type_of(name);
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
            self.lookup_type_of(field);
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
                self.lookup_type_of(field);
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
            self.lookup_type_of(arg_name);
            self.declare_name(&arg_name.name);
            self.define_name(&arg_name.name);
        }
        if let Some(t) = returns {
            self.lookup_type(t);
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

        self.predefine_block(containing_statements);

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
            self.lookup_type_of(arg_name);
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
