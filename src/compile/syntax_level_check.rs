use crate::parsing::ast::{Expr, Program, Stmt};
use crate::parsing::lexer::Token;
use indexmap::{IndexMap, IndexSet};
use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};

struct Checker {
    names: Vec<(ScopeType, Token, HashMap<String, bool>)>,
    total_variables: usize,
    current_block: Vec<Token>,
    current_function: Vec<Token>,
    variable_types: BlockNameMap,
    closed_names: ClosedNamesMap,
}

enum ScopeType {
    Block,
    Function,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum VariableType {
    Normal,
    Boxed,
    Closed,
}

pub type BlockNameMap = HashMap<Token, IndexMap<String, VariableType>>;
pub type ClosedNamesMap = HashMap<Token, IndexSet<String>>;

pub fn check(program: &Program) -> Result<(BlockNameMap, ClosedNamesMap), String> {
    let mut checker = Checker::new();
    let block_token = match program.as_ref() {
        Expr::Block(block_token, _block_end_token, _) => block_token,
        _ => panic!("this should never happen as program is parsed as block"),
    };
    checker.new_scope(ScopeType::Block, block_token);
    checker.current_block.push(block_token.clone());
    checker.visit_expr(program)?;
    Ok((checker.variable_types, checker.closed_names))
}

impl Checker {
    fn new() -> Checker {
        Checker {
            names: vec![],
            total_variables: 0,
            current_block: vec![],
            variable_types: HashMap::new(),
            current_function: vec![],
            closed_names: HashMap::new(),
        }
    }

    fn lookup_local(&mut self, name: &Token) -> Result<(), String> {
        let mut found_init = None;
        //try to lookup initialized value
        for (_scope_type, scope_identifier, scope_map) in self.names.iter_mut().rev() {
            match scope_map.entry(name.get_string().unwrap().clone()) {
                Entry::Occupied(entry) if *entry.get() => {
                    found_init = Some(scope_identifier.clone());
                    break;
                }
                _ => {}
            }
        }

        if found_init.is_some() {
            let found_init = found_init.unwrap();
            let mut passed_function_scope = false;

            for (scope_type, scope_identifier, _scope_map) in self.names.iter_mut().rev() {
                if scope_identifier == &found_init {
                    //we are in right scope
                    if passed_function_scope {
                        self.variable_types
                            .get_mut(scope_identifier)
                            .unwrap()
                            .insert(name.get_string().unwrap().clone(), VariableType::Boxed);
                    }

                    return Ok(());
                }

                if let ScopeType::Function = scope_type {
                    //mark closing
                    self.closed_names
                        .get_mut(scope_identifier)
                        .unwrap()
                        .insert(name.get_string().unwrap().clone());
                    passed_function_scope = true;
                }
            }
        }

        //try to lookup uninitialized in block right after passing function scope

        let mut passed_function_scope = false;
        let mut found_uninit = None;

        for (scope_type, scope_identifier, scope_map) in self.names.iter_mut().rev() {
            if passed_function_scope {
                if let Entry::Occupied(_entry) = scope_map.entry(name.get_string().unwrap().clone())
                {
                    found_uninit = Some(scope_identifier.clone());
                }
                break; //we only look in one scope
            }
            match scope_type {
                ScopeType::Block => {}
                ScopeType::Function => {
                    passed_function_scope = true;
                }
            }
        }

        if found_uninit.is_some() {
            let found_uninit = found_uninit.unwrap();

            for (scope_type, scope_identifier, _scope_map) in self.names.iter_mut().rev() {
                if scope_identifier == &found_uninit {
                    //we are in right scope
                    self.variable_types
                        .get_mut(&scope_identifier.clone())
                        .unwrap()
                        .insert(name.get_string().unwrap().clone(), VariableType::Boxed);
                    return Ok(());
                }

                if let ScopeType::Function = scope_type {
                    //mark closing
                    self.closed_names
                        .get_mut(scope_identifier)
                        .unwrap()
                        .insert(name.get_string().unwrap().clone());
                }
            }
        }
        Err(format!(
            "no variable `{}` found in scope [{}]",
            name.get_string().unwrap(),
            name.position
        ))
    }

    fn define_name(&mut self, variable_name: &Token) -> Result<(), String> {
        match self
            .names
            .last_mut()
            .unwrap()
            .2
            .entry(variable_name.get_string().unwrap().clone())
        {
            Entry::Occupied(mut is_defined) => {
                if *is_defined.get() {
                    return Err(format!(
                        "variable `{}` already defined in current scope [{}]",
                        variable_name.get_string().unwrap(),
                        variable_name.position
                    ));
                } else {
                    is_defined.insert(true);
                    Ok(())
                }
            }
            Entry::Vacant(_) => {
                return Err(format!(
                    "no variable `{}` declared in current scope [{}]",
                    variable_name.get_string().unwrap(),
                    variable_name.position
                ))
            }
        }
    }

    fn declare_name(&mut self, variable_name: &Token) -> Result<(), String> {
        if self
            .names
            .last()
            .unwrap()
            .2
            .contains_key(variable_name.get_string().unwrap())
        {
            return Err(format!(
                "name {} already exists in current scope [{}]",
                variable_name.get_string().unwrap(),
                variable_name.position
            ));
        }
        self.total_variables += 1;
        self.names
            .last_mut()
            .unwrap()
            .2
            .insert(variable_name.get_string().unwrap().clone(), false);

        let map: &mut IndexMap<String, VariableType> = self
            .variable_types
            .get_mut(self.current_block.last().unwrap())
            .unwrap();

        map.insert(
            variable_name.get_string().unwrap().clone(),
            VariableType::Normal,
        );
        Ok(())
    }

    fn new_scope(&mut self, scope_type: ScopeType, token: &Token) {
        if let ScopeType::Function = &scope_type {
            self.current_function.push(token.clone());
            self.closed_names.insert(token.clone(), IndexSet::new());
        }

        self.names.push((scope_type, token.clone(), HashMap::new()));
        self.current_block.push(token.clone());
        self.variable_types.insert(token.clone(), IndexMap::new());
    }

    fn pop_scope(&mut self) {
        let scope = self.names.pop().unwrap();

        if let ScopeType::Function = scope.0 {
            self.current_function.pop();
        }

        let items_in_scope = scope.2.len();
        drop(scope);
        self.total_variables -= items_in_scope;
        self.current_block.pop();
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Print(_t, e) => self.visit_expr(e),
            Stmt::VarDeclaration(name, body) => {
                body.as_ref()
                    .map(|e| self.visit_expr(e))
                    .unwrap_or(Ok(()))?;
                self.define_name(name)
            }
            Stmt::Assignment(target, expr) => {
                self.lookup_local(target)?;
                //we can only assign to locals
                self.visit_expr(expr)
            }
            Stmt::Expression(e) => self.visit_expr(e),
            Stmt::Assert(_kw, e) => self.visit_expr(e),
            Stmt::FunctionDeclaration { name, args, body } => {
                self.check_function(name, args, body)?;
                self.define_name(name)
            }
            Stmt::Pass(..) => Ok(()),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<(), String> {
        match expr {
            Expr::Number(_) => Ok(()),
            Expr::ConstString(_) => Ok(()),

            Expr::Name(n) => {
                let _ = self.lookup_local(n);
                //if we fail to lookup a name then treat it as global
                Ok(())
            }

            Expr::Binary(op, a, b) => {
                self.visit_expr(a)?;
                self.visit_expr(b)?;
                use crate::parsing::lexer::TokenKind::*;
                match &op.kind {
                    Plus | Minus | Star | Slash | CompareEquals | CompareNotEquals
                    | CompareGreater | CompareGreaterEqual | CompareLess | CompareLessEqual
                    | Mod | Power | Or => Ok(()),
                    _ => Err(format!("cannot compile operator {:?}", op)),
                }
            }
            Expr::IfExpr(cond, then_body, else_body) => {
                self.visit_expr(cond)?;
                self.visit_expr(then_body)?;
                else_body
                    .as_ref()
                    .map(|x| self.visit_expr(x.as_ref()))
                    .unwrap_or(Ok(()))
            }
            Expr::Block(bb, _be, b) => self.visit_block(b, bb),
            Expr::Call(target, args) => {
                self.visit_expr(target)?;
                for arg in args {
                    self.visit_expr(arg)?;
                }
                Ok(())
            }
            Expr::SingleStatement(s) => self.visit_stmt(s),
        }
    }

    fn check_function(&mut self, name: &Token, args: &[Token], body: &Expr) -> Result<(), String> {
        //let mut scope_stack = vec![];
        //std::mem::swap(&mut self.names, &mut scope_stack);
        //let previous_total_variables = self.total_variables;

        //self.total_variables = 0;

        self.new_scope(ScopeType::Function, name);
        self.declare_name(name)?;
        self.define_name(name)?; //define function inside itself
        for arg_name in args {
            self.declare_name(arg_name)?;
            self.define_name(arg_name)?;
        }
        self.visit_expr(body)?;
        self.pop_scope();
        //std::mem::swap(&mut self.names, &mut scope_stack);
        //self.total_variables = previous_total_variables;

        Ok(())
    }

    fn visit_block(&mut self, block: &[Stmt], block_id: &Token) -> Result<(), String> {
        self.new_scope(ScopeType::Block, block_id);

        //declare variables
        for statement in block {
            match statement {
                Stmt::VarDeclaration(name, _) => {
                    self.declare_name(name)?;
                }
                Stmt::FunctionDeclaration { name, .. } => {
                    self.declare_name(name)?;
                }
                _ => {}
            }
        }

        for item in block {
            self.visit_stmt(item)?;
        }
        self.pop_scope();
        Ok(())
    }
}
