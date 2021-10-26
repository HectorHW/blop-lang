use crate::compile::syntax_level_check::{BlockNameMap, ClosedNamesMap, VariableType};
use crate::data::objects::Value;
use crate::execution::chunk::{Chunk, Opcode};
use crate::parsing::ast::{Expr, Program, Stmt};
use crate::parsing::lexer::TokenKind::BeginBlock;
use crate::parsing::lexer::{Index, Token, TokenKind};
use std::collections::HashMap;

enum ValueRequirement {
    Nothing,
    Value,
    ReturnValue,
}

pub struct Compiler {
    names: Vec<HashMap<String, (VariableType, usize)>>,
    value_requirements: Vec<ValueRequirement>,
    chunks: Vec<Chunk>,
    per_chunk_indices: Vec<Vec<usize>>,
    total_variables: usize,
    total_closed_variables: usize,
    current_chunk_idx: Vec<usize>,
    current_block: Vec<Token>,
    current_function: Vec<Token>,
    variable_types: BlockNameMap,
    closed_names: ClosedNamesMap,
    current_function_was_possibly_overwritten: Vec<bool>,
}

struct FunctionCompilerState {
    names: Vec<HashMap<String, (VariableType, usize)>>,
    total_variables: usize,
    total_closed_variables: usize,
}

impl Compiler {
    pub fn new(variable_types: BlockNameMap, closed_names: ClosedNamesMap) -> Compiler {
        Compiler {
            names: vec![],
            value_requirements: vec![],
            chunks: vec![],
            per_chunk_indices: vec![],
            total_variables: 0,
            total_closed_variables: 0,
            current_chunk_idx: vec![0],
            current_block: vec![],
            current_function: vec![],
            variable_types,
            closed_names,
            current_function_was_possibly_overwritten: vec![],
        }
    }

    pub fn compile(
        program: &Program,
        variable_types: BlockNameMap,
        closed_names: ClosedNamesMap,
    ) -> Result<(Vec<Vec<usize>>, Vec<Chunk>), String> {
        let mut compiler = Compiler::new(variable_types, closed_names);

        compiler.chunks = vec![Chunk::new("<script>".to_string(), 0)];
        compiler.per_chunk_indices = vec![vec![]];
        let block_identifier = match program.as_ref() {
            Expr::Block(bb, _be, _) => bb,
            _ => &Token {
                kind: BeginBlock,
                position: Index(0, 0),
            }, //wont be used as we are not inside block
        };
        compiler.new_scope(block_identifier);

        compiler.require_return_value();
        let (mut indices, code) = compiler.visit_expr(program)?;
        compiler.current_chunk().append(code);
        compiler.current_indices().append(&mut indices);
        compiler.pop_requirement();
        *compiler.current_chunk() += Opcode::Return;
        compiler.current_indices().push(0);
        Ok((compiler.per_chunk_indices, compiler.chunks))
    }

    fn require_value(&mut self) {
        self.value_requirements.push(ValueRequirement::Value)
    }

    fn require_nothing(&mut self) {
        self.value_requirements.push(ValueRequirement::Nothing)
    }

    fn require_return_value(&mut self) {
        self.value_requirements.push(ValueRequirement::ReturnValue)
    }

    fn pop_requirement(&mut self) {
        self.value_requirements.pop();
    }

    fn needs_value(&self) -> bool {
        match self.value_requirements.last() {
            Some(ValueRequirement::Value) | Some(ValueRequirement::ReturnValue) => true,
            Some(ValueRequirement::Nothing) | None => false,
        }
    }

    fn needs_return_value(&self) -> bool {
        match self.value_requirements.last() {
            Some(ValueRequirement::ReturnValue) => true,
            _other => false,
        }
    }

    fn current_chunk(&mut self) -> &mut Chunk {
        self.chunks
            .get_mut(*self.current_chunk_idx.last().unwrap())
            .unwrap()
    }

    fn current_indices(&mut self) -> &mut Vec<usize> {
        self.per_chunk_indices
            .get_mut(*self.current_chunk_idx.last().unwrap())
            .unwrap()
    }

    fn lookup_local(&self, name: &str) -> Option<(VariableType, usize)> {
        for scope in self.names.iter().rev() {
            if scope.contains_key(name) {
                return Some(*scope.get(name).unwrap());
            }
        }
        None
    }

    fn lookup_block(&self, name: &str) -> Option<(VariableType, usize)> {
        self.names.last().unwrap().get(name).cloned()
    }

    fn define_local(
        &mut self,
        variable_name: &str,
        var_type: VariableType,
    ) -> Option<(VariableType, usize)> {
        if self.names.last().unwrap().contains_key(variable_name) {
            return None;
        }

        let var_index = self.total_variables;
        self.total_variables += 1;
        self.names
            .last_mut()
            .unwrap()
            .insert(variable_name.to_string(), (var_type, var_index));
        Some((var_type, var_index))
    }

    fn define_closed_variable(&mut self, variable_name: &str) -> Option<(VariableType, usize)> {
        let idx = self.total_closed_variables;
        self.total_closed_variables += 1;
        self.names
            .last_mut()
            .unwrap()
            .insert(variable_name.to_string(), (VariableType::Closed, idx))
    }

    fn new_scope(&mut self, token: &Token) {
        self.names.push(HashMap::new());
        self.current_block.push(token.clone());
    }

    fn pop_scope(&mut self) -> usize {
        let scope = self.names.pop().unwrap();
        self.current_block.pop();
        let items_in_scope = scope.len();
        drop(scope);
        self.total_variables -= items_in_scope;
        items_in_scope
    }

    fn create_function_compilation_state(
        &mut self,
        name: &Token,
        arity: usize,
    ) -> FunctionCompilerState {
        let mut scope_stack = vec![];
        std::mem::swap(&mut self.names, &mut scope_stack);
        let previous_state = FunctionCompilerState {
            names: scope_stack,
            total_variables: self.total_variables,
            total_closed_variables: self.total_closed_variables,
        };

        let new_chunk_idx = self.chunks.len();
        self.chunks.push(Chunk::new(
            format!("{} [{}]", name.get_string().unwrap(), name.position),
            arity,
        ));
        self.per_chunk_indices.push(vec![]);
        self.current_chunk_idx.push(new_chunk_idx);
        self.current_function_was_possibly_overwritten.push(false);
        self.total_variables = 0;
        self.total_closed_variables = 0;

        previous_state
    }

    fn pop_function_compilation_state(&mut self, s: FunctionCompilerState) -> usize {
        let mut state = s;
        std::mem::swap(&mut self.names, &mut state.names);
        let idx = self.current_chunk_idx.pop().unwrap();
        self.total_variables = state.total_variables;
        self.total_closed_variables = state.total_closed_variables;
        self.current_function_was_possibly_overwritten.pop();
        idx
    }

    fn compile_function(
        &mut self,
        name: &Token,
        args: &[Token],
        body: &Expr,
    ) -> Result<usize, String> {
        //save current compiler

        let prev_state = self.create_function_compilation_state(name, args.len());

        //compile body

        self.new_scope(name);

        let closures = (unsafe { (self as *const Compiler).as_ref().unwrap() })
            .closed_names
            .get(name)
            .unwrap();

        for closed_variable in closures {
            self.define_closed_variable(closed_variable);
        } //define closed values

        self.new_scope(name);

        self.define_local(name.get_string().unwrap(), VariableType::Normal)
            .unwrap();
        //define function inside itself

        for arg_name in args {
            match self.define_local(arg_name.get_string().unwrap(), VariableType::Normal) {
                Some(_) => {}
                None => {
                    return Err(format!(
                        "argument {} repeats in function {}",
                        arg_name.get_string().unwrap(),
                        name.get_string().unwrap()
                    ));
                }
            }
        }

        self.new_scope(name);

        let mut closed_arguments = 0;

        for arg in args {
            if let VariableType::Boxed = self
                .variable_types
                .get(name)
                .unwrap()
                .get(arg.get_string().unwrap())
                .unwrap()
            {
                let (_, real_idx) = self.lookup_local(arg.get_string().unwrap()).unwrap();
                *self.current_chunk() += Opcode::NewBox;
                *self.current_chunk() += Opcode::Duplicate;
                *self.current_chunk() += Opcode::LoadLocal(real_idx as u16);
                *self.current_chunk() += Opcode::StoreBox;

                self.current_indices().push(name.position.0);
                self.current_indices().push(name.position.0);
                self.current_indices().push(name.position.0);
                self.current_indices().push(name.position.0);

                self.define_local(arg.get_string().unwrap(), VariableType::Boxed);
                closed_arguments += 1;
            }
        }

        if closed_arguments == 0 {
            self.pop_scope();
        }

        self.require_return_value();
        let (mut code_indices, code) = self.visit_expr(body)?;
        self.pop_requirement();
        self.current_chunk().append(code);
        self.current_indices().append(&mut code_indices);

        *self.current_chunk() += Opcode::Return;
        {
            let last = *self.current_indices().last().unwrap();
            self.current_indices().push(last);
        }

        //load current compiler
        let new_chunk_idx = self.pop_function_compilation_state(prev_state);

        Ok(new_chunk_idx)
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<(Vec<usize>, Vec<Opcode>), String> {
        let mut result = vec![];
        let mut source_indices = vec![];
        match stmt {
            Stmt::Print(t, e) => {
                self.require_value();
                let (mut indices, mut compiled_expression) = self.visit_expr(e)?;
                self.pop_requirement();
                result.append(&mut compiled_expression);
                source_indices.append(&mut indices);
                result.push(Opcode::Print);
                source_indices.push(t.position.0);

                if self.needs_value() {
                    result.push(Opcode::LoadImmediateInt(0));
                    source_indices.push(t.position.0);
                }
            }

            Stmt::VarDeclaration(n, e) => {
                match self.lookup_block(n.get_string().unwrap()) {
                    Some((VariableType::Boxed, idx)) => {
                        result.push(Opcode::LoadLocal(idx as u16)); //load box
                        source_indices.push(n.position.0);
                        if e.is_none() {
                            result.push(Opcode::LoadImmediateInt(0));
                            source_indices.push(n.position.0);
                        } else {
                            self.require_value();
                            let (mut indices, mut assignment_body) =
                                self.visit_expr(e.as_ref().unwrap())?;
                            self.pop_requirement();
                            result.append(&mut assignment_body);
                            source_indices.append(&mut indices);
                        }
                        result.push(Opcode::StoreBox);
                        source_indices.push(n.position.0);
                    }

                    None => {
                        //variable is not forward-declared => not present on stack yet
                        if e.is_none() {
                            result.push(Opcode::LoadImmediateInt(0));
                            source_indices.push(n.position.0);
                        } else {
                            self.require_value();
                            let (mut indices, mut assignment_body) =
                                self.visit_expr(e.as_ref().unwrap())?;
                            self.pop_requirement();
                            result.append(&mut assignment_body);
                            source_indices.append(&mut indices);
                        }
                        let varname = n.get_string().unwrap();
                        let _ = self
                            .define_local(varname, VariableType::Normal)
                            .ok_or(format!("redefinition of variable {}", varname))?;
                    }
                    _a => panic!("{:?}", _a),
                }

                if self.needs_value() {
                    result.push(Opcode::LoadImmediateInt(0)); //TODO dup?
                    source_indices.push(*source_indices.last().unwrap());
                }
            }

            Stmt::Assignment(target, expr) => {
                let varname = target.get_string().unwrap();

                let (var_type, var_idx) = self.lookup_local(varname).ok_or(format!(
                    "assignment to undefined variable `{}` at {}",
                    varname, target.position
                ))?;

                if *self.current_chunk_idx.last().unwrap() != 0 {
                    //compiling some function
                    if var_idx == 0 {
                        //slot 0 - current function
                        *self
                            .current_function_was_possibly_overwritten
                            .last_mut()
                            .unwrap() = true;
                    }
                }
                //maybe we need to load pointer
                match var_type {
                    VariableType::Boxed => {
                        result.push(Opcode::LoadLocal(var_idx as u16));
                        source_indices.push(target.position.0);
                    }
                    VariableType::Closed => {
                        result.push(Opcode::LoadClosureValue(var_idx as u16));
                        source_indices.push(target.position.0);
                    }
                    VariableType::Normal => {
                        //nothing
                    }
                }

                self.require_value();
                let (mut indices, mut expr_body) = self.visit_expr(expr)?;
                self.pop_requirement();

                result.append(&mut expr_body);
                source_indices.append(&mut indices);

                match var_type {
                    VariableType::Normal => {
                        result.push(Opcode::StoreLocal(var_idx as u16)); //TODO extension
                        source_indices.push(target.position.0);
                    }
                    VariableType::Boxed => {
                        result.push(Opcode::StoreBox);
                        source_indices.push(target.position.0);
                    }
                    VariableType::Closed => {
                        result.push(Opcode::StoreBox);
                        source_indices.push(target.position.0);
                    }
                }

                if self.needs_value() {
                    result.push(Opcode::LoadImmediateInt(0));
                    source_indices.push(target.position.0);
                }
            }

            Stmt::Expression(e) => {
                let (mut indices, mut body) = self.visit_expr(e)?;
                result.append(&mut body);
                source_indices.append(&mut indices);
            }

            Stmt::Assert(token, expr) => {
                self.require_value();
                let (mut indices, mut body) = self.visit_expr(expr)?;
                self.pop_requirement();
                result.append(&mut body);
                source_indices.append(&mut indices);
                result.push(Opcode::Assert);
                source_indices.push(token.position.0);
                if self.needs_value() {
                    result.push(Opcode::LoadImmediateInt(0));
                    source_indices.push(token.position.0);
                }
            }

            Stmt::FunctionDeclaration {
                name: function_name,
                args,
                body,
            } => {
                let new_chunk_idx = self.compile_function(function_name, args, body)?;

                let const_idx = self.current_chunk().constants.len();
                self.current_chunk().constants.push(Value::Function {
                    chunk_id: new_chunk_idx,
                });

                if let Some((_, idx)) = self.lookup_block(function_name.get_string().unwrap()) {
                    //load pointer
                    result.push(Opcode::LoadLocal(idx as u16));
                    source_indices.push(function_name.position.0);
                }

                result.push(Opcode::LoadConst(const_idx as u16)); //code block

                if !self.closed_names.get(function_name).unwrap().is_empty() {
                    result.push(Opcode::NewClosure);
                    source_indices.push(function_name.position.0);
                }

                let map_iter = (unsafe { (self as *const Compiler).as_ref().unwrap() })
                    .closed_names
                    .get(function_name)
                    .unwrap();

                for closed_over_value in map_iter {
                    let name = self.lookup_local(closed_over_value).unwrap();

                    match name {
                        (VariableType::Normal, var_idx) => {
                            result.push(Opcode::LoadLocal(var_idx as u16)); //TODO extension
                            source_indices.push(function_name.position.0);
                        }
                        (VariableType::Boxed, var_idx) => {
                            result.push(Opcode::LoadLocal(var_idx as u16));
                            source_indices.push(function_name.position.0);
                        }
                        (VariableType::Closed, idx) => {
                            result.push(Opcode::LoadClosureValue(idx as u16));
                            source_indices.push(function_name.position.0);
                        }
                    };
                    result.push(Opcode::AddClosedValue);
                    source_indices.push(function_name.position.0);
                }

                //match self.lookup_local()

                match self.lookup_block(function_name.get_string().unwrap()) {
                    Some((VariableType::Boxed, _)) => {
                        //pointer is on stack
                        result.push(Opcode::StoreBox);
                        source_indices.push(function_name.position.0);
                    }
                    None => {
                        self.define_local(
                            function_name.get_string().unwrap(),
                            VariableType::Normal,
                        )
                        .ok_or_else(|| {
                            format!(
                                "redifinition of name {}",
                                function_name.get_string().unwrap()
                            )
                        })?;
                    }
                    _other => panic!("{:?}", _other),
                }

                if self.needs_value() {
                    result.push(Opcode::LoadImmediateInt(0));
                    source_indices.push(function_name.position.0);
                }
            }
            Stmt::Pass(token) => {
                if self.needs_value() {
                    result.push(Opcode::LoadImmediateInt(0));
                } else {
                    result.push(Opcode::Nop);
                }
                source_indices.push(token.position.0);
            }
        }
        Ok((source_indices, result))
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<(Vec<usize>, Vec<Opcode>), String> {
        let mut result = vec![];
        let mut source_indices = vec![];
        match expr {
            Expr::Number(token) => {
                let n = token.get_number().unwrap();
                if n >= (i16::MIN as i64) && n <= (i16::MAX as i64) {
                    result.push(Opcode::LoadImmediateInt(n as i16));
                    source_indices.push(token.position.0);
                } else {
                    let constant_index = self.current_chunk().constants.len();
                    self.current_chunk().constants.push(Value::Int(n));
                    result.push(Opcode::LoadConst(constant_index as u16)); //TODO extension
                    source_indices.push(token.position.0);
                }
                if !self.needs_value() {
                    result.push(Opcode::Pop(1));
                    source_indices.push(token.position.0);
                }
            }

            Expr::Binary(op, a, b) => {
                self.require_value();
                let (mut indices_a, mut a) = self.visit_expr(a)?;
                let (mut indices_b, mut b) = self.visit_expr(b)?;
                self.pop_requirement();

                result.append(&mut a);
                source_indices.append(&mut indices_a);
                result.append(&mut b);
                source_indices.append(&mut indices_b);

                result.push(match &op.kind {
                    TokenKind::Star => Opcode::Mul,
                    TokenKind::Slash => Opcode::Div,
                    TokenKind::Plus => Opcode::Add,
                    TokenKind::Minus => Opcode::Sub,
                    TokenKind::TestEquals => Opcode::TestEquals,
                    TokenKind::Mod => Opcode::Mod,
                    TokenKind::Power => Opcode::Power,
                    other => panic!("unimplemented binary operator {} [{}]", other, op.position),
                });
                source_indices.push(op.position.0);
                if !self.needs_value() {
                    result.push(Opcode::Pop(1));
                    source_indices.push(op.position.0);
                }
            }

            Expr::Name(n) => match self.lookup_local(n.get_string().unwrap()) {
                Some((VariableType::Normal, var_idx)) => {
                    result.push(Opcode::LoadLocal(var_idx as u16)); //TODO extension
                    source_indices.push(n.position.0);
                    if !self.needs_value() {
                        result.push(Opcode::Pop(1));
                        source_indices.push(n.position.0);
                    }
                }

                Some((VariableType::Boxed, var_idx)) => {
                    result.push(Opcode::LoadLocal(var_idx as u16));
                    source_indices.push(n.position.0);
                    result.push(Opcode::LoadBox);
                    source_indices.push(n.position.0);
                    if !self.needs_value() {
                        result.push(Opcode::Pop(1));
                        source_indices.push(n.position.0);
                    }
                }

                Some((VariableType::Closed, idx)) => {
                    result.push(Opcode::LoadClosureValue(idx as u16));
                    source_indices.push(n.position.0);
                    result.push(Opcode::LoadBox);
                    source_indices.push(n.position.0);
                    if !self.needs_value() {
                        result.push(Opcode::Pop(1));
                        source_indices.push(n.position.0);
                    }
                }

                None => return Err(format!("undeclared variable {}", n.get_string().unwrap())),
            },

            Expr::IfExpr(cond, then_body, else_body) => {
                self.require_value();
                let (mut cond_indices, mut condition) = self.visit_expr(cond)?;
                self.pop_requirement();

                let (mut then_indices, mut then_body) = self.visit_expr(then_body)?;

                let (mut else_indices, mut else_body) = else_body
                    .as_ref()
                    .map(|x| self.visit_expr(x.as_ref()))
                    .unwrap_or_else(|| {
                        Ok(if self.needs_value() {
                            (
                                vec![*cond_indices.get(0).unwrap()],
                                vec![Opcode::LoadImmediateInt(0)],
                            )
                        } else {
                            (vec![*cond_indices.get(0).unwrap()], vec![Opcode::Nop])
                        })
                    })?;

                result.append(&mut condition);
                source_indices.append(&mut cond_indices);

                let then_body_size = then_body.len();
                let else_body_size = else_body.len();

                result.push(Opcode::JumpIfFalse((then_body_size + 1 + 1) as u16));
                source_indices.push(*source_indices.last().unwrap());
                //instruction AFTER then_body and jump

                result.append(&mut then_body);
                source_indices.append(&mut then_indices);

                result.push(Opcode::JumpRelative((else_body_size + 1) as u16));
                source_indices.push(*source_indices.last().unwrap());

                result.append(&mut else_body);
                source_indices.append(&mut else_indices);

                result.push(Opcode::Nop);
                source_indices.push(*source_indices.last().unwrap());
            }

            Expr::Block(block_begin, block_end, block_body) => {
                let (sub_indices, body) = self.visit_block(block_body, block_begin, block_end)?;
                result = body;
                source_indices = sub_indices;
            }

            Expr::Call(target, args) => {
                self.require_value();
                let (mut target_indices, mut target) = self.visit_expr(target)?;
                self.pop_requirement();
                let target_indices_copy = target_indices.clone();

                if !*self.current_function_was_possibly_overwritten.last().unwrap_or(&true)
                    && target.len() == 1 //target is 1 op (not load_* load_box)
                    && target.last().unwrap().eq(&Opcode::LoadLocal(0)) //we load current function
                    && self.needs_return_value()
                //we will return after that
                {
                    let mut argument_indices = vec![];
                    //compute all arguments
                    for (i, arg) in args.iter().enumerate() {
                        self.require_value();
                        let (mut arg_indices, mut arg_code) = self.visit_expr(arg)?;
                        result.append(&mut arg_code);
                        source_indices.append(&mut arg_indices);
                        self.pop_requirement(); //compile arg load
                        argument_indices.push((1 + i) as u16);
                    }
                    //store arguments which are on stack in reverse order
                    for index in argument_indices.into_iter().rev() {
                        result.push(Opcode::StoreLocal(index));
                        source_indices.push(*source_indices.last().unwrap());
                    }
                    //pop locals
                    let locals_to_pop = self.total_variables
                        - 1 //current function
                        - args.len(); //arguments
                    if locals_to_pop != 0 {
                        result.push(Opcode::Pop(locals_to_pop as u16));
                        source_indices.push(*source_indices.last().unwrap());
                    }

                    //jump
                    result.push(Opcode::JumpAbsolute(0));
                    source_indices.push(*source_indices.last().unwrap());
                } else {
                    result.append(&mut target);
                    source_indices.append(&mut target_indices);
                    for arg in args {
                        self.require_value();
                        let (mut arg_indices, mut arg_code) = self.visit_expr(arg)?;
                        result.append(&mut arg_code);
                        source_indices.append(&mut arg_indices);
                        self.pop_requirement();
                    }

                    result.push(Opcode::Call(args.len() as u16));
                    source_indices.push(*target_indices_copy.last().unwrap());

                    if !self.needs_value() {
                        result.push(Opcode::Pop(1));
                        source_indices.push(*source_indices.last().unwrap());
                    }
                }
            }

            Expr::SingleStatement(s) => {
                self.require_value();
                let (sub_indices, body) = self.visit_stmt(s)?;

                self.pop_requirement();
                result = body;
                source_indices = sub_indices;
            }
        }

        Ok((source_indices, result))
    }

    fn visit_block(
        &mut self,
        block: &[Stmt],
        block_begin: &Token,
        block_end: &Token,
    ) -> Result<(Vec<usize>, Vec<Opcode>), String> {
        /*
        block that does not return value:
            stmt(return=false)
            ...
            stmt(return=false)
            pop(n) //locals
        */

        /*
        block that returns value:
            stmt(return=false)
            ...
            stmt(return=true)
            _ = pop(1)
            pop(n)
         */

        /*
        block with single statement
            stmt(return=return)
        */

        self.new_scope(block_begin);

        let mut result = vec![];
        let mut source_indices = vec![];

        if self.needs_value() && !self.needs_return_value() {
            //if we will return after that, then no tmp slot needed, value will just stay on top of stack
            self.define_local("_", VariableType::Normal).unwrap();
            result.push(Opcode::LoadImmediateInt(0)); // _ variable
            source_indices.push(block_begin.position.0);
        }

        let map_iter = (unsafe { (self as *const Compiler).as_ref().unwrap() })
            .variable_types
            .get(block_begin)
            .unwrap();

        for (name, var_type) in map_iter {
            if let VariableType::Boxed = var_type {
                self.define_local(name, VariableType::Boxed);
                result.push(Opcode::NewBox);
                source_indices.push(block_begin.position.0);
            }
        }

        let (last_statement, other_statements) = block.split_last().ok_or("got empty block")?;

        self.require_nothing();
        for item in other_statements {
            let (mut indices, mut stmt_code) = self.visit_stmt(item)?;
            result.append(&mut stmt_code);
            source_indices.append(&mut indices);
        }
        self.pop_requirement();

        let (mut indices, mut last_statement) = self.visit_stmt(last_statement)?;
        result.append(&mut last_statement);
        source_indices.append(&mut indices);
        if self.needs_value() && !self.needs_return_value() {
            let (_, fictional_slot) = self.lookup_local("_").unwrap();
            result.push(Opcode::StoreLocal(fictional_slot as u16));
            source_indices.push(block_end.position.0);
            let scope_variable_count = self.pop_scope();
            result.push(Opcode::Pop((scope_variable_count - 1) as u16));
            source_indices.push(block_end.position.0);
        } else if self.needs_return_value() {
            //do nothing - extra slots will be pop'ed by executing return instruction
        } else {
            let scope_variable_count = self.pop_scope();
            result.push(Opcode::Pop(scope_variable_count as u16));
            source_indices.push(block_end.position.0);
        }

        Ok((source_indices, result))
    }
}
