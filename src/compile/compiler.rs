use crate::compile::checks::{Annotations, VariableType};
use crate::compile::code_blob::AnnotatedCodeBlob;
use crate::data::gc::GC;
use crate::data::objects::{StackObject, StructDescriptor, Value};
use crate::execution::chunk::{Chunk, Opcode};
use crate::parsing::ast::{Expr, Program, Stmt};
use crate::parsing::lexer::{Index, Token, TokenKind};
use regex::Regex;
use std::collections::HashMap;

enum ValueRequirement {
    Nothing,
    Value,
    ReturnValue,
}

lazy_static! {
    static ref SCRIPT_TOKEN: Token = Token {
        kind: TokenKind::Name("`script`".to_string()),
        position: Index(0, 0),
    };
    static ref FIELD_INDEX_REGEX: Regex = Regex::new(r"^_(\d+)$").unwrap();
}

pub struct Compiler<'gc, 'annotations, 'chunk> {
    names: Vec<HashMap<String, (VariableType, bool, usize)>>,
    value_requirements: Vec<ValueRequirement>,
    total_variables: usize,
    total_closed_variables: usize,
    function_context: FunctionCompilationContext,
    current_chunk: &'chunk mut Chunk,
    annotations: &'annotations Annotations,
    gc: &'gc mut GC,
}

struct FunctionCompilationContext {
    arity: usize,
    name: Token,
}

impl<'gc, 'annotations, 'chunk> Compiler<'gc, 'annotations, 'chunk> {
    fn new(
        annotations: &'annotations Annotations,
        gc: &'gc mut GC,
        function_name: Token,
        function_arity: usize,
        chunk: &'chunk mut Chunk,
    ) -> Compiler<'gc, 'annotations, 'chunk> {
        Compiler {
            names: vec![],
            value_requirements: vec![],
            total_variables: 0,
            total_closed_variables: 0,
            function_context: FunctionCompilationContext {
                arity: function_arity,
                name: function_name,
            },
            current_chunk: chunk,
            annotations,
            gc,
        }
    }

    pub fn compile(
        program: &Program,
        annotations: Annotations,
        gc: &'gc mut GC,
    ) -> Result<StackObject, String> {
        let mut program_chunk = Chunk::new(SCRIPT_TOKEN.clone(), 0);

        let mut compiler = Compiler::new(
            &annotations,
            gc,
            SCRIPT_TOKEN.clone(),
            0,
            &mut program_chunk,
        );

        compiler.new_scope();

        compiler.require_value();
        let mut blob = compiler.visit_expr(program)?;
        compiler.pop_requirement();

        blob += (Opcode::Return, 0);

        program_chunk.append(blob);

        let pointer = gc.store(program_chunk);
        Ok(pointer)
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

    fn get_or_create_name(&mut self, name: &str) -> usize {
        for (i, item) in self.current_chunk.global_names.iter().enumerate() {
            if item == name {
                return i;
            }
        }
        self.current_chunk.global_names.push(name.to_string());
        self.current_chunk.global_names.len() - 1
    }

    fn get_or_create_constant(&mut self, constant: Value) -> usize {
        for (i, item) in self.current_chunk.constants.iter().enumerate() {
            if item == &constant {
                return i;
            }
        }
        self.current_chunk.constants.push(constant);
        self.current_chunk.constants.len() - 1
    }

    /// looks up variable by name, considering only well-defined variables
    /// (i.e. previously declared with var or def)
    fn lookup_local(&self, name: &str) -> Option<(VariableType, usize)> {
        for scope in self.names.iter().rev() {
            if let Some((var_type, true, var_idx)) = scope.get(name) {
                return Some((*var_type, *var_idx));
            }
        }
        None
    }

    /// looks up variable by name, searching for nearest declaration including forward declarations
    fn lookup_uninit_local(&self, name: &str) -> Option<(VariableType, usize)> {
        for scope in self.names.iter().rev() {
            if let Some((var_type, _any_state, var_idx)) = scope.get(name) {
                return Some((*var_type, *var_idx));
            }
        }
        None
    }

    fn lookup_block(&self, name: &str) -> Option<(VariableType, usize)> {
        self.names
            .last()
            .unwrap()
            .get(name)
            .cloned()
            .map(|t| (t.0, t.2))
    }

    fn declare_local(
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
            .insert(variable_name.to_string(), (var_type, false, var_index));
        Some((var_type, var_index))
    }

    fn define_local(&mut self, variable_name: &str) {
        self.names
            .last_mut()
            .unwrap()
            .entry(variable_name.to_string())
            .and_modify(|v| {
                v.1 = true;
            });
    }

    fn define_closed_variable(&mut self, variable_name: &str) -> Option<(VariableType, usize)> {
        let idx = self.total_closed_variables;
        self.total_closed_variables += 1;
        self.names
            .last_mut()
            .unwrap()
            .insert(variable_name.to_string(), (VariableType::Closed, true, idx))
            .map(|t| (t.0, t.2))
    }

    fn new_scope(&mut self) {
        self.names.push(HashMap::new());
    }

    fn pop_scope(&mut self) -> usize {
        let scope = self.names.pop().unwrap();
        let items_in_scope = scope.len();
        drop(scope);
        self.total_variables -= items_in_scope;
        items_in_scope
    }

    fn compile_function(
        &mut self,
        name: &Token,
        args: &[Token],
        body: &Expr,
    ) -> Result<StackObject, String> {
        //save current compiler

        let mut chunk = Chunk::new(name.clone(), args.len());

        let mut inner_compiler = Compiler::new(
            self.annotations,
            self.gc,
            name.clone(),
            args.len(),
            &mut chunk,
        );

        //compile body

        inner_compiler.new_scope();

        let closures = inner_compiler.annotations.get_closure_scope(name).unwrap();

        for closed_variable in closures {
            inner_compiler.define_closed_variable(closed_variable);
        } //define closed values

        inner_compiler.new_scope();

        inner_compiler
            .declare_local(name.get_string().unwrap(), VariableType::Normal)
            .unwrap();
        inner_compiler.define_local(name.get_string().unwrap());
        //define function inside itself

        inner_compiler.new_scope();
        for arg_name in args {
            match inner_compiler.declare_local(arg_name.get_string().unwrap(), VariableType::Normal)
            {
                Some(_) => {
                    inner_compiler.define_local(arg_name.get_string().unwrap());
                }
                None => {
                    return Err(format!(
                        "argument {} repeats in function {}",
                        arg_name.get_string().unwrap(),
                        name.get_string().unwrap()
                    ));
                }
            }
        }
        //define arguments

        inner_compiler.new_scope();

        let mut closed_arguments = 0;

        let mut current_chunk = AnnotatedCodeBlob::new();

        for arg in args {
            if let VariableType::Boxed = inner_compiler
                .annotations
                .get_block_scope(name)
                .unwrap()
                .get(arg.get_string().unwrap())
                .unwrap()
            {
                let (_, real_idx) = inner_compiler
                    .lookup_local(arg.get_string().unwrap())
                    .unwrap();
                current_chunk += (Opcode::NewBox, name.position.0);
                current_chunk += (Opcode::Duplicate, name.position.0);
                current_chunk += (Opcode::LoadLocal(real_idx as u16), name.position.0);
                current_chunk += (Opcode::StoreBox, name.position.0);
                inner_compiler.declare_local(arg.get_string().unwrap(), VariableType::Boxed);
                inner_compiler.define_local(arg.get_string().unwrap());
                closed_arguments += 1;
            }
        }

        if closed_arguments == 0 {
            inner_compiler.pop_scope();
        }

        inner_compiler.require_return_value();
        let body = inner_compiler.visit_expr(body)?;
        inner_compiler.pop_requirement();
        current_chunk = current_chunk + body;

        let return_index = current_chunk.last_index().unwrap();

        current_chunk += (Opcode::Return, return_index);

        chunk.append(current_chunk);

        let pointer = self.gc.store(chunk);
        Ok(pointer)
    }

    fn close_function(&mut self, function_name: &Token) -> Result<AnnotatedCodeBlob, String> {
        let mut result = AnnotatedCodeBlob::new();
        if self
            .annotations
            .get_closure_scope(function_name)
            .unwrap()
            .is_empty()
        {
            return Ok(result);
        }

        result.push(Opcode::NewClosure, function_name.position.0);

        let map_iter = (unsafe { (self as *const Compiler).as_ref().unwrap() })
            .annotations
            .get_closure_scope(function_name)
            .unwrap();

        for closed_over_value in map_iter {
            let name = self.lookup_uninit_local(closed_over_value).unwrap();

            match name {
                (VariableType::Normal, var_idx) => {
                    result.push(Opcode::LoadLocal(var_idx as u16), function_name.position.0);
                    //TODO extension
                }
                (VariableType::Boxed, var_idx) => {
                    result.push(Opcode::LoadLocal(var_idx as u16), function_name.position.0);
                }
                (VariableType::Closed, idx) => {
                    result.push(
                        Opcode::LoadClosureValue(idx as u16),
                        function_name.position.0,
                    );
                }
            };
            result.push(Opcode::AddClosedValue, function_name.position.0);
        }

        Ok(result)
    }

    fn create_named_entity(
        &mut self,
        name: &Token,
        value_emitting_code: AnnotatedCodeBlob,
    ) -> Result<AnnotatedCodeBlob, String> {
        let mut result = AnnotatedCodeBlob::new();

        match self.lookup_block(name.get_string().unwrap()) {
            Some((VariableType::Boxed, idx)) => {
                result.push(Opcode::LoadLocal(idx as u16), name.position.0); //load box

                result.append(value_emitting_code);

                result.push(Opcode::StoreBox, name.position.0);
            }

            None => {
                //variable is not forward-declared => not present on stack yet
                result.append(value_emitting_code);
                let varname = name.get_string().unwrap();
                let _ = self
                    .declare_local(varname, VariableType::Normal)
                    .ok_or(format!("redefinition of variable {}", varname))?;
            }
            _a => panic!("{:?}", _a),
        }
        self.define_local(name.get_string().unwrap());

        Ok(result)
    }

    fn try_parse_special_field_access(property: &Token) -> Result<Option<u16>, String> {
        if FIELD_INDEX_REGEX.is_match(property.get_string().unwrap()) {
            let idx = (property.get_string().unwrap()[1..])
                .parse::<u16>()
                .map_err(|_e| {
                    format!(
                        "{}: index too big [{}]",
                        property.get_string().unwrap(),
                        property.position
                    )
                })?;

            Ok(Some(idx))
        } else {
            Ok(None)
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<AnnotatedCodeBlob, String> {
        let mut result = AnnotatedCodeBlob::new();
        match stmt {
            Stmt::Print(t, e) => {
                self.require_value();
                let compiled_expression = self.visit_expr(e)?;
                self.pop_requirement();
                result.append(compiled_expression);
                result.push(Opcode::Print, t.position.0);

                if self.needs_value() {
                    result.push(Opcode::LoadImmediateInt(0), t.position.0);
                }
            }

            Stmt::VarDeclaration(n, e) => {
                let mut right_side = AnnotatedCodeBlob::new();

                if e.is_none() {
                    right_side.push(Opcode::LoadImmediateInt(0), n.position.0);
                } else {
                    self.require_value();
                    let assignment_body = self.visit_expr(e.as_ref().unwrap())?;
                    self.pop_requirement();
                    right_side.append(assignment_body);
                }

                result.append(self.create_named_entity(n, right_side)?);

                if self.needs_value() {
                    result.push(Opcode::LoadImmediateInt(0), result.last_index().unwrap());
                    //TODO dup?
                }
            }

            Stmt::StructDeclaration { name, fields } => {
                let struct_descriptor = StructDescriptor {
                    name: name.get_string().unwrap().to_string(),
                    fields: fields
                        .iter()
                        .map(|f| f.get_string().unwrap().to_string())
                        .collect(),
                };

                let struct_object_pointer = self.gc.store(struct_descriptor);
                let constant_idx = self.get_or_create_constant(struct_object_pointer);

                let struct_load_code = {
                    let mut blob = AnnotatedCodeBlob::new();
                    blob.push(Opcode::LoadConst(constant_idx as u16), name.position.0);
                    blob
                };

                result.append(self.create_named_entity(name, struct_load_code)?);

                if self.needs_value() {
                    result.push(Opcode::LoadImmediateInt(0), name.position.0);
                }
            }

            Stmt::Assignment(target, expr) => {
                let varname = target.get_string().unwrap();

                //check for self-assignment which is prohibited
                if self.function_context.name != *SCRIPT_TOKEN {
                    match self.lookup_local(varname) {
                        Some((VariableType::Closed, _)) => {} // current function cannot be in `closed`
                        Some((_any_other_type, 0)) => {
                            return Err(format!(
                                "cannot assign to function inside itself. Maybe try shadowing? [{}]",
                                target.position
                            ));
                        }
                        _ => {}
                    }
                }

                //if working with local variable, we may need to load pointer for box storing
                if let Some((var_type, var_idx)) = self.lookup_local(varname) {
                    //maybe we need to load pointer
                    match var_type {
                        VariableType::Boxed => {
                            result.push(Opcode::LoadLocal(var_idx as u16), target.position.0);
                        }
                        VariableType::Closed => {
                            result
                                .push(Opcode::LoadClosureValue(var_idx as u16), target.position.0);
                        }
                        VariableType::Normal => {
                            //nothing
                        }
                    }
                }
                //compile assignment target
                self.require_value();
                let expr_body = self.visit_expr(expr)?;
                self.pop_requirement();

                result.append(expr_body);

                //value is now on stack
                if let Some((var_type, var_idx)) = self.lookup_local(varname) {
                    //if we are storing it in local slot, emit instruction depending on type
                    match var_type {
                        VariableType::Normal => {
                            result.push(Opcode::StoreLocal(var_idx as u16), target.position.0);
                            //TODO extension
                        }
                        VariableType::Boxed => {
                            result.push(Opcode::StoreBox, target.position.0);
                        }
                        VariableType::Closed => {
                            result.push(Opcode::StoreBox, target.position.0);
                        }
                    }
                } else {
                    //otherwise, just put it in global name
                    let idx = self.get_or_create_name(varname);
                    result.push(Opcode::StoreGLobal(idx as u16), target.position.0);
                }
                //in case we need some result value
                if self.needs_value() {
                    result.push(Opcode::LoadImmediateInt(0), target.position.0);
                }
            }

            Stmt::PropertyAssignment(target, value) => match target {
                Expr::PropertyAccess(target, property) => {
                    self.require_value();
                    let target = self.visit_expr(target)?;
                    let value = self.visit_expr(value)?;
                    self.pop_requirement();

                    result.append(target); //load pointer
                    result.append(value); // value on top of pointer

                    //special field index access
                    result += (
                        match Compiler::try_parse_special_field_access(property)? {
                            Some(idx) => Opcode::StoreFieldByIndex(idx),
                            None => {
                                let name_idx =
                                    self.get_or_create_name(property.get_string().unwrap());
                                Opcode::StoreField(name_idx as u16)
                            }
                        },
                        property.position.0,
                    );

                    if self.needs_value() {
                        result.push(Opcode::LoadImmediateInt(0), property.position.0);
                    }
                }

                other => return Err(format!("unsupported assignment target {:?}", other)),
            },

            Stmt::Expression(e) => {
                let body = self.visit_expr(e)?;
                result.append(body);
            }

            Stmt::Assert(token, expr) => {
                self.require_value();
                let body = self.visit_expr(expr)?;
                self.pop_requirement();
                result.append(body);
                result.push(Opcode::Assert, token.position.0);
                if self.needs_value() {
                    result.push(Opcode::LoadImmediateInt(0), token.position.0);
                }
            }

            Stmt::FunctionDeclaration {
                name: function_name,
                args,
                body,
            } => {
                let new_chunk_idx = self.compile_function(function_name, args, body)?;

                let const_idx = self.get_or_create_constant(new_chunk_idx);

                let mut function = AnnotatedCodeBlob::new();

                function.push(
                    Opcode::LoadConst(const_idx as u16),
                    function_name.position.0,
                ); //code block

                let closing_code = self.close_function(function_name)?;

                function.append(closing_code);

                result.append(self.create_named_entity(function_name, function)?);

                if self.needs_value() {
                    result.push(Opcode::LoadImmediateInt(0), function_name.position.0);
                }
            }
            Stmt::Pass(token) => {
                result.push(
                    if self.needs_value() {
                        Opcode::LoadImmediateInt(0)
                    } else {
                        Opcode::Nop
                    },
                    token.position.0,
                );
            }
        }

        Ok(result)
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<AnnotatedCodeBlob, String> {
        let mut result = AnnotatedCodeBlob::new();
        match expr {
            Expr::Number(token) => {
                let n = token.get_number().unwrap();
                if n >= (i16::MIN as i64) && n <= (i16::MAX as i64) {
                    result += (Opcode::LoadImmediateInt(n as i16), token.position.0);
                } else {
                    let constant_index = self.get_or_create_constant(Value::Int(n));
                    result += (Opcode::LoadConst(constant_index as u16), token.position.0);
                    //TODO extension
                }
                if !self.needs_value() {
                    result += (Opcode::Pop(1), token.position.0);
                }
            }
            Expr::ConstString(s) => {
                let obj_ptr = self.gc.new_const_string(s.get_string().unwrap());
                let constant_index = self.get_or_create_constant(obj_ptr);
                result.push(Opcode::LoadConst(constant_index as u16), s.position.0);
                if !self.needs_value() {
                    result.push(Opcode::Pop(1), s.position.0);
                }
            }

            Expr::Unary(op, a) => {
                self.require_value();
                let expr = self.visit_expr(a)?;
                self.pop_requirement();

                result.append(expr);

                result.push(
                    match &op.kind {
                        TokenKind::Not => Opcode::LogicalNot,
                        other => {
                            panic!("unimplemented unary operator {} [{}]", other, op.position)
                        }
                    },
                    op.position.0,
                );

                if !self.needs_value() {
                    result.push(Opcode::Pop(1), op.position.0);
                }
            }

            Expr::Binary(op, a, b) => {
                self.require_value();
                let a = self.visit_expr(a)?;
                let b = self.visit_expr(b)?;
                self.pop_requirement();

                if let TokenKind::Or = op.kind {
                    /*
                    evaluation scheme:
                    eval(A)
                    JumpIfTrue end_or
                    eval(B)
                    end_or:
                     */

                    //eval (A)
                    result.append(a);
                    //jump PAST eval(B)
                    result.push(
                        Opcode::JumpIfTrueOrPop((b.code.len() + 1) as u16),
                        op.position.0,
                    );
                    //eval(B)
                    result.append(b);
                } else if let TokenKind::And = op.kind {
                    /*
                    evaluation scheme:
                    eval(A)
                    JumpIfFalse end_and
                    eval(B)
                    end_or:
                     */

                    //eval (A)
                    result.append(a);
                    //jump PAST eval(B)
                    result.push(
                        Opcode::JumpIfFalseOrPop((b.code.len() + 1) as u16),
                        op.position.0,
                    );
                    //eval(B)
                    result.append(b);
                } else {
                    result.append(a);

                    result.append(b);
                    result.push(
                        match &op.kind {
                            TokenKind::Star => Opcode::Mul,
                            TokenKind::Slash => Opcode::Div,
                            TokenKind::Plus => Opcode::Add,
                            TokenKind::Minus => Opcode::Sub,
                            TokenKind::CompareEquals => Opcode::TestEquals,
                            TokenKind::CompareNotEquals => Opcode::TestNotEquals,
                            TokenKind::CompareGreater => Opcode::TestGreater,
                            TokenKind::CompareGreaterEqual => Opcode::TestGreaterEqual,
                            TokenKind::CompareLess => Opcode::TestLess,
                            TokenKind::CompareLessEqual => Opcode::TestLessEqual,
                            TokenKind::Mod => Opcode::Mod,
                            TokenKind::Power => Opcode::Power,
                            other => {
                                panic!("unimplemented binary operator {} [{}]", other, op.position)
                            }
                        },
                        op.position.0,
                    );
                }

                if !self.needs_value() {
                    result.push(Opcode::Pop(1), op.position.0);
                }
            }

            Expr::Name(n) => match self.lookup_local(n.get_string().unwrap()) {
                Some((VariableType::Normal, var_idx)) => {
                    result.push(Opcode::LoadLocal(var_idx as u16), n.position.0); //TODO extension
                    if !self.needs_value() {
                        result.push(Opcode::Pop(1), n.position.0);
                    }
                }

                Some((VariableType::Boxed, var_idx)) => {
                    result.push(Opcode::LoadLocal(var_idx as u16), n.position.0);
                    result.push(Opcode::LoadBox, n.position.0);
                    if !self.needs_value() {
                        result.push(Opcode::Pop(1), n.position.0);
                    }
                }

                Some((VariableType::Closed, idx)) => {
                    result.push(Opcode::LoadClosureValue(idx as u16), n.position.0);
                    result.push(Opcode::LoadBox, n.position.0);
                    if !self.needs_value() {
                        result.push(Opcode::Pop(1), n.position.0);
                    }
                }

                None => {
                    //global
                    let idx = self.get_or_create_name(n.get_string().unwrap());

                    result.push(Opcode::LoadGlobal(idx as u16), n.position.0);

                    if !self.needs_value() {
                        result.push(Opcode::Pop(1), n.position.0);
                    }
                }
            },

            Expr::If(cond, then_body, else_body) => {
                self.require_value();
                let condition = self.visit_expr(cond)?;
                self.pop_requirement();

                let then_body = self.visit_expr(then_body)?;

                let else_body = else_body
                    .as_ref()
                    .map(|x| self.visit_expr(x.as_ref()))
                    .unwrap_or_else(|| {
                        let mut blob = AnnotatedCodeBlob::new();
                        if self.needs_value() {
                            {
                                blob += (
                                    Opcode::LoadImmediateInt(0),
                                    *condition.indices.get(0).unwrap(),
                                );
                            }
                        } else {
                            blob += (Opcode::Nop, *condition.indices.get(0).unwrap());
                        }
                        Ok(blob)
                    })?;

                result.append(condition);
                let then_body_size = then_body.code.len();
                let else_body_size = else_body.code.len();

                result.push(
                    Opcode::JumpIfFalseOrPop((then_body_size + 1 + 1) as u16),
                    *result.indices.last().unwrap(),
                );
                //instruction AFTER then_body and jump

                result.append(then_body);

                result.push(
                    Opcode::JumpRelative((1 + else_body_size + 1) as u16),
                    *result.indices.last().unwrap(),
                );
                //jump PAST POP else_body

                result.push(Opcode::Pop(1), *else_body.indices.first().unwrap());

                result.append(else_body);
                result.push(Opcode::Nop, *result.indices.last().unwrap());
            }

            Expr::Block(block_begin, block_end, block_body) => {
                let body = self.visit_block(block_body, block_begin, block_end)?;
                result = body;
            }

            Expr::Call(target, args) => {
                self.require_value();
                let target = self.visit_expr(target)?;
                self.pop_requirement();
                let target_indices_copy = target.clone();

                if self.function_context.name != *SCRIPT_TOKEN //we are compiling some function other that entry point 
                    && target.code.len() == 1 //target is 1 op (not load_* load_box)
                    && target.code.last().unwrap().eq(&Opcode::LoadLocal(0)) //we load current function
                    && self.needs_return_value()
                //we will return after that
                    && self.function_context.arity <= args.len()
                {
                    if args.len() > self.function_context.arity {
                        return Err(format!("compile error: arity mismatch when performing tail call: expected <={} but got {} args", 
                                           self.function_context.arity,
                                           args.len()
                        ));
                    }

                    let mut argument_indices = vec![];
                    //compute all arguments

                    for (i, arg) in args.iter().enumerate() {
                        self.require_value();
                        let arg_code = self.visit_expr(arg)?;
                        result.append(arg_code);
                        self.pop_requirement(); //compile arg load
                        argument_indices.push((1 + i) as u16);
                    }
                    //store arguments which are on stack in reverse order
                    for index in argument_indices.into_iter().rev() {
                        result.push(Opcode::StoreLocal(index), result.last_index().unwrap());
                    }
                    //pop locals
                    let locals_to_pop = self.total_variables
                        - 1 //current function
                        - args.len(); //arguments
                    if locals_to_pop != 0 {
                        result.push(
                            Opcode::Pop(locals_to_pop as u16),
                            result.last_index().unwrap(),
                        );
                    }

                    //jump
                    result.push(Opcode::JumpAbsolute(0), result.last_index().unwrap());
                    result.make_last_absolute();
                } else {
                    result.append(target);
                    for arg in args {
                        self.require_value();
                        let arg_code = self.visit_expr(arg)?;
                        result.append(arg_code);
                        self.pop_requirement();
                    }

                    result.push(
                        Opcode::Call(args.len() as u16),
                        target_indices_copy.last_index().unwrap(),
                    );

                    if !self.needs_value() {
                        result.push(Opcode::Pop(1), result.last_index().unwrap());
                    }
                }
            }

            Expr::PartialCall(target, args) => {
                self.require_value();
                let target = self.visit_expr(target)?;
                self.pop_requirement();
                let target_indices_copy = target.clone();

                result.append(target);

                for arg in args {
                    if arg.is_none() {
                        result.push(Opcode::LoadBlank, result.last_index().unwrap());
                        continue;
                    }
                    self.require_value();
                    let arg_code = self.visit_expr(arg.as_ref().unwrap())?;
                    result.append(arg_code);
                    self.pop_requirement();
                }

                result.push(
                    Opcode::CallPartial(args.len() as u16),
                    target_indices_copy.last_index().unwrap(),
                );

                if !self.needs_value() {
                    result.push(Opcode::Pop(1), result.last_index().unwrap());
                }
            }

            Expr::SingleStatement(s) => {
                //propagate requirement
                let body = self.visit_stmt(s)?;

                result = body;
            }
            Expr::AnonFunction(args, name, body) => {
                let new_chunk_idx = self.compile_function(name, args, body)?;

                let const_idx = self.get_or_create_constant(new_chunk_idx);

                result.push(Opcode::LoadConst(const_idx as u16), name.position.0); //code block

                let code = self.close_function(name)?;

                result.append(code);

                if !self.needs_value() {
                    result.push(Opcode::Pop(1), name.position.0);
                }
            }
            Expr::PropertyAccess(target, prop) => {
                self.require_value();
                let target = self.visit_expr(target.as_ref())?;
                self.pop_requirement();

                result.append(target);

                result += (
                    match Compiler::try_parse_special_field_access(prop)? {
                        Some(idx) => Opcode::LoadFieldByIndex(idx),
                        None => {
                            let idx = self.get_or_create_name(prop.get_string().unwrap());
                            Opcode::LoadField(idx as u16)
                        }
                    },
                    prop.position.0,
                );

                if !self.needs_value() {
                    result.push(Opcode::Pop(1), prop.position.0);
                }
            }

            Expr::PropertyTest(target, prop) => {
                self.require_value();
                let target = self.visit_expr(target.as_ref())?;
                self.pop_requirement();

                result.append(target);

                let idx = self.get_or_create_name(prop.get_string().unwrap());

                result.push(Opcode::TestProperty(idx as u16), prop.position.0);

                if !self.needs_value() {
                    result.push(Opcode::Pop(1), prop.position.0);
                }
            }
        }

        Ok(result)
    }

    fn visit_block(
        &mut self,
        block: &[Stmt],
        block_begin: &Token,
        block_end: &Token,
    ) -> Result<AnnotatedCodeBlob, String> {
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

        let fictive_variable_name = "`_`";

        self.new_scope();

        let mut result = AnnotatedCodeBlob::new();

        if self.needs_value() && !self.needs_return_value() {
            //if we will return after that, then no tmp slot needed, value will just stay on top of stack
            self.declare_local(fictive_variable_name, VariableType::Normal)
                .unwrap();
            self.define_local(fictive_variable_name);
            result += (Opcode::LoadImmediateInt(0), block_begin.position.0); // _ variable
        }

        let predeclared_names = self.annotations.get_block_scope(block_begin).unwrap();

        for (name, var_type) in predeclared_names {
            if let VariableType::Boxed = var_type {
                self.declare_local(name, VariableType::Boxed);
                //do not define yet
                result += (Opcode::NewBox, block_begin.position.0);
            }
        }

        let (last_statement, other_statements) = block.split_last().ok_or("got empty block")?;

        self.require_nothing();
        for item in other_statements {
            let blob = self.visit_stmt(item)?;
            result.append(blob);
        }
        self.pop_requirement();

        let last_statement = self.visit_stmt(last_statement)?;
        result.append(last_statement);

        if self.needs_value() && !self.needs_return_value() {
            let (_, fictional_slot) = self.lookup_local(fictive_variable_name).unwrap();
            result += (
                Opcode::StoreLocal(fictional_slot as u16),
                block_end.position.0,
            );
            let scope_variable_count = self.pop_scope();
            result += (
                Opcode::Pop((scope_variable_count - 1) as u16),
                block_end.position.0,
            );
        } else if self.needs_return_value() {
            //do nothing - extra slots will be pop'ed by executing return instruction
        } else {
            let scope_variable_count = self.pop_scope();
            result += (
                Opcode::Pop(scope_variable_count as u16),
                block_end.position.0,
            );
        }

        Ok(result)
    }
}
