use crate::compile::checks::{Annotations, VariableType};
use crate::compile::code_blob::AnnotatedCodeBlob;
use crate::data::gc::GC;
use crate::data::objects::{EnumDescriptor, StackObject, StructDescriptor, Value};
use crate::execution::arity::Arity;
use crate::execution::chunk::{Chunk, Opcode};
use crate::execution::module::Module;
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
    pub static ref SCRIPT_TOKEN: Token = Token {
        kind: TokenKind::Name("`script`".to_string()),
        position: Index(0, 0),
    };
    static ref FIELD_INDEX_REGEX: Regex = Regex::new(r"^_\d+$").unwrap();
}

pub struct Compiler<'gc, 'annotations, 'chunk> {
    names: Vec<HashMap<String, (VariableType, bool, usize)>>,
    value_requirements: Vec<ValueRequirement>,
    total_closed_variables: usize,
    stack_height: usize,
    function_context: FunctionCompilationContext,
    current_chunk: &'chunk mut Chunk,
    annotations: &'annotations Annotations,
    gc: &'gc mut GC,
}

struct FunctionCompilationContext {
    arity: Arity,
    name: Token,
}

impl<'gc, 'annotations, 'chunk> Compiler<'gc, 'annotations, 'chunk> {
    fn new(
        annotations: &'annotations Annotations,
        gc: &'gc mut GC,
        function_name: Token,
        function_arity: Arity,
        chunk: &'chunk mut Chunk,
    ) -> Compiler<'gc, 'annotations, 'chunk> {
        Compiler {
            names: vec![],
            value_requirements: vec![],
            total_closed_variables: 0,
            stack_height: 0,
            function_context: FunctionCompilationContext {
                arity: function_arity,
                name: function_name,
            },
            current_chunk: chunk,
            annotations,
            gc,
        }
    }

    pub fn compile_module(
        program: &Program,
        annotations: Annotations,
        module: Module,
        gc: &'gc mut GC,
    ) -> Result<StackObject, String> {
        let mut program_chunk = Chunk::new(SCRIPT_TOKEN.clone(), module, Arity::Exact(0));

        let mut compiler = Compiler::new(
            &annotations,
            gc,
            SCRIPT_TOKEN.clone(),
            Arity::Exact(0),
            &mut program_chunk,
        );

        compiler.new_scope();

        for stmt in program {
            match stmt {
                Stmt::VarDeclaration(name, _)
                | Stmt::FunctionDeclaration { name, .. }
                | Stmt::StructDeclaration { name, .. }
                | Stmt::EnumDeclaration { name, .. } => {
                    compiler.declare_local(name.get_string().unwrap(), VariableType::Global);
                }

                Stmt::Import { name, rename, .. } => {
                    let name = rename.as_ref().unwrap_or(name);
                    compiler.declare_local(name.get_string().unwrap(), VariableType::Global);
                }

                _ => {}
            }
        }

        let (last, other) = program.split_last().unwrap();

        let mut blob = AnnotatedCodeBlob::new();

        for stmt in other {
            compiler.require_nothing();
            blob.append(compiler.visit_stmt(stmt)?);
            compiler.pop_requirement();
        }

        compiler.require_value();

        blob.append(compiler.visit_stmt(last)?);
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
        match self.value_requirements.pop().unwrap() {
            ValueRequirement::Nothing => {}
            ValueRequirement::Value => self.inc_stack_height(),
            ValueRequirement::ReturnValue => self.inc_stack_height(),
        }
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

    fn inc_stack_height(&mut self) {
        self.stack_height += 1;
    }

    fn dec_stack_height(&mut self) {
        self.stack_height -= 1;
    }

    fn sub_stack_height(&mut self, items: usize) {
        self.stack_height -= items;
    }

    fn get_stack_height(&mut self) -> usize {
        self.stack_height
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

    fn get_or_create_import_name(&mut self, import_name: (Module, String)) -> usize {
        for (i, item) in self.current_chunk.import_names.iter().enumerate() {
            if item == &import_name {
                return i;
            }
        }
        self.current_chunk.import_names.push(import_name);
        self.current_chunk.import_names.len() - 1
    }

    /// looks up variable by name, considering only well-defined variables
    /// (i.e. previously declared with var, def, struct or enum)
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

        let var_index;
        if var_type != VariableType::Global {
            var_index = self.get_stack_height();
            //globals do not take stack space
            self.inc_stack_height();
        } else {
            var_index = 0;
        }

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
        self.sub_stack_height(items_in_scope);
        items_in_scope
    }

    fn compile_function(
        &mut self,
        name: &Token,
        args: &[Token],
        vararg: Option<&Token>,
        body: &Expr,
    ) -> Result<StackObject, String> {
        //save current compiler

        let arity = if vararg.is_some() {
            Arity::AtLeast(args.len())
        } else {
            Arity::Exact(args.len())
        };

        let mut chunk = Chunk::new(name.clone(), self.current_chunk.module.clone(), arity);

        let mut inner_compiler =
            Compiler::new(self.annotations, self.gc, name.clone(), arity, &mut chunk);

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
        for arg_name in args.iter().chain(vararg.into_iter()) {
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
                (VariableType::Global, _) => {
                    let idx = self.get_or_create_name(closed_over_value);
                    result.push(Opcode::LoadGlobal(idx as u16), function_name.position.0);
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

    fn create_named_entity<'a>(
        &mut self,
        name: &Token,
        value_emitting_code: &'a dyn Fn(&mut Compiler) -> Result<AnnotatedCodeBlob, String>,
    ) -> Result<AnnotatedCodeBlob, String> {
        let mut result = AnnotatedCodeBlob::new();

        match self.lookup_block(name.get_string().unwrap()) {
            Some((VariableType::Boxed, idx)) => {
                result.push(Opcode::LoadLocal(idx as u16), name.position.0); //load box
                self.inc_stack_height();
                result.append(value_emitting_code(self)?);

                result.push(Opcode::StoreBox, name.position.0);
                self.dec_stack_height(); //consumes two operands
                self.dec_stack_height();
            }
            Some((VariableType::Global, _)) => {
                let idx = self.get_or_create_name(name.get_string().unwrap());
                result.append(value_emitting_code(self)?);
                result.push(Opcode::StoreGLobal(idx as u16), name.position.0);
                self.dec_stack_height();
            }

            None => {
                //variable is not forward-declared => not present on stack yet
                result.append(value_emitting_code(self)?);
                self.dec_stack_height();
                //emitter should inc stack but we do not want to allocate our normal variable twice
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

    fn get_named_entity(&mut self, name: &Token) -> Result<AnnotatedCodeBlob, String> {
        let mut result = AnnotatedCodeBlob::new();
        let line = name.position.0;
        match self.lookup_local(name.get_string().unwrap()) {
            Some((VariableType::Normal, var_idx)) => {
                result.push(Opcode::LoadLocal(var_idx as u16), line);
                //TODO extension
            }

            Some((VariableType::Global, _)) => {
                let idx = self.get_or_create_name(name.get_string().unwrap());
                result.push(Opcode::LoadGlobal(idx as u16), line);
            }

            Some((VariableType::Boxed, var_idx)) => {
                result.push(Opcode::LoadLocal(var_idx as u16), line);
                result.push(Opcode::LoadBox, line);
            }

            Some((VariableType::Closed, idx)) => {
                result.push(Opcode::LoadClosureValue(idx as u16), line);
                result.push(Opcode::LoadBox, line);
            }

            None => {
                //global
                let idx = self.get_or_create_name(name.get_string().unwrap());

                result.push(Opcode::LoadGlobal(idx as u16), line);
            }
        }
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

    fn make_struct(&mut self, name: &Token, fields: &[Token]) -> Result<StackObject, String> {
        let struct_descriptor = StructDescriptor {
            name: name.get_string().unwrap().to_string(),
            fields: fields
                .iter()
                .map(|f| f.get_string().unwrap().to_string())
                .collect(),
            methods: HashMap::new(),
            enum_ref: None,
        };

        Ok(self.gc.store(struct_descriptor))
    }

    fn visit_stmt(&mut self, stmt: &Stmt) -> Result<AnnotatedCodeBlob, String> {
        let mut result = AnnotatedCodeBlob::new();
        match stmt {
            Stmt::VarDeclaration(n, e) => {
                let right_side = |slf: &mut Compiler| {
                    let mut right_side = AnnotatedCodeBlob::new();
                    if e.is_none() {
                        right_side.push(Opcode::LoadNothing, n.position.0);
                        slf.inc_stack_height();
                    } else {
                        slf.require_value();
                        let assignment_body = slf.visit_expr(e.as_ref().unwrap())?;
                        slf.pop_requirement();
                        right_side.append(assignment_body);
                    }
                    Ok(right_side)
                };

                result.append(self.create_named_entity(n, &right_side)?);

                if self.needs_value() {
                    result.push(Opcode::LoadNothing, result.last_index().unwrap());
                    //TODO dup?
                }
            }

            Stmt::EnumDeclaration { name, variants } => {
                let descriptor = EnumDescriptor {
                    name: name.get_string().unwrap().to_string(),
                    variants: Default::default(),
                    methods: Default::default(),
                };

                let descriptor = self.gc.store(descriptor);

                let constant_ref = self.get_or_create_constant(descriptor.clone());

                let right_side = |slf: &mut Compiler| {
                    let mut right_side = AnnotatedCodeBlob::new();
                    right_side += (Opcode::LoadConst(constant_ref as u16), name.position.0);
                    slf.inc_stack_height();
                    Ok(right_side)
                };

                let self_ref = descriptor.clone();

                let descriptor = descriptor.unwrap_enum_descriptor().unwrap();

                for variant in variants {
                    let variant = self.make_struct(&variant.name, &variant.fields)?;
                    descriptor.register_variant(self_ref.clone(), variant);
                }

                result.append(self.create_named_entity(name, &right_side)?);
                if self.needs_value() {
                    result.push(Opcode::LoadNothing, result.last_index().unwrap());
                }
            }

            Stmt::StructDeclaration { name, fields } => {
                let struct_object_pointer = self.make_struct(name, fields)?;
                let constant_idx = self.get_or_create_constant(struct_object_pointer);

                let struct_load_code = |slf: &mut Compiler| {
                    let mut blob = AnnotatedCodeBlob::new();
                    blob.push(Opcode::LoadConst(constant_idx as u16), name.position.0);
                    slf.inc_stack_height();
                    Ok(blob)
                };

                result.append(self.create_named_entity(name, &struct_load_code)?);

                if self.needs_value() {
                    result.push(Opcode::LoadNothing, name.position.0);
                }
            }

            Stmt::ImplBlock {
                name: struct_name,
                implementations,
            } => {
                //load pointer
                result.append(self.get_named_entity(struct_name)?);

                for item in implementations {
                    result.push(Opcode::Duplicate, struct_name.position.0);
                    //pointer
                    match item {
                        Stmt::FunctionDeclaration {
                            name,
                            args,
                            vararg,
                            body,
                        } => {
                            let base_function =
                                self.compile_function(name, args, vararg.as_ref(), body)?;
                            let index = self.get_or_create_constant(base_function);

                            result.push(Opcode::LoadConst(index as u16), name.position.0);
                            result.append(self.close_function(name)?);
                            //function on top of pointer

                            result.push(
                                Opcode::StoreField(
                                    self.get_or_create_name(name.get_string().unwrap()) as u16,
                                ),
                                name.position.0,
                            );

                            //field is stored, pointer is no longer on stack, therefore Duplicate
                        }
                        _ => unreachable!(),
                    }
                }
                if !self.needs_value() {
                    result.push(Opcode::Pop(1), struct_name.position.0);
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
                            self.inc_stack_height();
                        }
                        VariableType::Closed => {
                            result
                                .push(Opcode::LoadClosureValue(var_idx as u16), target.position.0);
                            self.inc_stack_height();
                        }
                        VariableType::Normal | VariableType::Global => {
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
                        VariableType::Global => {
                            let idx = self.get_or_create_name(varname);
                            result.push(Opcode::StoreGLobal(idx as u16), target.position.0);
                        }
                        VariableType::Boxed => {
                            result.push(Opcode::StoreBox, target.position.0);
                        }
                        VariableType::Closed => {
                            result.push(Opcode::StoreBox, target.position.0);
                        }
                    }
                    self.dec_stack_height();
                } else {
                    //otherwise, just put it in global name
                    let idx = self.get_or_create_name(varname);
                    result.push(Opcode::StoreGLobal(idx as u16), target.position.0);
                }
                //in case we need some result value
                if self.needs_value() {
                    result.push(Opcode::LoadNothing, target.position.0);
                }
            }

            Stmt::PropertyAssignment(target, value) => match target {
                Expr::PropertyAccess(target, property) => {
                    self.require_value();
                    let target = self.visit_expr(target)?;
                    self.pop_requirement();

                    self.require_value();
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
                        result.push(Opcode::LoadNothing, property.position.0);
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
                    result.push(Opcode::LoadNothing, token.position.0);
                }
            }

            Stmt::FunctionDeclaration {
                name: function_name,
                args,
                vararg,
                body,
            } => {
                let new_chunk_idx =
                    self.compile_function(function_name, args, vararg.as_ref(), body)?;

                let const_idx = self.get_or_create_constant(new_chunk_idx);

                let function = |slf: &mut Compiler| {
                    let mut function = AnnotatedCodeBlob::new();

                    function.push(
                        Opcode::LoadConst(const_idx as u16),
                        function_name.position.0,
                    ); //code block

                    slf.inc_stack_height();

                    let closing_code = slf.close_function(function_name)?;

                    function.append(closing_code);
                    Ok(function)
                };

                result.append(self.create_named_entity(function_name, &function)?);

                if self.needs_value() {
                    result.push(Opcode::LoadNothing, function_name.position.0);
                }
            }
            Stmt::Pass(token) => {
                result.push(
                    if self.needs_value() {
                        Opcode::LoadNothing
                    } else {
                        Opcode::Nop
                    },
                    token.position.0,
                );
            }

            Stmt::Import {
                module,
                name,
                rename,
            } => {
                let path = module
                    .iter()
                    .map(|t| t.get_string().unwrap().to_string())
                    .collect::<Vec<_>>();
                let module = Module::new(path);

                let idx = self
                    .get_or_create_import_name((module, name.get_string().unwrap().to_string()));
                let import = |slf: &mut Compiler| {
                    let mut importname = AnnotatedCodeBlob::new();

                    importname.push(Opcode::Import(idx as u16), name.position.0); //code block

                    slf.inc_stack_height();

                    Ok(importname)
                };

                result.append(self.create_named_entity(rename.as_ref().unwrap_or(name), &import)?);

                if self.needs_value() {
                    result.push(Opcode::LoadNothing, name.position.0);
                }
            }
        }

        Ok(result)
    }

    fn visit_expr(&mut self, expr: &Expr) -> Result<AnnotatedCodeBlob, String> {
        let mut result = AnnotatedCodeBlob::new();
        match expr {
            Expr::Bool(b) => {
                let value = match b.kind {
                    TokenKind::True => true,
                    TokenKind::False => false,
                    _ => unreachable!(),
                };

                let constant_index = self.get_or_create_constant(value.into());
                result += (Opcode::LoadConst(constant_index as u16), b.position.0);
                if !self.needs_value() {
                    result += (Opcode::Pop(1), b.position.0);
                }
            }

            Expr::FloatNumber(n) => {
                let value = n.get_float().unwrap();
                let constant_index = self.get_or_create_constant(Value::from(value));
                result += (Opcode::LoadConst(constant_index as u16), n.position.0);
                if !self.needs_value() {
                    result += (Opcode::Pop(1), n.position.0);
                }
            }

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
                let obj_ptr = self.gc.new_interned_string(s.get_string().unwrap());
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

                    self.dec_stack_height(); // a is popped if b is evaluated

                    self.require_value();
                    let b = self.visit_expr(b)?;
                    self.pop_requirement();

                    self.dec_stack_height(); // stack height is increased in outer code

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
                    JumpIfFalseOrPop end_and
                    eval(B)
                    end_and:
                     */

                    //eval (A)
                    result.append(a);

                    self.dec_stack_height(); // if b is evaluated then a must be popped

                    self.require_value();
                    let b = self.visit_expr(b)?;
                    self.pop_requirement();

                    self.dec_stack_height(); // stack height is increased in outer code
                                             //jump PAST eval(B)
                    result.push(
                        Opcode::JumpIfFalseOrPop((b.code.len() + 1) as u16),
                        op.position.0,
                    );
                    //eval(B)
                    result.append(b);
                } else {
                    result.append(a);

                    //note that stack height is increased by lhs

                    self.require_value();
                    let b = self.visit_expr(b)?;
                    self.pop_requirement();

                    self.dec_stack_height(); // lhs is removed
                    self.dec_stack_height(); // stack height is increased in outer code

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

            Expr::Name(n) => {
                result.append(self.get_named_entity(n)?);
                if !self.needs_value() {
                    result.push(Opcode::Pop(1), n.position.0);
                }
            }

            Expr::If(cond, then_body, else_body) => {
                self.require_value();
                let condition = self.visit_expr(cond)?;
                self.pop_requirement();

                self.dec_stack_height(); //condition result will be popped on jump

                let then_body = self.visit_expr(then_body)?;

                let else_body = else_body
                    .as_ref()
                    .map(|x| self.visit_expr(x.as_ref()))
                    .unwrap_or_else(|| {
                        let mut blob = AnnotatedCodeBlob::new();
                        if self.needs_value() {
                            {
                                blob += (Opcode::LoadNothing, *condition.indices.get(0).unwrap());
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
                    && usize::from(self.function_context.arity) <= args.len()
                {
                    if !self.function_context.arity.is_vararg()
                        && args.len() > self.function_context.arity.into()
                    {
                        return Err(format!("compile error: arity mismatch when performing tail call: expected {} but got {} args", 
                                           self.function_context.arity,
                                           args.len()
                        ));
                    }

                    self.dec_stack_height(); //no function pointer was loaded

                    let mut argument_indices = vec![];
                    //compute all arguments

                    for (i, arg) in args.iter().enumerate() {
                        self.require_value();
                        let arg_code = self.visit_expr(arg)?;
                        self.pop_requirement(); //compile arg load
                        result.append(arg_code);
                        argument_indices.push((1 + i) as u16);
                    }

                    if let Arity::AtLeast(var_arity) = self.function_context.arity {
                        let listed_args: usize = argument_indices.len() - var_arity;
                        self.sub_stack_height(listed_args);
                        result.push(
                            Opcode::MakeList(listed_args as u16),
                            self.function_context.name.position.0,
                        );
                        self.inc_stack_height();
                        let remaining_args = var_arity + 1usize;
                        argument_indices.truncate(remaining_args);
                    }

                    //store arguments that are on stack in reverse order
                    for &index in argument_indices.iter().rev() {
                        result.push(Opcode::StoreLocal(index), result.last_index().unwrap());
                        self.dec_stack_height();
                    }
                    //pop locals
                    let locals_to_pop = self.get_stack_height()
                        - 1 //current function
                        - argument_indices.len(); //arguments
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
                    self.sub_stack_height(args.len()); //arguments are removed
                    self.dec_stack_height(); // function pointer is removed
                                             //result is (maybe) added in outer code

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
                        self.inc_stack_height();
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

                self.sub_stack_height(args.len());

                if !self.needs_value() {
                    result.push(Opcode::Pop(1), result.last_index().unwrap());
                }
            }

            Expr::SingleStatement(s) => {
                //propagate requirement
                let body = self.visit_stmt(s)?;

                result = body;
            }
            Expr::AnonFunction(args, vararg, name, body) => {
                let new_chunk_idx = self.compile_function(name, args, vararg.as_ref(), body)?;

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
            result += (Opcode::LoadNothing, block_begin.position.0); // _ variable
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

        for item in other_statements {
            self.require_nothing();
            let blob = self.visit_stmt(item)?;
            self.pop_requirement();
            result.append(blob);
        }

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

#[cfg(test)]
mod complex_operators_stack_tests {
    use rstest::*;

    use super::Compiler;
    use crate::{
        compile::compiler::SCRIPT_TOKEN,
        data::gc::GC,
        execution::{arity::Arity, chunk::Chunk, module::Module},
        parsing::{
            ast::Expr,
            lexer::{Index, Token, TokenKind},
        },
    };

    static ZERO: Token = Token {
        position: Index(0, 0),
        kind: TokenKind::Number(0),
    };

    #[fixture]
    pub fn gc() -> GC {
        unsafe { GC::default_gc() }
    }

    fn compile_ast_with_value(mut gc: GC, ast: Expr) {
        let module = Module::from_dot_notation("`TEST`");
        let mut chunk = Chunk::new(SCRIPT_TOKEN.clone(), module, Arity::Exact(0));
        let annotations = Default::default();
        let mut compiler = Compiler::new(
            &annotations,
            &mut gc,
            SCRIPT_TOKEN.clone(),
            Arity::Exact(0),
            &mut chunk,
        );

        compiler.require_value();
        let _ = compiler.visit_expr(&ast);
        compiler.pop_requirement();
        assert_eq!(compiler.stack_height, 1);
    }

    #[rstest]
    fn compiler_should_produce_1_value_in_if(gc: GC) {
        let ast = Expr::If(
            Box::new(Expr::Number(ZERO.clone())),
            Box::new(Expr::Number(ZERO.clone())),
            Some(Box::new(Expr::Number(ZERO.clone()))),
        );
        compile_ast_with_value(gc, ast);
    }

    #[rstest]
    fn compiler_should_produce_1_value_in_or(gc: GC) {
        let ast = Expr::Binary(
            Token {
                kind: TokenKind::Or,
                position: Index(0, 0),
            },
            Box::new(Expr::Number(ZERO.clone())),
            Box::new(Expr::Number(ZERO.clone())),
        );

        compile_ast_with_value(gc, ast);
    }

    #[rstest]
    fn compiler_should_produce_1_value_in_and(gc: GC) {
        let ast = Expr::Binary(
            Token {
                kind: TokenKind::And,
                position: Index(0, 0),
            },
            Box::new(Expr::Number(ZERO.clone())),
            Box::new(Expr::Number(ZERO.clone())),
        );

        compile_ast_with_value(gc, ast);
    }

    #[rstest]
    fn compiler_should_produce_1_value_in_binary(gc: GC) {
        let ast = Expr::Binary(
            Token {
                kind: TokenKind::Plus,
                position: Index(0, 0),
            },
            Box::new(Expr::Number(ZERO.clone())),
            Box::new(Expr::Number(ZERO.clone())),
        );

        compile_ast_with_value(gc, ast);
    }
}
