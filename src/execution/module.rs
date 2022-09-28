use std::{
    error::Error,
    path::{Component, Path, PathBuf},
};

use crate::{
    compile::compiler::Compiler,
    data::{gc::GC, objects::Value},
    parsing,
};

use super::vm::VM;

pub const FILE_EXTENSION: &str = "txt";

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Module(Vec<String>);

impl TryFrom<&Path> for Module {
    type Error = Box<dyn Error>;

    fn try_from(value: &Path) -> Result<Self, Self::Error> {
        value
            .components()
            .flat_map(|c| match c {
                Component::CurDir => None,
                Component::Normal(s) => Some(
                    Path::new(s)
                        .file_stem()
                        .and_then(|s| s.to_str())
                        .map(ToOwned::to_owned)
                        .ok_or_else(|| "failed to build module path".to_string().into()),
                ),
                other => Some(Err(
                    format!("failed to build module path on {other:?}").into()
                )),
            })
            .collect::<Result<Vec<String>, Box<dyn Error>>>()
            .map(Self)
    }
}

impl From<&Module> for PathBuf {
    fn from(m: &Module) -> Self {
        let (head, tail) = m.0.split_last().unwrap();

        let mut buf: PathBuf = tail.iter().collect();
        buf.push(head);
        buf.set_extension(FILE_EXTENSION);
        buf
    }
}

impl Module {
    pub fn new(items: Vec<String>) -> Self {
        Self(items)
    }

    pub fn from_dot_notation(module_name: &str) -> Self {
        Self(module_name.split('.').map(ToOwned::to_owned).collect())
    }
}

pub fn normalize_string(s: &str) -> String {
    s.replace('\t', "    ") // 4 spaces
        .lines()
        .map(str::trim_end)
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn compile_program(
    program: &str,
    module: &Module,
    vm: &mut VM,
) -> Result<Value, Box<dyn Error>> {
    let file_content = normalize_string(program);
    let tokens = parsing::lexer::tokenize(&file_content)?;

    #[cfg(feature = "print-tokens")]
    {
        for token in &tokens {
            print!("{}", token.kind);
        }
        println!();
    }

    use parsing::parser::program_parser;

    let tokens = tokens.iter().collect::<Vec<_>>();

    let statements: Vec<parsing::ast::Stmt> = program_parser::program(tokens.as_slice())
        .map_err(|e| format!("{:?}\n{:?}", e, tokens[e.location]))?;

    let (statements, annotations) = crate::compile::checks::check_optimize(statements)?;

    #[cfg(feature = "print-ast")]
    println!("{:?}", statements);

    #[cfg(feature = "print-annotations")]
    println!("ANNOTATIONS:\n{annotations:?}");

    let types = crate::compile::typecheck::typecheck(&statements, &annotations)
        .map_err(|e| format!("{:?}", e))?;

    let pointer =
        Compiler::compile_module(&statements, &annotations, &types, module.clone(), vm.gc)?;

    vm.maybe_create_module(module);

    #[cfg(feature = "print-chunk")]
    {
        use crate::data::objects::OwnedObjectItem;
        for chunk in vm
            .gc
            .items()
            .filter(|p| matches!(p.item, OwnedObjectItem::Function(..)))
        {
            println!("{:?}", chunk);
            match &chunk.item {
                OwnedObjectItem::Function(chunk) => {
                    println!("{}", chunk)
                }
                _ => unreachable!(),
            }
        }
    }

    Ok(pointer)
}

pub fn compile_file(file_path: &Path, vm: &mut VM) -> Result<(String, Value), Box<dyn Error>> {
    let program = match std::fs::read_to_string(file_path) {
        Ok(content) => content,
        Err(e) => {
            return Err(format!("{e} ({})", file_path.display()).into());
        }
    };

    let module = Module::try_from(file_path)
        .map_err(|_| format!("failed to build module from path {file_path:?}"))?;

    let pointer = compile_program(&program, &module, vm)?;
    Ok((program, pointer))
}

#[allow(dead_code)]
pub fn run_file(filename: &Path) -> Result<(), Box<dyn Error>> {
    let mut gc = unsafe { GC::default_gc() };
    let builtins = super::builtins::builtin_factory();

    let mut vm = VM::new(&mut gc, &builtins);

    let (source_code, pointer) = compile_file(filename, &mut vm)?;
    exec_with_error_printing(&mut vm, pointer, source_code.as_str()).map(|_| ())
}

pub fn exec_with_error_printing(
    vm: &mut VM,
    pointer: Value,
    source_code: &str,
) -> Result<Value, Box<dyn Error>> {
    vm.run(pointer)
        .map_err(|e| crate::display_error(source_code, e).into())
}

#[cfg(test)]
mod test {
    use std::path::{Path, PathBuf};

    use crate::execution::module::FILE_EXTENSION;

    use super::Module;

    #[test]
    fn module_should_build_from_dots() {
        let module = Module::from_dot_notation("a.b.c");
        assert_eq!(
            module,
            Module(vec!["a".to_string(), "b".to_string(), "c".to_string()])
        )
    }

    #[test]
    fn module_should_build_from_path() {
        let module: Module = Path::new("std/option.txt").try_into().unwrap();
        assert_eq!(
            module,
            Module(vec!["std".to_string(), "option".to_string()])
        );
    }

    #[test]
    fn module_should_convert_into_path() {
        let module: &Module = &Module(vec!["std".into(), "option".into()]);
        let path: PathBuf = module.into();

        let mut expected = PathBuf::new();

        expected.push("std");
        expected.push("option");
        expected = expected.with_extension(FILE_EXTENSION);

        assert_eq!(path, expected);
    }
}
