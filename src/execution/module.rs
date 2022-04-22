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

pub const FILE_EXTENSION: &str = ".txt";

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
        let buf: PathBuf = m.0.iter().collect();
        buf.with_extension(FILE_EXTENSION)
    }
}

impl Module {
    pub fn from_dot_notation(module_name: &str) -> Self {
        Self(module_name.split('.').map(ToOwned::to_owned).collect())
    }
}

fn normalize_string(s: String) -> String {
    s.replace('\t', "    ") // 4 spaces
        .lines()
        .collect::<Vec<_>>()
        .join("\n")
}

pub fn compile_program(
    program: String,
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

    let pointer = Compiler::compile_module(&statements, annotations, module.clone(), vm.gc)?;

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

pub fn compile_file(filename: &str, vm: &mut VM) -> Result<(String, Value), Box<dyn Error>> {
    let program = match std::fs::read_to_string(filename) {
        Ok(content) => content,
        Err(e) => {
            return Err(format!("{e}").into());
        }
    };

    let module = Module::try_from(Path::new(filename))
        .map_err(|_| format!("failed to build module from path {filename}"))?;

    let pointer = compile_program(program.clone(), &module, vm)?;
    Ok((program, pointer))
}

#[allow(dead_code)]
pub fn run_file(filename: &str) -> Result<(), Box<dyn Error>> {
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
