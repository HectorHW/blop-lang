use crate::compile::compiler::Compiler;
use crate::data::gc::GC;
use crate::data::objects::Value;
use crate::execution::chunk::Chunk;
use crate::execution::vm::VM;
use crate::parsing::ast::Expr;
use peg::error::ParseError;
use std::env;
#[cfg(feature = "bench")]
use std::time::Instant;

extern crate indexmap;

mod compile;
mod data;
mod execution;
mod parsing;
#[cfg(test)]
mod test;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        eprintln!("please provide file to run as argument.");
        return;
    }
    let filename = args.get(1).unwrap();
    let file_content = std::fs::read_to_string(filename).expect("failed to read file");

    //normalize string - make all lines joined with \n (incase running on windows)
    let file_content = file_content.lines().collect::<Vec<_>>().join("\n");
    let tokens = match parsing::lexer::tokenize(&file_content) {
        Ok(v) => v,
        Err(e) => {
            println!("{}", e);
            return;
        }
    };

    #[cfg(feature = "print-tokens")]
    {
        for token in &tokens {
            print!("{}", token.kind);
        }
        println!();
    }

    use parsing::parser::program_parser;

    let statements = match program_parser::program(&tokens) {
        Ok(s) => s,
        Err(ParseError { location, expected }) => {
            println!("{:?}", ParseError { location, expected });
            println!("{:?}", tokens[location]);
            return;
        }
    };

    #[cfg(feature = "print-ast")]
    println!("{:?}", statements);

    let statements = compile::checks::check_optimize(statements).unwrap();

    let (variable_types, closed_names) = compile::syntax_level_check::check(&statements).unwrap();

    #[cfg(feature = "print-ast")]
    println!("{:?}", statements);
    let mut gc = unsafe { GC::default_gc() };
    let entry_point =
        Compiler::compile(&statements, variable_types, closed_names, &mut gc).unwrap();

    #[cfg(feature = "print-chunk")]
    {
        use crate::data::objects::OwnedObjectItem;
        for chunk in gc
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

    let mut vm = VM::new(&mut gc);

    println!("running");

    #[cfg(feature = "bench")]
    let start_time = Instant::now();

    vm.run(entry_point).unwrap_or_else(|error| {
        println!(
            "error {:?} at instruction {}\nat line {}",
            error,
            error.chunk.unwrap_function().unwrap().code[error.opcode_index],
            error.chunk.unwrap_function().unwrap().opcode_to_line[error.opcode_index],
        );
    }); /**/
    #[cfg(feature = "bench")]
    {
        let end_time = Instant::now();
        println!("{:?}", end_time - start_time);
    }
}

fn normalize_string(s: String) -> String {
    s.lines().collect::<Vec<_>>().join("\n")
}

pub fn run_file(filename: &str) -> Result<(), String> {
    let mut gc = unsafe { GC::default_gc() };

    let entry_point = compile_file(filename, &mut gc)?;
    let mut vm = VM::new(&mut gc);
    vm.run(entry_point).map_err(|error| {
        format!(
            "error {:?} at instruction {}\nat line {}",
            error,
            error.chunk.unwrap_function().unwrap().code[error.opcode_index],
            error.chunk.unwrap_function().unwrap().opcode_to_line[error.opcode_index],
        )
    })?;
    Ok(())
}

type CompilationResult = Value;

pub fn compile_file(filename: &str, gc: &mut GC) -> Result<CompilationResult, String> {
    let file_content = std::fs::read_to_string(filename)
        .map_err(|_e| format!("failed to read file {}", filename))?;
    let file_content = normalize_string(file_content);
    let tokens = parsing::lexer::tokenize(&file_content)?;
    use parsing::parser::program_parser;

    let statements: Expr = program_parser::program(&tokens)
        .map_err(|e| format!("{:?}\n{:?}", e, tokens[e.location]))?;

    let statements = compile::checks::check_optimize(statements).unwrap();

    let (variable_types, closed_names) = compile::syntax_level_check::check(&statements)?;
    let chunks = Compiler::compile(&statements, variable_types, closed_names, gc)?;
    Ok(chunks)
}
