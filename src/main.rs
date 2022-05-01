use crate::data::gc::GC;

use crate::execution::builtins::builtin_factory;
use crate::execution::chunk::Chunk;
use crate::execution::module::{compile_file, compile_program, Module};
use crate::execution::vm::VM;
use crate::parsing::ast::Expr;
use execution::chunk::Opcode;
use execution::vm::InterpretError;

use std::env;
use std::fmt::Write;
use std::io::{stdin, BufRead};
use std::path::Path;
#[cfg(feature = "bench")]
use std::time::Instant;

extern crate indexmap;
extern crate regex;

#[macro_use]
extern crate lazy_static;

mod compile;
mod data;
mod execution;
mod parsing;
#[cfg(test)]
mod test;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        run_repl();
        return;
    }
    let filename = args.get(1).unwrap();

    let mut gc = unsafe { GC::default_gc() };
    let builtins = builtin_factory();

    let mut vm = VM::new(&mut gc, &builtins);

    let (source, pointer) = compile_file(Path::new(filename), &mut vm).unwrap();

    println!("running");

    #[cfg(feature = "bench")]
    let start_time = Instant::now();

    let _ = vm
        .run(pointer)
        .map_err(|error| eprintln!("\n{}", display_error(source.as_str(), error)))
        .unwrap();
    #[cfg(feature = "bench")]
    {
        let end_time = Instant::now();
        println!("{:?}", end_time - start_time);
    }
}

fn display_error(source: &str, error: InterpretError) -> String {
    let mut result = String::new();
    writeln!(result, "error: {:?}", error.kind).unwrap();
    let instruction: Opcode = error.chunk.unwrap_function().unwrap().code[error.opcode_index];
    writeln!(
        result,
        "    at instruction #{} {}",
        error.opcode_index, instruction
    )
    .unwrap();
    let line_idx = error.chunk.unwrap_function().unwrap().opcode_to_line[error.opcode_index];
    writeln!(
        result,
        "    at line {}: `{}`",
        line_idx,
        source.lines().nth(line_idx - 1).unwrap()
    )
    .unwrap();

    result
}

pub fn run_repl() {
    let stdin = stdin();
    let mut stdin = stdin.lock();
    let mut input = String::new();
    let mut buffer = String::new();

    let mut gc = unsafe { GC::default_gc() };
    let builtins = builtin_factory();
    //VM is guranteed to work separately from compiler, so two borrows actually do not happen
    let mut vm = VM::new(&mut gc, &builtins);

    let module = Module::from_dot_notation("`REPL`");

    loop {
        buffer.clear();
        stdin.read_line(&mut buffer).unwrap();
        match buffer.as_str().trim() {
            "exit" => {
                break;
            }

            x if x.is_empty() => {
                println!("```\n{}\n```", input);

                let ptr = match compile_program(&input, &module, &mut vm) {
                    Ok(p) => p,
                    Err(e) => {
                        println!("error!\n{e}");
                        input = String::new();
                        continue;
                    }
                };

                match vm.run(ptr).map_err(|e| crate::display_error(&input, e)) {
                    Ok(value) => {
                        println!("Ok. result: {}", value);
                    }
                    Err(e) => {
                        println!("error!");
                        println!("{}", e);
                    }
                }

                input = String::new();
            }

            _any_other => {
                input.push_str(buffer.as_str());
            }
        }
    }
}
