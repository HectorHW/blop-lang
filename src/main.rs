use crate::compile::compiler::Compiler;
use crate::execution::vm::VM;
use crate::parsing::ast::Stmt;
use peg::error::ParseError;
use std::env;

mod compile;
mod data;
mod execution;
mod parsing;
#[cfg(test)]
mod test;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    if args.len() != 2 {
        println!("please provide file to run as argument.");
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
    for token in &tokens {
        print!("{}", token.kind);
    }
    println!();
    use parsing::parser::program_parser;

    let statements = match program_parser::program(&tokens) {
        Ok(s) => s,
        Err(ParseError { location, expected }) => {
            println!("{:?}", ParseError { location, expected });
            println!("{:?}", tokens[location]);
            return;
        }
    };

    for stmt in &statements {
        println!("{:?}", stmt);
    }

    compile::syntax_level_check::check(&statements).unwrap();
    let statements = compile::syntax_level_opt::optimize(statements);

    for stmt in &statements {
        println!("{:?}", stmt);
    }

    let chunks = Compiler::compile(&statements).unwrap();

    //println!("{}", chunks[0]);

    for chunk in &chunks {
        println!("{}", chunk);
    }

    println!("running");
    let mut vm = VM::new();
    vm.run(&chunks).unwrap_or_else(|error| {
        println!("{:?}", error);
        println!("{}", chunks[error.chunk_index].code[error.opcode_index])
    }); /**/
}

fn normalize_string(s: String) -> String {
    s.lines().collect::<Vec<_>>().join("\n")
}

pub fn run_file(filename: &str) -> Result<(), String> {
    let file_content = std::fs::read_to_string(filename).or(Err("failed to read file"))?;
    let file_content = normalize_string(file_content);
    let tokens = parsing::lexer::tokenize(&file_content)?;
    use parsing::parser::program_parser;

    let statements: Vec<Stmt> = program_parser::program(&tokens)
        .map_err(|e| format!("{:?}\n{:?}", e, tokens[e.location]))?;

    compile::syntax_level_check::check(&statements).unwrap();
    let statements = compile::syntax_level_opt::optimize(statements);

    let chunks = Compiler::compile(&statements)?;
    let mut vm = VM::new();
    vm.run(&chunks).map_err(|error| {
        format!(
            "{:?}\n{}",
            error, chunks[error.chunk_index].code[error.opcode_index]
        )
    })?;

    Ok(())
}
