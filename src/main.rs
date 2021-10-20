use crate::compile::compiler::Compiler;
use crate::execution::vm::VM;
use peg::error::ParseError;
use std::env;

mod compile;
mod data;
mod execution;
mod parsing;

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

    let indexed_tokens = tokens;
    use parsing::parser::program_parser;

    let statements = parsing::parser::program_parser::program(&indexed_tokens).unwrap();

    /*match parse_res {
        Ok(tree) => {
            println!("{:?}", tree);
        }
        Err(ParseError { location: x, .. }) => {
            println!("{:?}", parse_res);
            println!("{:?}", indexed_tokens[x]);
        }
        any_other => {
            println!("{:?}", any_other)
        }
    }*/

    /*
    let parser = parsing::parser::ProgramParser::new();

    let statements = parser.parse(indexed_tokens).unwrap();
    */
    for stmt in &statements {
        println!("{}", compile::lisp_printer::print_as_sexp(stmt));
    }

    let chunks = Compiler::compile(&statements).unwrap();

    let program = chunks.first().unwrap();

    println!("{}", program);

    println!("running");
    let mut vm = VM::new();
    vm.run(&program).unwrap_or_else(|error| {
        println!("{:?}", error);
        println!("{}", program.code[error.opcode_index])
    }); /**/
}
