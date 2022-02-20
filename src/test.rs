use super::compile_file;
use super::run_file;
use crate::{compile, GC};

macro_rules! test_file {
    ($name:ident) => {
        #[test]
        fn $name() {
            let mut path = String::new();
            path.push_str("examples/");
            path.push_str(stringify!($name));
            path.push_str(".txt");
            println!("{}", path);

            run_file(&path).unwrap();
        }
    };
}

macro_rules! test_fail_file {
    ($name:ident) => {
        #[test]
        fn $name() {
            let mut path = String::new();
            path.push_str("examples/");
            path.push_str(stringify!($name));
            path.push_str(".txt");
            println!("{}", path);

            run_file(&path).unwrap_err();
        }
    };
}

macro_rules! test_fail_compile {
    ($name:ident) => {
        #[test]
        fn $name() {
            let mut path = String::new();
            path.push_str("examples/");
            path.push_str(stringify!($name));
            path.push_str(".txt");

            let mut gc = unsafe { GC::default_gc() };

            assert!(compile_file(&path, &mut gc).is_err());
        }
    };
}

test_file! {simple_assertion}

test_file! {conditions}

test_file! {function_as_value}

test_file! {fib_fact}

test_file! {closing_over_variables}

test_file! {mutual_recursion}

test_fail_file! {fail_undefined_variable_inside_function}

test_fail_file! {fail_undefined_variable}

test_file! {closing_over_reassigned}

test_file! {closing_over_arguments}

test_file! {closing_over_multiple_functions}

test_file! {parens_override_indentation}

test_file! {shadowing}

#[test]
fn test_tail_call_optimization_application() {
    use crate::compile::compiler::Compiler;
    use crate::execution::vm::VM;
    use crate::parsing::ast::Expr;

    let filename = "examples/tail_opt_sum.txt";
    let file_content = std::fs::read_to_string(filename)
        .map_err(|_e| format!("failed to read file {}", filename))
        .unwrap();
    let file_content = super::normalize_string(file_content);
    let tokens = crate::parsing::lexer::tokenize(&file_content).unwrap();
    use crate::parsing::parser::program_parser;

    let statements: Expr = program_parser::program(&tokens)
        .map_err(|e| format!("{:?}\n{:?}", e, tokens[e.location]))
        .unwrap();

    let (statements, annotations) = compile::checks::check_optimize(statements).unwrap();

    //let (variable_types, closed_names) =
    //    crate::compile::syntax_level_check::check(&statements).unwrap();
    let mut gc = unsafe { GC::default_gc() };
    let entry = Compiler::compile(&statements, annotations, &mut gc).unwrap();
    println!(
        "{}",
        entry.unwrap_function().unwrap().constants[0]
            .unwrap_function()
            .unwrap()
            .constants[0]
            .unwrap_function()
            .unwrap()
    );
    let mut vm = VM::new(&mut gc);
    vm.override_stack_limit(20); //should be just fine (and is definetly <1000)
    vm.run(entry).unwrap();
}

test_file! {munchausen}

test_fail_compile! {fail_prohibit_assignment_to_function_inside_itself}

test_file! {operators}

test_file! {lambdas}

test_file! {partials}
