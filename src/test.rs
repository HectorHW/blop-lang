use super::execution::module::{compile_file, run_file};

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
            use crate::data::gc::GC;
            use crate::execution::builtins::builtin_factory;
            use crate::execution::vm::VM;
            let mut path = String::new();
            path.push_str("examples/");
            path.push_str(stringify!($name));
            path.push_str(".txt");

            let mut gc = unsafe { GC::default_gc() };
            let builtins = builtin_factory();

            let mut vm = VM::new(&mut gc, &builtins);

            compile_file(&path, &mut vm).err().unwrap();
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

test_file! { tail_opt_sum}

test_file! {munchausen}

test_fail_compile! {fail_prohibit_assignment_to_function_inside_itself}

test_file! {operators}

test_file! {lambdas}

test_file! {partials}

test_file! {structures}

test_file! {strings}

test_file! {builtins}

test_file! {varargs}

test_file! {enums}

test_file! {inline_blocks}
