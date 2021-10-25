use super::run_file;

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
