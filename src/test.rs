use super::run_file;

macro_rules! test_file {
    ($filename:expr) => {
        run_file($filename).unwrap()
    };
}

#[test]
fn test_simple_assertions() {
    test_file!("examples/simple_assertion.txt")
}

#[test]
fn test_conditions() {
    test_file!("./examples/conditions.txt")
}

#[test]
fn test_function_as_value() {
    test_file!("./examples/function_as_value.txt")
}

#[test]
fn fib_fact() {
    test_file!("./examples/fib_fact.txt")
}
