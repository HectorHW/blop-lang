use crate::execution::chunk::Chunk;
use crate::execution::chunk::Opcode::*;
use crate::execution::vm::InterpretErrorKind::{StackUnderflow, ZeroDivision};
use crate::execution::vm::VM;

use crate::data::objects::Value;

#[test]
fn single_push_should_create_value_on_stack() {
    let mut chunk = Chunk::new("test".to_string());
    chunk += LoadImmediateInt(5);

    let mut vm = VM::new();

    let _ = vm.run(&vec![chunk]);
    assert_eq!(vm.stack, [Value::Int(5)]);
}

#[test]
fn subtraction_of_two_values() {
    let mut chunk = Chunk::new("test".to_string());
    chunk += LoadImmediateInt(5);
    chunk += LoadImmediateInt(4);
    chunk += Sub;

    let mut vm = VM::new();

    let _ = vm.run(&vec![chunk]);
    assert_eq!(vm.stack, [Value::Int(1)]);
}

#[test]
fn should_return_zero_division() {
    let mut chunk = Chunk::new("test".to_string());
    chunk += LoadImmediateInt(5);
    chunk += LoadImmediateInt(0);
    chunk += Div;

    let mut vm = VM::new();

    let res = vm.run(&vec![chunk]);
    assert_eq!(res.err().unwrap().kind, ZeroDivision);
}
#[test]
fn should_return_stack_uderflow() {
    let mut chunk = Chunk::new("test".to_string());
    chunk += LoadImmediateInt(5);
    chunk += Div;

    let mut vm = VM::new();

    let res = vm.run(&vec![chunk]);
    assert_eq!(res.err().unwrap().kind, StackUnderflow);
}
