# BLOP language

BLOP (BLank OPerator) language is interpreted general-purpose functional programming language written entirely in Rust.

## Building

To build the interpreter you will need `cargo`, the build tool for Rust. Just install latest stable release of Rust.

Issue `cargo build` in the project root to build debug version of executable. To build optimized version, issue `cargo build --release`. For additional feature flags refer to [Cargo.toml](Cargo.toml).

Alternatively, issue `cargo run` or `cargo run --release` to compile and run interpreter.

The interpreter supports (somewhat working) REPL mode, but is mainly intended for running code stored in form of source files. In order to execute some file, just pass it to interpreter in form of argument e.g. `cargo run --release examples/partials.txt`.

## Features

Language provides a few basic building blocks:

* integer numbers like `42`
* `"strings in double quotes"`
* basic operators like `+` and `==`
* logic short-circuting operators `and`, `or`
* conditionals (`if`-`elif`-`else`)
* functions
* builtins like `int` for converting strings to numbers (In fact, depending on the progress, this may be the only implemented builtin)

The language uses Python-like indentation-based syntax.

To define a function, type:
```
def function(arg1, arg2, ..., argN) =
    some_expression
```
If function does not accept any arguments, this can be shortened down to:
```
def function =
    some_expression
```
Anonymous functions can be written as `(arg1, ..., argN) => some_expr`


Functions are first-class meaning that you can freely pass them around, return from other functions and so on. Functions also offer mechanism of closures.

Functions also support partial calls written as `f(_, _, arg2, _)` which returns special function-like partial object that now accepts arguments where blanks `_` are placed.

The language currently does not provide common loop constructs but offers tail call optimization.

For additional features refer to files in [examples directory](examples).

The language is still in early development stage. Features that are currently planned:
* Rust-like structs / enums with impl blocks and pattern matching
* Optional typechecking
* Standard library
