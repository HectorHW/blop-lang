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

You can define vararg functions using `*args` syntax:
```
def function_accepting_args(*args) = 
    print(args)
```

Later, this function may be called like any other function if provided enough arguments (at least 0 in our case):
```
function_accepting_args(1, 2, 3)
# prints "Vector [Int 1, Int 2, Int 3]"
```

The language currently does not provide common loop constructs but offers tail call optimization.

To define complex structures you may use `struct` keyword:
```
struct Pair:
    element1
    element2
```
To create instance just call struct descriptor like any other callable: 
```
var instance = Pair(1, 2)
```
To later access struct's attributes, use standard dot syntax:
```
instance.element1 # to access field
instance.element2 = 2 # to set field
instance?element1 # to check if field exists
```
Struct elements are fixed in position and count, and because of that you may also use special syntax to access fields by index:
```
instance._0 # to access field by index
instance._1 = 2 # to set field
```

To define methods, use impl blocks like so:
```
impl Pair:
    fn first(self) = self._0
    fn second(self) = self._1
```
(Note `self` parameter: declared methods are required to have at least one argument to store reference to bound object).

Use `enum` keyword to define enums:

```
enum Option:
    Some:
        value
    None
```

Each enum variant is a struct under the hood, so `.` and `?` are supported. You can add methods using `impl Option`, these methods will be available to all variants.


For additional features refer to files in [examples directory](examples).

The language is still in early development stage. Features that are currently planned:
* Pattern matching
* Optional typechecking
* Standard library
* Modules
