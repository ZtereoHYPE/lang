# Lang lang compiler

## General architecture overview:
The course and the assignment mostly focus on the _lowering_ stage of a compiler. That is, the stage where you slowly bring the parsed tree closer to the resulting x86 (or LLVM IR, if we want cool optimisations and compilation targets).

However, in order to build a fully-working compiler, one must perform additional steps before starting the lowering process. Generally, these break down into:
![Frontend Architecture](resources/Compiler.png)

The semantic analysis does not create an additional intermediate representation, but is fully based on the information already present on the initial AST. Once the analysis validated the tree, because operators only have one "overload", it is already possible to know what the types of the various expressions are.

## Language design
The language design borrows (pun intended) a lot from Rust, and a bit from Scala. It was designed to have a very very simple grammar whilst keeping nice features such as expression blocks and if statements.

Generally, almost everything in lang is an expression, including `if` and code blocks. Code blocks contain a list of statements, followed by an (optional) expression, like in Rust. Statements are differentiated by their terminal semicolon, and can be any expression, assignments, or variable declarations.

Here is a list of the requirements, and how they are implemented in Lang.

### Function definitions
Here is how you define a function:
```
fn my_name(arg: Bool, argus: I64): Unit = <expression here>
```
Since any expression can go there, the following are legal function definitions:
```
fn double(x: I64): I64 = x * 2

fn plus_one(x: I64): I64 = {
    let zero: Bool = false;
    if zero {0} else {x} 
}

fn nothing(): Unit = {} // no expression => implicitly returns ()

fn nothing_2(): Unit = ()
```

### Conditionals
`if`, optionally followed by an `else`, accepts any expression as its conditional, but _must_ be followed by a block and not just any expression. Both branches must return the same type. Any expression being accepted is an... interesting design decision, taken for the sake of the grammar's simplicity.
```
if <expression> {
    do_something()
}

if {!false} {
    do_something()
} else {
    do_nothing()
}

if {
    let tmp: I64 = 0;
    -0 == tmp
} {
    do_something();
}
```

### Boolean & Arithmetic expressions
Both kinds exist, but are considered a single kind in the grammar and get differentiated during the semantic analysis.

Operators are defined for one type and one type only. For example, `==` and `!=` are defined for numbers only (and return a boolean), use logical operators (`!`, `&`, `|`, `^`) to compare booleans. This choice was taken to facilitate semantic analysis while still keeping a decent feature-set to the language.

### Assignments / locals
yup

## Deadlines
- [x] Week 2.4: First meeting
- [ ] Week 2.7: Second meeting
- [ ] Jan 13: Final code and report
- [ ] Jan 15: Demo

