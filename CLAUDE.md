# SSA Compiler

This project is the beginnings of a compiler based on an SSA intermediate representation.
There is an original C version, as well as a Rust port which we are currently developing.
Assume that we will not be modifying any C code, or in any way needing to run the root directory Makefile or Python scripts.

## Rust version
The `rust` subfolder contains the Rust version, which we are porting from the C version.

I want to focus on creating a compiler which is very efficient in terms of cache friendly data structures and just general throughput.
With this as a base I want to leverage an SSA IR to implement some basic optimizations.

### Files
* lexer.rs: The lexical analyzer (`Lexer`)).
* ast.rs: Definition of the AST structure. A single AST is meant to represent a single module / file. The core type is `Node`.
* astprint.rs: The pretty printer (`AstPrinter`), used both for pretty printing synthesized AST trees (potentially), as well as for formatting parsed ASTs (then making use of formatting tokens from the input token stream).
* parse.rs: The AST parser (`Parser`), which consumes tokens from the lexer, and produces an AST.
* ir.rs: Definition of the SSA intermediate representation (IR), including the `Instr` type.
* irgen.rs: The IR generator (`IrGen`) which provides functions for building IR code into a `Code` buffer. Performs constant folding.
* irprint.rs: The IR printer (`IrPrinter`).
* code.rs: Buffer for holding IR (`Code`). Uses two vectors internally to implement buffer growable in both positive and negative directions. Negative indexes are used to store constant and pure instructions (they are unoredered, and we can schedule them later). Positive index instructions have an explicit scedule (given by their ordering).
* compile.rs: Defines the compiler (`Compiler`), which generates IR from the AST (very WIP).
* refmap.rs: Defines `RefMap`, which is array storage for indexing by the Ref-types that we use. Guarantees that the zeroth index is always unused, and allows lazy expansion with default-construction when mut-indexing.

### AST
I want the AST produced to be efficient in terms of interpretation / compilation, requiring few or no dynamic lookups. At the same time I want it to fully support virtually lossless pretty printing (enough to implement a built-in code formatter).

### IR
The IR is based on SSA. We're trying really hard to keep the instruction size at 8 bytes for now. We're achieving this by using 16 bit operands,
which means that single functions are limited in code size to what we can address with that.

#### Phi/Upsilon
It uses the concept of Phi and Upsilon instructions to handle join points. Phi instructions are placed at joint points
and the reference a unique ID corresponding to a hidden "variable". Each branch leading to the join point is required to be preceded by an Upsilon instruction
which "assigns" a value to the hidden "variable". It is basically a way of encoding the edge-value pairs with instructions in the IR at the predecessor blocks of the join point. An advantage is that if an optimization pass removes a branch arm for example, then it will remove the Upsilon contained, and a trivial post-process can detect
that the Phi has only a single Upsilon, and references to the Phi can be replaced with references to the value assigned by the Upsilon.

#### Block sealing
We generate SSA on the fly (using the algorithm described in "Simple and Efficient Construction of Static Single
Assignment Form" by Braun et al: https://c9x.me/compile/bib/braun13cc.pdf).
To do this effectively we need the concept of a sealed or unsealed block. A block is sealed if there will be no more incoming control flow to that block.
It is advantageous to seal a block as soon as possible, after the last jump to that block has been generated.

### Compiler
The compiler's main job is to produce IR from AST. However we also want to support Zig-like compile-time execution, and this happens though
and interplay between the IR generator and the compiler. The IR generator performs basic constant folding in the instruction emit methods.
However, to the full compile time execution concept requires the compiler to for example iterate over an `inline` loop until the (static-known)
termination condition is met, with each iteration emitting IR in a line following the previous iteration without any branches.
At the same time it has to support generating run-time executed loops using the IR control flow instructions.

#### Static
Anything marked static needs to be available compile time (it means statically known):

* A static function can only be invoked with compile-time known arguments, and the function will be executed at compile time.
* A static function argument needs to be compile-time known, and we'll generate a specialized version of the function for each specific combination of static arguments. (If all arguments are marked static, that is equivalent to marking the whole function as static, and there will be no run-time residual).
* A static block will cause everything inside to be compile-time evaluated, and the result will be a static value.

#### Inline
Inline means slightly different (but similar) things in different contexts:
* An inline function (`inline fn`) will have its body inlined at every call-site.
* An inline conditional (`inline if`) requires compile-time known condition value, and the conditional will be replaced with the corresponding branch arm.
* An inline loop (like `inline while`) requires the loop coindition to be compile-time known, and the IR for all iterations will be laid out sequentially without branches.

## C version
The root directory contains the original C implementation. This is legacy code, which we only use for reference.

### Building
Run `make` here in the roor repo directory to build the project.

### Testing
Run `make test` to run the test suite. Will build the project if necessary.

The test suite is simply all exported functions which have a `test_` prefix.
These tests are snapshot tests, and their corresponding snapshots are stored in the `snapshots/` folder.
Snapshots are simply a capture of stdout text generated by running the test function.
Snapshots should only be modified if the modified output is expected.
When a test fails the snapshot diff is printed on the lines following the line with the test name and the `failed!` status.
The diffs are generated with the Python difflib library, and will have usual `-` and `+` prefixed lines,
as well as ignorable `?` prefixed lines which show hints about what changed on the adjacent line.

### Debugging segfaults or memory problems
Use valgrind to debug for example specific test failures caused by memory issues:
```
valgrind ./bin/compiler --run-test test_instruction_scheduling
```

