# Frog

Frog is an experimental functional programming language that uses the
[Functional Machine Calculus (FMC)](https://arxiv.org/pdf/2212.08177.pdf) as
its underlying model of computation.

The compiler is handwritten in OCaml.

## TODO

* [ ] Translation to FMC terms.
  * [ ] Expressions.
    * [x] Primary expressions.
    * [x] Binary operations.
    * [x] Unary operations.
    * [x] Function application.
    * [x] If-then-else expressions.
    * [x] Let-in expressions.
    * [ ] Match expressions.
  * [ ] Top-level definitions.
    * [x] Top-level let.
    * [ ] Data definition.
  * [ ] Recursive functions.
* [ ] Additions based on FMC.
  * [ ] Multiple return values from functions.
  * [ ] TODO: What else?
* [ ] Lower priority improvements.
  * [ ] Ensure pattern matching covers all cases (form of semantic analysis).
  * [ ] Insert type information into FMC text format.
  * [ ] Proper handling of patterns including in let bindings.
  * [ ] Handle unit properly.

## Features

* System of effects based on FMC.
* Strong, static type system.
* Tagged union sum types.
* Pattern matching.

## Usage

### Compile From Source

The Frog compiler can be built from source using [Dune](https://dune.build/),
one of the most widely used build systems for OCaml.

With Dune installed, run the command `dune build`. A compiled executable
`_build/default/bin/main.exe` will be created (the `.exe` file extension will
be present regardless of operating system used).
