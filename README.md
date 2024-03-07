# Osaka

Osaka is an experimental functional programming language that uses the
[Functional Machine Calculus (FMC)](https://arxiv.org/pdf/2212.08177.pdf) as
its underlying model of computation.

The compiler is handwritten in OCaml.

## TODO

* [ ] Implement semantic analysis.
  * [ ] Type checking.
    * [ ] Are all let bindings assigned values of the specified type?
    * [ ] Do all paths of match and if-then-else expressions return values of
      the same type?
    * [ ] Is function application performed with arguments of the correct
      types?
    * [ ] Are values of the correct types pushed to locations?
    * [ ] Are arguments to built-in operators (+, -, etc.) of the correct
      types?
  * [ ] Ensure pattern matching covers all cases.
* [ ] Translation to FMC terms.
  * [ ] Expressions.
    * [x] Primary expressions.
    * [x] Binary operations.
    * [x] Unary operations.
    * [x] Function application.
    * [x] If-then-else expressions.
    * [ ] Let-in expressions.
    * [ ] Match expressions.
  * [ ] Top-level definitions.
    * [ ] Top-level let.
    * [ ] Data definition.

## Features

* System of effects based on FMC.
* Strong, static type system.
* Tagged union sum types.
* Pattern matching.

## Usage

### Compile From Source

The Osaka compiler can be built from source using [Dune](https://dune.build/),
one of the most widely used build systems for OCaml.

With Dune installed, run the command `dune build`. A compiled executable
`_build/default/bin/main.exe` will be created (the `.exe` file extension will
be present regardless of operating system used).
