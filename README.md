# Osaka

Osaka is an experimental functional programming language that uses the
[Functional Machine Calculus (FMC)](https://arxiv.org/pdf/2212.08177.pdf) as
its underlying model of computation.

The compiler is handwritten in OCaml.

## Roadmap

An overview of the work needed to complete the project.

* [x] Implement lexer.
* [ ] Implement parser.
  * [x] Design grammar.
  * [ ] Ensure grammar is unambigious.
  * [ ] Convert unambigious grammar to predictive parser.
* [ ] Implement semantic analysis.
  * [ ] Type checking.
  * [ ] Ensuring pattern matching covers all cases.
* [ ] FMC term generation.

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
