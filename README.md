# Frog

Frog is an experimental functional programming language that uses the
[Functional Machine Calculus (FMC)](https://arxiv.org/pdf/2212.08177.pdf) as
its underlying model of computation.

The compiler is handwritten in OCaml.

## Usage

The Frog compiler can be built from source using [Dune](https://dune.build/),
one of the most widely used build systems for OCaml.

With Dune installed, run the command `dune build`. A compiled executable
`_build/default/bin/main.exe` will be created (the `.exe` file extension will
be present regardless of operating system used).
