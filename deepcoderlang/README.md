# Synthesis of functional list manipulation programs

A quick runthrough of the codebase:

## api.rkt

A Racket implementation of the DSL. The combinators themselves are fairly straightforward. The predicate functions are organized in lists by signature. Most are also straightforward Racket implementations, but multiplication and division can't be used in synthesis with integer theory, so we approximate by synthesizing lookup tables.

## operator.rkt

First we define a instruction struct `dc-insn`, which describes an instruction in our program. This struct holds a set of list indexes, which determines which of a list of operators, previously computed values, and/or functions the instruction will execute. For example, if we had an instruction `(dc-insn 10 1 0 2 0 0)`, it would execute the combinator `map` on the second previously calculated list value with the predicate function `* 2`. The list of operators is in this file; the lists of predicate functions are in api.rkt.

Each combinator is represented in a `operator` struct, which holds the combinator name, a method that executes the instruction, and a method that prints the instruction.

## sketch.rkt

The `sketch` struct holds values that define the program sketch: a list of instructions, a value that selects which calculated value to return, and the number of program inputs. The important methods here are `synth-from-ref-impl`, which takes a sketch, a concrete implementation of the function that acts as a specification, and symbolic inputs and synthesizes a function; `synth-from-io-pairs`, which is similar but takes concrete input-output pairs as specifications, and `verify-sketch`, which takes a sketch, a reference implementation, a binding from a previous synthesis execution, and symbolic inputs, and determines if the synthesized program and the reference program are equivalent on those symbolic inputs.

## programs.rkt

Sample programs from the appendix of the Deepcoder paper. Some of these are fairly slow; comment/uncomment various synthesis calls as needed.

## random_benchmark.rkt

Randomly generate a program; randomly generate inputs and apply the program on those inputs to get outputs; synthesize a function using those input/output pairs as a spec; verify that the synthesized function is equivalent to the generated program.