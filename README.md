Model Checking using a Haskell Compiler
=======================================

This example demonstrates a simple refining compiler. The input language is a
simple calculator language with support for variables and user assertions.

A unified pass is used to compile tha language to a simple stack based VM and to
a SAT / SMT solver. The former can be executed immediately, and the latter is
run as part of the compilation process to look for counter-examples that cause
either built-in assertions (divide by zero, accessing an unset variable) or user
assertions to fail. The failing case is output as a trace showing steps to
reproduce the error.
