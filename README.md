% CLANG-INSTRUMENT(1) Clang Source Instrumentation
% GrammaTech Inc.
% May 2016

# NAME

clang-mutate - Instrument C-language source files

# SYNOPSIS

clang-instrument <*source*> [*options*]

# DESCRIPTION

`clang-instrument` instruments a C language source file for trace
collection during dynamic execution.

# OPTIONS

-help
:   Print usage information.

-counter
:   Print the counter of every full statement AST. 

# SEE ALSO

`clang-mutate` (1).
The `clang-mutate` tool is used by `clang-instrument` to performs
performs source-to-source transformations (or mutations) over C
language source..
