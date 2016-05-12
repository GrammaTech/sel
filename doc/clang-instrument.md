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

-c,--compiler *CC*
:   Use *CC* as the C compiler.  Defaults to the value of the `CC`
    environment variable.

-F,--flags *FLAGS*
:   Set the compiler flags to the comma-separated list specified in
    *FLAGS*.

-o,--out-file *FILE*
:   Write instrumented source to *FILE*.  By default instrumented
    source is written to `BASE-instrumented.EXT` where `BASE` and
    `EXT` are the basename and extension of the input source file
    respectively.

-O,--orig
:   Also write a formatted copy of the un-instrumented original to
    `BASE-original.EXT` where `BASE` and `EXT` are the basename and
    extension of the input source file respectively.

-p,--point *NUM*,*STRING*
:   Instrument the program to print *STRING* when ast numbered *NUM*
    is executed.

-q,--quiet
:   Set verbosity level to 0

-t,--trace-file *FILE*
:   Instrument program to write instrumentation to *FILE*.  By default
    instrumentation is written to `STDERR`.

-v,--verbose *NUM*
:   Set verbosity level to *NUM*.  *NUM* should be in the range [0-4].

# EXAMPLES

Use a named pipe and the `--file` option to pipe the TRACE data
directory to a test script.

1.  Create a new named pipe

        mkfifo /tmp/log-pipe

2.  Instrument the program to write to this new named pipe.  Then
    compile the resulting program.

        clang-instrument foo.c -f /tmp/log-pipe

3.  In one window, or in an asynchronous process in a shell script,
    read from the named pipe.

        cat /tmp/log-pipe

    In another window, or synchronously in the same shell script, run
    the instrumented program.  Trace data will be written to the pipe
    and read by the previous `cat` process.

        ./foo

# SEE ALSO

`clang-mutate` (1).
The `clang-mutate` tool is used by `clang-instrument` to performs
performs source-to-source transformations (or mutations) over C
language source..
