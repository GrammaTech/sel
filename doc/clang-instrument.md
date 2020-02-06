% CLANG-INSTRUMENT(1) Clang Source Instrumentation
% GrammaTech Inc.
% May 2016

# NAME

clang-instrument - Instrument C-language source files

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

-s,--strings
:   Print string variable values to traces.  Could be dangerous.

-S,--scope
:   Instrument program to write values of all variables in scope at
    every instrumentation point.

-t,--trace-file *FILE*
:   Instrument program to write instrumentation to *FILE*.  By default
    instrumentation is written to `STDERR`.

-v,--variables
:   Instrument program to write values of unbound variables at every
    instrumentation point.

-V,--verbose *NUM*
:   Set verbosity level to *NUM*.  *NUM* should be in the range [0-4].

# EXAMPLES

Use a named pipe and the `--file` option to pipe the TRACE data
directory to a test script.

1.  Create a new named pipe

        mkfifo /tmp/log-pipe

2.  Instrument the program to write to this new named pipe.  Then
    compile the resulting program.

        clang-instrument foo.c -t /tmp/log-pipe

3.  In one window, or in an asynchronous process in a shell script,
    read from the named pipe.

        cat /tmp/log-pipe

    In another window, or synchronously in the same shell script, run
    the instrumented program.  Trace data will be written to the pipe
    and read by the previous `cat` process.

        ./foo

The following example shell code may be used to directly compress
trace output before it hits the disk.  This is often useful as trace
can produce large volumes of highly repetitive output.

    #!/bin/bash
    PROG=$1
    PIPE=$PROG.trace

    mkfifo $PIPE
    { cat $PIPE|xz > $PIPE.xz; } &
    PID=$!

    run_it() {
        ./limit ./$PROG $@
        EXIT=$?
        while kill -0 "$PID" >/dev/null 2>/dev/null;do sleep 0.1; done
        rm $PIPE;
        exit $EXIT; }

    # [...]

    run_it [args];
