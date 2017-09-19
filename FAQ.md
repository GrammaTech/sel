# Frequently asked Questions

Background information, getting started guides, debugging
recommendations, and solutions to common issues.

- [Lisp](#lisp)
    - [Startup](#startup)
    - [Additional Resources](#additional-resources)
- [Debugging Recommendations](#debugging-recommendations)
    - [Log all interaction with the system shell `(setq *shell-debug* t)`](#log-all-interaction-with-the-system-shell-setq-shell-debug-t)
    - [Tracing specific functions](#tracing-specific-functions)
- [Issues and Solutions](#issues-and-solutions)
    - [`clang-mutate` is not on my path](#clang-mutate-is-not-on-my-path)
    - [Dependency on MongoDB](#dependency-on-mongodb)
    - [Cross compilation](#cross-compilation)
- [SEL](#sel)
    - [Adding new software types](#adding-new-software-types)
    - [Adding new mutations](#adding-new-mutations)
    - [Trace collection](#trace-collection)
    - [Terminology](#terminology)
      - [genome](#genome)
      - [phenome](#phenome)

## Lisp

### Startup

This [getting started guide](http://cliki.net/Getting+Started) is
pretty complete.  Use a reasonable editor (e.g., Emacs or VIM with
[ParEdit](http://emacswiki.org/emacs/ParEdit) or
[paredit.vim](https://github.com/vim-scripts/paredit.vim)
respecitvely) makes following these rules trivial.  Really unless you
have a strong reason you should use Emacs as no other editor has such
strong integration into the Lisp process (useful for finding function
definitions, automatically displaying information, compiling,
highlighting compiler warnings, evaluating, etc...).

Also,
[Chapter 2](http://www.gigamonkeys.com/book/lather-rinse-repeat-a-tour-of-the-repl.html)
of [Practical Common Lisp](http://www.gigamonkeys.com/book/) also
describes getting Emacs and Slime working (and is generally a good
book).

### Additional Resources

- The `#lisp` IRC room on the [freenode](https://freenode.net/) irc server
- [Introduction to Slime](http://www.cliki.net/slime-howto)
- [Introduction to ParEdit](https://www.emacswiki.org/emacs/ParEdit)
- [Introduction to Melpa](https://melpa.org/#/getting-started)
- [Debugging Lisp](http://malisper.me/2015/07/07/debugging-lisp-part-1-recompilation/)
- [Practical Common Lisp Book](http://www.gigamonkeys.com/book/)
- [On Lisp](https://www.csee.umbc.edu/courses/331/resources/lisp/onLisp/)
- [Curry compose reader macros, aka, those weird `[{}]` and `«»` symbols](https://github.com/eschulte/curry-compose-reader-macros)
- [`loop` vs. `iter`](https://sites.google.com/site/sabraonthehill/loop-v-iter#TOC-Collect). We prefer `iter`.
- [`iter` in detail](https://common-lisp.net/project/iterate/doc/index.html)
- [Using `bind`](https://common-lisp.net/project/metabang-bind/user-guide.html)

## Debugging Recommendations

### Log all interaction with the system shell `(setq *shell-debug* t)`

For problems related to the execution of external commands turn on
logging of all execution of shell commands by SEL.  This may be done
by setting the `*shell-debug*` variable to a non-nil value.

```lisp
(setq *shell-debug* t)
```

All subsequent executions of `shell` will now print logging
information.

### Tracing specific functions

Common lisp provides support for function-level tracing.  This may be
enabled and disabled using the `cl-user::trace` and `cl-user::untrace`
functions respectively, as shown in the following.

    CL-USER> (in-package :software-evolution-test)
    #<PACKAGE "SOFTWARE-EVOLUTION-TEST">
    SE-TEST> (hello-world-clang)
    T
    SE-TEST> (cl-user::trace snippet->clang-type)
    (SNIPPET->CLANG-TYPE)
    SE-TEST> (update-asts *hello-world*)
      0: (SNIPPET->CLANG-TYPE
          ((:ARRAY . "") (:COL . 0) (:DECL . "") (:FILE . "")
           (:HASH . 342363981814211589) (:LINE . 0) (:POINTER . T) (:REQS)
           (:SIZE . 4) (:TYPE . "char")))
      0: SNIPPET->CLANG-TYPE returned
           #S(CLANG-TYPE
              :ARRAY ""
              :COL 0
    ;;;...
    #<CLANG {1003AD88D3}>
    SE-TEST> 

### Use extra verbosity in command-line tools

Many command-line tools compiled from `sel` support various levels of
verbosity in their output.  The simplest first step in debugging these
tools should be to maximize the level of verbosity, e.g. `-v 5`.

## Issues and Solutions

### `clang-mutate` is not on my path

SEL assumes that the `clang-mutate` executable (see
[clang-mutate](https://git.grammatech.com/synthesis/clang-mutate)) is
available on the shell's `PATH`.  Ensure this is the case.  The path
used by SBCL may not inherit PATH changes made in your user
environment, so placing a clang-mutate executable on the standard
system search path, or updating the path in `/etc/profile` may be
required.

### Dependency on MongoDB

Currently cl-mongo is a dependency of SEL and some tests depend on a
MongoDB installation.

> Perhaps we should move the functionality that requires MongoDB to a
> new package.

### Cross compilation

SEL builds 32-bit binaries for most unit tests.  To enable
cross-compilation on 64-bit machines, execute
`sudo apt-get install gcc-multilib g++-multilib`.

## SEL

### Adding new software object types

SEL software objects should derive from the most-relevant base class.
For source code based software objects, this is the `ast` class.  To
define a new software object type from an existing base class, use
```lisp
(define-software new-software-type (base-software-type)
  ())
```

### Adding new mutations

New mutations should derive from the most-relevant base class.
To define a new mutation from an existing base class, use
```lisp
(define-mutation new-mutation (base-mutation)
  ())
```

The mutation may be initialized with two functions; a targeter and
picker.  The targeter is a function which, given a software object,
returns a list of mutation targets; this function defaults to `pick-bad`.
The picker is a function which, given a software object, returns
a random mutation target; this function defaults to a random element
returned by `pick-bad`.

After creating a software object and a mutation, use the
`apply-mutation` method to create a new mutant.

### Trace collection

Software objects which need to support trace collection should
include an `instrument` method and derive from `traceable`.

The `instrument` method should inject logging into the software object
before each full statement; to avoid intermingling instrumentation logs
with program output, the instrumentation should be printed to the
file given by the `__SEL_TRACE_FILE` environment variable.  At a minimum,
logging should include an AST counter (:C) and variables in scope at
the given point (:SCOPES).  Currently, only primitive types are instrumented.

As an example, consider the following program:
```c
int main(int argc, char** argv) {
    printf("Hello, World!");
    return 0;
}
```

The trace returned will contain the following elements:

```lisp
(((:INPUT :BIN) (:TRACE ((:C . 4)  (:SCOPES ("argc" "int" 1 NIL)))
                        ((:C . 10) (:SCOPES ("argc" "int" 1 NIL))))))
```

To collect traces, pass an instrumented version of the software object
to the `collect-traces` method along with a test suite of test cases
you wish to execute on the instrumented object.

### Terminology

#### Genome

In biology, the genome is the genetic material (DNA/RNA) of the cell.
In the software evolution library, this field should contain the
instructions which define the behavior of a software object. In most
cases, the code (source or assembly), should be stored on this field.

#### Phenome

A phenome is an expression of all the traits described in the genome.
In the software evolution library, this is an executable binary.  In
addition to the executable, this method also returns (2) an errno (a numeric
indication of compilation success), (3) stderr of the compilation
process or a string holding error output relevant to phenome generation,
(4) stdout of the evolution process  or a string holding non-error output
relevant to phenome generation, (5) the source file name used during
compilation.

To override the generic phenome method for your software object, use
the following:
```lisp
(defmethod phenome (obj my-class)
  ())
```
