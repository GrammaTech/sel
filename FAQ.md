Table of Contents
- [Debugging Recommendations](#debugging-recommendations)
    - [Log all interaction with the system shell `(setq *shell-debug* t)`](#log-all-interaction-with-the-system-shell-setq-shell-debug-t)
    - [Tracing specific functions](#tracing-specific-functions)
- [Issues and Solutions](#issues-and-solutions)
    - [`clang-mutate` is not on my path](#clang-mutate-is-not-on-my-path)
    - [Dependency on MongoDB](#dependency-on-mongodb)

# Debugging Recommendations

## Log all interaction with the system shell `(setq *shell-debug* t)`

For problems related to the execution of external commands turn on
logging of all execution of shell commands by SEL.  This may be done
by setting the `*shell-debug*` variable to a non-nil value.

```lisp
(setq *shell-debug* t)
```

All subsequent executions of `shell` will now print logging
information.

## Tracing specific functions

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

## Use extra verbosity in command-line tools

Many command-line tools compiled from `sel` support various levels of
verbosity in their output.  The simplest first step in debugging these
tools should be to maximize the level of verbosity, e.g. `-v 5`.

# Issues and Solutions

## `clang-mutate` is not on my path

SEL assumes that the `clang-mutate` executable (see
[clang-mutate](https://git.grammatech.com/synthesis/clang-mutate)) is
available on the shell's `PATH`.  Ensure this is the case.  The path
used by SBCL may not inherit PATH changes made in your user
environment, so placing a clang-mutate executable on the standard
system search path, or updating the path in `/etc/profile` may be
required.

## Dependency on MongoDB

Currently cl-mongo is a dependency of SEL and some tests depend on a
MongoDB installation.

> Perhaps we should move the functionality that requires MongoDB to a
> new package.
