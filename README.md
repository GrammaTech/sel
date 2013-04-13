Software Evolution
==================

A tool for the evolution of extant software.

                                                         population functions
    global variables                                     --------------------
    ----------------        +------------------+         incorporate
    *population*            |   *population*   |         evict
    *max-population-size*   |------------------|         tournament
    *tournament-size*       |      list of     |         mutate
    *fitness-predicate*     | software objects |         crossed
    *cross-chance*          +------------------+         new-individual
    *fitness-evals*                  |                   evolve
    *running*                      +-+-+
                                   | | |                 software functions
                            +------------------+         --------------
    evolve arguments        | software object  |         genome
    ----------------        |------------------|         phenome
    max-evals               | edits,           |         copy
    max-time                | fitness          |         pick-good
    target-fit              | ...              |         pick-bad
    period                  +------------------+         mutate
    period-func                       |                  crossover
                                      |
                  +---------------+---+------------+---------------+
                  |               |                |               |
         +---------------+  +------------+  +-------------+  +-------------+
         |      AST      |  |    asm     |  |     ELF     |  |    lisp     |
         |---------------|  |------------|  |-------------|  |-------------|
         |   Abstract    |  |  assembly  |  | Executable  |  | lisp source |
         |  Syntax Tree  |  |    code    |  |  Linkable   |  +-------------+
         +---------------+  +------------+  |   Format    |
                 |                          +-------------+
        +--------------+-------------------+
        |              |                   |
     +-------+  +----------------+   +----------+
     | Clang |  |       CIL      |   |   LLVM   |
     |-------|  |----------------|   |----------|
     | C AST |  | C Intermediate |   | LLVM IR  |
     +-------+  |    Language    |   +----------+
                +----------------+

The `*population*` is a global variable holding a list of evolving
software variants.  The evolutionary computation functions operate
directly on the `*population*` through the "population functions".
Exposure of the population through a global variable enables the user
to initialize the population in any way, and allows external functions
to monitor and interact with the population during evolution.  For
example `evolution/distributed.lisp` enables sharing of individuals
between multiple instances of evolution through two functions which
interact with the `*population*` variable directly.

Each software variant is an object whose class is a sub-class of the
general `software` class.  New representations are implemented through
sub-classing `software` and customizing the generic software functions
where necessary.

Parameterization of the EC algorithm is done through setting global
variables and through arguments to the `evolve` function.

Licensed under the GPLV3, see the COPYING file in this directory for
more information.
