Software Evolution --- A tool for the evolution of extant software.

Design 

    +-------------+    +-------------+    +--------+    +--------------+
    | interactive |    | distributed |    | repair |    | optimization |
    |-------------|    |  evolution  |    |--------|    |--------------|
    | live in the |    |-------------|    | fixing |    |   improve    |
    |   editor    |    |  using ZMQ  |    |  bugs  |    |  performance |
    +-------------+    +-------------+    +--------+    +--------------+ ...
           |                  |                |                 |
           +------------------+------+---------+-----------------+
                                     |
                                   +-+-+                   population functions
                                   | | |                   --------------------
    global variables        +------------------+           incorporate
    ----------------        |   *population*   |           evict
    *population*            |------------------|           tournament
    *max-population-size*   |      list of     |           mutate
    *tournament-size*       | software objects |           crossed
    *fitness-predicate*     +------------------+           new-individual
    *cross-chance*                   |                     evolve
    *fitness-evals*                +-+-+
    *running*                      | | |                   software functions
                            +------------------+           --------------
                            | software object  |           genome
    evolve arguments        |------------------|           phenome
    ----------------        | edits,           |           evaluate
    max-evals               | fitness          |           copy
    max-time                | ...              |           mutate
    max-inds                +------------------+           crossover
    max-fit                           |                    edit-distance
    min-fit                           |
    pop-fn        +---------------+---+------------+------------------+ ...
    ind-fn        |               |                |                  |
          +---------------+  +------------+  +---------------+  +-------------+
          |    C AST      |  |   ELF      |  |      asm      |  |    lisp     |
          |---------------|  |------------|  |---------------|  |-------------|
          |   C Abstract  |  | Executable |  | assembly code |  | lisp source |
          |  Syntax Tree  |  |  Linkable  |  +---------------+  +-------------+
          +---------------+  |   Format   |                         |  
                |            +------------+                   +----------+
        +--------------+                                      |          |
        |              |                            +------------+  +----------+
     +-------+  +----------------+                  |   lisp fn  |  | lisp ext |
     | clang |  |       cil      |                  |------------|  |----------|
     |-------|  |----------------|                  | individual |  | external |
     | C AST |  | C Intermediate |                  |  functions |  |   eval   |
     +-------+  |    Language    |                  +------------+  +----------+
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
