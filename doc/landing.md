# Software Evolution Library

## What is it?
Software Evolution Library (SEL) is a Common Lisp library for using evolutionary
search to programmatically modify and evaluate software.
It provides a common interface which abstracts over several kinds of software
objects including parsed abstract syntax trees, intermediate languages, compiled
assembler, or binaries.

Methods for evolution, mutation, crossover, and fitness evaluation are
implemented on top of this interface, supporting Search Based Software
Engineering (SBSE) techniques.

## How is it used?
SEL has been used to optimize benchmark programs (see
<https://github.com/eschulte/goa>) and to patch vulnerabilities in
closed source binaries (see <http://eschulte.github.io/netgear-repair>).
It's also being actively developed and used for other projects at GrammaTech.
One such project is
[MuSynth](https://link.springer.com/chapter/10.1007/978-3-319-66299-2_8),
which adapts source code from existing projects to fill "holes" in incomplete
programs whose behavior is partially specified by unit tests and developer hints.

[//]: # (TODO: MuSynth is behind a paywall and has no source. Do we still want to cite it?)
[//]: # (TODO: More application examples?)

## What kinds of software can be manipulated?
The evolution API operates on software objects, which provide a structured
representation of some kind of software artifact, such as an abstract syntax
tree or compiled binary. SEL provides implementations of software objects
for manipulating:

* C and C++ source
* compiled assembler
* linked ELF binaries
* LLVM IR
* CIL
* LISP
* Forth

Of these, the first three (C, C++, ASM, and ELF) are the most mature. The
latter four are more experimental. Support for for Java and Coq (Gallina) 
is forthcoming.

Additionally, the flexible design of SEL makes it possible to add new software
objects for other languages. Refer to the documentation for information about
creating new software objects.

[//]: # (TODO: Documentation on creating new software objects)

## What other features are provided?
SEL provides support for exploring many different aspects of both evolutionary
search and automated software manipulation. It includes:

* Multiple evolution techniques including generational or Monte Carlo Markov
chain
* New mutations tailored to each software object
* One- and two-point crossover
* Lexicase or multi-objective fitness evaluations
* Instrumentation of software objects and dynamic trace collection
* Use of "fodder databases" as a source of new statements or expressions
that can be injected by mutations
* Ancestry for candidate solutions
* Adaptive mutations to dynamically update probabilities based on
mutation success

Specially for C/C++ objects:

* Automatic repair for some compilation errors and warnings
* Special mutations for renaming variables, changing loops, and otherwise
manipulating statements
* Spectrum-based fault localization, which instruments source code and runs
(passing and failing) unit tests to attempt to locate errors
* Condition synthesis, a repair technique described
[here](http://groups.csail.mit.edu/pac/patchgen/papers/spr-fse15.pdf)
which tries to synthesize conditional statements that will correct source code
so that it passes failing unit tests

## That sounds awesome! How do I get it?
Check out the installation requirements and instructions in the INSTALL file in
our github repo [here](https://github.com/GrammaTech/software-evolution/tree/master/INSTALL)

[//]: # (TODO: update link to INSTALL file if needed)

## Now what?
Once you have SEL and its dependencies set up, have a look at the
[Getting Started]() guide to try out some of examples or start exploring
the [API]().

[//]: # (TODO: links to Getting Started and API)
