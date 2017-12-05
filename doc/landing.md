% Software Evolution Library
% GrammaTech

## What is it?

Software Evolution Library (SEL) is a Common Lisp library for using
evolutionary search to programmatically modify and evaluate software.
It provides a common interface which abstracts over several kinds of
software objects including parsed abstract syntax trees, intermediate
languages, compiled assembler, or binaries.

Methods for evolution, mutation, crossover, and fitness evaluation are
implemented on top of this interface, supporting Search Based Software
Engineering (SBSE) techniques.


## How is it used?

SEL has been used to improve or understand software in a number of ways:

- to optimize real-world software (see [github.com/eschulte/goa](https://github.com/eschulte/goa)),
- to patch vulnerabilities in closed source binaries (see [eschulte.github.io/netgear-repair](http://eschulte.github.io/netgear-repair)),
- to fill "holes" in incomplete programs whose behavior is partially
  specified by unit tests and developer hints (see
  [MuSynth](http://cs.unm.edu/~eschulte/data/musynth-ssbse-2017.pdf)),
- to adapt software for
  [DARPA's BRASS program](https://www.darpa.mil/program/building-resource-adaptive-software-systems),
- to inject bugs into software to create cyber-defense benchmarks
  [BUG-INJECTOR](https://www.grammatech.com/sponsored-research),
- to decompile binaries to C source [TODO](#todo),
- and to study the *mutational robustness* of software (see
  [cs.unm.edu/~eschulte/dissertation](http://cs.unm.edu/~eschulte/dissertation/)).


## What kinds of software can be manipulated?

The SEL API defines software objects using the CLOS, to provide a
uniform interface to many kinds of software artifact.  SEL currently
provides implementations of software objects representing:

- C and C++ source
- compiled assembler (ASM)
- linked ELF binaries
- Java (*in-progress*)
- Coq (*in-progress*)
- LLVM IR
- CIL
- LISP
- Forth

Of these, the first three (C, C++, ASM, and ELF) are the most
mature. The latter four are more experimental. Support for for Java
and Coq (Gallina) is forthcoming.

Additionally, the flexible design of SEL makes it possible to add new
software objects for other languages. Refer to the documentation for
information about creating new software objects.


## What other features are provided?

SEL provides support for exploring many different aspects of both
evolutionary search and automated software manipulation. It includes:

- Multiple evolution techniques including generational or Monte Carlo Markov chain
- New mutations tailored to each software object
- One- and two-point crossover
- Lexicase or multi-objective fitness evaluations
- Instrumentation of software objects and dynamic trace collection
- Use of "fodder databases" as a source of new statements or
  expressions that can be injected by mutations
- Ancestry for candidate solutions
- Adaptive mutations to dynamically update probabilities based on mutation success

Specially for C/C++ objects:

- Automatic repair for some compilation errors and warnings
- Special mutations for renaming variables, changing loops, and
  otherwise manipulating statements
- Spectrum-based fault localization, which instruments source code and
  runs (passing and failing) unit tests to attempt to locate errors
- Condition synthesis, a repair technique described
  [here](http://groups.csail.mit.edu/pac/patchgen/papers/spr-fse15.pdf)
  which tries to synthesize conditional statements that will correct
  source code so that it passes failing unit tests


## That sounds awesome! How do I get it?

Check out the installation requirements and instructions in the
[CONTRIBUTING](CONTRIBUTING.html) file and answers to frequently asked
questions in the [FAQ](FAQ.html) file in the [github
repo](https://github.com/GrammaTech/sel).


## Now what?

Once you have SEL and its dependencies installed, read the [SEL
manual](software-evolution-library/index.html#Top) and try out some of
the included
[examples](file:///home/eschulte/lisp/local-projects/sel/doc/software-evolution-library/Usage.html)
or start exploring the [API](api.html).

If you have questions, please refer to the [FAQ](FAQ.html) to make
sure your question hasn't already been answered before reporting an
issue.
