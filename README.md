# SOFTWARE-EVOLUTION-LIBRARY

The SOFTWARE-EVOLUTION-LIBRARY enables the programmatic modification
and evaluation of extant software.

A common interface abstracts over multiple types of software objects
including abstract syntax trees parsed from source code, LLVM IR,
compiled assembler, and linked ELF binaries.  Mutation and evaluation
methods are implemented on top of this interface supporting Search
Based Software Engineering (SBSE) techniques.

This library has been used to optimize benchmark programs (see
https://github.com/eschulte/goa) and to patch vulnerabilities in
closed source binaries (see http://eschulte.github.io/netgear-repair).

See the info manual for more information.  Available in the `doc/`
directory and at http://GrammaTech.github.io/sel.

Please cite Eric Schulte's Ph.D. dissertation when you publish results
that you have obtained with the SOFTWARE-EVOLUTION-LIBRARY.

```bibtex
@phdthesis{schulte2014dissertation,
  author  = {Eric Schulte},
  title   = {Neutral Networks of Real-World Programs and their
                  Application to Automated Software Evolution},
  school  = {University of New Mexico},
  address = {Albuquerque, USA},
  month   = {July},
  year    = {2014},
  note    = {https://cs.unm.edu/~eschulte/dissertation}
}
```
