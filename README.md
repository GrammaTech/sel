# SOFTWARE-EVOLUTION-LIBRARY

The SOFTWARE-EVOLUTION-LIBRARY enables the programmatic modification
and evaluation of software.

A common interface abstracts over multiple types of software objects
including source code abstract syntax trees (primarily using
[tree-sitter](https://tree-sitter.github.io/tree-sitter/)), compiled
assembler code, and binary software objects.  Software transformation,
analysis, and evaluation techniques are implemented on top of this
interface supporting automation of common software engineering tasks.

See the manual, available at [https://grammatech.github.io/sel](https://grammatech.github.io/sel/#Top).

SEL has been used to support many projects including:
- The Mnemosyne automated software development assistant.
  https://grammatech.gitlab.io/Mnemosyne/docs/
- A GitHub application for automated merge conflict resolution.
  https://mergeresolver.github.io
- [Automated Customized Bug-Benchmark Generation](https://arxiv.org/pdf/1901.02819.pdf)
  *Source Code Analysis and Manipulation (SCAM) 2019* (**distinguished paper**)
- [Evolving Exact Decompilation](https://eschulte.github.io/data/bed.pdf)
  *Workshop on Binary Analysis Research (BAR) 2018*
- [MuSynth: Program Synthesis via Code Reuse and Code Manipulation](https://eschulte.github.io/data/musynth-ssbse-2017.pdf)
  *International Symposium on Search Based Software Engineering 2017*
- [Repairing COTS router firmware without access to source code or test suites](https://eschulte.github.io/data/netgear-repair-preprint.pdf)
  *Workshop on Genetic Improvement 2015* (**best paper**)
- [Post-compiler Software Optimization for Reducing Energy](https://eschulte.github.io/data/asplos265-schulte.pdf)
  *Architectural Support for Programming Languages and Operating Systems (ASPLOS) 2014*
- [Automated Repair of Binary and Assembly Programs for Cooperating Embedded Devices](https://eschulte.github.io/data/schulte2013embedded.pdf)
  *Architectural Support for Programming Languages and Operating Systems (ASPLOS) 2013*
- [Software Mutational Robustness](https://arxiv.org/pdf/1204.4224.pdf)
  *Genetic Programming and Evolvable Machines 2013*

To cite SEL please use the following reference:
```bibtex
@manual{sel2018manual,
  title        = {Software Evolution Library},
  author       = {Eric Schulte and Contributors},
  organization = {GrammaTech},
  address      = {eschulte@grammatech.com},
  month        = 1,
  year         = 2018,
  note         = {https://grammatech.github.io/sel/}
}
```
