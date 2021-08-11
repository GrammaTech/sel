# Introduction

This document will serve as a repository of breaking API changes
between pypi major/minor releases and give advice on porting to
the newer version.

# Changelist

## v0.1.10.dev3 -> v0.2.0

Several changes were made to the AST contructor.  The source text
parameter and language parameters were reversed; now the source text
parameter is position one while the language parameter is position
two (previously the converse was true).  Additionally, the language
parameter was changed to an ASTLanguage enum object and may be
elided if the language can be inferred from the source text.

Examples of the old versus new approach are show below:

```
# v0.1.10.dev3
ast = asts.AST("python", "x + 88")
```

```
# v0.2.0
ast = asts.AST("x + 88", asts.ASTLanguage.Python)
```
