# Introduction

This document will serve as a repository of breaking API changes
between pypi major/minor releases and give advice on porting to
the newer version.

# Changelist

## v0.1.10.dev3 -> v0.2.0

Several changes were made to the AST constructor.  The source text
parameter and language parameters were reversed; now the source text
parameter is position one while the language parameter is position
two (previously the converse was true).  Additionally, the language
parameter was changed to an ASTLanguage enum object and may be
elided if the language can be inferred from the source text.

Examples of the old versus new approach are show below:

```python
# v0.1.10.dev3
ast = asts.AST("python", "x + 88")
```

```python
# v0.2.0
ast = asts.AST("x + 88", asts.ASTLanguage.Python)
```

## v0.2.2 -> v0.3.0

Changes were made to the AST constructor; clients should
now use the `AST.from_string` factory method in the Python
API to create ASTs instead of invoking the constructor
directly.  The parameters to `AST.from_string` match the
prior constructor, making porting relatively painless.

Examples of the old versus new approach are shown below:

```python
# v0.2.2
ast = asts.AST("x + 88", asts.ASTLanguage.Python)
```

```python
# v0.3.0
ast = asts.AST.from_string("x + 88", asts.ASTLanguage.Python)
```

# Tree-sitter revisions

This section contains the tree-sitter and tree-sitter language
module revisions associated with major and minor releases of
the ASTs pypi package.  This aids in reproducibility/tracing
of issues in older releases.

## v0.3.0

| Repository             | Revision |
|------------------------|----------|
| tree-sitter            | 59cd1c39 |
| tree-sitter-c          | 008008e3 |
| tree-sitter-cpp        | c6121241 |
| tree-sitter-javascript | 2c5b138e |
| tree-sitter-python     | d6210cea |
