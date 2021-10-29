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

## v0.3.0 -> v0.4.0

Several python methods were renamed and/or transformed into
properties.  These include:

- `oid` becoming a property
- `ast_language` renamed to `language` and becoming a property
- `source_text` becoming a property
- `children` becoming a property
- `child_slots` becoming a property
- `ast_refcount` renamed to `refcount`

Additionally, the before/after-asts child slots were removed
from the external python API.

Examples of the old versus new API are shown below:

```
# v0.3.0
>>> ast = asts.AST("x + 88", asts.ASTLanguage.Python)
>>> ast.oid()
1
>>> ast.ast_language()
<ASTLanguage.Python: 0>
>>> ast.source_text()
'x + 88'
>>> ast.children()
[<asts.types.PythonExpressionStatement0 0x2>]
>>> ast.child_slots()
[['BEFORE-ASTS', 0], ['CHILDREN', 0], ['AFTER-ASTS', 0]]
>>> ast.ast_refcount()
1
```

```
# v0.4.0
>>> ast = asts.AST("x + 88", asts.ASTLanguage.Python)
>>> ast.oid
1
>>> ast.language
<ASTLanguage.Python: 0>
>>> ast.source_text()
'x + 88'
>>> ast.children
[<asts.types.PythonExpressionStatement0 0x2>]
>>> ast.child_slots
[['CHILDREN', 0]]
>>> ast.refcount()
1
```

# Tree-sitter revisions

This section contains the tree-sitter and tree-sitter language
module revisions associated with releases of the ASTs pypi package.
This aids in reproducibility/tracing of issues in older releases.

## v0.0.0 -> v0.4.0

| Repository             | Revision |
|------------------------|----------|
| tree-sitter            | 59cd1c39 |
| tree-sitter-c          | 008008e3 |
| tree-sitter-cpp        | c6121241 |
| tree-sitter-javascript | 2c5b138e |
| tree-sitter-python     | d6210cea |

## v0.4.1

| Repository             | Revision |
|------------------------|----------|
| tree-sitter            | 0288dd4a |
| tree-sitter-c          | e348e8ec |
| tree-sitter-cpp        | a7652fce |
| tree-sitter-javascript | fdeb68ac |
| tree-sitter-typescript | ef6ee5b3 |
| tree-sitter-python     | 8600d7fa |

## v0.4.2

| Repository             | Revision |
|------------------------|----------|
| tree-sitter            | ddb12dc0 |
| tree-sitter-c          | e348e8ec |
| tree-sitter-cpp        | a7652fce |
| tree-sitter-javascript | fdeb68ac |
| tree-sitter-typescript | 5f904154 |
| tree-sitter-python     | 8600d7fa |

## v0.4.3

| Repository             | Revision |
|------------------------|----------|
| tree-sitter            | 67de9435 |
| tree-sitter-c          | e348e8ec |
| tree-sitter-cpp        | e8dcc9d2 |
| tree-sitter-javascript | fdeb68ac |
| tree-sitter-typescript | 11f8f151 |
| tree-sitter-python     | 8600d7fa |
