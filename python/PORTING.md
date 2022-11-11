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

## v0.6.x -> v0.7.0

The child slot property was changed to a method instead.
Clients using `ast.child_slot` should be rewritten
as `ast.child_slots()`.

## v0.7.x -> v0.8.0

A child slot storing terminal ASTs was added to several macro
AST types.

## v0.8.0 -> v0.9.0

- (Breaking) `function_asts` and `call_asts` are now free functions defined in
`asts.utility` instead of methods on an AST.

- (Breaking) AST types are no longer re-exported from the top-level `asts`
package; types are only exported from `asts.types`. For instance `asts.CallAST`
now must be referenced as `asts.types.CallAST`.

- (Potentially breaking) `child_slots` will return an empty list instead of
`None` when the arity of the slot is "\*" and there are no items for the child
slot. The type annotation of `child_slots` was updated accordingly and the
behavior of \*-arity AST properties now matches the existing type-signature
(`List[AST]`).

- (Potentially breaking) The non-atom elements returned by `child_slots`,
`ast_source_ranges`, and `ast_path` are now tuples, reflecting the type
annotation.

- The `language` property will now return None instead of throwing when
the AST type does not have an associated language (e.g. `InnerParent`).

- The type signature of 1-arity AST properties was corrected to `Optional["AST"]`.

- Child slot properties are now "rolled up" to the highest possible superclass.
For instance, `path` is now a property of `CXXPreprocInclude` instead of
both `CPreprocInclude` and `CPPPreprocInclude`, allowing for type checking
to work properly when operating at higher levels of AST type abstraction.

- CXX classes now inherit from abstract type mixins. For instance,
`CXXCommentAST` now inherits from `CommentAST` whereas previously
`CCommentAST` and `CPPCommentAST` inherited from `CXXCommentAST` and
`CommentAST`.

- The type annotations of `parent`, `ast_template`, `asts_from_template`,
`get_vars_in_scope`, and `copy` were corrected to reflect underlying behavior,
which remains unchanged.

## v0.9.0 -> v0.9.1

Added `__len__` method to ASTs.

## v0.9.1 -> v0.9.2

Deleted `__len__` method and instead exposed `size` property on ASTs.

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

## v0.4.3 -> 0.4.7

| Repository             | Revision |
|------------------------|----------|
| tree-sitter            | 67de9435 |
| tree-sitter-c          | e348e8ec |
| tree-sitter-cpp        | e8dcc9d2 |
| tree-sitter-javascript | fdeb68ac |
| tree-sitter-typescript | 11f8f151 |
| tree-sitter-python     | 8600d7fa |

## v0.4.8 -> 0.4.10

| Repository             | Revision |
|------------------------|----------|
| tree-sitter            | 23465709 |
| tree-sitter-c          | e348e8ec |
| tree-sitter-cpp        | 656d7ea4 |
| tree-sitter-javascript | fdeb68ac |
| tree-sitter-typescript | e8e8e8dc |
| tree-sitter-python     | 24b530ca |

## v0.5.0 -> 0.5.1

| Repository             | Revision |
|------------------------|----------|
| tree-sitter            | 23465709 |
| tree-sitter-c          | e348e8ec |
| tree-sitter-cpp        | 656d7ea4 |
| tree-sitter-java       | a24ae7d1 |
| tree-sitter-javascript | fdeb68ac |
| tree-sitter-typescript | e8e8e8dc |
| tree-sitter-python     | 24b530ca |

## v0.5.2

| Repository             | Revision |
|------------------------|----------|
| tree-sitter            | 5ef4ef4e |
| tree-sitter-c          | e348e8ec |
| tree-sitter-cpp        | 656d7ea4 |
| tree-sitter-java       | 8a6ab8e8 |
| tree-sitter-javascript | fdeb68ac |
| tree-sitter-typescript | e8e8e8dc |
| tree-sitter-python     | 24b530ca |

## v0.6.0 -> 0.6.2

| Repository             | Revision |
|------------------------|----------|
| tree-sitter            | 5ef4ef4e |
| tree-sitter-c          | e348e8ec |
| tree-sitter-cpp        | 656d7ea4 |
| tree-sitter-java       | 8a6ab8e8 |
| tree-sitter-javascript | fdeb68ac |
| tree-sitter-python     | 24b530ca |
| tree-sitter-typescript | e8e8e8dc |
| tree-sitter-rust       | a250c458 |

## v0.6.3 -> Latest

| Repository             | Revision |
|------------------------|----------|
| tree-sitter            | efe009f4 |
| tree-sitter-c          | 3ced8d6c |
| tree-sitter-cpp        | 38d8b495 |
| tree-sitter-java       | ac14b4b1 |
| tree-sitter-javascript | 78583130 |
| tree-sitter-python     | 24b530ca |
| tree-sitter-typescript | 1b3ba31c |
| tree-sitter-rust       | 36ae187e |
