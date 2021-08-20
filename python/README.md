<!--TOC-->

- [Generic Tree-Sitter AST API](#generic-tree-sitter-ast-api)
- [Quick Intro](#quick-intro)
- [Extended Tutorial](#extended-tutorial)
  - [AST Creation](#ast-creation)
    - [Constructor](#constructor)
    - [AST Templates](#ast-templates)
    - [AST Copy](#ast-copy)
  - [AST Methods](#ast-methods)
    - [Common Operations](#common-operations)
    - [Source Locations](#source-locations)
    - [Functions](#functions)
    - [Function Callsites](#function-callsites)
  - [AST Traversal](#ast-traversal)
  - [AST Manipulation](#ast-manipulation)
    - [Mutation Primitives](#mutation-primitives)
- [Architecture](#architecture)
- [License](#license)

<!--TOC-->

# Generic Tree-Sitter AST API

The ASTs package provides a Python API into GrammaTech's Software
Evolution Library ([SEL][]) for source code manipulation.  SEL
generalizes over GitHub's [tree-sitter][] parsing libraries providing
a uniform interface over multiple programming languages (primarily
Python, JavaScript, and C/C++), and providing additional functionality
for software inspection and modification.

# Quick Intro

Here's how to create and perform some common operations on an AST:

```python3
$ python3
Python 3.8.5
Type "help", "copyright", "credits" or "license" for more information.
>>> import asts
>>> root = asts.AST("x + 88", language=asts.ASTLanguage.Python)
>>> root.children()
[<asts.asts.AST object at 0x7f8e91fb52b0>]
>>> root.children()[0].children()
[<asts.asts.AST object at 0x7f8e918b7100>]
>>> root.children()[0].children()[0].children()
[<asts.asts.AST object at 0x7f8e918b7490>,
 <asts.asts.AST object at 0x7f8e918b73a0>,
 <asts.asts.AST object at 0x7f8e918b73d0>]
>>> root.children()[0].children()[0].children()[0].source_text()
'x'
>>> root.children()[0].children()[0].children()[1].source_text()
'+'
>>> root.children()[0].children()[0].children()[2].source_text()
'88'
>>> root.children()[0].children()[0].source_text()
'x + 88'
>>> root.children()[0].children()[0].child_slots()
[['BEFORE-ASTS', 0], ['PYTHON-LEFT', 1], ['PYTHON-OPERATOR', 1],
 ['PYTHON-RIGHT', 1], ['CHILDREN', 0], ['AFTER-ASTS', 0]]
>>> list(map(lambda x:x.source_text(), root.children()[0].children()[0].children()))
['x', '+', '88']
>>> list(map(lambda x:x.ast_type(), root.children()[0].children()[0].children()))
['PYTHON-IDENTIFIER', 'PYTHON-+', 'PYTHON-INTEGER']
```

# Extended Tutorial

The following examples assume you have imported the asts library using
`import asts`.  See the methods provided by asts.py for more
information.

<!-- TODO: Setup automatic documentation building. -->

## AST Creation

### Constructor

Creating an AST using the Python API requires source text and
(optionally) a language enumeration indicating the source text
language.  The example below shows the creation of a simple AST:

```python
>>> root = asts.AST("x + 88", language=asts.ASTLanguage.Python)
```

Language enumerations exist for `Python`, `C`, `Cpp`, and
`Javascript`.

For larger examples where the language may be inferred, the language
parameter may optionally be elided.  For instance:

```python
>>> text = """
... import sys
...
... def fib(n: int) -> int:
...     if n < 2:
...         return n
...     else:
...         return fib(n - 1) + fib(n - 2)
...
... def main():
...     if len(sys.argv) == 1:
...         print(fib(int(sys.argv[1])))
...
... if __name__ == '__main__':
...     main()
"""
>>> root = asts.AST(text)
```

Finally, by default, the AST returned is the top-level, module node of
the parsed AST.  However, in some cases, we may wish get the deepest
subnode still encompassing all of the given source text.  This allows
us to create statement AST nodes, for instance.  To do so, clients
may use the `deepest` keyword, as shown below:

```python
>>> root = asts.AST("x + 88", language=asts.ASTLanguage.Python, deepest=True)
>>> root.ast_type()
'PYTHON-BINARY-OPERATOR'
```

### AST Templates

#### Templates for building ASTs

ASTs may also be created using the AST template mechanism.  For
instance, the following snippet creates an AST equivalent to
`asts.AST("x = 2", language=asts.ASTLanguage.Python, deepest=True)`:

```python
>>> root = asts.AST.ast_template("$ID = 2", asts.ASTLanguage.Python, id="x")
>>> root.source_text()
'x = 2'
```

Metavariable names (e.g. `$ID` above) may contain uppercase characters,
digits, or underscores.  Metavariables may be scalars (e.g. `$`) or
lists (e.g. `@`), as shown below:

```python
>>> root = asts.AST.ast_template("fn(@ARGS)", asts.ASTLanguage.Python, args=[1,2,3])
>>> root.source_text()
'fn(1, 2, 3)'
```

Metavariables may also be positional arguments (e.g. `$1`, `$2`), as
shown below:

```python
>>> root = asts.AST.ast_template("$1 = 2", asts.ASTLanguage.Python, "x")
>>> root.source_text()
'x = 2'
```

However, you may not combine positional (e.g. `$1`) and keyword
(e.g. `$ID`) metavariables in a single template.  The corresponding
metavariable values passed in as arguments to `ast_template` may be
ASTs, literals, or lists.

#### Templates for building and destructuring ASTs

ASTs may also be directly created for the metavariables in an AST
template.  For instance, in the template `"$1 = $2"`, we may create
ASTs for `$1` and `$2` using `asts_from_template`, as shown below:

```python
>>> asts = asts.AST.asts_from_template("$1 = $2", asts.ASTLanguage.Python, "x", 1)
>>> len(asts)
2
>>> asts[0].source_text()
'x'
>>> asts[1].source_text()
'1'
```

For now, only the position syntax (e.g. `$1`, `$2`) is supported by
`asts_from_template`.  One AST is returned per metavariable, in
numeric order.

#### More information

More information on AST templates may be found in the SEL
[template documentation][].

### AST Copy

Copies of an AST may created using `AST.copy` or the python `copy`
module, as shown below:

```python
>>> root = asts.AST("x + 1", asts.ASTLanguage.Python, deepest=True)
>>> copy = asts.AST.copy(root)
>>> copy.source_text()
'x + 1'
```

In addition, clients may set child slots in the copy by passing in new
ASTs for each slot as keyword arguments, as shown below:

```python
>>> root = asts.AST("x + 1", asts.ASTLanguage.Python, deepest=True)
>>> y = asts.AST("y", asts.ASTLanguage.Python, deepest=True)
>>> copy = asts.AST.copy(root, python_left=y)
>>> copy.source_text()
'y + 1'
```

In addition to ASTs, clients may also pass in literals (e.g. strings,
code snippets, numbers) as keyword values, as shown below.  These are
automatically parsed into an AST to be inserted.

```python
>>> root = asts.AST("x + 1", asts.ASTLanguage.Python, deepest=True)
>>> copy = asts.AST.copy(root, python_left=1)
>>> copy.source_text()
'1 + 1'
>>> copy = asts.AST.copy(root, python_left="y")
>>> copy.source_text()
'y + 1'
```

To view the names of an AST's child slots, you may use the
`child_slots` method, as shown below:

```python
>>> root = asts.AST("x + 1", asts.ASTLanguage.Python, deepest=True)
>>> root.child_slots()
[['BEFORE-ASTS', 0], ['PYTHON-LEFT', 1], ['PYTHON-OPERATOR', 1],
 ['PYTHON-RIGHT', 1], ['CHILDREN', 0], ['AFTER-ASTS', 0]]
```

## AST Methods

### Common Operations

In practice, most clients will interact with ASTs by retrieving the AST
type, source text, parent AST, and child ASTs.  All of these operations
are supported by the python API.  To begin, let's examine retrieving AST
type.

There are two ways to retrieve the AST type using the python API -
`ast_type` and `ast_types`.  `ast_type` returns a string representation
of the most specialized subclass the object is an instance of;
`ast_types` returns a list of strings representing the entire class
hierarchy the object is embedded within on the Common Lisp side of the
interface.  This class hierarchy is rich and contains generic mixins
for common AST types across languages; for instance,
`"IF-STATEMENT-AST"`, `"STATEMENT-AST"`, `"IDENTIFIER-AST"`, etc.  For
cross-language applications, it is useful to utilize these
language-agnostic types.  An example of the `ast_type` and `ast_types`
methods is shown below; please note that `CALL-AST` is a generic,
cross-language mixin type for all function callsite ASTs.

```python
>>> root = asts.AST("print(x)", language=asts.ASTLanguage.Python, deepest=True)
>>> root.ast_type()
'PYTHON-CALL'
>>> root.ast_types()
['PYTHON-CALL', 'PYTHON-PRIMARY-EXPRESSION', 'PYTHON-EXPRESSION', 'PYTHON-AST',
 'TREE-SITTER-AST', 'INDENTATION', 'STRUCTURED-TEXT', 'FUNCTIONAL-TREE-AST',
 'NODE', 'IDENTITY-ORDERING-MIXIN', 'OID-OBJECT', 'STORED-HASH', 'CALL-AST',
 'EXPRESSION-AST', 'AST', 'STANDARD-OBJECT', 'SLOT-OBJECT']
```

Beyond AST types, retrieving the source text is another common
operation.  This may be accomplished using the `source_text` method,
as shown below:

```python
>>> root = asts.AST("print(x)", language=asts.ASTLanguage.Python, deepest=True)
>>> root.source_text()
'print(x)'
```

Finally, subtrees and parent trees may be accessed using the `children`
and `parent` methods, as shown below.  Please note that the parent
method requires the root of the subtree as a parameter.

```python
>>> root = asts.AST("print(x)", language=asts.ASTLanguage.Python, deepest=True)
>>> root.children()
[<asts.asts.AST object at 0x7fe1c31f9310>, <asts.asts.AST object at 0x7fe1c31f9370>]
>>> root.children()[0].source_text()
'print'
>>> identifier = root.children()[1].children()[0]
>>> identifier.source_text()
'x'
>>> identifier.parent(root).source_text()
'(x)'
```

An AST is composed of various child slots which are concatenated together
when using the `children` method.  To view the child slots for a particular
AST you may use the `child_slots` method, which returns a list of slot-name,
arity pairs.  An arity of one indicates the slot is a single AST, while
an arity of zero indicates the slot is composed of zero or more ASTs.
The AST(s) comprising a given slot may be accessed using the `child_slot`
method.  An example is shown below:

```python
>>> root = asts.AST("print(x)", language=asts.ASTLanguage.Python, deepest=True)
>>> root.child_slots()
[['BEFORE-ASTS', 0], ['PYTHON-FUNCTION', 1], ['PYTHON-ARGUMENTS', 1],
 ['CHILDREN', 0], ['AFTER-ASTS', 0]]
>>> root.child_slot("PYTHON-FUNCTION").source_text()
'print'
```


### Source Locations

For some applications, it may be useful to know the start/end locations
of each AST or retrieve the AST at a given location.  Clients may do
both using the `ast_source_ranges` and `ast_at_point` methods
respectively, as shown below.  Please note that for each method the
lines and columns are 1-indexed.

```python
>>> root = asts.AST("print(x)", language=asts.ASTLanguage.Python, deepest=True)
>>> root.ast_source_ranges()
[[<asts.asts.AST object at 0x7fe1c31f5f10>, [[1, 1], [1, 9]]],
 [<asts.asts.AST object at 0x7fe1c31f5d90>, [[1, 1], [1, 6]]],
 [<asts.asts.AST object at 0x7fe1c31f5df0>, [[1, 6], [1, 9]]],
 [<asts.asts.AST object at 0x7fe1c31f5bb0>, [[1, 7], [1, 8]]]]
>>> root.ast_at_point(1, 7).source_text()
'x'
```

### Functions

Function ASTs have special consideration in the python API, and clients
may retrieve various function attributes, such as name, parameters, and
body, using the respective AST methods, `function_name`,
`function_parameters`, and `function_body`, as shown below:

```python
>>> root = asts.AST("def foo(bar: int) -> int:\n    return bar / 2",
...                 language=asts.ASTLanguage.Python,
...                 deepest=True)
>>> root.function_name()
'foo'
>>> [param.source_text() for param in root.function_parameters()]
['bar: int']
>>> root.function_body().source_text()
'    return bar / 2'
```

### Function Callsites

In addition to function ASTs, function callsites also have special
consideration in the python API.  Clients may query for the library
containing the function implementation (`provided_by`), the function
portion of the callsite (`call_function`), and the callargs
(`call_arguments`).  An example incorporating these methods is shown
below:

```python
>>> root = asts.AST("import json\njson.dumps({})", language=asts.ASTLanguage.Python)
>>> callsite = root.children()[-1].children()[-1]
>>> callsite.provided_by(root)
'json'
>>> callsite.call_function().source_text()
'json.dumps'
>>> [callarg.source_text() for callarg in callsite.call_arguments()]
['{}']
```

## AST Traversal

ASTs may be explictly traversed in pre-order using the `traverse` method
which creates a generator that may be used anywhere a python `iterable`
is required.  An example usage is shown below:

```python
>>> root = asts.AST("x + 88", language=asts.ASTLanguage.Python)
>>> for a in root.traverse():
...     print(a.ast_type())
PYTHON-MODULE
PYTHON-EXPRESSION-STATEMENT-0
PYTHON-BINARY-OPERATOR
PYTHON-IDENTIFIER
PYTHON-+
PYTHON-INTEGER
```

Additionally, AST objects are themselves iterators and may be used
anywhere a python `iterable` is required, as shown below:

```python
>>> root = asts.AST("x + 88", language=asts.ASTLanguage.Python)
>>> for a in root:
...     print(a.ast_type())
PYTHON-MODULE
PYTHON-EXPRESSION-STATEMENT-0
PYTHON-BINARY-OPERATOR
PYTHON-IDENTIFIER
PYTHON-+
PYTHON-INTEGER
```

As expected, ASTs may be also be used in list comprehensions as shown:

```python
>>> root = asts.AST("x + 88", language=asts.ASTLanguage.Python)
>>> ids = [a for a in root if a.ast_type() == 'PYTHON-IDENTIFIER']
>>> len(ids)
1
```

## AST Manipulation

ASTs may also be manipulated (mutated) using the python API.  Mutation
operations create a new AST distinct from the inputs.  The input ASTs
may continue to be used as before; however, they are unchanged objects
distinct from the AST(s) created.

### Mutation Primitives

Currently, clients may cut, insert, or replace AST subtrees, as shown:

CUT:
```python
>>> root = asts.AST("x = 2\n", language=asts.ASTLanguage.Python)
>>> stmt = root.children()[0]
>>> root = asts.AST.cut(root, stmt)
>>> root.source_text()
''
```

INSERT:
```python
>>> root = asts.AST("y = 3\n", language=asts.ASTLanguage.Python)
>>> stmt = root.children()[0]
>>> new_stmt = asts.AST("x = 2\n", language=asts.ASTLanguage.Python, deepest=True)
>>> root = asts.AST.insert(root, stmt, new_stmt)
>>> root.source_text()
'x = 2\ny = 3\n'
```

REPLACE:
```python
>>> root = asts.AST("x = 2\n", language=asts.ASTLanguage.Python)
>>> literal = root.children()[0].children()[0].children()[-1]
>>> new_literal = asts.AST("3", language=asts.ASTLanguage.Python, deepest=True)
>>> root = asts.AST.replace(root, literal, new_literal)
>>> root.source_text()
"x = 3\n"
```

# Architecture

The python library is a thin wrapper around a Common Lisp program named
`tree-sitter-interface` which calls the required pieces of the
Software Evolution Library ([SEL][]).  Most API calls are delegated to
this interface which we communicate with using JSON formatted input/
output over stdio/stdout or a socket.

The python AST objects contain a handle attribute representing an
object id (oid) on the Common Lisp side of the interface; in essence,
the python ASTs are pointers to Common Lisp memory locations.  When
calling a python AST method, the oid is serialized to the Common Lisp
side of the interface where the underlying AST object is found
(dereferenced) and the operation performed.  You may get the object id
using the `get_oid` method on python ASTs; to test for python AST
equality, we check to see if the ASTs point to the same object using
the oids.

To allow for garbage collection, the ASTs are manually reference
counted.  Whenever a python AST (pointer) is created, the reference
count for the associated Common Lisp AST is incremented.  Similarly,
as python ASTs are garbage collected the reference counter is
decremented.  When the reference counter reaches zero, the Common Lisp
AST is released and may be garbage collected at the appropriate time.
To get the reference count for a particular python AST, you may use
the `ast_ref_count` method.

The underlying Common Lisp ASTs are themselves treated as immutable.
Therefore, when performing mutation operations (e.g. cut, replace,
insert), new ASTs are created in the process.

# License

GPLv3+

[tree-sitter]: https://tree-sitter.github.io/tree-sitter/
[SEL]: https://grammatech.github.io/sel/index.html#Software-Evolution-Library
[template documentation]: https://grammatech.github.io/sel/Templates.html#Templates
