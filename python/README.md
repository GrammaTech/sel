<!--TOC-->

- [Generic Tree-Sitter AST API](#generic-tree-sitter-ast-api)
- [Quick Intro](#quick-intro)
- [Extended Tutorial](#extended-tutorial)
  - [AST Creation](#ast-creation)
    - [Constructor](#constructor)
    - [From String](#from-string)
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
    - [Transformers](#transformers)
- [Architecture](#architecture)
- [FAQ](#faq)
  - [ASTs package does not faithfully reproduce original text](#asts-package-does-not-faithfully-reproduce-original-text)
  - [ASTs package inserts/deletes characters upon mutation.](#asts-package-insertsdeletes-characters-upon-mutation)
  - [ASTs package throws "Unable to match ... on on AST of type ..."](#asts-package-throws-unable-to-match--on-on-ast-of-type-)
  - [What is the difference between the "children" property and the "children" child slot?](#what-is-the-difference-between-the-children-property-and-the-children-child-slot)
  - [Why does every AST has a child slot named "children"?](#why-does-every-ast-has-a-child-slot-named-children)
  - [ASTs package throws "tree-sitter-interface crashed"](#asts-package-throws-tree-sitter-interface-crashed)
- [License](#license)

<!--TOC-->

# Generic Tree-Sitter AST API

The ASTs package provides a Python API into GrammaTech's Software
Evolution Library ([SEL][]) for source code manipulation.  SEL
generalizes over GitHub's [tree-sitter][] parsing libraries providing
a uniform interface over multiple programming languages (primarily
Python, JavaScript/TypeScript, and C/C++), and providing additional
functionality for software inspection and modification.

# Quick Intro

Here's how to create and perform some common operations on an AST:

```python3
$ python3
Python 3.8.5
Type "help", "copyright", "credits" or "license" for more information.
>>> import asts
>>> root = asts.AST.from_string("x + 88", language=asts.ASTLanguage.Python)
>>> root.children
[<asts.types.PythonExpressionStatement0 0x2>]
>>> root.children[0].children
[<asts.types.PythonBinaryOperator 0x3>]
>>> root.children[0].children[0].children
[<asts.types.PythonIdentifier 0x4>,
 <asts.types.PythonAdd 0x5>,
 <asts.types.PythonInteger 0x6>]
>>> root.children[0].children[0].children[0].source_text
'x'
>>> root.children[0].children[0].children[1].source_text
'+'
>>> root.children[0].children[0].children[2].source_text
'88'
>>> root.children[0].children[0].source_text
'x + 88'
>>> root.children[0].children[0].child_slots
[['LEFT', 1], ['OPERATOR', 1], ['RIGHT', 1], ['CHILDREN', 0]]
>>> list(map(lambda x:x.source_text, root.children[0].children[0].children))
['x', '+', '88']
```

# Extended Tutorial

The following examples assume you have imported the asts library using
`import asts`.  See the methods provided by asts.py for more
information.

<!-- TODO: Setup automatic documentation building. -->

## AST Creation

### Constructor

The default `AST` constructor should not be invoked directly by clients.
Instead, the static factory methods described below should be utilized.

### From String

ASTs may be created from source text using the `AST.from_string`
factory method in the Python API.  Using this method requires source
text and (optionally) a language enumeration indicating the source
text language.  The example below shows the creation of a simple AST:

```python
>>> root = asts.AST.from_string("x + 88", language=asts.ASTLanguage.Python)
```

Language enumerations exist for `Python`, `C`, `Cpp`, `Javascript`,
`TypescriptTs`, and `TypescriptTsx`.

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
... """
>>> root = asts.AST.from_string(text)
```

Finally, by default, the AST returned is the top-level, module node of
the parsed AST.  However, in some cases, we may wish get the deepest
subnode still encompassing all of the given source text.  This allows
us to create statement AST nodes, for instance.  To do so, clients
may use the `deepest` keyword, as shown below:

```python
>>> root = asts.AST.from_string(
...     "x + 88",
...     language=asts.ASTLanguage.Python,
...     deepest=True
... )
>>> type(root)
<class 'asts.types.PythonBinaryOperator'>
```

### AST Templates

#### Templates for building ASTs

ASTs may also be created using the AST template mechanism.  For
instance, the following snippet creates an AST equivalent to
`asts.AST.from_string("x = 2", language=asts.ASTLanguage.Python, deepest=True)`:

```python
>>> root = asts.AST.ast_template("$ID = 2", asts.ASTLanguage.Python, id="x")
>>> root.source_text
'x = 2'
```

Metavariable names (e.g. `$ID` above) may contain uppercase characters,
digits, or underscores.  Metavariables may be scalars (e.g. `$`) or
lists (e.g. `@`), as shown below:

```python
>>> root = asts.AST.ast_template("fn(@ARGS)", asts.ASTLanguage.Python, args=[1,2,3])
>>> root.source_text
'fn(1, 2, 3)'
```

Metavariables may also be positional arguments (e.g. `$1`, `$2`), as
shown below:

```python
>>> root = asts.AST.ast_template("$1 = 2", asts.ASTLanguage.Python, "x")
>>> root.source_text
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
>>> asts[0].source_text
'x'
>>> asts[1].source_text
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
>>> root = asts.AST.from_string("x + 1", asts.ASTLanguage.Python, deepest=True)
>>> copy = asts.AST.copy(root)
>>> copy.source_text
'x + 1'
```

In addition, clients may set child slots in the copy by passing in new
ASTs for each slot as keyword arguments, as shown below:

```python
>>> root = asts.AST.from_string("x + 1", asts.ASTLanguage.Python, deepest=True)
>>> y = asts.AST.from_string("y", asts.ASTLanguage.Python, deepest=True)
>>> copy = asts.AST.copy(root, left=y)
>>> copy.source_text
'y + 1'
```

In addition to ASTs, clients may also pass in literals (e.g. strings,
code snippets, numbers) as keyword values, as shown below.  These are
automatically parsed into an AST to be inserted.

```python
>>> root = asts.AST.from_string("x + 1", asts.ASTLanguage.Python, deepest=True)
>>> copy = asts.AST.copy(root, left=1)
>>> copy.source_text
'1 + 1'
>>> copy = asts.AST.copy(root, left="y")
>>> copy.source_text
'y + 1'
```

To view the names of an AST's child slots, you may use the
`child_slots` method, as shown below:

```python
>>> root = asts.AST.from_string("x + 1", asts.ASTLanguage.Python, deepest=True)
>>> root.child_slots
[['LEFT', 1], ['OPERATOR', 1],['RIGHT', 1], ['CHILDREN', 0]]
```

Alternatively, you may use the `dir` built-in to inspect an AST object
and find its child slots as python properties, as shown below:

```python
>>> root = asts.AST.from_string("x + 1", asts.ASTLanguage.Python, deepest=True)
>>> dir(root)
['__class__', ..., 'after_asts', ..., 'before_asts', ...,
 'left', ..., 'operator', ..., 'right', ..., 'traverse']
```

## AST Methods

### Common Operations

In practice, most clients will interact with ASTs using the AST's type and
retrieving the AST's source text, parent AST, and child ASTs.  All of
these operations are supported by the python API.  To begin, let's examine
using AST types.

ASTs are embedded in a rich type hierarchy generated by the tree-sitter
library; this type hierarchy is augmented by generic mixins in SEL for
common AST types across languages.  For instance, `IfAST`,
`StatementAST`, and `IdentifierAST` classes abstract common language
constructs and allow identification of these types of ASTs regardless
of the language of the underlying program.  As with other objects,
clients may use `isinstance` to test if an AST is an instance or
subclass of a given type, as shown below:

```python
>>> root = asts.AST.from_string(
...     "print(x)",
...     language=asts.ASTLanguage.Python,
...     deepest=True
... )
>>> isinstance(root, asts.PythonCall)
True
>>> isinstance(root, asts.CallAST)
True
>>> isinstance(root, asts.PythonAST)
True
>>> isinstance(root, asts.CStringLiteral)
False
```

Beyond AST types, accessing the source text of an AST is
another common operation.  This may be accomplished using
the `source_text` property on ASTs, as shown below:

```python
>>> root = asts.AST.from_string(
...     "print(x)",
...     language=asts.ASTLanguage.Python,
...     deepest=True
... )
>>> root.source_text
'print(x)'
```

Additionally, to view a list of the immediate children
of a particular AST, the `children` property may be used,
as shown below:

```python
>>> root = asts.AST.from_string(
...     "print(x)",
...     language=asts.ASTLanguage.Python,
...     deepest=True
... )
>>> root.children
[<asts.types.PythonIdentifier 0x4>, <asts.types.PythonArgumentList1 0x5>]
>>> root.children[0].source_text
'print'
>>> identifier = root.children[1].children[0]
>>> identifier.source_text
'x'
```

An AST is composed of various child slots which are concatenated together
when accessed via the `children` property.  To view the child slots for
a particular AST you may use the `child_slots` property, which returns
a list of slot-name, arity pairs.  An arity of one indicates the slot
is a single AST, while an arity of zero indicates the slot is composed
of zero or more ASTs.  The AST(s) comprising a given slot may be
accessed using the slot name as a python property accessor or by using
`child_slot` method, as shown below:

```python
>>> root = asts.AST.from_string(
...     "print(x)",
...     language=asts.ASTLanguage.Python,
...     deepest=True
... )
>>> root.child_slots
[['FUNCTION', 1], ['ARGUMENTS', 1], ['CHILDREN', 0]]
>>> root.function.source_text
'print'
>>> root.child_slot("FUNCTION").source_text
'print'
```

Finally, parent trees may be accessed using the `parent` and `parents`
methods, as shown below.  Please note that these methods require the
root of the subtree as a parameter.

```python
>>> root = asts.AST.from_string(
...     "print(x)",
...     language=asts.ASTLanguage.Python,
...     deepest=True
... )
>>> root.children
[<asts.types.PythonIdentifier 0x4>, <asts.types.PythonArgumentList1 0x5>]
>>> root.children[0].source_text
'print'
>>> identifier = root.children[1].children[0]
>>> identifier.source_text
'x'
>>> identifier.parent(root).source_text
'(x)'
>>> [p.source_text for p in identifier.parents(root)]
['(x)', 'print(x)']
```

#### Pattern Matching

In Python 3.10+, AST types and properties may be used in
[pattern matching][] for conditional logic and to destructure
an AST into its components parts.  For instance, to match
against assignments where the left-hand side of the assignment
is the variable "x", the following match clause may be used:

```python
>>> root = asts.AST.from_string(
...     "x = 1",
...     language=asts.ASTLanguage.Python,
...     deepest=True
... )
>>> match root:
...     case asts.PythonAssignment(left=asts.IdentifierAST(source_text="x")):
...         print("Match found")
...
Match found
```

As another example, to destructure the left-hand and right-hand sides
of an assignment into separate variables, the following match
clause may be used:

```python
>>> root = asts.AST.from_string(
...     "x = 1",
...     language=asts.ASTLanguage.Python,
...     deepest=True
... )
>>> match root:
...     case asts.PythonAssignment(left=lhs, right=rhs):
...         [lhs, rhs]
...
[<asts.types.PythonIdentifier 0x4>, <asts.types.PythonInteger 0x5>]
```

### Source Locations

For some applications, it may be useful to know the start/end locations
of each AST or retrieve the AST at a given location.  Clients may do
both using the `ast_source_ranges` and `ast_at_point` methods
respectively, as shown below.  Please note that for each method the
lines and columns are 1-indexed.

```python
>>> root = asts.AST.from_string(
...     "print(x)",
...     language=asts.ASTLanguage.Python,
...     deepest=True
... )
>>> root.ast_source_ranges()
[[<asts.types.PythonCall 0x3>, [[1, 1], [1, 9]]],
 [<asts.types.PythonIdentifier 0x4>, [[1, 1], [1, 6]]],
 [<asts.types.PythonArgumentList1 0x5>, [[1, 6], [1, 9]]],
 [<asts.types.PythonIdentifier 0x6>, [[1, 7], [1, 8]]]]
>>> root.ast_at_point(1, 7).source_text
'x'
```

### Functions

Function ASTs have special consideration in the python API, and clients
may retrieve various function attributes, such as name, parameters, and
body, using the respective AST methods, `function_name`,
`function_parameters`, and `function_body`, as shown below:

```python
>>> root = asts.AST.from_string(
...     "def foo(bar: int) -> int:\n    return bar / 2",
...     language=asts.ASTLanguage.Python,
...     deepest=True
... )
>>> root.function_name()
'foo'
>>> [param.source_text for param in root.function_parameters()]
['bar: int']
>>> root.function_body().source_text
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
>>> root = asts.AST.from_string(
...     "import json\njson.dumps({})",
...     language=asts.ASTLanguage.Python
... )
>>> callsite = root.children[-1].children[-1]
>>> callsite.provided_by(root)
'json'
>>> callsite.call_function().source_text
'json.dumps'
>>> [callarg.source_text for callarg in callsite.call_arguments()]
['{}']
```

## AST Traversal

ASTs may be explictly traversed in pre-order using the `traverse` method
which creates a generator that may be used anywhere a python `iterable`
is required.  An example usage is shown below:

```python
>>> root = asts.AST.from_string("x + 88", language=asts.ASTLanguage.Python)
>>> for a in root.traverse():
...     print(a)
<asts.types.PythonModule 0x1>
<asts.types.PythonExpressionStatement0 0x2>
<asts.types.PythonBinaryOperator 0x3>
<asts.types.PythonIdentifier 0x4>
<asts.types.PythonAdd 0x5>
<asts.types.PythonInteger 0x6>
```

Additionally, AST objects are themselves iterators and may be used
anywhere a python `iterable` is required, as shown below:

```python
>>> root = asts.AST.from_string("x + 88", language=asts.ASTLanguage.Python)
>>> for a in root:
...     print(a)
<asts.types.PythonModule 0x1>
<asts.types.PythonExpressionStatement0 0x2>
<asts.types.PythonBinaryOperator 0x3>
<asts.types.PythonIdentifier 0x4>
<asts.types.PythonAdd 0x5>
<asts.types.PythonInteger 0x6>
```

As expected, ASTs may be also be used in list comprehensions as shown:

```python
>>> root = asts.AST.from_string("x + 88", language=asts.ASTLanguage.Python)
>>> ids = [a for a in root if isinstance(a, asts.PythonIdentifier)]
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
>>> root = asts.AST.from_string("x = 2\n", language=asts.ASTLanguage.Python)
>>> stmt = root.children[0]
>>> root = asts.AST.cut(root, stmt)
>>> root.source_text
''
```

INSERT:
```python
>>> root = asts.AST.from_string(
...     "y = 3\n",
...     language=asts.ASTLanguage.Python
... )
>>> stmt = root.children[0]
>>> new_stmt = asts.AST.from_string(
...     "x = 2\n",
...     language=asts.ASTLanguage.Python,
...     deepest=True
... )
>>> root = asts.AST.insert(root, stmt, new_stmt)
>>> root.source_text
'x = 2\ny = 3\n'
```

REPLACE:
```python
>>> root = asts.AST.from_string(
...     "x = 2\n",
...     language=asts.ASTLanguage.Python
... )
>>> literal = root.children[0].children[0].children[-1]
>>> new_literal = asts.AST.from_string(
...     "3",
...     language=asts.ASTLanguage.Python,
...     deepest=True
... )
>>> root = asts.AST.replace(root, literal, new_literal)
>>> root.source_text
"x = 3\n"
```

As a useful shortcut, for small mutations, literals may be passed as the
values for insertion and replacement, as shown below:

```python
>>> root = asts.AST.from_string(
...     "x = 2\n",
...     language=asts.ASTLanguage.Python
... )
>>> literal = root.children[0].children[0].children[-1]
>>> root = asts.AST.replace(root, literal, 3)
>>> root.source_text
"x = 3\n"
```

### Transformers

In addition to simple mutation primitives, the API also supports walking
the AST tree and performing transformations on the nodes within.  This
mode of mutation requires the client to define a *transformer* function
which takes an AST node parameter as input and (optionally) returns an
AST or literal which should replace that node in the new tree.  If no
new node is provided by the transformer function, the node is not
replaced in the newly created tree.

For instance, consider a scenario where you wish to rewrite an AST,
replacing all `x` identifiers with `y`.  To begin, you would define
an `x_to_y` transformer function, as shown below:

```python
>>> def x_to_y(ast: asts.AST) -> Optional[asts.LiteralOrAST]:
...     """Convert 'x' identifier ASTs to 'y'."""
...     if isinstance(ast, asts.IdentifierAST) and "x" == ast.source_text:
...         return asts.AST.from_string("y", ast.language, deepest=True)
...
```

As you can see, this function returns a `y` identifier when it encounters
an `x` identifier.  To use the transformer to replace nodes in an AST tree,
you would use the `AST.transform` function, as shown below:

```python
>>> text = """
... x = 1
... print(x)
... """
>>> root = asts.AST.from_string(text, asts.ASTLanguage.Python)
>>> print(root.source_text.strip())
x = 1
print(x)
>>> transformed = asts.AST.transform(root, x_to_y)
>>> print(transformed.source_text.strip())
y = 1
print(y)
```

In the example above, the `x_to_y` transformer returned an AST.  The
transformer function may also return a literal value, which will be
parsed as an AST.  For instance, the below `x_to_y` transformer is
functionally equivalent to the example above:

```python
>>> def x_to_y(ast: asts.AST) -> Optional[asts.LiteralOrAST]:
...     """Convert 'x' identifier ASTs to 'y'."""
...     if isinstance(ast, asts.IdentifierAST) and "x" == ast.source_text:
...         return "y"
...
```

Additionally, when using Python 3.10+, you may use [pattern matching][]
to further simplify the implementation of the `x_to_y` transformer,
as shown below:

```python
>>> def x_to_y(ast: asts.AST) -> Optional[asts.LiteralOrAST]:
...     """Convert 'x' identifier ASTs to 'y'."""
...     match ast:
...         case asts.IdentifierAST(source_text="x"):
...             return "y"
...
```

Transformers may implement more complicated logic than the simple
`x_to_y` transform above.  For instance, one may wish to convert
`x` identifiers to `y`, but only for the left-hand side of assignment
statements, as shown below:

```python
>>> def x_to_y_assignment_lhs(ast: asts.AST) -> Optional[asts.LiteralOrAST]:
...     """Convert 'x' identifier ASTs to 'y' on the lhs of assignments."""
...     if (
...         isinstance(ast, asts.PythonAssignment)
...         and "x" == ast.left.source_text
...     ):
...         return asts.AST.copy(ast, left="y")
...
>>> text = """
... x = 1
... print(y)
... """
>>> root = asts.AST.from_string(text, asts.ASTLanguage.Python)
>>> transformed = asts.AST.transform(root, x_to_y_assignment_lhs)
>>> print(transformed.source_text.strip())
y = 1
print(y)
```

As before, the `x_to_y_assignment_lhs` transformer may be simplified
using [pattern matching][] in Python 3.10+, as shown below:

```python
>>> def x_to_y_assignment_lhs(ast: asts.AST) -> Optional[asts.LiteralOrAST]:
...     """Convert 'x' identifier ASTs to 'y' on the lhs of assignments."""
...     match ast:
...         case asts.PythonAssignment(left=asts.IdentifierAST(source_text="x")):
...             return asts.AST.copy(ast, left="y")
...
```

For these more complicated transforms, you may need to mutate the parent
of the node you are interested in.  For instance, above we create a
new python assignment node with the left-hand side modified to the
value we want.

Tying everything together, consider the case where wish to delete
all `print` statements from an AST.  This may be accomplished by
first defining a predicate for print statements, as shown below:

```python
>>> def is_print_statement(ast: asts.AST) -> bool:
...     """Return TRUE if AST is an statement calling the print function."""
...     if isinstance(ast, asts.ExpressionStatementAST):
...         fn_calls = [c.call_function().source_text for c in ast.call_asts()]
...         return "print" in fn_calls
...     return False
...
```

Once this predicate is in place, we may use it to define a transformer which
returns a node with the `print` statements immediately below it elided:

```python
>>> def delete_print_statements(ast: asts.AST) -> Optional[asts.LiteralOrAST]:
...     """Delete all print statements from the children of AST."""
...     if isinstance(ast, (asts.RootAST, asts.CompoundAST)):
...         # Build a list of new, non-comment children directly under the AST,
...         # eliding print statements.
...         new_children = [
...             c for c in ast.child_slot("children")
...             if not is_print_statement(c)
...         ]
...
...         # Special case; if no children remain, add a "pass" statement nop to
...         # avoid syntax errors.
...         new_children = new_children if new_children else ["pass\n"]
...
...         # Create the new node with print statements removed from the children.
...         return asts.AST.copy(ast, children=new_children)
...
```

Finally, as before, we may use the `delete_print_statements` transformer
to mutate an AST, as shown below:

```
>>> text = """
... x = 1
... y = 2
... if x > 1:
...     x = x ** y
...     print("Test One: %d" % x)
... else:
...     print("Test Two: %d" % x)
... print("y = %d", y)
... """
>>> root = asts.AST.from_string(text, asts.ASTLanguage.Python)
>>> transformed = asts.AST.transform(root, delete_print_statements)
>>> print(transformed.source_text.strip())
x = 1
y = 2
if x > 1:
    x = x ** y
else:
    pass
```

# Architecture

The python library is a thin wrapper around a Common Lisp program named
`tree-sitter-interface` which calls the required pieces of the
Software Evolution Library ([SEL][]).  Most API calls are delegated to
this interface which we communicate with using JSON formatted input/
output over stdio/stdout or a socket.

The python AST objects contain a oid attribute representing an
object id (oid) on the Common Lisp side of the interface; in essence,
the python ASTs are pointers to Common Lisp memory locations.  When
calling a python AST method, the oid is serialized to the Common Lisp
side of the interface where the underlying AST object is found
(dereferenced) and the operation performed.  You may get the object id
using the `oid` property on python ASTs; to test for python AST
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

# FAQ

## ASTs package does not faithfully reproduce original text

The source text property of an AST created using the ASTs package
may not match the original text given to the parser, even if no
mutations are performed.  An automatic source code formatter, such
as `black` or `clang-format`, may be used on the original source
text and the source text attribute of the parsed AST before printing
to ensure consistency if this is required.  This flexibility in
parsing allows for us to perform structured mutation operations -
for instance, automatically inserting a comma when an item is
added to an initializer list in C++.

## ASTs package inserts/deletes characters upon mutation.

As part of a mutation, the ASTs package may insert or delete whitespace
or separator characters (e.g. commas) between ASTs.  One common
idiom is to use an automatic source code formatter, such as `black`
or `clang-format`, on the source text after mutation to ensure
consistency before printing to screen or disk.

## ASTs package throws "Unable to match ... on on AST of type ..."

This error occurs after a mutation when an AST is inserted or
replaces another and the type of the new AST does not match that
expected by the tree-sitter grammer.  These errors are often subtle
and may be tricky to diagnose.  As an example, consider the following:

```python
>>> func = asts.AST.from_string("print(a)", asts.ASTLanguage.Python, deepest=True)
>>> type(func)
<class 'asts.types.PythonCall'>
```

In the above snippet, we create a `func` AST representing a call to the print
function.  Consider now that we wish to replace the `print` call with something
different, say `foo`.  We may believe the following will perform
the operation:

```python
>>> foo = asts.AST.from_string("foo", asts.ASTLanguage.Python)
>>> new_func = asts.AST.copy(func, function=foo)
>>> new_func.source_text
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File ".../Python-3.8.12/lib/python3.8/functools.py", line 967, in __get__
    val = self.func(instance)
  File ".../python/asts/asts.py", line 228, in source_text
    return _interface.dispatch(AST.source_text.func.__name__, self)
  File ".../python/asts/asts.py", line 589, in dispatch
    return deserialize(handle_errors(json.loads(response.decode())))
  File ".../python/asts/asts.py", line 539, in handle_errors
    raise ASTException(data["error"])
asts.asts.ASTException: An unhandled error condition has been signalled: Unable to match
(SEQ (FIELD PYTHON-FUNCTION PYTHON-PRIMARY-EXPRESSION)
 (FIELD PYTHON-ARGUMENTS PYTHON-GENERATOR-EXPRESSION PYTHON-ARGUMENT-LIST))
on AST of type
PYTHON-CALL
```

However, this does not work properly.  If we look at the type of `foo` and the type
of the function child slot of `new_func`, we can see as to why.

```python
>>> type(foo)
<class 'asts.types.PythonModule'>
>>> type(new_func.child_slot("function"))
<class 'asts.types.PythonModule'>
```

In this case, we attempt to insert a `PythonModule` AST as the function child
slot of `func`.  A `PythonModule` AST is a "root" AST type, associated with an
entire python file, replete with imports, functions, etc.  This AST type does
not make sense as the "function" portion of a call AST; in this case,
we want an identifier AST.  We can get the identifier by passing the `deepest`
keyword during AST creation, retrieving the deepest subnode still
encompassing all of the given source text, as shown below:


```python
>>> foo = asts.AST.from_string("foo", asts.ASTLanguage.Python, deepest=True)
>>> type(foo)
<class 'asts.types.PythonIdentifier'>
>>> new_func = asts.AST.copy(func, function=foo)
>>> new_func.source_text
'foo(a)'
```

## What is the difference between the "children" property and the "children" child slot?

An AST is composed of several child slots, the arity of which may
be 0 (zero or more) or 1 (single AST).  For instance, in the AST
for `print(a)`, there are two child slots, `[['FUNCTION', 1], ['ARGUMENTS', 1]]`,
allowing for destructuring into component parts of the AST.

In addition, internally, each AST has private before/after-AST slots
for storing comments that appear in the source code.  For instance,
in the AST for the function body below, the `return 0` AST has
the comment `# This is a comment` stored in the internal before-AST
slot.

```python
def foo():
    # This is a comment
    return 0
```

The `children` property on AST will return the concatenated list of
component slots of an AST and any comments included in each AST's
internal before or after AST slot.  The order of the ASTs will
match the order they appear in the source text.

The `children` child slot on an AST will return all ASTs not
assigned to an explicit slot (e.g. 'FUNCTION', 'ARGUMENTS' above)
and will NOT include any comment ASTs stored internally in an
AST's before/after-AST slots.

When using the AST `copy` method, care should be taken when
using the `children` key (e.g. `AST.copy(ast, children=...)`).
In this case, we are setting the `children` slot and not the
property.  Therefore, in almost all cases, we will want to
use the children slot, not property, of another AST to populate
the keyword argument (e.g. `AST.copy(ast, children=other.child_slot("children"))`).
If the property is utilized and a comment is added to the copy
AST's children slot, the error described above in "ASTs package
throws 'Unable to match...'" will occur.

## Why does every AST has a child slot named "children"?

To begin, lets consider the AST for `print(a)`; for this AST there
are two explicit child slots not named `children`:
`[['FUNCTION', 1], ['ARGUMENTS', 1]]`.  All ASTs not assigned to an
explicit child slot on an AST (e.g. "FUNCTION" or "ARGUMENTS"
previously) are assigned to the "catchall" slot named "children".
Additionally, for ASTs with no named slots (e.g. function
bodies), all of the ASTs are stored in the slot named "children".
In essence, the slot serves as a fall-thru place to store all
ASTs not assigned to an explicit, named slot.

## ASTs package throws "tree-sitter-interface crashed"

This error is raised when the Common Lisp binary (`tree-sitter-interface`)
backing the ASTs python package crashes.  If available, the error will
report the standard output and standard error streams for the
`tree-sitter-interface` process prior to the crash.

The most common error cause occurs upon startup/import of the asts package,
and the error message will reference "wrong ELF class: ELFCLASS32.".  This
occurs on systems which do not support 32-bit shared libraries (which the
ASTs package depends upon); to remedy this, you will need to apt-install
`gcc-multilib` on Ubuntu systems.

# License

GPLv3+

[tree-sitter]: https://tree-sitter.github.io/tree-sitter/
[SEL]: https://grammatech.github.io/sel/index.html#Software-Evolution-Library
[template documentation]: https://grammatech.github.io/sel/Templates.html#Templates
[pattern matching]: https://www.python.org/dev/peps/pep-0636/
