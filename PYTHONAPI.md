# Introduction

Below I have listed the API functions currently being utilized by
argument prediction and simplified AST creation.  For each function,
I tried to annotate the parameters and return types, provide a brief
description of the function itself, and provide a rationale for its
usage.

# API Functions

- parse/create(text: Text, language: Enum[cpp, python,...]) -> SWObject:
    Instantiate a tree-sitter software object from source code.

    We will need an interface to parse text into ASTs.

- genome(obj: SWObject) -> Node:
    Retrieve the genome of the software object (AST tree root).

    This will allow us to retrieve the root of the AST tree for
    usage in the functions below.

- node\_at\_point(root: Node, line: int, col: int) -> Node:
    Return the node at a given point (line, col).

    This is utilized in callarg prediction to get the AST under the cursor
    when a prediction code action is invoked.

- parent(node: Node) -> Node:
    Return the parent AST of a given node.

    This is utilized in callarg prediction to find the enclosing function
    callsite around the cursor when a prediction code action is invoked.

- children(node: Node) -> List[Node]:
    Return the children of the node.

    This is not stricly necessary, but is an easy addition to the API
    that is likely to come in handy at some future point.

- prefix(node: Node) -> Text:
    Return the non-code text prior to the AST.

    For the leaf nodes of simplified ASTs, we store leading non-code text as
    a "leading" field on the node.  We should be able to reconstruct the
    full text of the AST from the simplified form.  See also [1].

- suffix(node: Node) -> Text:
    Return the non-code text trailing the AST.

    For the leaf nodes of simplified ASTs, we store trailing non-code text as
    a "trailing" field on the node.  We should be able to reconstruct the
    full text of the AST from the simplified form.  See also [1]

- source-text(node: Node) -> Text:
    Return the source text for the given AST.

    For the leaf nodes of simplified ASTs, we store the source text as a
    "token" field on the node.  We should be able to reconstruct the full
    the text of the AST from the simplified form.  See also [1]. 

- hash(node: Node) -> int:
    Return a hashcode for the given AST.

    This is utilitized to ensure we do not store duplicate simplified ASTs
    in the database and for deduplication of the the corpora of programs
    itself.

- type(node: Node) -> Enum[Function, Identifier, IfStmt, ...]
    Return the type of the node (in a language-agnostic form, if possible).

    In the simplified ASTs, we mark non-keyword and non-operator leaf nodes
    as "leafs" (see also [1]).  Additionally, we need to identify keyword
    and operator nodes when generating the simplified AST label.  Finally,
    when creating a database of callarg information for argument prediction,
    we need a way to distinguish variable identifier callargs from constant
    callargs such as string and integer literals.

- in\_scope\_names(node: Node) -> List[str]:
    Return a list of names which are in scope for the given node.
    Note this includes globally available names such as `__file__`
    and `__name__` in python.

    This is utilized in callarg prediction to retrieve candidate
    argument completions and in AST simplification to determine
    if an AST is a variable.

- functions(node: Node) -> List[Node]:
    Return a list of functions (both free functions and object methods)
    defined in node and its children.

    This is utilized when generating simplified ASTs as the
    paper calls for iterating over each function and generating
    a simpilified AST from each function body.

- function\_name(node: Function) -> str:
    Return the fully qualified name of the function (free function
    or object method).  Examples are given below:
    ```
    def foo(): => foo

    def bar():
        def foo(): => bar.foo

    class Baz:
        def foo(): => Baz.foo
    ```

    This is utilized when generating a database of simplified ASTs.
    We walk over each function body in the file and store a mapping
    of fully qualified name => simplified AST in a database.
    The fully qualified name allows us to avoid collisions for
    common names such as `__init__` for python.

- function\_body(node: Function) -> Node:
    Return the body of the function (free function or object method).

    This is utilized when generating simplified ASTs as we
    create the simplified ASTs from function bodies.

- callsites(node: Node) -> List[Node]:
    Return a list of function callsites in node and its children.
    These may be complete or incomplete function callsites (e.g.
    `foo(a, b)` or `foo(a,`).

    This is utilized in callarg prediction when building a database
    of API usage and associated common callargs.  Additionally, it
    is used in simplified AST creation when building a index of
    API usage -> simplified ASTs containing calls to the given API.
    Finally, it may be utilized to find the enclosing function
    callsite around the cursor when a code action is invoked
    for argument prediction.

- callsite\_signature(node: Callsite) -> List[str]:
    Return a list of parameters from the function definition of the
    API utilized at the callsite node.  The callsite may be complete
    or incomplete (e.g. `foo(a, b)` or `foo(a,`).

    This is utilized in callarg prediction.  We determined empirically
    that ~40% of callarg identifiers match the corresponding function
    parameter exactly; we use this knowledge to search for names in scope
    which match (or approximately match) the parameter name corresponding
    to the given callarg.

- callsite\_module(node: Callsite) -> str:
    Return the module the function at the callsite is defined within.
    The callsite may be complete or incomplete (e.g. `foo(a, b)`
    or `foo(a,`).  For builtin functions, such as `open` or `print`,
    `builtins` is returned; additionally, the function currently
    handles aliases (e.g. `import foo as bar`), name imports
    (e.g. `from foo import bar`), and object methods (e.g.
    `path.resolve()`) properly.

    We utilize this function in callarg prediction when building
    a database mapping module -> function -> common callargs.
    Additionally, when building simplified AST trees, we store
    a mapping of module -> function -> simplified ASTs containing
    a call to the given API which we can utilize when generating
    example usages.  Finally, when building the simplified AST
    trees, we normalize function calls so all invocations
    use the full, unaliased module and function name (e.g.
    `os.path.join`); this allows the algorithm described in
    "Exampla Gratis (E.G.): Code Examples for Free" [2] to perform
    optimally.

- callsite\_function(node: Callsite) -> str:
    Return the name of the function being invoked at the callsite.
    The callsite may be complete or incomplete (e.g. `foo(a, b)`
    or `foo(a,`).

    The usage of this function is the same as `callsite_module`
    above.

- callargs(node: Callsite) -> List[Node]:
    Return a list of callargs for the function call at node.
    The callsite may be complete or incomplete (e.g. `foo(a, b)`
    or `foo(a,`).

    We utilize this function to build a database of common callargs
    for a given API and during argument prediction directly to
    determine the current argument being predicted.

# API Implementation Thoughts

More technically, for the LISP/python interface, I don't believe
we need access to the AST nodes themselves; a pointer handle would
suffice as long as we are able to query for certain items
(e.g. the type of the AST pointed to) as described above.
Therefore, the SWObject and Node types annotated above could
likely just be pointers.

Beyond this, I'm unsure if the `callsite_module`/`callsite_signature`
functions might be more easily implemented on the python side of
the interface using ctags or another method of resolving callsites
to defintions.  If they were to be implemented in python, we would
likely need to expose a few more functions from the LISP interface,
such as one to retrieve the imports/includes in scope.

# References

[1] https://github.com/facebookresearch/aroma-paper-artifacts/blob/master/reference/data/example_query.json

[2] https://arxiv.org/abs/2011.01407
