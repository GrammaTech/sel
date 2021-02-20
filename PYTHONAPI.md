# Introduction

Below I have listed the API functions currently being used by
argument prediction and simplified AST creation.  For each function,
I tried to annotate the parameters and return types, provide a brief
description of the function itself, and provide a rationale for its
usage.

# API Functions

- `parse/create`(text: Text, language: Enum[cpp, python,...]) -> SWObject:
    Instantiate a tree-sitter software object from source code.

    We will need an interface to parse text into ASTs.

- `genome`(obj: SWObject) -> Node:
    Retrieve the genome of the software object (AST tree root).

    This will allow us to retrieve the root of the AST tree for
    usage in the functions below.

    > Instead of creating (`parse/create`) a software object and then
    > accessing it's ASTs (`genome`), what about just providing a
    > method to convert a string into an AST?

- `node_at_point`(root: Node, line: int, col: int) -> Node:
    Return the node at a given point (line, col).

    This is used in callarg prediction to get the AST under the cursor
    when a prediction code action is invoked.

- `parent`(node: Node) -> Node:
    Return the parent AST of a given node.

    This is used in callarg prediction to find the enclosing function
    callsite around the cursor when a prediction code action is invoked.

- `enclosing_function`(node: Node) -> Node:
    Return the enclosing function of a node.

- `children`(node: Node) -> List[Node]:
    Return the children of the node.

    This is not stricly necessary, but is an easy addition to the API
    that is likely to come in handy at some future point.

- `prefix`(node: Node) -> Text:
    Return the non-code text prior to the AST.

    For the leaf nodes of simplified ASTs, we store leading non-code text as
    a "leading" field on the node.  We should be able to reconstruct the
    full text of the AST from the simplified form.  See also [1].

- `suffix`(node: Node) -> Text:
    Return the non-code text trailing the AST.

    For the leaf nodes of simplified ASTs, we store trailing non-code text as
    a "trailing" field on the node.  We should be able to reconstruct the
    full text of the AST from the simplified form.  See also [1]

- `source-text`(node: Node) -> Text:
    Return the source text for the given AST.

    For the leaf nodes of simplified ASTs, we store the source text as a
    "token" field on the node.  We should be able to reconstruct the full
    the text of the AST from the simplified form.  See also [1]. 

- `hash`(node: Node) -> int:
    Return a hashcode for the given AST.

    This is used to ensure we do not store duplicate simplified ASTs
    in the database and for deduplication of the corpora of programs
    itself.

    > Which of the following is included in the hash?
    > - `AST text
    > - AST structure
    > - AST context
    >
    > Put another way, when should the hashes of two ASTs be equal?

- `type`(node: Node) -> Enum[Function, Identifier, IfStmt, ...]
    Return the type of the node (in a language-agnostic form, if possible).

    In the simplified ASTs, we mark non-keyword and non-operator leaf nodes
    as "leafs" (see also [1]).  Additionally, we need to identify keyword
    and operator nodes when generating the simplified AST label.  Finally,
    when creating a database of callarg information for argument prediction,
    we need a way to distinguish variable identifier callargs from constant
    callargs such as string and integer literals.

    > Would it be okay to return the "stack" of super-classes of the
    > AST.  Likely the most useful list would come from collecting
    > every occurance of the most specific class in
    > `*tree-sitter-ast-superclasses*`.

- `in_scope_names`(node: Node) -> List[str]:
    Return a list of names which are in scope for the given node.
    Note this includes globally available names such as `__file__`
    and `__name__` in python.

    This is used in callarg prediction to retrieve candidate
    argument completions and in AST simplification to determine
    if an AST is a variable.

- `functions`(node: Node) -> List[Node]:
    Return a list of functions (both free functions and object methods)
    defined in node and its children.

    This is used when generating simplified ASTs as the
    paper calls for iterating over each function and generating
    a simpilified AST from each function body.

    > (find-if {typep _ 'function-ast} ast)

- `function_name`(node: Function) -> str:
    Return the fully qualified name of the function (free function
    or object method).  Examples are given below:
    ```
    def foo(): => foo

    def bar():
        def foo(): => bar.foo

    class Baz:
        def foo(): => Baz.foo
    ```

    This is used when generating a database of simplified ASTs.
    We walk over each function body in the file and store a mapping
    of fully qualified name => simplified AST in a database.
    The fully qualified name allows us to avoid collisions for
    common names such as `__init__` for python.

- `function_body`(node: Function) -> Node:
    Return the body of the function (free function or object method).

    This is used when generating simplified ASTs as we
    create the simplified ASTs from function bodies.

    > We also have `function-parameters` and `function-arguments` both
    > of which might be useful?

- `callsites`(node: Node) -> List[Node]:
    Return a list of function callsites in node and its children.
    These may be complete or incomplete function callsites (e.g.
    `foo(a, b)` or `foo(a,`).

    This is used in callarg prediction when building a database
    of API usage and associated common callargs.  Additionally, it
    is used in simplified AST creation when building a index of
    API usage -> simplified ASTs containing calls to the given API.
    Finally, it may be used to find the enclosing function
    callsite around the cursor when a code action is invoked
    for argument prediction.

    > A bunch of these seem like they could easily be implemented with
    > tree traversal, type matching, and general accessors.  E.g. for
    > this one:
    >
    > ```common-lisp
    >   (remove-if-not (op (match _1 ((function-ast :function-name "foo") t))) ast)
    > ```
    >
    > Would it be better to expose these as whole functions from lisp,
    > or to expose the iterators and primitives, or both?
    >
    > I guess exposing traversal routines would involve callbacks in
    > the C/Python API which could ge complex...

- `callsite_signature`(node: Callsite) -> List[str]:
    Return a list of parameters from the function definition of the
    API used at the callsite node.  The callsite may be complete
    or incomplete (e.g. `foo(a, b)` or `foo(a,`).

    This is used in callarg prediction.  We determined empirically
    that ~40% of callarg identifiers match the corresponding function
    parameter exactly; we use this knowledge to search for names in scope
    which match (or approximately match) the parameter name corresponding
    to the given callarg.

    > So is it enough to get the function name and the
    > function-parameter names?  Do you want any other information?

- `callsite_module`(node: Callsite) -> str:
    Return the module the function at the callsite is defined within.
    The callsite may be complete or incomplete (e.g. `foo(a, b)`
    or `foo(a,`).  For builtin functions, such as `open` or `print`,
    `builtins` is returned; additionally, the function currently
    handles aliases (e.g. `import foo as bar`), name imports
    (e.g. `from foo import bar`), and object methods (e.g.
    `path.resolve()`) properly.

    We use this function in callarg prediction when building
    a database mapping module -> function -> common callargs.
    Additionally, when building simplified AST trees, we store
    a mapping of module -> function -> simplified ASTs containing
    a call to the given API which we can use when generating
    example usages.  Finally, when building the simplified AST
    trees, we normalize function calls so all invocations
    use the full, unaliased module and function name (e.g.
    `os.path.join`); this allows the algorithm described in
    "Exampla Gratis (E.G.): Code Examples for Free" [2] to perform
    optimally.

    > This one will actually require some non-trivial analysis.  We
    > can write a new `function-module` method in SEL.

- `callsite_function`(node: Callsite) -> str:
    Return the name of the function being invoked at the callsite.
    The callsite may be complete or incomplete (e.g. `foo(a, b)`
    or `foo(a,`).

    The usage of this function is the same as `callsite_module`
    above.

- `callargs`(node: Callsite) -> List[Node]:
    Return a list of callargs for the function call at node.
    The callsite may be complete or incomplete (e.g. `foo(a, b)`
    or `foo(a,`).

    We use this function to build a database of common callargs
    for a given API and during argument prediction directly to
    determine the current argument being predicted.

> This is great, I appreciate the clear expression of what's needed.
> Hopefully much of this is already immediately available in SEL and
> will be easy to expose.
>
> I suggest we basicalyl export every function and AST class exported
> by the sel/sw/ts package.  That should cover everything here after
> we add a couple of additional functions to that file.
>
> Note the library will also have an `init` function which will need
> to be called to boot the lisp environment before any additional
> functions can be called.  I don't know when this should happen (on
> module load or an explicit function invocation.)

# API Implementation Thoughts

More technically, for the LISP/python interface, I don't believe
we need access to the AST nodes themselves; a pointer handle would
suffice as long as we are able to query for certain items
(e.g. the type of the AST pointed to) as described above.
Therefore, the SWObject and Node types annotated above could
likely just be pointers.

> Yeah, this sounds like the best bet to me.  Probably some opaque
> pointer to the nodes themselves and then an Enum AST types?  (I
> don't know Python so let me know if something else would be more
> pythonic.)
>
> It might be nice to expose the existing sequence functions over
> nodes to Python.  Do any of these have natural analogs that you
> think would be useful?
>
> - `mapc` Traverse the software for side effects.  Should be easy.
>
> - `reduce` Traverse the software collecting the results into some
>   user supplied structure passed to the function with every node.
>
> - `remove-if` Return a list of the nodes satisfying a function.
>
> - `find-if` Return the first node satisfying a function.
>
> - `position-if` Return the path to the first node satisfying
>   function in some AST.  This path could be a sequence of numberic
>   indicies and slot names (enums?).
>
> - `@` Given a path into a tree, return the node at that location.
>
> It might be tempting to add support for modifying and creating
> nodes, but maybe we leave that out of the first pass at least.

Beyond this, I'm unsure if the `callsite_module`/`callsite_signature`
functions might be more easily implemented on the python side of
the interface using ctags or another method of resolving callsites
to defintions.  If they were to be implemented in python, we would
likely need to expose a few more functions from the LISP interface,
such as one to retrieve the imports/includes in scope.

> Yeah, I was wondering the same.  The nice part of implementing this
> upstream is that I imagine we're likely going to eventually want
> this same functionality in other projects downstream from SEL.  It
> might be marginally faster as well (may not matter).

# References

[1] https://github.com/facebookresearch/aroma-paper-artifacts/blob/master/reference/data/example_query.json

[2] https://arxiv.org/abs/2011.01407
