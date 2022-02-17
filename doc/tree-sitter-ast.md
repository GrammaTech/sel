---
title: "SEL's tree-sitter ASTs"
subtitle: (more like CSTs)
---

SEL provides abstract syntax trees (ASTs) which can be used for source
code transformation and (light) static analysis.  The ASTs contain all
original source information so they are technically concrete syntax
trees (CSTs) not ASTs although we will refer to them as ASTs
throughout.  Rather than abstracting the information in the ASTs, SEL
provides generic functions which enable the generic treatment of CSTs.

This document details SEL's use of tree-sitter to build its AST
representation.


# What is tree-sitter?

From <https://tree-sitter.github.io/tree-sitter/>:

> Tree-sitter is a parser generator tool and an incremental parsing
> library. It can build a concrete syntax tree for a source file and
> efficiently update the syntax tree as the source file is edited.

SEL's usage:

- Doesn't use the incremental abilities.

- Software transformation and analysis which varies widely from the
  primary upstream use for syntax highlighting.

## Benefits of tree-sitter

- Adoption by several projects, such as Neovim.

- Many available languages.

- Fairly uniform representation of languages.

  Previously, each language had a different way of getting an AST.
  These different ways often varied widely in how useful they were.
  As an example, Python 3's AST representation isn't the most ideal
  for SEL's use case.

- Many people and organizations contribute to tree-sitter resulting in
  high-quality grammars that span many languages and stay up to date
  with evolving languages.

## What does tree-sitter provide?

-   A shared object that provides functionality to take a string and
    generate a CST.  This CST is fairly error-resilient.

-   JSON files which specify the structure of the CST.  Specifically:

    - node-types.json

    - grammar.json (commonly referred to as "the JSON" in discussions)

## Pain Points

- Tree-sitter has trouble parsing some languages, such as C++.

- Frequently SEL needs to patch the ASTs that come from tree sitter to
  store enough information to reproduce source text.


# SEL Additions

Cross-language Mixins

:   Similar ideas, such as functions, loops, class definitions, etc.,
    can be labeled as such.

    -   This allows for generic function implementations which can be
        used across languages. As an example is the c/cpp mixin which
        allows C and C++ to easily share code. ECMAScript is another
        example allowing multiple JavaScript languages to share their
        implementation.

    -   Mixins are much simpler in lisp and multiple inheritance is much
        simpler to leverage with CLOS.

Source Text Reproduction

:   The source text can be reproduced from the AST.

Modification of Grammar and Node Types

:   The grammar can be patched and node definitions can be modified
    before the generation of lisp code. To support this, there's
    functionality to transform parse trees after they are read into lisp
    such that they match the new grammar and definitions.

    -   For example, there's an issue with newlines having a source
        text range that is larger than one character. This causes
        issues with languages which expect newlines in a
        rule/production. This is commonly a problem with the C
        preprocessor.

Validation of AST Structure

:   If an AST cannot be matched against the grammar, an error will be
    thrown.  This is convenient to flag grammatically invalid
    transformations immediately.  This is particularly useful for
    language agnostic transformations, such as cutting an AST or
    swapping ASTs.

Indentation Maintenance
:   Indentation is stored as the number of spaces. A variable can be set
    to allow for tabs and how many spaces a tab uses. This allows for
    the two most common identation styles--spaces and tabs + spaces. On
    top of this, insertion of new ASTs will maintain the correct amount
    of indentation. This functionality was developed to better support
    Python software.

# Structured Text

## Interleaved Text (Previous Representation)

-   ASTs are interleaved with strings in a list.

-   Eventually evolved into "named childlren"
    -   This assigned children their own named slot/field/member variable

-   This became very cumbersome to use and maintain the strings. As an
    example, removing or adding the 'else' clause of an 'if'
    statement required checking the interleaved text and patching it if
    necessary.

## Structured Text (Current Representation)

Assigns every child a relevant slot in the AST's class definition.
This allows for implicit text between children when printing ASTs back
to source and allows automation of much of the toil of AST
transformation.

### Implicit Text

A large portion of text can be inferred based on the class of an AST.
This is determined by its relevant rule/production in the grammar
file.  As an example, if you have a parenthesized expression AST then
the parenthesis aren't stored anywhere in an instance of that object.
Instead, this information is put into an output-transformation method
which transforms an AST into its relevant string representation, i.e.,
source text.

### Before/After slots

The before and after text slots store whitespace, comments, and any
other information that is, more or less, ignored by the grammar.

- This information is pushed down from the parent into the closest
  child AST.

- If the relevant information is surround by child ASTs, the relevant
  before slot is preferred over the after.  (This is by convention and
  heuristic as opposed to any technical reason.)

In practice the above two rules often results in the *expected*
result.  E.g., both functions at the top level and statements within a
function are often preceded by explanatory comments.  The above
assures that these comments move with the code to which they apply.

### Internal AST slots

There are places where whitespace and comments occur which do not have
any surrounding children. Instead of pushing it to children, it is
instead stored in internal AST slots. Adding these slots to nodes is
part of the code generation process.

### Computed Text (Variable Part)

Variable text, such as function names and variable names, need to be
computed and stored at AST creation. These can, however, have children
of their own, so they can't always be stored as only text.

-   Text Fragments are used to separate the children ASTs from the
    variable part. This maintains a list of ASTs instead of regressing
    back to the interleaved text representation.

### Source Text Fragments

In some cases the the grammar can't be matched exactly. This may occur
when a file is actively being edited. The resulting AST can no longer be
matched when validating the parent AST's rule. The parent is instead
turned into a source text fragment AST which still allows for source
text reproduction.

-   tree-sitter either adds a zero-width token to force a matching
    rule/production or removes tokens from consideration until something
    does match.

-   Note that tree-sitter frequently gets the parent AST's type
    incorrect when an error occurs, so source-text fragments protects
    against incorrect information.

### Choice Expansion Subclasses

Each choice/alternation in the rule/production for node or AST is
expanded to create a new subclass for each branch that could be taken.
This is done for every choice branch which is not in a
repeat/repetition.

-   We can only match/validate on information that is stored in the
    AST/node object. By creating these different subclasses, information
    is stored implicitly based on the subclass. This allows semantically
    identical ASTs to have different implicit source text.

-   Of note, when a subclass can no longer match on the values stored
    in its slots, it will go through every possible sibling subclass
    until it can match on one. This is done by literally changing the
    class of the object. This is particularly useful when mutating
    classes like `if` statements because removal or addition of the
    `else` clause no longer requires any analysis to determine if the
    source text is still valid.

# Code Generation

Much of the code building SEL's ASTs from tree-sitter's parser and
grammars is dynamically generated at build time.  SEL code parses
tree-sitter's grammars and then automatically builds the relevant SEL
classes and methods to support the above functionality.  This allows
SEL to *consistently* and *efficiently* track the latest tree-sitter
grammars.

The actual code used to drive this is only about 2-3 KLoC.  It
generates hundreds of KLoC of code.

<center>

| Language   | Generated LoC |
|------------|---------------|
| C          | ~18K          |
| Python     | ~20K          |
| JavaScript | ~20K          |
| Rust       | ~35K          |
| C++        | ~50K          |
| TypeScript | ~110K         |

</center>

The code generation process is shown below.

<center>

<!-- !include code-generation.md -->
![Code Generation](./figures/code-generation.svg)

</center>

The code generation sequence is as follows:

1. Identifies every tree-sitter module available for the system.
2. Both the node-types and grammar JSON files are read in
3. The initial classes are created based on the node-types file and
   stored in a hash table so that they can be modified during code
   generation
   1.  Additional slots, superclasses, and patches to node structures
       are added as specified by several special variables defined at
       the top-level.
4. The grammar JSON is extensively patched.
   1.  The rule/production is traversed looking for places where two
       unnamed/unnamed/terminal symbols occur back-to-back without
       either of them being assigned to a slot. Internal-ast slots are
       assigned in between these such that the whitespace and/or
       comments can be stored and reproduced.
   2.  All aliases have their rule/production added to the node types
       it aliases to by wrapping the rule in a choice/alternation and
       adding a new branch with it. This solved several reoccuring
       problems.
   3.  Several different versions of the rule are generated. Each
       serves a different purpose: unchanged json rule, pruned rule,
       and collapsed rule. The pruned rule removed any subtree which
       doesn't contain information stored that is to be stored on an
       object and replaces that subtree with nil. This is used
       extensively for matching parse trees and ASTs. The collapsed
       rule collapses nested choices and sequences on themselves, and
       further removes any nil's from the tree. It is only used for
       detecting problematic rules.
   4.  Problematic rules are identified and printed out at compile time.
       These are the rules where the correct reproduction of source text
       could be ambiguous due to lack of stored information.
5. Add functionality to classes
   1. Choice expansion subclasses are generated <!-- I don't know what there are -->
   2. Rules, choice expansion subclass information are attached to slots
      with class allocation.
   3. Several methods are generated.
      1.  A conversion from a parse tree to a tree-sitter-ast.
      2.  An output transformation which transfroms an AST into
          source-text. This inserts any implicit text.


# AST creation

The actual process of parsing source text and creating SEL ASTs is
shown in the following diagram.

<center>

<!-- !include ast-creation.md -->
![AST Creation](./figures/ast-creation.svg)

</center>

The sequence of steps is as follows:

1.  cl-tree-sitter calls tree-sitter SO and converts the C structure
    into a tree.
2.  transform-parse-tree traverses the tree and runs
    transform-parse-tree methods on any subtree which has a
    transformation defined. This allows for bugs and extra features to
    be patched by SEL.
3.  The transformed parse tree has surrounding text annotations added to
    it and inner whitespace subtrees are created for any whitespace that
    can't be attached to a child AST.
4.  The parse tree is passed to match-parsed-children and a valid choice
    expansion subclass is found if needed. Otherwise, it simply checks
    if the class matches if there aren't any subclasses. If it fails to
    match, an error occurs.
5.  An instance of the relevant class is instantiated and its slots are
    populated with its named children, internal ASTs, and surrounding
    text.
6.  The AST has its indentation converted from a string into a number.
