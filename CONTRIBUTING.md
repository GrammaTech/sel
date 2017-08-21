# How to contribute

Basic procedures to contribute code or documentation.

- [Setup](#setup)
    - [User Quicklisp](#user-quicklisp)
- [Coding Standards](#coding-standards)
- [Testing](#testing)
    - [Unit Tests](#unit-tests)
- [Commit Review and Merge Requests](#commit-review-and-merge-requests)
    - [Git Commit Messages](#commit-messages)

## Setup

Install [SBCL](http://www.sbcl.org/) and
[QuickLisp](https://www.quicklisp.org/beta/).

If you want to compile binaries install
[BuildApp](http://www.xach.com/lisp/buildapp/).

### User Quicklisp

Assuming you want to use the Makefile set the `USER_QUICK_LISP`
environment variable to point to your local quicklisp installation
(e.g., `~/quicklisp`).

1. Ensure at least the following repositories are installed in your
    `${USER_QUICK_LISP}/local-projects` directory (you can consult any
    project's `qlfile` for a list of required packages which aren't
    automatically found by Quicklisp):
    [curry-compose-reader-macros](https://github.com/eschulte/curry-compose-reader-macros.git),
    [cl-arrows](https://github.com/eschulte/cl-arrows),
    [elf](https://github.com/eschulte/elf),

2. Clone the project's directory into your
    `${USER_QUICK_LISP}/local-projects` directory.

3. Start up your lisp normally, it should load quicklisp by default
    assuming you added it to your local init file.  Run the following
    to load your project and all required dependencies.

        ? (ql:register-local-projects)

        NIL

        ? (ql:quickload :PROJECT)
        To load "PROJECT":
          Load 1 ASDF system:
            PROJECT
        ; Loading "PROJECT"
        ...
        (:PROJECT)

## Coding Standards

Google's
[Common Lisp guide](http://google.github.io/styleguide/lispguide.xml)
is generally applicable.  Specifically the sections on
[#Formatting](http://google.github.io/styleguide/lispguide.xml#Formatting)
and
[#Comment_semicolons](http://google.github.io/styleguide/lispguide.xml?showone=Comment_semicolons#Comment_semicolons).

Of particular importance are the following points.

- whitespace 
    - no tabs
    - no closing parenthesis on lines by themselves
    - indent everything as would GNU Emacs
    - typically only include vertical whitespace between top-level
       forms, sections of large functions may be demarcated by vertical
       whitespace but it is better to use smaller functions
    - no trailing whitespace
    - no whitespace following an open-paren

- comments (number of semicolons matters)
    - 3 (or 4) semicolons at the beginning of a line for block comments
       outside of any top level form
    - 2 semicolons for comments that appear between lines of code
    - 1 semicolon for comments that appear after code at the end of a
       line
    - vertical align end-of-line comments when possible
    - always use a space after the last semicolon and before comment
       text

Regardless of language you should look carefully for existing utility
functions before re-implementation (what you want probably already
exists!).  For common lisp in particular you should check the
following places before implementation of any utility.

   1. Run `(apropos "thing")` in the repl

   2. Look in the
      [hyperspec](http://www.lispworks.com/documentation/HyperSpec/Front/)
      (this lookup is a simple key-combo from a slime mode and is
      worth learning).  The hyperspec is a reference, good for lookup
      and bad for browsing.

   3. Check the
      [Alexandria](https://common-lisp.net/project/alexandria/)
      package.

   4. Check the "utilities" package of SEL.

## Testing

### Unit Tests

Unit tests are defined and run using
[Stefil](http://www.cliki.net/Stefil).  See their documentation for
more information.  Stefil's integration into the REPL makes it a
comfortable tool for test driven development (meaning defining a test
of desired behavior before implementing the behavior, and then
leveraging the test and fixture from the REPL during development).

## Commit Review and Merge Requests

All changes should go through a merge request before landing in the
master branch.  The following should be confirmed before accepting any
merge request.

- Every merge request should first pass all unit-tests.  These are
    typically defined in a `PROJECT-test` project within the project
    repository.  This should be confirmed for both SBCL and CCL.

- All modified files should compile *without warning* for both SBCL
    and CCL.

- All new code should confirm to these coding standards.

- Every commit message should follow the
  [Git Commit Messages](#commit-messages) standards.

### Git Commit Messages

Follow [the seven rules of git commit messages](https://chris.beams.io/posts/git-commit/#seven-rules).
