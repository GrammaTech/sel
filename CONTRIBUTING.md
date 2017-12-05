% How to Contribute
% GrammaTech

Basic procedures to contribute code or documentation.

- [Setup](#setup)
    - [User Quicklisp](#user-quicklisp)
- [Coding Standards](#coding-standards)
    - [Use the compiler(s)](#use-the-compiler(s))
    - [Whitespace](#whitespace)
    - [Comments (number of semicolons matters)](#comments-number-of-semicolons-matters)
    - [Use existing utility functions (don't write your own)](#use-existing-utility-functions-dont-write-your-own)
    - [Packages](#packages)
    - [Portability](#portability)
    - [Documentation](#documentation)
    - [Don't use superfluous `let` or `let*` bindings](#dont-use-superfluous-let-or-let-bindings)
    - [Map instead of iterate](#map-instead-of-iterate)
    - [Judicious use of "arrow" (`->`, `->>`, etc...) macros](#judicious-use-of-arrow-etc-macros)

- [Testing](#testing)
    - [Unit Tests](#unit-tests)
    - [REPL in Docker image](#repl-in-docker-image)
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

Of particular importance are the following points:


### Use the compiler(s)

All code should compile without warning in both SBCL and CCL.  Files
may easily be compiled from within Emacs by running `M-x
slime-compile-file` (which is typically bound to `C-c M-k`).  These
warnings are often useful and catch type errors and outright mistakes
that can easily slip through testing.  This will also ensure
[portability](#portability).


### Whitespace

- no tabs
- no closing parenthesis on lines by themselves
- indent everything as would GNU Emacs
- typically only include vertical whitespace between top-level
   forms, sections of large functions may be demarcated by vertical
   whitespace but it is better to use smaller functions
- no trailing whitespace
- no whitespace following an open-paren


### Comments (number of semicolons matters)

- All comments should be complete sentences with capitalization and a period.
- 3 (or 4) semicolons at the beginning of a line for block comments outside of any top level form
- 2 semicolons for comments that appear between lines of code
- 1 semicolon for comments that appear after code at the end of a line
- vertical align end-of-line comments when possible
- always use a space after the last semicolon and before comment text
- In Emacs `M-;` inserts a comment of the appropriate type.


### Use existing utility functions (don't write your own)

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


### Packages

Only use packages which are explicitly included in your current
package.  E.g., calling `cl-fad:foo` just because `cl-fad` happens to
be loaded in the lisp image every time you've run tests is *not*
acceptable.  Instead the `:use` option to `defpackage` should
explicitly include the required package and if necessary `:shadow` and
`:shadowing-import-from` should be used to limit the symbols imported
(see their use in the package.lisp file in this directory for an
example).


### Portability

All code should be portable across at least
[SBCL](http://www.sbcl.org/) and [CCL](https://ccl.clozure.com/).  Any
code which is specific to a particular implementation must be
protected by `#+impl` guards.


### Documentation

#### For functions and methods

Write documentation strings for *every* function *always*.  Also,
ensure every `defmethod` has a `defgeneric` which has a documentation
string.

#### For files and modules

Large new functional modules (e.g., a new file of code) should be
documented in the manual.  The SEL manual is located in the `doc/`
subdirectory and is written in texinfo.  Follow the example set by
existing documentation to add new sections to this manual.


### Don't use superfluous `let` or `let*` bindings

Any `let*` which can be changed to a `let` should be changed to a `let`.

Typically, if a let-bound variable is only used once it should not be
bound but instead it's definition should replace it's sole use.  This
is suggested because each variable binding forces every subsequent
reader to perform a dereference.  For example, this

```lisp
(let* ((subject (quick brown fox))
       (object (lazy dog))
       (sentence (The subject jumps over the object.)))
  sentence)
```

is harder to read than this.

```lisp
(The quick brown fox jumps over the lazy dog)
```

There are some exceptions to this rule:

- If the variable is often used in debugging (printf or debugger) and
    it thus matters that it is bound at that point in the code then
    this may be acceptable.

- If the let-bound value appears as, lets say, the third argument to
    some function the in-lining of the long calculation may obscure
    the flow of the function with it's other arguments.

- If use of the let avoids horrible indentation issues then it maybe
    be acceptable.


### Map instead of iterate

Generally `mapc`, `mapcar`, `mappend`, and `reduce` should be
preferred to use of the `iterate` macro (which should itself be
preferred to `loop` which should never be used).  In general `iterate`
should be limited to cases where non-trivial accumulation variables or
incremental state are needed.  Potentially there are cases where a
straightforward `iterate` has better indentation behavior, in which
case it might be acceptable (but I can't think of one now).


### Judicious use of "arrow" (`->`, `->>`, etc...) macros

We sometimes over-use the `->` and `->>` macros.  These *typically*
only make sense to chain multiple calls, not single (or often double)
nested call.  One exception here is to aid indentation (avoid going
over 80 characters) when regular nesting would require contortionist
indentation.  For example in the following excerpt from our tests.
This


```lisp
    (is (equalp (->> (stmt-starting-with-text *collatz* "int collatz")
                     (function-body *collatz*))
                #|...|#))
```

is nicer than this.

```lisp
    (is (equalp (function-body
                 *collatz*
                 (stmt-starting-with-text *collatz* "int collatz"))
                #|...|#))
```


## Testing

### Unit Tests

Unit tests are defined and run using
[Stefil](http://www.cliki.net/Stefil).  See their documentation for
more information.  Stefil's integration into the REPL makes it a
comfortable tool for test driven development (meaning defining a test
of desired behavior before implementing the behavior, and then
leveraging the test and fixture from the REPL during development).


### REPL in Docker image

Sometimes it is useful to run a REPL in a docker image and connect to
that REPL from your host machine.  This allows for interactive
debugging of error conditions which may only occur in the image.  The
following steps will launch a docker image (in this case) for SEL, and
connect to it from Emacs running on your host.

1. Ensure you have the latest version of the image.

        docker pull docker.grammatech.com:14850/synthesis/sel:arch-linux

2. Launch the image using your local machine's network stack (NOTE:
     this may pose a security risk --
     https://docs.docker.com/articles/networking/).

        docker run --net=host -e LOCAL_USER=root -it docker.grammatech.com:14850/synthesis/sel:arch-linux /bin/bash

3. In the image cd into the `/gt/sel` directory and launch a swank
    server.  (If you'll be running tests build the test artifacts as
    well.)

        make test-artifacts
        SWANK_PORT=4005 make swank

4. Connect to the swank instance running in the image from Emacs on
    your host with `M-x slime-connect RET 127.0.0.1 RET 4005 RET`.

Alternately, you can build the sel docker image locally, and then
connect to it.

1. Build the image locally.

        sed 's/CI_COMMIT_SHA/master/' Dockerfile > Dockerfile.tmp
        docker build -f Dockerfile.tmp .

2. Run swank in the resulting image (where `${IMAGE_HASH}` is the hash
    of the image built in step 1 printed to STDOUT at the end of the
    build).

        docker run --net=host -e LOCAL_USER=root -it ${IMAGE_HASH} \
              /bin/bash -c "cd /gt/sel && SWANK_PORT=4005 make swank"

    The options have the following effects.
    - `--net=host` runs the image on the host's network stack
    - `-it` run the image interactive so it keeps running after swank
         is launched

3. Connect to the swank instance running in the image from Emacs on
    your host with `M-x slime-connect RET 127.0.0.1 RET 4005 RET`.

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
