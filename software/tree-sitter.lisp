;;;; tree-sitter.lisp --- software representations with a tree-sitter backend.
;;;
;;; @menu
;;; * Setting up libtree-sitter:: Install the main tree-sitter library
;;; * Per-language Modules:: Install language-specific tree-sitter libraries
;;; * cl-tree-sitter Setup:: Install the Common Lisp tree-sitter bindings
;;; * Structured Text:: Tree-sitter ASTs with implicit structured source text
;;; * Templates:: Templates for easy AST creation and destructuring
;;; @end menu
;;;
;;; The @refapiclass{sel/sw/ts:tree-sitter} software object class is the primary
;;; base class for representing software source code which has been
;;; parsed into structured abstract syntax trees (ASTs) or more
;;; accurately Concrete Syntax Trees (CSTs).  This class uses GitHub's
;;; tree-sitter (see @url{https://tree-sitter.github.io/tree-sitter/})
;;; libraries to parse source code into ASTs.
;;;
;;; TREE-SITTER allows for many different languages to be represented
;;; uniformly through libtree-sitter and the analysis of its corresponding
;;; language modules. This cuts down on maintenance time across different
;;; languages and enables immediate inclusion of any language that has a
;;; language module for libtree-sitter.
;;;
;;; Tree sitter has support for many source code languages.  See
;;; @url{https://tree-sitter.github.io/tree-sitter/#available-parsers,Available
;;; Parsers} for full list.  The TREE-SITTER classes are generated at
;;; compile time. This is done by analyzing the @file{node-types.json}
;;; and @file{grammar.json} files that are located in tree-sitter
;;; language modules, of which the format can be found here:
;;; @url{https://tree-sitter.github.io/tree-sitter/using-parsers#static-node-types}.
;;; The @env{SEL_TREE_SITTER_LANGUAGE_DIR}
;;; environment variable can be set to modify where the JSON files are
;;; expected. By default,
;;; @file{/usr/share/tree-sitter/},@file{/usr/local/share/tree-sitter/},
;;; and @file{$HOME/.local/share/tree-sitter/} are searched for
;;; tree-sitter files. If a directory is found and has
;;; @file{node-types.json} and @file{grammar.json} in it, the
;;; directory's name is added to the list of languages to create
;;; classes for. For example, a directory named
;;; @file{/usr/share/tree-sitter/python/} that has the relevant JSON
;;; files will result in class for python being generated.  On the
;;; lisp side, SEL looks in the directories specified by
;;; @refapivariable{sel/sw/ts:*tree-sitter-language-directories*} to
;;; find the @code{grammar.json} and @code{node-types.json} files that
;;; define a tree-sitter language.  By default these files are
;;; expected to be in @code{/usr/share/tree-sitter/$language/}.  The
;;; @refapivariable{sel/sw/ts:*tree-sitter-language-directories*}
;;; variable may be customized to control where SEL searches for these
;;; files.  For even more control over the tree-sitter files used the
;;; @refapivariable{sel/sw/ts:*tree-sitter-language-files*} variable
;;; may be set directly.  These files are then parsed to automatically
;;; define associated common lisp classes for every type of AST node
;;; defined by the parser.  Currently only the
;;; @refapiclass{sel/sw/ts:python} and
;;; @refapiclass{sel/sw/ts:javascript} tree-sitter classes have the
;;; full complement of more sophisticated
;;; @refapiclass{sel/sw/parseable:parseable} methods (e.g.,
;;; @refapigeneric{sel/sw/parseable:scopes}) defined.
;;;
;;; TREE-SITTER support currently depends on cl-tree-sitter and the fork
;;; of CFFI it depends on. It also depends on libtree-sitter and
;;; tree-sitter language modules for every desired language.
;;;
;;; @node Setting up libtree-sitter, Per-language Modules, Source Code with @code{tree-sitter}, Source Code with @code{tree-sitter}
;;; @subsection Setting up libtree-sitter
;;; @cindex setting-up-libtree-sitter
;;;
;;; Clone the following repo: @url{https://github.com/tree-sitter/tree-sitter}.
;;;
;;; Run the following from its base directory:
;;;
;;;     # From tree-sitter/
;;;     # sudo make all install can run to do both of the following.
;;;     make
;;;     # Move the shared object to a place where it can be found.
;;;     sudo mv libtree-sitter.so /usr/lib/
;;;
;;; @node Per-language Modules, cl-tree-sitter Setup, Setting up libtree-sitter, Source Code with @code{tree-sitter}
;;; @subsection Per-language Modules
;;; @cindex pre-language-modules
;;;
;;; Each language has its own module that can be used with tree-sitter. All of
;;; the languages that are currently supported can be found here:
;;; @url{https://github.com/tree-sitter}.
;;;
;;; For convenience:
;;; - @url{https://github.com/tree-sitter/tree-sitter-c}
;;; - @url{https://github.com/tree-sitter/tree-sitter-java}
;;; - @url{https://github.com/tree-sitter/tree-sitter-javascript}
;;; - @url{https://github.com/tree-sitter/tree-sitter-json}
;;; - @url{https://github.com/tree-sitter/tree-sitter-python}
;;;
;;; To set-up languages, a script is provided as
;;; tools/tree-sitter-install.sh in the SEL directory.
;;;
;;; To install only a specific subset of languages, they can be passed at
;;; the command line:
;;;
;;;     tools/tree-sitter-setup.sh language1 language2
;;;
;;; To install all supported languages, run the script without
;;; specifying any languages:
;;;
;;;     tools/tree-sitter-setup.sh
;;;
;;; By default the tree-sitter parsers installed by the script are
;;; pinned to specific commits. You can install the latest versions by
;;; setting the NOPIN environment variable:
;;;
;;;     NOPIN=1 tools/tree-sitter-setup.sh
;;;
;;; On Arch Linux the
;;; @url{https://aur.archlinux.org/packages/tree-sitter-languages-git/,
;;; tree-sitter-languages-git} AUR package may be used to install
;;; tree-sitter support for many languages.
;;;
;;; On MacOS the following shell script may be used to achieve similar
;;; results:
;;;
;;;     #!/bin/bash
;;;
;;;     declare -a LANGUAGES
;;;     LANGUAGES=agda bash c c-sharp cpp css go html java javascript jsdoc json julia ocaml/ocaml ocaml/interface php python ql regex ruby rust scala typescript/tsx typescript/typescript
;;;
;;;     clone() {
;;;       for language in ${LANGUAGES};do
;;;         LANG="$(echo ${language}|sed 's|/|-|')"
;;;         git clone https://github.com/tree-sitter/tree-sitter-${LANG} src/tree-sitter-${LANG}
;;;       done
;;;     }
;;;
;;;     package() {
;;;       for language in ${LANGUAGES};do
;;;         echo $language
;;;         LANG="$(echo ${language}|sed 's|/|-|')"
;;;         BASE="tree-sitter-${LANG}"
;;;         cd src/tree-sitter-${language}/src
;;;         if test -f "scanner.cc"; then
;;;             c++ -fPIC scanner.cc -c -lstdc++
;;;             cc -std=c99 -fPIC parser.c -c
;;;             ar rcs ${BASE}.a scanner.o parser.o
;;;             c++ -dynamiclib -Wl,-install_name,/usr/local/lib/${BASE}.0.dylib scanner.o parser.o -o ${BASE}.0.0.dylib
;;;             ln -sf ${BASE}.0.0.dylib ${BASE}.dylib
;;;             ln -sf ${BASE}.0.0.dylib ${BASE}.0.dylib
;;;             sudo install -d "/usr/local/lib/tree-sitter/$LANG"
;;;             sudo install -m755 ${BASE}.a "/usr/local/lib/tree-sitter/${BASE}".a
;;;             sudo install -m755 ${BASE}.0.0.dylib '/usr/local/lib'/${BASE}.0.0.dylib
;;;             ln -sf ${BASE}.0.0.dylib '/usr/local/lib'/${BASE}.0.dylib
;;;             ln -sf ${BASE}.0.0.dylib '/usr/local/lib'/${BASE}.dylib
;;;         elif test -f "scanner.c"; then
;;;             cc -std=c99 -fPIC scanner.c -c
;;;             cc -std=c99 -fPIC parser.c -c
;;;             ar rcs ${BASE}.a scanner.o parser.o
;;;             c++ -dynamiclib -Wl,-install_name,/usr/local/lib/${BASE}.0.dylib scanner.o parser.o -o ${BASE}.0.0.dylib
;;;             ln -sf ${BASE}.0.0.dylib ${BASE}.dylib
;;;             ln -sf ${BASE}.0.0.dylib ${BASE}.0.dylib
;;;             sudo install -d "/usr/local/lib/tree-sitter/$LANG"
;;;             sudo install -m755 ${BASE}.a "/usr/local/lib/tree-sitter/${BASE}".a
;;;             sudo install -m755 ${BASE}.0.0.dylib '/usr/local/lib'/${BASE}.0.0.dylib
;;;             ln -sf ${BASE}.0.0.dylib '/usr/local/lib'/${BASE}.0.dylib
;;;             ln -sf ${BASE}.0.0.dylib '/usr/local/lib'/${BASE}.dylib
;;;         else
;;;             cc -std=c99 -fPIC parser.c -c
;;;             ar rcs ${BASE}.a parser.o
;;;             c++ -dynamiclib -Wl,-install_name,/usr/local/lib/${BASE}.0.dylib parser.o -o ${BASE}.0.0.dylib
;;;             ln -sf ${BASE}.0.0.dylib ${BASE}.dylib
;;;             ln -sf ${BASE}.0.0.dylib ${BASE}.0.dylib
;;;             sudo install -d "/usr/local/lib/tree-sitter/$LANG"
;;;             sudo install -m755 ${BASE}.a "/usr/local/lib/tree-sitter/${BASE}".a
;;;             sudo install -m755 ${BASE}.0.0.dylib '/usr/local/lib'/${BASE}.0.0.dylib
;;;             ln -sf ${BASE}.0.0.dylib '/usr/local/lib'/${BASE}.0.dylib
;;;             ln -sf ${BASE}.0.0.dylib '/usr/local/lib'/${BASE}.dylib
;;;         fi
;;;         sudo install -d /usr/local/share/tree-sitter/${language}/
;;;         sudo install -m644 grammar.json node-types.json /usr/local/share/tree-sitter/${language}/
;;;         cd - >/dev/null
;;;       done
;;;
;;;     }
;;;
;;;     mkdir -p src/
;;;     clone
;;;     package
;;;
;;; @node cl-tree-sitter Setup, Structured Text, Per-language Modules, Source Code with @code{tree-sitter}
;;; @subsection cl-tree-sitter Setup
;;; @cindex cl-tree-sitter-setup
;;;
;;; Clone the following repositories to the local-projects directory for quicklisp:
;;; - @url{https://github.com/death/cl-tree-sitter}
;;;
;;; @node Structured Text, Templates, cl-tree-sitter Setup, Source Code with @code{tree-sitter}
;;; @subsection Structured Text
;;; @cindex structured-text
;;;
;;; The classes generated for tree-sitter use the rules stored in each language's
;;; grammar file to enable implicit source text reproduction at
;;; the class level. This makes working with and mutating the AST much simpler.
;;; As an example, if an 'if' statement AST without an 'else' clause has an
;;; 'else'clause added to it, the source text of the AST will reflect that an
;;; 'else'clause has been added to it without needing to make any other updates.
;;; (Prior to structured text, slots holding connective white-space and punctuation
;;; required manual updates to accompany most changes to the content of an AST.)
;;;
;;; Each class that is generated can have multiple subclasses which represent
;;; the different representations of source text that the base class can take.
;;; For example, the update expression in C represents both the pre-increment
;;; and post-increment. Two subclasses are generated to disambiguate between the
;;; source text representations--one for pre-increment and one for
;;; post-increment.
;;;
;;; Frequently, these subclass ASTs can be copied with slight modifications to
;;; their slot values. This can leave the AST copy in an invalid state for the
;;; subclass it had been copied from. When this is detected, the AST's class
;;; will be changed dynamically to the first subclass of the base class which can
;;; successfully produce source text with the given slot values:
;;;
;;; SEL/SW/TS> (find-if (of-type 'python-parameters) (convert 'python-ast "def x(): pass"))
;;; #<PYTHON-EMPTY-PARAMETERS 847 :TEXT "()">
;;; SEL/SW/TS> (copy * :children (convert 'python-ast "a" :deepest t))
;;; #<PYTHON-PARAMETERS-0 847 :TEXT "(a)">
;;; SEL/SW/TS> (copy * :children nil)
;;; #<PYTHON-EMPTY-PARAMETERS 847 :TEXT "()">
;;;
;;; This behavior also applies to objects created with the base class, but it may
;;; choose a subclass that's source text is not the desired representation, so
;;; it's best to specify the exact subclass in case where this matters, such as
;;; update expressions in C.
;;;
;;; Structured text ASTs contain at least 4 slots which help store information
;;; that isn't implicit to the AST or its parent AST:
;;;
;;; - before-text :: stores text that directly precedes the AST but is not part
;;;                  of the rule associated with the AST. This is generally
;;;                  whitespace. This slot is preferred over the after-text slot
;;;                  when creating ASTs from a string with @code{#'convert}. It
;;;                  is possible for this to be an instance of `alternative-ast'
;;;                  instead of a string.
;;;
;;; - after-text :: stores text that directly procedes the AST but is not part
;;;                 of the rule associated with the AST. This is generally
;;;                 whitespace. This slot is preferred when a terminal token
;;;                 directly follows the AST which does not have a before-text
;;;                 slot due to being implicit source text. It is also possible
;;;                 for this to be an instance of `alternative-ast' instead of a
;;;                 string.
;;;
;;; - before-asts :: stores comment and error ASTs that occur before the AST
;;;                  and before the contents of the before-text slot. The
;;;                  contents of this slot are considered children of the parent
;;;                  AST. This slot is preferred over the after-text slot when
;;;                  creating ASTs from a string with #'convert.
;;;
;;; - after-asts :: stores comment and error ASTs that occur before the AST
;;;                 and after the contents of the after-text slot. The
;;;                 contents of this slot are considered children of the parent
;;;                 AST. This slot is preferred when a terminal token
;;;                 directly follows the AST which does not have a before-text
;;;                 slot due to being implicit source text.
;;;
;;; - internal-asts-|#| :: store ASTs which are between two terminal tokens
;;;                        which are implicit source text. This slot can contain
;;;                        comment, error and inner-whitespace ASTs.
;;;
;;; The internal-asts slots are generated based on the rule associated with the
;;; AST. Any possible place in the rule where two terminal tokens can appear
;;; consecutively, an internal-asts slot is placed.
;;;
;;; A further 'text' slot is also used for a subset of ASTs that are known
;;; computed-text ASTs. These ASTs hold information that is variable and must
;;; be computed and stored when the AST is created. The ASTs that are computed
;;; text can be identified by being of type @code{computed-text}.
;;;
;;; When creating ASTs, @code{patch-whitespace} can be used to insert whitespace in
;;; relevant places. This utilizes @code{whitespace-between} to determine how much
;;; whitespace should be placed in each slot. This currently does not populate
;;; inner-asts whitespace.
;;;
;;; @node Templates, , Structured Text, Source Code with @code{tree-sitter}
;;; @subsection Templates
;;; @cindex templates
;;; @subsubsection Templates for building ASTs
;;;
;;; SEL supports building (and destructuring) tree-sitter ASTs using
;;; string templates. Holes in string templates are called
;;; metavariables.
;;;
;;; The function that builds templates is called @code{ast-template}.
;;; You probably don't want to use this function directly; supported
;;; languages allow you to use a function with the same name as
;;; shorthand for creating a template:
;;;
;;;     (python "$ID = 1" :id "x")
;;;     ≡ (ast-template "$ID = 1" 'python-ast :id "x")
;;;
;;; There is also a function, @code{ast-template*}, that behaves like
;;; @code{ast-template} except that it parses tolerantly. What
;;; "tolerant" means depends on the language, but for C-like languages
;;; it means supplying a missing semicolon if one is required. Again,
;;; you probably don't want to use this function directly, since you
;;; can use starred versions of the language-specific shorthands.
;;;
;;;     (cpp "$X+$Y;" :x 1 :y 2)
;;;     => #<cpp-expression-statement "1+2;">
;;;
;;;     (cpp* "$X+$Y" :x 1 :y 2)
;;;     => #<cpp-binary-expression "1+2">
;;;
;;; By default metavariables look like @code{$X}, where the name can
;;; contain only uppercase characters, digits, or underscores. (The
;;; syntax of templates is inspired by
;;; @url{https://semgrep.dev,Semgrep}, but syntax can vary by
;;; language.)
;;;
;;; Metavariables can also look like @code{@ARGS}. In this case they stand for
;;; a list of ASTs, rather than a single AST.
;;;
;;;     (ast-template "fn(@ARGS)" :args '(1 2 3))
;;;     => <python-call "fn(1, 2, 3)">
;;;
;;; There are two syntaxes for the arguments to @code{ast-template}.
;;;
;;; The arguments can be keyword arguments, defaults for the
;;; corresponding metavariables (converting dashes to underscores):
;;;
;;;     (ast-template "$LEFT_HAND_SIDE = $RIGHT_HAND_SIDE" 'python-ast
;;;                   :left-hand-side "x"
;;;                   :right-hand-side 1)
;;;     => <python-assignment "x = 1">
;;;
;;; The arguments can be positional (keywords not allowed!). In this
;;; case metavariables must be numbered (@code{$1}, @code{$2}, etc.):
;;;
;;;     (ast-template "$1 = $2" 'python-ast "x" 1)
;;;     ≡ (ast-template "$1 = $2" 'python-ast :1 "x" :2 1)
;;;
;;;     ;; Also works for list arguments.
;;;
;;;     (ast-template "fn(@1)" '(1 2 3))
;;;     => <python-call "fn(1, 2, 3)">
;;;
;;; Values in the arguments must be ASTs, literals, or lists. Lists
;;; are processed recursively (but only to one level). Atoms that are
;;; not ASTs or literals are converted into ASTs using
;;; @code{template-subtree}, a generic function. Atoms that are ASTs
;;; are copied into the resulting tree. Atoms that are literals (such
;;; as strings or integers) are inlined into the string and parsed in
;;; place. (This is necessary as the AST that corresponds to a string
;;; may only be parseable in context.)
;;;
;;;     (ast-template "$1 = value" 'python-ast "x")
;;;     ≡ (ast-template "$1 = value" 'python-ast
;;;                     (convert "x" 'python-ast :deepest t))
;;;
;;; @subsubsection Templates for destructuring ASTs
;;;
;;; Both keyword and positional syntaxes (as well as list
;;; metavariables with @code{@}) can also be used as
;;; @url{https://github.com/guicho271828/trivia,Trivia} patterns for
;;; destructuring.
;;;
;;;     (match (python "x = 2 + 2")
;;;       ((python "$1 = $2" var (python "$X + $Y" :x x :y y))
;;;        (list var x y)))
;;;     => (#<python-identifier "x"> #<python-integer "2">
;;;         #<python-integer "2">)
;;;
;;; @subsubsection Templates that build and destructure
;;;
;;; You can combine building and destructuring into one step using
;;; @code{ast-from-template}.
;;;
;;;     (ast-from-template "$1;" 'cpp-ast "\"Foo: %d\"")
;;;     => #<cpp-string-literal "\"Foo: %d\"">
;;;
;;; Must use the positional syntax of @code{ast-template}. Returns one value
;;; per metavariable, in numeric order (@code{$1}, @code{$2}, etc.).
;;;
;;; This is useful because not every kind of AST node can be parsed
;;; directly as a template. E.g. in Python a tuple, an argument list,
;;; and a parameter list all use the same syntax and can only be
;;; distinguished in context. Using @code{ast-from-template} lets you
;;; provide throwaway context to the parser while pulling out only the
;;; particular nodes that you want.
;;;
;;; @texi{tree-sitter}
(uiop:define-package :software-evolution-library/software/tree-sitter
    (:nicknames :sel/software/tree-sitter :sel/sw/tree-sitter
                :sel/software/ts :sel/sw/ts)
  (:use :gt/full
        :babel
        :cl-json
        :functional-trees/attrs
        :software-evolution-library
        :software-evolution-library/utility/json
        :software-evolution-library/utility/range
        :software-evolution-library/utility/include
        :software-evolution-library/software/parseable
        :software-evolution-library/software/compilable
        :software-evolution-library/components/file
        :software-evolution-library/components/formatting)
  (:import-from :trivia.fail :fail)
  (:import-from :uiop)
  (:import-from :software-evolution-library/software/project
                :include-paths :include-paths-mixin)
  (:import-from :cffi :translate-camelcase-name :load-foreign-library-error)
  (:import-from :functional-trees :map-children)
  (:import-from :trivial-garbage :make-weak-hash-table)
  (:import-from :abstract-classes :abstract-class)
  #.(if (asdf:find-system :cl-tree-sitter nil)
        '(:import-from :cl-tree-sitter :register-language)
        (values))
  #.(if (asdf:find-system :cl-tree-sitter nil)
        '(:shadowing-import-from :cl-tree-sitter :parse-string)
        (values))
  ;; Ensure symbols are interned for definitions that could be skipped
  ;; when building without tree-sitter.
  (:intern :explicit-namespace-qualifiers
           :template-parameter-types
           :unqualified-name)
  (:shadow :condition)
  (:export :tree-sitter-ast
           :tree-sitter
           :*tree-sitter-language-directories*
           :*tree-sitter-language-files*
           :ast-type-to-rebind-p
           :ast-mixin-subclasses
           :ast-language-class
           :language-ast-class
           :parse-tree
           :copy-with-surrounding-text

           :matching-error
           :rule-matching-error
           :rule-matching-error-rule
           :rule-matching-error-ast
           :parse-tree-matching-error
           :parse-tree-matching-error-superclass
           :parse-tree-matching-error-parse-tree
           :parse-tree-matching-error-subclasses
           :parse-tree-matching-error-child-types
           :no-enclosing-declaration-error
           :no-enclosing-declaration-error.type
           :no-enclosing-declaration-error.root
           :no-enclosing-declaration-error.id
           :unresolved-overloads-error
           :unresolved-overloads-error.ast
           :unresolved-overloads-error.overloads
           :unqualifiable-ast-error
           :unqualifiable-ast-error.asts
           :unqualifiable-ast-error.error

           :javascript
           :python
           :source-text-fragment
           :before-text
           :after-text
           :before-asts
           :after-asts
           :computed-text
           :structured-text
           ;; Python
           :get-asts-in-namespace
           :get-vars
           :identical-name-p
           :in-class-def-p
           ;; C
           :c-source-text-fragment
           :c-variadic-declaration
           :c-canonical-type
           ;; Cpp
           :cpp-source-text-fragment
           :system-header-names
           :parse-header-synopsis
           :cpp-variadic-declaration
           :cpp-canonical-type
           :+cpp-multi-declaration-keys+
           :c/cpp-function-declaration-definitions
           :*morally-noexcept*
           ;; C/Cpp
           :c/cpp-canonical-type
           :specifier
           :declarator
           :bitfield
           :system-header-names
           ;; Cross-language Mix-ins
           :c/cpp
           :c-like-syntax
           :normal-scope
           :normal-scope-ast
           :ecma
           :typescript
           :ecma-ast
           :typescript-ast
           :c/cpp-ast
           :c-like-syntax-ast
           :root-ast
           :parse-error-ast
           :comment-ast
           :definition-ast
           :statement-ast
           :expression-ast
           :subexpression-ast
           :parenthesized-expression-ast
           :compound-ast
           :conditional-ast
           :control-flow-ast
           :if-ast
           :while-ast
           :for-ast
           :loop-ast
           :switch-ast
           :switch-expression-ast
           :switch-statement-ast
           :continue-ast
           :continuable-ast
           :break-ast
           :breakable-ast
           :throw-ast
           :throw-statement-ast
           :class-ast
           :scope-ast-p
           :ltr-eval-ast-p
           :function-ast
           :parameters-ast
           :declaration-ast
           :variable-declaration-ast
           :function-declaration-ast
           :type-declaration-ast
           :macro-declaration-ast
           :variable-initialization-ast
           :assignment-ast
           :identifier-ast
           :type-ast
           :lambda-ast
           :literal-ast
           :number-ast
           :float-ast
           :integer-ast
           :string-ast
           :char-ast
           :call-ast
           :arguments-ast
           :boolean-ast
           :boolean-true-ast
           :boolean-false-ast
           :unary-ast
           :binary-ast
           :jump-ast
           :break-ast
           :return-ast
           :goto-ast
           :terminal-symbol
           :canonical-type
           ;; Variation points
           :variation-point
           :error-variation-point
           :source-text-fragment-variation-point
           :*use-variation-point-tree*
           ;; Generics
           ;; TODO: should this be in parseable?
           :collect-fun-uses
           :collect-var-uses
           :collect-arg-uses
           :assignments
           :pointer-assignments
           :get-declaration-ast
           :get-declaration-asts
           :relevant-declaration-type
           :get-initialization-ast
           :get-declaration-id
           :get-declaration-ids
           :variable-declaration-ids
           :same-variable-p
           :same-place-p
           :variable-use-p
           :patch-whitespace
           :prettify-software
           :output-transformation
           :get-representative-ast
           ;; Cross-language Generics
           :constant-fold
           :direct-children
           :body
           :lhs
           :rhs
           :maybe-side-effect-p
           ;; Not currently exported, too many conflicts.
           ;; :condition
           :consequence
           :alternative
           :assignee
           :assignees
           :operator
           :argument
           :control-flow-condition
           :end-of-parameter-list
           :field-names
           :field-name-asts
           :function-name
           :identifiers
           :call-arguments
           :call-arguments-ast
           :function-parameters
           :parameter-type
           :parameter-name
           :parameter-names
           :function-body
           :call-name
           :call-function
           :variable-name
           :no-fallthrough
           :infer-type
           :infer-type-as-nil          ;Restart
           :infer-expression-type
           :expression-type
           :placeholder-type-p
           :boolean-type-p
           :resolve-declaration-type
           :declaration-type
           :find-enclosing
           :find-all-enclosing
           :find-outermost
           :find-preceding
           :find-previous-sibling
           :find-following
           :find-next-sibling
           :sort-descendants
           :comments-for
           :definition-name
           :definition-name-ast
           :declarator-name
           :declarator-name-ast
           :enclosing-definition
           :ast-imports
           :provided-by
           :comparisonp
           :contains-error-ast-p
           :aliasee
           :alias-set
           :entry-control-flow
           :exit-control-flow
           :subexpression-exit-control-flow
           :normal-exit-control-flow
           :exception-set
           :function-exception-set
           :template-specializations
           :possible-types
           :c-functions
           :c
           :cpp
           :c-path
           :json
           :json-ast
           :golang
           :bash
           :java
           :text-fragment
           :choice-superclass
           :tree-sitter-class-name
           :canonicalize-declarator
           :canonicalize-type
           ;; Styles
           :c-style-indentation
           ;; string-clauses.lisp
           :ast-for-match
           :parse-tolerant
           :wildcard?
           :ellipsis-match
           ;; Attributes
           :namespace
           :imports
           :attrs-root*
           ;; Symbol Table
           :symbol-table
           :find-in-symbol-table
           :find-enclosing-declaration
           :multi-map-symbol-table-union
           :symbol-table-union
           :multi-declaration-keys
           ;; template.lisp
           :ast-template
           :ast-template*
           :template-placeholder
           :template-metavariable
           :template-subtree
           :ast-from-template
           :ast-from-template*
           :*tree-sitter-mutation-types*
           :tree-sitter-replace
           :tree-sitter-swap
           :tree-sitter-insert
           :tree-sitter-move
           :tree-sitter-cut
           :tree-sitter-nop
           :parameter-ast
           :return-type
           :contextualize-ast
           ;; Blotting
           :*use-blotting*
           :blot-out-ranges
           :blot-out
           ;; C++ modules
           :module?
           :exported?
           :module-unit
           :module-unit-full-name
           :module-unit-module-name
           :module-unit-partition-name
           :module-unit-declaration
           :module-interface-unit
           :module-implementation-unit
           :implementation-unit
           :module-partition-unit
           :primary-module-interface-unit
           :anonymous-implementation-unit
           :module-partition-interface-unit
           :module-partition-implementation-unit
           ;; C/C++ analysis
           :public? :private? :protected?
           :skip-include
           :circular-inclusion
           :+exception-top-type+
           :+exception-bottom-type+
           :specified-noexcept?
           ;; Field tables
           :add-namespaced-field
           :adjoin-fields
           :class-fields
           :direct-field-table
           :field-adjoin
           :field-table
           :field-table-ids
           :field-table-lookup)
  (:local-nicknames
   (:dbg :software-evolution-library/utility/debug)
   #+sbcl (:md5 :sb-md5)
   #-sbcl (:md5 :md5)
   (:task :software-evolution-library/utility/task)))
(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

(eval-always
 (defvar *tree-sitter-packages*
   '(:sel/sw/ts))
 (defvar *tree-sitter-exports* ()
   "Preserve exports from tree-sitter across package redefinitions."))

(eval-always
 (defun export-tree-sitter* (syms)
   (synchronized ('*tree-sitter-exports*)
     (unionf *tree-sitter-exports* (ensure-list syms)))
   (dolist (package *tree-sitter-packages*)
     (import syms package)
     (export syms package))))

(defmacro export/tree-sitter (syms)
  `(eval-always
    (export-tree-sitter* ,syms)))

;;; Export all of the cached exports, hidden by package redefinition.
(eval-always
 (export-tree-sitter*
  *tree-sitter-exports*))

(define-software tree-sitter (software-indentation parseable) ()
  (:documentation "tree-sitter software representation."))

(defun finalize-tree-sitter-classes ()
  "Finalize all tree-sitter classes."
  (dolist (package *tree-sitter-packages*)
    (do-symbols (sym package)
      (when (eql (symbol-package sym) package)
        (when-let (class (find-class sym nil))
          (ensure-finalized class))))))

;;; There are a lot of tree-sitter classes. Finalizing the classes
;;; produces notable delays during initial parsing in an image
;;; including tree-sitter. Make sure the classes are finalized
;;; *before* the image is dumped.
(uiop:register-image-dump-hook 'finalize-tree-sitter-classes)
