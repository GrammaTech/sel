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
;;; For convience:
;;; - @url{https://github.com/tree-sitter/tree-sitter-c}
;;; - @url{https://github.com/tree-sitter/tree-sitter-java}
;;; - @url{https://github.com/tree-sitter/tree-sitter-javascript}
;;; - @url{https://github.com/tree-sitter/tree-sitter-json}
;;; - @url{https://github.com/tree-sitter/tree-sitter-python}
;;;
;;; To set-up a language, the following script can be used from the base
;;; directory of the language module's directory:
;;;
;;;     #!/bin/bash
;;;     language=$1
;;;
;;;     [ $# -eq 0 ] && { echo "Usage: $0 language_name"; exit 1; }
;;;
;;;     cd src/
;;;
;;;     if test -f "scanner.cc"; then
;;;         clang++ -fPIC scanner.cc -c -lstdc++
;;;         clang -std=c99 -fPIC parser.c -c
;;;         clang++ -shared scanner.o parser.o -o /usr/lib/tree-sitter-$(echo ${language}|sed 's|/|-|').so
;;;     elif test -f "scanner.c"; then
;;;         clang -std=c99 -fPIC scanner.c -c
;;;         clang -std=c99 -fPIC parser.c -c
;;;         clang -shared scanner.o parser.o -o /usr/lib/tree-sitter-$(echo ${language}|sed 's|/|-|').so
;;;     else
;;;         clang -std=c99 -fPIC parser.c -c
;;;         clang -shared parser.o -o /usr/lib/tree-sitter-$(echo ${language}|sed 's|/|-|').so
;;;     fi;
;;;
;;;     sudo mkdir -p /usr/share/tree-sitter/${language}/
;;;     sudo cp grammar.json node-types.json /usr/share/tree-sitter/${language}
;;;
;;; If everything is desired, the following can be used:
;;;
;;;     #!/bin/bash
;;;     for language in agda bash c c-sharp cpp css go html java javascript jsdoc json julia ocaml/ocaml ocaml/interface php python ql regex ruby rust scala typescript/tsx typescript/typescript;do
;;;         [ -d tree-sitter-${language%/*} ] || git clone --depth=1 https://github.com/tree-sitter/tree-sitter-${language%/*}
;;;         cd tree-sitter-${language}/src
;;;         if test -f "scanner.cc"; then
;;;             clang++ -fPIC scanner.cc -c -lstdc++
;;;             clang -std=c99 -fPIC parser.c -c
;;;             clang++ -shared scanner.o parser.o -o /usr/lib/tree-sitter-$(echo ${language}|sed 's|/|-|').so
;;;         elif test -f "scanner.c"; then
;;;             clang -std=c99 -fPIC scanner.c -c
;;;             clang -std=c99 -fPIC parser.c -c
;;;             clang -shared scanner.o parser.o -o /usr/lib/tree-sitter-$(echo ${language}|sed 's|/|-|').so
;;;         else
;;;             clang -std=c99 -fPIC parser.c -c
;;;             clang -shared parser.o -o /usr/lib/tree-sitter-$(echo ${language}|sed 's|/|-|').so
;;;         fi
;;;         mkdir -p /usr/share/tree-sitter/${language}/
;;;         cp grammar.json node-types.json /usr/share/tree-sitter/${language}
;;;         cd -
;;;     done
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
;;; - @url{https://github.com/death/cffi}
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
;;;                  is possible for this to be an instance of `conflict-ast'
;;;                  instead of a string.
;;;
;;; - after-text :: stores text that directly procedes the AST but is not part
;;;                 of the rule associated with the AST. This is generally
;;;                 whitespace. This slot is preferred when a terminal token
;;;                 directly follows the AST which does not have a before-text
;;;                 slot due to being implicit source text. It is also possible
;;;                 for this to be an instance of `conflict-ast' instead of a
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
;;; text can be identified by @code{computed-text-node-p}.
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
;;; The function that builds templates is called @code{ast-template}.
;;; You probably don't want to use this function directly; supported
;;; languages allow you to use a function with the same name as
;;; shorthand for creating a template:
;;;
;;;     (python "$ID = 1" :id "x")
;;;     ≡ (ast-template "$ID = 1" 'python-ast :id "x")
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
;;; distinguished in context. Or (at the time of writing) the C and
;;; C++ parsers for tree-sitter cannot correctly parse unterminated
;;; statements. Using @code{ast-from-template} lets you provide
;;; throwaway context to the parser while pulling out only the
;;; particular nodes that you want.
;;;
;;; @texi{tree-sitter}
(uiop:define-package :software-evolution-library/software/tree-sitter
    (:nicknames :sel/software/tree-sitter :sel/sw/tree-sitter
                :sel/software/ts :sel/sw/ts)
  (:use :gt/full
        :babel
        :cl-json
        :software-evolution-library
        :software-evolution-library/utility/json
        :software-evolution-library/utility/range
        :software-evolution-library/software/parseable
        :software-evolution-library/software/compilable
        :software-evolution-library/components/file
        :software-evolution-library/components/formatting)
  (:import-from :uiop)
  (:import-from :software-evolution-library/software/project
                :find-include-files :include-paths :include-paths-mixin)
  (:import-from :cffi :translate-camelcase-name :load-foreign-library-error)
  (:import-from :functional-trees :map-children)
  #.(if (asdf:find-system :cl-tree-sitter nil)
        '(:import-from :cl-tree-sitter :register-language)
        (values))
  #.(if (asdf:find-system :cl-tree-sitter nil)
        '(:shadowing-import-from :cl-tree-sitter :parse-string)
        (values))
  (:export :tree-sitter-ast
           :tree-sitter
           :*tree-sitter-language-directories*
           :*tree-sitter-language-files*
           :ast-type-to-rebind-p
           :ast-mixin-subclasses
           :collect-fun-uses
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
           :find-if-in-scopes
           :get-asts-in-namespace
           :get-vars
           :identical-name-p
           :in-class-def-p
           ;; C
           :c-source-text-fragment
           :c-variadic-declaration
           ;; Cpp
           :cpp-source-text-fragment
           :cpp-variadic-declaration
           ;; Cross-language Mix-ins
           :c/cpp
           :ecma
           :typescript
           :ecma-ast
           :typescript-ast
           :parse-error-ast
           :comment-ast
           :definition-ast
           :statement-ast
           :expression-ast
           :parenthesized-expression-ast
           :compound-ast
           :conditional-ast
           :control-flow-ast
           :if-ast
           :while-ast
           :loop-ast
           :class-ast
           :function-ast
           :parameters-ast
           :variable-declaration-ast
           :identifier-ast
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
           :return-ast
           :goto-ast
           :terminal-symbol
           ;; Generics
           ;; TODO: should this be in parseable?
           :collect-var-uses
           :variable-use-p
           :patch-whitespace
           :prettify-software
           :output-transformation
           ;; Cross-language Generics
           :direct-children
           :body
           :lhs
           :rhs
           :operator
           :control-flow-condition
           :end-of-parameter-list
           :field-name
           :function-name
           :call-arguments
           :function-parameters
           :parameter-type
           :parameter-name
           :parameter-names
           :function-body
           :call-name
           :call-function
           :variable-name
           :no-fallthrough
           :type-in
           :find-enclosing
           :find-preceding
           :find-following
           :comments-for
           :definition-name
           :declarator-name
           :enclosing-definition
           :imports
           :provided-by
           :comparisonp
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
           ;; Styles
           :c-style-indentation
           ;; string-clauses.lisp
           :ast-for-match
           :wildcard?
           :ellipsis-match
           ;; template.lisp
           :ast-template
           :template-placeholder
           :template-metavariable
           :template-subtree
           :ast-from-template
           :*tree-sitter-mutation-types*
           :tree-sitter-replace
           :tree-sitter-swap
           :tree-sitter-insert
           :tree-sitter-move
           :tree-sitter-cut
           :tree-sitter-nop
           :parameter-ast
           :return-type)
  (:local-nicknames
   #+sbcl (:md5 :sb-md5)
   #-sbcl (:md5 :md5)))
(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

(define-software tree-sitter (software-indentation parseable) ()
  (:documentation "tree-sitter software representation."))


;;; Shared object set-up
(eval-always
  ;; TODO: REMOVE THIS AFTER ALL DOWNSTREAM CIs HAVE BEEN UPDATED
  (unless (member :sel/structured-text *features*)
    (push :sel/structured-text *features*))

  (define-condition matching-error (error)
    ())

  (define-condition rule-matching-error (matching-error)
    ((rule :initarg :rule-matching-error-rule :initform nil
           :reader rule-matching-error-rule)
     (ast :initarg :rule-matching-error-ast :initform nil
          :reader rule-matching-error-ast))
    (:documentation "Error thrown when a rule fails to match on an AST.")
    (:report
     (lambda (condition stream)
       (format stream "Unable to match~%~a~%on AST of type ~%~a"
               (rule-matching-error-rule condition)
               (type-of (rule-matching-error-ast condition))))))

  (define-condition parse-tree-matching-error (matching-error)
    ((superclass :initarg :superclass :initform nil
                 :reader parse-tree-matching-error-superclass)
     (parse-tree :initarg :parse-tree :initform nil
                 :reader parse-tree-matching-error-parse-tree)
     (subclasses :initarg :subclasses :initform nil
                 :reader parse-tree-matching-error-subclasses))
    (:documentation "Error when a parse tree can't be matched.")
    (:report
     (lambda (condition stream)
       (with-slots (superclass parse-tree subclasses) condition
         (let ((*print-circle* nil))
           (format stream
                   "Unable to match tree to any subclass of ~s:~%~s~2%Candidates: ~s"
                   superclass parse-tree subclasses))))))

  (defstruct (parse-tree (:type list) (:copier nil))
    type range children)

  (defun copy-parse-tree (parse-tree
                          &key
                            (type (parse-tree-type parse-tree))
                            (range (parse-tree-range parse-tree))
                            (children (parse-tree-children parse-tree)))
    (make-parse-tree :type type :range range :children children))

  (defpattern parse-tree (type range children)
    `(list ,type ,range ,children))

  (defvar *superclass->language* (make-hash-table)
    "Maps an AST superclass to its tree-sitter language. When
convert is called, the superclass can then be used to look up
which language--and its relevant shared object--should be used
to parse the string.")

  (defvar *tree-sitter-language-directories*
    (or (when-let ((env (getenv "SEL_TREE_SITTER_LANGUAGE_DIR")))
          (split-sequence #\, env))
        (remove-if-not
         #'probe-file `("/usr/share/tree-sitter/"
                        "/usr/local/share/tree-sitter/"
                        ,@(when-let (home (getenv "HOME"))
                            (list (string+ home "/.local/share/tree-sitter/")))))
        (prog1 nil (format *error-output* "No tree-sitter language directory found.")))
    "A list of directories that hold directories of json files
defining supported tree-sitter languages.  These directories are
searched to populate `*tree-sitter-language-files*'.")

  (defun collect-tree-sitter-language-files
      (&optional (directories *tree-sitter-language-directories*) &aux results)
    "Collect tree-sitter language definition files."
    (dolist (dir directories results)
      (walk-directory
       dir
       (lambda (file)
         (let* ((base (pathname-directory-pathname file))
                (name (string-trim '(#\/) (pathname-relativize dir base))))
           (push (list name
                       (merge-pathnames "grammar.json" base)
                       (merge-pathnames "node-types.json" base))
                 results)))
       :test [{string= "node-types"} #'namestring #'pathname-name])))

  (defvar *tree-sitter-language-files* (collect-tree-sitter-language-files)
    "Files defining tree sitter languages.")


  (define-software c/cpp (include-paths-mixin)
    ()
    (:documentation "Mix-in class for tree-sitter c and cpp classes")
    (:default-initargs :include-paths nil))

  (define-software ecma ()
    ()
    (:documentation "Mix-in class for tree-sitter ECMAScript variants."))

  (define-software typescript (ecma)
    ()
    (:documentation "Mix-in class for tree-sitter TypeScript variants."))

  (defclass ecma-ast ()
    ()
    (:documentation "Mix-in class for tree-sitter ECMAScript ASTs."))

  (defclass typescript-ast (ecma-ast)
    ()
    (:documentation "Mix-in class for tree-sitter TypeScript ASTs."))

  (defparameter *tree-sitter-software-superclasses*
    '((:c compilable normal-scope c/cpp)
      (:cpp compilable normal-scope c/cpp)
      (:javascript normal-scope ecma)
      (:typescript-ts typescript)
      (:typescript-tsx typescript)))

  (defparameter *tree-sitter-software-direct-slots* '()
    "Alist of direct slots for software classes, such as `python' or
    `c'.")

  (defparameter *tree-sitter-base-ast-superclasses*
    '((:typescript-ts typescript-ast)
      (:typescript-tsx typescript-ast)
      (:javascript ecma-ast))
    "Alist of superclasses for the base class of a language (e.g.
    `python-ast').")

  (defparameter *tree-sitter-ast-extra-slots*
    '((:c
       (c-parameter-declaration
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (c-field-declaration
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (c-declaration
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (c-function-definition
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (c-sized-type-specifier (:modifiers (:multiple . t)))
       (c-type-descriptor
        (:pre-type-qualifiers (:multiple . t))
        (:post-type-qualifiers (:multiple . t)))
       (c-case-statement
        (:statements (:multiple . t)))
       (c-labeled-statement (:statement))
       (c-for-statement (:body)))
      (:cpp
       (cpp-parameter-declaration
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (cpp-field-declaration
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (cpp-declaration
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (cpp-function-definition
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (cpp-for-range-loop
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (cpp-operator-cast
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (cpp-operator-name (:name))
       (cpp-template-instantiation
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (cpp-optional-parameter-declaration
        (:pre-specifiers (:multiple . t))
        (:post-specifiers (:multiple . t)))
       (cpp-type-descriptor
        (:pre-type-qualifiers (:multiple . t))
        (:post-type-qualifiers (:multiple . t)))
       (cpp-case-statement
        (:statements (:multiple . t)))
       (cpp-labeled-statement (:statement))
       (cpp-for-statement (:body))
       (cpp-sized-type-specifier (:modifiers (:multiple . t)))
       (cpp-access-specifier (:keyword))
       (cpp-type-parameter-declaration (:keyword))
       (cpp-reference-declarator (:valueness)))
      (:python
       (python-function-definition (:async))
       (python-for-statement (:async))
       (python-with-statement (:async)))
      (:javascript
       (javascript-function-declaration (:async))
       (javascript-export-statement (:default)))
      (:typescript-ts
       (typescript-ts-export-statement (:default))
       (typescript-ts-method-definition (:getter-setter) (:optional) (:async))
       (typescript-ts-public-field-definition (:declare) (:modifiers) (:optional))
       (typescript-ts-method-signature (:optional))
       (typescript-ts-abstract-method-signature (:optional))
       (typescript-ts-property-signature (:optional))
       (typescript-ts-member-expression (:operator))
       (typescript-ts-subscript-expression (:operator))
       (typescript-ts-call-expression (:operator))
       (typescript-ts-arrow-function (:async))
       (typescript-ts-function (:async))
       (typescript-ts-function-declaration (:async))
       (typescript-ts-function-signature (:async))
       (typescript-ts-generator-function-declaration (:async))
       (typescript-ts-enum-declaration (:kind))
       (typescript-ts-required-parameter (:modifiers (:multiple . t)))
       (typescript-ts-optional-parameter (:modifiers (:multiple . t))))
      (:typescript-tsx
       (typescript-tsx-export-statement (:default))
       (typescript-tsx-method-definition (:getter-setter) (:optional) (:async))
       (typescript-tsx-public-field-definition (:declare) (:modifiers) (:optional))
       (typescript-tsx-method-signature (:getter-setter) (:optional))
       (typescript-tsx-abstract-method-signature (:optional))
       (typescript-tsx-property-signature (:optional))
       (typescript-tsx-member-expression (:operator))
       (typescript-tsx-subscript-expression (:operator))
       (typescript-tsx-call-expression (:operator))
       (typescript-tsx-arrow-function (:async))
       (typescript-tsx-function (:async))
       (typescript-tsx-function-declaration (:async))
       (typescript-tsx-function-signature (:async))
       (typescript-tsx-generator-function-declaration (:async))
       (typescript-tsx-enum-declaration (:kind))
       (typescript-tsx-required-parameter (:modifiers (:multiple . t)))
       (typescript-tsx-optional-parameter (:modifiers (:multiple . t)))))
    "Alist from languages to classes with extra slots.
The form should be the same as the fields in the note-types.json
for the language.")

  (defparameter *tree-sitter-ast-extra-slot-options*
    '((:c
       (c-init-declarator
        (c-declarator :initarg :lhs :reader lhs)
        (c-value :initarg :rhs :reader rhs))
       (c-assignment-expression
        (c-left :initarg :lhs :reader lhs)
        (c-right :initarg :rhs :reader rhs))
       (c-call-expression
        (c-function :reader call-function)
        (c-arguments :reader call-arguments))
       (c-while-statement
        (c-body :reader body :initarg :body))
       (c-do-statement
        (c-body :reader body :initarg :body))
       (c-for-statement
        (c-body :reader body :initarg :body))
       (c-if-statement
        (c-consequence :initarg :consequence :reader consequence)
        (c-alternative :initarg :alternative :reader alternative)))
      (:cpp
       (cpp-init-declarator
        (cpp-declarator :initarg :lhs :reader lhs)
        (cpp-value :initarg :rhs :reader rhs))
       (cpp-assignment-expression
        (cpp-left :initarg :lhs :reader lhs)
        (cpp-right :initarg :rhs :reader rhs))
       (cpp-call-expression
        (cpp-function :reader call-function)
        (cpp-arguments :reader call-arguments))
       (cpp-while-statement
        (cpp-body :reader body :initarg :body))
       (cpp-do-statement
        (cpp-body :reader body :initarg :body))
       (cpp-for-statement
        (cpp-body :reader body :initarg :body))
       (cpp-for-range-loop
        (cpp-body :reader body :initarg :body))
       (cpp-if-statement
        (cpp-consequence :initarg :consequence :reader consequence)
        (cpp-alternative :initarg :alternative :reader alternative)))
      (:javascript
       (javascript-switch-case
        (javascript-body :reader body)))
      (:python
       (python-call
        (python-function :reader call-function)
        (python-arguments :reader call-arguments))
       (python-assignment
        (python-left :initarg :lhs :reader lhs)
        (python-right :initarg :rhs :reader rhs))
       (python-augmented-assignment
        (python-left :initarg :lhs :reader lhs)
        (python-right :initarg :rhs :reader rhs))
       (python-binary-operator
        (python-left :initarg :lhs :reader lhs)
        (python-right :initarg :rhs :reader rhs)
        (python-operator :initarg :operator :reader operator))
       (python-boolean-operator
        (python-left :initarg :lhs :reader lhs)
        (python-right :initarg :rhs :reader rhs)
        (python-operator :initarg :operator :reader operator))
       (python-function-definition
        (python-body :reader body)
        (python-return-type :reader return-type))
       (python-keyword-argument
        (python-name :initarg :lhs :reader lhs)
        (python-value :initarg :rhs :reader rhs))
       (python-unary-operator
        (python-operator :initarg :operator :reader operator))
       (python-while-statement
        (python-body :reader body)))
      ((:typescript-ts :typescript-tsx)
       ;; Anonymous function (function keyword).
       (typescript-ts-function
        (typescript-ts-return-type
         :reader return-type))
       (typescript-tsx-function
        (typescript-tsx-return-type
         :reader return-type))
       ;; Function declaration.
       (typescript-ts-function-declaration
        (typescript-ts-return-type
         :reader return-type)
        (typescript-ts-name
         :reader typescript-name))
       (typescript-tsx-function-declaration
        (typescript-tsx-return-type
         :reader return-type)
        (typescript-tsx-name
         :reader typescript-name))
       ;; Function signature (overload).
       (typescript-ts-function-signature
        (typescript-ts-return-type
         :reader return-type))
       (typescript-ts-function-signature
        (typescript-ts-return-type
         :reader return-type))
       ;; Arrow function.
       (typescript-ts-arrow-function
        (typescript-ts-return-type
         :reader return-type))
       (typescript-tsx-arrow-function
        (typescript-tsx-return-type
         :reader return-type))
       ;; Optional parameter.
       (typescript-ts-optional-parameter
        (typescript-ts-type
         :reader parameter-type))
       (typescript-tsx-optional-parameter
        (typescript-tsx-type
         :reader parameter-type))
       ;; Required parameter.
       (typescript-ts-required-parameter
        (typescript-ts-type
         :reader parameter-type))
       (typescript-tsx-required-parameter
        (typescript-tsx-type
         :reader parameter-type))))
    "Alist from languages to classes with extra slot options.")

  (defparameter *tree-sitter-ast-superclasses*
    '((:c
       (:root-ast c-translation-unit)
       (:comment-ast c-comment)
       (:definition-ast c-type-definition c-struct-specifier c-union-specifier
        c-field-declaration c-enum-specifier c-preproc-def
        c-preproc-function-def)
       (:statement-ast c--statement c-function-definition c-declaration)
       (:expression-statement-ast c-expression-statement)
       (:expression-ast c--expression)
       (:parenthesized-expression-ast c-parenthesized-expression)
       (:compound-ast c-compound-statement)
       (:control-flow-ast c-switch-statement c-case-statement)
       (:if-ast c-if-statement)
       (:while-ast c-while-statement)
       (:loop-ast c-while-statement c-for-statement c-do-statement)
       (:parse-error-ast c-error)
       (:variable-declaration-ast c-init-declarator c-assignment-expression)
       (:function-ast c-function-definition)
       (:identifier-ast c-identifier)
       (:field-ast c-field-expression)
       (:subscript-ast c-subscript-expression)
       (:string-ast c-string-literal)
       (:number-ast c-number-literal)
       (:call-ast c-call-expression)
       (:boolean-true-ast c-true)
       (:boolean-false-ast c-false)
       (:unary-ast c-unary-expression)
       (:binary-ast c-binary-expression)
       (:return-ast c-return-statement)
       (:goto-ast c-goto-statement)
       (:c/cpp-+ c-+)
       (:c/cpp-- c--)
       (:c/cpp-* c-*)
       (:c/cpp-/ c-/)
       (:c/cpp-! c-!)
       (:c/cpp-&& c-&&)
       (:c/cpp-\|\| c-\|\|)
       (:c/cpp-& c-&)
       (:c/cpp-\| c-\|)
       (:c/cpp-<< c-<<)
       (:c/cpp->> c->>)
       (:c/cpp-~ c-~)
       (:c/cpp-^ c-^)
       (:c/cpp-% c-%)
       (:c/cpp-== c-==)
       (:c/cpp-<= c-<=)
       (:c/cpp->= c->=)
       (:c/cpp-!= c-!=)
       (:c/cpp-< c-<)
       (:c/cpp-> c->)
       (:c/cpp-++ c-++)
       (:c/cpp--- c---)
       (:c/cpp-argument-list c-argument-list)
       (:c/cpp-assignment-expression c-assignment-expression)
       (:c/cpp-array-declarator c-array-declarator)
       (:c/cpp-binary-expression c-binary-expression)
       (:c/cpp-comment c-comment)
       (:c/cpp-error c-error)
       (:c/cpp-expression-statement c-expression-statement)
       (:c/cpp-break-statement c-break-statement)
       (:c/cpp-call-expression c-call-expression)
       (:c/cpp-case-statement c-case-statement)
       (:c/cpp-char-literal c-char-literal)
       (:c/cpp-compound-statement c-compound-statement)
       (:c/cpp-condition-clause c-condition-clause)
       (:c/cpp-continue-statement c-continue-statement)
       (:c/cpp-declaration c-declaration)
       (:c/cpp-declarator c-declarator)
       (:c/cpp-do-statement c-do-statement)
       (:c/cpp-enum-specifier c-enum-specifier)
       (:c/cpp-enumerator c-enumerator)
       (:c/cpp-field-declaration c-field-declaration)
       (:c/cpp-field-identifier c-field-identifier)
       (:c/cpp-field-expression c-field-expression)
       (:c/cpp-for-statement c-for-statement)
       (:c/cpp-function-declarator c-function-declarator)
       (:c/cpp-function-definition c-function-definition)
       (:c/cpp-identifier c-identifier)
       (:c/cpp-if-statement c-if-statement)
       (:c/cpp-initializer-pair c-initializer-pair)
       (:c/cpp-init-declarator c-init-declarator)
       (:c/cpp-number-literal c-number-literal)
       (:c/cpp-parameter-declaration c-parameter-declaration)
       (:c/cpp-parenthesized-declarator c-parenthesized-declarator)
       (:c/cpp-parenthesized-expression c-parenthesized-expression)
       (:c/cpp-pointer-declarator c-pointer-declarator)
       (:c/cpp-pointer-expression c-pointer-expression)
       (:c/cpp-preproc-arg c-preproc-arg)
       (:c/cpp-preproc-def c-preproc-def)
       (:c/cpp-preproc-elif c-preproc-elif)
       (:c/cpp-preproc-else c-preproc-else)
       (:c/cpp-preproc-function-def c-preproc-function-def)
       (:c/cpp-preproc-include c-preproc-include)
       (:c/cpp-preproc-params c-preproc-params)
       (:c/cpp-primitive-type c-primitive-type)
       (:c/cpp-return-statement c-return-statement)
       (:c/cpp-string-literal c-string-literal)
       (:c/cpp-struct-specifier c-struct-specifier)
       (:c/cpp-subscript-expression c-subscript-expression)
       (:c/cpp-switch-statement c-switch-statement)
       (:c/cpp-type-definition c-type-definition)
       (:c/cpp-type-identifier c-type-definition)
       (:c/cpp-unary-expression c-unary-expression)
       (:c/cpp-union-specifier c-union-specifier)
       (:c/cpp-update-expression c-update-expression)
       (:c/cpp-while-statement c-while-statement))
      (:cpp
       (:root-ast cpp-translation-unit)
       (:comment-ast cpp-comment)
       (:definition-ast cpp-type-definition cpp-struct-specifier
        cpp-union-specifier
        cpp-field-declaration
        cpp-enum-specifier cpp-preproc-def
        cpp-preproc-function-def)
       (:parenthesized-expression-ast cpp-parenthesized-expression)
       (:statement-ast cpp--statement cpp-function-definition cpp-declaration)
       (:expression-statement-ast cpp-expression-statement)
       (:compound-ast cpp-compound-statement)
       (:string-ast cpp-string-literal cpp-raw-string-literal)
       (:char-ast cpp-char-literal)
       (:call-ast cpp-call-expression)
       (:boolean-true-ast cpp-true)
       (:boolean-false-ast cpp-false)
       (:variable-declaration-ast cpp-assignment-expression cpp-init-declarator)
       (:function-ast cpp-function-definition)
       (:unary-ast cpp-unary-expression)
       (:binary-ast cpp-binary-expression)
       (:number-ast cpp-number-literal)
       (:return-ast cpp-return-statement)
       (:goto-ast cpp-goto-statement)
       (:identifier-ast cpp-identifier cpp-namespace-identifier)
       (:field-ast cpp-field-expression)
       (:subscript-ast cpp-subscript-expression)
       (:catch-ast cpp-catch-clause)
       (:loop-ast
        cpp-while-statement
        cpp-for-statement
        cpp-for-range-loop
        cpp-do-statement)
       (:c/cpp-+ cpp-+)
       (:c/cpp-- cpp--)
       (:c/cpp-* cpp-*)
       (:c/cpp-/ cpp-/)
       (:c/cpp-! cpp-!)
       (:c/cpp-&& cpp-&&)
       (:c/cpp-\|\| cpp-\|\|)
       (:c/cpp-& cpp-&)
       (:c/cpp-\| cpp-\|)
       (:c/cpp-<< cpp-<<)
       (:c/cpp->> cpp->>)
       (:c/cpp-~ cpp-~)
       (:c/cpp-^ cpp-^)
       (:c/cpp-% cpp-%)
       (:c/cpp-== cpp-==)
       (:c/cpp-<= cpp-<=)
       (:c/cpp->= cpp->=)
       (:c/cpp-!= cpp-!=)
       (:c/cpp-< cpp-<)
       (:c/cpp-> cpp->)
       (:c/cpp-++ cpp-++)
       (:c/cpp--- cpp---)
       (:c/cpp-argument-list cpp-argument-list)
       (:c/cpp-assignment-expression cpp-assignment-expression)
       (:c/cpp-array-declarator cpp-array-declarator)
       (:c/cpp-binary-expression cpp-binary-expression)
       (:c/cpp-break-statement cpp-break-statement)
       (:c/cpp-call-expression cpp-call-expression)
       (:c/cpp-case-statement cpp-case-statement)
       (:c/cpp-char-literal cpp-char-literal)
       (:c/cpp-comment cpp-comment)
       (:c/cpp-compound-statement cpp-compound-statement)
       (:c/cpp-condition-clause cpp-condition-clause)
       (:c/cpp-continue-statement cpp-continue-statement)
       (:c/cpp-declaration cpp-declaration)
       (:c/cpp-declarator cpp-declarator)
       (:c/cpp-do-statement cpp-do-statement)
       (:c/cpp-enum-specifier cpp-enum-specifier)
       (:c/cpp-enumerator cpp-enumerator)
       (:c/cpp-error cpp-error)
       (:c/cpp-expression-statement cpp-expression-statement)
       (:c/cpp-field-declaration cpp-field-declaration)
       (:c/cpp-field-identifier cpp-field-identifier)
       (:c/cpp-field-expression cpp-field-expression)
       (:c/cpp-for-statement cpp-for-statement)
       (:c/cpp-function-declarator cpp-function-declarator)
       (:c/cpp-function-definition cpp-function-definition)
       (:c/cpp-identifier cpp-identifier)
       (:c/cpp-if-statement cpp-if-statement)
       (:c/cpp-initializer-pair cpp-initializer-pair)
       (:c/cpp-init-declarator cpp-init-declarator)
       (:c/cpp-number-literal cpp-number-literal)
       (:c/cpp-parameter-declaration cpp-parameter-declaration)
       (:c/cpp-parenthesized-declarator cpp-parenthesized-declarator)
       (:c/cpp-parenthesized-expression cpp-parenthesized-expression)
       (:c/cpp-pointer-declarator cpp-pointer-declarator)
       (:c/cpp-pointer-expression cpp-pointer-expression)
       (:c/cpp-preproc-arg cpp-preproc-arg)
       (:c/cpp-preproc-def cpp-preproc-def)
       (:c/cpp-preproc-elif cpp-preproc-elif)
       (:c/cpp-preproc-else cpp-preproc-else)
       (:c/cpp-preproc-function-def cpp-preproc-function-def)
       (:c/cpp-preproc-include cpp-preproc-include)
       (:c/cpp-preproc-params cpp-preproc-params)
       (:c/cpp-primitive-type cpp-primitive-type)
       (:c/cpp-return-statement cpp-return-statement)
       (:c/cpp-string-literal cpp-string-literal)
       (:c/cpp-struct-specifier cpp-struct-specifier)
       (:c/cpp-subscript-expression cpp-subscript-expression)
       (:c/cpp-switch-statement cpp-switch-statement)
       (:c/cpp-type-definition cpp-type-definition)
       (:c/cpp-type-identifier cpp-type-identifier)
       (:c/cpp-unary-expression cpp-unary-expression)
       (:c/cpp-union-specifier cpp-union-specifier)
       (:c/cpp-update-expression cpp-update-expression)
       (:c/cpp-while-statement cpp-while-statement))
      (:golang
       (:comment-ast golang-comment))
      (:java
       (:statement-ast java-statement)
       (:return-ast java-return-statement))
      (:javascript
       (:root-ast javascript-program)
       (:comment-ast javascript-comment)
       (:class-ast javascript-class-declaration)
       (:control-flow-ast
        javascript-switch-statement javascript-try-statement)
       (:if-ast javascript-if-statement)
       (:while-ast javascript-while-statement)
       (:expression-ast javascript--expression)
       (:parenthesized-expression-ast javascript-parenthesized-expression)
       (:compound-ast javascript-statement-block)
       (:boolean-true-ast javascript-true)
       (:boolean-false-ast javascript-false)
       (:function-ast
        javascript-function javascript-function-declaration
        javascript-arrow-function)
       (:lambda-ast javascript-function javascript-arrow-function)
       (:parameters-ast javascript-formal-parameters)
       (:variable-declaration-ast javascript-variable-declaration-ast)
       (:identifier-ast
        javascript-identifier javascript-property-identifier
        javascript-shorthand-property-identifier
        javascript-shorthand-property-identifier-pattern)
       (:field-ast javascript-member-expression)
       (:subscript-ast javascript-subscript-expression)
       (:float-ast javascript-number)
       (:string-ast javascript-string)
       (:loop-ast
        javascript-for-statement javascript-do-statement
        javascript-while-statement)
       (:statement-ast javascript--statement javascript-statement)
       (:expression-statement-ast javascript-expression-statement)
       (:call-ast javascript-call-expression)
       (:arguments-ast javascript-arguments)
       (:unary-ast javascript-unary-expression)
       (:binary-ast javascript-binary-expression)
       (:return-ast javascript-return-statement)
       (:catch-ast javascript-catch-clause))
      ((:javascript :typescript-ts :typescript-tsx)
       (:ecma-comment
        javascript-comment
        typescript-ts-comment
        typescript-tsx-comment)
       (:ecma-error
        javascript-error
        typescript-ts-error
        typescript-tsx-error)
       (:ecma-variable-declarator
        javascript-variable-declarator
        typescript-ts-variable-declarator
        typescript-tsx-variable-declarator)
       (:ecma-assignment-expression
        javascript-assignment-expression
        typescript-ts-assignment-expression
        typescript-tsx-assignment-expression)
       (:ecma-rest-pattern
        javascript-rest-pattern
        typescript-ts-rest-pattern
        typescript-tsx-rest-pattern)
       (:ecma-assignment-pattern
        javascript-assignment-pattern
        typescript-ts-assignment-pattern
        typescript-tsx-assignment-pattern)
       (:ecma-call-expression
        javascript-call-expression
        typescript-ts-call-expression
        typescript-tsx-call-expression)
       (:ecma-arguments
        javascript-arguments
        typescript-ts-arguments
        typescript-tsx-arguments)
       (:ecma-member-expression
        javascript-member-expression
        typescript-ts-member-expression
        typescript-tsx-member-expression))
      (:python
       (:root-ast python-module)
       (:comment-ast python-comment)
       (:class-ast python-class-definition)
       (:control-flow-ast
        python-try-statement python-conditional-expression
        python-list-comprehension python-set-comprehension
        python-generator-expression python-dictionary-comprehension)
       (:if-ast python-if-statement)
       (:while-ast python-while-statement)
       (:expression-ast python-expression)
       (:parenthesized-expression-ast python-parenthesized-expression)
       (:function-ast
        python-function-definition python-lambda)
       (:parameters-ast python-parameters python-lambda-parameters)
       (:boolean-true-ast python-true)
       (:boolean-false-ast python-false)
       (:identifier-ast python-identifier)
       (:field-ast python-attribute)
       (:subscript-ast python-subscript)
       (:lambda-ast python-lambda)
       (:integer-ast python-integer)
       (:float-ast python-float)
       (:string-ast python-string)
       (:loop-ast
        python-while-statement python-for-statement python-for-in-clause)
       (:statement-ast python--compound-statement python--simple-statement)
       (:expression-statement-ast python-expression-statement)
       (:compound-ast python-block)
       (:call-ast python-call)
       (:arguments-ast python-argument-list)
       (:unary-ast python-unary-operator python-not-operator)
       (:binary-ast python-binary-operator python-boolean-operator)
       (:return-ast python-return-statement)
       (:variable-declaration-ast python-assignment python-keyword-argument)
       (:catch-ast python-except-clause))
      ((:typescript-ts :typescript-tsx)
       (:root-ast
        typescript-ts-program
        typescript-tsx-program)
       (:class-ast
        typescript-ts-class-declaration
        typescript-tsx-class-declaration)
       (:boolean-true-ast
        typescript-ts-true
        typescript-tsx-true)
       (:boolean-false-ast
        typescript-ts-false
        typescript-tsx-false)
       (:identifier-ast
        typescript-ts-identifier
        typescript-tsx-identifier
        typescript-ts-property-identifier
        typescript-tsx-property-identifier
        typescript-ts-shorthand-property-identifier
        typescript-tsx-shorthand-property-identifier
        typescript-ts-shorthand-property-identifier-pattern
        typescript-tsx-shorthand-property-identifier-pattern)
       (:call-ast
        typescript-ts-call-expression
        typescript-tsx-call-expression)
       (:control-flow-ast
        typescript-ts-switch-statement
        typescript-tsx-switch-statement
        typescript-ts-while-statement
        typescript-tsx-while-statement)
       (:function-ast
        typescript-ts-function-declaration
        typescript-tsx-function-declaration
        typescript-ts-function-signature
        typescript-tsx-function-signature
        typescript-ts-function
        typescript-tsx-function
        typescript-ts-arrow-function
        typescript-tsx-arrow-function)
       (:lambda-ast
        typescript-ts-function
        typescript-tsx-function
        typescript-ts-arrow-function
        typescript-tsx-arrow-function)
       (:parameter-ast
        typescript-ts-required-parameter
        typescript-tsx-required-parameter
        typescript-ts-optional-parameter
        typescript-tsx-optional-parameter)
       (:typescript-program
        typescript-ts-program
        typescript-tsx-program)
       (:typescript-type-annotation
        typescript-ts-type-annotation
        typescript-tsx-type-annotation)
       (:typescript-parameter
        typescript-ts-required-parameter
        typescript-tsx-required-parameter
        typescript-ts-optional-parameter
        typescript-tsx-optional-parameter)
       (:typescript-required-parameter
        typescript-ts-required-parameter
        typescript-tsx-required-parameter)
       (:typescript-optional-parameter
        typescript-ts-optional-parameter
        typescript-tsx-optional-parameter)
       (:typescript-rest-pattern
        typescript-ts-rest-pattern
        typescript-tsx-rest-pattern)
       (:typescript-assignment-pattern
        typescript-ts-assignment-pattern
        typescript-tsx-assignment-pattern)
       (:typescript-function-declaration
        typescript-ts-function-declaration
        typescript-tsx-function-declaration)
       (:typescript-function-signature
        typescript-ts-function-signature
        typescript-tsx-function-signature)
       (:typescript-variable-declarator
        typescript-ts-variable-declarator
        typescript-tsx-variable-declarator)
       (:typescript-assignment-expression
        typescript-ts-assignment-expression
        typescript-tsx-assignment-expression)
       (:typescript-identifier
        typescript-ts-identifier
        typescript-tsx-identifier)
       (:typescript-tuple-type
        typescript-ts-tuple-type
        typescript-tsx-tuple-type)
       (:typescript-array-type
        typescript-ts-array-type
        typescript-tsx-array-type)
       (:typescript-intersection-type
        typescript-ts-intersection-type
        typescript-tsx-intersection-type)
       (:typescript-union-type
        typescript-ts-union-type
        typescript-tsx-union-type)
       (:typescript-type-identifier
        typescript-ts-type-identifier
        typescript-tsx-type-identifier)
       (:typescript-generic-type
        typescript-ts-generic-type
        typescript-tsx-generic-type)
       (:typescript-object-type
        typescript-ts-object-type
        typescript-tsx-object-type)
       (:typescript-index-signature
        typescript-ts-index-signature
        typescript-tsx-index-signature)
       (:typescript-property-signature
        typescript-ts-property-signature
        typescript-tsx-property-signature)
       (:typescript-property-identifier
        typescript-ts-property-identifier
        typescript-tsx-property-identifier)
       (:typescript-flow-maybe-type
        typescript-ts-flow-maybe-type
        typescript-tsx-flow-maybe-type)
       (:typescript-literal-type
        typescript-ts-literal-type
        typescript-tsx-literal-type)
       (:typescript-type-arguments
        typescript-ts-type-arguments
        typescript-tsx-type-arguments)
       (:typescript-type-argument
        typescript-ts-type-argument
        typescript-tsx-type-argument)
       (:typescript-predefined-type
        typescript-ts-predefined-type
        typescript-tsx-predefined-type)
       (:return-ast
        typescript-ts-return-statement
        typescript-tsx-return-statement)))
    "Specifies which classes should inherit from which mixins.
An alist from a language (or list of languages) to an alist of mixins
and tree-sitter AST classes that should inherit from them.

A language may appear multiple times; in this case all the mixins for that language apply.

Note that mixins used here will be automatically exported later, and
those that do not have separate class definitions will be given stub
definitions.")

  ;; TODO: it may make sense to have a way to 'rebind' a subclass when
  ;;       the subclass no longer applies after a mutation.
  (defparameter *tree-sitter-choice-expansion-subclasses*
    '((:c
       ;; TODO: this should be moved over to using the pruned-rule before
       ;;       merging into master.
       (c-update-expression
        (c-update-expression-prefix
         (:seq (:field c-operator c--- c-++) (:field c-argument c--expression)))
        (c-update-expression-postfix
         (:seq (:field c-argument c--expression) (:field c-operator c--- c-++))))
       (c-expression-statement
        (c-expression-statement-
         (:seq (:child c--expression c-comma-expression)))
        (c-empty-expression-statement
         (:seq))))
      (:python
       (python-tuple
        (python-empty-tuple
         (:seq (:slot python-internal-asts-0))))
       (python-dictionary
        (python-empty-dictionary
         (:seq (:slot python-internal-asts-0))))
       (python-parameters
        (python-empty-parameters
         (:seq (:slot python-internal-asts-0))))
       (python-argument-list
        (python-empty-argument-list
         (:seq (:slot python-internal-asts-1)))))))

  (defparameter *tree-sitter-json-rule-substitutions*
    '(((:c :cpp)
       ;; These are common to C and C++.
       ;; NOTE: remove this if this is patched upstream.
       (:PREPROC-PARAMS (:TYPE . "SEQ")
        ;; Adds variadic declaration nodes in place of literal ellipses.
        (:MEMBERS
         ((:TYPE . "IMMEDIATE_TOKEN")
          (:CONTENT (:TYPE . "STRING") (:VALUE . "(")))
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "SEQ")
            (:MEMBERS
             ((:TYPE . "CHOICE")
              (:MEMBERS
               ((:TYPE . "SYMBOL") (:NAME . "identifier"))
               ((:TYPE . "SYMBOL") (:NAME . "variadic_declaration"))))
             ((:TYPE . "REPEAT")
              (:CONTENT
               (:TYPE . "SEQ")
               (:MEMBERS
                ((:TYPE . "STRING") (:VALUE . ","))
                ((:TYPE . "CHOICE")
                 (:MEMBERS
                  ((:TYPE . "SYMBOL") (:NAME . "identifier"))
                  ((:TYPE . "SYMBOL")
                   (:NAME . "variadic_declaration")))))))))
           ((:TYPE . "BLANK"))))
         ((:TYPE . "STRING") (:VALUE . ")"))))
       (:SIZED-TYPE-SPECIFIER (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "REPEAT1")
          (:CONTENT
           (:TYPE . "FIELD") (:NAME . "modifiers")
           (:CONTENT
            (:TYPE . "CHOICE")
            (:MEMBERS
             ((:TYPE . "STRING") (:VALUE . "signed"))
             ((:TYPE . "STRING") (:VALUE . "unsigned"))
             ((:TYPE . "STRING") (:VALUE . "long"))
             ((:TYPE . "STRING") (:VALUE . "short"))))))
         ((:TYPE . "FIELD") (:NAME . "type")
          (:CONTENT
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "CHOICE")
             (:MEMBERS
              ((:TYPE . "PREC_DYNAMIC")
               (:VALUE . -1)
               (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_type_identifier")))
              ((:TYPE . "SYMBOL") (:NAME . "primitive_type"))))
            ((:TYPE . "BLANK")))))))
       (:TYPE-DESCRIPTOR
        (:TYPE . "SEQ")
        (:MEMBERS
         ;; Wrap a field around the type qualifier.
         ((:TYPE . "REPEAT")
          (:CONTENT
           (:TYPE . "FIELD") (:NAME . "pre_type_qualifiers")
           (:CONTENT
            (:TYPE . "SYMBOL") (:NAME . "type_qualifier"))))
         ;; Wrap a field around the type specifier.
         ((:TYPE . "FIELD")
          (:NAME . "type")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_type_specifier")))
         ((:TYPE . "REPEAT")
          (:CONTENT
           ;; Wrap a field around the post type qualifiers.
           (:TYPE . "FIELD") (:NAME . "post_type_qualifiers")
           (:CONTENT
            (:TYPE . "SYMBOL") (:NAME . "type_qualifier"))))
         ((:TYPE . "FIELD")
          (:NAME . "declarator")
          (:CONTENT
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "SYMBOL") (:NAME . "_abstract_declarator"))
            ((:TYPE . "BLANK")))))))
       (:CASE-STATEMENT
        (:TYPE . "PREC_RIGHT")
        (:VALUE . 0)
        (:CONTENT
         (:TYPE . "SEQ")
         (:MEMBERS
          ((:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "SEQ")
             (:MEMBERS
              ((:TYPE . "STRING") (:VALUE . "case"))
              ((:TYPE . "FIELD")
               (:NAME . "value")
               (:CONTENT
                (:TYPE . "SYMBOL")
                (:NAME . "_expression")))))
            ((:TYPE . "STRING") (:VALUE . "default"))))
          ((:TYPE . "STRING") (:VALUE . ":"))
          ((:TYPE . "FIELD")
           (:NAME . "STATEMENTS")
           (:CONTENT
            (:TYPE . "REPEAT")
            (:CONTENT
             (:TYPE . "CHOICE")
             (:MEMBERS
              ((:TYPE . "ALIAS")
               (:CONTENT
                (:TYPE . "SYMBOL")
                (:NAME . "attributed_non_case_statement"))
               (:NAMED . T)
               (:VALUE . "attributed_statement"))
              ((:TYPE . "SYMBOL") (:NAME . "_non_case_statement"))
              ((:TYPE . "SYMBOL") (:NAME . "declaration"))
              ((:TYPE . "SYMBOL") (:NAME . "type_definition")))))))))
       (:LABELED-STATEMENT
        (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "FIELD")
          (:NAME . "label")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_statement_identifier")))
         ((:TYPE . "STRING") (:VALUE . ":"))
         ((:TYPE . "FIELD")
          (:NAME . "statement")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_statement")))))
       (:FOR-STATEMENT
        (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "STRING") (:VALUE . "for"))
         ((:TYPE . "STRING") (:VALUE . "("))
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "FIELD")
            (:NAME . "initializer")
            (:CONTENT
             (:TYPE . "SYMBOL")
             (:NAME . "declaration")))
           ((:TYPE . "SEQ")
            (:MEMBERS
             ((:TYPE . "FIELD")
              (:NAME . "initializer")
              (:CONTENT
               (:TYPE . "CHOICE")
               (:MEMBERS
                ((:TYPE . "CHOICE")
                 (:MEMBERS
                  ((:TYPE . "SYMBOL") (:NAME . "_expression"))
                  ((:TYPE . "SYMBOL") (:NAME . "comma_expression"))))
                ((:TYPE . "BLANK")))))
             ((:TYPE . "STRING") (:VALUE . ";"))))))
         ((:TYPE . "FIELD")
          (:NAME . "condition")
          (:CONTENT
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "SYMBOL") (:NAME . "_expression"))
            ((:TYPE . "BLANK")))))
         ((:TYPE . "STRING") (:VALUE . ";"))
         ((:TYPE . "FIELD")
          (:NAME . "update")
          (:CONTENT
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "CHOICE")
             (:MEMBERS
              ((:TYPE . "SYMBOL") (:NAME . "_expression"))
              ((:TYPE . "SYMBOL") (:NAME . "comma_expression"))))
            ((:TYPE . "BLANK")))))
         ((:TYPE . "STRING") (:VALUE . ")"))
         ;; Add a body field.
         ((:TYPE . "FIELD")
          (:NAME . "body")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_statement"))))))
      (:c
       ;; These are specific to C.
       (:-DECLARATION-SPECIFIERS (:TYPE . "SEQ")
        (:MEMBERS
         ;; Inline _declaration_modifiers and wrap a field around it.
         ((:TYPE . "FIELD") (:NAME . "pre-specifiers")
          (:CONTENT
           (:TYPE . "REPEAT")
           (:CONTENT
            (:TYPE . "CHOICE")
            (:MEMBERS
             ((:TYPE . "SYMBOL") (:NAME . "storage_class_specifier"))
             ((:TYPE . "SYMBOL") (:NAME . "type_qualifier"))
             ((:TYPE . "SYMBOL") (:NAME . "attribute_specifier"))
             ((:TYPE . "SYMBOL") (:NAME . "ms_declspec_modifier"))))))
         ;; Wrap a field around the type specifier.
         ((:TYPE . "FIELD") (:NAME . "type")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_type_specifier")))
         ;; Inline declaration-modifiers and wrap a field around it.
         ((:TYPE . "FIELD") (:NAME . "post-specifiers")
          (:CONTENT
           (:TYPE . "REPEAT")
           (:CONTENT
            (:TYPE . "CHOICE")
            (:MEMBERS
             ((:TYPE . "SYMBOL") (:NAME . "storage_class_specifier"))
             ((:TYPE . "SYMBOL") (:NAME . "type_qualifier"))
             ((:TYPE . "SYMBOL") (:NAME . "attribute_specifier"))
             ((:TYPE . "SYMBOL") (:NAME . "ms_declspec_modifier"))))))))
       (:STRUCT-SPECIFIER (:TYPE . "SEQ")
        (:MEMBERS ((:TYPE . "STRING") (:VALUE . "struct"))
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "ms_declspec_modifier"))
           ((:TYPE . "BLANK"))))
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "SEQ")
            (:MEMBERS
             ((:TYPE . "FIELD")
              (:NAME . "name")
              (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_type_identifier")))
             ((:TYPE . "CHOICE")
              (:MEMBERS
               ((:TYPE . "FIELD")
                (:NAME . "body")
                (:CONTENT (:TYPE . "SYMBOL") (:NAME . "field_declaration_list")))
               ((:TYPE . "BLANK"))))))
           ((:TYPE . "FIELD")
            (:NAME . "body")
            (:CONTENT
             (:TYPE . "SYMBOL") (:NAME . "field_declaration_list")))))))
       (:LABELED-STATEMENT
        (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "FIELD")
          (:NAME . "label")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_statement_identifier")))
         ((:TYPE . "STRING") (:VALUE . ":"))
         ((:TYPE . "FIELD")
          (:NAME . "statement")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_statement"))))))
      (:cpp
       ;; These are specific to C++.
       (:REFERENCE-DECLARATOR
        (:TYPE . "PREC_DYNAMIC") (:VALUE . 1)
        (:CONTENT (:TYPE . "PREC_RIGHT") (:VALUE . 0)
         (:CONTENT (:TYPE . "SEQ")
          (:MEMBERS
           ;; Wrap in a field.
           ((:type . "FIELD")
            (:name . "valueness")
            (:content
             (:TYPE . "CHOICE")
             ;; Convert strings to symbols.
             (:MEMBERS ((:TYPE . "SYMBOL") (:name . "&"))
                       ((:TYPE . "SYMBOL") (:name . "&&")))))
           ((:TYPE . "SYMBOL") (:NAME . "_declarator"))))))
       (:-DECLARATION-SPECIFIERS (:TYPE . "SEQ")
        (:MEMBERS
         ;; Inline _declaration_modifiers and wrap a field around it.
         ((:TYPE . "FIELD") (:NAME . "pre-specifiers")
          (:CONTENT
           (:TYPE . "REPEAT")
           (:CONTENT
            (:TYPE . "CHOICE")
            (:MEMBERS
             ((:TYPE . "SYMBOL") (:NAME . "storage_class_specifier"))
             ((:TYPE . "SYMBOL") (:NAME . "type_qualifier"))
             ((:TYPE . "SYMBOL") (:NAME . "attribute_specifier"))
             ((:TYPE . "SYMBOL") (:NAME . "ms_declspec_modifier"))
             ;; Added for C++.
             ((:TYPE . "SYMBOL") (:NAME . "virtual_function_specifier"))
             ((:TYPE . "SYMBOL") (:NAME . "explicit_function_specifier"))))))
         ;; Wrap a field around the type specifier.
         ((:TYPE . "FIELD") (:NAME . "type")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_type_specifier")))
         ;; Inline declaration-modifiers and wrap a field around it.
         ((:TYPE . "FIELD") (:NAME . "post-specifiers")
          (:CONTENT
           (:TYPE . "REPEAT")
           (:CONTENT
            (:TYPE . "CHOICE")
            (:MEMBERS
             ((:TYPE . "SYMBOL") (:NAME . "storage_class_specifier"))
             ((:TYPE . "SYMBOL") (:NAME . "type_qualifier"))
             ((:TYPE . "SYMBOL") (:NAME . "attribute_specifier"))
             ((:TYPE . "SYMBOL") (:NAME . "ms_declspec_modifier"))
             ;; Added for C++.
             ((:TYPE . "SYMBOL") (:NAME . "virtual_function_specifier"))
             ((:TYPE . "SYMBOL") (:NAME . "explicit_function_specifier"))))))))
       (:-CONSTRUCTOR-SPECIFIERS (:TYPE . "REPEAT1")
        (:CONTENT (:TYPE . "PREC_RIGHT") (:VALUE . 0)
         (:CONTENT
          (:TYPE . "FIELD")
          (:NAME . "pre_specifiers")
          (:CONTENT
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "SYMBOL") (:NAME . "storage_class_specifier"))
            ((:TYPE . "SYMBOL") (:NAME . "type_qualifier"))
            ((:TYPE . "SYMBOL") (:NAME . "attribute_specifier"))
            ((:TYPE . "SYMBOL") (:NAME . "virtual_function_specifier"))
            ((:TYPE . "SYMBOL") (:NAME . "explicit_function_specifier")))))))
       ;; TODO This one could probably be upstreamed.
       (:OPERATOR-NAME
        (:TYPE . "PREC") (:VALUE . 1)
        (:CONTENT (:TYPE . "SEQ")
         (:MEMBERS ((:TYPE . "STRING") (:VALUE . "operator"))
          ;; Wrap a "name" field around the operators.
          ((:type . "FIELD")
           (:name . "name")
           (:content
            (:TYPE . "CHOICE")
            (:MEMBERS ((:TYPE . "STRING") (:VALUE . "co_await"))
                      ((:TYPE . "STRING") (:VALUE . "+")) ((:TYPE . "STRING") (:VALUE . "-"))
                      ((:TYPE . "STRING") (:VALUE . "*")) ((:TYPE . "STRING") (:VALUE . "/"))
                      ((:TYPE . "STRING") (:VALUE . "%")) ((:TYPE . "STRING") (:VALUE . "^"))
                      ((:TYPE . "STRING") (:VALUE . "&")) ((:TYPE . "STRING") (:VALUE . "|"))
                      ((:TYPE . "STRING") (:VALUE . "~")) ((:TYPE . "STRING") (:VALUE . "!"))
                      ((:TYPE . "STRING") (:VALUE . "=")) ((:TYPE . "STRING") (:VALUE . "<"))
                      ((:TYPE . "STRING") (:VALUE . ">")) ((:TYPE . "STRING") (:VALUE . "+="))
                      ((:TYPE . "STRING") (:VALUE . "-=")) ((:TYPE . "STRING") (:VALUE . "*="))
                      ((:TYPE . "STRING") (:VALUE . "/=")) ((:TYPE . "STRING") (:VALUE . "%="))
                      ((:TYPE . "STRING") (:VALUE . "^=")) ((:TYPE . "STRING") (:VALUE . "&="))
                      ((:TYPE . "STRING") (:VALUE . "|=")) ((:TYPE . "STRING") (:VALUE . "<<"))
                      ((:TYPE . "STRING") (:VALUE . ">>")) ((:TYPE . "STRING") (:VALUE . ">>="))
                      ((:TYPE . "STRING") (:VALUE . "<<=")) ((:TYPE . "STRING") (:VALUE . "=="))
                      ((:TYPE . "STRING") (:VALUE . "!=")) ((:TYPE . "STRING") (:VALUE . "<="))
                      ((:TYPE . "STRING") (:VALUE . ">=")) ((:TYPE . "STRING") (:VALUE . "&&"))
                      ((:TYPE . "STRING") (:VALUE . "||")) ((:TYPE . "STRING") (:VALUE . "++"))
                      ((:TYPE . "STRING") (:VALUE . "--")) ((:TYPE . "STRING") (:VALUE . ","))
                      ((:TYPE . "STRING") (:VALUE . "->*")) ((:TYPE . "STRING") (:VALUE . "->"))
                      ((:TYPE . "STRING") (:VALUE . "()")) ((:TYPE . "STRING") (:VALUE . "[]"))
                      ((:TYPE . "SEQ")
                       (:MEMBERS
                        ((:TYPE . "CHOICE")
                         (:MEMBERS ((:TYPE . "STRING") (:VALUE . "new"))
                                   ((:TYPE . "STRING") (:VALUE . "delete"))))
                        ((:TYPE . "CHOICE")
                         (:MEMBERS ((:TYPE . "STRING") (:VALUE . "[]")) ((:TYPE . "BLANK"))))))
                      ((:TYPE . "SEQ")
                       (:MEMBERS ((:TYPE . "STRING") (:VALUE . "\"\""))
                                 ((:TYPE . "SYMBOL") (:NAME . "identifier")))))))))))
      (:python
       ;; NOTE: this removes semicolons. This can be further amended if it
       ;;       becomes problematic.
       (:-SIMPLE-STATEMENTS (:TYPE . "SYMBOL") (:NAME . "_simple_statement"))
       (:FUNCTION-DEFINITION (:TYPE . "SEQ")
        (:MEMBERS
         ;; Add a field for async.
         ((:TYPE . "FIELD") (:NAME . "async")
          (:CONTENT
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "STRING") (:VALUE . "async")) ((:TYPE . "BLANK")))))
         ((:TYPE . "STRING") (:VALUE . "def"))
         ((:TYPE . "FIELD") (:NAME . "name")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "identifier")))
         ((:TYPE . "FIELD") (:NAME . "parameters")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "parameters")))
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "SEQ")
            (:MEMBERS ((:TYPE . "STRING") (:VALUE . "->"))
                      ((:TYPE . "FIELD")
                       (:NAME . "return_type")
                       (:CONTENT (:TYPE . "SYMBOL") (:NAME . "type")))))
           ((:TYPE . "BLANK"))))
         ((:TYPE . "STRING") (:VALUE . ":"))
         ((:TYPE . "FIELD") (:NAME . "body")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_suite")))))
       (:FOR-STATEMENT (:TYPE . "SEQ")
        (:MEMBERS
         ;; Add a field for async.
         ((:TYPE . "FIELD") (:NAME . "async")
          (:CONTENT
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "STRING") (:VALUE . "async")) ((:TYPE . "BLANK")))))
         ((:TYPE . "STRING") (:VALUE . "for"))
         ((:TYPE . "FIELD") (:NAME . "left")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_left_hand_side")))
         ((:TYPE . "STRING") (:VALUE . "in"))
         ((:TYPE . "FIELD") (:NAME . "right")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_expressions")))
         ((:TYPE . "STRING") (:VALUE . ":"))
         ((:TYPE . "FIELD") (:NAME . "body")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_suite")))
         ((:TYPE . "FIELD") (:NAME . "alternative")
          (:CONTENT (:TYPE . "CHOICE")
           (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "else_clause"))
                     ((:TYPE . "BLANK")))))))
       (:WITH-STATEMENT (:TYPE . "SEQ")
         (:MEMBERS
          ;; Add a field for async.
          ((:TYPE . "FIELD") (:NAME . "async")
           (:CONTENT
            (:TYPE . "CHOICE")
            (:MEMBERS
             ((:TYPE . "STRING") (:VALUE . "async")) ((:TYPE . "BLANK")))))
          ((:TYPE . "STRING") (:VALUE . "with"))
          ((:TYPE . "SYMBOL") (:NAME . "with_clause"))
          ((:TYPE . "STRING") (:VALUE . ":"))
          ((:TYPE . "FIELD") (:NAME . "body")
           (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_suite")))))
       (:POSITIONAL-ONLY-SEPARATOR (:TYPE . "STRING") (:VALUE . "/"))
       (:KEYWORD-ONLY-SEPARATOR (:TYPE . "STRING") (:VALUE . "*"))
       ;; NOTE: remove this if backtracking is implemented for
       ;;       match-parsed-children.
       (:COMPARISON-OPERATOR (:TYPE . "PREC_LEFT") (:VALUE . 2)
        (:CONTENT (:TYPE . "SEQ")
         (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "primary_expression"))
          ((:TYPE . "REPEAT1")
           (:CONTENT
            (:TYPE . "SEQ")
            (:MEMBERS
             ((:TYPE . "FIELD")
              (:NAME . "operators")
              (:CONTENT
               (:TYPE . "CHOICE")
               (:MEMBERS
                ((:TYPE . "STRING") (:VALUE . "<"))
                ((:TYPE . "STRING") (:VALUE . "<="))
                ((:TYPE . "STRING") (:VALUE . "=="))
                ((:TYPE . "STRING") (:VALUE . "!="))
                ((:TYPE . "STRING") (:VALUE . ">="))
                ((:TYPE . "STRING") (:VALUE . ">"))
                ((:TYPE . "STRING") (:VALUE . "<>"))
                ((:TYPE . "SEQ")
                 (:MEMBERS
                  ((:TYPE . "STRING") (:VALUE . "not"))
                  ((:TYPE . "STRING") (:VALUE . "in"))))
                ((:TYPE . "STRING") (:VALUE . "in"))
                ;; The next two clauses are swapped.
                ((:TYPE . "SEQ")
                 (:MEMBERS ((:TYPE . "STRING") (:VALUE . "is"))
                           ((:TYPE . "STRING") (:VALUE . "not"))))
                ((:TYPE . "STRING") (:VALUE . "is")))))
             ((:TYPE . "SYMBOL") (:NAME . "primary_expression")))))))))
      ((:javascript :typescript-ts :typescript-tsx)
       (:-SEMICOLON (:TYPE . "CHOICE")
        (:MEMBERS
         ;; Put the string before the symbol.
         ((:TYPE . "STRING") (:VALUE . ";"))
         ((:TYPE . "SYMBOL") (:NAME . "_automatic_semicolon")))))
      (:javascript
       ;; TODO: add a substitution for javascript-array.
       ;;       This will add a javascript-blank-ast between any two
       ;;       consecutive commas via a parse tree transformation.
       (:FUNCTION-DECLARATION
        (:TYPE . "PREC_RIGHT") (:VALUE . "declaration")
        (:CONTENT
         (:TYPE . "SEQ")
         (:MEMBERS
          ;; Add a field for `async'.
          ((:TYPE . "FIELD") (:NAME . "async")
           (:CONTENT
            (:TYPE . "CHOICE")
            (:MEMBERS
             ((:TYPE . "STRING") (:VALUE . "async"))
             ((:TYPE . "BLANK")))))
          ((:TYPE . "STRING") (:VALUE . "function"))
          ((:TYPE . "FIELD") (:NAME . "name")
           (:CONTENT
            (:TYPE . "SYMBOL") (:NAME . "identifier")))
          ((:TYPE . "SYMBOL") (:NAME . "_call_signature"))
          ((:TYPE . "FIELD") (:NAME . "body")
           (:CONTENT
            (:TYPE . "SYMBOL") (:NAME . "statement_block")))
          ((:TYPE . "CHOICE")
           (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "_automatic_semicolon"))
                     ((:TYPE . "BLANK")))))))
       (:EXPORT-STATEMENT (:TYPE . "CHOICE")
        (:MEMBERS
         ((:TYPE . "SEQ")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "export"))
           ((:TYPE . "CHOICE")
            (:MEMBERS
             ;; NOTE: the order here has been changed from the tree-sitter rule.
             ((:TYPE . "SEQ")
              (:MEMBERS
               ((:TYPE . "SYMBOL") (:NAME . "export_clause"))
               ((:TYPE . "SYMBOL") (:NAME . "_from_clause"))
               ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))
             ((:TYPE . "SEQ")
              (:MEMBERS
               ((:TYPE . "SYMBOL") (:NAME . "namespace_import"))
               ((:TYPE . "SYMBOL") (:NAME . "_from_clause"))
               ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))
             ((:TYPE . "SEQ")
              (:MEMBERS
               ((:TYPE . "SYMBOL") (:NAME . "export_clause"))
               ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))
             ((:TYPE . "SEQ")
              (:MEMBERS
               ((:TYPE . "STRING") (:VALUE . "*"))
               ((:TYPE . "SYMBOL") (:NAME . "_from_clause"))
               ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))))))
         ((:TYPE . "SEQ")
          (:MEMBERS
           ((:TYPE . "REPEAT")
            (:CONTENT
             (:TYPE . "FIELD") (:NAME . "decorator")
             (:CONTENT (:TYPE . "SYMBOL") (:NAME . "decorator"))))
           ((:TYPE . "STRING") (:VALUE . "export"))
           ((:TYPE . "CHOICE")
            (:MEMBERS
             ((:TYPE . "SEQ")
              (:MEMBERS
               ((:TYPE . "FIELD")
                (:NAME . "default")
                (:CONTENT (:TYPE . "STRING") (:VALUE . "default")))
               ((:TYPE . "CHOICE")
                (:MEMBERS
                 ((:TYPE . "FIELD") (:NAME . "declaration")
                                    (:CONTENT (:TYPE . "SYMBOL") (:NAME . "declaration")))
                 ((:TYPE . "SEQ")
                  (:MEMBERS
                   ((:TYPE . "FIELD") (:NAME . "value")
                                      (:CONTENT (:TYPE . "SYMBOL") (:NAME . "expression")))
                   ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))))))
             ((:TYPE . "FIELD")
              (:NAME . "declaration")
              (:CONTENT (:TYPE . "SYMBOL") (:NAME . "declaration"))))))))))
      ((:typescript-ts :typescript-tsx)
       (:-PARAMETER-NAME
        (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "REPEAT")
          (:CONTENT (:TYPE . "FIELD") (:NAME . "decorator")
           (:CONTENT (:TYPE . "SYMBOL") (:NAME . "decorator"))))
         ;; Group these as a modifiers field.
         ((:type . "FIELD")
          (:name . "modifiers")
          (:content
           (:type . "SEQ")
           (:members
            ((:TYPE . "CHOICE")
             (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "accessibility_modifier"))
                       ((:TYPE . "BLANK"))))
            ((:TYPE . "CHOICE")
             (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "override_modifier"))
                       ((:TYPE . "BLANK"))))
            ((:TYPE . "CHOICE")
             ;; Upstream tree-sitter has this as a STRING instead of a
             ;; symbol, which means it is always printed.
             (:MEMBERS ((:type . "SYMBOL") (:name . "readonly")) ((:TYPE . "BLANK")))))))
         ((:TYPE . "FIELD") (:NAME . "pattern")
          (:CONTENT (:TYPE . "CHOICE")
           (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "pattern"))
                     ((:TYPE . "SYMBOL") (:NAME . "this")))))))
       (:EXPORT-STATEMENT
        (:TYPE . "CHOICE")
        (:MEMBERS
         ;; The following is the same as the Javascript above, but
         ;; substituted for the original.
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "SEQ")
            (:MEMBERS ((:TYPE . "STRING") (:VALUE . "export"))
                      ((:TYPE . "CHOICE")
                       (:MEMBERS
                        ;; NOTE: the order here has been changed from the tree-sitter rule.
                        ((:TYPE . "SEQ")
                         (:MEMBERS
                          ((:TYPE . "SYMBOL") (:NAME . "export_clause"))
                          ((:TYPE . "SYMBOL") (:NAME . "_from_clause"))
                          ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))
                        ((:TYPE . "SEQ")
                         (:MEMBERS
                          ;; This has been changed into an explicit alias.
                          ((:TYPE . "ALIAS")
                           (:CONTENT (:TYPE . "SYMBOL")
                                     (:NAME . "namespace_import_export"))
                           (:NAMED . T) (:VALUE . "namespace_export"))
                          ((:TYPE . "SYMBOL") (:NAME . "_from_clause"))
                          ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))
                        ((:TYPE . "SEQ")
                         (:MEMBERS
                          ((:TYPE . "SYMBOL") (:NAME . "export_clause"))
                          ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))
                        ((:TYPE . "SEQ")
                         (:MEMBERS
                          ((:type . "FIELD")
                           (:name . "default")
                           (:content
                            (:TYPE . "STRING") (:VALUE . "*")))
                          ((:TYPE . "SYMBOL") (:NAME . "_from_clause"))
                          ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))))))
           ((:TYPE . "SEQ")
            (:MEMBERS
             ((:TYPE . "REPEAT")
              (:CONTENT
               (:TYPE . "FIELD") (:NAME . "decorator")
               (:CONTENT (:TYPE . "SYMBOL") (:NAME . "decorator"))))
             ((:TYPE . "STRING") (:VALUE . "export"))
             ((:TYPE . "CHOICE")
              (:MEMBERS
               ((:TYPE . "SEQ")
                (:MEMBERS
                 ((:TYPE . "FIELD")
                  (:NAME . "default")
                  (:CONTENT (:TYPE . "STRING") (:VALUE . "default")))
                 ((:TYPE . "CHOICE")
                  (:MEMBERS
                   ((:TYPE . "FIELD") (:NAME . "declaration")
                                      (:CONTENT (:TYPE . "SYMBOL") (:NAME . "declaration")))
                   ((:TYPE . "SEQ")
                    (:MEMBERS
                     ((:TYPE . "FIELD") (:NAME . "value")
                                        (:CONTENT (:TYPE . "SYMBOL") (:NAME . "expression")))
                     ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))))))
               ((:TYPE . "FIELD")
                (:NAME . "declaration")
                (:CONTENT (:TYPE . "SYMBOL") (:NAME . "declaration")))))))))
         ((:TYPE . "SEQ")
          (:MEMBERS
           ((:TYPE . "STRING") (:VALUE . "export"))
           ((:TYPE . "SYMBOL") (:NAME . "type"))
           ((:TYPE . "SYMBOL") (:NAME . "export_clause"))))
         ((:TYPE . "SEQ")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "export"))
           ((:type . "FIELD")
            (:name . "default")
            (:content
             (:TYPE . "STRING") (:VALUE . "=")))
           ((:TYPE . "SYMBOL") (:NAME . "identifier"))
           ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))
         ((:TYPE . "SEQ")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "export"))
           ((:type . "FIELD")
            (:name . "default")
            (:content
             (:type . "SEQ")
             (:members
              ((:TYPE . "SYMBOL") (:NAME . "as"))
              ((:TYPE . "SYMBOL") (:NAME . "namespace")))))
           ((:TYPE . "SYMBOL") (:NAME . "identifier"))
           ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))))
       (:method-definition
        (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "accessibility_modifier"))
           ((:TYPE . "BLANK"))))
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "static")) ((:TYPE . "BLANK"))))
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "override_modifier"))
           ((:TYPE . "BLANK"))))
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "readonly")) ((:TYPE . "BLANK"))))
         ;; Add a field for async.
         ((:type . "FIELD")
          (:name . "async")
          (:content
           (:TYPE . "CHOICE")
           (:MEMBERS ((:TYPE . "STRING") (:VALUE . "async")) ((:TYPE . "BLANK")))))
         ;; Added a new field to hold get/set.
         ((:type . "FIELD")
          (:name . "getter-setter")
          (:content
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "CHOICE")
             (:MEMBERS ((:TYPE . "STRING") (:VALUE . "get"))
                       ((:TYPE . "STRING") (:VALUE . "set"))
                       ((:TYPE . "STRING") (:VALUE . "*"))))
            ((:TYPE . "BLANK")))))
         ((:TYPE . "FIELD") (:NAME . "name")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_property_name")))
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "?")) ((:TYPE . "BLANK"))))
         ((:TYPE . "SYMBOL") (:NAME . "_call_signature"))
         ((:TYPE . "FIELD") (:NAME . "body")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "statement_block")))))
       (:public-field-definition
        (:TYPE . "SEQ")
        (:MEMBERS
         ;; Add a field for declare.
         ((:type . "FIELD")
          (:name . "declare")
          (:content
           (:TYPE . "CHOICE")
           (:MEMBERS ((:TYPE . "STRING") (:VALUE . "declare")) ((:TYPE . "BLANK")))))
         ;; Added a "modifiers" field.
         ((:type . "FIELD")
          (:name . "modifiers")
          (:content
           (:type . "SEQ")
           (:members
            ((:TYPE . "CHOICE")
             (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "accessibility_modifier"))
                       ((:TYPE . "BLANK"))))
            ((:TYPE . "CHOICE")
             (:MEMBERS
              ((:TYPE . "SEQ")
               (:MEMBERS
                ((:TYPE . "CHOICE")
                 (:MEMBERS ((:TYPE . "STRING") (:VALUE . "static")) ((:TYPE . "BLANK"))))
                ((:TYPE . "CHOICE")
                 (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "override_modifier"))
                           ((:TYPE . "BLANK"))))
                ((:TYPE . "CHOICE")
                 ;; string -> symbol
                 (:MEMBERS ((:TYPE . "SYMBOL") (:name . "readonly"))
                           ((:TYPE . "BLANK"))))))
              ((:TYPE . "SEQ")
               ;; Convert abstract and readonly from strings to symbols.
               (:MEMBERS
                ((:TYPE . "CHOICE")
                 (:MEMBERS ((:TYPE . "SYMBOL") (:name . "abstract"))
                           ((:TYPE . "BLANK"))))
                ((:TYPE . "CHOICE")
                 (:MEMBERS ((:TYPE . "SYMBOL") (:name . "readonly"))
                           ((:TYPE . "BLANK"))))))
              ((:TYPE . "SEQ")
               (:MEMBERS
                ((:TYPE . "CHOICE")
                 (:MEMBERS ((:TYPE . "SYMBOL") (:name . "readonly"))
                           ((:TYPE . "BLANK"))))
                ((:TYPE . "CHOICE")
                 (:MEMBERS ((:TYPE . "SYMBOL") (:name . "abstract"))
                           ((:TYPE . "BLANK")))))))))))
         ((:TYPE . "FIELD") (:NAME . "name")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_property_name")))
         ((:type . "FIELD")
          (:name . "optional")
          (:content
           (:TYPE . "CHOICE")
           (:MEMBERS
            ((:TYPE . "CHOICE")
             (:MEMBERS ((:TYPE . "STRING") (:VALUE . "?"))
                       ((:TYPE . "STRING") (:VALUE . "!"))))
            ((:TYPE . "BLANK")))))
         ((:TYPE . "FIELD") (:NAME . "type")
          (:CONTENT (:TYPE . "CHOICE")
           (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "type_annotation"))
                     ((:TYPE . "BLANK")))))
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "_initializer"))
           ((:TYPE . "BLANK"))))))
       (:call-expression
        (:TYPE . "CHOICE")
        (:MEMBERS
         ;; Swap order.
         ((:TYPE . "PREC") (:VALUE . "member")
          (:CONTENT (:TYPE . "SEQ")
           (:MEMBERS
            ((:TYPE . "FIELD") (:NAME . "function")
                               (:CONTENT (:TYPE . "SYMBOL") (:NAME . "primary_expression")))
            ;; Wrap with operator field.
            ((:type . "FIELD")
             (:name . "operator")
             (:content
              (:TYPE . "STRING") (:VALUE . "?.")))
            ((:TYPE . "FIELD") (:NAME . "type_arguments")
                               (:CONTENT (:TYPE . "CHOICE")
                                         (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "type_arguments"))
                                                   ((:TYPE . "BLANK")))))
            ((:TYPE . "FIELD") (:NAME . "arguments")
                               (:CONTENT (:TYPE . "SYMBOL") (:NAME . "arguments"))))))
         ((:TYPE . "PREC") (:VALUE . "call")
          (:CONTENT (:TYPE . "SEQ")
           (:MEMBERS
            ((:TYPE . "FIELD") (:NAME . "function")
                               (:CONTENT (:TYPE . "SYMBOL") (:NAME . "expression")))
            ((:TYPE . "FIELD") (:NAME . "type_arguments")
                               (:CONTENT (:TYPE . "CHOICE")
                                         (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "type_arguments"))
                                                   ((:TYPE . "BLANK")))))
            ((:TYPE . "FIELD") (:NAME . "arguments")
                               (:CONTENT (:TYPE . "CHOICE")
                                         (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "arguments"))
                                                   ((:TYPE . "SYMBOL") (:NAME . "template_string"))))))))))))
    "A mapping of JSON rule substitutions to be performed on the JSON file
before class generation and analysis.

Using this variable allows you to override the definitions of rules in
the `grammar.json' file for a given language.

Most commonly this is used to insert fields in places where
tree-sitter would otherwise lose data. Note that in that case you will
also need to (1) add a slot for the field in
`*tree-sitter-ast-extra-slots*' above and (2) define a corresponding
`transform-parse-tree' method to postprocess the output of
tree-sitter.")

  (defparameter *tree-sitter-json-rule-patches*
    '((:cpp
       (:field-expression
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "."))
           ((:TYPE . "STRING") (:VALUE . "->"))))
         :as "operator"))
       (:access-specifier
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "public"))
           ((:TYPE . "STRING") (:VALUE . "private"))
           ((:TYPE . "STRING") (:VALUE . "protected"))))
         :as "keyword"))
       (:type-parameter-declaration
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "typename"))
           ((:TYPE . "STRING") (:VALUE . "class"))))
         :as "keyword")))
      ((:typescript-ts :typescript-tsx)
       (:property-signature
        (:replace
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "?"))
           ((:TYPE . "BLANK"))))
         :with
         ((:type . "FIELD")
          (:name . "optional")
          (:content
           (:TYPE . "CHOICE")
           ;; String to symbol.
           (:MEMBERS ((:type . "SYMBOL") (:name . "?"))
                     ((:TYPE . "BLANK")))))))
       ;; TODO JS too?
       (:member-expression
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "."))
           ((:TYPE . "STRING") (:VALUE . "?."))))
         :as "operator"))
       (:subscript-expression
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "?.")) ((:TYPE . "BLANK"))))
         :as "operator"))
       ((:arrow-function
         :function
         :function-declaration
         :function-signature
         :generator-function-declaration)
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "async"))
           ((:TYPE . "BLANK"))))
         :as "async"))
       (:enum-declaration
        (:label
         ((:TYPE . "CHOICE")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "const"))
           ((:TYPE . "BLANK"))))
         :as "kind"))
       (:class-body
        (:replace
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "SYMBOL")
            (:NAME . "_function_signature_automatic_semicolon"))
           ((:TYPE . "STRING") (:VALUE . ","))))
         :with
         ((:TYPE . "CHOICE")
          ;; The function_signature_automatic_semicolon token from the
          ;; external scanner confuses children-parser.
          (:MEMBERS
           ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))
           ((:TYPE . "STRING") (:VALUE . ","))))))))
    "Nested alist of patches to JSON rules.
Organized first by relevant language (or list of relevant languages)
and then by AST types.

Supported patch syntaxes are:
- `(:replace AST1 :with AST2)' Replace every instance of AST1 with AST2.
- `(:wrap AST1 :with AST2)' Replace every instance of AST1 with
  AST2, substituting AST1 for every instance of `_' in AST2.
- `(:label AST1 :as NAME)` Wrap every instance of AST1 with a field
  named NAME.

All tests are done with `EQUAL'.")

  (defparameter *tree-sitter-json-node-type-substitutions*
    '((:python
       ((:TYPE . "positional_only_separator") (:NAMED . T))
       ((:TYPE . "keyword_only_separator") (:NAMED . T))
       ((:TYPE . "parameter") (:NAMED . T)
        (:SUBTYPES
         ((:TYPE . "positional_only_separator") (:NAMED . T))
         ((:TYPE . "keyword_only_separator") (:NAMED . T))
         ((:TYPE . "default_parameter") (:NAMED . T))
         ((:TYPE . "dictionary_splat_pattern") (:NAMED . T))
         ((:TYPE . "identifier") (:NAMED . T))
         ((:TYPE . "list_splat_pattern") (:NAMED . T))
         ((:TYPE . "tuple_pattern") (:NAMED . T))
         ((:TYPE . "typed_default_parameter") (:NAMED . T))
         ((:TYPE . "typed_parameter") (:NAMED . T)))))
      ;; Both the TSX and TypeScript node-types.json have duplicate
      ;; number types, one named and one not named, but in the
      ;; opposite orders. This makes them consistent (and allows TSX
      ;; to parse numbers!).
      ((:typescript-ts :typescript-tsx)
       ((:type . "number") (:named . t)))
      (:rust
       ((:TYPE . "_pattern") (:NAMED . T)
        (:SUBTYPES
         ;; NOTE: removes the "_" from here which unfortunately causes some
         ;;       difficult problems to solve in SEL. Removing this information
         ;;       shouldn't be an issue as it should be implicit when the parent
         ;;       is taken into account. It may cause some problems with parsing,
         ;;       these will need to be addressed when Rust becomes a target
         ;;       language.
         ((:TYPE . "_literal_pattern") (:NAMED . T))
         ((:TYPE . "captured_pattern") (:NAMED . T))
         ((:TYPE . "const_block") (:NAMED . T))
         ((:TYPE . "identifier") (:NAMED . T))
         ((:TYPE . "mut_pattern") (:NAMED . T))
         ((:TYPE . "or_pattern") (:NAMED . T))
         ((:TYPE . "range_pattern") (:NAMED . T))
         ((:TYPE . "ref_pattern") (:NAMED . T))
         ((:TYPE . "reference_pattern") (:NAMED . T))
         ((:TYPE . "remaining_field_pattern") (:NAMED . T))
         ((:TYPE . "scoped_identifier") (:NAMED . T))
         ((:TYPE . "slice_pattern") (:NAMED . T))
         ((:TYPE . "struct_pattern") (:NAMED . T))
         ((:TYPE . "tuple_pattern") (:NAMED . T))
         ((:TYPE . "tuple_struct_pattern") (:NAMED . T))))))
    "A mapping of JSON node type substitutions to be performed on the JSON file
before class generation and analysis. This effectively allows the definition
of new classes.")

  (defparameter *tree-sitter-json-subtree-choice-resolver* nil
    ;; Currently unused.
    "A mapping of functions for resolving which choice branch should be
chosen when gathering a string representation of a JSON subtree.")

  (defparameter *tree-sitter-json-field-transformations*
    `((:c
       (:symbol-names
        ("#ifdef" "#ifndef")
        :slot-name "operator"      ; Find a better name for this slot.
        :predicate ,(constantly t)
        :transform
        (lambda (parse-tree)
          (copy-parse-tree
           parse-tree
           :children
           (mapcar
            (lambda (child-tree &aux (child-type (parse-tree-type child-tree)))
              (cond
                ((member child-type '(:|#IFDEF| :|#IFNDEF|))
                 (cons (list :operator child-type) (cdr child-tree)))
                (t child-tree)))
            (parse-tree-children parse-tree))))))
      (:cpp
       (:symbol-names ("#ifdef" "#ifndef")
        :slot-name "operator"      ; Find a better name for this slot.
        :predicate ,(constantly t)
        :transform
        (lambda (parse-tree)
          (copy-parse-tree
           parse-tree
           :children
           (mapcar
            (lambda (child-tree &aux (child-type (parse-tree-type child-tree)))
              (cond
                ((member child-type '(:|#IFDEF| :|#IFNDEF|))
                 (cons (list :operator child-type) (cdr child-tree)))
                (t child-tree)))
            (parse-tree-children parse-tree))))))
      ((:javascript :typescript-ts :typescript-tsx)
       (:symbol-names ("_semicolon")
        :slot-name "semicolon"
        :predicate ,(constantly t)
        :transform
        (lambda (parse-tree)
          (copy-parse-tree
           parse-tree
           :children
           (mapcar
            (lambda (child-tree &aux (child-type (parse-tree-type child-tree)))
              (cond
                ((eql child-type :|;|)
                 (cons (list :semicolon child-type) (cdr child-tree)))
                (t child-tree)))
            (parse-tree-children parse-tree))))))
      ;; This is needed to handle case in TypeScript where it allows
      ;; commas to be used in place of semicolons (e.g. :object-type).
      ((:typescript-ts :typescript-tsx)
       (:symbol-names (",")
        :slot-name "comma"
        :predicate ,(constantly t)
        :transform
        (lambda (parse-tree)
          (with-modify-parse-tree (parse-tree)
            ((:|,|) (label-as :comma)))))))
    "A mapping of tree-sitter symbol names that should have fields wrapped
around them. It is also followed by the slot that should be added to the relevant
class, a predicate that determines whether a subtree should be encapsulated and
the body of a parse tree transformation for each class that uses it.")

  (defparameter *tree-sitter-ast-superclass-table*
    (lret ((table (make-hash-table)))
      (iter
       (for (langs . alist) in *tree-sitter-ast-superclasses*)
       (iter (for lang in (ensure-list langs))
             (let* ((lang (intern (string lang) :sel/sw/ts))
                    (lang-table
                     (ensure-gethash lang table (make-hash-table))))
               (iter
                (for (mixin . subclasses) in alist)
                (let ((mixin (find-symbol (string mixin) :sel/sw/ts)))
                  (dolist (subclass subclasses)
                    (push subclass (gethash mixin lang-table)))))))))
    "Nested hash table from language and mixin to a list of classes
    that inherit from that mixin.")

  (defparameter *tree-sitter-ast-extra-prefixes*
    '((:c c/cpp)
      (:cpp c/cpp)
      (:typescript-ts typescript ecma)
      (:typescript-tsx typescript ecma)
      (:javascript ecma))
    "Alist of languages and extra prefixes.
For every extra prefix, every slot will get an extra reader and an
extra initarg with that prefix.")

  (defparameter *tree-sitter-computed-text-asts*
    '((:python python-string)
      (:javascript javascript-template-string))
    "Alist of languages and their classes which should be computed-text ASTs
but aren't detected as such. This is usually due to insufficient information
stored on the AST or external rules.")

  (defun aget-all (key alist)
    "Like `aget', but with two differences:
1. It also matches on conses where the car is a list contains KEY.
2. It collects *all* the matching clauses and appends them."
    (mappend #'cdr
             (filter (lambda (cons)
                       (member key (ensure-list (car cons))))
                     alist)))

  (defun tree-sitter-ast-classes (name grammar-file node-types-file)
    (nest
     (flet ((alternate-class-name (name)
              (string-case name
                ("GO" "GOLANG")
                ("TYPESCRIPT-TYPESCRIPT" "TYPESCRIPT-TS")
                (t name)))))
     (let* ((path-name (replace-all name "/" "-"))
            (class-name (alternate-class-name (string-upcase path-name)))
            (class-keyword (make-keyword class-name))))
     `((register-tree-sitter-language
        ,(string-join (list "tree-sitter" path-name) #\-)
        ,class-keyword
        ',(intern (string-join (list class-name "AST") #\-)
                  :software-evolution-library/software/tree-sitter))
       ,(create-tree-sitter-classes
         node-types-file
         grammar-file
         class-name
         :ast-superclasses
         (aget-all class-keyword *tree-sitter-ast-superclasses*)
         :base-ast-superclasses
         (aget-all class-keyword *tree-sitter-base-ast-superclasses*)
         :software-superclasses
         (aget-all class-keyword *tree-sitter-software-superclasses*)
         :software-direct-slots
         (aget-all class-keyword *tree-sitter-software-direct-slots*)
         :ast-extra-slot-options
         (aget-all class-keyword *tree-sitter-ast-extra-slot-options*)
         :ast-extra-slots
         (aget-all class-keyword *tree-sitter-ast-extra-slots*)
         :node-type-substitutions
         (aget-all class-keyword *tree-sitter-json-node-type-substitutions*)
         :json-subtree-choice-resolver
         (car (aget-all class-keyword *tree-sitter-json-subtree-choice-resolver*))
         :json-field-transformations
         (aget-all class-keyword *tree-sitter-json-field-transformations*))))))

(-> ast-mixin-subclasses ((or symbol class) (or symbol class)) list)
(defun ast-mixin-subclasses (class language)
  "Return a list of AST classes for LANGUAGE that inherit from CLASS."
  (declare ((and symbol (not keyword)) class language))
  (let ((table *tree-sitter-ast-superclass-table*))
    (when-let (language-table (gethash language table))
      (gethash class language-table))))

(defmacro register-tree-sitter-language (lib-name language ast-superclass)
  "Setup LANGUAGE to map to AST-SUPERCLASS and use LIB-NAME for parsing."
  (let ((register-language #.(when (asdf:find-system :cl-tree-sitter nil) t)))
    `(eval-always
       (handler-case
           (progn
             (when ,register-language
               (handler-case
                   (progn
                     (when ,register-language
                       (register-language
                        ,language ,lib-name
                        ,@(string-case lib-name
                            ("tree-sitter-typescript-typescript"
                             '(:fn-name "tree_sitter_typescript"))
                            ("tree-sitter-typescript-tsx"
                             '(:fn-name "tree_sitter_tsx")))))
                     (setf (gethash ,ast-superclass *superclass->language*) ,language))
                 ;; Try again with an augmented library search path.
                 (load-foreign-library-error ()
                   (register-language ,language ,(concatenate 'string "/usr/lib/" lib-name)))))
             (setf (gethash ,ast-superclass *superclass->language*) ,language))
         (load-foreign-library-error ()
           (format
            *error-output*
            "Failed to load '~a'. Support for '~a' will not be available.~%"
            ,lib-name ,language))))))


;;; Defining tree-sitter classes

(defmacro define-template-builder (class ast-class)
  `(progn
     (defun ,class
         (template &rest args)
       "Short for (ast-template TEMPLATE python-ast ARGS...)."
       (apply #'ast-template template ',ast-class args))

     ;; Define compiler macro so the template can be
     ;; statically checked.
     (define-compiler-macro ,class (template &rest args)
       (list* 'ast-template template '',ast-class args))

     (defpattern ,class (template &rest args)
       (list* 'ast-template template ',ast-class args))))

(eval-always
 ;; TODO The methods on null for before-text and after-text are insurance
 ;; against tree-manipulation methods that might affect the slots if they
 ;; contain conflict ASTs.

 (defgeneric before-text (ast)
   (:method :around (ast)
     (let ((result (call-next-method)))
       (if (null result) "" result))))

 (defgeneric after-text (ast)
   (:method :around (ast)
     (let ((result (call-next-method)))
       (if (null result) "" result))))

  ;; TODO: let over a basic string for empty strings?
  (defclass structured-text ()
    ((before-text
      :accessor before-text
      :initarg :before-text
      :initform ""
      :documentation "The text before the first token of an AST.")
     ;; NOTE: the primary usage of this slot is for AST text, like
     ;;       identifiers, which doesn't belong in the before or after slot.
     (text
      :accessor text
      :initarg :text
      :initform nil
      ;; NOTE: it's hard to name this slot descriptively such that its usage
      ;;       is obvious.
      :documentation "The text that can vary between ASTs of the same class. It
stores the part that is computed from the variable definition. This should be
stored as a list of interleaved text. This should ideally only be used for leaf
 nodes.")
     (after-text
      :accessor after-text
      :initarg :after-text
      :initform ""
      :documentation "The text after the last token of an AST.")
     (before-asts
      :accessor before-asts
      :initarg :before-asts
      :initform nil
      :documentation
      "A list of comments and errors that precede the before text of an AST.")
     (after-asts
      :accessor after-asts
      :initarg :after-asts
      :initform nil
      :documentation
      "A list of comments and errors that procede the after text of an AST."))
    (:documentation "Mix-in for structured text ASTs."))

  (defclass tree-sitter-ast (indentation
                             structured-text
                             functional-tree-ast)
    ()
    (:documentation "AST for input from tree-sitter."))

  (defclass computed-text ()
    ()
    (:documentation "A mixin for computed text ASTs."))

  (defclass text-fragment (tree-sitter-ast)
    ((text
      :accessor text
      :initarg :text
      :initform nil)
     (child-slot-specifiers
      :initform nil
      :allocation :class))
    (:documentation "A wrapper class for text fragments in computed text ASTs."))

  (defclass source-text-fragment (computed-text)
    ((text :accessor text
           :initform ""
           :initarg :text)
     (choice-subclasses
      :initform nil
      :reader choice-subclasses
      :allocation :class))
    (:documentation "A mixin for ASTs that represent fragments of source text.")
    (:default-initargs :indent-adjustment 0))

  (defclass inner-whitespace (text-fragment)
    ()
    (:documentation "An AST that represents whitespace between two
terminal tokens."))

  (define-node-class inner-parent (tree-sitter-ast)
    ((children
      :initform nil
      :initarg :children
      :accessor children)
     (child-slots
      :initform '((children . 0))
      :reader child-slots
      :allocation :class))
    (:documentation "A container class that holds multiple children which occur
between the same two terminal tokens."))

  (defclass definition-ast (ast) ()
    (:documentation "AST for something that associates a name with a thing.
The name string is obtained by by DEFINITION-NAME"))

  (defclass comment-ast (ast) ()
    (:documentation "Mix-in for AST classes that are comments.

Superclass of every generated LANGUAGE-comment class."))

  (defclass ecma-comment (comment-ast) ()
    (:documentation "Mix-in for comments in dialects of ECMAScript."))

  (defclass root-ast (ast) ()
    (:documentation "Mix-in for AST classes which are roots."))

  (defclass statement-ast (ast) ()
    (:documentation "Mix-in for AST classes that are statements."))

  (defclass expression-statement-ast (ast) ()
    (:documentation "Mix-in for AST classes that are expression statements."))

  (defclass expression-ast (ast) ()
    (:documentation "Mix-in for AST classes that are expressions."))

  (defclass parenthesized-expression-ast (ast) ()
    (:documentation "Mix-in for AST classes that are parenthesized
    expressions."))

  (defclass compound-ast (ast) ()
    (:documentation "Mix-in for AST classes that are compounds."))

  (defclass conditional-ast (ast) ()
    (:documentation "Mix-in for AST classes that have a conditional."))

  (defclass control-flow-ast (ast) ()
    (:documentation "Mix-in for AST classes that have control flow."))

  (defclass if-ast (control-flow-ast conditional-ast) ()
    (:documentation "Mix-in for AST classes that are ifs."))

  (defclass while-ast (control-flow-ast conditional-ast) ()
    (:documentation "Mix-in for AST classes that are whiles."))

  (defclass loop-ast (control-flow-ast) ()
    (:documentation "Mix-in for AST classes that are loops."))

  (defclass class-ast (ast) ()
    (:documentation "Mix-in for AST classes that are classes."))

  (defclass parse-error-ast (ast) ()
    (:documentation
     "Mix-in for AST classes that represent tree-sitter parsing errors.

Superclass of every generated LANGUAGE-error class."))

  (defclass function-ast (ast) ()
    (:documentation "Mix-in for AST classes that are functions."))

  (defclass parameters-ast (ast) ()
    (:documentation "Mix-in for AST classes that are parameter lists."))

  (defclass variable-declaration-ast (ast) ()
    (:documentation "Mix-in for AST classes that are variable declarations."))

  (defclass identifier-ast (ast) ()
    (:documentation "Mix-in for AST classes that are identifiers."))

  (defclass field-ast (ast) ()
    (:documentation "Mix-in for AST classes that are fields."))

  (defclass subscript-ast (ast) ()
    (:documentation "Mix-in for AST classes that are subscripts."))

  (defclass lambda-ast (ast) ()
    (:documentation "Mix-in for AST classes that are lambdas."))

  (defclass literal-ast (ast) ()
    (:documentation "Mix-in for AST classes that are literals."))

  (defclass boolean-ast (literal-ast) ()
    (:documentation "Mix-in for AST classes that are booleans."))

  (defclass boolean-true-ast (boolean-ast) ()
    (:documentation "Mix-in for AST classes that are true booleans."))

  (defclass boolean-false-ast (boolean-ast) ()
    (:documentation "Mix-in for AST classes that are false booleans."))

  (defclass char-ast (literal-ast) ()
    (:documentation "Mix-in for AST classes that are literal chars."))

  (defclass string-ast (literal-ast) ()
    (:documentation "Mix-in for AST classes that are literal strings."))

  (defclass number-ast (literal-ast) ()
    (:documentation "Mix-in for AST classes that are literal numbers."))

  (defclass integer-ast (number-ast) ()
    (:documentation "Mix-in for AST classes that are literal integers."))

  (defclass float-ast (number-ast) ()
    (:documentation "Mix-in for AST classes that are literal floats."))

  (defclass call-ast (expression-ast) ()
    (:documentation "Mix-in for AST classes that are calls."))

  (defclass arguments-ast (ast) ()
    (:documentation "Mix-in for AST classes that are lists of arguments."))

  (defclass unary-ast (expression-ast) ()
    (:documentation "Mix-in for AST classes that are unary expressions."))

  (defclass binary-ast (expression-ast) ()
    (:documentation "Mix-in for AST classes that are binary expressions."))

  (defclass return-ast (statement-ast) ()
    (:documentation "Mix-in for AST classes that are return statements."))

  (defclass goto-ast (statement-ast) ()
    (:documentation "Mix-in for AST classes that are goto statements."))

  (defclass catch-ast (statement-ast) ()
    (:documentation "Mix-in for AST classes that are error catch clauses."))

  (defclass terminal-symbol ()
    ()
    (:documentation "Mix-in for terminal symbols. Note that this won't fully
cover every terminal symbol, only the ones that aren't named.")
    (:default-initargs :indent-adjustment 0))

  (defun convert-name (name-string)
    (substitute #\- #\_  (string-upcase (string name-string))))

  (defun translate-to-slot-name (name prefix)
    "Translate NAME into a slot name that is unlikely
     to collide with inherited slot names by prepending
     PREFIX. If NAME is 'children', the prefix is not
     attached."
    (cond
      ((string= name :children)
       (format-symbol 'sel/sw/ts "~a" name))
      (t (format-symbol 'sel/sw/ts "~a-~a" prefix name))))

  ;; NOTE: while a :child-order annotation is currently being generated
  ;;       for every ast converted from a string, having the slot order
  ;;       is useful for converting from a list where the :child-order
  ;;       annotation would need to be generated and slot order is likely
  ;;       already correct except in a few rare cases.
  (defun slot-order (name expected-fields grammar-rules
                     &aux dependencies fields
                       (expected-fields (mapcar #'car expected-fields)))
    "Return the slot order of the fields in the production specified
by NAME. If NIL is returned, there are either no fields or the order
of fields needs to be determined at parse-time."
    (labels ((add-dependency (preceding-fields field)
               "Add a dependency for each on each item
              in PRECEDING-FIELDS for field."
               ;; NOTE: this can potentially add duplicate dependencies
               ;;       though this likely isn't much of a problem.
               (mapc
                (lambda (preceding-field)
                  (unless (equal preceding-field field)
                    (push (list preceding-field field) dependencies)))
                preceding-fields))
             (add-field (name)
               "Add NAME to the list of used fields."
               ;; NOTE: avoid adding the same field more than once.
               ;;       This can occur with 'CHOICE' rules.
               (pushnew name fields :test #'equal))
             (handle-choice (rule &optional preceding-fields visited-rules)
               "Handle RULE as a 'CHOICE' rule."
               (remove-duplicates
                (iter
                  (for member in (aget :members rule))
                  (appending
                   (handle-rule member preceding-fields visited-rules)))
                :test #'equal))
             (handle-seq (rule &optional preceding-fields visited-rules)
               "Handle RULE as a 'SEQ' rule."
               (iter
                 (for member in (aget :members rule))
                 (for preceding
                      initially preceding-fields
                      then (or (handle-rule member preceding visited-rules)
                               preceding))
                 (finally (return preceding))))
             (handle-repeat (rule &optional preceding-fields visited-rules)
               "Handle RULE as a 'REPEAT' rule."
               ;; NOTE: perform twice to loop the ending field of the repeat
               ;;       back to the front of the repeat. This will create
               ;;       an inconsistency if one exists. Also note that
               ;;       a dependency of a field on itself is ignored.
               (iter
                 (repeat 3)
                 (for preceding
                      initially preceding-fields
                      then (or (handle-rule
                                (aget :content rule) preceding visited-rules)
                               preceding))
                 (finally (return preceding))))
             (handle-field (rule &optional preceding-fields
                            &aux (name (aget :name rule)))
               "Handle RULE as a 'FIELD' rule and add a dependency from
              the field to PRECEDING-FIELDS if it exists."
               (when (member (make-keyword (convert-name name)) expected-fields)
                 (add-field name)
                 (add-dependency preceding-fields name)
                 (list name)))
             (handle-rule (rule &optional preceding-fields visited-rules)
               "Handles dispatching RULE to its relevant rule handler."
               ;; NOTE: this will throw an error if the json schema for
               ;;       the grammar.json files has changed.
               (string-ecase (aget :type rule)
                 (("ALIAS" "BLANK" "IMMEDIATE_TOKEN" "TOKEN" "PATTERN" "STRING"))
                 ("CHOICE" (handle-choice rule preceding-fields visited-rules))
                 ("FIELD" (handle-field rule preceding-fields))
                 (("PREC" "PREC_DYNAMIC" "PREC_LEFT" "PREC_RIGHT")
                  ;; pass-through
                  (handle-rule
                   (aget :content rule) preceding-fields visited-rules))
                 (("REPEAT" "REPEAT1")
                  (handle-repeat rule preceding-fields visited-rules))
                 ("SEQ" (handle-seq rule preceding-fields visited-rules))
                 ("SYMBOL"
                  (let* ((name-string (aget :name rule))
                         (name (make-keyword (convert-name name-string))))
                    ;; NOTE: the rules starting with an #\_ are special
                    ;;       and are the only ones that should be considered
                    ;;       when searching for fields that may be down the line
                    ;;       in different rules.
                    (when-let ((name (and (eql #\_ (aref name-string 0))
                                          (not (member name visited-rules))
                                          (aget name grammar-rules))))
                      (handle-rule name
                                   preceding-fields
                                   (cons name visited-rules))))))))
      ;; NOTE: tree-sitter/cli/src/generate/grammar-schema.json
      ;;       The grammar schema contains information on the
      ;;       possible rule types.
      (let* ((name-keyword (make-keyword (convert-name name)))
             (name-rule (aget name-keyword grammar-rules)))
        (when name-rule
          (handle-rule name-rule nil (list name-keyword))
          (mapcar
           #'make-keyword
           (mapcar
            #'convert-name
            (handler-case (sort fields (toposort dependencies :test #'equal))
              (inconsistent-graph ()
                ;; NOTE: the order doesn't matter as a :child-order
                ;;       annotation will be used instead of it.
                ;;       This is only provided for #'sorted-children
                ;;       to use.
                fields))))))))

  ;; STRUCTURED TEXT
  ;;
  ;; structured-text uses the JSON files provided by tree-sitter to generate
  ;; methods that reconstruct what the source code should look like for an AST.
  ;;
  ;; The methods primarily operate on a "transformed" JSON rule and its
  ;; equivalent "pruned" rule. The transformed JSON rule is an alist
  ;; representation which has removed any precedence rules, inlined every
  ;; rule starting with an underscore that isn't a supertype, distributed fields
  ;; such that they are less ambiguous, added choices for ambiguous alias rules,
  ;; and added extra slots to store information between terminal tokens.
  ;; The pruned rule transforms the transformed JSON rule by turning it into a
  ;; list instead of an alist, and replaces any subtree that does not have a slot
  ;; usage with a nil.
  ;;
  ;; The structured-text class has 5 slots--before-text stores text that comes
  ;; before the node, after-text stores text that comes after the node,
  ;; 'text' stores any text that can vary between different instances
  ;; of a class, before-asts stores comment and error ASTs that occur before
  ;; the before-text, and after-asts stores comment and error ASTs that occur
  ;; after the after-text. The text slot will also contain the text for any
  ;; class that doesn't have any slot usages. This allows for classes, such as
  ;; primitive_type in C, that don't store anything but have variable text to
  ;; still produce something meaningful without needing to modify the rule for
  ;; the AST.
  ;;
  ;; There are two main generated methods: output-transformation and parse-order.
  ;; output-transformation accepts an AST and returns a list of strings and its
  ;; children. This format has been chosen so that indentation can be reinserted
  ;; later. parse-order returns a list of an AST's children in-order. It also
  ;; contains "bookmarks" that indicate which "CHOICE" branch was taken or
  ;; how many times a "REPEAT" should be taken.
  ;;
  ;; There are two parsers: children-parser and match-parsed-children.
  ;; children-parser is a hack and should probably be replaced by something
  ;; generated from a parser generator. It is the back-bone of parse-order
  ;; and is what actually assembles its return value. match-parsed-children
  ;; matches a rule against a parse tree returned by cl-tree-sitter:parse-string.
  ;;
  ;; To disambiguate between different choice branches that a rule can take,
  ;; several different 'CHOICE expansion' subclasses are generated. Each one of
  ;; these expands a different branch in a 'CHOICE' present in the rule such that
  ;; they have a unique output transformation. The subclasses have names
  ;; generated based on the base class with a number added to the end of it.
  ;;
  ;; NOTE: these are some notes on the functions that may or may not be
  ;;       up-to-date
  ;;
  ;; transform-json transforms a json-rule by removing any precedent rules and
  ;; inlining any rule that starts with an underscore.
  ;;
  ;; prune-rule-tree transforms a json list representation into an easier to
  ;; work with list and prunes any subtree that doesn't include something that
  ;; will go into a slot--these are either field or child rules
  ;;
  ;; collapse-rule-tree further transforms the rule tree by collapsing duplicate
  ;; rules on each other, such as a sequence nested inside a sequence, and removes
  ;; any nils from the rule tree. Ideally, none of the code generation should use
  ;; this representation and prefer the pruned representation instead. Since the
  ;; collapsed representation was used early on, some code needs refactored to
  ;; account for this.
  ;;
  ;; structured-rule-p checks whether a rule is "structured" which I've deemed as
  ;; a fancy way of saying that it's unlikely to be problematic reproducing the
  ;; source text of the class associated with this rule. This should be useful at
  ;; compile time so that we know immediately which problems are likely to be
  ;; problematic. Note that it won't find problems with information that should be
  ;; stored in its own class but is currently treated as interleaved text by
  ;; tree-sitter.
  ;;
  ;; expand-choice-branches expands all choice subtrees outside of repeats into
  ;; their own rules without the choice rule. This allows for what I'm referring to
  ;; as "choice expansion" subclassing which allows for different slot orderings to
  ;; be implicit based on the AST subclass. There are currently some major
  ;; performance issues with this function on some languages.
  ;;
  ;; generate-computed-text-method determines if an AST class is largely variable
  ;; text, such as identifiers or literals, and generates a predicate method that
  ;; indicates whether the node should be read in specially--the variable text is
  ;; stored when read in.
  ;;
  ;; children-parser is largely a hack at the moment. It takes a collapsed rule
  ;; tree that has converted its strings to relevant class and slot names and an
  ;; AST. It then traverses the rule and pulls the children from the relevant slot
  ;; as they are encountered in the rule. This function does not back track right
  ;; now, and based on a cursory look at the rules, I haven't seen an instance
  ;; where it would be needed, so I'm putting that off until it becomes a problem.
  ;;
  ;; get-json-subtree-string takes a subtree that doesn't contain any slot uses
  ;; and generates a string representation of it. This is used for reproducing
  ;; the interleaved text. It prioritizes "BLANK" rules with "CHOICE" branches
  ;; and doesn't take any "REPEAT" rules.
  (defun map-json (map-fun tree &key (traversal :postorder))
    "Maps MAP-FUN over TREE as if it were a list representation of JSON
and returns the result."
    (map-tree
     (lambda (subtree)
       ;; TODO: this appears to call map-fun with the cdr of :members
       ;;       which isn't an alist that is use-able.
       (cond
         ;; first five clauses filter out anything that can't
         ;; be used as an alist. This is assumed based on the
         ;; structure of the json read in. NOTE that we may get
         ;; a non-alist still, but it should prevent any errors
         ;; that may occur from using something like #'aget.
         ((atom subtree) subtree)
         ((atom (cdr subtree)) subtree)
         ((not (listp (car subtree))) subtree)
         ((eql (car subtree) :members) subtree)
         ((eql (car subtree) :content) subtree)
         ((funcall map-fun subtree))))
     tree :tag 'prune :traversal traversal))

  (defun walk-json (function tree &key (traversal :preorder))
    "Walks FUNCTION over TREE as if it were a list representation of JSON."
    (walk-tree
     (lambda (subtree)
       (cond
         ;; first five clauses filter out anything that can't
         ;; be used as an alist. This is assumed based on the
         ;; structure of the json read in. NOTE that we may get
         ;; a non-alist still, but it should prevent any errors
         ;; that may occur from using something like #'aget.
         ((atom subtree))
         ((atom (cdr subtree)))
         ((not (every 'consp subtree))) ; all list elements must be conses
                                        ; in alist
         ((funcall function subtree))))
     tree :tag 'prune :traversal traversal))

  (defun substitute-json-rules (language rules)
    "Update rules in RULES based on mappings found for LANGUAGE.

First patches (from `*tree-sitter-json-rule-patches*') are applied,
then whole substitutions (from
`*tree-sitter-json-rule-substitutions*')."
    ;; NOTE: this will become inefficient with a lot of rule
    ;;       substitutions.
    (labels ((patch->cons (patch)
               "Expand PATCH into a cons that can be passed to sublis."
               (ematch patch
                 ((lambda-list &key replace with)
                  (cons replace with))
                 ((lambda-list &key wrap with)
                  (cons wrap (subst wrap '_ with)))
                 ((lambda-list &key label as)
                  (patch->cons
                   `(:wrap ,label
                     :with ((:type . "FIELD")
                            (:name . ,(assure string as))
                            (:content . _)))))))
             (patch-rule (rule patches)
               (let ((alist (mapcar #'patch->cons patches)))
                 (sublis alist rule :test #'equal)))
             (patch-rules (rules)
               (let ((patches (aget-all (make-keyword language)
                                        *tree-sitter-json-rule-patches*)))
                 (iter (for (rule-type . rule) in rules)
                       (if-let ((relevant-patches (aget-all rule-type patches)))
                         (collect (cons rule-type
                                        (patch-rule rule relevant-patches))
                                  into patched)
                         (collect (cons rule-type rule) into unchanged))
                       ;; Preserve the property that the changed rules
                       ;; are moved to the front of the list.
                       (finally (return (nconc patched unchanged))))))
             (substitute-rules (rules)
               (let ((substitutions
                      (aget-all (make-keyword language)
                                *tree-sitter-json-rule-substitutions*)))
                 (reduce
                  (lambda (rules substitution)
                    (areplace (car substitution) (cdr substitution) rules))
                  substitutions :initial-value rules))))
      (patch-rules (substitute-rules rules))))

  (defun substitute-json-node-types (substitutions node-types)
    "Substitute types in NODE-TYPES based on mappings found for LANGUAGE."
    (let* ((node-types
            ;; NOTE: this will become inefficient with a lot of node
            ;;       type substitutions.
            (reduce
             (lambda (node-types substitution)
               (cons substitution (remove (cdar substitution) node-types
                                          :key #'cdar :test #'equal)))
             substitutions :initial-value (copy-list node-types)))
           (constraints
            (iter constraints
                  (for node-type in node-types)
                  (for type = (cdar node-type))
                  (when-let (subtypes (aget :subtypes node-type))
                    (iter (for subtype in subtypes)
                          (in constraints
                              (collect (list type (cdar subtype))))))))
           (ordering (toposort constraints :test #'equal)))
      (stable-sort node-types ordering :key #'cdar)))

  (defun add-aliased-rules
      (rules
       &aux (rule-name-set
             (alist-hash-table (mapcar (op (cons (car _) t))
                                       rules))))
    "Add rules for named types which are only used as aliases in RULES.
This is to prevent certain classes from being seen as terminal symbols."
    (labels ((superset-alias-p
                 (subset superset &aux (subtype (aget :type subset))
                                    (supertype (aget :type superset)))
               "RETURN T if SUPERSET is a superset of SUBSET."
               ;; NOTE: this only considers supersets that start with a "CHOICE".
               (cond
                 ((and (equal subtype "CHOICE") (equal subtype supertype))
                  (iter
                    (for branch in (aget :members subset))
                    (always
                     (find-if (op (equal _ branch)) (aget :members superset)))))
                 ((equal supertype "CHOICE")
                  (find-if (op (equal _ subset)) (aget :members superset)))))
             (collect-strictly-aliased-types
                 (&aux (alias->content (make-hash-table :test #'equal)))
               "Collects all named aliases that have a :VALUE which
                doesn't exist as a rule."
               (walk-json
                (lambda (alist &aux (content (aget :content alist)))
                  (when (and
                         (aget :named alist)
                         (equal "ALIAS" (aget :type alist))
                         (not
                          (gethash
                           (make-keyword (convert-name (aget :value alist)))
                           rule-name-set)))
                    (symbol-macrolet
                        ((hash-value (gethash (aget :value alist)
                                              alias->content)))
                      (cond
                        ((or (not hash-value)
                             (superset-alias-p hash-value content))
                         ;; add if it is the first time seeing this.
                         ;; NOTE: that it is updated if hash-value is a
                         ;;       subset or equal to content.
                         (setf hash-value content))
                        ;; do nothing when a superset is already stored.
                        ((superset-alias-p content hash-value))
                        ;; when there isn't a super set, a new choice needs
                        ;; created.
                        ;; NOTE: there's a possibility that the choice will be
                        ;;       misordered, but if there's the generated choice
                        ;;       is seen later in the rules, it will be updated
                        ;;       by the first clause to be in the order seen
                        ;;       in the rules.
                        (t
                         (let ((previous (if (equal (aget :type hash-value)
                                                    "CHOICE")
                                             (aget :members hash-value)
                                             (list hash-value)))
                               (current (if (equal (aget :type content)
                                                   "CHOICE")
                                            (aget :members content)
                                            (list content))))
                           (setf hash-value
                                 `((:type . "CHOICE")
                                   (:members
                                    ,@(filter-map
                                       (distinct :test #'equal)
                                       (append previous current)))))))))))
                rules)
               alias->content)
             (expand-alias-content (content)
               "Expands CONTENT into a rule."
               (map-json
                (lambda (alist)
                  ;;TODO: only interested in symbols. Shouldn't follow nested
                  ;;      aliases; just prune. Sould only expand once.
                  (if (aget :type alist)
                      (string-case (aget :type alist)
                        ;; TODO: check prune here. nil may be incorrect.
                        ("ALIAS"
                         (throw 'prune alist))
                        ("SYMBOL"
                         (aget (make-keyword (convert-name (aget :name alist)))
                               rules))
                        (t alist))
                      alist))
                content))
             (create-aliased-rules (alias->content)
               "Create rules for every alias in ALIAS->CONTENT."
               (maphash-return
                (lambda (name content)
                  (cons (make-keyword (convert-name name))
                        (expand-alias-content content)))
                alias->content)))
      (append rules (create-aliased-rules (collect-strictly-aliased-types)))))

  (defun combine-aliased-rules (rule-table)
    "Search each rule for aliases. When an alias is found, add a 'CHOICE'
around the relevant rule and have a choice branch for the original rule and
a branch for the alias. This should help get around several issues that come
up due to aliases."
    (labels ((ensure-choice (rule)
               "Ensure that RULE begins with a 'CHOICE'."
               (if (string= "CHOICE" (aget :type rule))
                   rule
                   `((:TYPE . "CHOICE") (:MEMBERS ,@rule))))
             (add-aliases (rule aliased-content)
               ;; NOTE: an error here indicates a case that hasn't
               ;;       been considered yet.
               ;; TODO: this is a hack. string-ecase should be used.
               (string-case (aget :type aliased-content)
                 (("BLANK" "STRING" "PATTERN")
                  `((:TYPE . "CHOICE")
                    (:MEMBERS
                     ,@(aget :members rule)
                     ,aliased-content)))
                 ("SYMBOL"
                  `((:TYPE . "CHOICE")
                    (:MEMBERS
                     ,@(aget :members rule)
                     ,@(gethash
                        (make-keyword
                         (convert-name (aget :name aliased-content)))
                        rule-table))))
                 ("CHOICE"
                  (reduce #'add-aliases (aget :members aliased-content)
                          :initial-value rule))))
             (update-rule
                 (rule-name aliased-content
                  &aux (rule-key (make-keyword (convert-name rule-name))))
               "Update the rule specified by RULE-NAME such that it considers
                ALIASED-CONTENT to be a valid, match-able state of the rule."
               (symbol-macrolet ((rule (gethash rule-key rule-table)))
                 (let ((result (add-aliases
                                (ensure-choice rule) aliased-content)))
                   ;; NOTE: this is working around using string-case above
                   ;;       and is purposefully avoiding certain alias content.
                   (unless (or (member nil result)
                               (null result))
                     (setf rule (list result)))))))
      (iter
        (for (nil rule) in-hashtable rule-table)
        (walk-json (lambda (subtree)
                     (when (and (string= "ALIAS" (aget :type subtree))
                                (aget :named subtree))
                       (update-rule
                        (aget :value subtree) (aget :content subtree))))
                   rule))
      rule-table))

  (defun insert-internal-ast-slots
      (language-prefix transformed-json-rule insert-paths
       class-name class-name->class-definition
       &key top-level-rule
       &aux (internal-asts-postfix -1))
    "Insert internal-asts slots into TRANSFORMED-JSON-RULE for each path in
INSERT-PATHS.
TOP-LEVEL-RULE indicates that TRANSFORMED-JSON-RULE is the rule that the parser
matches as the root of the AST. Internal AST slots are added around the rule when
it is true."
    ;; TODO: at some point, prevent the insertion of any internal-asts slots in
    ;;       in rules that represent computed text ASTs.
    (labels ((trim-paths (paths)
               "Remove the first number from every path."
               (mapcar #'cdr paths))
             (order-paths (paths)
               "Order paths such that they're assorted by the first number in
                their path."
               (sort (assort paths :key #'car) #'< :key #'caar))
             (generate-internal-asts-slot
                 (&aux (slot-name (format nil "INTERNAL-ASTS-~a"
                                          (incf internal-asts-postfix)))
                    (slot-symbol (format-symbol :sel/sw/ts "~a-~a"
                                                language-prefix slot-name)))
               "Generate a new internal-asts slot for the current rule and
                add the slot to the class definition."
               (add-slot-to-class-definition
                class-name
                class-name->class-definition
                `(,slot-symbol
                  :accessor ,slot-symbol
                  :initarg ,(make-keyword slot-symbol)
                  :initform nil)
                :add-to-child-slots t)
               `((:TYPE . "SLOT")
                 (:NAME . ,slot-name)))
             (add-top-level-slot (rule-tree)
               "Add a proceding internal AST slot for RULE-TREE.
                This is a special edge case."
               ;; NOTE: at some point, there may need to be a preceding
               ;;       slot too, but it will add more complexity to the
               ;;       implementation. There aren't any examples where
               ;;       it would be useful to do this at the moment.
               (if (equal (aget :type rule-tree) "SEQ")
                   `((:TYPE . "SEQ")
                     (:MEMBERS
                      ,@(aget :members rule-tree)
                      ,(generate-internal-asts-slot)))
                   `((:TYPE . "SEQ")
                     (:MEMBERS
                      ,rule-tree
                      ,(generate-internal-asts-slot)))))
             (add-preceding-internal-asts-field (subtree)
               "Add a preceding internal-asts slot to SUBTREE and return
                the result."
               `((:TYPE . "SEQ")
                 (:MEMBERS
                  ,(generate-internal-asts-slot)
                  ,subtree)))
             (add-succeeding-internal-asts-field (subtree)
               "Add a proceding internal-asts slot to SUBTREE and return
                the result."
               `((:TYPE . "SEQ")
                 (:MEMBERS
                  ,subtree
                  ,(generate-internal-asts-slot))))
             (add-surrounding-internal-asts-fields (subtree)
               "Add a surrounding internal-asts slot to SUBTREE and return
                the result."
               `((:TYPE . "SEQ")
                 (:MEMBERS
                  ,(generate-internal-asts-slot)
                  ,subtree
                  ,(generate-internal-asts-slot))))
             (handle-choice
                 (rule paths &aux (ordered-paths (order-paths paths)))
               "Handle RULE as a 'CHOICE' rule."
               `((:TYPE . "CHOICE")
                 (:MEMBERS
                  ,@(iter
                      (for i upfrom 0)
                      (for member in (aget :members rule))
                      (if (eql i (caaar ordered-paths))
                          (collect (handle-rule
                                    member (trim-paths (pop ordered-paths))))
                          (collect member))))))
             (handle-seq (rule paths &aux (ordered-paths (order-paths paths)))
               "Handle RULE as a 'SEQ' rule."
               `((:TYPE . "SEQ")
                 (:MEMBERS
                  ,@(iter
                      (for i upfrom 0)
                      (for member in (aget :members rule))
                      (if-let ((current-paths (and (eql i (caaar ordered-paths))
                                                   (pop ordered-paths))))
                        (cond
                          ((= 1 (length (car current-paths)))
                           (collect (generate-internal-asts-slot))
                           (collect member))
                          (t (collect (handle-rule
                                       member (trim-paths current-paths)))))
                        (collect member))))))
             (handle-repeat (rule paths)
               "Handle RULE as a 'REPEAT' rule."
               `((:TYPE . "REPEAT")
                 (:CONTENT
                  ,@(handle-rule (aget :CONTENT rule) (trim-paths paths)))))
             (handle-alias (rule paths)
               "Handle RULE as an 'ALIAS' rule."
               ;; NOTE: assume that internal-asts slots will only occur before this.
               (if (member nil paths)
                   (add-preceding-internal-asts-field rule)
                   rule))
             (handle-string (rule paths)
               "HANDLE RULE as a string."
               ;; TODO: handle-string is a workaround for an upstream bug that
               ;;       causes newline terminals to have a range which spans
               ;;       multiple newlines. Remove this and have string be
               ;;       considered a terminal once it is fixed. To check if it
               ;;       has been fixed, try #'cl-tree-sitter:parse-string with a
               ;;       c-preproc-include and check if the newline spans multiple
               ;;       newlines.
               (let ((newline-p (member '(newline) paths :test #'equal))
                     (preceding-internal-asts-p (member nil paths)))
                 (cond
                   ((and newline-p preceding-internal-asts-p)
                    (add-surrounding-internal-asts-fields rule))
                   (newline-p
                    (add-succeeding-internal-asts-field rule))
                   (preceding-internal-asts-p
                    (add-preceding-internal-asts-field rule))
                   (rule))))
             (handle-terminal (rule paths)
               "Handle RULE as a terminal."
               (if (member nil paths)
                   (add-preceding-internal-asts-field rule)
                   rule))
             (handle-rule (rule paths)
               "Handles dispatching RULE to its relevant rule handler."
               ;; NOTE: this will throw an error if the json schema for
               ;;       the grammar.json files has changed.
               (string-ecase (aget :type rule)
                 ("ALIAS" (handle-alias rule paths))
                 ("CHOICE" (handle-choice rule paths))
                 ("REPEAT" (handle-repeat rule paths))
                 ("SEQ" (handle-seq rule paths))
                 (("PATTERN" "TOKEN") (handle-terminal rule paths))
                 ("STRING" (handle-string rule paths)))))
      (let ((internal-asts-rule
              (if insert-paths
                  (handle-rule transformed-json-rule insert-paths)
                  transformed-json-rule)))
        (if top-level-rule
            (add-top-level-slot internal-asts-rule)
            internal-asts-rule))))

  (defun add-internal-ast-slots
      (language-prefix transformed-json-rule class-name class-name->class-definition
       &key top-level-rule
       &aux insert-paths in-field-flag*)
    "Return a modified version of TRANSFORMED-JSON-RULE with internal-asts slots
added in between consecutive terminal symbols.
TOP-LEVEL-RULE indicates that TRANSFORMED-JSON-RULE is the rule that the parser
matches as the root of the AST."
    (declare (special in-field-flag*))
    (labels ((handle-choice (rule path &optional preceding-terminal?)
               "Handle RULE as a 'CHOICE' rule."
               (iter
                 (for i upfrom 0)
                 (for member in (aget :members rule))
                 (thereis
                  (handle-rule member (cons i path) preceding-terminal?))))
             (handle-seq (rule path &optional preceding-terminal?)
               "Handle RULE as a 'SEQ' rule."
               (iter
                 (for i upfrom 0)
                 (for member in (aget :members rule))
                 (for preceding
                      initially preceding-terminal?
                      then (handle-rule member (cons i path) preceding))
                 (finally (return preceding))))
             (handle-repeat (rule path &optional preceding-terminal?)
               "Handle RULE as a 'REPEAT' rule."
               ;; NOTE: perform twice to loop the ending field of the repeat
               ;;       back to the front of the repeat. This will determine
               ;;       if a repeat will need a slot before or after certain
               ;;       branches.
               (iter
                 (repeat 3)
                 (for preceding
                      initially preceding-terminal?
                      then (or (handle-rule
                                (aget :content rule)
                                (cons 0 path)
                                preceding)
                               preceding))
                 ;; NOTE: take into account preceding-terminal? for cases
                 ;;       where the repeat is blank.
                 (finally (return (or preceding preceding-terminal?)))))
             (handle-alias (rule path &optional preceding-terminal?)
               "Handle RULE as an 'ALIAS' rule."
               (unless (aget :named rule)
                 (handle-terminal path preceding-terminal?)))
             (handle-terminal (path &optional preceding-terminal?)
               "Handle RULE as a terminal."
               ;; NOTE: when inside the content of a FIELD, a BLANK is the only
               ;;       thing that will still be considered a terminal.
               (when (and (not in-field-flag*) preceding-terminal?)
                 (push (reverse path) insert-paths))
               (not in-field-flag*))
             (handle-string (rule path &optional preceding-terminal?)
               "Handle RULE as a string."
               ;; TODO: handle-string is a workaround for an upstream bug that
               ;;       causes newline terminals to have a range which spans
               ;;       multiple newlines. Remove this and have string be
               ;;       considered a terminal once it is fixed. To check if it
               ;;       has been fixed, try #'cl-tree-sitter:parse-string with a
               ;;       c-preproc-include and check if the newline spans multiple
               ;;       newlines.
               (when (not in-field-flag*)
                 (cond-every
                   (preceding-terminal? (push (reverse path) insert-paths))
                   ((equal #.(fmt "~%") (aget :value rule))
                    (push (reverse (cons 'newline path)) insert-paths))))
               (not in-field-flag*))
             (handle-field (rule path &optional preceding-terminal?
                            &aux (in-field-flag* t))
               (declare (special in-field-flag*))
               (handle-rule
                (aget :content rule)
                (cons 0 path)
                preceding-terminal?))
             (handle-rule (rule path &optional preceding-terminal?
                           &aux (type (aget :type rule)))
               "Handles dispatching RULE to its relevant rule handler."
               ;; NOTE: this will throw an error if the json schema for
               ;;       the grammar.json files has changed.
               (when type
                 (string-ecase type
                   ("ALIAS" (handle-alias rule path preceding-terminal?))
                   ;; NOTE: immediate tokens shouldn't have internal-asts before them.
                   ;;       Blanks will be surrounded by internal-asts slots if it
                   ;;       is treated normally.
                   ("BLANK" t)
                   ("CHOICE" (handle-choice rule path preceding-terminal?))
                   ("FIELD" (handle-field rule path preceding-terminal?))
                   ("IMMEDIATE_TOKEN" (not in-field-flag*))
                   (("PATTERN" "TOKEN")
                    (handle-terminal path preceding-terminal?))
                   ("REPEAT" (handle-repeat rule path preceding-terminal?))
                   ("SEQ" (handle-seq rule path preceding-terminal?))
                   ("STRING" (handle-string rule path preceding-terminal?))
                   ("SYMBOL")))))
      (handle-rule (car transformed-json-rule) nil)
      (list
       (insert-internal-ast-slots
        language-prefix
        (car transformed-json-rule)
        insert-paths
        class-name
        class-name->class-definition
        :top-level-rule top-level-rule))))

  (defun transform-json-rule (rule grammar rule-key-stack)
    "Expand inline rules base on GRAMMAR and :repeat1's in RULE.
RULE-KEY-STACK is a stack of rules that have already been visited in the
recursive calls for this function. Returns a transformed version of RULE."
    (labels ((propagate-field (tree &aux (field-name (aget :name tree)))
               "Return a modified version of TREE such that its field is
                propagated to its relevant subtrees and remove the enclosing
                field."
               (map-json
                (lambda (subtree &aux (type (aget :type subtree)))
                  (if type
                    (string-case type
                      (("STRING" "SYMBOL" "ALIAS" "BLANK")
                       (throw 'prune `((:TYPE . "FIELD")
                                       (:NAME . ,field-name)
                                       (:CONTENT ,@subtree))))
                      (t subtree))
                    subtree))
                (aget :content tree)
                :traversal :preorder))
             (propagate-field-p (tree)
               "Return T if TREE is a field rule that should be replaced
                with a transformed version of its content that transfers
                the field rule to the relevant subtrees."
               (when (equal "FIELD" (aget :type tree))
                 (walk-json
                  (lambda (subtree)
                    ;; TODO: determine if anything else would cause this.
                    (when (member (aget :type subtree) '("REPEAT" "SEQ")
                                  :test #'equal)
                      (return-from propagate-field-p t)))
                  tree)))
             (propagate-and-collapse-fields (tree)
               "Return a modified version of TREE with every field that
                satisfies propagate-field-p removed and replaced with
                a propagated version of its content."
               (map-json
                (lambda (subtree)
                  (if (propagate-field-p subtree)
                      (throw 'prune (propagate-field subtree))
                      subtree))
                tree
                :traversal :preorder))
             (expand-repeat1s (tree)
               "Expand all :REPEAT1 rules in an equivalent :SEQ and :REPEAT."
               (map-json (lambda (alist)
                           (if-let ((content (and (equal (aget :type alist)
                                                         "REPEAT1")
                                                  (aget :content alist))))
                             `((:type . "SEQ")
                               (:members
                                ,content
                                ((:type . "REPEAT")
                                 (:content ,@content))))
                             alist))
                         tree))
             (expand-inline-rules
                 (tree &aux (inline-rules (aget :inline grammar)))
               "Expand all inline rules."
               (map-json
                (lambda (alist)
                  (let* ((name-string (aget :name alist))
                         (name-key (make-keyword (convert-name name-string)))
                         (inlinep (member name-string inline-rules
                                          :test #'equal)))
                    ;; NOTE: it appears that all "SYMBOL"s that start with
                    ;;       an underscore are inlined.
                    ;;       DON'T inline the supertype rules.
                    (cond-let result
                      ;; Not inlineable
                      ((not (and (equal (aget :type alist) "SYMBOL")
                                 (or
                                  (and (eql #\_ (aref name-string 0))
                                       ;; Prevent infinite recursion on
                                       ;; rules already expanded.
                                       (not (member name-key rule-key-stack)))
                                  ;; Python has one inline rule without
                                  ;; an underscore.
                                  inlinep)
                                 ;; Don't inline supertypes unless they are
                                 ;; explicitly inlined.
                                 (or (not (member name-string
                                              (aget :supertypes grammar)
                                                  :test #'equal))
                                     inlinep)))
                       alist)
                      ((aget name-key (aget :rules grammar))
                       ;; Transform it again before inlining.
                       (transform-json-rule
                        result grammar (cons name-key rule-key-stack)))
                      ((member name-string (aget :externals grammar)
                               :test #'equal
                               :key {aget :name})
                       ;; Remove it from consideration.
                       `((:type . "BLANK")))
                      (t alist))))
                tree))
             (remove-prec-rules (tree)
               "Removes all precedent rules from TREE, replacing them with
                their content."
               (map-json (lambda (alist)
                           (if (member (aget :type alist)
                                       '("PREC" "PREC_DYNAMIC"
                                         "PREC_LEFT" "PREC_RIGHT")
                                       :test #'equal)
                               (aget :content alist)
                               alist))
                         tree))
             (branch-comparison-form (subtree)
               "Map SUBTREE to a 'branch comparison form'. This
                removes any types from 'SYMBOL' and 'FIELD' alists such that
                the types won't be considered for equality when an #'equal
                comparison is made. Named 'ALIAS' subtrees are also replaced
                with a 'SYMBOL' alist as they behave the same."
               ;; NOTE: it is unlikely that this will become a bottleneck, but
               ;;       a custom walk-tree in unison function could be written
               ;;       instead of remapping the subtree here.
               (map-json (lambda (alist)
                           (cond
                             ((equal (aget :type alist) "SYMBOL")
                              (adrop '(:name) alist))
                             ((equal (aget :type alist) "FIELD")
                              (adrop '(:content) alist))
                             ((and (equal (aget :type alist) "ALIAS")
                                   (aget :name alist))
                              `((:type . "SYMBOL")))
                             (t alist)))
                         subtree))
             (branch-similar-p (branch1 branch2)
               "Compare BRANCH1 with BRANCH2 and return T if they have
                identical branch comparison forms."
               (equal (branch-comparison-form branch1)
                      (branch-comparison-form branch2)))
             (collect-branch-types (branch &aux type-stack)
               "Collect the relevant type information from BRANCH
                that can be merged into a single branch."
               (walk-json
                (lambda (alist)
                  (cond
                    ((equal (aget :type alist) "SYMBOL")
                     (push (aget :name alist) type-stack))
                    ((equal (aget :type alist) "FIELD")
                     ;; NOTE: this should be expanded into a
                     ;;       choice inside the field content.
                     (push (aget :content alist) type-stack)
                     (throw 'prune nil))
                    ((and (equal (aget :type alist) "ALIAS")
                          (aget :named alist))
                     (push (aget :value alist) type-stack)
                     (throw 'prune nil))
                    (t alist)))
                branch :traversal :preorder)
               (reverse type-stack))
             (merge-choice-branch (similar-branches)
               "Merge SIMILAR-BRANCHES into one branch by
                adding multiple types onto the end of the relevant
                cons in the alist."
               (if (< 1 (length similar-branches))
                   (let ((types-stack
                           ;; NOTE: creating a stack of types that can
                           ;;       be used to markup the initial branch
                           ;;       while it mapped.
                           (apply
                            {mapcar #'list}
                            (mapcar #'collect-branch-types
                                    (cdr similar-branches)))))
                     (map-json
                      (lambda (alist)
                        (cond
                          ((equal (aget :type alist) "SYMBOL")
                           (throw 'prune
                             (areplace
                              :name
                              (append (ensure-cons (aget :name alist))
                                      ;; This flatten is a bit of a hack.
                                      (flatten (pop types-stack)))
                              alist)))
                          ((equal (aget :type alist) "FIELD")
                           ;; NOTE: this should be expanded into a
                           ;;       choice inside the field content.
                           (throw 'prune
                             (areplace
                              :content
                              ;; TODO: don't create a new CHOICE every time,
                              ;;       only create it if it is needed.
                              `((:type . "CHOICE")
                                (:members
                                 ,(aget :content alist)
                                 ,@(pop types-stack)))
                              alist)))
                          ((and (equal (aget :type alist) "ALIAS")
                                (aget :named alist))
                           (throw 'prune
                             (areplace
                              :value
                              (append (ensure-cons (aget :value alist))
                                      ;; This flatten is a bit of a hack.
                                      (flatten (pop types-stack)))
                              alist)))
                          (t alist)))
                      (car similar-branches)
                      :traversal :preorder))
                   ;; Just return the only branch.
                   (car similar-branches)))
             (merge-choice-branches (subtree)
               "Return SUBTREE with its choice branches merged."
               (areplace
                :members
                (mapcar #'merge-choice-branch
                        (assort (aget :members subtree)
                                :test #'branch-similar-p))
                subtree))
             (merge-similar-choice-branches (tree)
               "Merge choice branches with identical output transformations in
                TREE."
               (map-json
                (lambda (alist)
                  (if (equal "CHOICE" (aget :type alist))
                      (merge-choice-branches alist)
                      alist))
                tree)))
      (propagate-and-collapse-fields
       (merge-similar-choice-branches
        (expand-repeat1s
         (remove-prec-rules
          (expand-inline-rules rule)))))))

  (defun substitute-field-symbols
      (json-rule class-name language-prefix symbol-substitutions
       class-name->class-definition class-name->parse-tree-transforms)
    "Substitute symbols in TRANSFORMED-JSON that should be treated as fields.
A slot is added to the relevant definition in CLASS-NAME->CLASS-DEFINITION and
the list of parse tree transforms is updated for the relevant class in
CLASS-NAME->PARSE-TREE-TRANSFORMS."
    (labels ((update-class-definition
                 (symbol-substitution
                  &aux (slot-name (format-symbol
                                   'sel/sw/ts "~a-~a"
                                   language-prefix
                                   (convert-name (getf symbol-substitution
                                                       :slot-name)))))
               "Update class-name->class-definition with information from
                SYMBOL-SUBSTITUTION."
               ;; TODO: add slot name to symbols that need exported.
               (add-slot-to-class-definition
                (format-symbol 'sel/sw/ts "~a-~a" language-prefix class-name)
                class-name->class-definition
                `(,slot-name :accessor ,slot-name
                             :initarg ,(make-keyword slot-name)
                             :initform nil)
                :add-to-child-slots t))
             (update-class-transforms (symbol-substitution)
               "Update class-name->parse-tree-transforms with a cons of
                slot name and the function used for transforming the parse tree."
               (symbol-macrolet ((class-transforms
                                   (gethash class-name
                                            class-name->parse-tree-transforms)))
                 (setf class-transforms
                       (cons (getf symbol-substitution :transform)
                             class-transforms))))
             (encapsulate-symbol (subtree symbol-substitution)
               "Encapsulates SUBTREE with a field and updates the relevant
                hash tables."
               ;; Some rules don't have a corresponding class if they are
               ;; aliased.
               (when (update-class-definition symbol-substitution)
                 (update-class-transforms symbol-substitution))
               `((:TYPE . "FIELD")
                 (:NAME . ,(getf symbol-substitution :slot-name))
                 (:CONTENT
                  ,@subtree))))
      (if symbol-substitutions
          (map-json
           (lambda (subtree)
             (cond-let substitution
               ((not (member (aget :type subtree) '("SYMBOL" "ALIAS" "STRING")
                             :test #'equal))
                subtree)
               ((find-if
                 (lambda (symbol-substitution
                          &aux (symbol-name (or (aget :name subtree)
                                                (aget :value subtree))))
                   (member symbol-name symbol-substitution :test #'equal))
                 symbol-substitutions :key (op (getf _ :symbol-names)))
                ;; Check if the filter function passes.
                (if (funcall (getf substitution :predicate)
                             class-name json-rule subtree)
                    (encapsulate-symbol subtree substitution)
                    subtree))
               (t subtree)))
           json-rule)
          json-rule)))

  (defun prune-rule-tree (transformed-json-rule)
    (labels ((gather-field-types (content)
               "Return a list of symbols that CONTENT could be for a field."
               ;; TODO: this WHEN check should not be necessary
               (when (listp content) ; ignore if not a list
                 ;; TODO: remove duplicates from the gathered field types.
                 (remove-duplicates
                  (string-case (aget :type content)
                    ;; TODO: may need to add more things here.
                    ;;       It also might make sense to use walk-json here
                    ;;       since it wasn't available when this was originally
                    ;;       written.
                    ("STRING"
                     (list (aget :value content)))
                    ("SYMBOL"
                     (list (aget :name content)))
                    ("ALIAS"
                     (list (aget :value content)))
                    (("CHOICE" "SEQ")
                     (mappend #'gather-field-types (aget :members content)))
                    ("REPEAT"
                     (gather-field-types (aget :content content)))
                    ("BLANK"
                     (list 'null)))
                  :test #'equal)))
             (handle-seq (rule)
               "Handle RULE as a 'SEQ', 'REPEAT', 'REPEAT1', or 'CHOICE' rule."
               (let ((children (if-let ((members (aget :members rule)))
                                 ;; seq and choice
                                 (mapcar #'handle-rule members)
                                 ;; repeat and repeat1
                                 (list (handle-rule (aget :content rule))))))
                 (unless (every #'null children)
                   (cons (make-keyword (aget :type rule)) children))))
             (handle-field (rule)
               "Handle RULE as a 'FIELD' rule."
               `(:field
                 ,(aget :name rule)
                 ,@(flatten (gather-field-types (aget :content rule)))))
             (handle-symbol (rule)
               "Handle RULE as a 'SYMBOL' rule. This checks if
                it is part of the AST's children."
               `(:child ,@(ensure-cons (aget :name rule))))
             (handle-alias (rule)
               ;; NOTE: this assumes that all named aliases go into
               ;;       the children slot.
               (when (aget :named rule)
                 `(:child ,@(ensure-cons (aget :value rule)))))
             (handle-slot (rule)
               "Handle RULE as a 'SLOT' rule. Note that slot rules
                are not part of tree-sitter and are added by
                add-internal-ast-slots."
               `(:slot ,@(ensure-cons (aget :name rule))))
             (handle-rule (rule)
               "Handles dispatching RULE to its relevant rule handler."
               ;; NOTE: this will throw an error if the json schema for
               ;;       the grammar.json files has changed.
               ;; TODO: this WHEN check of the rule should not be necessary
               (when rule
                   (string-ecase (aget :type rule)
                     ("ALIAS" (handle-alias rule))
                     (("BLANK" "IMMEDIATE_TOKEN" "TOKEN"
                               "PATTERN" "STRING"))
                     (("CHOICE" "SEQ" "REPEAT" "REPEAT1")
                      (handle-seq rule))
                     ("FIELD" (handle-field rule))
                     ("SLOT" (handle-slot rule))
                     ("SYMBOL"
                      ;; NOTE: this assumes that all 'SYMBOL's that are seen
                      ;;       going into the children slot. Also NOTE that
                      ;;       the inline rules should be inlined before entering
                      ;;       this function.
                      (handle-symbol rule))))))
      (if transformed-json-rule ; watch for null rule
          (handle-rule transformed-json-rule))))

  ;; TODO: update doc string
  (defun collapse-rule-tree (rule-tree)
    "Return a new version of TREE that removes empty subtrees,
expands :REPEAT1 rules, and collapses contiguous, nested
:SEQ rules."
    ;; NOTE: the collapsed representation makes analysis much easier
    ;;       especially when dealing with nested choices.
    (labels ((collapse-on (tree value)
               "Collapse subtrees of TREE that start with VALUE."
               (cond
                 ((atom tree) tree)
                 ((eql (car tree) value)
                  (iter
                    (for child in tree)
                    (let ((result (collapse-on child value)))
                      (cond
                        ((atom result) (collect result))
                        ((eql (car result) value)
                         (mapcar (lambda (child) (collect child)) (cdr result)))
                        (t (collect result))))))
                 (t (mapcar {collapse-on _ value} tree))))
             (collapse-sequences (tree)
               "Collapse nested sequences into a single sequence."
               (collapse-on tree :seq))
             (collapse-choices (tree)
               "Collapse nested choices into a single choice."
               (collapse-on tree :choice))
             (prune-empty-subtrees (tree)
               "Remove subtrees without :field's or :child's."
               (prune-if (lambda (node)
                           (or (null node)
                               (and (listp node)
                                    (= (length node) 1))))
                         tree)))
      (collapse-sequences
       (collapse-choices
        (prune-empty-subtrees rule-tree)))))

  (defun collect-rule-tree (predicate tree &aux accumulator)
    "Collect every subtree in TREE that satisfies PREDICATE."
    (walk-tree
     (lambda (subtree)
       (when (and (listp subtree)
                  (funcall predicate subtree))
         (push subtree accumulator)))
     tree)
    accumulator)

  (defun collect-rule-slots (tree &key include-slots)
    "Collect all slots used in TREE."
    (let ((slot-identifiers `(:field :child ,@(when include-slots '(:slot)))))
      (collect-rule-tree
       (lambda (subtree)
         (member (car subtree) slot-identifiers))
       tree)))

  ;; TODO: at some point, this may need to treat interal-asts slots as fields
  ;;       when considering whether they are problematic.
  (defun structured-rule-p (collapsed-rule pruned-rule)
    ;; NOTE: this will only operate on fields and children.
    ;;       There may be potential for interleaved text to have their own issues,
    ;;       but if they don't have their own field then it's an issue with the
    ;;       parser.

    ;; TODO: At some point, maybe consider variations on the strings
    ;;       (non-slot fields) too, but it might not be reasonable (or it's
    ;;       difficult with repeats) to do this?
    (labels ((collect-repeats (tree)
               "Collect all repeat subtrees in TREE."
               (collect-rule-tree
                (lambda (subtree)
                  ;; NOTE: :repeat1's should be expanded at this point.
                  (eql (car subtree) :repeat))
                tree))
             (collect-choices (tree)
               "Collect all choice subtrees in TREE."
               (collect-rule-tree
                (lambda (subtree)
                  (eql (car subtree) :choice))
                tree))
             (collect-branch-slots (choice)
               "Collect the slots in each branch of CHOICE."
               (iter
                 (for branch in (cdr choice))
                 (when-let ((slots (collect-rule-slots branch)))
                   (collect slots))))
             (incompatible-choice-p (branch-slots-1 branch-slots-2)
               "Compare BRANCH-SLOTS-1 to BRANCH-SLOTS-2 and return T
                if they are 'incompatible'."
               ;; NOTE: :child pairs will contain a type string in the
               ;;        #'cadr position that doesn't matter since it
               ;;        will go into the same slot, so the :child types
               ;;        are stripped here before comparison.
               ;; TODO: it may be the case that a :choice with an empty
               ;;       branch is problematic too or we can assume the
               ;;       empty branch isn't interesting when reproducing the
               ;;       text.
               (let* ((slots-1 (mapcar (lambda (slot-pair)
                                         (if (eql (car slot-pair) :child)
                                             :child
                                             (cadr slot-pair)))
                                       branch-slots-1))
                      (slots-2 (mapcar (lambda (slot-pair)
                                         (if (eql (car slot-pair) :child)
                                             :child
                                             (cadr slot-pair)))
                                       branch-slots-2))
                      (comparison-func {equal (car slots-1)}))
                 (not
                  (if (every comparison-func slots-1)
                      ;; NOTE: all the same. This covers the case where
                      ;;       there's just one slot usage.
                      (every comparison-func slots-2)
                      ;; NOTE: this maintains the same order otherwise.
                      (equal slots-1 slots-2)))))
             (incompatible-choice-in-repeat-p (rule)
               "Return T if there is an incompatible choice in a repeat."
               ;; NOTE: this should only consider a choice in a repeat problematic
               ;;       if the choice has two separate slots in two separate
               ;;       branches, there are multiple slots in a different order,
               ;;       or a branch has a slot while another only has terminals.
               ;;       This causes an issue trying to reproduce the ordering.
               (iter
                 (for choice in (mappend #'collect-choices (collect-repeats rule)))
                 (for slots-in-branches = (collect-branch-slots choice))
                 ;; NOTE: compare all branches to the first branch. If one
                 ;;       matches the first, then it should be the same as
                 ;;       comparing to the first again.
                 (thereis (or
                           (and slots-in-branches (some #'null choice))
                           (some {incompatible-choice-p (car slots-in-branches)}
                                 slots-in-branches)))))
             (use-after-repeat-p (tree &key in-repeat? repeat-alist)
               "Return non-NIL if TREE contains a problematic usage of a slot after
                a repeat on that slot."
               ;; NOTE: it isn't strictly the case that a use after repeat will
               ;;       be problematic; it can be handled by counting the number of
               ;;       uses after the repeat that are needed for a specific type.
               ;;       It is problematic any time a slot occurs in two separate
               ;;       repeats.
               (when (listp tree)
                 (case (car tree)
                   (:seq
                    (iter
                      (for subtree in tree)
                      (for (values subtree-repeat-alist use-after?)
                           first (values repeat-alist)
                           then (use-after-repeat-p
                                 subtree :in-repeat? in-repeat?
                                 :repeat-alist subtree-repeat-alist))
                      (when use-after?
                        (leave (values nil use-after?)))
                      (finally
                       (return (values subtree-repeat-alist)))))
                   (:repeat
                    (iter
                      (for subtree in tree)
                      (for (values subtree-repeat-alist use-after?)
                           first (values repeat-alist)
                           then (use-after-repeat-p
                                 subtree :in-repeat? tree
                                 :repeat-alist subtree-repeat-alist))
                      (when use-after?
                        (leave (values nil use-after?)))
                      (finally
                       (return (values subtree-repeat-alist)))))
                   (:choice
                    (iter
                      (for subtree in tree)
                      (for (values subtree-repeat-alist use-after?)
                           = (use-after-repeat-p
                              subtree :in-repeat? in-repeat?
                              :repeat-alist repeat-alist))
                      (if use-after?
                          (leave (values nil use-after?))
                          (collect subtree-repeat-alist into choice-alists))
                      (finally
                       ;; NOTE: if we're in a repeat, there could be a use after
                       ;;       a repeat in separate choice branches, but the check
                       ;;       for this would have already been done in
                       ;;       incompatible-choice-in-repeat-p, so don't make the
                       ;;       same check again.
                       (return
                         (remove-duplicates
                          (reduce #'append choice-alists)
                          :key #'car)))))
                   ((:field :child)
                    (let ((types
                            (if (eql (car tree) :field)
                                (cddr tree)
                                (cdr tree))))
                      (cond-let result
                        ((some (lambda (type)
                                 (aget type repeat-alist :test #'equal))
                               types)
                         (values nil result))
                        (in-repeat?
                         (append (mapcar {cons _ in-repeat?} types)
                                 repeat-alist))
                        (t repeat-alist))))))))
      (not (or (incompatible-choice-in-repeat-p pruned-rule)
               (nth-value 1 (use-after-repeat-p collapsed-rule))))))

  (defun expand-choice-branches (pruned-rule transformed-json)
    "Expand the choice branches the PRUNED-RULE for NODE-TYPE has
outside of repeats."
    (labels ((get-path (tree path)
               "Get the subtree in TREE at PATH."
               ;; This will blowup if a bad path is given to it.
               (if (listp path)
                   (reduce (lambda (tree position) (nth position tree)) path
                           :initial-value tree)
                   tree))
             (replace-at (list index value)
               "Replace the value at INDEX in list, copying as little as possible."
               (let ((tail (nthcdr index list)))
                 (if (eql value (first tail))
                     list
                     (nconc (ldiff list tail)
                            (list value)
                            (rest tail)))))
             (replace-at-path (tree path value)
               (if (endp path) value
                   (replace-at tree (first path)
                               (replace-at-path (nth (first path) tree)
                                                (rest path)
                                                value))))
             (get-json-path (json-tree path)
               "Get the subtree in JSON-TREE at path."
               (if (listp path)
                   (reduce (lambda (json-tree position)
                             (nth (1- position) (or (aget :members json-tree)
                                                    (aget :content json-tree))))
                           path
                           :initial-value json-tree)
                   json-tree))
             (replace-at-json-path (json-tree path value)
               (if (endp path) value
                   (let* ((entry
                           (or (assoc :members json-tree)
                               (assoc :content json-tree)))
                          (new-cdr
                           (replace-at (cdr entry)
                                       (1- (car path))
                                       (replace-at-json-path
                                        (nth (1- (car path))
                                             (cdr entry))
                                        (cdr path)
                                        value)))
                          (new-entry
                           (reuse-cons (car entry) new-cdr entry)))
                     (if (eql entry new-entry) json-tree
                         (substitute new-entry entry json-tree :count 1)))))
             (get-leaf-choice (tree &optional (path nil))
               "Gets the first 'CHOICE' in TREE that doesn't have any nested
               'CHOICE's within it."
               (cond
                 ((not (listp tree)) nil)
                 ;; exclude choices in repeats from consideration.
                 ((eql (car tree) :repeat) nil)
                 (t
                  (iter
                    (for child in tree)
                    (for i upfrom 0)
                    (collect (get-leaf-choice child (cons i path))
                      into nested-choices)
                    (finally (return (or (find-if-not #'null nested-choices)
                                         (and (eql (car tree) :choice)
                                              (reverse path)))))))))
             (expand-choice (tree path)
               "Expands the 'CHOICE' at PATH in TREE into several different trees."
               (let ((choice (get-path tree path)))
                 (assert (eql (car choice) :choice))
                 (if path
                     (mapcar (op (replace-at-path tree path _))
                             (cdr choice))
                     (cdr choice))))
             (expand-json-choice (json-tree path)
               "Expands the 'CHOICE' at PATH in JSON-TREE into several different
                trees."
               (let ((choice (get-json-path json-tree path)))
                 (assert (equal (aget :type choice) "CHOICE"))
                 (if path
                     (mapcar (op (replace-at-json-path json-tree path _))
                             (aget :members choice))
                     (aget :members choice))))
             (remove-duplicate-rules (pruned-branches json-branches)
               "Remove the duplicates from PRUNED-BRANCHES while also
                removing the 'same' item from JSON-BRANCHES. The two
                modified lists are returned as values."
               (let ((removed-duplicates
                      (filter-map (distinct :key #'car :test #'equal)
                                  (mapcar #'cons pruned-branches json-branches))))
                 (values
                  (mapcar #'car removed-duplicates)
                  (mapcar #'cdr removed-duplicates))))
             (expand-choices (tree json-tree)
               "Expand all nested choices into their own branches."
               (if (eql (car tree) :choice)
                   (values (expand-choice tree nil)
                           (expand-json-choice json-tree nil))
                   (iter
                     (for expansion-stack initially (list tree)
                          then (mappend (lambda (expansion)
                                          (expand-choice expansion choice-path))
                                        expansion-stack))
                     (for json-expansion-stack initially (list json-tree)
                          then (mappend
                                (lambda (expansion)
                                  (expand-json-choice expansion choice-path))
                                json-expansion-stack))
                     (for choice-path = (get-leaf-choice (car expansion-stack)))
                     (while choice-path)
                     (finally
                      (return (remove-duplicate-rules
                               expansion-stack json-expansion-stack)))))))
      (expand-choices pruned-rule transformed-json)))

  (defun computed-text-ast-p (language class-name json-rule &aux children?)
    "Return T if JSON-RULE and TYPE represent an AST that contains variable text."
    ;; TODO: update this to be more intelligent and not mark everything without
    ;;       children as a computed-text node.
    (or
     (member class-name
             (aget (make-keyword language) *tree-sitter-computed-text-asts*))
     (walk-json
      (lambda (subtree)
        ;(format t "subtree: ~A~%" subtree)
        ;; TODO: maybe also look at node types to see if it has any children
        ;;       default to computed text node p if it doesn't?
        (when-let (type (and (listp subtree) (aget :type subtree)))
          ;; TODO: this WHEN check shouldn't be necessary as type should always
          ;; be a string if it is found at all.
          (when (stringp type)
              (string-case type
                ;; TODO: figure out a way to remove the token rules from this.
                (("PATTERN" "TOKEN" "IMMEDIATE_TOKEN")
                 ;; PATTERN indicates that there
                 ;; is variable text.
                 ;; TOKEN and IMMEDIATE_TOKEN don't
                 ;; necessarily indicate variable text,
                 ;; but they generally have a CHOICE that
                 ;; selects from some default values, and
                 ;; since this isn't stored in a slot, it's
                 ;; generally a good idea to store it since
                 ;; things break otherwise. An example of this
                 ;; can be seen with language-provided types in
                 ;; C.
                 (return-from computed-text-ast-p t))
                ("ALIAS"
                 (if (and (listp subtree) (aget :named subtree))
                     ;; A named alias should be treated like a symbol.
                     (setf children? t)
                     ;; Aliases can have patterns in them,
                     ;; but they are recast to something else,
                     ;; so they do not need to be stored.
                     (throw 'prune nil)))
                (("FIELD" "SYMBOL" "SLOT")
                 ;; The presence of any of these indicate that there are
                 ;; children.
                 (setf children? t))))))
      json-rule)
     (not children?)))

  ;;; TODO: clean up everything that calls this. This no longer generates
  ;;;       anything but instead adds a superclass to a relevant class
  ;;;       definition. All of the callsites should be updated to reflect
  ;;;       this, and it should be renamed.
  ;;;
  ;;;       This was created before the class-name->class-definition hash table
  ;;;       was available.
  (defun generate-computed-text-method
      (transformed-json-rule class-name language class-name->class-definition
       &key skip-checking-json)
    "Generate an input transformation method for RULE if one is needed.
CLASS-NAME is used as the specialization for the generated method.
The definition of CLASS-NAME is updated to include the computed-text mixin."
    (when (or skip-checking-json
              (computed-text-ast-p language class-name transformed-json-rule))
      (add-superclass-to-class-definition
       class-name class-name->class-definition 'computed-text)
      nil))


  (defun add-slot-to-class-definition
      (class-name class-name->class-definition slot-spec &key add-to-child-slots)
    "Destructively add SLOT-SPEC to CLASS-NAME's definition in
CLASS-NAME->CLASS-DEFINITION. Return NIL on failure and non-NIL on success."
    (labels ((update-child-slots (slots)
               "Update SLOTS such that child-slots contains the new
                slot-spec."
               (let* ((child-slots (assoc 'child-slots slots))
                      (slot-list (lastcar (getf (cdr child-slots) :initform))))
                 (acons
                  'child-slots
                  `(:initform
                    ',(append (butlast slot-list)
                              ;; Assume arity of 0.
                              `((,(car slot-spec) . 0))
                              (last slot-list))
                    :allocation :class)
                  (remove child-slots slots)))))
      (when-let ((class-definition (gethash class-name class-name->class-definition)))
        (symbol-macrolet ((slots (cadddr class-definition)))
          (unless (aget (car slot-spec) slots)
            (setf slots
                  (cons slot-spec
                        (if add-to-child-slots
                            (update-child-slots slots)
                            slots))))))))

  (defun add-superclass-to-class-definition
      (class-name class-name->class-definition superclass)
    "Destructively add SUPERCLASS to CLASS-NAME's definition in
CLASS-NAME->CLASS-DEFINITION."
    (when-let ((class-definition (gethash class-name class-name->class-definition)))
      (symbol-macrolet ((supers (caddr class-definition)))
        (pushnew superclass supers))))

  (defun generate-children-method
      (pruned-rule json-rule class-name class-name->class-definition)
    ;; TODO: rename this method now that it doesn't generate any methods.
    "Add slots for PRUNED-RULE and JSON-RULE to the definition of CLASS-NAME in
CLASS-NAME->CLASS-DEFINITION."
    (mapc {add-slot-to-class-definition
           class-name class-name->class-definition}
          `((pruned-rule
             :accessor pruned-rule
             :initform ',pruned-rule
             :allocation :class
             :documentation
             "A rule used to order the children of this class.")
            (json-rule
             :initform ',json-rule
             :reader json-rule
             :allocation :class
             :documentation
             "A rule used to determine where inner ASTs are assigned."))))

  (defun get-json-subtree-string (json-subtree choice-resolver)
    "Get the string representation of JSON-SUBTREE. This assumes that there aren't
any slot usages in JSON-SUBTREE."
    ;; NOTE: assume that token, immediate_token, and pattern automatically
    ;;       mean that a node has variable text. Thus, if any subtree has one of
    ;;       these rules, the AST itself should be printed specially using the
    ;;       computed-text ('text') slot.
    (labels ((handle-alias (rule)
               ;; NOTE: only consider unnamed nodes unless it becomes problematic
               ;;       to not be considering the named ones.
               (if (aget :named rule)
                   ;; TODO: remove this before committing?
                   ;;
                   ;; If we reach here, we may have a problem with
                   ;; pruning rules?
                   (error "unhandled named alias")
                   (aget :value rule)))
             (handle-blank ()
               "")
             (handle-choice (rule &aux (branches (aget :members rule)))
               ;; Prefer blank branches
               (cond
                 (choice-resolver
                  (rule-handler (or (funcall choice-resolver branches)
                                    (car branches))))
                 (t (rule-handler (or (find-if
                                       (lambda (branch)
                                         (equal "BLANK" (aget :type branch)))
                                       branches)
                                      (car branches))))))
             (handle-repeat ()
               "")
             (handle-seq (rule)
               (mapcar #'rule-handler (aget :members rule)))
             (handle-string (rule)
               (aget :value rule))
             (rule-handler (rule)
               "Handles dispatching RULE to its relevant rule handler."
               ;; NOTE: this will throw an error if an unexpected rule is
               ;;       encountered.
               (string-ecase (aget :type rule)
                 ("ALIAS" (handle-alias rule))
                 ("BLANK" (handle-blank))
                 ("CHOICE" (handle-choice rule))
                 ("REPEAT" (handle-repeat))
                 ("SEQ" (handle-seq rule))
                 ("SLOT" "")
                 ("STRING" (handle-string rule)))))
      (apply #'string+ (flatten (rule-handler json-subtree)))))

  (defun generate-output-transformation
      (pruned-rule transformed-json-rule language class-name
       class-name->class-definition choice-resolver)
    (labels ((generate-choice (json-rule pruned-rule)
               "Generate a quoted form which handles choices."
               ;; NOTE: coming in, the in-order children stack should have a
               ;;       list as its top item. This will be of the form
               ;;       (:choice branch-number). The correct branch can be chosen
               ;;       based on branch number.
               `(ecase (cadr (pop parse-stack))
                  ,@(iter
                      (for branch in (aget :members json-rule))
                      (for pruned-branch in (cdr pruned-rule))
                      (for i upfrom 0)
                      (collect
                          (list
                           i (generate-rule-handler branch pruned-branch))))))
             (generate-field ()
               "Generate a quoted form which handles fields."
               `(pop parse-stack))
             (generate-repeat (json-rule pruned-rule)
               "Generate a quoted form which handles repeats."
               `(iter
                  (ecase (car (pop parse-stack))
                    ((:repeat :continue)
                     (collect ,(generate-rule-handler (aget :content json-rule)
                                                      (cadr pruned-rule))))
                    (:end-repeat (finish)))))
             (generate-seq (json-rule pruned-rule)
               "Generate a quoted form which handles sequences."
               `(list
                 ,@(mapcar #'generate-rule-handler
                           (aget :members json-rule) (cdr pruned-rule))))
             (generate-symbol ()
               "Generate a quoted form which handles symbols."
               `(pop parse-stack))
             (generate-slot ()
               "Generate a quoted form which handles slots."
               ;; TODO: there may be some issues here.
               `(pop parse-stack))
             (generate-rule-handler (json-rule pruned-rule)
               "Handles dispatching JSON-RULE to its relevant rule handler."
               (if pruned-rule
                   ;; NOTE: this will throw an error if the json schema for
                   ;;       the grammar.json files has changed or the form of
                   ;;       the json-rule or pruned-rule change.
                   ;;       This can also throw an error if pruning trees is
                   ;;       incorrectly done.
                   (string-ecase (aget :type json-rule)
                     ("ALIAS" (generate-symbol))
                     ("CHOICE" (generate-choice json-rule pruned-rule))
                     ("REPEAT" (generate-repeat json-rule pruned-rule))
                     ("FIELD" (generate-field))
                     ("SEQ" (generate-seq json-rule pruned-rule))
                     ("SLOT" (generate-slot))
                     ("SYMBOL" (generate-symbol)))
                   (get-json-subtree-string json-rule choice-resolver)))
             (generate-ast-list (json-rule pruned-rule)
               "Generate the form that creates a list of ASTs and strings."
               (generate-rule-handler json-rule pruned-rule))
             (generate-body (transformed-json-rule pruned-rule)
               "Generate the body of the output transformation method."
               `(list
                 (before-text ast)
                 ;; Expand all rules here.
                 ,(generate-ast-list transformed-json-rule pruned-rule)
                 (after-text ast)))
             (generate-method (transformed-json-rule pruned-rule)
               "Generate the output transformation method."
               (cond
                 ((computed-text-ast-p
                   language class-name transformed-json-rule)
                  nil)
                 ((every #'null (cdr pruned-rule))
                  ;; This is the case where we have a string representation for
                  ;; the full thing.
                  (add-slot-to-class-definition
                   class-name class-name->class-definition
                   `(text ; computed-text
                     :accessor text
                     :initarg :text
                     :allocation :class
                     :initform
                     ',(get-json-subtree-string transformed-json-rule
                                                 choice-resolver)))
                  nil)
                 (t `(defmethod output-transformation
                       ((ast ,class-name) &rest rest &key &aux (parse-stack (parse-order ast)))
                     (declare (ignorable parse-stack rest))
                     (flatten ,(generate-body transformed-json-rule pruned-rule)))))))
      (generate-method transformed-json-rule pruned-rule)))

  (defun convert-to-lisp-type (prefix type-string)
    ;; TODO: this function is also used above. Refactor after rebasing with
    ;;       master.
    (format-symbol 'sel/sw/ts "~a-~a" prefix (convert-name type-string)))

  (defun generate-input/output-handling
      (pruned-rule json-rule superclass language-prefix child-types
       class-name->class-definition choice-resolver computed-text-ast?
       &key symbols-to-export
       &aux (subclass-counter -1)
         (subclasses
          (aget superclass
                (aget (make-keyword language-prefix)
                      *tree-sitter-choice-expansion-subclasses*))))
    "Generate a method for a type of AST that returns a choice expansion
subclass based on the order of the children were read in."
    (labels ((report-problematic-rule ()
               "Reports 'unstructured' rules to *error-output*."
               (unless (or computed-text-ast?
                           (structured-rule-p
                            (collapse-rule-tree pruned-rule) pruned-rule))
                 (format *error-output* "Problematic Rule: ~a~%" superclass)))
             (get-subclass-name (collapsed-rule)
               "Get the subclass name for COLLAPSED-RULE. If one isn't in
                subclasses, generate a new name."
               (assure (and symbol (not null))
                 (or
                  (lret ((name
                          (car (find-if (lambda (pair)
                                          (equal collapsed-rule (cadr pair)))
                                        subclasses))))
                    (when name
                      (ensure-gethash name symbols-to-export t)))
                  (format-symbol :sel/sw/ts "~a-~a"
                                 ;; If there are name collisions from the counter,
                                 ;; move back to the gensym-based approach.
                                 superclass (incf subclass-counter)))))
             (convert-to-lisp-types (rule)
               "Converts all strings in RULE to lisp types."
               (map-tree
                (lambda (node)
                  (if (typep node 'string)
                      (convert-to-lisp-type language-prefix node)
                      node))
                rule))
             (add-subclass-list-slot (subclass-pairs)
               "Add a slot to superclass which contains a list of subclasses
                based on SUBCLASS-PAIRS."
               (add-slot-to-class-definition
                superclass
                class-name->class-definition
                `(choice-subclasses
                  :initform ',(mapcar #'car subclass-pairs)
                  :reader choice-subclasses
                  :allocation :class)))
             (generate-subclass (subclass-pair
                                 &aux (class-name (car subclass-pair)))
               "Generate a defclass form for SUBCLASS-PAIR."
               ;; TODO: should also consider adding support for mixins
               ;;       and additional slot options.
               (push class-name
                     (gethash 'class-order class-name->class-definition))
               (setf (gethash class-name class-name->class-definition)
                     `(defclass ,class-name (,superclass)
                        ((rule
                          :initform ',(cadr subclass-pair)
                          :reader rule
                          :allocation :class)
                         (choice-superclass
                          :initform ',superclass
                          :reader choice-superclass
                          :allocation :class)
                         (choice-subclasses
                          :initform nil
                          :reader choice-subclasses
                          :allocation :class)))))
             (generate-subclasses (subclass-pairs)
               "Generate a defclass forms for SUBCLASS-PAIRS."
               ;; TODO: rename this function since it doesn't generate anything
               ;;       internal to this function anymmore.
               (map nil #'generate-subclass subclass-pairs))
             (generate-children-methods
                 (subclass-pairs json-expansions expansions?)
               "Generate the methods for handling children for
                every subclass pair in SUBCLASS-PAIRS."
               ;; TODO: rename this function since it doesn't generate anything
               ;;       anymore.
               (mapc
                (lambda (subclass-pair json-expansion)
                  (generate-children-method
                   (caddr subclass-pair)
                   json-expansion
                   (car subclass-pair)
                   class-name->class-definition))
                (if expansions?
                    ;; Generate children information for the superclass so it
                    ;; can be used as a default class.
                    (cons
                     (cons superclass (cdr (car subclass-pairs)))
                     subclass-pairs)
                    subclass-pairs)
                (if expansions?
                    (cons json-rule json-expansions)
                    json-expansions)))
             (generate-input-subclass-dispatch (json-expansions subclass-pairs)
               "Generate a method to return the name of the subclass
                to be used by the parse-tree returned by tree-sitter."
               `(defmethod get-choice-expansion-subclass
                    ((class (eql ',superclass)) parse-tree
                     &aux (child-types ',child-types))
                  (cond
                   ,@(mapcar
                      (lambda (json-rule subclass-pair)
                        `((match-parsed-children
                           ,language-prefix
                           ',json-rule
                           ',(caddr subclass-pair) child-types parse-tree)
                          ',(car subclass-pair)))
                      json-expansions
                      subclass-pairs)
                   (t
                    (error 'parse-tree-matching-error
                           :superclass ',superclass
                           :parse-tree parse-tree
                           :child-types ',child-types
                           :subclasses ',(mapcar #'car subclass-pairs))))))
             (generate-computed-text-methods (json-expansions subclass-pairs)
               "Generate the variable text methods for the rules in
                JSON-EXPANSION."
               (map nil
                    (op (generate-computed-text-method
                         _ (car _) language-prefix class-name->class-definition))
                    json-expansions subclass-pairs))
             (generate-output-transformations
                 (pruned-expansions json-expansions subclass-pairs)
               "Generate the output transformations for each subclass."
               (mapcar (lambda (pruned-rule json-rule class-name)
                         (generate-output-transformation
                          pruned-rule json-rule language-prefix class-name
                          class-name->class-definition choice-resolver))
                       pruned-expansions
                       json-expansions
                       (mapcar #'car subclass-pairs)))
             (generate-superclass-slot-usage ()
               "Generate the slot-usage slot for the superclass."
               ;; NOTE: store slot usage at the superclass level so that
               ;;       #'children-parser is able to tell when any slot that
               ;;       a subclass could use hasn't been used.
               (let ((slots
                       (remove-duplicates
                        (mapcar
                         (lambda (cons)
                           (if (member (car cons) '(:field :slot))
                               (cadr cons)
                               'children))
                         (collect-rule-slots
                          (convert-to-lisp-types
                           (collapse-rule-tree pruned-rule))
                          :include-slots t)))))
                 (add-slot-to-class-definition
                  superclass
                  class-name->class-definition
                  `(slot-usage
                    :accessor slot-usage
                    :initform ',slots
                    :allocation :class
                    :documentation
                    "A set of slots that are used in the pruned-rule.")))))
      (report-problematic-rule)
      ;; TODO: refactor this and children-parser as it accepts a
      ;;       pruned rule and wasn't before.
      (if (computed-text-ast-p language-prefix superclass json-rule)
          ;; Don't expand choices if the super class is computed text.
          `(progn
             ,(generate-computed-text-method
               json-rule superclass language-prefix
               class-name->class-definition))
          (mvlet* ((pruned-rule-expansions
                    ;; NOTE: if json-expansions has 1 item, it does NOT mean that
                    ;;       it is the same as json-rule! This is important
                    ;;       because the rule slot needs to be set with the item
                    ;;       in json-expansions to match pruned-rule.
                    json-expansions
                    (expand-choice-branches pruned-rule json-rule))
                   (expansions? (not (= 1 (length pruned-rule-expansions))))
                   (collapsed-rule-expansions
                    (mapcar [#'convert-to-lisp-types #'collapse-rule-tree]
                            pruned-rule-expansions))
                   (subclass-pairs
                    (if expansions?
                        (mapcar (lambda (collapsed-rule pruned-rule)
                                  (list (get-subclass-name collapsed-rule)
                                        ;; TODO: get rid of this?
                                        collapsed-rule
                                        pruned-rule))
                                collapsed-rule-expansions
                                (mapcar
                                 #'convert-to-lisp-types pruned-rule-expansions))
                        `((,superclass
                           ,(car collapsed-rule-expansions)
                           ,(convert-to-lisp-types
                             (car pruned-rule-expansions)))))))
            (when expansions?
              (generate-subclasses subclass-pairs)
              (add-subclass-list-slot subclass-pairs))
            (generate-superclass-slot-usage)
            (generate-children-methods
             subclass-pairs json-expansions expansions?)
            `(progn
               ,(and expansions? (generate-input-subclass-dispatch
                                  json-expansions subclass-pairs))
               ;; TODO: at some point, determine how to remove this.
               ,@(generate-computed-text-methods json-expansions subclass-pairs)
               ,@(generate-output-transformations
                  pruned-rule-expansions json-expansions subclass-pairs))))))

  (defun generate-structured-text-methods
      (grammar types language-prefix
       class-name->class-definition choice-resolver
       json-field-transformations
       &key symbols-to-export
       &aux (class-name->parse-tree-transforms (make-hash-table)))
    ;; NOTE: it might make sense to integrate the loop into
    ;;       the class generation above at some point.

    ;; TODO: NOTE: figure out how to handle the superclasses instead of
    ;;             treating them as terminal nodes.
    ;;
    ;;             There's a top-level "superclasses" form in the grammar.
    ;;             can probably ignore based on that.

    ;; NOTE: things we know at this point:
    ;;        - If a choice occurs in a repeat, all branches share the same slots.
    ;;        - Choices outside of repeats will be expanded if any of the branches
    ;;          contain different slots or ordering of slots.
    (labels ((generate-code
                 (transformed-json type-json
                  &aux (class-name (convert-to-lisp-type
                                    language-prefix (aget :type type-json))))
               "Generate the code for TRANSFORMED-JSON that is of the type
                specified by TYPE-JSON."
               (generate-input/output-handling
                (prune-rule-tree transformed-json)
                transformed-json
                class-name
                ;; NOTE: this should be a keyword.
                language-prefix
                (mapcar [{convert-to-lisp-type language-prefix}
                         {aget :type}]
                        (aget :types (aget :children type-json)))
                class-name->class-definition
                choice-resolver
                (computed-text-ast-p
                 language-prefix class-name transformed-json)
                :symbols-to-export symbols-to-export))
             (generate-terminal-code (type-string class-name)
               "Generate the code for a terminal symbol."
               ;; TODO: rename this function
               ;; destructively modify the class definition.
               (add-slot-to-class-definition
                class-name class-name->class-definition
                `(text
                  :accessor text
                  :initarg :text
                  :allocation :class
                  :initform ',type-string)))
             (generate-parse-tree-transform (class-name transforms)
               "Generate a method for generated-transform-parse-tree based on
                class-name and transforms."
               `(defmethod generated-transform-parse-tree
                    ((language (eql ',(make-keyword language-prefix)))
                     (class (eql ',(format-symbol :sel/sw/ts "~a-~a"
                                    language-prefix class-name)))
                     parse-tree)
                  (reduce
                   (lambda (tree transform)
                     (funcall transform tree))
                   ,(cons 'list transforms)
                   :initial-value parse-tree)))
             (get-transformed-json-table ()
               "Get a hash table containing transformed JSON rules."
               (let* ((rules (add-aliased-rules
                              (substitute-json-rules
                               language-prefix
                               (aget :rules grammar))))
                      (new-grammar (areplace :rules rules grammar))
                      (rule-table (alist-hash-table
                                   (mapcar (lambda (rule)
                                             (list (car rule) (cdr rule)))
                                           rules))))
                 (iter
                   (iter:with root-rule-name = (caar (aget :rules grammar)))
                   (for (key value) in-hashtable
                        (combine-aliased-rules rule-table))
                   (setf (gethash key rule-table)
                         (add-internal-ast-slots
                          language-prefix
                          (transform-json-rule
                           (substitute-field-symbols
                            value
                            key language-prefix
                            json-field-transformations
                            class-name->class-definition
                            class-name->parse-tree-transforms)
                           new-grammar (list key))
                          (convert-to-lisp-type language-prefix key)
                          class-name->class-definition
                          :top-level-rule (eq root-rule-name key))))
                 rule-table))
             (get-superclasses-set ()
               "Get a hash set containing the names of superclasses for the
                language."
               (alist-hash-table
                (mapcar {cons _ t} (aget :supertypes grammar))
                :test #'equal))
             (get-parse-tree-transforms ()
               "Get a list of generated-transform-parse-tree methods based on
                values in class-name->class-definition."
               (maphash-return #'generate-parse-tree-transform
                               class-name->parse-tree-transforms)))
      `(progn
         ,@(iter
             (iter:with super-classes-set = (get-superclasses-set))
             (iter:with rule-table = (get-transformed-json-table))
             (iter:with type-set = (make-hash-table :test #'equal))
             (for type in types)
             (for type-string = (aget :type type))
             (for converted-type = (convert-name type-string))
             (for lisp-type = (convert-to-lisp-type
                               language-prefix converted-type))
             (for terminal-lisp-type = (format-symbol 'sel/sw/ts "~a-~a"
                                                      lisp-type 'terminal))
             (cond-let result
               ;; skip superclass interfaces.
               ;; TODO: at some point, the interfaces will always be at the top,
               ;;       so they can be counted out and skipped instead of this.
               ((gethash type-string super-classes-set))
               ((gethash converted-type type-set)
                ;; If the name is already in the type set, that means a rule
                ;; has the same name as a language keyword. As an example,
                ;; Python has a 'lambda' rule but also has a 'lambda' keyword
                ;; as part of the language.
                (generate-terminal-code type-string terminal-lisp-type))
               ((gethash (make-keyword converted-type) rule-table)
                ;; Only add to the type-set here since there won't be a
                ;; name-clash for terminal symbols.
                (setf (gethash converted-type type-set) t)
                (collect (generate-code (car result) type)))
               ((aget :named type)
                ;; If a type is named and doesn't have a rule present assume
                ;; that it is a computed text node. This is an edge case
                ;; that shouldn't happen often and should cover external
                ;; named nodes.
                (generate-computed-text-method
                 nil lisp-type language-prefix class-name->class-definition
                 :skip-checking-json t))
               (t
                ;; If a type doesn't have a rule and is unnamed, it is considered
                ;; a terminal symbol.
                (generate-terminal-code
                 type-string
                 (if (gethash terminal-lisp-type class-name->class-definition)
                     terminal-lisp-type
                     lisp-type)))))
         ,@(get-parse-tree-transforms))))

  (defun create-tree-sitter-classes
      (node-types-file grammar-file name-prefix
       &key ast-superclasses base-ast-superclasses
         software-superclasses software-direct-slots
         ast-extra-slot-options
         ast-extra-slots
         node-type-substitutions
         json-subtree-choice-resolver
         json-field-transformations
       &aux (subtype->supertypes (make-hash-table))
         (symbols-to-export (make-hash-table))
         (class->extra-slot-options (make-hash-table))
         (class->extra-slots (make-hash-table))
         (ast-superclass (symbolicate
                          name-prefix
                          "-"
                          (convert-name "ast")))
         (class-name->class-definition (make-hash-table))
         (*json-identifier-name-to-lisp* #'convert-name)
         (node-types
          (substitute-json-node-types
           node-type-substitutions
           (decode-json-from-string
            (file-to-string node-types-file))))
         (grammar (decode-json-from-string (file-to-string grammar-file)))
         ;; TODO: consider turning this into a hash table.
         (grammar-rules (aget :rules grammar)))
    "Create the classes for a tree-sitter language.

Creates one class for a software object (named NAME-PREFIX) and many
classes for ASTs, using NAME-PREFIX as their prefix.

AST-SUPERCLASSES is an alist of superclasses and the names of the AST
classes that should inherit from them.

BASE-AST-SUPERCLASSES is a list of superclasses for the base
class (`X-ast') of the language's ASTs.

SOFTWARE-SUPERCLASSES is a list of superclass names for the software
object.

SOFTWARE-DIRECT-SLOTS is a list of slots to be added to the created
sofware class.

AST-EXTRA-SLOT-OPTIONS is an alist from classes to extra options for
their slots.

AST-EXTRA-SLOTS is an alist from classes to extra slots."
    (labels ((initialize-subtype->supertypes ()
               "Initialize subtype->supertypes with the super types that
                aren't parsed from the json files."
               (mapc
                (lambda
                    (types-list &aux (supertype (symbolicate (car types-list))))
                  (mapc
                   (lambda (subtype)
                     (push supertype (gethash subtype subtype->supertypes)))
                   (cdr types-list)))
                ast-superclasses)
               ;; Add super class into all of these to ensure it's present. When
               ;; add-supertypes-to-subtypes is called, it will need to remove
               ;; it.
               (maphash-keys
                (lambda (subtype)
                  (push ast-superclass (gethash subtype subtype->supertypes)))
                subtype->supertypes)
               ;; Return for easier debugging.
               subtype->supertypes)
             (initialize-class->extra-slot-options ()
               (iter (for (class . fields) in ast-extra-slot-options)
                     (setf (gethash class class->extra-slot-options) fields))
               ;; Return for easier debugging.
               class->extra-slot-options)
             (initialize-class->extra-slots ()
               (iter (for (class . slots) in ast-extra-slots)
                 (setf (gethash class class->extra-slots) slots))
               ;; Return for easier debugging.
               class->extra-slots)
             (populate-supertypes ()
               "Populate the subtypes to supertypes with supertypes. This is
                to have the information available if the definition occurs after
                its first usage."
               (mapc
                (lambda
                    (supertype
                     &aux (type (find-if (op (equal supertype (aget :type _)))
                                         node-types)))
                  ;; TODO: add error output if type is nil.
                  (when type
                    (add-supertype-to-subtypes
                     (aget :type type) (aget :subtypes type))))
                (aget :supertypes grammar)))
             (make-class-name (&optional name-string)
               "Create a class name based on NAME-STRING and add it to the
                symbols that need exported."
               ;; NOTE: this has the potential for name clashes
               ;;       though it's probably unlikely.
               (lret* ((*package* (find-package :sel/sw/ts))
                       (name
                        (if name-string
                            (symbolicate
                             name-prefix
                             "-"
                             (convert-name name-string))
                            (symbolicate name-prefix))))
                 (ensure-gethash name symbols-to-export t)))
             (make-accessor-name (prefix name-keyword)
               "Create an accessor name based on NAME-KEYWORD and
                PREFIX and add it to the symbols that need exported."
               (lret ((name (symbolicate
                             prefix
                             "-"
                             name-keyword)))
                 (ensure-gethash name symbols-to-export t)))
             (make-accessor-names (name-keyword)
               "Create accessor names based on NAME-PREFIX and
                `*tree-sitter-ast-extra-prefixes*` and add them to the
                symbols that need exporting."
               (cons (make-accessor-name name-prefix name-keyword)
                     (mapcar (op (make-accessor-name _ name-keyword))
                             (aget name-prefix
                                   *tree-sitter-ast-extra-prefixes*
                                   :test #'string=))))
             (get-supertypes-for-type (type)
               "Retrieve the supertypes of TYPE."
               (gethash (make-class-name type) subtype->supertypes))
             (add-supertype-to-subtypes (supertype subtypes)
               "Add SUPERTYPE to the list of superclasses for
                each type in SUBTYPES."
               (mapc
                (lambda (subtype &aux (name (aget :type subtype)))
                  (symbol-macrolet ((subtype-hash (gethash (make-class-name name)
                                                           subtype->supertypes)))
                    (setf subtype-hash
                          (cons (make-class-name supertype)
                                ;; Remove ast-superclass to prevent
                                ;; circular class dependency.
                                (remove ast-superclass subtype-hash)))))
                subtypes))
             (create-slot (type field)
               "Create a slot for TYPE based on FIELD."
               (let ((names (make-accessor-names (car field))))
                 `(,(first names)
                   ,@(mappend (op `(:accessor ,_)) names)
                   ;; Prefixed initargs.
                   ,@(mappend (op `(:initarg ,(make-keyword _))) names)
                   :initform nil
                   ,@(lookup-extra-slot-options type (first names)))))
             (lookup-extra-slot-options (type field-name)
               (declare (symbol type field-name))
               (aget field-name (gethash type class->extra-slot-options)))
             (create-slots (type fields)
               "Create the slots for a new class of TYPE based on
                FIELDS and CHILDREN. Currently, slot types aren't
                supported, but there is enough information to limit
                slots to certain types."
               (declare (symbol type) (list fields))
               ;; NOTE: there is a small possibility for name overlaps when
               ;;       generating these slots.
               (mapcar {create-slot type} fields))
             (create-supertype-class (type
                                      &aux (class-name (make-class-name type)))
               "Create a new class for subtypes to inherit from."
               `(defclass ,class-name
                    (,@(or (get-supertypes-for-type type)
                           `(,ast-superclass)))
                  ()
                  (:documentation ,(format nil "Generated for ~a." type))))
             (create-type-class (type fields grammar-rules
                                 &aux (class-name (make-class-name type)))
               "Create a new class for TYPE using FIELDS and CHILDREN for slots."
               (let* ((extra-fields (gethash class-name class->extra-slots))
                      (all-fields (append fields extra-fields))
                      (child-slot-order
                        (append
                         (when all-fields
                           (mapcar
                            (lambda (slot-keyword)
                              (cons
                               (translate-to-slot-name slot-keyword name-prefix)
                               (if (aget :multiple
                                         (aget slot-keyword all-fields))
                                   0
                                   1)))
                            (slot-order type all-fields grammar-rules)))
                         (when extra-fields
                           ;; Assume an arity of 0 for now.
                           (mapcar
                            (op (cons
                                 (translate-to-slot-name (car _1) name-prefix)
                                 (if (aget :multiple (cdr _1))
                                     0
                                     1)))
                            extra-fields)))))
                 `(define-node-class
                      ,class-name
                      (,@(or (get-supertypes-for-type type)
                             `(,ast-superclass)))
                    (,@(create-slots class-name all-fields)
                     (child-slots
                      :initform
                      ',(append '((before-asts . 0))
                                child-slot-order
                                '((children . 0))
                                '((after-asts . 0)))
                      :allocation :class))
                    ;; NOTE: this is primarily for determing which rule this
                    ;;       was generated for.
                    (:documentation ,(format nil "Generated for ~a." type))
                    (:method-options :skip-children-definition))))
             (create-terminal-symbol-class (type)
               "Create a new class that represents a terminal symbol.
                In the case that there's a non-terminal with the same name,
                append '-terminal' to the end of it."
               `(define-node-class
                    ,(if (gethash
                          (format-symbol 'sel/sw/ts "~a-~a"
                                         name-prefix
                                         (convert-name type))
                          class-name->class-definition)
                         (make-class-name
                          (format-symbol 'sel/sw/ts "~a-~a"
                                         (string-upcase type) 'terminal))
                         (make-class-name type))
                    (,@(or (get-supertypes-for-type type)
                           `(,ast-superclass))
                     terminal-symbol)
                  ()
                  (:documentation
                   ,(format nil "Generated for terminal symbol '~a'" type))))
             (create-node-class
                 (grammar-rules node-type
                  &aux (type (aget :type node-type))
                    (subtypes (aget :subtypes node-type))
                    (named-p (aget :named node-type)))
               "Create a class for  NODE-TYPE."
               (let* ((class-definition
                        (cond
                          (subtypes (create-supertype-class type))
                          (named-p
                           (create-type-class
                            type
                            (aget :fields node-type)
                            grammar-rules))
                          ;; Terminal Symbol
                          (t (create-terminal-symbol-class type))))
                      (class-name (cadr class-definition)))
                 (push class-name
                       (gethash 'class-order class-name->class-definition))
                 (setf (gethash class-name class-name->class-definition)
                       class-definition)))
             (create-external-class (name)
               "Create a class for an external rule."
               (when name
                 `(define-node-class ,(make-class-name name) (,ast-superclass) ())))
             (create-external-classes (grammar)
               "Create classes for the external rules for the grammar file."
               (filter-map (op (create-external-class (aget :name _)))
                           (aget :externals grammar))))
      (initialize-subtype->supertypes)
      (initialize-class->extra-slot-options)
      (initialize-class->extra-slots)
      (populate-supertypes)
      ;; populate hash table of tree-sitter classes.
      (map nil {create-node-class grammar-rules} node-types)
      (let ((structured-text-code
              (generate-structured-text-methods
               grammar node-types name-prefix class-name->class-definition
               json-subtree-choice-resolver json-field-transformations
               :symbols-to-export symbols-to-export))
            (root-rule-name (caar (aget :rules grammar))))
        `(progn
           (eval-always
             (define-software ,(make-class-name) (tree-sitter
                                                  ,@software-superclasses)
               (,@software-direct-slots)
               (:documentation
                ,(format nil "~a tree-sitter software representation."
                         name-prefix)))

             (define-template-builder ,(make-class-name)
                 ,(make-class-name "ast"))

             (define-node-class ,(make-class-name "ast")
                 (tree-sitter-ast ,@(mapcar [#'car #'ensure-list]
                                            base-ast-superclasses))
               ;; NOTE: ensure there is always a children slot.
               ;;       This is important for classes that don't have
               ;;       it but can have comments mixed in.
               ((children ,@(mappend (op `(:accessor ,_))
                                     (make-accessor-names :children))
                          :accessor direct-children
                          :documentation
                          "Returns all language-specific children.
Unlike the `children` methods which collects all children of an AST from any slot."
                          :initarg :children
                          :initarg :direct-children
                          :initform nil)
                (child-slots :allocation :class
                             :initform '((children . 0))))
               (:documentation
                ,(format nil "AST for ~A from input via tree-sitter."
                         name-prefix))
               (:method-options :skip-children-definition))

             ;; TODO: the error and comment classes may be created by some
             ;;       languages?
             ;; NOTE: the following are to handle results returned from
             ;;       cl-tree-sitter.
             (define-node-class ,(make-class-name "comment")
                 ,(remove-duplicates
                   `(,ast-superclass
                     ,@(get-supertypes-for-type "comment")
                     comment-ast)
                   :from-end t)
               ()
               (:documentation "Generated for parsed comments."))

             (define-node-class ,(make-class-name "error")
                 ,(remove-duplicates
                   `(,ast-superclass
                     ,@(get-supertypes-for-type "error")
                     parse-error-ast)
                   :from-end t)
               ((children
                 :initarg :children
                 :initarg :direct-children
                 :initform nil)
                (child-slots :initform '((children . 0))
                             :allocation :class))
               (:documentation "Generated for parsing errors."))

             (define-node-class ,(make-class-name "inner-whitespace")
                 ,(remove-duplicates
                   `(,ast-superclass
                     ,@(get-supertypes-for-type "inner-whitespace")
                     inner-whitespace)
                   :from-end t)
               ()
               (:documentation "Generated for inner whitespace."))

             (define-node-class ,(make-class-name "source-text-fragment")
                 ,(remove-duplicates
                   `(,ast-superclass
                     ,@(get-supertypes-for-type "source-text-fragment")
                     source-text-fragment)
                   :from-end t)
               ()
               (:documentation "Generated for source text fragments."))

             ,@(create-external-classes grammar)

             ;; NOTE: we want to maintain the order of the classes as they
             ;;       were created. Since hash tables are unordered, a stack
             ;;       is kept at the key 'class-order so that they can be
             ;;       retrieved in order.
             ,@(iter
                 (for
                  class-name in
                  (reverse (gethash 'class-order class-name->class-definition)))
                 (collect (gethash class-name class-name->class-definition)))

             (define-mutation ,(make-class-name "mutation") (parseable-mutation)
               ()
               (:documentation
                ,(format nil "Mutation interface for ~a software objects."
                         name-prefix)))

             (export ',(iter
                         (for (symbol) in-hashtable symbols-to-export)
                         (collect symbol))))

           (defmethod convert
               ((to-type (eql ',ast-superclass)) (spec ,ast-superclass)
                &key &allow-other-keys)
             spec)

           (defmethod convert ((to-type (eql ',ast-superclass)) (spec list)
                               &key string-pass-through
                                 computed-text-parent-p
                                 line-octets-cache
                                 patch-whitespace
                               &allow-other-keys)
             (convert 'tree-sitter-ast spec
                      :patch-whitespace patch-whitespace
                      :superclass to-type
                      :string-pass-through string-pass-through
                      :computed-text-parent-p computed-text-parent-p
                      :line-octets-cache line-octets-cache))

           (defmethod convert ((to-type (eql ',ast-superclass)) (string string)
                               &rest args &key &allow-other-keys)
             (apply #'convert 'tree-sitter-ast string :superclass to-type args))

           (defmethod parse-asts ((obj ,(make-class-name))
                                  &optional (source (genome-string obj)))
             (convert ',(make-class-name "ast") source))

           (defmethod computed-text-node-p ((ast ,(make-class-name "error")))
             t)

           (defmethod computed-text-node-p
               ((ast ,(make-class-name "inner-whitespace")))
             t)

           (defmethod root-rule-ast-p ((ast ,(make-class-name root-rule-name)))
             t)

           (defmethod root-rule-ast-p
               ((name (eql ,(make-keyword (convert-name root-rule-name)))))
             t)

           ;; This works around a bug with ranges for newlines in tree-sitter
           ;; and should be removed when fixed upstream.
           (defmethod transform-parse-tree
               ((language (eql ',(make-keyword name-prefix)))
                (class (eql ',(format-symbol :sel/sw/ts "~a-~%" name-prefix)))
                parse-tree
                &rest rest &key lines
                &aux (start-range (caadr parse-tree))
                  (start-line (cadr start-range)))
             (declare (ignorable rest))
             (if (equal start-range (cadadr parse-tree))
                 ;; Don't modify zero-width tokens.
                 parse-tree
                 `(,(car parse-tree)
                   ,(list
                     ;; TODO: will this become an issue with unicode?
                     (list (position (char-code #\newline) (aref lines start-line)
                                     :start (car start-range))
                           start-line)
                     (list 0 (1+ start-line)))
                   ,(caddr parse-tree))))

           ,structured-text-code)))))

(defmacro define-and-export-all-mixin-classes ()
  "Ensure that all mixin classes are defined and exported."
  (let ((classes
         (nest (remove-duplicates)
               (mapcar #'car)
               (mappend #'cdr)
               *tree-sitter-ast-superclasses*)))
    `(progn
       ,@(iter (for class in classes)
               (let ((class (intern (string class) :sel/sw/ts))
                     (description
                      (nest
                       (string-downcase)
                       (substitute #\Space #\-)
                       (if (string$= '-ast class)
                           (drop -4 (string class)))
                       (string class))))
                 (collect `(progn
                             (export ',class :sel/sw/ts)
                             (unless (find-class ',class nil)
                               (defclass ,class (ast)
                                 ()
                                 (:documentation
                                  ,(fmt "Mix-in for ~a AST classes."
                                        description)))))))))))

(eval-always
 (define-and-export-all-mixin-classes))

(defmethod pruned-rule ((ast t)) nil)
(defmethod slot-usage ((ast t)) nil)

(defgeneric parse-order (ast &key &allow-other-keys)
  (:documentation "Return a list of children intermixed with keywords
that specify which CHOICE branches are taken and how many times a REPEAT rule
repeats.")
  (:method (ast &key) nil)
  (:method ((ast structured-text) &key)
    (if-let ((rule (or (and (slot-exists-p ast 'pruned-rule)
                            (pruned-rule ast))))
             (slots (and (slot-exists-p ast 'slot-usage)
                         (slot-usage ast))))
      (children-parser ast rule slots)
      (call-next-method))))

(defun matches-rule (ast &optional pruned-rule)
  "Given a tree-sitter ast, returns true iff the pruned rule is valid.
 This is useful when synthesizing new asts, at each step you can validate
 that the rule has not been broken."
  (if-let ((rule (or pruned-rule (pruned-rule ast)))
           (slots (slot-usage ast)))
    (handler-case
        (children-parser ast rule slots)
      (rule-matching-error () nil))))

(defun check-ast-replacement (root ast new-ast)
  "Given a root, an ast (a member of the root tree), and new-ast,
 an ast which is not a member, see if the new-ast is a valid replacement
 for ast. If so, returns the mutated root, otherwise returns NIL."
  ;; optimization: See if the new ast can be parsed by the previous ast's
  ;; pruned-rule. If not we assume it will error during mutation attempt.
  (if (and (slot-exists-p ast 'pruned-rule) (pruned-rule ast))
      (unless (and (slot-exists-p new-ast 'pruned-rule) (pruned-rule new-ast))
        (matches-rule new-ast (pruned-rule ast))
        (return-from check-ast-replacement nil)))
  (handler-case
      ;; go ahead and try the mutation
      (with root ast new-ast)
    (rule-matching-error () nil)))

(defun check-ast-swappable (root ast1 ast2)
  "Given a root, and 2 asts (members of the root tree), see if 2 asts can
 be swapped. If so, returns the mutated root, otherwise returns NIL."
  (handler-case
      ;; go ahead and try the mutation
      (swap root ast1 ast2)
    (rule-matching-error () nil)))

(defun check-ast-insertable (root ast new-ast)
  "Given a root, an ast (a member of the root tree), and new-ast,
 an ast which is not a member, see if the new-ast can be inserted
 at ast. If so, returns the mutated root, otherwise returns NIL."
  (handler-case
      ;; go ahead and try the mutation
      (insert root ast new-ast)
    (rule-matching-error () nil)))

(defun check-ast-cut (root ast)
  "Given a root, an ast (a member of the root tree), see if the selected
 ast can be cut, without a rule error. If so, returns the mutated root,
 otherwise returns NIL."
  (handler-case
      ;; go ahead and try the mutation
      (less root ast)
    (rule-matching-error () nil)))

(defun change-to-subclass (ast subclasses)
"Dynamically change the class until a subclass matches on a rule.
This is only used when a superclass instance is manually created.
Note that this won't always pick the correct subclass."
  (iter
    (iter:with superclass = (type-of ast))
    (for subclass in subclasses)
    (change-class ast subclass)
    (for result = (handler-case (parse-order ast)
                    (rule-matching-error ()
                      (next-iteration))))
    (return result)
    (finally
     (change-class ast superclass)
     (error 'rule-matching-error
            :rule-matching-error-rule (pruned-rule ast)
            :rule-matching-error-ast ast))))

(defgeneric output-transformation
    (ast &rest rest &key &allow-other-keys)
  (:documentation "Return a list of strings and AST objects that
are ordered for reproduction as source text.")
  (:method ((ast structured-text) &rest rest &key &allow-other-keys)
    (declare (ignorable rest))
    (computed-text-output-transformation ast))
  (:method ((ast text-fragment) &rest rest &key &allow-other-keys)
    (declare (ignorable rest))
    (list (before-text ast) (text ast) (after-text ast)))
  (:method :around ((ast structured-text)
                    &rest rest &key finalized-type &allow-other-keys)
    (declare (ignorable rest))
    (labels ((append-before-and-after-asts (output-transformation)
               "Append before and after ASTs to OUTPUT-TRANSFORMATION."
               (mappend
                (lambda (output)
                  (cond
                    ((typep output 'structured-text)
                     ;; TODO Currently before-text and after-text can contain
                     ;; conflict ASTs.
                     (append (and (typep (before-text output) 'conflict-ast)
                                  (list (before-text output)))
                             (before-asts output)
                             (list output)
                             (after-asts output)
                             (and (typep (after-text output) 'conflict-ast)
                                  (list (after-text output)))))
                    (t (list output))))
                output-transformation)))
      (cond-let subclasses
        ((and (not finalized-type)
              (slot-exists-p ast 'choice-subclasses)
              (choice-subclasses ast))
         ;; This allows instances of ASTs to be created from their superclass.
         ;; A subclass with a matching rule is assigned here.
         (change-to-subclass ast subclasses)
         (output-transformation ast :finalized-type t))
        ((computed-text-node-p ast)
         (append-before-and-after-asts
          (computed-text-output-transformation ast)))
        (t
         (block nil
           ;; Use handler-bind so we don't unwind if we can't handle.
           (handler-bind
               ((rule-matching-error
                 (lambda (rule-error)
                   (declare (ignore rule-error))
                   (if (and (not finalized-type)
                            (slot-exists-p ast 'choice-superclass))
                       ;; Try to find a relevant subclass if the current one does
                       ;; not match.
                       (return
                         (output-transformation
                          (change-class ast (slot-value ast 'choice-superclass))))))))
             (append-before-and-after-asts (call-next-method)))))))))

(defmethod predecessor ((root structured-text) (node structured-text))
  (when-let (parent (parent root node))
    (let ((predecessor
           (nest (second)
                 (member node)
                 (reverse)
                 (remove-if (conjoin #'stringp #'emptyp))
                 (output-transformation parent))))
      (if (stringp predecessor)
          (make-keyword predecessor)
          predecessor))))

(defmethod successor ((root structured-text) (node structured-text))
  (when-let (parent (parent root node))
    (let ((successor
           (nest (second)
                 (member node)
                 (remove-if (conjoin #'stringp #'emptyp))
                 (output-transformation parent))))
      (if (stringp successor)
          (make-keyword successor)
          successor))))

;;; TODO: deprecate this generic.
(defgeneric computed-text-node-p (ast)
  (:documentation "Return T if AST is a computed-text node. This is a
node where part of the input will need to be computed and stored to reproduce
the source-text.")
  (:method (ast) nil)
  (:method ((ast computed-text)) t))

(defgeneric root-rule-ast-p (ast)
  (:documentation "Return T if AST represents the root rule or entry point
of a grammar.")
  (:method (ast) nil))

(defgeneric get-choice-expansion-subclass (class spec)
  (:documentation "Get the subclass of CLASS associated with SPEC.")
  (:method (class spec)
    (declare (ignorable spec))
    class))

(defun label-as (field-name &rest types)
  "Return a function that wraps parse trees of TYPE as fields named
FIELD-NAME.

If there are no types, always wrap."
  (check-type field-name keyword)
  (assert (every #'keywordp types))
  (lambda (tree)
    (let ((type (parse-tree-type tree)))
      (when (or (null types) (member type types))
        (cons (list field-name type)
              (cdr tree))))))

(defun ignore-types (&rest types)
  (assert types)
  (lambda (tree)
    (and (member (parse-tree-type tree) types)
         tree)))

(defun rename-type-to (to from)
  (lambda (tree)
    (and (eql (parse-tree-type tree) from)
         (copy-parse-tree tree :type to))))

(-> modify-parse-tree (list &rest function) list)
(defun modify-parse-tree (parse-tree &rest fns)
  "For each child of PARSE-TREE, call each function in FNS with the child tree.

The first function to return non-nil is treated as the new child;
otherwise the original child is used."
  (copy-parse-tree
   parse-tree
   :children
   (mapcar
    (lambda (child-tree)
      (cond ((consp (parse-tree-type child-tree))
             child-tree)
            ((some (op (funcall _ child-tree)) fns))
            (t child-tree)))
    (parse-tree-children parse-tree))))

(defmacro with-modify-parse-tree ((parse-tree &key) &body body)
  "Like `modify-parse-tree', but with a case-like syntax.
Each clause consists of a key (or list of keys) and a function call.
The keys are simply appended to the function call.

If the key is `t', the call is left unchanged."
  `(modify-parse-tree
    ,parse-tree
    ,@(iter (for (key/s call) in body)
            (collect (if (eql key/s t) call
                         (append call (ensure-list key/s)))))))

(defgeneric transform-parse-tree
    (language class parse-tree &rest rest &key &allow-other-keys)
  (:documentation "Transform PARSE-TREE based on LANGUAGE and CLASS.")
  (:method (language class parse-tree &rest rest &key &allow-other-keys
            &aux (descriptor (and (listp parse-tree)
                                  (car parse-tree))))
    (cond
      (class parse-tree)
      ((and descriptor (not (= 3 (length parse-tree))))
       parse-tree)
      ;; :class
      ((keywordp descriptor)
       (apply
        {transform-parse-tree
         language
         (convert-to-lisp-type language descriptor)
         parse-tree}
        rest))
      ;; :slot, :class list
      ((and (consp descriptor)
            (keywordp (cadr descriptor)))
       (apply
        {transform-parse-tree
         language
         (convert-to-lisp-type language (cadr descriptor))
         parse-tree}
        rest))
      (t parse-tree)))
  (:method :around (language class parse-tree &rest rest &key)
    ;; Create source-text-fragments where they're needed.
    (declare (ignore rest))
    (if class
        (transform-malformed-parse-tree (call-next-method) :recursive nil)
        (call-next-method))))

(defgeneric generated-transform-parse-tree (language class parse-tree)
  (:documentation "Transform PARSE-TREE based on LANGUAGE with SPEC.
This is generated while creating tree-sitter code while transform-parse-tree
is hand-written.")
  (:method (language class parse-tree
            &aux (descriptor (and (listp parse-tree)
                                  (car parse-tree))))
    (cond
      (class parse-tree)
      ((and descriptor (not (= 3 (length parse-tree))))
       parse-tree)
      ;; :class
      ((keywordp descriptor)
       (generated-transform-parse-tree
        language
        (convert-to-lisp-type language descriptor)
        parse-tree))
      ;; :slot, :class list
      ((and (consp descriptor)
            (keywordp (cadr descriptor)))
       (generated-transform-parse-tree
        language
        (convert-to-lisp-type language (cadr descriptor))
        parse-tree))
      (t parse-tree))))

(defmethod children ((ast structured-text))
  (remove-if
   (of-type 'inner-whitespace)
   (remove-if-not (of-type 'ast) (output-transformation ast))))

;;; TODO This handles conflict ASTs in {before-,after-,}text slots.
(defmethod child-slots :around ((ast structured-text))
  "When there are ASTs in before-text, after-text, or text, expose them as
children. (This can happen when they store conflict ASTs)."
  (macrolet ((wrap-slot (slot)
               `(when (typep (slot-value ast ',slot) 'conflict-ast)
                  '((,slot . 1)))))
    (append
     (wrap-slot before-text)
     (wrap-slot text)
     (call-next-method)
     (wrap-slot after-text))))

;;; TODO This also handles conflict ASTs in {before-,after-,}text slots.
(defmethod child-slot-specifiers :around ((ast structured-text))
  "If there are ASTs in before-text, after-text, or text, expose them as
children. (This can happen when they store conflict ASTs)."
  (macrolet ((wrap-slot (slot)
               `(when (typep (slot-value ast ',slot) 'conflict-ast)
                  (load-time-value
                   (list
                    (make 'ft::slot-specifier
                          :class t
                          :slot ',slot
                          :arity 1))))))
    (append (wrap-slot before-text)
            (wrap-slot text)
            (call-next-method)
            (wrap-slot after-text))))


;;; tree-sitter parsing
(defun position-after-leading-newline (str &aux (pos 0))
  "Returns 1+ the position of the first newline in STR,
assuming it can be reached only by skipping over whitespace
or comments.  NIL if no such newline exists."
  (loop
    (when (>= pos (length str)) (return nil))
    (let ((c (elt str pos)))
      (case c
        (#\Newline (return (1+ pos)))
        ((#\Space #\Tab)
         (incf pos))
        (t (return nil))))))

(defun get-language-from-superclass (superclass)
  "Get the tree-sitter  language associated with SUPERCLASS."
  (or (gethash superclass *superclass->language*)
      (error "No tree-sitter language known for ~a." superclass)))

(defmethod convert :around ((to-type (eql 'tree-sitter-ast)) (string string)
                            &key deepest &allow-other-keys)
  (if deepest
      (let* ((ast (call-next-method))
             (ranges (ast-source-ranges ast))
             (runs (runs ranges :test #'equal? :key #'cdr :count 1)))
        (car (lastcar (first runs))))
      (call-next-method)))

;;;
;;; This is used by the individual language files (c.lisp, python.lisp, etc.)
;;;
(defmacro create-tree-sitter-language (name)
  "Given the name (string) of the tree-sitter language, generate
 all the classes, methods and other artifacts that define the language."
  (when-let ((tree-sitter-files
               (find name
                     *tree-sitter-language-files*
                     :key 'car :test 'equal)))
    `(eval-always
      (encode-tree-sitter-version ,@tree-sitter-files)
       (progn
       ,@(apply 'tree-sitter-ast-classes
                tree-sitter-files)
       ;; add the language :TREE-SITTER-<name> to the *FEATURES* list
       (pushnew
        (intern (concatenate 'string
                             "TREE-SITTER-"
                             (string-upcase ,name)) :keyword)
        *features*)))))

(defun file-md5 (file)
  "Return the MD5 of FILE as a hex string."
  (with-output-to-string (out)
    (do-each (byte (md5:md5sum-file file) 'list)
      (format out "~(~2,'0x~)" byte))))

(defmacro encode-tree-sitter-version (name grammar-file node-types-file)
  "Warn at load time if GRAMMAR-FILE and NODE-TYPES-FILE have changed
 since the definitions were compiled."
  (let ((grammar-file-hash (file-md5 grammar-file))
        (node-types-file-hash (file-md5 node-types-file)))
    `(eval-when (:load-toplevel)
       (when (file-exists-p ,grammar-file)
         (unless (equal ,grammar-file-hash (file-md5 ,grammar-file))
           (warn "~a has changed, recompile ~a"
                 ,grammar-file
                 ',name)))
       (when (file-exists-p ,node-types-file)
         (unless (equal ,node-types-file-hash (file-md5 ,node-types-file))
           (warn "~a has changed, recompile ~a"
                 ,node-types-file
                 ',name))))))

(defmethod get-parent-full-stmt (obj (ast tree-sitter-ast))
  (if (typep ast 'statement-ast)
      ast
      (get-parent-full-stmt obj (get-parent-ast obj ast))))

(defgeneric ast-type-to-rebind-p (ast)
  (:documentation "Return T if AST is of a type where its variables/functions
should be rebound.")
  (:method (ast) nil)
  (:method ((ast identifier-ast)) t))

(defmethod enclosing-scope ((obj tree-sitter) (ast ast))
  "Return the enclosing scope of AST in OBJ.
- OBJ tree-sitter software object
- AST ast to return the enclosing scope for"
  (or (find-if (lambda (ast)
                 (typep ast
                        '(or function-ast loop-ast compound-ast)))
               (get-parent-asts* obj ast))
      (genome obj)))

(defun ensure-children (value)
  (etypecase value
    (ast (values (direct-children value)
                 value))
    (list value)))

(defgeneric statements-in-scope (obj scope ast)
  (:documentation "Return all child statements of SCOPE prior to AST.")
  (:method (obj (scope ast) (ast ast))
    (iter (for c in (remove nil (children scope)))
          (while (path-later-p obj ast c))
          (collect c))))

(defgeneric identifiers (ast)
  (:documentation "Return all identifier nodes in AST and its children.")
  (:method ((ast ast))
    (collect-if {typep _ 'identifier-ast} ast)))

(defgeneric call-arguments (ast)
  (:documentation "Return the arguments of AST.")
  (:method-combination standard/context)
  (:method :context ((ast t))
    (ensure-children (call-next-method))))

(defgeneric function-parameters (ast)
  (:documentation "Return the parameters of AST.")
  (:method-combination standard/context)
  (:method :context ((ast t))
    (ensure-children (call-next-method))))

(defgeneric return-type (ast)
  (:documentation "Get the return type of AST."))

(defgeneric parameter-type (parameter-ast)
  (:documentation "Return a representation of the TYPE of PARAMETER-AST."))

(defgeneric parameter-name (parameter-ast)
  (:documentation "Return the name of PARAMETER-AST."))

(defgeneric function-body (ast)
  (:documentation "Return the body of AST."))

(defgeneric call-function (call-ast)
  (:documentation "Return the function of CALL-AST."))

(defgeneric call-name (call-ast)
  (:documentation "Return the name of CALL-AST.")
  (:method ((call-ast call-ast)) (source-text (call-function call-ast))))

(defgeneric variable-name (variable-ast)
  (:documentation "Return the name of VARIABLE-AST."))

(defvar no-return-function-names '("exit" "abort"))

(defgeneric no-fallthrough (ast)
  (:documentation "Return t if AST will never fall through.")
  (:method ((otherwise t)) nil)
  (:method ((ast call-ast))
    (member (call-name ast) no-return-function-names :test #'string=))
  (:method ((ast return-ast)) t)
  (:method ((ast goto-ast)) t)
  (:method ((ast compound-ast))
    (no-fallthrough (lastcar (children ast)))))

(defgeneric inner-declarations (ast)
  (:documentation "Return a list of variable declarations affecting inner scopes.")
  (:method ((ast ast)) nil)
  (:method ((ast function-ast))
    (mappend #'identifiers (function-parameters ast))))

(defgeneric outer-declarations (ast)
  (:documentation
   "Return a list of variable declarations affecting outer scopes.")
  (:method ((ast ast)) nil))

(defgeneric lhs (ast)
  (:documentation "Return the left-hand side of an AST."))

(defgeneric rhs (ast)
  (:documentation "Return the right-hand side of an AST."))

(defgeneric operator (ast)
  (:method-combination standard/context)
  (:method :context ((ast t))
    (make-keyword (trim-whitespace (source-text (call-next-method)))))
  (:documentation "Return the operator from an AST as a keyword."))

(defgeneric control-flow-condition (control-flow-ast)
  (:documentation "Return the condition from a CONTROL-FLOW-AST."))

(defclass normal-scope () ()
  (:documentation "Tree-sitter mixin for languages with \"normal\" scoping."))

(defgeneric get-parent-decl (obj identifier)
  (:documentation "For the given IDENTIFIER AST, return the parent declaration.")
  (:method (obj identifier)
    (car (remove-if-not {typep _ 'variable-declaration-ast}
                        (get-parent-asts obj identifier)))))

(defgeneric ast-to-scope-alist (obj scope ast)
  (:documentation "Return a scope alist based on AST with SCOPE.
The alist should contain at least the following:
 - :name :: a string which contains the name of the identifier.
 - :decl :: an AST which represents the declaration.
 - :scope :: an AST which represents the scope of the identifier.")
  (:method (obj scope ast)
    `((:name . ,(source-text ast))
      (:decl . ,(or (get-parent-decl obj ast) ast))
      (:scope . ,scope))))

(defmethod scopes ((obj normal-scope) (ast ast))
  (unless (null (ast-path obj ast))
    (let ((scope (enclosing-scope obj ast)))
      (cons (reverse
             (mapcar {ast-to-scope-alist obj scope}
                     (append (inner-declarations scope)
                             (mappend #'outer-declarations
                                      (statements-in-scope obj scope ast)))))
            (scopes obj scope)))))

(defgeneric find-enclosing (type software ast)
  (:documentation "Return the nearest enclosing AST of TYPE in SOFTWARE.")
  (:method ((type t) (software tree-sitter) (ast ast))
    ;; (assert (typep type '(or symbol (cons symbol t) class)))
    (find-if {typep _ type} (get-parent-asts software ast))))

(defgeneric find-preceding (type software ast)
  (:documentation "Return any siblings of TYPE preceding AST in SOFTWARE.")
  (:method ((type t) (software software) (ast tree-sitter-ast))
    (find-preceding type (genome software) ast))
  (:method ((type t) (root ast) (ast tree-sitter-ast))
    ;; (assert (typep type '(or symbol (cons symbol t) class)))
    (when-let ((parent (get-parent-ast root ast)))
      (iter (for child in (children parent))
            (until (eql child ast))
            (when (typep child type)
              (collect child)))))
  (:method ((type t) (root ast) (ast conflict-ast))
    (when-let (default (first (conflict-ast-default-children ast)))
      (find-preceding type root default))))

(defgeneric find-following (type software ast)
  (:documentation "Return any siblings of TYPE following AST in SOFTWARE.")
  (:method ((type t) (software tree-sitter) (ast tree-sitter-ast))
    ;; (assert (typep type '(or symbol (cons symbol t) class)))
    (when-let ((parent (get-parent-ast software ast)))
      (nest
       (filter (of-type type))
       (rest)
       (drop-until (eqls ast))
       (sorted-children parent)))))

(defgeneric comments-for (software ast)
  (:documentation "Return the comments for AST in SOFTWARE.")
  (:method ((software tree-sitter) (ast tree-sitter-ast))
    (or (find-preceding 'comment-ast software ast)
        ;; In this case walk up to the enclosing function, if there is
        ;; one.
        (when-let (fn (find-enclosing 'function-ast software ast))
          (find-preceding 'comment-ast software fn)))))

(defgeneric definition-name (ast)
  (:documentation "Return a string that is the name of the things
defined by a definition.  Return NIL if AST is not a definition.")
  (:method ((ast t)) nil))

(defgeneric declarator-name (ast)
  (:documentation "Returns a string that is the name of a things
declared in a declarator, or in the first element of a list of declarators.
Return NIL on the empty list.")
  (:method ((ast null)) nil)
  (:method ((ast list))
    (declarator-name (car ast))))

(defgeneric enclosing-definition (sw ast)
  (:documentation "Find the enclosing definition AST in which AST resides,
or NIL if none."))

(defgeneric imports (software ast &key)
  (:documentation "Return a list of the imports available in SOFTWARE at AST.
Every element in the list has the following form:
    (full-name alias/nickname named-imports)"))

(defgeneric provided-by (software ast)
  (:documentation
   "Return the library, package, or system in SOFTWARE providing AST."))

(defgeneric comparisonp (ast)
  (:documentation "Is AST a comparison?")
  (:method ((ast t)) nil))

(defgeneric evaluation-order-children (ast)
  (:documentation
   "Return the children of AST in the order they are evaluated.")
  (:method (ast) (children ast)))

(defmethod get-unbound-vals ((obj normal-scope) (ast ast)
                             &key local-declarations)
  (labels ((function-call-identifier-p
               (identifier &aux (parent (parent (genome obj) identifier)))
             "Return T if identifier is the function identifier in a
              function call."
             (and (typep parent 'call-ast)
                  (eq identifier (call-function parent))))
           (unbound-val-p (identifier)
             "Return T if IDENTIFIER is unbound in LOCAL-SCOPES and
              isn't the function identifier in a function call."
             (and
              (not (or (member (source-text identifier) local-declarations
                               :key #'source-text
                               :test #'equal)
                       (function-call-identifier-p identifier)))
              (variable-use-p obj identifier)))
           (remove-declaration-identifiers
               (unbound-identifiers local-declarations)
             "Return UNBOUND-IDENTIFIERS with all ASTs present in
              LOCAL-DECLARATIONS removed from it."
             (remove-if
              (lambda (identifier)
                ;; unbound-identifiers and local-declarations are lists ASTs of
                ;; type identifier-ast. Any unbound identifier which is used
                ;; in a declaration should not be considered unbound.
                (member identifier local-declarations :test #'eq))
              unbound-identifiers))
           (get-unbound-children ()
             "Return all unbound vals in the children of AST."
             (iter
               (for locals first
                    (append local-declarations (inner-declarations ast))
                    then (append locals (outer-declarations child)))
               ;; NOTE: use evaluation order here for cases where
               ;;       variable shadowing may become an issue.
               (for child in (evaluation-order-children ast))
               (appending
                   (get-unbound-vals obj child :local-declarations locals)
                 into unbound-children)
               (finally
                (return (remove-declaration-identifiers
                         unbound-children locals))))))
    (if (and (typep ast 'identifier-ast) (unbound-val-p ast))
        (list ast)
        (get-unbound-children))))

(defgeneric variable-use-p (obj identifier &key &allow-other-keys)
  (:documentation "Return T if IDENTIFIER occurs in OBJ as a variable."))

(defgeneric collect-var-uses (obj identifier &key &allow-other-keys)
  (:documentation "Collect uses of IDENTIFIER in OBJ.")
  (:method ((obj normal-scope) (identifier identifier-ast)
            &key &aux after-decl-flag)
    (labels ((initial-declaration-p ()
               "Return T the first time this is called per collect-var-uses
                call."
               (when (not after-decl-flag)
                 (setf after-decl-flag t)))
             (contains-identifier-p (declarations)
               "Return T if DECLARATIONS contains a declaration with the
                same name as identifier."
               (member (source-text identifier) declarations
                       :test #'equal :key #'source-text))
             (variable-shadowed-p (declarations)
               "Return T if the variable is shadowed by a declaration
                in DECLARATIONS."
               (and (contains-identifier-p declarations)
                    ;; This is a hack to get around the original decl of the
                    ;; variable. This is caused by collect-var-use-children
                    ;; starting at the enclosing scope and
                    ;; outer/inner-declarations not returning enough information
                    ;; to determine if the decl is the original one.
                    (not (initial-declaration-p))))
             (get-declaration-ast ()
               "Get the declaration AST associated with identifier."
               (or
                ;; Check if this identifier is part of a declaration before
                ;; checking scopes to avoid returning a shadowed variable.
                (iter
                  (for parent in (get-parent-asts* obj identifier))
                  (when (member identifier
                                (append (outer-declarations parent)
                                        (inner-declarations parent))
                                ;; Looking for the exact AST.
                                :test #'eq)
                    (return parent)))
                (aget :decl
                      (find-if-in-scopes
                       {equal (source-text identifier)}
                       (scopes obj identifier)
                       :key {aget :name}))))
             (collect-var-use-children (ast parents)
               "Return all variable uses in the children of AST."
               (cond
                 ((and (typep ast 'identifier-ast)
                       (variable-use-p obj ast :parents parents)
                       (equal (source-text identifier) (source-text ast)))
                  (list ast))
                 ((variable-shadowed-p (inner-declarations ast)) nil)
                 (t
                  (iter
                    ;; NOTE: use evaluation order here for cases where
                    ;;       variable shadowing may become an issue.
                    (for child in (evaluation-order-children ast))
                    (appending
                     (collect-var-use-children child (cons ast parents)))
                    (until (variable-shadowed-p (outer-declarations child))))))))
      (when-let ((declaration-ast (get-declaration-ast)))
        (remove-if-not
         (op (path-later-p obj _ declaration-ast))
         (collect-var-use-children
          (enclosing-scope obj declaration-ast) nil))))))


;;;; Cross-language generics and methods
(defgeneric end-of-parameter-list (software node)
  (:documentation "Find the end position of the parameters of FUNCTION-NODE.")
  (:method (software (node function-ast))
    (error "END-OF-PARAMETER-LIST undefined for ~a" (type-of node))))

(defgeneric function-name (node)
  (:documentation "Extract the name of the function from NODE.
If NODE is not a function node, return nil.")
  (:method ((node t)) nil)
  (:method ((node function-ast) &aux (type (type-of node)))
    (unless (subtypep type 'lambda-ast)
      (warn "FUNCTION-NAME undefined for ~a" type))
    nil))

(defgeneric field-name (node)
  (:documentation "Extract the name (as a string) of a field from NODE.
If NODE is not a thing that has fields, return nil.")
  (:method ((node t)) nil)
  (:method :around ((node t))
    (let ((result (call-next-method)))
      (if (typep result 'tree-sitter-ast)
          (source-text result)
          result))))

(defgeneric type-in (software ast)
  (:documentation "Return the type of AST in SOFTWARE."))

(defmethod is-stmt-p ((ast statement-ast)) t)


;;;; Structured text
;;; TODO: remove this; it's for debugging.
(defmacro labels+ ((&rest forms) &body body)
  `(progn
     ,@(iter
         (for form in forms)
         (collect `(declaim (notinline ,(car form))))
         (collect (cons 'defun form)))
     ,@body))

(defun children-parser (ast pruned-rule slots &aux (child-stack-key '#.(gensym)))
  "Return the children of AST in order based on PRUNED-RULE. SLOTS specifies
which slots are expected to be used."
  ;; TODO: quit copying hash tables and use indices into them or find a
  ;;       different way altogether if this method performs poorly since
  ;;       it's very hacky right now. It may be best to do the following note.

  ;; NOTE: this does not back track; not sure if this will be a problem,
  ;;       but there aren't any rules that obviously require it--this may
  ;;       be the case if a rule is a subtype of another.
  ;; NOTE: esrap or smug may be useful if needed in the future.

  ;; TODO:
  ;;     Paul R.'s suggestion:
  ;;     I think FSet maps would be a good match for what you're doing here.
  ;;     Possibly in concert with box if you don't want to be bothered passing
  ;;     the map around.
  (labels ((conflict-ast-substitution (conflict-ast)
             "Retrieve a value from CONFLICT-AST which can be used to match on."
              (some #'cadr (conflict-ast-child-alist conflict-ast)))
           (get-matchable-value (value)
             "Get a value that can be matched on by the tree-sitter rules."
             (if (typep value 'conflict-ast)
                 (conflict-ast-substitution value)
                 value))
           (populate-slot->stack ()
             "Create a hash table that maps a slot name to its
              corresponding stack."
             (alist-hash-table
              (mapcar
               (lambda (slot)
                 (let* ((slot-value (slot-value ast slot)))
                   (cons slot (and slot-value (ensure-cons slot-value)))))
                 slots)))
           (identical-slot-stacks-p (slot->stack1 slot->stack2)
             "Return T if the slot stacks in slot->stack are identical
              except the child stack."
             (iter
               (for (slot stack) in-hashtable slot->stack1)
               (always (or (eql slot child-stack-key)
                           (eq stack (gethash slot slot->stack2))))))
           (push-child-stack (value slot->stack)
             "Push VALUE onto the child stack in SLOT->STACK."
             (push value (gethash child-stack-key slot->stack)))
           (trim-slot-stack (slot slot->stack)
             "Removes one item from SLOT's stack in SLOT->STACK and returns
              a copy of SLOT->STACK with this change."
             (let ((copy (copy-hash-table slot->stack)))
               (symbol-macrolet ((slot-hash (gethash slot copy)))
                 (push-child-stack (car slot-hash) copy)
                 (setf slot-hash (cdr slot-hash))
                 copy)))
           (handle-child (rule slot->stack)
             (when (typep (get-matchable-value
                           (car (gethash 'children slot->stack)))
                          ;; Treat source-text-fragment as a wild card.
                          (append '(or source-text-fragment) (cdr rule)))
               (trim-slot-stack 'children slot->stack)))
           (handle-field (rule slot->stack &aux (slot (cadr rule)))
             (when (typep (get-matchable-value
                           (car (gethash slot slot->stack)))
                          (append '(or source-text-fragment) (cddr rule)))
               (trim-slot-stack slot slot->stack)))
           (handle-slot (rule slot->stack &aux (slot (cadr rule)))
             (ensure-gethash slot slot->stack (slot-value ast slot))
             (trim-slot-stack slot slot->stack))
           (handle-choice (rule slot->stack)
             ;; NOTE: since this doesn't back track it won't work on certain
             ;;       rules. Currently, I'm not aware of any rules that
             ;;       need back tracking, so I'm ignoring this for now.

             ;; NOTE: this marks which branch was taken by adding a
             ;;       list of (:choice branch-num) into the children list.
             (if (every #'null (cdr rule))
                 ;; If every branch is nil, assume a match on the first one.
                 (lret ((copy (copy-hash-table slot->stack)))
                   (push-child-stack `(:choice 0) copy))
                 (iter
                   (iter:with empty-match?)
                   (for branch in (cdr rule))
                   (for i upfrom 0)
                   (cond
                     (branch
                      (for copy = (copy-hash-table slot->stack))
                      (push-child-stack `(:choice ,i) copy)
                      (when-let ((matched? (rule-handler branch copy)))
                        (leave matched?)))
                     (t (setf empty-match? i)))
                   (finally
                    (return
                      ;; Handles the case where it matched on an empty branch.
                      (when-let ((copy (and empty-match?
                                            (copy-hash-table slot->stack))))
                        (push-child-stack `(:choice ,empty-match?)
                                          copy)
                        copy))))))
           (handle-repeat (rule slot->stack
                           &optional continuation?
                           &aux (copy (copy-hash-table slot->stack)))
             ;; NOTE: since this doesn't back track it won't work on certain
             ;;       rules. Currently, I'm not aware of any rules that
             ;;       need back tracking, so I'm ignoring this for now.

             ;; NOTE: this marks the beginning of a repeat with (:repeat),
             ;;       every subsequent iteration of that repeat with a
             ;;       (:continue), and then end of the repeat with (:end-repeat)
             (push-child-stack
              (if continuation? '(:continue) '(:repeat))
              copy)
             (let ((repeat-slot->stack (rule-handler (cadr rule) copy)))
               (cond
                 ((and repeat-slot->stack
                       ;; Prevent infinite recursion on a nested, empty repeat.
                       (not (eq copy repeat-slot->stack))
                       ;; Check if they are the same except the child-stack.
                       (not (identical-slot-stacks-p
                             slot->stack repeat-slot->stack)))
                  (handle-repeat rule repeat-slot->stack t))
                 (t
                  ;; NOTE: this should only triggered once when the
                  ;;       repeat can't procede.
                  (push-child-stack '(:end-repeat) slot->stack)
                  slot->stack))))
           (handle-seq (rule slot->stack &aux (seq (cdr rule)))
             (iter
               (for subrule in seq)
               (unless subrule (next-iteration))
               (for sub-slot->stack first (rule-handler subrule slot->stack)
                    then (rule-handler subrule sub-slot->stack))
               (always sub-slot->stack)
               (finally
                (return
                  (cond
                    ((eql t sub-slot->stack) slot->stack)
                    (sub-slot->stack)
                    (t slot->stack))))))
           (rule-handler (rule slot->stack)
             "Handles dispatching RULE to its relevant rule handler."
             (ecase (car rule)
               (:CHOICE (handle-choice rule slot->stack))
               (:REPEAT (handle-repeat rule slot->stack))
               (:FIELD (handle-field rule slot->stack))
               (:CHILD (handle-child rule slot->stack))
               (:SEQ (handle-seq rule slot->stack))
               (:SLOT (handle-slot rule slot->stack))))
           (slot-stacks-empty-p (slot->stack)
             "Return T if SLOT->STACK doesn't have any slots that
              map to a non-empty stack."
             (every #'null
                    (maphash-return
                     (lambda (slot stack)
                       (unless (eq slot child-stack-key)
                         stack))
                     slot->stack))))
    (let ((slot->stack (rule-handler pruned-rule (populate-slot->stack))))
      (if (and slot->stack (slot-stacks-empty-p slot->stack))
          (reverse (gethash child-stack-key slot->stack))
          (error 'rule-matching-error
                 :rule-matching-error-rule pruned-rule
                 :rule-matching-error-ast ast)))))

(defun computed-text-output-transformation (ast)
  "Gives the variable text output transformation for AST. This
representation is interleaved text though it's unlikely to
be more than one string outside of string literals."
  (flatten
   (list
    (before-text ast)
    (or (slot-value ast 'children)
        (text ast))
    (after-text ast))))

(defun match-parsed-children-json
    (json-rule parse-tree &aux (immediate-token-count 0))
  "Match a cl-tree-sitter PARSE-TREE as a JSON-RULE if possible.
Returns as values the updated parse tree, whether it matched, and
the number of immediate tokens encountered."
  ;; NOTE: this could be expanded to match on the string too
  ;;       though the current representation of transformed json
  ;;       rules and pruned rules likely wouldn't benefit from it.
  (labels ((handle-alias (rule tree &aux (alias (car (car tree))))
             (cond
               ((aget :named rule)
                ;; Named aliases are unhandled by #'match-parsed-children-json.
                (error "Named alias in JSON subtree"))
               (t
                (and (string-equal (if (consp alias)
                                       (cadr alias)
                                       alias)
                                   (aget :value rule))
                     (values (cdr tree) t)))))
           (handle-blank (tree) (values tree t))
           (handle-choice (rule tree)
             (mvlet* ((branches (aget :members rule))
                      (blank not-blank
                       (partition (lambda (rule)
                                    (equal "BLANK" (aget :type rule)))
                                  branches))
                      ;; Always try rules other than blanks first.
                      (branches
                       (append not-blank blank)))
               (iter
                (for branch in branches)
                (for (values result matched?) = (rule-handler branch tree))
                (when matched?
                  (leave (values result t))))))
           (handle-repeat (rule tree)
             ;; NOTE: since this doesn't back track it won't work on certain
             ;;       rules. Currently, I'm not aware of any rules that
             ;;       need back tracking, so I'm ignoring this for now.
             (mvlet ((result matched? (rule-handler (aget :content rule) tree)))
               (cond
                 ((and matched?
                       ;; Prevent infinite recursion. This will probably
                       ;; never be an issue for JSON matching.
                       (not (eq tree result)))
                  (handle-repeat rule result))
                 (t (values tree t)))))
           (handle-seq (rule tree)
             (iter
               (for child in (aget :members rule))
               (for (values result matched?)
                    first (rule-handler child tree)
                    then (rule-handler child result))
               (unless matched?
                 (leave))
               (finally (return (values result t)))))
           (handle-string (rule tree &aux (token (car tree)))
             (when (and (consp token)
                        (atom (car token))
                        (string-equal (car token) (convert-name (aget :value rule))))
               (values (cdr tree) t)))
           (handle-token (rule tree &aux (content (aget :content rule)))
             (when (equal (aget :type rule) "IMMEDIATE_TOKEN")
               (incf immediate-token-count))
             (cond
               ((equal "STRING" (aget :type content))
                (rule-handler content tree))
               (t (values tree t))))
           (rule-handler (rule tree)
             "Handles dispatching RULE to its relevant rule handler."
             ;; NOTE: this will throw an error if an unexpected rule is
             ;;       encountered.
             (string-ecase (aget :type rule)
               ("ALIAS" (handle-alias rule tree))
               ("BLANK" (handle-blank tree))
               ("CHOICE" (handle-choice rule tree))
               ;; TODO: token rules are handled differently if they only
               ;;       produce one token.
               (("IMMEDIATE_TOKEN" "TOKEN") (handle-token rule tree))
               ("REPEAT" (handle-repeat rule tree))
               ("SEQ" (handle-seq rule tree))
               ("SLOT" (values tree t))
               ("STRING" (handle-string rule tree)))))
    (mvlet ((tree matched? (rule-handler json-rule parse-tree)))
      (values tree matched? immediate-token-count))))

(defun match-parsed-children
    (language-prefix json-rule pruned-rule child-types parse-tree)
  "Match a cl-tree-sitter PARSE-TREE as a PRUNED-RULE in LANGUGE-PREFIX.
CHILD-TYPES is a list of lisp types that the children slot can contain.
Returns as values whether the match succeeded and if so, returns a list
specifying how to populate the inner-asts slots--symbols indicate a slot
to store the next grouping of ASTs on the inner-asts stack while a number
indicates the number of groupings to drop from the stack."
  (labels ((remove-ignorable-items (tree)
             "Remove ignorable items from PARSE-TREE. This includes comments,
              errors, and internal ast slots."
             (when tree
               `(,(car tree)
                 ,(cadr tree)
                 ,(mapcar
                   #'remove-ignorable-items
                   (remove-if
                    (op (member (car _) '(:comment :error :inner-whitespace)))
                    (caddr tree))))))
           (get-children ()
             "Get the children slots and their types from parse-tree."
             (iter
               (for child in (caddr parse-tree))
               (for slot-pair = (car child))
               (for child-type = (unless (listp slot-pair)
                                   (convert-to-lisp-type
                                    language-prefix slot-pair)))
               (cond
                 ((not child-type)
                  (collect
                      (list
                       (convert-to-lisp-type
                        language-prefix (car slot-pair))
                       (convert-to-lisp-type
                        language-prefix (cadr slot-pair)))))
                 ((subtypep child-type 'comment-ast))
                 ((member child-type child-types :test #'subtypep)
                  (collect (convert-to-lisp-type language-prefix slot-pair))))))
           (handle-child (rule parse-stack inner-asts-order
                          &aux (child (car (car parse-stack))))
             (cond
               ((and (atom child)
                     (not (null child))
                     ;; Confirm tree is the relevant thing on the stack.
                     (member (convert-to-lisp-type language-prefix child)
                             ;; Treat source-text-fragment as a wild card.
                             (cons 'source-text-fragment
                                   (cdr rule))
                             :test #'subtypep))
                (values (cdr parse-stack) t inner-asts-order))
               ;; This is an edge case for rules that allow null children.
               ((member 'null (cdr rule))
                (values parse-stack t inner-asts-order))))
           (handle-field (rule parse-stack inner-asts-order
                          &aux (parsed-field (car parse-stack))
                            (field-pair (and (consp parsed-field)
                                             (car parsed-field))))
             ;; Must handle field that isn't provided but has null.
             (cond
               ((and (consp field-pair)
                     (eql (cadr rule)
                          (convert-to-lisp-type
                           language-prefix (car field-pair)))
                     (member
                      (convert-to-lisp-type
                       language-prefix (cadr field-pair))
                      ;; Treat source-text-fragment as a wild card.
                      (cons 'source-text-fragment
                            (cddr rule))
                      :test #'subtypep))
                (values (cdr parse-stack) t inner-asts-order))
               ;; This is an edge case for a field that allows nil.
               ((member 'null (cddr rule))
                (values parse-stack t inner-asts-order))))
           (handle-slot (rule parse-stack inner-asts-order)
             (values parse-stack t (cons (cadr rule) inner-asts-order)))
           (handle-choice (rule json parse-stack inner-asts-order)
             ;; NOTE: since this doesn't back track it won't work on certain
             ;;       rules. Currently, I'm not aware of any rules that
             ;;       need back tracking, so I'm ignoring this for now.
             (iter
               (for branch in (cdr rule))
               (for json-branch in (aget :members json))
               (for (values stack matched? inner-asts) =
                    (rule-handler
                     branch json-branch parse-stack inner-asts-order))
               (when matched?
                 (return (values stack t inner-asts)))))
           (handle-repeat (rule json parse-stack inner-asts-order)
             ;; NOTE: since this doesn't back track it won't work on certain
             ;;       rules. Currently, I'm not aware of any rules that
             ;;       need back tracking, so I'm ignoring this for now.
             (mvlet ((stack
                      matched?
                      inner-asts
                      (rule-handler
                       (cadr rule) (aget :content json) parse-stack
                       inner-asts-order)))
               ;; Prevent infinite recursion when the parse-stack is never
               ;; used.
               (if (and matched? (not (equal parse-stack stack)))
                   (handle-repeat rule json stack inner-asts)
                   (values parse-stack t inner-asts-order))))
           (handle-seq (rule json parse-stack inner-asts-order)
             (iter
               (for subrule in (cdr rule))
               (for json-subrule in (aget :members json))
               (for (values stack matched? inner-asts)
                    first (rule-handler
                           subrule json-subrule parse-stack inner-asts-order)
                    then (rule-handler
                          subrule json-subrule stack inner-asts))
               (always matched?)
               (finally (return (values stack t inner-asts)))))
           (rule-handler (rule json parse-stack inner-asts-order)
             "Handles dispatching RULE to its relevant rule handler."
             (ecase (car rule)
               (:CHOICE (handle-choice rule json parse-stack inner-asts-order))
               (:REPEAT (handle-repeat rule json parse-stack inner-asts-order))
               (:FIELD (handle-field rule parse-stack inner-asts-order))
               (:CHILD (handle-child rule parse-stack inner-asts-order))
               (:SEQ (handle-seq rule json parse-stack inner-asts-order))
               (:SLOT (handle-slot rule parse-stack inner-asts-order))
               ((nil)
                (mvlet ((stack
                         matched?
                         immediate-token-count
                         (match-parsed-children-json json parse-stack)))
                  (when matched?
                    (values
                     stack t (cons immediate-token-count inner-asts-order))))))))
    (cond
      ;; Prevent matching on an empty rule when there are children.
      ((or (not pruned-rule)
           (= 1 (length pruned-rule)))
       (not (get-children)))
      (pruned-rule
       (mvlet ((parse-stack
                success?
                inner-asts-order
                (rule-handler
                 pruned-rule
                 json-rule
                 (caddr (remove-ignorable-items
                         parse-tree))
                 nil)))
         ;; Avoid matching a rule if parse tree tokens still exist.
         (unless parse-stack
           (values success? (reverse inner-asts-order))))))))

;;;
;;; Primitive mutation types
;;;
(defclass tree-sitter-mutation (mutation)
  ()
  (:documentation "Specialization of the mutation interface for tree-sitter ast
 software objects."))

;;; Mutations
(defparameter *tree-sitter-mutation-types*
  (cumulative-distribution
   (normalize-probabilities
    '((tree-sitter-insert . 1)
      (tree-sitter-swap . 1)
      (tree-sitter-move . 1)
      (tree-sitter-replace . 1)
      (tree-sitter-cut . 1)
      (tree-sitter-nop . 1))))
  "Cumulative distribution of normalized probabilities of weighted mutations.")

(defparameter *max-targeter-old-tries* 1000
  "Max number of attempts to find a compatible ast.")

(defparameter *max-targeter-new-tries* 100
  "Max number of attempts to find a compatible ast for reuse.")

(defparameter *max-targeter-moveable-tries* 100
  "Max number of attempts to find a compatible moveable target ast pair.")

(defun evolution-candidate-ast-p (ast)
  "Returns true iff the ast is one which we will select for mutations."
  (not (or (typep ast 'inner-whitespace) (typep ast 'comment-ast))))

(defun evolution-candidate-asts (software
                                 &key (filter
                                   (lambda (x) (declare (ignore x)) t)))
  "Returns list of asts in software genome which are valid mutation candidates."
  (iter (for a in-tree (genome software))
    (when (and (evolution-candidate-ast-p a)
               (funcall filter a))
      (collect a))))

(defmethod mutation-targets ((software tree-sitter)
                             &key (filter nil) (stmt-pool nil))
  "Return a list of target ASTs from STMT-POOL for mutation.

* OBJ software object to query for mutation targets
* FILTER filter AST from consideration when this function returns nil
* STMT-POOL (non-empty) list of ASTs, or a method on OBJ returning a
   non-emptylist of ASTs, or NIL (to indicate default pool)"
  (cond ((consp stmt-pool)
         (if filter (remove-if-not filter stmt-pool) stmt-pool))
        ((null stmt-pool)
         (apply 'evolution-candidate-asts
                software (if filter `(:filter ,filter))))
        (t (if filter
               (remove-if-not filter (funcall stmt-pool software))
               (funcall stmt-pool software)))))

(defun pick-2-replaceable (software)
  (let* ((asts (evolution-candidate-asts software)))
    (do* ((old (random-elt asts) (random-elt asts))
          (new (tree-copy (random-elt asts)))
          (old-tries 0 (+ old-tries 1))
          (new-tries 0)
          (valid #1=(check-ast-replacement (genome software) old new) #1#))
         ((or valid (> new-tries *max-targeter-new-tries*))
          (if valid (list old new)))
      (if (> old-tries *max-targeter-old-tries*)
          (setf new (random-elt asts)
                new-tries (+ new-tries 1)
                old-tries 0)))))

(defun pick-2-insertable (software)
  (let* ((asts (evolution-candidate-asts software)))
    (do* ((old (random-elt asts) (random-elt asts))
          (new (tree-copy (random-elt asts)))
          (old-tries 0 (+ old-tries 1))
          (new-tries 0)
          (valid #1=(check-ast-insertable (genome software) old new) #1#))
         ((or valid (> new-tries *max-targeter-new-tries*))
          (if valid (list old new)))
      (if (> old-tries *max-targeter-old-tries*)
          (setf new (random-elt asts)
                new-tries (+ new-tries 1)
                old-tries 0)))))

(defun pick-2-swappable (software)
  "Return a random target of two asts which can be swapped in the software."
  (let* ((asts (evolution-candidate-asts software)))
    (do* ((ast1 (random-elt asts) (random-elt asts))
          (ast2 (random-elt asts))
          (ast1-tries 0 (+ ast1-tries 1))
          (ast2-tries 0)
          (valid #1=(check-ast-swappable (genome software) ast1 ast2) #1#))
         ((or valid (> ast2-tries *max-targeter-new-tries*))
          (if valid (list ast1 ast2)))
      (if (> ast1-tries *max-targeter-old-tries*)
          (setf ast2 (random-elt asts)
                ast2-tries (+ ast2-tries 1)
                ast1-tries 0)))))

(defun pick-2-moveable (software)
  (iter (for i from 1 to *max-targeter-moveable-tries*)
    (let* ((pick (pick-2-replaceable software)))
      ;; make sure the old can be cut
      (if (check-ast-cut (genome software) (first pick))
          (leave pick)))))

(defun pick-1-cuttable (software)
  (let* ((asts (evolution-candidate-asts software)))
    (do* ((old (random-elt asts) (random-elt asts))
          (old-tries 0 (+ old-tries 1))
          (valid #1=(check-ast-cut (genome software) old) #1#))
         ((or valid (> old-tries *max-targeter-old-tries*))
          (if valid (list old))))))

(define-mutation tree-sitter-replace (tree-sitter-mutation)
  ((targeter :initform #'pick-2-replaceable))
  (:documentation
   "Replace a randomly selected ast with a compatible ast from the same tree."))

(define-mutation tree-sitter-swap (tree-sitter-mutation)
  ((targeter :initform #'pick-2-swappable))
  (:documentation
   "Replace a randomly selected ast with a compatible ast from the same tree."))

(define-mutation tree-sitter-cut (tree-sitter-mutation)
  ((targeter :initform #'pick-1-cuttable))
  (:documentation
   "Replace a randomly selected ast with a compatible ast from the same tree."))

(define-mutation tree-sitter-insert (tree-sitter-mutation)
  ((targeter :initform #'pick-2-insertable))
  (:documentation
   "Replace a randomly selected ast with a compatible ast from the same tree."))

(define-mutation tree-sitter-move (tree-sitter-mutation)
  ((targeter :initform #'pick-2-moveable))
  (:documentation
   "Replace a randomly selected ast with a compatible ast from the same tree."))

(define-mutation tree-sitter-nop (tree-sitter-mutation) ())

(defmethod apply-mutation ((software tree-sitter)
                           (mutation tree-sitter-replace))
  (let ((target (targets mutation)))
    ;; need deep copy (tree-copy) to get new serial numbers on new ast
    ;; TODO: remove this tree-copy when functional-trees get copy-on-collision
    ;; support added.
    (setf (genome software)
          (with (genome software)
                (first target)
                (tree-copy (second target))))
    software))

(defmethod apply-mutation ((software tree-sitter)
                           (mutation tree-sitter-insert))
  (let ((target (targets mutation)))
    ;; need deep copy (tree-copy) inserted ast to get new serial numbers
    ;; TODO: remove this tree-copy when functional-trees get copy-on-collision
    ;; support added.
    (setf (genome software)
          (insert (genome software)
                (first target)
                (tree-copy (second target))))
    software))

(defmethod apply-mutation ((software tree-sitter) (mutation tree-sitter-swap))
  (let ((target (targets mutation)))
    ;; since we are doing a swap we don't need a deep copy of either mutation
    (setf (genome software)
          (swap
           (genome software)
                (first target)
                (second target)))
    software))

(defmethod apply-mutation ((software tree-sitter) (mutation tree-sitter-move))
  (let ((target (targets mutation)))
    ;; since we are doing a swap we don't need a deep copy of either mutation
    (setf (genome software)
          ;; first, remove the ast from its current location, then add it back
          ;; in at new location
          (with
           (less
            (genome software)
            (second target))
           (first target)
           (second target)))
    software))

(defmethod apply-mutation ((software tree-sitter) (mutation tree-sitter-cut))
  (let ((target (targets mutation)))
    (setf (genome software)
          (less (genome software)
                (first target)))
    software))

(defmethod apply-mutation ((software tree-sitter) (mutation tree-sitter-nop))
  (declare (ignorable software mutation))
  software)

(defmethod pick-mutation-type ((obj tree-sitter))
  "Select type of mutation to apply to OBJ."
  (random-pick *tree-sitter-mutation-types*))

(defgeneric whitespace-between/parent (parent style ast1 ast2)
  (:method ((parent t) style ast1 ast2)
    (whitespace-between style ast1 ast2))
  ;; No whitespace inside a terminal symbol.
  (:method ((parent terminal-symbol) s ast1 ast2)
    "")
  ;; No whitespace after a terminal symbol in a unary AST.
  (:method ((parent unary-ast) s (ast1 terminal-symbol) ast2)
    "")
  (:method (p s (ast1 ast) (ast2 string))
    (if (notevery #'whitespacep ast2)
        (whitespace-between/parent p s ast1 (make-keyword ast2))
        ""))
  (:method (p s (ast1 string) (ast2 ast))
    (if (notevery #'whitespacep ast1)
        (whitespace-between/parent p s (make-keyword ast1) ast2)
        ""))
  (:method (p s (ast1 inner-whitespace) ast2)
    "")
  (:method (p s ast1 (ast2 inner-whitespace))
    "")
  (:method ((parent computed-text) s ast1 ast2) ""))

(defgeneric whitespace-between (style ast1 ast2)
  (:method (s ast1 ast2) "")
  (:method (s (ast1 null) ast2) "")
  (:method (s (ast1 (eql :||)) ast2) "")
  (:method (s ast1 (ast2 null)) "")
  (:method (s ast1 (ast2 (eql :||))) "")
  ;; Sensible defaults for most (all?) programming languages.
  (:method (s (ast1 symbol) (ast2 ast))
    "No whitespace after an opening delimiter or blank."
    (if (some (op (string$= _ ast1))
              #.(string+ "([{" whitespace))
        "" " "))
  (:method (s (ast1 null) (ast2 ast)) "")
  (:method (s (ast1 ast) (ast2 symbol))
    "No whitespace before a closing delimiter or a comma."
    (if (some (op (string^= _ ast2)) ")]},") "" " "))
  (:method (s (ast1 ast) (ast2 null)) "")
  (:method (s (ast1 ast) (ast2 ast)) " ")
  (:method (s (ast1 inner-whitespace) ast2) "")
  (:method (s ast1 (ast2 inner-whitespace)) "")
  (:documentation "Return a string of whitespace that should occur between
AST1 and AST2.

STYLE can be used to control whitespace based on a standard format or
on the calculated format of a particular file."))

(defgeneric get-style-indentation (style software ast &key)
  (:method (style software (ast indentation) &key) (indent-children ast))
  (:documentation "Return a the indent-children value for AST in SOFTWARE for
STYLE."))

(defgeneric get-style-indentation-adjustment
    (style software ast parent &key parents)
  (:method (style software (ast indentation) parent &key parents)
    (declare (ignorable parents))
    (indent-adjustment ast))
  (:documentation "Return a value to set the indent-adjustment slot of AST."))

(defun empty-sequence-p (x)
  "Return T if X is an empty sequence."
  (and (sequencep x)
       (emptyp x)))

(defun copy-with-surrounding-text (copy-node reference-node)
  "Copy COPY-NODE with the surrounding text of REFERENCE-NODE. This is done
on a per slot basis, and the copy of text only happens if the relevant slot
doesn't already have a non-empty value and the value in REFERENCE-NODE isn't
also empty. This will prevent unnecessary copying."
  (let* ((before-copy (before-text copy-node))
         (after-copy (after-text copy-node))
         (before-reference (before-text reference-node))
         (after-reference (after-text reference-node))
         (empty-before-copy (empty-sequence-p before-copy))
         (empty-after-copy (empty-sequence-p after-copy))
         (not-empty-before-reference
          (not (empty-sequence-p before-reference)))
         (not-empty-after-reference
          (not (empty-sequence-p after-reference))))
    (cond
      ((and empty-before-copy empty-after-copy
            not-empty-before-reference not-empty-after-reference)
       (copy copy-node :before-text before-reference
                       :after-text after-reference))
      ((and empty-before-copy not-empty-before-reference)
       (copy copy-node :before-text before-reference))
      ((and empty-after-copy not-empty-after-reference)
       (copy copy-node :after-text after-reference))
      (t copy-node))))

(defmethod with :around ((ast structured-text) (value1 structured-text)
                         &optional value2)
  (if  (typep value2 'structured-text)
       (call-next-method ast value1 (copy-with-surrounding-text value2 value1))
       (call-next-method)))

(defmethod with :around ((ast structured-text) (value1 list) &optional value2)
  (if-let* ((structuredp (typep value2 'structured-text))
            (old (@ ast value1))
            (structuredp (typep old 'structured-text)))
    (call-next-method ast value1 (copy-with-surrounding-text value2 old))
    (call-next-method)))

(defgeneric patch-whitespace (ast &key)
  (:documentation "Destructively patch whitespace on AST by adding a
  space to the before-text and after-text slots that need it.")
  (:method ((ast t) &key) ast)
  (:method ((ast terminal-symbol) &key) ast)
  (:method ((ast structured-text) &key style software prettify (recursive t))
    "Destructively patch whitespace on AST by adding a
space to the before-text and after-text slots that need it.

:software can be provided for to help determine indentation that
depends on parent ASTS.

:prettify will always set the relevant before/after text as opposed to only
setting it if it isn't already set."
    ;; TODO: add functionality to create inner-whitespace objects and store them
    ;;       in the relevant inner-asts slot. This will be useful for some ASTs
    ;;       which have two terminals in a row that require whitespace between
    ;;       them, such as python-yield with 'yield' and 'from'.
    (labels ((update-indentation (ast parents)
               "Update the indentation of AST."
               (let ((indentation (get-style-indentation style software ast))
                     (indentation-adjustment
                       (get-style-indentation-adjustment
                        style software ast (car parents) :parents parents)))
                 (symbol-macrolet ((indent-children (indent-children ast))
                                   (indent-adjustment (indent-adjustment ast)))
                   (cond-every
                     ((or prettify
                          (and indentation (not indent-children)))
                      (setf indent-children indentation))
                     ((or prettify
                          (and indentation-adjustment (not indent-adjustment)))
                      (setf indent-adjustment indentation-adjustment))))))
             (patch-whitespace (ast parents)
               (update-indentation ast parents)
               (iter
                 (for item in (cdr (butlast (output-transformation ast))))
                 (for previous-item previous item)
                 (for white-space =
                      (whitespace-between/parent ast
                                                 style
                                                 previous-item item))
                 (when (and recursive
                            (typep item '(and tree-sitter-ast
                                          (not terminal-symbol)))
                            (not (computed-text-node-p ast)))
                   (patch-whitespace item (cons ast parents)))
                 (cond
                   ((and (not prettify) (emptyp white-space)))
                   ((typep item 'structured-text)
                    (when (or prettify (emptyp (before-text item)))
                      (if (typep previous-item 'structured-text)
                          (when (emptyp (after-text previous-item))
                            (setf (before-text item) white-space))
                          (setf (before-text item) white-space))))
                   ((typep previous-item 'structured-text)
                    (when (or prettify (emptyp (after-text previous-item)))
                      (setf (after-text previous-item) white-space))))
                 (finally (return ast)))))
      (declare (dynamic-extent #'patch-whitespace))
      (patch-whitespace ast (and software (get-parent-ast software ast))))))

(defgeneric prettify-software (style software &key ast)
  (:documentation "Return a copy of SOFTWARE with its whitespace inserted based
on STYLE.

:AST can be provided to prettify an AST locally. It will still patch its
parent in the cases where the parent indentation changes.")
  (:method (style software &key (ast (genome software)))
      (lret ((ast-path (ast-path software ast))
             (new-software (copy software :genome (tree-copy (genome software)))))
        (when-let ((parent (lookup new-software (butlast ast-path))))
          (setf (indent-children parent)
                (get-style-indentation style software parent)))
        (patch-whitespace
         (lookup new-software ast-path)
         :recursive t :prettify t :software new-software :style style))))

(defmacro define-empty-whitespace-methods ((&optional (style t))
                                           &body pairs)
  `(progn
     ,@(iter (for (x y) in (batches pairs 2 :even t))
             (let ((x (if (keywordp x) `(eql ,x) x))
                   (y (if (keywordp y) `(eql ,y) y)))
               (collect `(defmethod whitespace-between ((style ,style)
                                                        (x ,x)
                                                        (y ,y))
                           ""))))))

(define-empty-whitespace-methods (t)
  identifier-ast parameters-ast
  identifier-ast arguments-ast)

(defgeneric prefer-child-indentation-p (ast)
  (:method (ast) nil)
  (:documentation " Return t if AST is a type that prefers its indentation be
attached to its indent-children slot as opposed to its parent's. This is
primarily for process-indentation and working around python-block which is a
unique edge case."))

(defun process-indentation (root &aux indentation-carryover indentation-ast)
  "Process the indentation of ROOT such that indentation information is stored in
the indentation slots."
  (labels ((adjusted-spaces-from-tabs
               (subseq &aux (tab-count (count #\tab subseq)))
             "Return the number of spaces that are used for tabs minus
              the number of tabs."
             (- (* tab-count *spaces-per-tab*)
                tab-count))
           (starts-with-indentation-p (string)
             "If STRING starts with indentation, return
              the first position without indentation."
             (when indentation-carryover
               (mvlet ((start end (scan "^[ \\t]+" string)))
                 (declare (ignorable start))
                 end)))
           (ends-with-newline-p (string)
             "If STRING ends with a newline and optionally indentation,
              return the position of the newline."
             (iter
               (for i from (1- (length string)) downto 0)
               (for character = (aref string i))
               (when (eql character #\newline)
                 (return i))
               (while (member character '(#\space #\tab)))))
           (backpatch-indent-adjustment-slots (asts indentation current-ast)
             "Backpatch items in ASTS such that any item before CURRENT-AST
              has its indent-adjustment slot set to INDENTATION less than its
              current value."
             ;; NOTE: this function is necessary for erratic indentation that
             ;;       is heavily nested in an AST.
             (mapc
              (lambda (ast)
                ;; The idea here is that, after we have transferred
                ;; indentation *i* from the before-text of child *n*
                ;; to the parent, we need to go back and remove *i*
                ;; spaces of indentation from the children 0..n-1.
                ;; If there is no explicit indentation adjustment on a
                ;; child, assume the current value is 0.
                (symbol-macrolet ((indent-adjustment (indent-adjustment ast)))
                  (if indent-adjustment
                      (decf indent-adjustment indentation)
                      (setf indent-adjustment (- indentation)))))
              (ldiff asts (member current-ast asts))))
           (backpatch-indent-children-slots (ast indentation)
             "Backpatch any child of AST that already has a value in the
              indent-children slot such that its value is INDENTATION less
              than its current."
             ;; NOTE: this function is necessary for erratic indentation that
             ;;       is heavily nested in an AST.
             (mapc
              (lambda (child)
                (symbol-macrolet ((indent-children (indent-children child)))
                  (when indent-children
                    (setf indent-children (- indent-children indentation)))))
              (children ast)))
           (update-indentation-slots
               (ast parents indentation text
                &aux (parent (car parents))
                  (adjusted-indentation
                   ;; total - inherited
                   (- (+ indentation indentation-carryover)
                      (get-indentation-at ast parents)))
                  (only-indentation? (not (scan "[^ \\t\\n]" text))))
             "Patch either AST or PARENT to have INDENTATION for the
              relevant line or lines."
             (symbol-macrolet ((indent-children-parent (indent-children parent))
                               (indent-adjustment (indent-adjustment ast))
                               (indent-children-current (indent-children ast)))
               (cond
                 ;; Avoid wasting the newline on empty text or indentation
                 ;; before reaching a child.
                 ((and only-indentation?
                       indentation-ast
                       (ancestor-of-p root indentation-ast ast)))
                 ;; Don't indent if the current AST already has an
                 ;; indentation slot assigned as this will result in
                 ;; back-propagation of indentation.
                 (indent-children-current
                  ;; In this case, we need to reset the indentation if
                  ;; any non-indentation is in this string since the reset
                  ;; will not happen anywhere after this.
                  ;;
                  ;; NOTE: because of this, certain forms, such as compound
                  ;;       statements which represent pairs of curly braces, will
                  ;;       not be reproduced exactly if they don't line up
                  ;;       with each other when on their own lines. There is no
                  ;;       way around this besides having ASTs that represent the
                  ;;       individual tokens or adding another list to keep track
                  ;;       of indentation at the interleaved-text level.
                  (unless only-indentation?
                    (setf indentation-carryover nil
                          indentation-ast nil)))
                 ((prefer-child-indentation-p ast)
                  (backpatch-indent-children-slots ast adjusted-indentation)
                  (setf indent-children-current adjusted-indentation
                        indentation-carryover nil
                        indentation-ast nil))
                 ((and parent (not indent-children-parent))
                  (backpatch-indent-adjustment-slots
                   (children parent) adjusted-indentation ast)
                  (setf indent-children-parent adjusted-indentation
                        indentation-carryover nil
                        indentation-ast nil))
                 ;; Backpatching shouldn't be needed for the following.
                 (t (setf indent-adjustment adjusted-indentation
                          indentation-carryover nil
                          indentation-ast nil)))))
           (patch-leading-indentation
               (text ast parents
                &key before-text
                &aux (indentation (starts-with-indentation-p text))
                  (not-empty-string-p (not (emptyp text))))
             "Return TEXT with the leading indentation removed and
              the relevant indentation slot updated."
             (cond-let leading-indentation
               ((and indentation
                     (= indentation (length text))
                     not-empty-string-p)
                (setf indentation-carryover
                      (+ indentation-carryover
                         indentation
                         (adjusted-spaces-from-tabs
                          (subseq text 0 indentation))))
                "")
               ((and not-empty-string-p
                     (ends-with-newline-p text))
                (prog1
                    ;; Treat it like trailing indentation.
                    (patch-trailing-indentation text ast)
                  ;; But update it if it's before-text.
                  (when before-text
                    (update-indentation-slots
                     ast parents (adjusted-spaces-from-tabs
                                  (subseq
                                   text (position #\newline text :from-end t)))
                     text))))
               ((or indentation
                    ;; NOTE: check if text exists here so that
                    ;;       the inherited indentation can be
                    ;;       set to 0. This prevents back-propagation
                    ;;       of indentation to previous siblings.
                    (and indentation-carryover
                         ;; TODO: add carriage return here?
                         (scan "[^ \\t\\n]" text)))
                (update-indentation-slots
                 ast parents (+ leading-indentation
                                (adjusted-spaces-from-tabs
                                 (subseq text 0 leading-indentation)))
                 text)
                (subseq text leading-indentation))
               (t text)))
           (patch-trailing-indentation (text ast)
             "Return TEXT with the trailing indentation removed and
              indentation-carryover updated."
             (cond-let trailing-indentation
               ((ends-with-newline-p text)
                (setf indentation-carryover
                      (+ (- (length text) (1+ trailing-indentation))
                         (adjusted-spaces-from-tabs
                          (subseq text trailing-indentation)))
                      indentation-ast ast)
                (subseq text 0 (1+ trailing-indentation)))
               (t text)))
           (patch-internal-indentation (text)
             "Return TEXT where all newlines proceeded by indentation
              are replaced with a newline."
             (cl-ppcre:regex-replace-all "\\n[ \\t]+" text #.(format nil "~%")))
           (patch-text (text ast parents)
             "Patch TEXT such that it useable for inherited indentation.
              Updates AST and PARENTS slots if necessary."
             (patch-trailing-indentation
              (patch-leading-indentation text ast parents)
              ast))
           (process-indentation*
               (ast &optional parents
                &aux (output-transformation (output-transformation ast)))
             "Process the text of AST such that its indentation
              is in the indentation slots."
             (setf (before-text ast)
                   (patch-internal-indentation
                    (patch-leading-indentation (car output-transformation)
                                               ast parents :before-text t)))
             (mapc (lambda (output)
                     (cond
                       ((stringp output)
                        (patch-text output ast parents))
                       ((indentablep output)
                        (process-indentation* output (cons ast parents)))))
                   (cdr (butlast output-transformation)))
             (setf (after-text ast)
                   (patch-internal-indentation
                    (patch-trailing-indentation (lastcar output-transformation) ast)))))
    (process-indentation* root)
    root))

(defun convert-parse-tree
    (spec prefix superclass string
     &key computed-text-parent-p line-octets-cache
     &aux (*package* (symbol-package superclass))
       (class (symbolicate prefix '-
                           (let ((type (car spec)))
                             ;; The form can either be
                             ;; - :type
                             ;; - (:slot-name :type)
                             (if (listp type)
                                 (cadr type)
                                 type))))
       (instance (make-instance
                  (get-choice-expansion-subclass class spec)
                  :annotations
                  (when computed-text-parent-p
                    `((:range-start ,(caadr spec))
                      (:range-end . ,(cdadr spec))))))
       (error-p (eql class (symbolicate prefix '-error)))
       (line-octets
        (or line-octets-cache
            (map
             'vector
             #'string-to-octets
             (lines string :keep-eols t))))
       (computed-text-p (computed-text-node-p instance)))
  "Convert SPEC from a parse tree into an instance of SUPERCLASS."
  (labels ((safe-subseq
               (start end
                &aux (start-loc
                      (make-instance
                       'source-location
                       :line (cadr start) :column (car start)))
                  (end-loc
                   (make-instance
                    'source-location
                    :line (cadr end) :column (car end))))
             "Return STRING in the range [START, END) or an empty string if
              the offsets are invalid."
             (if (and
                  start
                  end
                  (source-< start-loc end-loc))
                 (octets-to-string
                  (source-range-subseq
                   line-octets
                   (make-instance 'source-range :begin start-loc :end end-loc)))
                 ""))
           (get-start (ast)
             "Return the start offset into STRING from the AST representation."
             (car (ast-annotation ast :range-start)))
           (get-end (ast)
             "Return the end offset into STRING from the AST representation."
             (car (ast-annotation ast :range-end)))
           ;; NOTE: this may be useful for computed-text ASTs.
           (ranges (children from to)
             "Return the offsets of the source text ranges between CHILDREN.
              And drop their annotations."
             ;; TODO: at some point, refactor this.
             ;;       It's a bit of a hack to reuse existing code.
             (iter
               (for child in children)
               (for previous-child previous child)
               (collect (cons (if previous-child (get-end previous-child) from)
                              (get-start child))
                 into ranges)
               (when previous-child
                 (setf (slot-value previous-child 'annotations)
                       (adrop '(:range-start :range-end)
                              (slot-value previous-child 'annotations))))
               (finally
                (return
                  (prog1
                      (append ranges (list (cons (get-end child) to)))
                    (setf (slot-value child 'annotations)
                          (adrop '(:range-start :range-end)
                                 (slot-value child 'annotations))))))))
           (find-terminal-symbol-class (class-name)
             ;; Check for a '-terminal first in case there's a name overlap.
             (let ((terminal-with
                     (format-symbol
                      'sel/sw/ts "~a-~a-TERMINAL" prefix class-name))
                   (terminal-without
                     (format-symbol
                      'sel/sw/ts "~a-~a" prefix class-name)))
               (cond
                 ((find-class terminal-with nil) terminal-with)
                 ((find-class terminal-without nil) terminal-without))))
           (terminal-symbol-class-p (class-name)
             "Return true if CLASS inherits from the terminal symbol
              mix-in."
             (when-let ((class (find-terminal-symbol-class class-name)))
               (subtypep class 'terminal-symbol)))
           ;; TODO: refactor as this function is used under a different
           ;;       name below.
           (skip-terminal-field-p (field-spec slot-info)
             "Return T if FIELD-SPEC represents a terminal symbol that shouldn't
              appear in the resulting AST."
             (cond
               ;; Has an associated slot or has children
               ((or (listp slot-info) (caddr field-spec)) nil)
               (t (terminal-symbol-class-p slot-info))))
           (get-converted-fields ()
             "Get the value of each field after it's been converted
              into an AST."
             (iter
               ;; Keep track of all groupings of internal ASTs that are dropped
               ;; between two terminal tokens. NIL should be inserted when
               ;; nothing is dropped between two terminals.
               (iter:with internal-asts-stack)
               (iter:with comment-error-and-whitespace-stack)
               ;; Keep track of the last non-comment/error AST seen. It is used
               ;; for populating 'after' comments.
               (iter:with previous-field)
               (for field in (caddr spec))
               (for slot-info = (car field))
               (for i upfrom 0)
               (when (skip-terminal-field-p field slot-info)
                 (cond
                   (previous-field
                    (setf (slot-value previous-field 'after-asts)
                          (reverse comment-error-and-whitespace-stack)))
                   (comment-error-and-whitespace-stack
                    (push comment-error-and-whitespace-stack
                          internal-asts-stack))
                   ;; Don't consider the initial AST as the whitespace before it
                   ;; should be part of the before text of its parent.
                   ((not (= i 0))
                    (push nil internal-asts-stack)))
                 ;; Reset the converted-field so that comments aren't pushed back
                 ;; to the last AST that wasn't a terminal symbol which could
                 ;; have been proceded by terminal symbols.
                 (setf previous-field nil
                       comment-error-and-whitespace-stack nil)
                 (next-iteration))
               (for converted-field =
                    (convert superclass field
                             :string-pass-through string
                             :computed-text-parent-p computed-text-p
                             :line-octets-cache line-octets))
               ;; cl-tree-sitter appears to put the
               ;; slot name first unless the list goes
               ;; into the children slot.
               (cond
                 ((and (listp slot-info) (not error-p))
                  (setf (slot-value converted-field 'before-asts)
                        (reverse comment-error-and-whitespace-stack)
                        comment-error-and-whitespace-stack nil
                        previous-field converted-field)
                  (collect (list (car slot-info)
                                 converted-field)
                    into fields))
                 ;; Ignore inner whitespace in computed-text nodes as it
                 ;; will be handled regardless.
                 ((and computed-text-p
                       (typep converted-field 'inner-whitespace)))
                 ((and (not computed-text-p)
                       (typep converted-field '(or comment-ast parse-error-ast
                                                inner-whitespace)))
                  ;; NOTE: this won't put the comment in the children slot
                  ;;       when it's allowed. There may need to be a function
                  ;;       that signals their ability to be stored there if
                  ;;       that functionality is ever desired.
                  (push converted-field comment-error-and-whitespace-stack))
                 (t
                  (setf (slot-value converted-field 'before-asts)
                        (reverse comment-error-and-whitespace-stack)
                        comment-error-and-whitespace-stack nil
                        previous-field converted-field)
                  (collect converted-field into children)))
               (finally
                (cond
                  (previous-field
                   (setf (slot-value previous-field 'after-asts)
                         (reverse comment-error-and-whitespace-stack)))
                  (t
                   (push comment-error-and-whitespace-stack
                         internal-asts-stack)))
                (return
                  (values
                   (if children
                       (push `(:children ,children) fields)
                       fields)
                   (reverse internal-asts-stack))))))
           (merge-same-fields (field-list)
             "Merge all fields that belong to the same slot.
              This is used for setting slots with an arity of 0."
             (mapcar
              (lambda (grouping)
                (apply #'append
                       (list (caar grouping))
                       (mapcar #'cdr grouping)))
              (assort field-list :key #'car)))
           (set-inner-ast-slots (inner-asts stack-directives
                                 &aux (stack-directive (car stack-directives))
                                   (grouping (car inner-asts)))
             "Set the inner-asts slots in instance that
              correspond to INNER-ASTS and STACK-DIRECTIVES.
              STACK-DIRECTIVES is a list of symbols and numbers.
              A symbol indicates the next grouping in INNER-ASTS is
              to be stored in a slot specified by the symbol. A number
              indicates the number of groupings to be dropped in INNER-ASTS
              due to IMMEDIATE_TOKENs."
             (symbol-macrolet ((slot-value
                                 (slot-value instance stack-directive)))
               (etypecase stack-directive
                 (null)
                 (number
                  (set-inner-ast-slots (nthcdr stack-directive inner-asts)
                                       (cdr stack-directives)))
                 (symbol
                  (let ((value (if (cdr grouping)
                                   ;; Store multiple ASTs in a wrapper AST so
                                   ;; that they continue to work with functional
                                   ;; trees. This notably occurs when multiple
                                   ;; comments are between two terminal tokens.
                                   (list
                                    (make-instance 'inner-parent
                                                   :children (reverse grouping)))
                                   grouping)))
                    (setf slot-value (append slot-value value)))
                  (set-inner-ast-slots
                   (cdr inner-asts) (cdr stack-directives))))))
           (set-slot-values (slot-values inner-asts)
             "Set the slots in instance to correspond to SLOT-VALUES."
             (when (and (slot-exists-p instance 'json-rule)
                        (not (computed-text-node-p instance)))
               (set-inner-ast-slots
                inner-asts
                (nth-value
                 1
                 (match-parsed-children
                  prefix (json-rule instance) (pruned-rule instance)
                  (slot-usage instance) spec))))
             (mapc
              (lambda (list)
                (setf (slot-value
                       instance (translate-to-slot-name (car list) prefix))
                      (if (null (cddr list))
                          (cadr list)
                          (cdr list))))
              slot-values))
           (set-surrounding-text ()
             "Set the before and after slots of instance."
             (unless computed-text-parent-p
               (let* ((before (car (cadddr spec)))
                      (after (cadr (cadddr spec)))
                      (start (car (cadr spec)))
                      (end (cadr (cadr spec)))
                      (before-text
                        (and before start (safe-subseq before start)))
                      (after-text (and end after (safe-subseq end after))))
                 (unless (emptyp before-text)
                   (setf (before-text instance) before-text))
                 (unless (emptyp after-text)
                   (setf (after-text instance) after-text)))))
           (set-text (&aux (from (car (cadr spec)))
                        (to (cadr (cadr spec))))
             "Set the text slot in instance if it needs set."
             (when computed-text-p
               (if-let* ((children (children instance))
                         (text-fragments
                          (mapcar (lambda (range)
                                    (destructuring-bind (from . to) range
                                      (make-instance
                                       'text-fragment
                                       :text (safe-subseq from to))))
                                  (ranges children from to))))
                 (setf (slot-value instance 'children)
                       (iter
                         (while (and children text-fragments))
                         (collect (pop text-fragments) into result)
                         (collect (pop children) into result)
                         (finally (return (append result text-fragments)))))
                 ;; Else set it to everything in the range.
                 (setf (text instance) (safe-subseq from to)))))
           (update-slots-based-on-arity ()
             "Update any slot in instance that needs to be converted to a list
              to match its arity. This is primarily for #'sorted-children."
             (mapc
              (lambda (slot-arity
                       &aux (slot (car slot-arity)))
                (symbol-macrolet ((slot-value (slot-value instance slot)))
                  (unless (listp slot-value)
                    (setf slot-value (list slot-value)))))
              (remove-if-not {eql 0} (slot-value instance 'child-slots)
                             :key #'cdr))))
    ;; Don't store the ASTs inside errors since they're likely not useful.
    (mvlet ((converted-fields inner-asts (and (not error-p)
                                              (get-converted-fields))))
      (unless error-p
        (set-slot-values (merge-same-fields converted-fields) inner-asts)))
    (set-surrounding-text)
    (set-text)
    (update-slots-based-on-arity)
    instance))

;; Make inline to save stack space.
(declaim (inline convert-spec))
(defun convert-spec (spec prefix superclass
                     &aux (package (symbol-package superclass)))
  "Convert SPEC into an ast of type SUPERCLASS. PREFIX is used to find the
correct class name for subclasses of SUPERCLASS."
  (lret ((instance
          (make-instance
           (symbol-cat-in-package
            package
            prefix
            (let ((class (aget :class spec)))
              (if (stringp class)
                  (nest (make-keyword)
                        (string-upcase)
                        (translate-camelcase-name)
                        class)
                  class))))))
    (iter
      (iter:with child-types = (child-slots instance))
      (iter:with annotations = nil)
      (for (slot . value) in (adrop '(:class) spec))
      (for key = (format-symbol package "~a" slot))
      (for translated-key = (translate-to-slot-name key prefix))
      (cond
        ((slot-exists-p instance translated-key)
         ;; TODO: look into a better way to do this for structured text?
         (setf (slot-value instance translated-key)
               (if-let ((spec (find translated-key child-types :key #'car)))
                 (ematch spec
                   ;; (cons key arity)
                   ((cons _ 1) (convert superclass value))
                   ((cons _ 0) (iter (for item in value)
                                 (collect (convert superclass item)))))
                 value)))
        ;; Account for slots in superclasses.
        ((slot-exists-p instance key) (setf (slot-value instance key) value))
        (t (push (cons slot value) annotations)))
      (finally
       (with-slots ((annotations-slot annotations)) instance
         (setf annotations-slot (append annotations annotations-slot)))))))

(defmethod convert ((to-type (eql 'tree-sitter-ast)) (spec tree-sitter-ast)
                     &key &allow-other-keys)
  "Pass thru an existing tree-sitter AST. This useful in manual AST creation."
  spec)

(defmethod convert :around
    ((to-type (eql 'tree-sitter-ast)) (spec list)
     &key patch-whitespace &allow-other-keys)
  (lret ((ast (call-next-method)))
    (when patch-whitespace
      (patch-whitespace ast))))

(defmethod convert ((to-type (eql 'tree-sitter-ast)) (spec list)
                    &key superclass string-pass-through computed-text-parent-p
                      line-octets-cache
                    &allow-other-keys)
  "Create a TO-TYPE AST from the SPEC (specification) list."
  (if string-pass-through
      (convert-parse-tree
       spec (get-language-from-superclass superclass) superclass
       string-pass-through
       :computed-text-parent-p computed-text-parent-p
       :line-octets-cache line-octets-cache)
      (convert-spec
       spec (get-language-from-superclass superclass) superclass)))

(defmethod convert ((to-type (eql 'tree-sitter-ast)) (string string)
                    &key superclass &allow-other-keys
                    &aux (prefix (get-language-from-superclass superclass))
                      (line-octets-cache
                       (map
                        'vector
                        #'string-to-octets
                        (lines string :keep-eols t))))
  (labels
      ((ensure-beginning-bound (parse-tree)
         "Desctructively ensures that the beginning bound of PARSE-TREE is the
          beginning of the string."
         (setf (car (cadr parse-tree)) '(0 0))
         parse-tree)
       (transform-tree (parse-tree)
         "Map transform-parse-tree over PARSE-TREE."
         ;; TODO: at some point, don't cons if nothing has changed.
         (generated-transform-parse-tree
          prefix nil
          (transform-parse-tree
           prefix nil
           `(,(car parse-tree)
             ,(cadr parse-tree)
             ,(mapcar #'transform-tree (caddr parse-tree)))
           :lines line-octets-cache)))
       (get-start (ast)
         "Return the start offset into STRING from the AST representation."
         (car (cadr ast)))
       (get-end (ast)
         "Return the end offset into STRING from the AST representation."
         (cadr (cadr ast)))
       (find-terminal-symbol-class (class-name)
         ;; Check for a '-terminal first in case there's a name overlap.
         ;; TODO: copied from above function
         (let ((terminal-with
                 (format-symbol
                  'sel/sw/ts "~a-~a-TERMINAL" prefix class-name))
               (terminal-without
                 (format-symbol
                  'sel/sw/ts "~a-~a" prefix class-name)))
           (cond
             ((find-class terminal-with nil) terminal-with)
             ((find-class terminal-without nil) terminal-without))))
       (terminal-symbol-class-p (class-name)
         "Return true if CLASS inherits from the terminal symbol
              mix-in."
         ;; TODO: copied from above function
         (when-let ((class (find-terminal-symbol-class class-name)))
           (subtypep class 'terminal-symbol)))
       (terminal-symbol-p (field-spec &aux (slot-info (car field-spec)))
         "Return T if FIELD-SPEC represents a terminal symbol that shouldn't
          appear in the resulting AST."
         ;; TODO: copied from above function
         (cond
           ;; Has an associated slot or has children
           ((or (listp slot-info) (caddr field-spec)) nil)
           (t (terminal-symbol-class-p slot-info))))
       ;; NOTE: any consecutive terminals in a loop are going to be a problem
       ;;       if a parse tree transformation + json substitution isn't used.
       (annotate-surrounding-text (subtree-spec &key parent-from parent-to)
         "Annotate SUBTREE-SPEC by adding a fourth item to each subtree which
          contains a potential starting position for before text and another
          for after text."
         (iter
           (iter:with from)
           ;; NOTE: we shouldn't need to store the 'to' as it will
           ;;       be recursively called immediately with it.
           (for child in (caddr subtree-spec))
           (for previous-child previous child)
           (for terminal-child-p = (terminal-symbol-p child))
           (for terminal-previous-child-p =
                (when previous-child
                  (terminal-symbol-p previous-child)))
           (cond
             ((not (or previous-child terminal-child-p))
              ;; Set to the start of the current node passed in as an argument.
              (setf from (get-start subtree-spec)))
             ;; First child is a terminal.
             ((and (not previous-child) terminal-child-p))
             ;; NOTE: (not previous-child) should be covered by the
             ;;       previous two conditions.
             ((and (not terminal-previous-child-p) terminal-child-p)
              ;; Send the before text to the after of the previous child.
              (collect
                  (annotate-surrounding-text
                   previous-child :parent-from from :parent-to (get-start child))
                into annotated-children at beginning)
              (setf from nil))
             ((and (not terminal-previous-child-p) (not terminal-child-p))
              (collect
                  (annotate-surrounding-text previous-child :parent-from from)
                into annotated-children at beginning)
              ;; Prefer storing text in the before-text slot.
              (setf from (get-end previous-child)))
             ((and terminal-previous-child-p (not terminal-child-p))
              (collect previous-child into annotated-children at beginning)
              ;; Store the text in the before-text of child.
              (setf from (get-end previous-child)))
             ((and terminal-previous-child-p terminal-child-p)
              ;; This whitespace doesn't have an AST to be attached to.
              (collect previous-child into annotated-children at beginning)
              (collect `(:inner-whitespace ,(list (get-end previous-child)
                                                     (get-start child)))
                into annotated-children at beginning)
              (setf from nil)))
           (finally
            ;; Patch the final child if needed and then
            ;; reassemble the subtree spec with updated information.
            (return
              (list
               (car subtree-spec)
               (cadr subtree-spec)
               (reverse
                (append
                 (cond
                   ((not child) nil)
                   ((not terminal-child-p)
                    (list
                     (annotate-surrounding-text
                      child :parent-from from
                            :parent-to (get-end subtree-spec))))
                   ((or (root-rule-ast-p (car subtree-spec))
                        (not (equal (get-end child) (get-end subtree-spec))))
                    ;; This is an edge case where the after text won't be
                    ;; handled without an inner whitespace AST.
                    (list `(:inner-whitespace ,(list (get-end child)
                                                     (get-end subtree-spec)))
                          child))
                   (child (list child)))
                 annotated-children))
               ;; If either is nil, it means that there isn't text for that slot.
               (list parent-from parent-to)))))))
    (process-indentation
     (convert
      to-type
      (annotate-surrounding-text
       (transform-tree
        (ensure-beginning-bound
         (parse-string (get-language-from-superclass superclass) string
                       :produce-cst t))))
      :superclass superclass
      :string-pass-through string
      :line-octets-cache line-octets-cache))))

;;; By default, don't indent comments, text fragments or parsing errors.
(defmethod indentablep ((ast comment-ast)) nil)
(defmethod indentablep ((ast parse-error-ast)) nil)
(defmethod indentablep ((ast text-fragment)) nil)
(defmethod indentablep ((ast source-text-fragment)) nil)

(defmethod get-indentation-at ((ast inner-whitespace) (parents list)
                               &aux (parent (car parents)))
  "Get the indentation at AST when it is an inner-whitespace AST. This
is handled differently than other ASTs because it should be considered part
of the parent."
  (reduce (lambda (total parent)
            (+ total
               (or (indent-adjustment parent) 0)
               (or (indent-children parent) 0)))
          (cdr parents)
          :initial-value (or (and parent (indent-adjustment parent))
                             0)))

(defgeneric surrounding-text-transform (text)
  (:documentation "Transform VALUE into a string representation. This is useful
for ASTs which need to appear in the surrounding text slots.")
  (:method (text) text)
  (:method ((ast null)) "")
  (:method ((ast conflict-ast)) (source-text ast))
  (:method ((ast source-text-fragment)) (text ast)))

;;; TODO: with unindentable ASTs, we still want to know if the last thing seen
;;;       was a newline or not.
(defmethod source-text ((ast indentation)
                        &key stream parents
                          ;; These are "boxed" values since they have
                          ;; to be propagated between adjacent
                          ;; siblings (not just down the stack).
                          (indent-p (box nil))
                          (indentation-ast (box nil))
                          (trim t)
                          (root ast))
  ;; Trim removes the before and after text from the output and the comments.
  ;; Note that it will always trim with the first AST it sees since
  ;; the top most AST shouldn't have any before or after text this
  ;; should maintain previous functionality while still being able to
  ;; reproduce the source-text at the top-level.
  (declare (special trim))
  (labels ((ends-with-newline-p (string)
             "Return T if STRING ends with a newline."
             (unless (emptyp string)
               (eql #\newline (last-elt string))))
           (make-indentation-string (indentation)
             "Create the string representation of INDENTATION.
            This handles converting spaces to tabs."
             ;; Protect from negative numbers.
             (let ((protected-indentation (if (< indentation 0) 0 indentation)))
               (if *indent-with-tabs-p*
                   (mvlet ((tabs spaces (floor protected-indentation
                                               *spaces-per-tab*)))
                     (concatenate
                      'string
                      (repeat-sequence "	" tabs)
                      (repeat-sequence " " spaces)))
                   (repeat-sequence " " protected-indentation))))
           (indentation-length (ast parent-list)
             "Get the indentation at AST with its parents provided
            in PARENT-LIST."
             ;; Patch the indent-children slots of AST if
             ;; the value is T. The value T could be provided
             ;; from a #'convert invocation.
             (cond
               ((typep ast 'indentation)
                (when (eq t (indent-children ast))
                  (setf (indent-children ast)
                        (get-default-indentation ast parent-list)))
                (get-indentation-at ast parent-list))
               (t (get-indentation-at (car parent-list) (cdr parent-list)))))
           (patch-inner-indentation (text ast parents
                                     &aux (indentation
                                           (indentation-length ast parents))
                                       (split-text (split "\\r?\\n" text)))
             "Patch the newlines that occur inside interleaved text.
            This assumes that the indentation should be the same
            as the parent."
             (cond
               ((not (indentablep ast))
                ;; NOTE: this won't correctly handle source-text fragments that
                ;;       end with newlines. This should only be a problem with
                ;;       ASTs that have an implicit newline.
                (setf (unbox indent-p) nil
                      (unbox indentation-ast) nil)
                text)
               ((and (< 1 (length split-text))
                     (not (computed-text-node-p ast)))
                (with-output-to-string (s)
                  (write-string (car split-text) s)
                  (dolist (subseq (cdr split-text))
                    (format s "~%~a~a"
                            (make-indentation-string
                             (if (emptyp subseq)
                                 0
                                 indentation))
                            subseq))
                  ;; Add the newline back in if there was one at the end
                  ;; of the string since it gets removed by #'split.
                  (format s "~a"
                          (if (ends-with-newline-p text) #\newline ""))))
               (t text)))
           (handle-leading-newline (text)
             "If the first character in TEXT is a newline, reset the
            indentation variables so no indentation is printed."
             (when (scan "^\\r?\\n" text)
               (setf (unbox indent-p) nil
                     (unbox indentation-ast) nil)))
           (handle-trailing-newline (text ast indentablep)
             "If the last character in TEXT is a newline, set the
            indentation variables."
             (when (and (ends-with-newline-p text)
                        indentablep)
               (setf (unbox indent-p) t
                     (unbox indentation-ast) ast)))
           (handle-indentation (text ast indentablep parents
                                &key ancestor-check
                                &aux (empty? (emptyp text)))
             "If indentation to be written to stream, handle
            writing it."
             (when (and (unbox indent-p)
                        ;; Prevent indentation from being
                        ;; wasted on empty strings before it
                        ;; reaches a child. This is checking if
                        ;; it should not be skipped as opposed
                        ;; to the logic in process-indentation
                        ;; which is checking if it should be
                        ;; skipped.
                        (not (and ancestor-check
                                  empty?
                                  (ancestor-of-p
                                   root (unbox indentation-ast) ast))))
               (unless empty?
                 (setf (unbox indent-p) nil
                       (unbox indentation-ast) nil))
               (unless (or empty? (not indentablep) trim)
                 (write-string
                  (make-indentation-string (indentation-length ast parents))
                  stream))))
           (handle-text (text ast indentablep parents
                         &key ancestor-check surrounding-text)
             "Handle writing TEXT to stream, updating any indentation
            variables that need updated."
             ;; Suppress indentation if TEXT begins with a newline.
             (let ((text (if surrounding-text
                             (surrounding-text-transform text)
                             text)))
               (handle-leading-newline text)
               (handle-indentation text ast indentablep parents
                                   :ancestor-check ancestor-check)
               ;; Set indentation flag  when TEXT ends with a newline.
               (handle-trailing-newline text ast indentablep)
               (unless trim
                 (write-string
                  (patch-inner-indentation text ast parents)
                  stream))))
           (handle-ast (output &key (ast-parents (cons ast parents)))
             "Handle the source text of AST."
             (source-text output
                          :stream stream
                          :parents ast-parents
                          :indent-p indent-p
                          :indentation-ast indentation-ast
                          :trim nil
                          :root root)))
    (let ((indentablep (indentablep ast)))
      ;; before and after text is always considered indentable.
      (handle-text (before-text ast) ast t parents :surrounding-text t)
      (mapc (lambda (output &aux trim)
              (declare (special trim))
              (if (stringp output)
                  (handle-text output ast indentablep parents
                               :ancestor-check t)
                  (handle-ast output)))
            (cdr (butlast (output-transformation ast))))
      (handle-text (after-text ast) ast t parents :surrounding-text t))))

(defmethod rebind-vars ((ast tree-sitter-ast)
                        var-replacements fun-replacements)
  "Replace variable and function references, returning a new AST.
* AST node to rebind variables and function references for
* VAR-REPLACEMENTS list of old-name, new-name pairs defining the rebinding
* FUN-REPLACEMENTS list of old-function-info, new-function-info pairs defining
the rebinding"
  (if (ast-type-to-rebind-p ast)
      (copy ast :text (rebind-vars (text ast) var-replacements fun-replacements))
      (apply #'copy ast
             (mappend (lambda (child-slot)
                        (destructuring-bind (name . arity) child-slot
                          (list (make-keyword name)
                                (cond ((= arity 0)
                                       (mapcar {rebind-vars _ var-replacements
                                                            fun-replacements}
                                               (slot-value ast name)))
                                      ((slot-value ast name)
                                       (rebind-vars (slot-value ast name)
                                                    var-replacements
                                                    fun-replacements))))))
                      (child-slots ast)))))

;;; TODO: the ast-hash specialization for AST only considers #'children.
;;;       Does it make sense to create a specialization for structured-text
;;;       which considers the output-transformation?
(defmethod ast-hash ast-combine-hash-values ((ast computed-text))
  (ast-hash (text ast)))

(defmethod ast-hash ast-combine-hash-values ((ast structured-text))
  ;; TODO: need to add support for internal ASTs too.
  (ast-hash (flatten
             (list (before-text ast) (after-text ast)
                   (before-asts ast) (after-asts ast)))))

(defmethod equal? ((ast-a computed-text) (ast-b computed-text))
  (equal (source-text ast-a) (source-text ast-b)))

(defmethod equal? :around ((ast-a structured-text) (ast-b structured-text))
  (and (call-next-method)
       (equal (before-text ast-a) (before-text ast-b))
       (equal (after-text ast-a) (after-text ast-b))))


;;;; Whitespace styles
(defclass c-style-indentation ()
  ;; NOTE: it might make sense to store indentation size on a style object
  ;;       instead of the software object.
  ()
  (:documentation "A class used to represent indentation in c-style languages."))

(defmethod get-style-indentation ((style c-style-indentation) software ast &key)
  nil)

(defmethod get-style-indentation
    ((style c-style-indentation) software (ast compound-ast) &key)
  t)

(defmethod get-style-indentation
    ((style c-style-indentation) software (ast if-ast) &key)
  (not (or (typep (consequence ast) 'compound-ast)
           (typep (alternative ast) 'compound-ast))))

(defmethod get-style-indentation
    ((style c-style-indentation) software (ast loop-ast) &key)
  (not (typep (body ast) 'compound-ast)))

(defmethod get-style-indentation-adjustment
    ((style c-style-indentation) software ast (parent if-ast)
     &key parents)
  (let ((target-child-list (list (consequence parent) (alternative parent))))
    (when (and (not (typep ast 'compound-ast))
               (member ast target-child-list :test #'eq)
               (some {typep _ 'compound-ast} target-child-list))
      (get-default-indentation ast parents))))

(defmethod whitespace-between/parent ((parent root-ast)
                                      (style c-style-indentation)
                                      (ast1 ast)
                                      (ast2 ast))
  #.(fmt "~%"))

(defmethod whitespace-between/parent ((parent compound-ast)
                                      (style c-style-indentation)
                                      (ast1 statement-ast)
                                      (ast2 statement-ast))
  #.(fmt "~%"))

(defmethod whitespace-between/parent ((parent compound-ast)
                                      (style c-style-indentation)
                                      (ast1 (eql ':|{|))
                                      (ast2 statement-ast))
  #.(fmt "~%"))

(defmethod whitespace-between/parent ((parent compound-ast)
                                      (style c-style-indentation)
                                      (ast1 statement-ast)
                                      (ast2 (eql ':|}|)))
  #.(fmt "~%"))

(defmethod whitespace-between/parent ((parent if-ast)
                                      (style c-style-indentation)
                                      ast1
                                      (ast2 statement-ast))
  (if (and (not (typep ast2 'compound-ast))
           (or (eq (consequence parent) ast2)
               (eq (alternative parent) ast2)))
      #.(fmt "~%")
      (call-next-method)))

(defmethod whitespace-between/parent ((parent if-ast)
                                      (style c-style-indentation)
                                      (ast1 statement-ast)
                                      ast2)
  (if (and (not (typep ast1 'compound-ast))
           (or (eq (consequence parent) ast1)
               (eq (alternative parent) ast1)))
      #.(fmt "~%")
      (call-next-method)))

(defmethod whitespace-between/parent ((parent loop-ast)
                                      (style c-style-indentation)
                                      ast1
                                      (ast2 statement-ast))
  (if (and (not (typep ast2 'compound-ast))
           (eq (body parent) ast2))
      #.(fmt "~%")
      (call-next-method)))

(defmethod whitespace-between/parent (parent
                                      (style c-style-indentation)
                                      ast1
                                      (ast2 comment-ast))
  #.(fmt "~%"))

(defmethod whitespace-between/parent (parent
                                      (style c-style-indentation)
                                      (ast1 comment-ast)
                                      ast2)
  #.(fmt "~%"))


;;;; Parse Tree Util
(defun add-operator-to-binary-operation (parse-tree)
  "Adds the operator in a binary operation to the :operator slot."
  (with-modify-parse-tree (parse-tree)
    ((:error :comment) (ignore-types))
    (t (label-as :operator))))

(defun transform-malformed-parse-tree (parse-tree &key (recursive t))
  "Return a modified version of PARSE-TREE if it is malformed.
This occurs when the source text is not accurately represented by the parse tree
which is caused by dropped tokens or added zero-width tokens.
Otherwise, returns PARSE-TREE."
  (labels ((walk-parse-tree (function parse-tree)
             (funcall function parse-tree)
             (map nil {walk-parse-tree function} (caddr parse-tree)))
           (zero-width-p (subtree &aux (range (cadr subtree)))
             (when (listp range)
               (equal (car range) (cadr range))))
           (problematic-p (tree)
             (if recursive
                 (walk-parse-tree
                  (lambda (subtree)
                    (when (and (listp subtree) (zero-width-p subtree))
                      (return-from problematic-p t)))
                  tree)
                 (find-if
                  (lambda (subtree)
                    (and (listp subtree) (zero-width-p subtree)))
                  (caddr tree)))))
    (if (problematic-p parse-tree)
        `(,(if (consp (car parse-tree))
               (list (caar parse-tree) :source-text-fragment)
               :source-text-fragment)
          ,(cadr parse-tree) nil)
        parse-tree)))

(defun transform-c-style-variadic-parameter (parse-tree)
  "If PARSE-TREE represents a variadic function, return a modified version of it.
Otherwise, return PARSE-TREE."
  (with-modify-parse-tree (parse-tree)
    (:|...| (rename-type-to :variadic-declaration))))
