;;;; tree-sitter.lisp --- software representations with a tree-sitter backend.
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
;;; language modules.  The @env{SEL_TREE_SITTER_LANGUAGE_DIR}
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
;;; @subsubheading Setting up libtree-sitter
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
;;; @subsubheading Per-language Modules
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
;;; @subsubheading cl-tree-sitter setup
;;;
;;; Clone the following repositories to the local-projects directory for quicklisp:
;;; - @url{https://github.com/death/cl-tree-sitter}
;;; - @url{https://github.com/death/cffi}
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
           :collect-var-uses
           :collect-fun-uses
           :javascript
           :python
           ;; Python
           :find-if-in-scopes
           :get-asts-in-namespace
           :get-vars
           :identical-name-p
           :in-class-def-p
           ;; Interleaved text
           :interleaved-text
           :check-interleaved-text
           ;; Cross-language Mix-ins
           :c/cpp
           :ecma
           :typescript
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
           :boolean-ast
           :boolean-true-ast
           :boolean-false-ast
           :unary-ast
           :binary-ast
           :return-ast
           :goto-ast
           :terminal-symbol
           ;; Cross-language Generics
           :body
           :lhs
           :rhs
           :operator
           :control-flow-condition
           :end-of-parameter-list
           :function-name
           :call-arguments
           :function-parameters
           :parameter-type
           :parameter-name
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
           :comparisonp))
(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

(define-software tree-sitter (software-indentation parseable) ()
  (:documentation "tree-sitter software representation."))


;;; Shared object set-up
(eval-always
  ;; TODO: REMOVE THIS BEFORE MERGE
  (unless (member :sel/structured-text *features*)
    (push :sel/structured-text *features*))

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

  (defparameter *tree-sitter-software-superclasses*
    '((:c compilable normal-scope c/cpp)
      (:cpp compilable normal-scope c/cpp)
      (:javascript normal-scope ecma)
      (:typescript-typescript typescript)
      (:typescript-tsx typescript)))

  (defparameter *tree-sitter-software-direct-slots* '()
    "Alist of direct slots for software classes, such as `python' or
    `c'.")

  (defparameter *tree-sitter-base-ast-superclasses* '()
    "Alist of superclasses for the base class of a language (e.g.
    `python-ast').")

  (defparameter *tree-sitter-ast-extra-slots*
    '((:c
       (c-sized-type-specifier
        (c-modifiers
         :accessor c-modifiers
         :initarg :c-modifiers
         :initform nil)))
      (:python
       (python-function-definition
        (python-async
         :accessor python-async
         :initarg python-async
         :initform nil))))
    "Alist from languages to classes with extra slots.")

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
        (c-body :reader body)))
      (:cpp
       (cpp-init-declarator
        (cpp-declarator :initarg :lhs :reader lhs)
        (cpp-value :initarg :rhs :reader rhs))
       (cpp-assignment-expression
        (cpp-left :initarg :lhs :reader lhs)
        (cpp-right :initarg :rhs :reader rhs))
       (cpp-call-expression
        (cpp-function :reader call-function)
        (cpp-arguments :reader call-arguments)))
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
        (python-body :reader body))
       (python-keyword-argument
        (python-name :initarg :lhs :reader lhs)
        (python-value :initarg :rhs :reader rhs))
       (python-unary-operator
        (python-operator :initarg :operator :reader operator))
       (python-while-statement
        (python-body :reader body))))
    "Alist from languages to classes with extra slot options.")

  (defparameter *tree-sitter-ast-superclasses*
    '((:c
       (:comment-ast c-comment)
       (:definition-ast c-type-definition c-struct-specifier c-union-specifier
        c-field-declaration c-enum-specifier c-preproc-def
        c-preproc-function-def)
       (:statement-ast c--statement c-function-definition)
       (:expression-ast c--expression)
       (:parenthesized-expression-ast c-parenthesized-expression)
       (:compound-ast c-compound-statement)
       (:control-flow-ast c-switch-statement c-case-statement)
       (:if-ast c-if-statement)
       (:while-ast c-while-statement)
       (:loop-ast c-while-statement c-for-statement)
       (:parse-error-ast c-error)
       (:variable-declaration-ast c-init-declarator c-assignment-expression)
       (:function-ast c-function-definition)
       (:identifier-ast c-identifier)
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
       (:c/cpp-function-declarator c-function-declarator)
       (:c/cpp-function-definition c-function-definition)
       (:c/cpp-identifier c-identifier)
       (:c/cpp-if-statement c-if-statement)
       (:c/cpp-number-literal c-number-literal)
       (:c/cpp-parameter-declaration c-parameter-declaration)
       (:c/cpp-parenthesized-declarator c-parenthesized-declarator)
       (:c/cpp-pointer-declarator c-pointer-declarator)
       (:c/cpp-preproc-arg c-preproc-arg)
       (:c/cpp-preproc-def c-preproc-def)
       (:c/cpp-preproc-function-def c-preproc-function-def)
       (:c/cpp-preproc-include c-preproc-include)
       (:c/cpp-preproc-params c-preproc-params)
       (:c/cpp-primitive-type c-primitive-type)
       (:c/cpp-return-statement c-return-statement)
       (:c/cpp-string-literal c-string-literal)
       (:c/cpp-struct-specifier c-struct-specifier)
       (:c/cpp-switch-statement c-switch-statement)
       (:c/cpp-type-definition c-type-definition)
       (:c/cpp-type-identifier c-type-definition)
       (:c/cpp-union-specifier c-union-specifier)
       (:c/cpp-while-statement c-while-statement))
      (:cpp
       (:comment-ast cpp-comment)
       (:definition-ast cpp-type-definition cpp-struct-specifier
        cpp-union-specifier
        cpp-field-declaration
        cpp-enum-specifier cpp-preproc-def
        cpp-preproc-function-def)
       (:parenthesized-expression-ast cpp-parenthesized-expression)
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
       (:c/cpp-function-declarator cpp-function-declarator)
       (:c/cpp-function-definition cpp-function-definition)
       (:c/cpp-identifier cpp-identifier)
       (:c/cpp-if-statement cpp-if-statement)
       (:c/cpp-number-literal cpp-number-literal)
       (:c/cpp-parameter-declaration cpp-parameter-declaration)
       (:c/cpp-parenthesized-declarator cpp-parenthesized-declarator)
       (:c/cpp-pointer-declarator cpp-pointer-declarator)
       (:c/cpp-preproc-arg cpp-preproc-arg)
       (:c/cpp-preproc-def cpp-preproc-def)
       (:c/cpp-preproc-function-def cpp-preproc-function-def)
       (:c/cpp-preproc-include cpp-preproc-include)
       (:c/cpp-preproc-params cpp-preproc-params)
       (:c/cpp-primitive-type cpp-primitive-type)
       (:c/cpp-return-statement cpp-return-statement)
       (:c/cpp-string-literal cpp-string-literal)
       (:c/cpp-struct-specifier cpp-struct-specifier)
       (:c/cpp-switch-statement cpp-switch-statement)
       (:c/cpp-type-definition cpp-type-definition)
       (:c/cpp-type-identifier cpp-type-identifier)
       (:c/cpp-union-specifier cpp-union-specifier)
       (:c/cpp-while-statement cpp-while-statement))
      (:java
       (:statement-ast java-statement)
       (:return-ast java-return-statement))
      (:javascript
       (:comment-ast javascript-comment)
       (:ecma-comment javascript-comment)
       (:ecma-error javascript-error)
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
       (:variable-declaration-ast javascript-variable-declaration-ast)
       (:identifier-ast
        javascript-identifier javascript-property-identifier
        javascript-shorthand-property-identifier
        javascript-shorthand-property-identifier-pattern)
       (:float-ast javascript-number)
       (:string-ast javascript-string)
       (:loop-ast
        javascript-for-statement javascript-do-statement
        javascript-while-statement)
       (:statement-ast javascript--statement)
       (:call-ast javascript-call-expression)
       (:unary-ast javascript-unary-expression)
       (:binary-ast javascript-binary-expression)
       (:return-ast javascript-return-statement))
      (:python
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
       (:boolean-true-ast python-true)
       (:boolean-false-ast python-false)
       (:identifier-ast python-identifier)
       (:lambda-ast python-lambda)
       (:integer-ast python-integer)
       (:float-ast python-float)
       (:string-ast python-string)
       (:loop-ast
        python-while-statement python-for-statement python-for-in-clause)
       (:statement-ast python--compound-statement python--simple-statement)
       (:call-ast python-call)
       (:unary-ast python-unary-operator python-not-operator)
       (:binary-ast python-binary-operator python-boolean-operator)
       (:return-ast python-return-statement)
       (:variable-declaration-ast python-assignment python-keyword-argument))
      (:typescript-tsx
       (:ecma-comment typescript-tsx-comment)
       (:ecma-error typescript-tsx-error))
      (:typescript-typescript
       (:ecma-comment typescript-typescript-comment)
       (:ecma-error typescript-typescript-error)))
    "Specifies which classes should inherit from which mixins.
An alist from languages to alists of mixins and tree-sitter AST
classes that should inherit from them.

Note that mixins used here will be automatically exported later, and
those that do not have separate class definitions will be given stub
definitions.")

  ;; TODO: it may make sense to have a way to 'rebind' a subclass when
  ;;       the subclass no longer applies after a mutation.
  (defparameter *tree-sitter-choice-expansion-subclasses*
    '((:c
       ;; TODO: this should be moved over to using the pruned-rule before
       ;;       merging into master.
       ;; TODO: at some point, subclasses should be exported automatically.
       (c-update-expression
        (c-update-expression-prefix
         (:seq (:field c-operator c--- c-++) (:field c-argument c--expression)))
        (c-update-expression-postfix
         (:seq (:field c-argument c--expression) (:field c-operator c--- c-++)))))
      (:python
       (python-tuple
        ;; TODO: think of an actual name for this? The trailing hyphen seems
        ;;       reasonable though.
        (python-tuple-
         (:seq
          (:child python-expression python-yield
           python-list-splat
           python-parenthesized-list-splat)
          (:repeat
           (:seq
            (:choice
             (:child python-expression python-yield
                     python-list-splat
                     python-parenthesized-list-splat))))))
        (python-empty-tuple
         (:seq)))
       (python-dictionary
        (python-dictionary-
         (:seq (:child python-pair python-dictionary-splat)
          (:repeat
           (:seq
            (:choice
             (:child python-pair python-dictionary-splat))))))
        (python-empty-dictionary
         (:seq))))))

  (defparameter *tree-sitter-json-rule-substitutions*
    '((:c
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
       (:POINTER-DECLARATOR (:TYPE . "PREC_DYNAMIC") (:VALUE . 1)
        (:CONTENT (:TYPE . "PREC_RIGHT") (:VALUE . 0)
         (:CONTENT (:TYPE . "SEQ")
          (:MEMBERS
           ((:TYPE . "CHOICE")
            (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "ms_based_modifier"))
                      ((:TYPE . "BLANK"))))
           ((:TYPE . "STRING") (:VALUE . "*"))
           ((:TYPE . "REPEAT")
            (:CONTENT (:TYPE . "SYMBOL") (:NAME . "ms_pointer_modifier")))
           ((:TYPE . "REPEAT")
            (:CONTENT (:TYPE . "SYMBOL") (:NAME . "type_qualifier")))
           ((:TYPE . "FIELD")
            (:NAME . "declarator")
            (:CONTENT
             (:TYPE . "CHOICE")
             (:MEMBERS
              ((:TYPE . "SYMBOL") (:NAME . "_declarator"))
              ;; TODO: instead of patching this rule, maybe look into using the
              ;;       node types file and adding in every possible type a field
              ;;       allows to the rule. Need to consider whether this could
              ;;       break anything.
              ;; Workaround for field declaration issues.
              ((:TYPE . "SYMBOL") (:NAME . "_field_declarator"))
              ((:TYPE . "SYMBOL") (:NAME . "_type_declarator")))))))))
       (:ARRAY-DECLARATOR (:TYPE . "PREC") (:VALUE . 1)
        (:CONTENT (:TYPE . "SEQ")
         (:MEMBERS
          ((:TYPE . "FIELD") (:NAME . "declarator")
           (:CONTENT
             (:TYPE . "CHOICE")
             (:MEMBERS
              ((:TYPE . "SYMBOL") (:NAME . "_declarator"))
              ((:TYPE . "SYMBOL") (:NAME . "_field_declarator")))))
          ((:TYPE . "STRING") (:VALUE . "["))
          ((:TYPE . "REPEAT")
           (:CONTENT (:TYPE . "SYMBOL") (:NAME . "type_qualifier")))
          ((:TYPE . "FIELD") (:NAME . "size")
           (:CONTENT
            (:TYPE . "CHOICE")
            (:MEMBERS
             ((:TYPE . "CHOICE")
              (:MEMBERS ((:TYPE . "SYMBOL") (:NAME . "_expression"))
                        ((:TYPE . "STRING") (:VALUE . "*"))))
             ((:TYPE . "BLANK")))))
          ((:TYPE . "STRING") (:VALUE . "]")))))
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
       (:PREPROC-PARAMS (:TYPE . "SEQ")
        (:MEMBERS ((:TYPE . "STRING") (:VALUE . "("))
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "SEQ")
            (:MEMBERS
             ((:TYPE . "CHOICE")
              (:MEMBERS
               ((:TYPE . "SYMBOL") (:NAME . "identifier"))
               ((:TYPE . "STRING") (:VALUE . "..."))))
             ((:TYPE . "REPEAT")
              (:CONTENT
               (:TYPE . "SEQ")
               (:MEMBERS
                ((:TYPE . "STRING") (:VALUE . ","))
                ((:TYPE . "CHOICE")
                 (:MEMBERS
                  ((:TYPE . "SYMBOL") (:NAME . "identifier"))
                  ((:TYPE . "STRING") (:VALUE . "...")))))))))
           ((:TYPE . "BLANK"))))
         ((:TYPE . "STRING") (:VALUE . ")")))))
      ;; TODO: add in substitutions and parse tree transformations for 'async'
      ;;       classes.
      (:python
       ;; NOTE: this removes semicolons. This can be further amended if it
       ;;       becomes problematic.
       (:-SIMPLE-STATEMENTS (:TYPE . "SYMBOL") (:NAME . "_simple_statement"))
       (:FUNCTION-DEFINITION (:TYPE . "SEQ")
        (:MEMBERS
         ((:TYPE . "FIELD") (:NAME . "async")
          (:CONTENT
           (:TYPE . "CHOICE")
           (:MEMBERS ((:TYPE . "STRING") (:VALUE . "async")) ((:TYPE . "BLANK")))))
         ((:TYPE . "STRING") (:VALUE . "def"))
         ((:TYPE . "FIELD") (:NAME . "name")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "identifier")))
         ((:TYPE . "FIELD") (:NAME . "parameters")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "parameters")))
         ((:TYPE . "CHOICE")
          (:MEMBERS
           ((:TYPE . "SEQ")
            (:MEMBERS ((:TYPE . "STRING") (:VALUE . "->"))
                      ((:TYPE . "FIELD") (:NAME . "return_type")
                                         (:CONTENT (:TYPE . "SYMBOL") (:NAME . "type")))))
           ((:TYPE . "BLANK"))))
         ((:TYPE . "STRING") (:VALUE . ":"))
         ((:TYPE . "FIELD") (:NAME . "body")
          (:CONTENT (:TYPE . "SYMBOL") (:NAME . "_suite"))))))
      (:javascript
       (:-SEMICOLON (:TYPE . "CHOICE")
        (:MEMBERS
         ((:TYPE . "STRING") (:VALUE . ";"))
         ((:TYPE . "SYMBOL") (:NAME . "_automatic_semicolon"))))
       (:EXPORT-STATEMENT (:TYPE . "CHOICE")
        (:MEMBERS
         ((:TYPE . "SEQ")
          (:MEMBERS ((:TYPE . "STRING") (:VALUE . "export"))
           ((:TYPE . "CHOICE")
            (:MEMBERS
             ((:TYPE . "SEQ")
              (:MEMBERS
               ((:TYPE . "SYMBOL") (:NAME . "export_clause"))
               ((:TYPE . "SYMBOL") (:NAME . "_from_clause"))
               ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))
             ((:TYPE . "SEQ")
              (:MEMBERS
               ((:TYPE . "STRING") (:VALUE . "*"))
               ((:TYPE . "SYMBOL") (:NAME . "_from_clause"))
               ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))
             ((:TYPE . "SEQ")
              (:MEMBERS
               ((:TYPE . "SYMBOL") (:NAME . "export_clause"))
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
             ((:TYPE . "FIELD")
              (:NAME . "declaration")
              (:CONTENT (:TYPE . "SYMBOL") (:NAME . "declaration")))
             ((:TYPE . "SEQ")
              (:MEMBERS
               ((:TYPE . "STRING") (:VALUE . "default"))
               ((:TYPE . "FIELD")
                (:NAME . "value")
                (:CONTENT (:TYPE . "SYMBOL") (:NAME . "expression")))
               ((:TYPE . "SYMBOL") (:NAME . "_semicolon"))))))))))))
    "A mapping of JSON rule substitutions to be performed on the JSON file
before class generation and analysis.")

  (defparameter *tree-sitter-json-subtree-choice-resolver*
    `((:javascript
       ;; Work around automatic semicolon preference.
       ,(lambda (branches)
          (find-if
           (lambda (branch)
             (or
              ;; Prefer semi-colons over blanks.
              (and (equal "STRING" (aget :type branch))
                   (equal ";" (aget :value branch)))
              (equal "BLANK" (aget :type branch))))
           branches))))
    "A mapping of functions for resolving which choice branch should be
chosen when gathering a string representation of a JSON subtree.")

  (defparameter *tree-sitter-ast-superclass-table*
    (lret ((table (make-hash-table)))
      (iter
       (for (lang . alist) in *tree-sitter-ast-superclasses*)
       (let ((lang (intern (string lang) :sel/sw/ts))
             (lang-table (make-hash-table)))
         (setf (gethash lang table) lang-table)
         (iter
          (for (mixin . subclasses) in alist)
          (let ((mixin (find-symbol (string mixin) :sel/sw/ts)))
            (dolist (subclass subclasses)
              (push subclass (gethash mixin lang-table))))))))
    "Nested hash table from language and mixin to a list of classes
    that inherit from that mixin.")

  (defparameter *tree-sitter-ast-extra-prefixes*
    '((:c c/cpp)
      (:cpp c/cpp)
      (:typescript-typescript typescript ecma)
      (:typescript-tsx typescript ecma)
      (:javascript ecma))
    "Alist of languages and extra prefixes.
For every extra prefix, every slot will get an extra reader and an
extra initarg with that prefix.")

  (defparameter *tree-sitter-computed-text-asts*
    '((:python python-string))
    "Alist of languages and their classes which should be computed-text ASTs
but aren't detected as such. This is usually due to insufficient information
stored on the AST or external rules.")

  (defun tree-sitter-ast-classes (name grammar-file node-types-file)
    (nest
     (flet ((alternate-class-name (name)
              (string-case name
                ("GO" "GOLANG")
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
         (aget class-keyword *tree-sitter-ast-superclasses*)
         :base-ast-superclasses
         (aget class-keyword *tree-sitter-base-ast-superclasses*)
         :software-superclasses
         (aget class-keyword *tree-sitter-software-superclasses*)
         :software-direct-slots
         (aget class-keyword *tree-sitter-software-direct-slots*)
         :ast-extra-slot-options
         (aget class-keyword *tree-sitter-ast-extra-slot-options*)
         :ast-extra-slots
         (aget class-keyword *tree-sitter-ast-extra-slots*)
         :json-subtree-choice-resolver
         (car (aget class-keyword
                    *tree-sitter-json-subtree-choice-resolver*)))))))

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
                       (register-language ,language ,lib-name))
                     (setf (gethash ,ast-superclass *superclass->language*) ,language))
                 ;; Try again with an augmented library search path.
                 (load-foreign-library-error ()
                   (register-language ,language ,(concatenate 'string "/usr/lib/" lib-name)))))
             (setf (gethash ,ast-superclass *superclass->language*) ,language))
         (load-foreign-library-error ()
           (format *error-output*
                   "Failed to load '~a'. Support for '~a' will not be available."
                   ,lib-name ,language))))))


;;; Defining tree-sitter classes
(eval-always
  (defclass interleaved-text ()
    ((interleaved-text :initarg :interleaved-text :initform nil :type list
                       :reader interleaved-text
                       :documentation "Interleaved text between children."))
    (:documentation "Mixin for interleaved-text slot"))

  ;; TODO: let over a basic string for empty strings?
  (defclass structured-text ()
    ((before-text
      :accessor before-text
      :initarg :before-text
      :initform ""
      :documentation "The text before the first token of an AST.")
     ;; NOTE: the primary usage of this slot is for AST text, like
     ;;       identifiers, which doesn't belong in the before or after slot.
     (computed-text
      :accessor computed-text
      :initarg :computed-text
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
     (before-comments
      :accessor before-comments
      :initarg :before-comments
      :initform nil
      :documentation
      "A list of comments the precede the before text of an AST.")
     (after-comments
      :accessor after-comments
      :initarg :after-comments
      :initform nil
      :documentation
      "A list of comments the procede the after text of an AST."))
    (:documentation "Mix-in for structured text ASTs."))

  (defclass tree-sitter-ast (indentation
                             structured-text
                             functional-tree-ast)
    ()
    (:documentation "AST for input from tree-sitter."))

  (defclass definition-ast (ast) ()
    (:documentation "AST for something that associates a name with a thing.
The name string is obtained by by DEFINITION-NAME"))

  (defclass comment-ast (ast) ()
    (:documentation "Mix-in for AST classes that are comments.

Superclass of every generated LANGUAGE-comment class."))

  (defclass statement-ast (ast) ()
    (:documentation "Mix-in for AST classes that are statements."))

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

  (defclass variable-declaration-ast (ast) ()
    (:documentation "Mix-in for AST classes that are variable declarations."))

  (defclass identifier-ast (ast) ()
    (:documentation "Mix-in for AST classes that are identifiers."))

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

  (defclass unary-ast (expression-ast) ()
    (:documentation "Mix-in for AST classes that are unary expressions."))

  (defclass binary-ast (expression-ast) ()
    (:documentation "Mix-in for AST classes that are binary expressions."))

  (defclass return-ast (statement-ast) ()
    (:documentation "Mix-in for AST classes that are return statements."))

  (defclass goto-ast (statement-ast) ()
    (:documentation "Mix-in for AST classes that are goto statements."))

  (defclass terminal-symbol ()
    ()
    (:documentation "Mix-in for terminal symbols. Note that this won't fully
cover every terminal symbol, only the ones that aren't named."))

  (defun convert-name (name-string)
    (simplified-camel-case-to-lisp (substitute #\- #\_  (string name-string))))

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
                 (repeat 2)
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
  ;; methods that reconstruct what the source code might look like for an AST.
  ;;
  ;; The methods primarily operate on a "transformed" JSON rule and its
  ;; equivalent "pruned" rule. The transformed JSON rule is an alist
  ;; representation which has removed any precedence rules and inlined every
  ;; rule starting with an underscore. The pruned rule transforms the transformed
  ;; JSON rule by turning it into a list instead of an alist, and replaces any
  ;; subtree that does not have a slot usage with a nil.
  ;;
  ;; The structured-text class has 3 slots--before-text stores text that comes
  ;; before the node, after-text stores text that comes after the node, and
  ;; computed-text stores any text that can vary between different instances
  ;; of a class. The computed-text slots will also contain the text for any
  ;; class that doesn't have any slot usages. This allows for classes, such as
  ;; primitive_type in C, that don't store anything but have variable text to
  ;; still produce something meaningful.
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
  ;; There are some glaring issues at the moment:
  ;;  - choice expansion takes way too long, so it might make sense to find
  ;;    another way to do this or generate them at run-time while still limiting
  ;;    which choices are expanded so that it doesn't expand indefinitely.
  ;;    By not expanding choice branches with identical reproductions--i.e.,
  ;;    slot usages come from the same slot and order but may differ in type--
  ;;    there could be a meaningful reduction in the number of choice
  ;;    expansion subclasses.
  ;;
  ;;  - method dispatch for output-transformation takes forever. When several
  ;;    languages have their methods generated, it is unlikely to return.
  ;;    This could potentially be alleviated by storing the function with
  ;;    class allocation instead. If there's only one language used, it
  ;;    performs reasonably.
  ;;
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
         ((not (listp (car subtree))))
         ((eql (car subtree) :members))
         ((eql (car subtree) :content))
         ((funcall function subtree))))
     tree :tag 'prune :traversal traversal))

  (defun substitute-json-rules (language rules)
    "Substitute rules in RULES based on mappings found for LANGUAGE."
    (let ((substitutions
            (aget (make-keyword language)
                  *tree-sitter-json-rule-substitutions*)))
      (reduce
       (lambda (rules substitution)
         (areplace (car substitution) (cdr substitution) rules))
       substitutions :initial-value rules)))

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

  (defun transform-json-rule (rule grammar)
    "Expand inline rules base on GRAMMAR and :repeat1's in RULE."
    (labels ((expand-repeat1s (tree)
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
                  (let ((name-string (aget :name alist)))
                    ;; NOTE: it appears that all "SYMBOL"s that start with
                    ;;       an underscore are inlined.
                    ;;       DON'T inline the supertype rules.
                    (cond-let result
                      ((not (and (equal (aget :type alist)
                                       "SYMBOL")
                                 (or (eql #\_ (aref name-string 0))
                                     ;; Python has one inline rule without
                                     ;; an underscore.
                                     (member name-string inline-rules
                                             :test #'equal))
                                 (not (member name-string
                                              (aget :supertypes grammar)
                                              :test #'equal))))
                       alist)
                      ((aget (make-keyword (convert-name name-string))
                             (aget :rules grammar))
                       ;; Transform it again before inlining.
                       (transform-json-rule result grammar))
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
                     (push (aget :content alist) type-stack))
                    ((and (equal (aget :type alist) "ALIAS")
                          (aget :named alist))
                     (push (aget :value alist) type-stack))
                    (t alist)))
                branch :traversal :postorder)
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
                           (areplace
                            :name
                            (cons (aget :name alist) (pop types-stack))
                            alist))
                          ((equal (aget :type alist) "FIELD")
                           ;; NOTE: this should be expanded into a
                           ;;       choice inside the field content.
                           (areplace
                            :content
                            `((:type . "CHOICE")
                              (:members
                               ,(aget :content alist)
                               ,@(pop types-stack)))
                            alist))
                          ((and (equal (aget :type alist) "ALIAS")
                                (aget :named alist))
                           (areplace
                            :value
                            (cons (aget :value alist) (pop types-stack))
                            alist))
                          (t alist)))
                      (car similar-branches)))
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
      (merge-similar-choice-branches
       (expand-repeat1s (remove-prec-rules (expand-inline-rules rule))))))

  (defun prune-rule-tree (transformed-json-rule)
    (labels ((gather-field-types (content)
               "Return a list of symbols that CONTENT could be for a field."
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
                  (when (aget :named content)
                    (list (aget :value content))))
                 (("CHOICE" "SEQ")
                  (mappend #'gather-field-types (aget :members content)))
                 ("REPEAT"
                  (gather-field-types (aget :content content)))
                 ("BLANK"
                  (list 'null))))
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
             (handle-rule (rule)
               "Handles dispatching RULE to its relevant rule handler."
               ;; NOTE: this will throw an error if the json schema for
               ;;       the grammar.json files has changed.
               (string-ecase (aget :type rule)
                 ("ALIAS" (handle-alias rule))
                 (("BLANK" "IMMEDIATE_TOKEN" "TOKEN" "PATTERN" "STRING"))
                 (("CHOICE" "SEQ" "REPEAT" "REPEAT1")
                  (handle-seq rule))
                 ("FIELD" (handle-field rule))
                 ("SYMBOL"
                  ;; NOTE: this assumes that all 'SYMBOL's that are seen
                  ;;       going into the children slot. Also NOTE that
                  ;;       the inline rules should be inlined before entering
                  ;;       this function.
                  (handle-symbol rule)))))
      (handle-rule transformed-json-rule)))

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

  (defun collect-rule-slots (tree)
    "Collect all slots used in TREE."
    (collect-rule-tree
     (lambda (subtree)
       (member (car subtree) '(:field :child)))
     tree))

  ;; TODO: run this at before code gen to print out problematic rules.
  (defun structured-rule-p (collapsed-rule)
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
               ;;       branches or there are multiple slots in a different order.
               ;;       This causes an issue trying to reproduce the ordering.
               (iter
                 (for choice in (mappend #'collect-choices (collect-repeats rule)))
                 (for slots-in-branches = (collect-branch-slots choice))
                 ;; NOTE: compare all branches to the first branch. If one
                 ;;       matches the first, then it should be the same as
                 ;;       comparing to the first again.
                 (thereis (some {incompatible-choice-p (car slots-in-branches)}
                                slots-in-branches))))
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
      (not (or (incompatible-choice-in-repeat-p collapsed-rule)
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
    (or
     (member class-name
             (aget (make-keyword language) *tree-sitter-computed-text-asts*))
     (walk-json
      (lambda (subtree)
        ;; TODO: maybe also look at node types to see if it has any children
        ;;       default to computed text node p if it doesn't?
        (when-let (type (aget :type subtree))
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
             (if (aget :named subtree)
                 ;; A named alias should be treated like a symbol.
                 (setf children? t)
                 ;; Aliases can have patterns in them,
                 ;; but they are recast to something else,
                 ;; so they do not need to be stored.
                 (throw 'prune nil)))
            (("FIELD" "SYMBOL")
             ;; The presence of either of these indicate that there are
             ;; children.
             (setf children? t)))))
      json-rule)
     (not children?)))

  (defun generate-computed-text-method
      (transformed-json-rule class-name language &key skip-checking-json)
    "Generate an input transformation method for RULE if one is needed.
CLASS-NAME is used as the specialization for the generated method."
    (when (or skip-checking-json
              (computed-text-ast-p language class-name transformed-json-rule))
      ;; TODO: NOTE: create a generic and default for this somewhere.
      `(defmethod computed-text-node-p ((instance ,class-name)) t)))


  (defun add-slot-to-class-definition
      (class-name class-name->class-definition slot-spec)
    "Destructively add SLOT-SPEC to CLASS-NAME's definition in
CLASS-NAME->CLASS-DEFINITION."
    (let ((class-definition (gethash class-name class-name->class-definition)))
      (symbol-macrolet ((slots (cadddr class-definition)))
        (unless (aget (car slot-spec) slots)
          (setf slots (cons slot-spec slots))))))



  (defun generate-children-method
      (collapsed-rule pruned-rule class-name class-name->class-definition)
    ;; TODO: rename this method now that it doesn't generate any methods.
    ;; NOTE: passing the class name to handle the choice expansion subclassing
    ;;       outside of this method.
    ;; TODO: maybe grab the slots from the collapsed-rule here? If
    ;;       possible make the collapsed-rule an argument.
    ;; TODO: it may make sense to use the pruned rule here instead of the
    ;;       collapsed since the input transformation stuff will ideally
    ;;       be using the pruned rule too.
    ;; TODO: It may also make sense to start storing these rules on the
    ;;       class itself in general?
    (let ((slots
            (remove-duplicates
             (mapcar
              (lambda (cons)
                (if (eql :field (car cons))
                    (cadr cons)
                    'children))
              (collect-rule-slots collapsed-rule)))))
      (when slots
        (mapc {add-slot-to-class-definition
               class-name class-name->class-definition}
              `((pruned-rule
                 :accessor pruned-rule
                 :initform ',pruned-rule
                 :allocation :class
                 :documentation
                 "A rule used to order the children of this class.")
                (slot-usage
                 :accessor slot-usage
                 :initform ',slots
                 :allocation :class
                 :documentation
                 "A set of slots that are used in the pruned-rule."))))))

  (defun get-json-subtree-string (json-subtree choice-resolver)
    "Get the string representation of JSON-SUBTREE. This assumes that there aren't
any slot usages in JSON-SUBTREE."
    ;; TODO: test this function.
    ;; NOTE: assume that token, immediate_token, and pattern automatically
    ;;       mean that a node has variable text. Thus, if any subtree has one of
    ;;       these rules, the AST itself should be printed specially using the
    ;;       computed-text slot.
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
                   `(computed-text
                     :accessor computed-text
                     :initarg :computed-text
                     :allocation :class
                     :initform
                     ',(list (get-json-subtree-string transformed-json-rule))))
                  nil)
                 (t `(defmethod output-transformation
                       ((ast ,class-name)&rest rest &key &aux (parse-stack (parse-order ast)))
                     (declare (ignorable parse-stack rest))
                     (flatten ,(generate-body transformed-json-rule pruned-rule)))))))
      (generate-method transformed-json-rule pruned-rule)))

  (defun convert-to-lisp-type (prefix type-string)
    ;; TODO: this function is also used above. Refactor after rebasing with
    ;;       master.
    (format-symbol 'sel/sw/ts "~a-~a" prefix (convert-name type-string)))
;;; TODO: name this something more descriptive?

  (defun generate-input/output-handling
      (pruned-rule json-rule super-class language-prefix child-types
       class-name->class-definition choice-resolver
       &aux (subclasses
             (aget super-class
                   (aget (make-keyword language-prefix)
                         *tree-sitter-choice-expansion-subclasses*))))
    "Generate a method for a type of AST that returns a choice expansion
subclass based on the order of the children were read in."
    ;; TODO: at some point, it probably makes sense to use the pruned rules over
    ;;       the collapsed rules for the key due to potential collisions.
    ;;       This will require being able to parse the full json rule and
    ;;       relating it back to the cl-tree-sitter return value.

    ;; TODO: language-prefix should be a keyword.

    ;; TODO: child-types needs to be actual lisp types.
    ;;       collapsed-rule needs to have its strings also converted to lisp
    ;;       types.
    ;;       It's likely that we'll have the collapsed rules coming in since
    ;;       we need them in more than one place.
    (labels ((report-problematic-rule ()
               "Reports 'unstructured' rules to *error-output*."
               (unless (structured-rule-p (collapse-rule-tree pruned-rule))
                 (format *error-output* "Problematic Rule: ~a~%" super-class)))
             (get-subclass-name (collapsed-rule)
               "Get the subclass name for COLLAPSED-RULE. If one isn't in
                subclasses, generate a new name."
               ;;NOTE: the types should not be strings but lisp types.
               (or
                (car (find-if (lambda (pair)
                                (equal collapsed-rule (cadr pair)))
                              subclasses))
                (format-symbol :sel/sw/ts "~a" (string-gensym super-class))))
             (convert-to-lisp-types (rule)
               "Converts all strings in RULE to lisp types."
               (map-tree
                (lambda (node)
                  (if (typep node 'string)
                      (convert-to-lisp-type language-prefix node)
                      node))
                rule))
             (generate-subclass
                 (subclass-pair &aux (class-name (car subclass-pair)))
               "Generate a defclass form for SUBCLASS-PAIR."
               ;; TODO: add this to the class-definition table.
               ;; TODO: should also consider adding support for mixins
               ;;       and additional slot options.
               (setf (gethash class-name class-name->class-definition)
                     `(defclass ,class-name (,super-class)
                        ((rule :initform ',(cadr subclass-pair)
                               :reader rule
                               :allocation :class)))))
             (generate-subclasses (subclass-pairs)
               "Generate a defclass forms for SUBCLASS-PAIRS."
               ;; TODO: rename this function since it doesn't generate anything
               ;;       internal to this function anymmore.
               (map nil #'generate-subclass subclass-pairs))
             (generate-children-methods (subclass-pairs)
               "Generate the methods for handling children for
                every subclass pair in SUBCLASS-PAIRS."
               ;; TODO: rename this function since it doesn't generate anything
               ;;       anymore.
               (mapc
                (op (generate-children-method
                     (cadr _) (caddr _1) (car _1) class-name->class-definition))
                subclass-pairs))
             (generate-input-subclass-dispatch (json-expansions subclass-pairs)
               "Generate a method to return the name of the subclass
                to be used by the parse-tree returned by tree-sitter."
               `(defmethod get-choice-expansion-subclass
                    ((class (eql ',super-class)) parse-tree
                     &aux (child-types ',child-types))
                  (econd
                   ,@(mapcar
                      (lambda (json-rule subclass-pair)
                        `((match-parsed-children
                           ,language-prefix
                           ',json-rule
                           ',(caddr subclass-pair) child-types parse-tree)
                          ',(car subclass-pair)))
                      json-expansions
                      subclass-pairs))))
             (generate-computed-text-methods (json-expansions subclass-pairs)
               "Generate the variable text methods for the rules in
                JSON-EXPANSION."
               (mapcar
                (op (generate-computed-text-method _ (car _) language-prefix))
                json-expansions subclass-pairs))
             (generate-output-transformations
                 (pruned-expansions json-expansions subclass-pairs)
               "Generate the output transformations for each subclass."
               (mapcar (lambda (pruned-rule json-rule class-name)
                         (generate-output-transformation
                          pruned-rule json-rule language-prefix class-name
                          class-name->class-definition choice-resolver))
                       pruned-expansions json-expansions
                       (mapcar #'car subclass-pairs))))
      (report-problematic-rule)
      ;; TODO: refactor this and children-parser as it accepts a
      ;;       pruned rule and wasn't before.
      (mvlet* ((pruned-rule-expansions
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
                            (mapcar #'convert-to-lisp-types pruned-rule-expansions))
                    `((,super-class
                       ,(car collapsed-rule-expansions)
                       ,(convert-to-lisp-types (car pruned-rule-expansions)))))))
        (when expansions? (generate-subclasses subclass-pairs))
        (generate-children-methods subclass-pairs)
        `(progn
           ,(and expansions? (generate-input-subclass-dispatch
                              json-expansions subclass-pairs))
           ,@(generate-computed-text-methods json-expansions subclass-pairs)
           ,@(generate-output-transformations
              pruned-rule-expansions json-expansions subclass-pairs)))))

  (defun generate-structured-text-methods
      (grammar types language-prefix
       class-name->class-definition choice-resolver)
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
    (labels ((generate-code (transformed-json type-json)
               "Generate the code for TRANSFORMED-JSON that is of the type
                specified by TYPE-JSON."
               (generate-input/output-handling
                (prune-rule-tree transformed-json)
                transformed-json
                (convert-to-lisp-type language-prefix (aget :type type-json))
                ;; NOTE: this should be a keyword.
                language-prefix
                (mapcar [{convert-to-lisp-type language-prefix}
                         {aget :type}]
                        (aget :types (aget :children type-json)))
                class-name->class-definition
                choice-resolver))
             (generate-terminal-code (type-string class-name)
               "Generate the code for a terminal symbol."
               ;; TODO: rename this function
               ;; destructively modify the class definition.
               (add-slot-to-class-definition
                class-name class-name->class-definition
                `(computed-text
                  :accessor computed-text
                  :initarg :computed-text
                  :allocation :class
                  :initform ',(list type-string))))
             (get-transformed-json-table ()
               "Get a hash table containing transformed JSON rules."
               (let ((rules (add-aliased-rules
                             (substitute-json-rules
                              language-prefix
                              (aget :rules grammar)))))
                 (alist-hash-table
                  (mapcar (lambda (rule)
                            (list (car rule)
                                  (transform-json-rule
                                   (cdr rule)
                                   (areplace :rules rules grammar))))
                          rules))))
             (get-superclasses-set ()
               "Get a hash set containing the names of superclasses for the
                language."
               (alist-hash-table
                (mapcar {cons _ t} (aget :supertypes grammar))
                :test #'equal)))
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
             (for named? = (aget :named type))
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
                 nil lisp-type language-prefix :skip-checking-json t))
               (t
                ;; If a type doesn't have a rule and is unnamed, it is considered
                ;; a terminal symbol.
                (generate-terminal-code
                 type-string
                 (if (gethash terminal-lisp-type class-name->class-definition)
                     terminal-lisp-type
                     lisp-type))))))))

  (defun create-tree-sitter-classes
      (node-types-file grammar-file name-prefix
       &key ast-superclasses base-ast-superclasses
         software-superclasses software-direct-slots
         ast-extra-slot-options
         ast-extra-slots
         json-subtree-choice-resolver
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
         (node-types (decode-json-from-string
                      (file-to-string node-types-file)))
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
               (let* ((child-slot-order
                       (when fields
                         (mapcar
                          (lambda (slot-keyword)
                            (cons
                             (translate-to-slot-name slot-keyword name-prefix)
                             (if (aget :multiple (aget slot-keyword fields))
                                 0
                                 1)))
                          (slot-order type fields grammar-rules))))
                      (definer
                       (if fields 'define-node-class 'defclass)))
                 `(,definer
                   ,class-name
                   (,@(or (get-supertypes-for-type type)
                          `(,ast-superclass)))
                   (,@(create-slots class-name fields)
                    (child-slots
                     :initform
                     ',(append '((before-comments . 0))
                               child-slot-order
                               '((children . 0))
                               '((after-comments . 0)))
                     :allocation :class)
                    ,@(gethash class-name class->extra-slots))
                   ;; NOTE: this is primarily for determing which rule this
                   ;;       was generated for.
                   (:documentation ,(format nil "Generated for ~a." type))
                   ,@(when (eq definer 'define-node-class)
                       `((:method-options :skip-children-definition))))))
             (create-terminal-symbol-class (type)
               "Create a new class that represents a terminal symbol.
                In the case that there's a non-terminal with the same name,
                append '-terminal' to the end of it."
               `(defclass ,(if (gethash
                                (format-symbol 'sel/sw/ts "~a-~a"
                                               name-prefix
                                               (string-upcase type))
                                symbols-to-export)
                               (make-class-name
                                (format-symbol 'sel/sw/ts "~a-~a"
                                               (string-upcase type) 'terminal))
                               (make-class-name type))
                    (,ast-superclass terminal-symbol)
                  ()
                  (:documentation
                   ,(format nil "Generated for terminal symbol '~a'" type))))
             (create-node-class
                 (grammar-rules node-type
                  &aux (type (aget :type node-type))
                    (subtypes (aget :subtypes node-type))
                    (named-p (aget :named node-type)))
               "Create a class for  NODE-TYPE."
               (let ((class-definition
                       (cond
                         (subtypes (create-supertype-class type))
                         (named-p
                          (create-type-class
                           type
                           (aget :fields node-type)
                           grammar-rules))
                         ;; Terminal Symbol
                         (t (create-terminal-symbol-class type)))))
                 (setf (gethash (cadr class-definition)
                                class-name->class-definition)
                       class-definition)))
             (create-external-class (name)
               "Create a class for an external rule."
               `(defclass ,(make-class-name name) (,ast-superclass) ()))
             (create-external-classes (grammar)
               "Create classes for the external rules for the grammar file."
               (mapcar (op (create-external-class (aget :name _)))
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
               json-subtree-choice-resolver)))
        `(progn
           (eval-always
             (define-software ,(make-class-name) (tree-sitter
                                                  ,@software-superclasses)
               (,@software-direct-slots)
               (:documentation
                ,(format nil "~a tree-sitter software representation."
                         name-prefix)))

             (define-node-class ,(make-class-name "ast")
                 (tree-sitter-ast ,@(mapcar [#'car #'ensure-list]
                                            base-ast-superclasses))
               ;; NOTE: ensure there is always a children slot.
               ;;       This is important for classes that don't have
               ;;       it but can have comments mixed in.
               ((children ,@(mappend (op `(:accessor ,_))
                                     (make-accessor-names :children))
                          :documentation
                          "Returns all language-specific children.
Unlike the `children` methods which collects all children of an AST from any slot."
                          :initarg :children
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
             (defclass ,(make-class-name "comment")
                 ,(remove-duplicates
                   `(,ast-superclass
                     ,@(get-supertypes-for-type "comment")
                     comment-ast)
                   :from-end t)
               ()
               (:documentation "Generated for parsed comments."))

             (defclass ,(make-class-name "error")
                 ,(remove-duplicates
                   `(,ast-superclass
                     ,@(get-supertypes-for-type "error")
                     parse-error-ast)
                   :from-end t)
               ((children :initarg :children :initform nil)
                (child-slots :initform '((children . 0))
                             :allocation :class))
               (:documentation "Generated for parsing errors."))

             ,@(create-external-classes grammar)

             ,@(iter (for (nil definition) in-hashtable
                          class-name->class-definition)
                 (collect definition))

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
                               &allow-other-keys)
             (convert 'tree-sitter-ast spec
                      :superclass to-type
                      :string-pass-through string-pass-through
                      :computed-text-parent-p computed-text-parent-p))

           (defmethod convert ((to-type (eql ',ast-superclass)) (string string)
                               &rest args &key &allow-other-keys)
             (apply #'convert 'tree-sitter-ast string :superclass to-type args))

           (defmethod parse-asts ((obj ,(make-class-name))
                                  &optional (source (genome-string obj)))
             (convert ',(make-class-name "ast") source))

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

           (defgeneric output-transformation
               (ast &rest rest &key &allow-other-keys)
             (:documentation "Return a list of strings and AST objects that
are ordered for reproduction as source text.")
             (:method ((ast structured-text) &rest rest &key &allow-other-keys)
               (declare (ignorable rest))
               (flatten
                (list
                 (before-text ast)
                 ;; Expand all rules here.
                 (computed-text-output-transformation ast)
                 (after-text ast))))
             (:method :around ((ast structured-text)
                               &rest rest &key &allow-other-keys)
               (declare (ignorable rest))
               ;; TODO: go through the list and grab the comments around each
               ;;       AST, reinserting them.
               (mappend
                (lambda (output)
                  (cond
                    ((typep output 'structured-text)
                     (append (before-comments output)
                             (list output)
                             (after-comments output)))
                    (t (list output))))
                (call-next-method))))

           (defmethod children ((ast structured-text))
             (remove-if-not {typep _ 'ast} (output-transformation ast)))

           (defgeneric computed-text-node-p (ast)
             (:documentation "Return T if AST is a computed-text node. This is a
node where part of the input will need to be computed and stored to reproduce
the source-text.")
             (:method (ast) nil))

           (defgeneric get-choice-expansion-subclass (class spec)
             (:documentation "Get the subclass of CLASS associated with SPEC.")
             (:method (class spec)
               (declare (ignorable spec))
               class))

           (defgeneric transform-parse-tree (language class parse-tree)
             (:documentation "Transform PARSE-TREE based on LANGUAGE and CLASS.")
             (:method (language class parse-tree
                       &aux (descriptor (and (listp parse-tree)
                                             (car parse-tree))))
               (cond
                 (class parse-tree)
                 ((and descriptor (not (= 3 (length parse-tree))))
                  parse-tree)
                 ;; :class
                 ((keywordp descriptor)
                  (transform-parse-tree
                   language
                   (convert-to-lisp-type language descriptor)
                   parse-tree))
                 ;; :slot, :class list
                 ((and (consp descriptor)
                       (keywordp (cadr descriptor)))
                  (transform-parse-tree
                   language
                   (convert-to-lisp-type language (cadr descriptor))
                   parse-tree))
                 (t parse-tree))))

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
      (find-deepest [{string= string} #'source-text] (call-next-method))
      (call-next-method)))


;;;; Tree-sitter language definitions.

(progn #.`(progn ,@(mappend {apply #'tree-sitter-ast-classes}
                            *tree-sitter-language-files*)))

(defmacro when-class-defined ((software-class) &body body)
  "Checks if SOFTWARE-CLASS is defined at compile-time. If so,
it expands into BODY. Otherwise, the expansion is nil."
  (when (find-class software-class nil)
    `(progn
       ,@body)))

(defun interpreted-phenome (obj bin)
  "Create a phenotype of the interpreted software OBJ."
  (to-file obj bin)
  (values bin 0 nil nil nil))

(defmethod get-parent-full-stmt (obj (ast tree-sitter-ast))
  (if (typep ast 'statement-ast)
      ast
      (get-parent-full-stmt obj (get-parent-ast obj ast))))

(defgeneric ast-type-to-rebind-p (ast)
  (:documentation "Return T if AST is of a type where its variables/functions
should be rebound.")
  (:method (ast) nil)
  (:method ((ast identifier-ast)) t))

(defmethod rebind-vars ((ast interleaved-text)
                        var-replacements fun-replacements)
  "Replace variable and function references, returning a new AST.
* AST node to rebind variables and function references for
* VAR-REPLACEMENTS list of old-name, new-name pairs defining the rebinding
* FUN-REPLACEMENTS list of old-function-info, new-function-info pairs defining
the rebinding"
  (if (ast-type-to-rebind-p ast)
      (copy ast :interleaved-text (mapcar {rebind-vars _ var-replacements
                                                         fun-replacements}
                                          (interleaved-text ast)))
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
    (ast (values (children value)
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

(defgeneric parameter-type (parameter-ast)
  (:documentation "Return a representation of the TYPE of PARAMETER-AST."))

(defgeneric parameter-name (parameter-ast)
  (:documentation "Return the name of PARAMETER-AST."))

(defgeneric function-body (ast)
  (:documentation "Return the body of AST."))

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

(defmethod scopes ((obj normal-scope) (ast ast))
  (labels ((get-parent-decl (obj identifier)
             "For the given IDENTIFIER AST, return the parent declaration."
             (car (remove-if-not {typep _ 'variable-declaration-ast}
                                 (get-parent-asts obj identifier))))
           (ast-to-scope (obj scope ast)
             `((:name . ,(source-text ast))
               (:decl . ,(or (get-parent-decl obj ast) ast))
               (:scope . ,scope))))
    (unless (null (ast-path obj ast))
      (let ((scope (enclosing-scope obj ast)))
        (cons (reverse
               (mapcar {ast-to-scope obj scope}
                       (append (inner-declarations scope)
                               (mappend #'outer-declarations
                                        (statements-in-scope obj scope ast)))))
              (scopes obj scope))))))

(defgeneric find-enclosing (type software ast)
  (:documentation "Return the nearest enclosing AST of TYPE in SOFTWARE.")
  (:method ((type t) (software tree-sitter) (ast ast))
    ;; (assert (typep type '(or symbol (cons symbol t) class)))
    (find-if {typep _ type} (get-parent-asts software ast))))

(defgeneric find-preceding (type software ast)
  (:documentation "Return any siblings of TYPE preceding AST in SOFTWARE.")
  (:method ((type t) (software tree-sitter) (ast tree-sitter-ast))
    ;; (assert (typep type '(or symbol (cons symbol t) class)))
    (when-let ((parent (get-parent-ast software ast)))
      (iter (for child in (children parent))
            (until (eql child ast))
            (when (typep child type)
              (collect child))))))

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

(defgeneric imports (ast)
  (:documentation "Return a list of the imports of AST.
Every element in the list has the following form:
    (full-name alias/nickname . named-imports)"))

(defgeneric provided-by (software ast)
  (:documentation
   "Return the library, package, or system in SOFTWARE providing AST."))

(defgeneric comparisonp (ast)
  (:documentation "Is AST a comparison?")
  (:method ((ast t)) nil))

;;;; Python
;;; Move this to its own file?
(when-class-defined (python)

  (defmethod transform-parse-tree
      ((language (eql ':python)) (class (eql 'python-function-definition))
       parse-tree)
    "Transform PARSE-TREE such that an asyn field is present for 'async'
identifiers."
    (append
     (butlast parse-tree)
     (list
      (mapcar
       (lambda (child-tree)
         (cond
           ((equal (car child-tree) :async)
            (cons (list :async :async) (cdr child-tree)))
           (t child-tree)))
       (lastcar parse-tree)))))

  (defmethod comparisonp ((ast python-comparison-operator))
    t)

  ;; Methods common to all software objects
  (defmethod imports ((software python))
    (imports (genome software)))

  (defmethod imports ((ast python-ast)) nil)

  (defmethod imports ((ast python-module))
    (mappend #'imports (children ast)))

  (defmethod imports ((ast python-import-statement))
    (mapcar #'imports (python-name ast)))

  (defmethod imports ((ast python-dotted-name))
    (list (source-text ast)))

  (defmethod imports ((ast python-aliased-import))
    (list (source-text (python-name ast)) (source-text (python-alias ast))))

  (defmethod imports ((ast python-import-from-statement))
    (if (python-name ast)
        (mapcar [{list (source-text (python-module-name ast)) nil} #'source-text]
                (python-name ast))
        (list (list (source-text (python-module-name ast))))))

  (defmethod provided-by ((software python) ast)
    (provided-by (genome software) ast))

  (defmethod provided-by ((root python-ast) (ast python-identifier))
    (car (find-if [{equalp (source-text ast)} #'third] (imports root))))

  (defmethod provided-by ((root python-ast) (ast python-attribute))
    (labels ((top-attribute (root ast)
               (let ((parent (get-parent-ast root ast)))
                 (if (and parent (typep parent 'python-attribute))
                     (top-attribute root parent)
                     ast))))
      (source-text (python-object (top-attribute root ast)))))

  (defmethod provided-by (root (ast python-expression-statement))
    (provided-by root (first (children ast))))

  (defmethod provided-by ((root python-ast) (ast python-call))
    (provided-by root (call-function ast)))

  (defmethod phenome ((obj python) &key (bin (temp-file-name)))
    (interpreted-phenome obj bin))

  (defmethod type-in ((obj python) (ast python-ast))
    nil)

  (defmethod type-in ((obj python) (ast python-identifier))
    (let ((name (source-text ast))
          (scopes (scopes obj ast)))
      (when-let* ((binding
                   (find-if-in-scopes
                    (op (equal (aget :name _) name))
                    scopes))
                  (decl (aget :decl binding)))
        (make-keyword
         (string-upcase
          (source-text
           (or
            ;; Extract just the name of the variable from a parameter.
            (some
             (lambda (parent)
               (and (typep parent 'python-parameter)
                    (find-if
                     (lambda (child)
                       (and (typep child 'python-identifier)
                            (equal name (source-text ast))))
                     parent)))
             decl)
            decl)))))))

  (defmethod enclosing-scope ((obj python) (ast python-ast))
    "Return the enclosing scope of AST in OBJ.
OBJ python software object
AST ast to return the enclosing scope for"
    (or (find-if (lambda (parent)
                   ;; Normal case: AST is a member of a class
                   ;; of ASTs defining a new scope.
                   (typep parent '(or
                                   python-function-definition
                                   python-class-definition
                                   python-lambda)))
                 (get-parent-asts* obj ast))
        (genome obj)))

  (defmethod scopes ((obj python) (target-ast python-ast)
                     &aux (enclosing-scope (enclosing-scope obj target-ast)))
    "Return lists of variables in each enclosing scope of AST.
Each variable is represented by an alist containing :NAME, :DECL, and :SCOPE.
OBJ python software object
AST ast to return the scopes for"
    ;; NOTE: in the unlikely event this function becomes a bottleneck, it may
    ;;       make sense to cache the get-vars calls.
    (labels ((build-alist (ast name scope)
               "Return an alist containing :name, :decl, and :scope for the
                variable in AST."
               `((:decl . ,ast)
                 (:name . ,name)
                 (:scope . ,scope)))
             (build-alist* (get-vars-alist
                            &aux (definition (aget :definition get-vars-alist)))
               "Return an alist containing :name, :decl, and :scope for
                GET-VARS-ALIST."
               (build-alist
                definition
                (aget :name get-vars-alist)
                (aget :scope get-vars-alist)))
             (name-in-get-vars-p (obj ast name)
               "Return the variable alist that corresponds to
                NAME if it exists."
               (find-if
                (lambda (alist)
                  (equal name (aget :name alist)))
                (get-vars obj ast)))
             (find-get-vars-binding (obj ast enclosing-scope name)
               "Find a variable bound in AST that is named NAME and in
                ENCLOSING-SCOPE."
               (find-if
                (lambda (ast)
                  (eq (aget :scope (name-in-get-vars-p obj ast name))
                      enclosing-scope))
                ast))
             (find-declaration (function ast)
               "Find the declaration that is returned by FUNCTION
                starting at AST."
               (find-if
                (lambda (ast)
                  (when-let ((declaration (funcall function ast)))
                    (return-from find-declaration declaration)))
                ast))
             (find-nonlocal-binding* (name enclosing-scope)
               "Find the nonlocal binding for NAME in ENCLOSING-SCOPE."
               (find-declaration
                (lambda (ast)
                  (if (typep ast 'python-nonlocal-statement)
                      (when (and
                             (find-if
                              (lambda (identifier)
                                (equal name
                                       (source-text identifier :trim t)))
                              (remove-if-not {typep _ 'python-identifier}
                                             (python-children ast)))
                             (not (eq enclosing-scope (genome obj))))
                        (find-nonlocal-binding
                         name (enclosing-scope obj enclosing-scope)))
                      (find-get-vars-binding obj ast enclosing-scope name)))
                (get-asts-in-namespace obj enclosing-scope)))
             (find-nonlocal-binding (name enclosing-scope)
               "Find and build the alist for the nonlocal binding for NAME
                in ENCLOSING-SCOPE."
               (build-alist
                (find-nonlocal-binding* name enclosing-scope)
                name enclosing-scope))
             (find-global-binding
                 (identifier &aux (genome (genome obj))
                               (name (car (computed-text identifier))))
               "Find the global binding for NAME in ENCLOSING-SCOPE."
               (build-alist
                (find-declaration
                 (lambda (ast)
                   (find-get-vars-binding obj ast genome name))
                 (remove nil (children genome)))
                name genome))
             (find-enclosing-bindings (scope)
               "Find the enclosing bindings that occur in scope."
               (mapcar
                #'build-alist*
                (remove-if-not
                 (lambda (alist &aux (attributes (aget :attributes alist)))
                   (cond
                     ;; NOTE: imports behave differently than other bindings
                     ;;       that are available from enclosing scopes.
                     ((member :import attributes)
                      (not
                       (path-later-p obj (aget :definition alist) target-ast)))
                     ((intersection '(:class :function) attributes) t)))
                 (mappend {get-vars obj} (remove nil (children scope))))))
             (find-local-bindings ()
               "Find local bindings in scope. Returns the py-name
                objects associated with the bindings."
               ;; NOTE: this doesn't correctly return bindings
               ;;       that occur based on control flow like with if-else
               ;;       statements. This typically isn't something that can
               ;;       be accounted for before runtime.
               (remove-duplicates
                (mappend
                 (lambda (ast)
                   (remove-if-not
                    (lambda (alist)
                      ;; Check for child scopes allows for
                      ;; namespace bindings in list comps and
                      ;; such.
                      (shares-path-of-p
                       obj target-ast (aget :scope alist)))
                    (get-vars obj ast)))
                 ;; Remove ASTs after.
                 (remove-if
                  (lambda (ast)
                    (path-later-p obj ast target-ast))
                  (get-asts-in-namespace obj enclosing-scope)))
                :test (lambda (alist1 alist2)
                        (equal (aget :name alist1)
                               (aget :name alist2)))
                :from-end t))
             (get-global-bindings ()
               "Get the global bindings in scope."
               (mappend (lambda (ast)
                          (mapcar #'find-global-binding
                                  (remove-if-not {typep _ 'python-identifier}
                                                 (python-children ast))))
                        (remove-if-not
                         {typep _ 'python-global-statement}
                         (get-asts-in-namespace obj enclosing-scope))))
             (get-nonlocal-bindings ()
               "Get the nonlocal bindings in scope."
               (mappend (lambda (ast)
                          (mapcar
                           {find-nonlocal-binding
                            _ (enclosing-scope obj enclosing-scope)}
                           (mapcar
                            (op (source-text _ :trim t))
                            (remove-if-not {typep _ 'python-identifier}
                                           (python-children ast)))))
                        (remove-if-not
                         {typep _ 'python-nonlocal-statement}
                         (get-asts-in-namespace obj enclosing-scope))))
             (get-enclosing-bindings
                 (scope &aux (enclosing-scope (enclosing-scope obj scope))
                          (enclosing-bindings (find-enclosing-bindings scope)))
               "Get the enclosing bindings available in scope."
               (if (eq scope enclosing-scope)
                   enclosing-bindings
                   (append enclosing-bindings
                           (get-enclosing-bindings enclosing-scope))))
             (get-local-bindings ()
               "Get the local bindings available in scope."
               ;; Remove bindings after
               (remove-if-not
                (lambda (binding-alist)
                  (path-later-p obj target-ast (aget :decl binding-alist)))
                ;; build-alist
                (mapcar #'build-alist* (find-local-bindings))))
             (group-by-scope (bindings)
               "Group BINDINGS by scope."
               (assort bindings :key (lambda (alist) (aget :scope alist))))
             (sort-top->down (scopes)
               "Sort SCOPES from the top-most to the bottom-most."
               (sort scopes
                     (lambda (ast1 ast2)
                       (path-later-p obj ast2 ast1))
                     :key (lambda (list)
                            (aget :scope (car list))))))
      (sort-top->down
       (group-by-scope
        (remove-duplicates
         (remove-if
          #'null
          ;; NOTE: order of the append matters here for get-except-binding and
          ;;       get-local-bindings.
          (append (get-global-bindings)
                  (get-nonlocal-bindings)
                  (get-enclosing-bindings enclosing-scope)
                  (get-local-bindings)))
         :test (lambda (alist1 alist2)
                 (equal (aget :name alist1) (aget :name alist2)))
         :from-end t)))))

  (defmethod get-unbound-vals ((obj python) (ast python-ast))
    "Return all variables used (but not defined) within AST.
* OBJ python software object containing AST
* AST ast to retrieve unbound variables within"
    (labels ((call-name-p (parent name)
               "Return T if NAME is a function or method call."
               (typecase parent
                 (python-call
                  (let ((func (call-function parent)))
                    (typecase func
                      ;; free function
                      (python-identifier (eq func name))
                      ;; method call
                      (python-attribute (eq (python-attribute func) name)))))
                 (python-attribute
                  (call-name-p (get-parent-ast obj parent) name))))
             (bound-name-p (parent)
               (typep parent
                      '(or
                        python-function-definition
                        python-class-definition)))
             (get-unbound-vals-helper (obj parent ast)
               (remove-duplicates
                (apply #'append
                       (when (and (typep ast 'python-identifier)
                                  (not (or (bound-name-p parent)
                                           (call-name-p parent ast))))
                         (list (cons :name (source-text ast))))
                       (mapcar {get-unbound-vals-helper obj ast}
                               (remove nil (children ast))))
                :test #'equal)))
      (get-unbound-vals-helper obj (get-parent-ast obj ast) ast)))

  (defmethod get-unbound-funs ((obj python) (ast python-ast)
                               &aux (children (remove nil (children ast))))
    "Return all functions used (but not defined) within AST.  The returned
value will be of the form (list FUNCTION-ATTRS) where FUNCTION-ATTRS is a
list of form (FUNCTION-NAME UNUSED UNUSED NUM-PARAMS).

* OBJ python software object containing AST
* AST ast to retrieve unbound functions within"
    (remove-duplicates
     (apply #'append
            (when-let ((callee (and (typep ast 'python-call)
                                    (call-function ast))))
              (cond ((typep callee 'python-identifier)
                     ;; Free function call
                     (list (list (source-text callee)
                                 nil nil
                                 (length (call-arguments ast)))))
                    ((typep callee 'python-attribute)
                     ;; Member Function call
                     ;;
                     (list (list (source-text (python-attribute callee))
                                 nil nil
                                 (length
                                  (call-arguments ast)))))
                    (t nil)))
            (mapcar {get-unbound-funs obj} children))
     :test #'equal))

  ;; TODO: move into parseable?
  (-> find-if-in-scopes (function list) list)
  (defun find-if-in-scopes (predicate scopes)
    "Return the first binding in SCOPES that satisfies PREDICATE."
    (mapc
     (lambda (scope)
       (when-let ((return-value (find-if predicate scope)))
         (return-from find-if-in-scopes return-value)))
     scopes)
    nil)

  (defmethod get-function-from-function-call ((obj python) (ast python-ast))
    (match ast
      ((python-call
        (python-function identifier))
       (when-let ((function-alist
                   (find-if-in-scopes
                    (lambda (scope)
                      (and (equal (source-text identifier)
                                  (aget :name scope))
                           (typep (aget :decl scope)
                                  'python-function-definition)))
                    (scopes obj ast))))
         (aget :decl function-alist)))))

  (defmethod map-arguments-to-parameters
      ((obj python) (funcall python-ast)
       &aux (function (get-function-from-function-call obj funcall)))
    ;; This method assumes a well-formed function and function call.
    (unless function
      ;; Exit early if the function isn't found to prevent errors.
      (return-from map-arguments-to-parameters nil))
    (labels ((get-parameter-alist (parameters)
               "Construct an alist of all the parameters."
               (let ((positional-only
                       ;; This is a hack around a lack of PEP 570 support
                       ;; in tree-sitter-python.
                       (position-if [{equal "/,"} {source-text}] parameters))
                     (list-splat
                       (position-if {typep _ 'python-list-splat-pattern}
                                    parameters))
                     (dictionary-splat
                       (position-if {typep _ 'python-dictionary-splat-pattern}
                                    parameters)))
                 `((:positional ,@(when positional-only
                                    (subseq parameters 0 positional-only)))
                   (:regular ,@(subseq parameters
                                       (or (and positional-only
                                                (1+ positional-only))
                                           0)
                                       (or list-splat dictionary-splat)))
                   (:list-splat ,(when-let ((list-splat-ast
                                             (and list-splat
                                                  (nth list-splat parameters))))
                                   (unless (equal (source-text list-splat-ast)
                                                  "*")
                                     list-splat-ast)))
                   (:keyword ,@(when list-splat
                                 (subseq
                                  parameters (1+ list-splat) dictionary-splat)
                                 (subseq parameters (1+ list-splat))))
                   (:dictionary-splat ,(when dictionary-splat
                                         (nth dictionary-splat parameters))))))
             (same-name-p (arg parameter)
               "Return T if PARAMETER1 and PARAMETER2 represent the same id."
               (equal (source-text arg) (source-text parameter)))
             (get-identifier (ast)
               "Get the relevant identifier for AST if one exists."
               (typecase ast
                 ((or python-default-parameter python-keyword-argument)
                  (python-name ast))
                 ((or python-list-splat-pattern python-dictionary-splat-pattern)
                  (car (python-children ast)))
                 (t ast)))
             (get-default-parameters (parameters)
               "Get a mapping of default parameters to their defaults."
               ;; NOTE: collect all defaults and remove the duplicates later.
               (mapcar
                (lambda (default)
                  (cons (python-value default)
                        (python-name default)))
                (remove-if-not {typep _ 'python-default-parameter}
                               parameters)))
             (get-positional-args-to-parameters (parameters-alist args-list)
               "Get a mapping of positional arguments to their parameters."
               (let ((positionals  (append (aget :positional parameters-alist)
                                           (aget :regular parameters-alist)))
                     (list-splat (car (aget :list-splat parameters-alist)))
                     (positional-args
                       (remove-if {typep _ 'python-keyword-argument} args-list)))
                 (append (mapcar
                          (lambda (arg parameter)
                            (cons arg (get-identifier parameter)))
                          positional-args positionals)
                         (when list-splat
                           (list
                            (cons (create-tuple
                                   (drop (length positionals) positional-args))
                                  (get-identifier list-splat)))))))
             (create-keyword-dictionary (keyword-args)
               "Create the dictionary created for '**' style parameters."
               (create-dictionary
                (mapcar
                 (lambda (arg)
                   (convert
                    'python-ast
                    `((:class . :string)
                      (:computed-text
                       ,(format nil "\"~a\""
                                (source-text
                                 (python-name arg)))))))
                 keyword-args)
                (mapcar #'python-value keyword-args)))
             (get-keyword-args-to-parameters (parameters-alist args-list)
               "Get a mapping of keyword arguments to their parameters."
               (iter
                 (iter:with
                  dict-splat = (car (aget :dictionary-splat parameters-alist)))
                 (iter:with dict-splat-args)
                 (for arg in (remove-if-not {typep _ 'python-keyword-argument}
                                            args-list))
                 (if-let ((parameter
                           (find-if
                            [{same-name-p (get-identifier arg)}
                             #'get-identifier]
                            ;; potential keywords
                            (append (aget :regular parameters-alist)
                                    (aget :keyword parameters-alist)))))
                   (collect (cons (python-value arg) (get-identifier parameter))
                     into mapping)
                   (push arg dict-splat-args))
                 (finally
                  (return
                    (if dict-splat
                        (cons (cons (create-keyword-dictionary dict-splat-args)
                                    (get-identifier dict-splat))
                              mapping)
                        mapping))))))
      (let* ((parameters (python-children (python-parameters function)))
             (parameters-alist (get-parameter-alist parameters))
             (args-list (call-arguments funcall)))
        ;; NOTE: all default parameters are returned by get-default-parameters.
        ;;       The defaults that are actually used need to be removed here.
        (remove-duplicates
         ;; NOTE: append order matters.
         (append (get-positional-args-to-parameters
                  parameters-alist args-list)
                 (get-keyword-args-to-parameters
                  parameters-alist args-list)
                 (get-default-parameters parameters))
         :test #'same-name-p :key #'cdr :from-end t))))

  (defmethod assign-to-var-p ((ast python-ast) (identifier python-ast))
    ;; Return the python-identifier that matches in case the caller wants
    ;; to check if it is the same as identifier.
    (match ast
      ((python-augmented-assignment :python-left lhs)
       (and (identical-name-p identifier lhs) lhs))
      ((python-assignment
        :python-left lhs)
       (typecase lhs
         (python-identifier (and (identical-name-p identifier lhs) lhs))
         (python-pattern-list
          (find-if {identical-name-p identifier} (python-children lhs)))))))

  (defmethod function-name ((node python-function-definition))
    (source-text (python-name node)))

  (defmethod function-name ((node python-attribute))
    (source-text (python-attribute node)))

  (defmethod function-parameters ((ast python-function-definition))
    (function-parameters (second (children ast))))

  (defmethod function-parameters ((ast python-parameters))
    (children ast))

  (defmethod end-of-parameter-list
      ((software python) (function-node function-ast))
    (ematch function-node
      ((python-function-definition
        :python-parameters (and parameters (type node)))
       (ematch (ast-end software parameters)
         ((source-location :line line :column column)
          (make 'source-location :line line :column column))))
      ((python-lambda :python-parameters (and parameters (type node)))
       (ast-end software parameters))
      ((python-lambda :python-parameters nil)
       (ematch (ast-start software function-node)
         ((source-location :line line :column column)
          (make 'source-location
                :line line
                :column (+ column #.(length "lambda"))))))))

  (defmethod function-body ((ast python-function-definition)) (python-body ast))

  ;; NB There is no single "operator" for a chained comparison.
  (defmethod lhs ((ast python-comparison-operator))
    (if (single (python-operators ast))
        (first (sorted-children ast))
        (call-next-method)))
  (defmethod rhs ((ast python-comparison-operator))
    (if (single (python-operators ast))
        (third (sorted-children ast))
        (call-next-method)))
  (defmethod operator ((ast python-comparison-operator))
    (if (single (python-operators ast))
        (first (python-operators ast))
        (call-next-method)))

  (defmethod operator ((ast python-not))
    (first (interleaved-text ast)))

  (defmethod control-flow-condition ((ast python-if-statement)) (car (children ast)))
  (defmethod control-flow-condition ((ast python-while-statement)) (car (children ast)))

  ;; Indentation
  (defmethod indentablep ((ast python-string)) nil)

  (defmethod get-default-indentation ((ast python-ast) (parents list))
    ;; Search for the first AST with a colon in the interleaved-text and
    ;; uses its indent-children value. If none are found, call-next-method.
    (labels ((indented-obj-p (ast &aux (indent-children (indent-children ast)))
               "Return T if AST is an obj that should have indented children."
               (and
                indent-children
                (not (eql t indent-children))
                (typep ast '(or
                             ;; TODO: at some point, add more classes here.
                             python-class-definition python-function-definition
                             python-if-statement python-while-statement
                             python-for-statement)))))
      (if-let ((indented-obj (find-if #'indented-obj-p (or (lastcar parents)
                                                           ast))))
        (indent-children indented-obj)
        (call-next-method))))

  ;; Helper functions
  (-> collect-var-uses (python python-ast) (values list &optional))
  (defun collect-var-uses (obj ast)
    "Collect uses of AST in OBJ."
    ;;TODO: at some point, expand this to work inside classes.
    ;;      This may require significat modifications to acount
    ;;      for 'self' variables.
    (labels ((same-name-p (ast name)
               "Return T if AST represents an AST that contains the same
                name as NAME."
               (typecase ast
                 (python-identifier (equal (source-text ast) name))
                 ((or python-global-statement
                      python-nonlocal-statement)
                  (find-if {same-name-p _ name} (python-children ast)))))
             (find-name-in-scopes (name scopes)
               "Search SCOPES for a variable named NAME."
               (mappend
                (lambda (scope)
                  (find-if
                   (lambda (var-info)
                     (equal name (aget :name var-info)))
                   scope))
                scopes))
             (get-analysis-set (scope first-occurrence name)
               "Collect all relevant asts with NAME in SCOPE. BINDING-CLASS
                determines whether 'global' or 'nonlocal' should be used to
                determine if NAME is in-scope for assignments."
               ;; Currently, python-identifier and either
               ;; python-nonlocal-statement or python-global-statement are
               ;; relevant.
               (remove-if-not
                (lambda (ast)
                  (or (same-name-p ast name) (eq ast first-occurrence)))
                (collect-if
                 (lambda (ast)
                   (typep ast 'python-identifier))
                 scope)))
             (find-var-uses (assorted-by-scope binding-class)
               "Search assorted-by-scope for usages of variables
                with the same name. BINDING-CLASS specifies whether
                the variable is global or local and provides the
                name of the class used for binding it to a scope."
               (iter
                 (iter:with out-of-scope = nil)
                 (iter:with
                  local-var-p = (eq binding-class 'python-nonlocal-statement))
                 (for vars-in-scope in assorted-by-scope)
                 (for scope = (enclosing-scope obj (car vars-in-scope)))
                 ;; Prune any scope that occurs after the local binding
                 ;; has been squashed.
                 (when out-of-scope
                   (if (shares-path-of-p obj scope out-of-scope)
                       (next-iteration)
                       (setf out-of-scope nil)))
                 (cond
                   ((find-if
                     (lambda (ast)
                       (find-if-in-parents {typep _ 'python-parameters} obj ast))
                     vars-in-scope)
                    ;; All nested scopes are out-of-scope.
                    (and local-var-p (setf out-of-scope scope)))
                   ((find-if
                     (lambda (var)
                       (find-if-in-parents {typep _ binding-class} obj var))
                     vars-in-scope)
                    (collect vars-in-scope))
                   ((find-if
                     (lambda (var)
                       (find-if-in-parents
                        (lambda (parent)
                          (and (typep parent 'python-assignment)
                               (assign-to-var-p parent var)))
                        obj var))
                     vars-in-scope)
                    ;; All nested scopes are out-of-scope.
                    (and local-var-p (setf out-of-scope scope)))))))
      (let* ((name (and (typep ast 'python-identifier) (source-text ast)))
             (var-info (find-name-in-scopes name (scopes obj ast)))
             (scope (or (aget :scope var-info) (enclosing-scope obj ast)))
             ;; The path will be nil when given the global scope.
             (binding-class (if (ast-path obj scope)
                                'python-nonlocal-statement
                                'python-global-statement))
             (assorted-by-scope
               ;; Make sure the top-most scope comes first.
               (sort
                (assort
                 (get-analysis-set scope (or (aget :decl var-info)
                                             (get-parent-full-stmt obj ast))
                                   name)
                 :key {enclosing-scope obj})
                (lambda (ast1 ast2)
                  (< (length (ast-path obj (enclosing-scope obj ast1)))
                     (length (ast-path obj (enclosing-scope obj ast2)))))
                :key #'car)))
        ;; Don't pass in the first scope of assorted-by-scope as the first
        ;; one may include a parameter which find-var-uses would misinterpret
        ;; as squashing the binding's scope.
        (flatten (cons (car assorted-by-scope)
                       (find-var-uses (cdr assorted-by-scope)
                                      binding-class))))))

  (-> collect-fun-uses (python python-ast) list)
  (defun collect-fun-uses (obj ast)
    (labels ((same-name-p (ast name)
               "Return T if AST represents an AST that contains the same
                name as NAME."
               (equal (source-text ast) name))
             (get-analysis-set (scope name)
               "Collect all relevant asts with NAME in SCOPE."
               ;; Currently, py-name, py-arg, and either py-nonlocal or py-global
               ;; are relevant.
               (mapcar
                ;; Map the ASTs into parents that are easier
                ;; to work with.
                (lambda (ast)
                  (cond-let result
                    ((find-if-in-parents {typep _ 'python-parameters} obj ast)
                     result)
                    ((find-if-in-parents {typep _ 'python-function-definition}
                                         obj ast)
                     (if (eq (python-name result) ast)
                         result
                         ast))
                    (t ast)))
                (remove-if-not
                 (lambda (ast)
                   (same-name-p ast name))
                 (collect-if
                  (lambda (ast)
                    (member
                     (type-of ast)
                     `(python-identifier python-function-definition)))
                  scope))))
             (get-shadowed-asts (analysis-set shadowing-ast)
               "Get the ASTs in ANALYSIS-SET that are shadowed by SHADOWING-AST."
               (intersection
                analysis-set
                (remove-if
                 (lambda (ast)
                   (path-later-p obj shadowing-ast ast))
                 (get-asts-in-namespace
                  obj (enclosing-scope obj shadowing-ast)))))
             (shadowing-ast-p (ast)
               "Return T if AST is an AST that shadows the function."
               (etypecase ast
                 ((or python-parameters python-function-definition) t)
                 (python-identifier
                  ;; TODO: at some point,for loops and other binding forms
                  ;;       can also shadow, but support for identifying this
                  ;;       still needs to be done.
                  (find-if-in-parents
                   (lambda (parent)
                     (assign-to-var-p parent ast))
                   obj ast))))
             (remove-shadowed-asts (analysis-set)
               "Remove all ASTs that are shadowing the target function
                from the analysis set."
               ;; The initial definition is seen as a shadowing ast,
               ;; so remove it from consideration and add it back
               ;; after analysis.
               (cons
                (car analysis-set)
                (iter
                  (iter:with shadowed-asts)
                  (for ast in (cdr analysis-set))
                  (when (shadowing-ast-p ast)
                    (setf shadowed-asts
                          (append shadowed-asts
                                  (get-shadowed-asts analysis-set ast))))
                  (unless (member ast shadowed-asts)
                    (collect ast))))))
      (when (typep ast 'python-function-definition)
        (remove-shadowed-asts
         (get-analysis-set
          (enclosing-scope obj ast)
          (source-text (python-name ast)))))))

  (defgeneric get-vars (obj ast)
    (:documentation "Get the variables that are bound by AST.")
    (:method ((obj python) ast) nil))

  (defun create-var-alist (obj definition name &key attributes scope)
    "Create an alist with information about a variable."
    `((:name . ,name)
      (:definition . ,definition)
      (:scope . ,(or scope (enclosing-scope obj definition)))
      ,@(when attributes
          (list (cons :attributes attributes)))))

  (defun get-vars-name-handler (obj ast &key scope)
    "Handle AST as a py-name object."
    (create-var-alist
     obj ast (source-text ast)
     :scope scope
     :attributes '(:variable)))

  (defun get-vars-name-or-tuple-handler (obj ast &key scope)
    "Handle AST as a py-name or a py-tuple object."
    (typecase ast
      ((or python-tuple python-pattern-list)
       (mapcar
        (lambda (element)
          (create-var-alist
           obj element (source-text element)
           :scope scope
           :attributes '(:variable)))
        (python-children ast)))
      ((or python-identifier python-dotted-name)
       (list
        (get-vars-name-handler obj ast :scope scope)))))

  (defmethod get-vars ((obj python) (ast python-for-statement))
    (get-vars-name-or-tuple-handler obj (python-left ast)))

  ;; TODO: python-except-clause doesn't appear to have any slots.
  ;;       This may be an issue to open in tree-sitter-python.
  (defmethod get-vars ((obj python) (ast python-except-clause))
    ;; NOTE: try except appears to unbind the variable in the namespace.
    ;;       This may be because the exception has been freed by the time
    ;;       it is out of the except handler.
    ;;       This may require a special attribute for handling it.
    (let ((name-ast (cadr (python-children ast))))
      (when (typep name-ast 'python-identifier)
        (list
         (create-var-alist
          obj ast (source-text name-ast)
          :attributes '(:variable))))))

  (defun get-vars-comprehension-handler (obj ast)
    ;; NOTE: this is tricky since there are essentially two ASTs
    ;;       that the variable binding is available in. The chances
    ;;       of this becoming an issue are probably slim.
    ;;
    ;;       x = [1, 2, 3, 4, 5]
    ;;       x = [x for x in x]
    (mappend
     (lambda (for-in-clause)
       (mapcar
        (lambda (name-or-tuple)
          (get-vars-name-or-tuple-handler
           obj name-or-tuple :scope ast))
        (python-left for-in-clause)))
     (remove-if-not {typep _ 'python-for-in-clause}
                    (python-children ast))))

  (defmethod get-vars ((obj python) (ast python-list-comprehension))
    (get-vars-comprehension-handler obj ast))

  (defmethod get-vars ((obj python) (ast python-set-comprehension))
    (get-vars-comprehension-handler obj ast))

  (defmethod get-vars ((obj python) (ast python-dictionary-comprehension))
    (get-vars-comprehension-handler obj ast))

  (defmethod get-vars ((obj python) (ast python-generator-expression))
    (get-vars-comprehension-handler obj ast))

  (defun get-vars-import-handler (obj ast)
    "Handle AST as a python-import-statement or python-import-from-statement."
    (mapcar
     (lambda (name)
       (create-var-alist
        obj ast
        (typecase name
          ;; TODO: at some point,figure out how we want to handle dotted names.
          (python-dotted-name
           (source-text name))
          (python-aliased-import
           (source-text (python-alias name))))
        :attributes '(:import)))
     (python-name ast)))

  (defmethod get-vars ((obj python) (ast python-import-statement))
    (get-vars-import-handler obj ast))

  (defmethod get-vars ((obj python) (ast python-import-from-statement))
    (get-vars-import-handler obj ast))

  (defmethod get-vars ((obj python) (ast python-with-statement))
    (remove
     nil
     (mappend
      (lambda (clause)
        (mapcar
         (lambda (item)
           (when-let ((var (python-alias item)))
             (get-vars-name-handler obj var)))
         (python-children clause)))
      (python-children ast))))

  (defmethod get-vars ((obj python) (ast python-assignment)
                       &aux (lhs (python-left ast)))
    (typecase lhs
      (python-pattern-list
       (mapcar
        (lambda (item)
          (create-var-alist
           obj ast (source-text item)
           :attributes '(:variable)))
        (python-children lhs)))
      (python-identifier
       (list
        (create-var-alist obj ast (source-text lhs) :attributes '(:variable))))))

  (defmethod get-vars ((obj python) (ast python-function-definition))
    (append
     (list (create-var-alist obj ast (source-text (python-name ast))
                             :attributes '(:function)))
     (when-let ((parameters (python-parameters ast)))
       (mapcar
        (lambda (parameter)
          (create-var-alist
           obj parameter
           (source-text
            ;; Avoid printing the type as part of the name.
            (or (find-if (of-type 'identifier-ast)
                         (children parameter))
                parameter))
           :scope ast
           :attributes '(:variable)))
        (python-children parameters)))))

  (defmethod get-vars ((obj python) (ast python-lambda))
    (mapcar
     (lambda (parameter)
       (create-var-alist
        obj parameter (source-text parameter)
        :scope ast
        :attributes '(:variable)))
     (when-let ((parameters (python-parameters ast)))
       (python-children parameters))))

  (defmethod get-vars ((obj python) (ast python-class-definition))
    (unless (in-class-def-p obj ast)
      (list
       (create-var-alist
        obj ast (source-text (python-name ast))
        :attributes '(:class)))))

  (-> in-class-def-p (python python-ast)
    (values (or null python-class-definition) &optional))
  (defun in-class-def-p (obj ast)
    "Return the class definition if AST is inside one."
    (find-if-in-parents {typep _ 'python-class-definition} obj ast))

  (-> identical-name-p (python-ast python-ast) boolean)
  (defun identical-name-p (name1 name2)
    "Return T if the IDs of NAME1 and NAME2 are the same."
    (and (typep name1 'python-identifier)
         (typep name2 'python-identifier)
         (equal (source-text name1) (source-text name2))))

  (-> get-asts-in-namespace (python python-ast) list)
  (defun get-asts-in-namespace (obj ast)
    "Get all of the ASTs in AST which are considered to be
in the same namespace."
    ;; Note that with the first call to this function, AST should be the start
    ;; of a namespace.
    (labels ((new-namespace-p (ast)
               "Return T if AST starts a new namespace."
               ;; TODO: probably need to add some more types here.
               (typep
                ast '(or python-function-definition python-class-definition)))
             (collect-asts (namespace)
               "Collect the asts in NAMESPACE."
               (let ((children (remove nil (children namespace))))
                 (append children
                         (mappend (lambda (child)
                                    (unless (new-namespace-p child)
                                      (collect-asts child)))
                                  children)))))
      (cons ast (sort (collect-asts ast)
                      (lambda (ast1 ast2)
                        (path-later-p obj ast2 ast1))))))

  (-> create-tuple (list) (values python-ast &optional))
  (defun create-tuple (values)
    "Create a new tuple AST that contains values."
    (convert 'python-ast
             `((:class . ,(if values :tuple- :empty-tuple))
               (:children . ,values))))

  (-> create-dictionary (list list) (values (or python-ast null) &optional))
  (defun create-dictionary (keys values &aux (length (length keys)))
    "Create a new dictionary AST that maps KEYS to VALUES.
Returns nil if the length of KEYS is not the same as VALUES'."
    (when (= length (length values))
      (convert 'python-ast
               `((:class . ,(if values :dictionary- :empty-dictionary))
                 (:children
                  ,@(mapcar
                     (lambda (key value)
                       (convert
                        'python-ast
                        `((:class . :pair)
                          (:key . ,key)
                          (:value . ,value))))
                     keys values))))))

  ;; Implement the generic format-genome method for python objects.
  (defmethod format-genome ((obj python) &key)
    "Format the genome of OBJ using YAPF (Yet Another Python Formatter)."
    (yapf obj)))


;;;; Javascript
(when-class-defined (javascript)

  ;; Methods common to all software objects
  (defmethod phenome ((obj javascript) &key (bin (temp-file-name)))
    (interpreted-phenome obj bin))

  (defmethod function-parameters ((ast javascript-function-declaration))
    (javascript-children (javascript-parameters ast)))

  (defmethod function-body ((ast javascript-function-declaration)) (javascript-body ast))

  (defmethod enclosing-scope ((obj javascript) (ast javascript-ast))
    "Return the enclosing scope of AST in OBJ.
OBJ javascript software object
AST ast to return the enclosing scope for"
    (or (find-if (lambda (ast)
                   (typep ast
                          '(or
                            javascript-statement-block
                            javascript-function-declaration
                            javascript-program
                            javascript-arrow-function
                            javascript-for-statement
                            javascript-for-in-statement)))
                 (get-parent-asts* obj ast))
        (genome obj)))

  (defmethod inner-declarations ((ast javascript-arrow-function))
    (if-let ((parameter (javascript-parameter ast)))
      (identifiers parameter)
      (mappend #'identifiers (javascript-children (javascript-parameters ast)))))

  (defmethod inner-declarations ((ast javascript-for-in-statement))
    (identifiers (javascript-left ast)))

  (defmethod inner-declarations ((ast javascript-for-statement))
    (identifiers (javascript-initializer ast)))

  (defmethod outer-declarations ((ast javascript-object-pattern))
    (mappend #'identifiers (javascript-children ast)))

  (defmethod outer-declarations ((ast javascript-variable-declaration))
    (mappend #'outer-declarations (javascript-children ast)))

  (defmethod outer-declarations ((ast javascript-array-pattern))
    (mappend #'identifiers (javascript-children ast)))

  (defmethod outer-declarations ((ast javascript-rest-pattern))
    (identifiers ast))

  (defmethod outer-declarations ((ast javascript-variable-declarator))
    (identifiers (javascript-name ast)))

  (defmethod get-unbound-vals ((obj javascript) (ast javascript-ast))
    "Return all variables used (but not defined) within AST.
* OBJ javascript software object containing AST
* AST ast to retrieve unbound variables within"
    (labels ((get-unbound-vals-helper (obj parent ast)
               (remove-duplicates
                (apply #'append
                       (when (and (typep ast 'javascript-identifier)
                                  (not
                                   (typep parent
                                          '(or
                                            javascript-call-expression
                                            javascript-member-expression
                                            javascript-function-declaration
                                            javascript-arrow-function
                                            javascript-class-declaration
                                            javascript-meta-property
                                            javascript-break-statement
                                            javascript-class-declaration
                                            javascript-continue-statement
                                            javascript-labeled-statement
                                            javascript-import-specifier
                                            javascript-export-statement
                                            javascript-variable-declarator))))
                         (list (cons :name (source-text ast))))
                       (mapcar {get-unbound-vals-helper obj ast}
                               (children ast)))
                :test #'equal)))
      (get-unbound-vals-helper obj (get-parent-ast obj ast) ast)))

  (defmethod get-unbound-funs ((obj javascript) (ast javascript-ast)
                               &aux (children (remove nil (children ast)))
                                 (callee (first children)))
    "Return all functions used (but not defined) within AST.
* OBJ javascript software object containing AST
* AST ast to retrieve unbound functions within"
    (remove-duplicates
     (apply #'append
            (when (typep ast 'javascript-call-expression)
              (cond ((typep callee 'javascript-identifier)
                     ;; Free function call
                     (list (list (source-text callee)
                                 nil nil (length (cdr children)))))
                    ((typep callee 'javascript-member-expression)
                     ;; Member function call
                     (list (list (nest (source-text)
                                       (second)
                                       (children callee))
                                 nil nil (length (cdr children)))))
                    (t nil)))
            (mapcar {get-unbound-funs obj} children))
     :test #'equal))

  (defmethod get-parent-full-stmt ((obj javascript) (ast javascript-ast))
    (find-if #'is-stmt-p (get-parent-asts obj ast)))

  (defmethod get-function-from-function-call
      ((obj javascript) (callexpr javascript-ast))
    ;; NOTE: this currently only handles
    ;;       named functions declared with 'function'.
    (match callexpr
      ;; TODO: when needed, add support for member expression
      ;;       function calls.
      ((javascript-call-expression
        :javascript-function
        (javascript-identifier
         :computed-text (list name)))
       (enclosing-find-function obj callexpr name))))

  (defmethod function-name ((node javascript-function-declaration))
    (match node
      ((javascript-function-declaration :javascript-name name)
       (source-text name))
      ((javascript-function :javascript-name name)
       (source-text name))))

  (defmethod end-of-parameter-list
      ((software javascript) (function-node function-ast))
    (ematch function-node
      ((or (javascript-function-declaration :javascript-parameters params)
           (javascript-arrow-function
            :javascript-parameters (and params (type node))))
       (ast-end software params))
      ((javascript-arrow-function :javascript-parameter (and param (type node)))
       (ast-end software param))))

  (defmethod lhs ((decl javascript-variable-declarator)) (javascript-name decl))

  (defmethod rhs ((decl javascript-variable-declarator)) (javascript-value decl))

  ;; Helper Functions.
  (-> enclosing-find-function (javascript javascript-ast string)
    (values (or null javascript-ast) &optional))
  (defun enclosing-find-function (obj start-ast function-name)
    "Find the function with the name FUNCTION-NAME in OBJ that is in
scope of START-AST."
    ;; NOTE: this currently only handles
    ;;       named functions declared with 'function'.
    (flet ((target-function (ast)
             (match ast
               ((javascript-function-declaration
                 :javascript-name
                 (javascript-identifier
                  :computed-text (list name)))
                (equal name function-name)))))
      (find-if-in-scope #'target-function obj start-ast)))

  ;; Implement the generic format-genome method for Javascript objects.
  (defmethod format-genome ((obj javascript) &key)
    (prettier obj)))

;;; C/C++

(nest
 (when-class-defined (c))
 (when-class-defined (cpp)

   (defmethod function-name ((ast c/cpp-function-definition))
     (source-text (c/cpp-declarator (c/cpp-declarator ast))))

   (defmethod function-parameters ((ast c/cpp-function-definition))
     (children (c/cpp-parameters (c/cpp-declarator ast))))

   (defmethod call-arguments ((node c/cpp-call-expression))
     (children (c/cpp-arguments node)))

  (defmethod function-body ((ast c-function-definition)) (c-body ast))

   (defmethod no-fallthrough ((ast c/cpp-continue-statement)) t)
   (defmethod no-fallthrough ((ast c/cpp-break-statement)) t)

   (defmethod inner-declarations ((ast c/cpp-function-declarator))
     (remove-if-not {typep _ 'c/cpp-parameter-declaration}
                    (convert 'list (c/cpp-parameters ast))))

   (defmethod outer-declarations ((ast c/cpp-declaration))
     ;; Special handling for uninitialized variables.
     (iter (for d in (c/cpp-declarator ast))
           (collect (if (typep d 'c/cpp-identifier)
                        d
                        (c/cpp-declarator d)))))

  (defmethod enclosing-definition ((sw c/cpp) (ast t))
    (find-enclosing '(or definition-ast cpp-class-specifier
                      c/cpp-primitive-type)
                    sw ast))

  (defmethod definition-name ((ast c/cpp-function-definition))
    (declarator-name (c/cpp-declarator ast)))
  (defmethod definition-name ((ast c/cpp-struct-specifier))
    (source-text (c/cpp-name ast)))
  (defmethod definition-name ((ast c/cpp-union-specifier))
    (source-text (c/cpp-name ast)))
  (defmethod definition-name ((ast c/cpp-type-definition))
    (declarator-name (c/cpp-declarator ast)))
  (defmethod definition-name ((ast c/cpp-preproc-def))
    (source-text (c/cpp-name ast)))
  (defmethod definition-name ((ast c/cpp-preproc-function-def))
    (source-text (c/cpp-name ast)))

  (defmethod declarator-name ((ast c/cpp-identifier))
    (source-text ast))
  (defmethod declarator-name ((ast c/cpp-type-identifier))
    (source-text ast))
  (defmethod declarator-name ((ast c/cpp-parenthesized-declarator))
    (source-text (car (children ast))))
  (defmethod declarator-name ((ast c/cpp-pointer-declarator))
    (declarator-name (c/cpp-declarator ast)))
  (defmethod declarator-name ((ast c/cpp-array-declarator))
    (declarator-name (c/cpp-declarator ast)))
  (defmethod declarator-name ((ast c/cpp-function-declarator))
    (declarator-name (c/cpp-declarator ast)))
  ))


;;;; C
(when-class-defined (c)

  (defmethod initialize-instance :after ((c c)
                                         &key &allow-other-keys)
             "If no compiler was specified, default to cc."
             (unless (compiler c)
               (setf (compiler c) "cc")))

  (defmethod ext :around ((obj c)) (or (call-next-method) "c"))

  (defmethod transform-parse-tree
      ((language (eql ':c)) (class (eql 'c-sized-type-specifier)) parse-tree)
    "Transform PARSE-TREE such that all modifiers are stored in the :modifiers
field."
    (append
     (butlast parse-tree)
     (list
      (mapcar
       (lambda (child-tree &aux (node-type (car child-tree)))
         (cond
           ((consp node-type) child-tree)
           ((member node-type '(:error :comment)) child-tree)
           (t (cons (list :modifiers node-type) (cdr child-tree)))))
       (lastcar parse-tree)))))

  (defgeneric pointers (c-declarator)
    (:documentation "Return the number of pointers around C-DECLARATOR.")
    (:method ((ast c-parameter-declaration)) (pointers (c-declarator ast)))
    (:method ((ast c-pointer-declarator)) (1+ (pointers (c-declarator ast))))
    (:method ((ast c-identifier)) 0))

  (defmethod parameter-type ((ast c-parameter-declaration))
    "Return format is (BASE-TYPE POINTER-DEPTH . QUALIFIERS)."
    (list* (source-text (c-type ast))
           (pointers ast)
           ;; This assumes that ordering doesn't matter for
           ;; _declaration_specifiers.
           (mapcar #'source-text (slot-value ast 'children))))

  (defmethod parameter-name ((ast c-parameter-declaration)) (parameter-name (c-declarator ast)))
  (defmethod parameter-name ((ast c-pointer-declarator)) (parameter-name (c-declarator ast)))
  (defmethod parameter-name ((ast c-identifier)) (source-text ast))

  (defmethod variable-name ((ast c-identifier)) (source-text ast))

  (defmethod no-fallthrough ((ast c-continue-statement)) t)
  (defmethod no-fallthrough ((ast c-break-statement)) t)

  (defmethod inner-declarations ((ast c-for-statement))
    (c-left (c-initializer ast)))

  (defmethod type-in ((c c) (ast c-ast))
    (when-let ((decl (find-if «or {typep _ 'c-declaration}
                               {typep _ 'c-parameter-declaration}»
                               (get-parent-asts c ast))))
      (if (typep (c-declarator decl) 'c-pointer-declarator)
          :pointer
          (make-keyword (string-upcase (source-text (c-type decl)))))))

  (defmethod enclosing-definition ((sw c) (ast t))
    (find-enclosing '(or definition-ast c-primitive-type)
                    sw ast))

  (defmethod definition-name ((ast c-function-definition))
    (declarator-name (c-declarator ast)))
  (defmethod definition-name ((ast c-struct-specifier))
    (source-text (c-name ast)))
  (defmethod definition-name ((ast c-union-specifier))
    (source-text (c-name ast)))
  (defmethod definition-name ((ast c-type-definition))
    (declarator-name (c-declarator ast)))
  (defmethod definition-name ((ast c-preproc-def))
    (source-text (c-name ast)))
  (defmethod definition-name ((ast c-preproc-function-def))
    (source-text (c-name ast)))

  (defmethod declarator-name ((ast c-identifier))
    (source-text ast))
  (defmethod declarator-name ((ast c-type-identifier))
    (source-text ast))
  (defmethod declarator-name ((ast c-parenthesized-declarator))
    (source-text (car (children ast))))
  (defmethod declarator-name ((ast c-pointer-declarator))
    (declarator-name (c-declarator ast)))
  (defmethod declarator-name ((ast c-array-declarator))
    (declarator-name (c-declarator ast)))
  (defmethod declarator-name ((ast c-function-declarator))
    (declarator-name (c-declarator ast)))

  ;; TODO: Convert other methods implemented for JavaScript but not C.

  ;; Implement the generic format-genome method for C objects.
  (defmethod format-genome ((obj c) &key)
    (clang-format obj))

  (defmethod get-function-from-function-call
      ((obj c) (callexpr c-ast))
      "Given a c software object and a call-expression, return the
 function definition."
    (match callexpr
      ((c-call-expression
        :c-function
        (c-identifier
         :interleaved-text (list name)))
       (enclosing-find-c-function obj callexpr name))))

  (defun c-functions (c-soft)
    "Returns the list of c functions in the C software object.
 Each returned function is a cons of the form (<function-name> . <ast>)
 where <function-name> is a string, and <ast> is a c-function-definition."
    (let ((funcs '()))
      (mapc (lambda (x)
              (if (typep x 'c-function-definition)
                  (push (cons (function-name x) x) funcs)))
            c-soft)
      funcs))

  (defun enclosing-find-c-function (obj start-ast function-name)
    "Find the C function with the name FUNCTION-NAME in OBJ."
    (declare (ignore start-ast))
    (cdr (find function-name (c-functions obj) :test 'equal :key 'car))))

;;;; CPP
(when-class-defined (cpp)

  (defmethod initialize-instance :after ((cpp cpp)
                                         &key &allow-other-keys)
    "If no compiler was specified, default to cc."
    (unless (compiler cpp)
      (setf (compiler cpp) "c++")))

  (defmethod ext :around ((obj cpp)) (or (call-next-method) "cpp"))

  (defmethod function-body ((ast cpp-function-definition)) (cpp-body ast))

  (defmethod cpp-declarator ((ast cpp-reference-declarator))
    (if (single (children ast))
        (cpp-declarator (first (children ast)))
        (call-next-method)))

  (defmethod c/cpp-declarator ((ast cpp-reference-declarator))
    (cpp-declarator ast))

  (defmethod definition-name ((ast cpp-class-specifier))
    (source-text (cpp-name ast)))
  )


;;;; Interleaved text
(defmethod initialize-instance :after ((ast interleaved-text)
                                       &key &allow-other-keys)
  "Wrapper around AST creation to populate the interleaved text field
with empty strings between each child if the field is not populated."
  (setf (slot-value ast 'interleaved-text)
        (or (interleaved-text ast)
            (nest (repeat-sequence '(""))
                  (1+)(length)
                  (remove nil)
                  (children ast)))))

(defmethod ast-hash ast-combine-hash-values ((ast interleaved-text))
  (ast-hash (interleaved-text ast)))

(defmethod equal? :around ((ast-a interleaved-text) (ast-b interleaved-text))
  (when (call-next-method)
    (let ((hash1 (slot-value ast-a 'stored-hash))
          (hash2 (slot-value ast-b 'stored-hash)))
      (if (and hash1 hash2 (not (eql hash1 hash2)))
          nil
          (and (eq (type-of ast-a) (type-of ast-b))
               (equal? (interleaved-text ast-a)
                       (interleaved-text ast-b))
               (length= (children ast-a)
                        (children ast-b))
               (every #'equal? (children ast-a) (children ast-b)))))))

(defgeneric check-interleaved-text (ast)
  (:documentation "Assert that AST's interleaved text is valid.

Interleaved text is valid when the number of interleaved strings is
one greater than the number of children.")
  (:method ((ast interleaved-text))
    (let ((children (sorted-children ast)))
      (assert (= (1+ (length children)) (length (interleaved-text ast))) (ast)
              "The ~a to be printed has ~d children and ~d element(s) of ~
          interleaved text.  The AST must have interleaved text between ~
          each child, ~d element(s) total."
              (type-of ast)
              (length children) (length (interleaved-text ast))
              (1+ (length children)))))
  (:method (ast) nil))

(defmethod source-text :before ((ast interleaved-text)
                                &key stream)
  (declare (ignorable stream))
  (check-interleaved-text ast))

(defmethod copy :around ((ast interleaved-text) &key &allow-other-keys)
  "Wrapper around copy to perform various fixups to the interleaved-text field
and child-order annotations of the copied AST in response to child AST
insertions or removals.  These fixups exist to mimic the behavior of ASTs when
child ASTs were stored in a flat list of children and not in named children
slots."
  (let* ((copy (call-next-method))
         (old-children (remove nil (children ast)))
         (new-children (remove nil (children copy)))
         (old-children-length (length old-children))
         (new-children-length (length new-children)))
    (labels ((text-changed ()
               "Return T if the interleaved-text field changed between
               the old and new ASTs."
               (not (equal (interleaved-text copy) (interleaved-text ast))))
             (child-order-changed ()
               "Return T if the :child-order field is populated and has
               changed after the AST copy."
               (let ((old-child-order (ast-annotation ast :child-order))
                     (new-child-order (ast-annotation copy :child-order)))
                 (and old-child-order new-child-order
                      (not (equal old-child-order new-child-order)))))
             (ast-additions ()
               "Return a list of new AST children in COPY."
               (set-difference new-children old-children :key #'serial-number))
             (ast-removals ()
               "Return a list of AST children removed in COPY."
               (set-difference old-children new-children :key #'serial-number))
             (ast-in-difference-p (ast difference)
               "Return T if AST is a member of DIFFERENCE."
               (member (serial-number ast) difference :key #'serial-number))
             (fixup-child-order (difference)
               "Return a new child order annotation after adjusting for AST
               removals in DIFFERENCE."
               (iter (for child in (sorted-children ast))
                 (unless (ast-in-difference-p child difference)
                   (collect (position child copy)))))
             (ith-in-difference-p (i children difference)
               "Return T if the Ith AST in CHILDREN is a member of DIFFERENCE."
               (when-let ((ith-child (nth i children)))
                 (ast-in-difference-p ith-child difference)))
             (fixup-interleaved-text (difference)
               "Return new interleaved text after adjusting for AST insertions
               or removals in DIFFERENCE."
               ;; This mimics the behavior of JS and python ASTs when child
               ;; ASTs were stored in a flat list of children and not in
               ;; named child slots.  For AST deletions, the interleaved text
               ;; is concatenated into a single value; for AST insertions,
               ;; an empty string is added.  There are issues with this
               ;; approach which may be addressed in a different changeset;
               ;; this simply preserves existing behavior.
               ;;
               ;; TODO: This is C-style code; refactor to use LISP idioms.
               (let ((text (interleaved-text copy))
                     (children (if (< new-children-length old-children-length)
                                   (sorted-children ast)
                                   (sorted-children copy)))
                     (child-i 0)
                     (text-i 0))
                 (iter (while (< child-i (length children)))
                   (if (ith-in-difference-p child-i children difference)
                       (if (< new-children-length old-children-length)
                           ;; cut operation, concatenate the text in the cut
                           (collect
                               (iter (while (ith-in-difference-p child-i
                                                                 children
                                                                 difference))
                                 (collect (nth text-i text) into rslt)
                                 (incf text-i)
                                 (unless (first-iteration-p)
                                   (incf child-i))
                                 (finally
                                  (return (apply #'concatenate 'string
                                                 (remove nil rslt))))))
                           ;; insert operation, pad "" for the insertions
                           (appending
                            (iter (while (ith-in-difference-p child-i
                                                              children
                                                              difference))
                              (when (first-iteration-p)
                                (collect (nth text-i text))
                                (incf text-i))
                              (incf child-i)
                              (collect ""))))
                       (prog1
                           (collect (nth text-i text))
                         (incf child-i)
                         (incf text-i)))))))

      ;; Determine which ASTs were added or removed, if any.
      (when-let ((difference (cond ((< old-children-length new-children-length)
                                    (ast-additions))
                                   ((> old-children-length new-children-length)
                                    (ast-removals))
                                   (t nil))))
        ;; Assertions to force the client to provide more information when
        ;; we cannot clearly ascertain intent within this method.
        ;; This occurs when ASTs are both added and removed or when
        ;; inserting into an AST with an explicitly defined child order.
        (assert (or (text-changed) (not (and (ast-additions) (ast-removals))))
          (ast)
          "When creating an AST copy with both child AST additions and ~
                removals, the interleaved-text field must also be explicity ~
                set.")
        (assert (or (null (ast-annotation copy :child-order))
                    (and (child-order-changed)
                         (= new-children-length
                            (length (ast-annotation copy :child-order)))))
          (ast)
          "When creating an AST copy with an explicit child order ~
                annotation, child AST additions are not allowed without ~
                explicitly setting a matching :child-order annotation.")

        ;; Update the :child-order annotation and interleaved-text field to
        ;; reflect the AST changes.
        (setf (slot-value copy 'annotations)
              (if (or (null (ast-annotation copy :child-order))
                      (child-order-changed))
                  (ast-annotations copy)
                  (cons (cons :child-order (fixup-child-order difference))
                        (adrop '(:child-order) (ast-annotations copy)))))
        (setf (slot-value copy 'interleaved-text)
              (if (text-changed)
                  (interleaved-text copy)
                  (fixup-interleaved-text difference)))))
    copy))


;;; Overrides for parseable representations with interleaved-text.
(defmethod prepend-text-to-genome ((obj tree-sitter) (text string)
                                   &aux (root (genome obj)))
  "Prepend non-AST TEXT to OBJ's genome."
  (setf (genome obj)
        (nest (copy root :interleaved-text)
              (cons (concatenate 'string text (car (interleaved-text root)))
                    (cdr (interleaved-text root))))))

(defmethod append-text-to-genome-preamble ((obj tree-sitter)
                                           (text string)
                                          &aux (root (genome obj)))
  "Append non-AST TEXT to OBJ's genome preamble."
  (setf (genome obj)
        (nest (copy root :interleaved-text)
              (cons (concatenate 'string (car (interleaved-text root)) text)
                    (cdr (interleaved-text root))))))

(defmethod append-text-to-genome ((obj tree-sitter) (text string)
                                  &aux (root (genome obj)))
  "Prepend non-AST TEXT to OBJ's genome."
  (setf (genome obj)
        (nest (copy root :interleaved-text)
              (append (butlast (interleaved-text root)))
              (list)
              (concatenate 'string text)
              (lastcar (interleaved-text root)))))


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

#+nil
(let ((*json-identifier-name-to-lisp* #'convert-name))
  (setf rules
        (aget :rules
              (setf grammar (decode-json-from-string (file-to-string "~/Programs/tree-sitter-c/src/grammar.json"))))))

#+nil
(setf transformed-json
      (mapcar (lambda (rule)
                (cons (car rule) (transform-json-rule (cdr rule) grammar)))
              rules))

#+nil
(setf types (decode-json-from-string (file-to-string "~/Programs/tree-sitter-c/src/node-types.json")))

#+nil
(setf pruned (mapcar (op (list (car _) (prune-rule-tree (cdr _1))))  transformed-json))

#+nil
(setf collapsed (mapcar (op (list (car _) (collapse-rule-tree (cadr _1))))  pruned))

;;; TODO: this needs to be added into the code generation process and printed out
;;;       to *error-output* so that these issues are apparent.
#+nil
(mapcar (lambda (rule)
          ;; This will map rules to whether they're problematic or not.
          (list (car rule) (structured-rule-p (cadr rule))))
        collapsed)

#+nil
(setf expansions (mapcar (lambda (json)
                           (expand-choice-branches (aget (car json) pruned) (cdr json)))
                         transformed-json))

#+nil
(defmacro defthings () (generate-structured-text-methods grammar types :c (make-hash-table)))

(defun children-parser (ast pruned-rule slots &aux (child-stack-key '#.(gensym)))
  "Return the children of AST in order based on PRUNED-RULE. SLOTS specifies
which slots are expected to be used."
  ;; TODO: NOTE: this currently breaks on comments and errors in the children
  ;;             slot.

  ;; TODO: assume that the pruned-rule has converted the names for the slots.

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
  (labels ((populate-slot->stack ()
             "Create a hash table that maps a slot name to its
              corresponding stack."
             (alist-hash-table
              (mapcar
               (lambda (slot &aux (value (slot-value ast slot)))
                 (cons slot (ensure-cons value)))
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
             (when (typep (car (gethash 'children slot->stack))
                          (cons 'or (cdr rule)))
               (trim-slot-stack 'children slot->stack)))
           (handle-field (rule slot->stack &aux (slot (cadr rule)))
             (when (typep (car (gethash slot slot->stack))
                          (cons 'or (cddr rule)))
               (trim-slot-stack slot slot->stack)))
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

                        ;; TODO: add the choice onto the stack
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
           (handle-seq (rule slot->stack)
             (iter
               (for subrule in (cdr rule))
               (unless subrule (next-iteration))
               (for sub-slot->stack first (rule-handler subrule slot->stack)
                    then (rule-handler subrule sub-slot->stack))
               (always sub-slot->stack)
               (finally (return (if (eql t sub-slot->stack)
                                    slot->stack
                                    sub-slot->stack)))))
           (rule-handler (rule slot->stack)
             "Handles dispatching RULE to its relevant rule handler."
             (ecase (car rule)
               (:CHOICE (handle-choice rule slot->stack))
               (:REPEAT (handle-repeat rule slot->stack))
               (:FIELD (handle-field rule slot->stack))
               (:CHILD (handle-child rule slot->stack))
               (:SEQ (handle-seq rule slot->stack)))))
    ;; TODO: need to ensure that the returned hash table doesn't still have
    ;;       ASTs stored in it. If it does, figure out a way to add them in
    ;;       some how?
    (if-let ((result (rule-handler pruned-rule (populate-slot->stack))))
      (reverse (gethash child-stack-key result))
      (error "Unable to match~%~a~%on~%~a" pruned-rule ast))))

(defun computed-text-output-transformation (ast)
  "Gives the variable text output transformation for AST. This
representation is interleaved text though it's unlikely to
be more than one string outside of string literals."
  (iter
    (iter:with interleaved-text = (computed-text ast))
    (iter:with children = (slot-value ast 'children))
    (while (and interleaved-text children))
    (collect (pop interleaved-text) into result)
    (collect (pop children) into result)
    (finally
     (return
        (cond
          (interleaved-text (append result interleaved-text))
          (children (append result children))
          (t result))))))

(defun match-parsed-children-json (json-rule parse-tree)
  "Match a cl-tree-sitter PARSE-TREE as a JSON-RULE if possible."
  ;; NOTE: this could be expanded to match on the string too
  ;;       though the current representation of transformed json
  ;;       rules and pruned rules likely wouldn't benefit from it.
  (labels ((handle-alias (rule tree &aux (alias (car tree)))
             (cond
               ((aget :named rule)
                (and (string-equal (car alias) (aget :value rule))
                     (values (cdr tree) t)))
               ;; Named aliases are unhandled by #'match-parsed-children-json.
               (t (error "Named alias in JSON subtree"))))
           (handle-blank (tree) (values tree t))
           (handle-choice (rule tree)
             (iter
               (for branch in (aget :members rule))
               (for (values result matched?) = (rule-handler branch tree))
               (when matched?
                 (leave (values result t)))))
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
                        (string-equal (car token) (aget :value rule)))
               (values (cdr tree) t)))
           (handle-token (rule tree &aux (content (aget :content rule)))
             ;; TODO: in certain cases, an unnamed node will be produced.
             ;;       Need to figure out what these cases are and account for
             ;;       them. The following is a simple stopgap in the meantime.
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
               ("STRING" (handle-string rule tree)))))
    (rule-handler json-rule parse-tree)))

(defun match-parsed-children
    (language-prefix json-rule pruned-rule child-types parse-tree)
  "Match a cl-tree-sitter PARSE-TREE as a PRUNED-RULE in LANGUGE-PREFIX.
CHILD-TYPES is a list of lisp types that the children slot can contain."
  (labels ((remove-comments (tree)
             "Remove comments from PARSE-TREE."
             (when tree
               `(,(car tree)
                 ,(cadr tree)
                 ,(mapcar #'remove-comments
                          (remove-if (op (eq :comment (car _)))
                                     (caddr tree))))))
           (get-children ()
             "Get the children slots and their types from parse-tree."
             ;; TODO: work off the parse-tree exclusively and only use this
             ;;       for a preliminary check.
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
           (handle-child (rule parse-stack
                          &aux (child (car (car parse-stack))))
             (cond
               ((and (atom child)
                     ;; Confirm tree is the relevant thing on the stack.
                     (member (convert-to-lisp-type language-prefix child)
                             (cdr rule)
                             :test #'subtypep))
                (values (cdr parse-stack) t))
               ;; This is an edge case for rules that allow null children.
               ((member 'null (cdr rule))
                (values parse-stack t))))
           (handle-field (rule parse-stack
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
                      (cddr rule)
                      :test #'subtypep))
                (values (cdr parse-stack) t))
               ;; This is an edge case for a field that allows nil.
               ((member 'null (cddr rule))
                (values parse-stack t))))
           (handle-choice (rule json parse-stack)
             ;; NOTE: since this doesn't back track it won't work on certain
             ;;       rules. Currently, I'm not aware of any rules that
             ;;       need back tracking, so I'm ignoring this for now.
             (iter
               (for branch in (cdr rule))
               (for json-branch in (aget :members json))
               (for (values stack matched?) =
                    (rule-handler branch json-branch parse-stack))
               (when matched?
                 (return (values stack t)))))
           (handle-repeat (rule json parse-stack)
             ;; NOTE: since this doesn't back track it won't work on certain
             ;;       rules. Currently, I'm not aware of any rules that
             ;;       need back tracking, so I'm ignoring this for now.
             (mvlet ((stack
                      matched?
                      (rule-handler
                       (cadr rule) (aget :content json) parse-stack)))
               ;; Prevent infinite recursion when the parse-stack is never
               ;; used.
               (if (and matched? (not (equal parse-stack stack)))
                   (handle-repeat rule json stack)
                   (values parse-stack t))))
           (handle-seq (rule json parse-stack)
             (iter
               (for subrule in (cdr rule))
               (for json-subrule in (aget :members json))
               (for (values stack matched?)
                    first (rule-handler subrule json-subrule parse-stack)
                    then (rule-handler subrule json-subrule stack))
               (always matched?)
               (finally (return (values stack t)))))
           (rule-handler (rule json parse-stack)
             "Handles dispatching RULE to its relevant rule handler."
             (ecase (car rule)
               (:CHOICE (handle-choice rule json parse-stack))
               (:REPEAT (handle-repeat rule json parse-stack))
               (:FIELD (handle-field rule parse-stack))
               (:CHILD (handle-child rule parse-stack))
               (:SEQ (handle-seq rule json parse-stack))
               ((nil)
                (mvlet ((stack
                         matched?
                         (match-parsed-children-json json parse-stack)))
                  (when matched?
                    (values stack t)))))))
    (cond
      ;; Prevent matching on an empty rule when there are children.
      ;; TODO: should probably update this to check if the parse-tree
      ;;       matches.
      ((or (not pruned-rule)
           (= 1 (length pruned-rule)))
       (not (get-children)))
      (pruned-rule
       (mvlet ((parse-stack
                success?
                (rule-handler
                 pruned-rule json-rule (caddr (remove-comments parse-tree)))))
         ;; Avoid matching a rule if parse tree tokens still exist.
         (and (not parse-stack) success?))))))

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
           (update-indentation-slots
               (ast parents indentation text
                &aux (parent (car parents))
                  (total-indentation (+ indentation indentation-carryover))
                  (inherited-indentation
                   (get-indentation-at ast parents)))
             "Patch either AST or PARENT to have INDENTATION for the
              relevant line or lines."
             (symbol-macrolet ((indent-children-parent (indent-children parent))
                               (indent-adjustment (indent-adjustment ast))
                               (indent-children-current (indent-children ast)))
               (cond
                 ;; Avoid wasting the newline on empty text before
                 ;; reaching a child.
                 ((and (emptyp text) (ancestor-of-p root indentation-ast ast)))
                 ;; Don't indent if the current AST already has an
                 ;; indentation slot assigned as this will result in
                 ;; back-propogation of indentation.
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
                  (when (scan "[^ \\t\\n]" text)
                    (setf indentation-carryover nil
                          indentation-ast nil)))
                 ((and parent (not indent-children-parent))
                  (setf indent-children-parent (- total-indentation
                                                  inherited-indentation)
                        indentation-carryover nil
                        indentation-ast nil))
                 (t (setf indent-adjustment (- total-indentation
                                               inherited-indentation)
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
                     (or (eql #\newline (first text))
                         ;; Assume the second is a newline in this case.
                         (eql #\return (first text))))
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
                    ;;       set to 0. This prevents back-propogation
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
             ;; TODO: there will need to be a separate function for computed text
             ;;       ASTs? Test this with a string literal with escape
             ;;       sequences. It's possible that this doesn't matter.
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

(defun convert-initializer
    (spec prefix superclass string &key computed-text-parent-p
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
       (line-octets (map
                     'vector
                     #'string-to-octets
                     (serapeum:lines string :keep-eols t)))
       (computed-text-p (computed-text-node-p instance)))
  "Initialize an instance of SUPERCLASS with SPEC."
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
               (collect (cons (if previous-child (get-end previous-child) from) (get-start child))
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
               (iter:with comment-stack)
               ;; Keep track of the last non-comment AST seen. It is used for
               ;; populating 'after'comments.
               (iter:with previous-field)
               (for field in (caddr spec))
               (for slot-info = (car field))
               (for i upfrom 0)
               (when (skip-terminal-field-p field slot-info)
                 (when (and previous-field comment-stack)
                   (setf (after-comments previous-field)
                         (reverse comment-stack)))
                 ;; Reset the converted-field so that comments aren't pushed back
                 ;; to the last AST that wasn't a terminal symbol which could
                 ;; have been proceded by terminal symbols.
                 (setf previous-field nil
                       comment-stack nil)
                 (next-iteration))
               (for converted-field =
                    (convert superclass field
                             :string-pass-through string
                             :computed-text-parent-p computed-text-p))
               ;; cl-tree-sitter appears to put the
               ;; slot name first unless the list goes
               ;; into the children slot.
               (cond
                 ((and (listp slot-info) (not error-p))
                  (setf (before-comments converted-field) (reverse comment-stack)
                        comment-stack nil
                        previous-field converted-field)
                  (collect (list (car slot-info)
                                 converted-field)
                    into fields))
                 ((typep converted-field 'comment-ast)
                  ;; NOTE: this won't put the comment in the children slot
                  ;;       when it's allowed. There may need to be a function
                  ;;       that signals their ability to be stored there if
                  ;;       that functionality is ever desired.
                  (push converted-field comment-stack))
                 (t
                  (setf (slot-value converted-field 'before-comments)
                        (reverse comment-stack)
                        comment-stack nil
                        previous-field converted-field)
                  (collect converted-field into children)))
               (finally
                (when (and comment-stack previous-field)
                  (setf (after-comments previous-field) (reverse comment-stack)))
                (return
                  (if children
                      (push `(:children ,children) fields)
                      fields)))))
           (merge-same-fields (field-list)
             "Merge all fields that belong to the same slot.
              This is used for setting slots with an arity of 0."
             (mapcar
              (lambda (grouping)
                (apply #'append
                       (list (caar grouping))
                       (mapcar #'cdr grouping)))
              (assort field-list :key #'car)))
           (set-slot-values (slot-values)
             "Set the slots in instance to correspond to SLOT-VALUES."
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
                      (before-text (safe-subseq before start))
                      (after-text (safe-subseq end after)))
                 (unless (emptyp before-text)
                   (setf (before-text instance) before-text))
                 (unless (emptyp after-text)
                   (setf (after-text instance) after-text)))))
           ;; TODO: this may be useful for variable text as a reference.
           ;; TODO: maybe reformat it to use the spec instead of annotations?
           (set-computed-text (&aux (from (car (cadr spec)))
                                 (to (cadr (cadr spec))))
             "Set the computed-text slot in instance if it needs set."
             (when computed-text-p
               (setf (computed-text instance)
                     (if-let ((children (children instance)))
                       (mapcar (lambda (range)
                                 (destructuring-bind (from . to) range
                                   (safe-subseq from to)))
                               (ranges children from to))
                       ;; Else set it to everything in the range.
                       (list (safe-subseq from to))))))
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
    (when error-p
      (error "Convert failed to parse without errors"))
    (set-slot-values
     (merge-same-fields (get-converted-fields)))
    (set-surrounding-text)
    (set-computed-text)
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

(defmethod convert ((to-type (eql 'tree-sitter-ast)) (spec list)
                     &key superclass string-pass-through computed-text-parent-p
                     &allow-other-keys)
  "Create a TO-TYPE AST from the SPEC (specification) list."
  (if string-pass-through
      (convert-initializer
       spec (get-language-from-superclass superclass) superclass
       string-pass-through :computed-text-parent-p computed-text-parent-p)
      (convert-spec
       spec (get-language-from-superclass superclass) superclass)))

;;; TODO: when reading in, only consider the before and after text between an
;;;       unnamed and named node or a named and named node.

;;; TODO: we need to handle variable text. It's wholly possible that,
;;;       coming in, the fields are correctly populated by #'convert.
;;;       By first checking if the AST is a computed-text node, we can
;;;       generate a different method.
;;;       Yes, we want to populate the computed-text slot when #'convert
;;;       runs.


;;; TODO: we're going to run into an issue with figuring out how much white
;;;       space is between unnamed nodes. Some of it could be assumed, but
;;;       there are cases--IMMEDIATE_TOKENS--where white spaces can not precede
;;;       the token. This will likely require support in output-transformation.
;;;       Paul D. brought up this potential problem at some point.
;;;       Finding two unnamed nodes in a row probably isn't common across
;;;       AST types.
(defmethod convert ((to-type (eql 'tree-sitter-ast)) (string string)
                    &key superclass &allow-other-keys
                    &aux (prefix (get-language-from-superclass superclass)))
  (labels
      ((ensure-beginning-bound (parse-tree)
         "Desctructively ensures that the beginning bound of PARSE-TREE is the
          beginning of the string."
         (setf (cadr (car (cadr parse-tree))) 0)
         parse-tree)
       (transform-tree (parse-tree)
         "Map transform-parse-tree over PARSE-TREE."
         ;; NOTE: it might make sense not to use map-tree here and
         ;;       write a custom function instead.
         (map-tree {transform-parse-tree prefix nil} parse-tree
                   :traversal :postorder))
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
              ;; Text is lost in this case. This might not happen
              ;; very often.
              (collect previous-child into annotated-children at beginning)
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
                 (if (and child (not terminal-child-p))
                     (list
                      (annotate-surrounding-text
                       child :parent-from from :parent-to (get-end subtree-spec)))
                     (and child (list child)))
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
      :string-pass-through string))))

(defmethod source-text ((ast indentation)
                        &key stream parents
                          ;; These are "boxed" values since they have
                          ;; to be propagated between adjacent
                          ;; siblings (not just down the stack).
                          (indent-p (box nil))
                          (indentation-ast (box nil))
                          (trim t)
                        &aux (root ast))
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
               ((not (indentablep ast)) text)
               ((< 1 (length split-text))
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
                                &key ancestor-check)
             "If indentation to be written to stream, handle
            writing it."
             (when (and (unbox indent-p)
                        indentablep
                        ;; Prevent indentation from being
                        ;; wasted on empty strings before it
                        ;; reaches a child. This is checking if
                        ;; it should not be skipped as opposed
                        ;; to the logic in process-indentation
                        ;; which is checking if it should be
                        ;; skipped.
                        (not (and ancestor-check
                                  (emptyp text)
                                  (ancestor-of-p
                                   root (unbox indentation-ast) ast))))
               (setf (unbox indent-p) nil
                     (unbox indentation-ast) nil)
               (unless trim
                 (write-string
                  (make-indentation-string (indentation-length ast parents))
                  stream))))
           (handle-text (text ast indentablep parents
                         &key ancestor-check)
             "Handle writing TEXT to stream, updating any indentation
            variables that need updated."
             ;; Suppress indentation if TEXT begins with a newline.
             (handle-leading-newline text)
             (handle-indentation text ast indentablep parents
                                 :ancestor-check ancestor-check)
             ;; Set indentation flag  when TEXT ends with a newline.
             (handle-trailing-newline text ast indentablep)
             (unless trim
               (write-string
                (patch-inner-indentation text ast parents)
                stream)))
           (handle-ast (output &key (ast-parents (cons ast parents)))
             "Handle the source text of AST."
             (source-text output
                          :stream stream
                          :parents ast-parents
                          :indent-p indent-p
                          :indentation-ast indentation-ast
                          :trim nil)))
    (let ((indentablep (indentablep ast)))
      (handle-text (before-text ast) ast indentablep parents)
      (mapc (lambda (output &aux trim)
              (declare (special trim))
              (if (stringp output)
                  (handle-text output ast indentablep parents
                               :ancestor-check t)
                  (handle-ast output)))
            (cdr (butlast (output-transformation ast))))
      (handle-text (after-text ast) ast indentablep parents))))

(defmethod rebind-vars ((ast tree-sitter-ast)
                        var-replacements fun-replacements)
  "Replace variable and function references, returning a new AST.
* AST node to rebind variables and function references for
* VAR-REPLACEMENTS list of old-name, new-name pairs defining the rebinding
* FUN-REPLACEMENTS list of old-function-info, new-function-info pairs defining
the rebinding"
  (if (ast-type-to-rebind-p ast)
      (copy ast :computed-text (mapcar {rebind-vars _ var-replacements
                                                       fun-replacements}
                                          (computed-text ast)))
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
