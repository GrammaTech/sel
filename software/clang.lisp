;;; clang.lisp --- clang software representation
;;;
;;; DOCFIXME Need a page or so introduction to clang software objects.
;;;
;;; @texi{clang}
(defpackage :software-evolution-library/software/clang
  (:nicknames :sel/software/clang :sel/sw/clang)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :metabang-bind
        :iterate
        :split-sequence
        :cl-ppcre
        :cl-json
        :uiop/pathname
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/ast
        :software-evolution-library/software/source
        :software-evolution-library/software/parseable
        :software-evolution-library/components/formatting
        :software-evolution-library/components/searchable
        :software-evolution-library/components/fodder-database)
  (:import-from :uiop :nest)
  (:export :clang
           :clang-base
           :clang-ast
           :clang-ast-p
           :clang-ast-base
           :clang-ast-base-p
           :headers
           :macros
           :includes
           :types
           :globals
           :normalize-flags
           :pick-guarded-compound
           :clang-mutation
           :bind-free-vars
           :binding-for-var
           :binding-for-function
           :crossover-2pt-inward
           :crossover-2pt-outward
           :intraprocedural-2pt-crossover
           :function-containing-ast
           :function-body
           :function-body-p
           :function-decl-p
           :adjust-stmt-range
           :random-point-in-function
           :select-intraprocedural-pair
           :clang-mutate
           :apply-clang-mutate-ops
           :update-headers-from-snippet
           :update-headers-from-ast
           :prototypes
           :functions
           :get-entry
           :stmt-asts
           :non-stmt-asts
           :good-stmts
           :bad-stmts
           :get-parent-full-stmt
           :wrap-ast
           :wrap-child
           :+c-numeric-types+
           :+c-relational-operators+
           :+c-arithmetic-binary-operators+
           :+c-arithmetic-assignment-operators+
           :+c-bitwise-binary-operators+
           :+c-bitwise-assignment-operators+
           :+c-arithmetic-unary-operators+
           :+c-bitwise-unary-operators+
           :+c-sign-unary-operators+
           :+c-pointer-unary-operators+
           :ast-declarations
           :declared-type
           :find-var-type
           :typedef-type
           :random-function-name
           :*clang-max-json-size*
           :*clang-mutation-types*
           :*free-var-decay-rate*
           :*matching-free-var-retains-name-bias*
           :*matching-free-function-retains-name-bias*
           :*clang-json-required-fields*
           :*clang-json-required-aux*
           :*clang-ast-aux-fields*
           :*clang-mutate-additional-args*
           :delete-decl-stmts
           :ancestor-after
           :common-ancestor
           :ancestor-of
           :scopes-between
           :begins-scope
           :begins-scope*
           :nesting-depth
           :full-stmt-p
           :block-p
           :enclosing-full-stmt
           :enclosing-block
           :nesting-relation
           :match-nesting
           :block-predeccessor
           :block-successor
           :get-children-using
           :get-declared-variables
           :get-used-variables
           :add-include
           :add-type
           :add-type*
           ;; :find-type
           :find-or-add-type
           :type-decl-string
           :type-trace-string
           :type-trace-string*
           :type-from-trace-string
           :type-from-trace-string*
           :add-macro
           :nullify-asts
           :keep-partial-asts
           :retry-mutation
           :clang-cut
           :clang-cut-same
           :clang-cut-full
           :clang-cut-full-same
           :clang-insert
           :clang-insert-same
           :clang-insert-full
           :clang-insert-full-same
           :clang-swap
           :clang-swap-same
           :clang-swap-full
           :clang-swap-full-same
           :clang-move
           :clang-replace
           :clang-replace-same
           :clang-replace-full
           :clang-replace-full-same
           :clang-promote-guarded
           :clang-nop
           :pick-for-loop
           :explode-for-loop
           :pick-while-loop
           :coalesce-while-loop
           :cut-decl
           :pick-cut-decl
           :swap-decls
           :pick-swap-decls
           :rename-variable
           :pick-rename-variable
           :expand-arithmatic-op
           :full-stmt-filter
           :same-class-filter
           :clang-ast
           :clang-ast-node
           :ast-args
           :ast-args-equal
           :ast-declares
           :ast-expr-type
           ;; AST structures.
           :ast-full-stmt
           :ast-guard-stmt
           :ast-in-macro-expansion
           :ast-includes
           :ast-includes-in-obj
           :ast-is-decl
           :ast-macros
           :ast-name
           :ast-opcode
           :ast-ret
           :ast-syn-ctx
           :ast-types
           :ast-unbound-funs
           :ast-unbound-vals
           :ast-varargs
           :ast-void-ret
           :make-clang-ast
           :copy-clang-ast
           :make-clang-ast-node
           :copy-clang-ast-node
           :clang-type
           :type-array
           :type-decl
           :type-hash
           :type-i-file
           :type-pointer
           :type-const
           :type-volatile
           :type-restrict
           :type-storage-class
           :type-reqs
           :type-name
           :make-clang-type
           :copy-clang-type
           :clang-macro
           :macro-name
           :macro-body
           :macro-hash
           :make-clang-macro
           :copy-clang-macro
           ;; FIXME: Clang literal building.
           :make-statement
           :make-statement*
           :make-literal
           :make-operator
           :make-block
           :make-parens
           :make-while-stmt
           :make-for-stmt
           :make-if-stmt
           :make-var-reference
           :make-var-decl
           :make-cast-expr
           :make-call-expr
           :make-array-subscript-expr
           :make-label
           :make-switch-stmt
           :make-break-stmt
           :fix-semicolons
           :clang-fixup-mutation
           :parse-source-snippet-clang-common
           :ast-declarations
           :ast-declarations*
           :ast-var-declarations
           :ast-var-declarations*
           :append-string-to-node
           :add-semicolon
           :add-semi-before
           :add-semi-after))
(in-package :software-evolution-library/software/clang)
(in-readtable :curry-compose-reader-macros)

(define-software clang-base (parseable)
  ((includes :initarg :includes :accessor includes
             :initform nil :copier :direct
             :type #+sbcl '(list string *) #-sbcl list
             :documentation "Names of headers included.")
   (types :initarg :types :accessor types
          :initform (make-hash-table)
          :copier copy-hash-table
          :type #+sbcl hash-table #-sbcl hash-table
          :documentation "Hash table of types keyed by HASH id.")
   (macros :initarg :macros :accessor macros
           :initform nil :copier :direct
           :type list
           :documentation "List of macros.")
   (stmt-asts :initarg :stmt-asts :reader stmt-asts
              :initform nil :copier :direct
              :type #+sbcl '(list (cons keyword *) *) #-sbcl list
              :documentation
              "List of statement ASTs which exist within a function body.")
   ;; TODO: We should split non-statement ASTs into typedefs,
   ;;       structs/classes, and global variables, all of which should
   ;;       have different mutation types defined.  This needs more design.
   (non-stmt-asts :initarg :non-stmt-asts :reader non-stmt-asts
                  :initform nil :copier :direct
                  :type #+sbcl '(list (cons keyword *) *) #-sbcl list
                  :documentation
                  "List of global AST which live outside of any function.")
   (functions :initarg :functions :reader functions
              :initform nil :copier :direct
              :type #+sbcl '(list (cons keyword *) *) #-sbcl list
              :documentation "Complete functions with bodies.")
   (prototypes :initarg :prototypes :reader prototypes
               :initform nil :copier :direct
               :type #+sbcl '(list (cons keyword *) *) #-sbcl list
               :documentation "Function prototypes."))
  (:documentation "Base class for C/C++ objects parsed with Clang front end"))

(define-software clang (clang-base) ()
  (:documentation
   "C language (C, C++, C#, etc...) ASTs using Clang, C language frontend for LLVM.
See http://clang.llvm.org/."))


;;; clang object creation
(defun normalize-flags (dir flags)
  "Normalize the list of compiler FLAGS so all search paths are fully
expanded relative to DIR.

* DIR base directory for all relative paths
* FLAGS list of compiler flags
"
  (labels ((split-flags (flags)
             (nest (remove-if #'emptyp)
                   (mapcar #'trim-left-whitespace)
                   (mappend (lambda (flag) ; Split leading "L".
                              (split-quoted
                               (replace-all flag "-L" "-L "))))
                   (mappend (lambda (flag) ; Split leading "-I".
                              (split-quoted
                               (replace-all flag "-I" "-I ")))
                            flags))))
    (iter (for f in (split-flags flags))
          (for p previous f)
          (collect (if (and dir (or (string= p "-I") (string= p "-L")))
                       ;; Ensure include/library paths
                       ;; point to the correct location
                       ;; and not a temporary build directory.
                       (if (absolute-pathname-p f)
                           (nest (namestring)
                                 (ensure-directory-pathname)
                                 (canonical-pathname f))
                           (nest (namestring)
                                 (canonical-pathname)
                                 (merge-pathnames-as-directory
                                  (ensure-directory-pathname dir)
                                  (make-pathname :directory
                                                 (list :relative f)))))
                       ;; Pass the flag thru.
                       f)))))

(defmethod from-file ((obj clang-base) path)
  "Initialize OBJ with the contents of PATH."
  (setf path (if (absolute-pathname-p path)
                 (namestring path)
                 (truenamestring path)))
  (setf (genome obj) (file-to-string path))
  (setf (ext obj) (pathname-type (pathname path)))
  (setf (flags obj) (nest (normalize-flags (pathname-directory-pathname path))
                          (cons (format nil "-I~a"
                                        (pathname-directory-pathname path)))
                          (flags obj)))
  obj)


;;; clang individual ast data structures
(defstruct (clang-ast-base (:include ast))
  "Base structure class used to dispatch on clang-only
methods")

;;; Generalize this later with other object types
(deftype clang-name () 'string)

(define-ast (clang-ast (:conc-name ast) (:include clang-ast-base))
  "AST generated by clang-mutate."
  (args nil :type list)   ;; List of lists (<name-string> <hash>)
  (declares nil :type list)
  (expr-type nil :type (or number null))
  (full-stmt nil :type boolean)
  (guard-stmt nil :type boolean)
  (in-macro-expansion nil :type boolean)
  (includes nil :type list)
  (is-decl nil :type boolean)
  (macros nil :type list)
  (name nil :type (or clang-name null))  ;; or a name object
  (opcode nil :type (or string null))
  (ret nil :type (or number null))
  (syn-ctx nil :type (or symbol null))
  (types nil :type list)
  (unbound-funs nil :type list)
  (unbound-vals nil :type list)
  (varargs nil)
  (void-ret nil :type boolean)) ;; this is set by from-alist

(define-immutable-node-struct (clang-type (:conc-name type-impl))
  "TypeDB entry generated by clang-mutate."
  (array "" :type string)
  (decl "" :type (or string null))
  (hash nil :type (or number null))
  (i-file nil :type (or string null))
  (pointer nil :type boolean)
  (const nil :type boolean)
  (volatile nil :type boolean)
  (restrict nil :type boolean)
  (storage-class nil :type (or symbol null))
  (reqs nil :type list)
  (name nil :type string))

(defmethod ast-name ((x null)) nil)

;; Convert the accessors to generic functions
;; so other implementations can coexist.

(defgeneric type-array (obj)
  (:method ((obj clang-type)) (type-impl-array obj)))
(defgeneric type-decl (obj)
  (:method ((obj clang-type)) (type-impl-decl obj)))
(defgeneric type-hash (obj)
  (:method ((obj clang-type)) (type-impl-hash obj)))
(defgeneric type-i-file (obj)
  (:method ((obj clang-type)) (type-impl-i-file obj)))
(defgeneric type-pointer (obj)
  (:method ((obj clang-type)) (type-impl-pointer obj)))
(defgeneric type-const (obj)
  (:method ((obj clang-type)) (type-impl-const obj)))
(defgeneric type-volatile (obj)
  (:method ((obj clang-type)) (type-impl-volatile obj)))
(defgeneric type-restrict (obj)
  (:method ((obj clang-type)) (type-impl-restrict obj)))
(defgeneric type-storage-class (obj)
  (:method ((obj clang-type)) (type-impl-storage-class obj)))
(defgeneric type-reqs (obj)
  (:method ((obj clang-type)) (type-impl-reqs obj)))
(defgeneric type-name (obj)
  (:method ((obj clang-type)) (type-impl-name obj)))

(define-immutable-node-struct (clang-macro (:conc-name macro))
  "MacroDB entry generated by clang-mutate."
  (hash nil :type (or number null))
  (name nil :type (or string null))
  (body nil :type (or string null)))


(defmethod ast-includes ((c conflict-ast)) nil)
(defmethod ast-declares ((c string)) nil)
(defmethod ast-declares ((c null)) nil)

(defgeneric ast-includes-in-obj (obj ast)
  (:documentation
   "Like AST-INCLUDES, but is also passed the SW object in
which the ast sits, if that is useful for computing the includes.")
  (:method (obj ast)
    ;; Default back to AST-INCLUDES
    (declare (ignore obj))
    (ast-includes ast)))

(defmethod print-object ((obj clang-ast-node) stream)
  "Print a representation of the clang-ast-node OBJ to STREAM.
* OBJ clang-ast to print
* STREAM stream to print OBJ to
"
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t)
        (format stream "~a~@[ ~a~]" (ast-class obj) (ast-name obj)))))

(defmethod ast-class-meld-p ((ast-class (eql :CompoundStmt)) ast)
  (declare (ignorable ast-class ast))
  t)

(defun make-statement* (class syn-ctx children
                        &key expr-type full-stmt guard-stmt opcode
                          types unbound-funs unbound-vals declares includes
                          aux-data)
  "Create a statement AST.

TYPES, UNBOUND-FUNS, and UNBOUND-VALS will be computed from children
if not given.

* CLASS class name of the AST node
* SYN-CTX surrounding syntactic context of the AST node
* CHILDREN children of the AST node
* EXPR-TYPE type hash of the expression represented by the AST node
* FULL-STMT boolean indicating if the AST represents a complete statement
* GUARD-STMT  boolean indicating if the AST is a control-flow predicate
* OPCODE name of the operation for Unary/BinaryOp AST nodes
* TYPES list of type hashes for types used in the AST node
* UNBOUND-FUNS list of free function in the AST node
* UNBOUND-VALS list of free variables in the AST node
* DECLARES list of identifiers declared by the AST node
* INCLUDES header files used by the AST node
"
  (labels
      ((union-child-vals (function)
         (remove-duplicates
          (apply #'append
                 (mapcar function
                         (remove-if-not #'clang-ast-base-p children)))
          :test #'equal)))
    (let ((types (or types (union-child-vals #'ast-types)))
          (unbound-funs (or unbound-funs
                            (union-child-vals #'ast-unbound-funs)))
          (unbound-vals (or unbound-vals
                            (union-child-vals #'ast-unbound-vals))))
      (make-clang-ast
        :path nil
        :node (make-clang-ast-node
                :class class
                :syn-ctx syn-ctx
                :expr-type expr-type
                :full-stmt full-stmt
                :guard-stmt guard-stmt
                :opcode opcode
                :types types
                :declares declares
                :unbound-funs unbound-funs
                :unbound-vals unbound-vals
                :includes includes
                :aux-data aux-data)
        :children children))))

(defgeneric make-literal (value &optional kind &rest rest)
  (:documentation
   "Create a literal AST of VALUE.
* Optional value KIND specified the type of literal to
  create (:integer, :unsigned, :float, :string, :quoated-string).
  Defaults based on the type of value
* VALUE value for the literal AST to have
* REST additional arguments to `make-statement'
"))

(defmethod make-literal (value &optional (kind (etypecase value
                                                 (integer :integer)
                                                 (single-float :float)
                                                 (simple-array :string)))
                         &rest rest)
  (multiple-value-bind (class text)
      (ecase kind
        (:integer (values :IntegerLiteral
                          (format nil "~a" (round value))))
        (:unsigned (values :IntegerLiteral
                           (format nil "~du" (round value))))
        (:float (values :FloatingLiteral
                        (format nil "~a" value)))
        (:string
         (values :StringLiteral
                 (format nil (if (and (eq (aref value 0) #\")
                                      (eq (aref value (1- (length value))) #\"))
                                 "~a" "~s") value))))
    (apply #'make-statement class :generic (list text) rest)))

(defun make-operator (syn-ctx opcode child-asts &rest rest &key full-stmt &allow-other-keys)
  "Create a unary or binary operator AST.
* SYN-CTX surrounding syntactic context of the AST node
* OPCODE name of the operation for Unary/BinaryOp AST nodes
* CHILD-ASTS children of the AST node
* REST additional arguments to `make-statement'
"
  (destructuring-bind (class . children)
      (ecase (length child-asts)
        (1 (cons :UnaryOperator
                 (list opcode (car child-asts))))
        (2 (cons :BinaryOperator
                 (list (first child-asts)
                       (format nil " ~a " opcode)
                       (second child-asts)))))
    (apply #'make-statement class syn-ctx (if full-stmt (append children (list ";"))
                                              children)
           :opcode opcode rest)))

(defun make-block (children &rest rest)
  "Create a compount statement AST.
* CHILDREN children of the AST node
* REST additional arguments to `make-statement'
"
  (apply #'make-statement :CompoundStmt :braced
         `(,(format nil "{~%") ,@children ,(format nil "~%}"))
         :full-stmt t
         rest))

(defun make-parens (children &rest rest)
  "Create a parenthesis expression AST.
* CHILDREN children of the AST node
* REST additional arguments to `make-statement'
"
  (apply #'make-statement :ParenExpr :generic
         `("(" ,@children ")")
         rest))

(defun make-while-stmt (syn-ctx condition body &rest rest)
  "Create a while loop AST.
* SYN-CTX surrounding syntactic context of the AST node
* CONDITION ast conditional for the while statement
* BODY ast body for the while statement
* REST additional arguments to `make-statement'
"
  (apply #'make-statement :WhileStmt syn-ctx
         `("while ("
           ,condition
           ") "
           ,body)
         :full-stmt t
         rest))

(defun make-for-stmt (syn-ctx initialization condition update body &rest rest)
  "Create a for loop AST.
* SYN-CTX surrounding syntactic context of the AST node
* INITIALIZATION ast for the initialization of the loop
* CONDITION ast for the loop conditional
* UPDATE ast for the loop update
* BODY ast for the loop body
* REST additional arguments to `make-statement'
"
  (apply #'make-statement :ForStmt syn-ctx
         (remove nil
           `("for ("
             ,initialization "; "
             ,condition "; "
             ,update ") "
             ,body))
         :full-stmt t
         rest))

(defun make-if-stmt (condition then &optional else &rest rest)
  "Create an if statement AST.
* CONDITION ast for the if statement conditional
* THEN ast for the if statement then body
* ELSE optional ast for the if statement else
* REST optional additional arguments to `make-statement'
"
  (apply #'make-statement :IfStmt :fullstmt
         (append `("if ("
                   ,condition ") "
                   ,then)
                 (unless (or (eq :CompoundStmt (ast-class then))
                             (not else))
                   '("; "))
                 (when else
                   `(" else " ,else)))
         :full-stmt t
         rest))

(defun make-var-reference (name type &rest rest
                           &aux (hash (when type (type-hash type))))
  "Create a variable reference AST.
* NAME name of the variable to reference
* TYPE type of the variable to reference
* REST optional additional arguments to `make-statement'
"
  (apply #'make-statement :ImplicitCastExpr :generic
         (list (make-statement :DeclRefExpr :generic
                               (list (unpeel-bananas name))
                               :expr-type hash
                               :unbound-vals (list name)))
         :expr-type hash
         rest))

(defun make-var-decl (name type &optional initializer &rest rest
                      &aux (decls (list name)))
  "Create a variable declaration AST.
* NAME name of the variable to declare
* TYPE type of the variable
* INITIALIZER optional AST to initialize the variable
* REST additional arguments to `make-statement'
"
  (apply #'make-statement
         :DeclStmt :fullstmt
         (list (make-statement :Var :generic
                               (if initializer
                                   (list (format nil "~a ~a = "
                                                 (type-decl-string type) name)
                                         initializer
                                         ";")
                                   (list (format nil "~a ~a;"
                                                 (type-decl-string type) name)))
                               :types (list (type-hash type))
                               :declares decls))
         :declares decls
         :full-stmt t
         rest))

(defun make-array-subscript-expr (array-expr subscript-expr &rest rest)
  "Create a array subscript expression AST.
* ARRAY-EXPR AST expression with an array type
* SUBSCRIPT-EXPR AST expression with an integer type to be used as array
  subscript
* REST additional arguments to `make-statement'
"
  (apply #'make-statement :ArraySubscriptExpr :generic
         (list array-expr "[" subscript-expr "]")
         rest))

(defun make-cast-expr (type child &rest rest)
  "Create a c-style cast expression AST.
* TYPE clang-type to cast the expression to
* CHILD ast to be cast
* REST additional arguments to `make-statement'
"
  (apply #'make-statement :CStyleCastExpr :generic
         (list (format nil "(~a)" (type-name type))
               child)
         :types (list (type-hash type))
         rest))

(defun make-call-expr (name args syn-ctx &rest rest)
  "Create a call expression AST.
* NAME Name of the function
* ARGS list of ast arguments to the function
* SYN-CTX surrounding syntactic context of the AST node
* REST additional arguments to `make-statement'
"
  (apply #'make-statement :CallExpr syn-ctx
         `(,(make-statement :ImplictCastExpr :generic
                         (list (make-statement :DeclRefExpr :generic
                                               (list (unpeel-bananas name)))))
           "("
           ,@(interleave args ", ")
           ")")
         rest))

(defun make-label (name child &rest rest)
  "Create a label AST.
* NAME name of the label
* CHILD ast to be labeled
"
  (apply #'make-statement :LabelStmt :fullstmt
         (list (format nil "~a:~%" name) child)
         rest))

(defun make-break-stmt (&rest rest)
  "Create a break statement AST.
* REST additional arguments to `make-statement'
"
  (apply #'make-statement :BreakStmt :fullstmt
         (list "break")
         :full-stmt t
         rest))

(defun make-switch-stmt (value cases &rest rest)
  "Create a switch statement AST.
* VALUE the AST of the expression to switch on
* CASES list of cases
* SYN-CTX surrounding syntactic context of the AST node
* REST additional arguments to `make-statement'

Each element of CASES has the form ((values ...) stmts ), where each
value is an integer (or T for the default case) and each stmt is a
full-statement AST. Don't forget to include BreakStmt ASTs as they
will not be generated automatically.
"
  (labels ((make-case (values stmts)
             (bind (((v . vals) values)
                    (children (list* (format nil ":~%")
                                     ;; The first statement following the
                                     ;; "case" is its child.
                                     (if vals
                                         (make-case vals (take 1 stmts))
                                         (take 1 stmts))))
                    )
               `(,(if (eq v t)
                      (apply #'make-statement :DefaultStmt :generic
                             (cons "default" children)
                             rest)
                      (apply #'make-statement :CaseStmt :fullstmt
                             (cons "case "
                                   (cons (make-literal v)
                                         children))
                             rest))
                  ;; The remaining statements in this case are siblings. This
                  ;; is weird but it matches clang's AST.
                  ,@(cdr (interleave stmts (format nil ";~%")))
                  ,(format nil ";~%")))))

    (apply #'make-statement :SwitchStmt :fullstmt
           `("switch ("
             ,value
             ,")"
             ,(apply #'make-block (mappend (lambda-bind ((values stmts))
                                             (make-case values stmts))
                                           cases) rest))
           :full-stmt t
           rest)))


;;; Handling header information (formerly "Michondria")
(defgeneric add-type (software type)
  (:documentation "Add TYPE to `types' of SOFTWARE, unique by hash."))

(defmethod add-type ((obj clang-base) (type clang-type))
  "Add TYPE to `types' of OBJ, unique by hash.
* OBJ software object to modify
* TYPE type to be added
"
  (add-type* obj type))

(defun add-type* (obj type)
  (unless (gethash (type-hash type) (types obj))
    (if (type-i-file type)
      ;; add requisite includes for this type
      (add-include obj (type-i-file type))
      ;; only add to the genome if there isn't a type with the same type-decl
      ;; already known
      (let ((td (type-decl type)))
        (unless (or (not td)
                    (equal td "")
                    (member td
                            (hash-table-values (types obj))
                            :key #'type-decl
                            :test #'string=))
          ;; FIXME: ideally this would insert an AST for the type decl
          ;; instead of just adding the text.
          (prepend-to-genome obj td))))
    ;; always add type with new hash to types hashtable
    (setf (gethash (type-hash type) (types obj)) type))
  type)

(defmethod add-type ((obj clang-base) (type null))
  "Add TYPE to `types' of OBJ, unique by hash.
* OBJ software object to modify
* TYPE null to allow for nop when nil is passed for the type argument
"
  nil)

(defmethod find-type ((obj clang-base) hash)
  "Return the type in OBJ with the given type HASH.
* OBJ clang object to search for HASH
* HASH type hash to search for
"
  (gethash hash (types obj)))

(defmethod find-or-add-type ((obj clang) name &key
                             (pointer nil pointer-arg-p)
                             (array "" array-arg-p)
                             (const nil const-arg-p)
                             (volatile nil volatile-arg-p)
                             (restrict nil restrict-arg-p)
                             (storage-class :None storage-class-arg-p)
                             &aux (trace-type (type-from-trace-string name)))
  "Find the type with given properties, or add it to the type DB.

* OBJ software object to modify or search
* NAME name of the type
* ARRAY string indicating array modifiers to the type
* CONST boolean indicating if the type is const qualifed
* VOLATILE boolean indicating if the type is volatile qualified
* RESTRICT boolean indicating if the type is restrict qualified
* STORAGE-CLASS symbol indicating the type storage class (e.g. :static)
"
  (let ((type (copy trace-type
                    :hash (->> (hash-table-values (types obj))
                               (mapcar #'type-hash)
                               (apply #'max)
                               (1+))
                    :pointer (if pointer-arg-p
                                 pointer
                                 (type-pointer trace-type))
                    :array (if array-arg-p
                               array
                               (type-array trace-type))
                    :const (if const-arg-p
                               const
                               (type-const trace-type))
                    :volatile (if volatile-arg-p
                                  volatile
                                  (type-volatile trace-type))
                    :restrict (if restrict-arg-p
                                  restrict
                                  (type-restrict trace-type))
                    :storage-class (if storage-class-arg-p
                                       storage-class
                                       (type-storage-class trace-type)))))
    (or (find-if «and [{string= (type-name type)} #'type-name]
                      [{string= (type-array type)} #'type-array]
                      [{eq (type-pointer type)} #'type-pointer]
                      [{eq (type-const type)} #'type-const]
                      [{eq (type-volatile type)} #'type-volatile]
                      [{eq (type-restrict type)} #'type-restrict]
                      [{eq (type-storage-class type)} #'type-storage-class]»
                 (hash-table-values (types obj)))
        (add-type obj type) type)))

(defgeneric declared-type (ast variable-name)
  (:documentation "Guess the type of the VARIABLE-NAME in AST.
VARIABLE-NAME should be declared in AST."))

(defmethod declared-type ((ast clang-ast-base) variable-name)
  "Guess the type of the VARIABLE-NAME in AST.
VARIABLE-NAME should be declared in AST."
  ;; NOTE: This is very simple and probably not robust to variable
  ;; declarations which are "weird" in any way.
  (declare (ignorable variable-name))
  (first
   (split-sequence #\Space (source-text ast) :remove-empty-subseqs t)))

(defgeneric find-var-type (software variable)
  (:documentation "Return the type of VARIABLE in SOFTWARE."))

(defmethod find-var-type ((obj clang-base) (variable list))
  "Return the type of VARIABLE in SOFTWARE"
  (some->> (aget :type variable)
           (find-type obj)))

(defgeneric typedef-type (software type)
  (:documentation "Return the underlying type if TYPE is a typedef"))

(defmethod typedef-type ((obj clang-base) (type clang-type))
  "Return the underlying type in OBJ if TYPE is a typedef"
  (labels ((typedef-type-helper (obj type)
             (if (and (equal 1 (length (type-reqs type)))
                      (equal 0 (search "typedef" (type-decl type))))
                 (typedef-type-helper obj
                                      (find-type obj
                                                 (first (type-reqs type))))
                 type)))
    (copy type :hash (if (= (type-hash (typedef-type-helper obj type))
                            (type-hash type))
                         (type-hash type)
                         0)
               :name (type-name (typedef-type-helper obj type))
               :decl (type-decl (typedef-type-helper obj type)))))

(defgeneric type-decl-string (type &key qualified)
  (:documentation "The source text used to declare variables of TYPE.

This will have stars on the right, e.g. char**. ")
  (:method ((type clang-type) &key (qualified t))
    "Return the source text used to declare variables of TYPE.
* TYPE type to convert to a declaration string
* QUALIFIED add type qualifiers such as const or volatile if non-nil.
"
    (format nil "~a~a~a~a~a~a~a"
            (if (and qualified (type-const type)) "const " "")
            (if (and qualified (type-volatile type)) "volatile " "")
            (if (and qualified (type-restrict type)) "restrict " "")
            (if (and qualified (not (eq :None (type-storage-class type))))
                (format nil "~a " (->> (type-storage-class type)
                                    (symbol-name)
                                    (string-downcase)))
                "")
            (cond ((equal 0 (search "struct" (type-decl type)))
                   "struct ")
                  ((equal 0 (search "union" (type-decl type)))
                   "union ")
                  (t ""))
            (type-name type)
            (if (type-pointer type) " *" ""))))

(defgeneric type-trace-string (type &key qualified)
  (:documentation "The text used to describe TYPE in an execution trace.

This will have stars on the left, e.g **char."))
(defmethod type-trace-string ((type clang-type) &key (qualified t))
  "Return the text used to describe TYPE in an execution trace.
* TYPE type to convert to a trace string
* QUALIFIED add type qualifiers such as const or volatile if non-nil.
"
  (type-trace-string* type qualified))

;; Split out so it can be called from a new-clang method
(defun type-trace-string* (type qualified)
  (concatenate 'string
               (when (type-pointer type) "*")
               (when (not (emptyp (type-array type))) (type-array type))
               (when (and qualified (type-const type)) "const ")
               (when (and qualified (type-volatile type)) "volatile ")
               (when (and qualified (type-restrict type)) "restrict ")
               (when (and qualified
                          (not (eq :None (type-storage-class type))))
                 (format nil "~a " (->> (type-storage-class type)
                                        (symbol-name)
                                        (string-downcase))))
               (type-name type)))

(defgeneric type-from-trace-string (name)
  (:documentation
   "Create a clang-type from a name used in an execution trace.
The resulting type will not be added to any clang object and will not have a
valid hash."))
(defmethod type-from-trace-string ((name string))
  "Create a clang-type from a name used in an execution trace.
The resulting type will not be added to any clang object and will not have a
valid hash.

* NAME type name as expressed in an execution trace
"
  (type-from-trace-string* #'make-clang-type name))

(defun type-from-trace-string* (fn name)
  (funcall fn
    :pointer (not (null (find #\* name)))
    :array (if (find #\[ name) (scan-to-strings "\\[[^\\[\\]]*\\]" name) "")
    :const (not (null (search "const" name)))
    :volatile (not (null (search "volatile" name)))
    :restrict (not (null (search "restrict" name)))
    :storage-class (or (register-groups-bind (storage-class)
                           ("(extern|static|__private_extern__|auto|register)"
                            name)
                         (make-keyword (string-upcase storage-class)))
                       :None)
    :hash 0
    :name (regex-replace
           (format nil
                   "^(\\*|\\[[^\\[\\]]*\\]|const |volatile |restrict |extern |~
            static |__private_extern__ |auto |register )*")
           name "")))

(defgeneric add-macro (software macro)
  (:documentation "Add MACRO to `macros' of SOFTWARE, unique by hash."))
(defmethod add-macro ((obj clang-base) (macro clang-macro))
  "Add MACRO to `macros' of OBJ, unique by hash.
* OBJ object to modify with macro
* MACRO macro to add"
  (unless (find-macro obj (macro-hash macro))
    (prepend-to-genome obj (format nil "#define ~a~&" (macro-body macro)))
    (push macro (macros obj)))
  obj)

(defmethod find-macro ((obj clang-base) hash)
  "Return the macro in OBJ with the given HASH.
* OBJ object to search for HASH
* HASH macro hash to find
"
  (find-if {= hash} (macros obj) :key #'macro-hash))

(defgeneric add-include (software include)
  (:documentation "Add an #include directive for an INCLUDE to SOFTWARE."))

(defmethod add-include ((obj clang-base) (include string))
  "Add an #include directive for an INCLUDE to OBJ.
* OBJ object to modify
* INCLUDE header to include in OBJ
"
  (unless (member include (includes obj) :test #'string=)
    (prepend-to-genome obj (format nil "#include ~a~&" include))
    (push include (includes obj)))
  obj)

(defmethod force-include ((obj clang-base) include)
  "Add an #include directive for an INCLUDE to OBJ
even if such an INCLUDE already exists in OBJ.
* OBJ object to modify
* INCLUDE header to include in OBJ
"
  (prepend-to-genome obj (format nil "#include ~a~&" include))
  (unless (member include (includes obj) :test #'string=)
    (push include (includes obj)))
  obj)


;;; Constants
(define-constant +c-numeric-types+
    '("char" "short" "int" "long" "float" "double" "long double")
  :test #'equalp
  :documentation "C Numeric type names.")

(define-constant +c-relational-operators+
    '("<" "<=" "==" "!=" ">=" ">")
  :test #'equalp
  :documentation "C Relational operators.")

(define-constant +c-arithmetic-binary-operators+
    '("+" "-" "*" "/" "%")
  :test #'equalp
  :documentation "C arithmetic operators on two arguments.")

(define-constant +c-arithmetic-assignment-operators+
    '("+=" "-=" "*=" "/=" "%=")
  :test #'equalp
  :documentation "C arithmetic assignment operators.")

(define-constant +c-bitwise-binary-operators+
    '("&" "|" "^" "<<" ">>")
  :test #'equalp
  :documentation "C bitwise operators on two arguments.")

(define-constant +c-bitwise-assignment-operators+
    '("&=" "|=" "^=" "<<=" ">>=")
  :test #'equalp
  :documentation "C bitwise assignment operators.")

(define-constant +c-arithmetic-unary-operators+
    '("++" "--")
  :test #'equalp
  :documentation "C arithmetic operators on one arguments.")

(define-constant +c-bitwise-unary-operators+
    '("~" "!")
  :test #'equalp
  :documentation "C bitwise operators on one arguments.")

(define-constant +c-sign-unary-operators+
    '("+" "-" )
  :test #'equalp
  :documentation "C sign operators on one arguments.")

(define-constant +c-pointer-unary-operators+
    '("&" "*" )
  :test #'equalp
  :documentation "C pointer operators on one arguments.")


;;; Mutations
;;;
;;; TODO: Loop iteration order flip.  \cite{Nicholas Harrand}
;;;
(defclass clang-mutation (mutation)
  ()
  (:documentation "Specialization of the mutation interface for clang software
objects."))

;; Filters for use with Targeting functions
(defun full-stmt-filter (ast &optional first-pick)
  "Targeting filter returning true if AST is a full statement.
* AST possible second targeting function pick
* FIRST-PICK first targeting function pick
"
  (declare (ignorable first-pick))
  (ast-full-stmt ast))

(defun same-class-filter (ast &optional first-pick)
  "Targeting filter returning true if AST and FIRST-PICK have the same AST class.
* AST possible second targeting function pick
* FIRST-PICK first targeting function pick
"
  (if first-pick
      (eq (ast-class ast) (ast-class first-pick))
      t))

;; Insert
(define-mutation clang-insert (clang-mutation)
  ((targeter :initform #'pick-bad-good))
  (:documentation "Perform an insertion operation on a clang software object."))

(defmethod build-op ((mutation clang-insert) software)
  "Return an association list with the operations to apply a `clang-insert'
MUTATION to SOFTWARE.
* MUTATION defines targets of insertion operation
* SOFTWARE object to be modified by the mutation
"
  (declare (ignorable software))
  `((:insert . ,(targets mutation))))

(define-mutation clang-insert-full (clang-insert)
  ((targeter :initform {pick-bad-good _ :filter #'full-stmt-filter}))
  (:documentation  "Perform an insertion operation on a clang software object,
only inserting full statements."))

(define-mutation clang-insert-same (clang-insert)
  ((targeter :initform {pick-bad-good _ :filter #'same-class-filter}))
  (:documentation "Perform an insertion operation on a clang software object,
only inserting statements of the same AST class as the preceding statement."))

(define-mutation clang-insert-full-same (clang-insert)
  ((targeter :initform {pick-bad-good _ :filter «and #'full-stmt-filter
                                                     #'same-class-filter»}))
  (:documentation "Perform an insertion operation on a clang software object,
only inserting full statements of the same AST class as the preceding
statement."))

;;; Swap
(define-mutation clang-swap (clang-mutation parseable-swap)
  ((targeter :initform #'pick-bad-bad))
  (:documentation "Perform a swap operation on a clang software object."))

(defmethod build-op ((mutation clang-swap) software)
  "Return an association list with the operations to apply a `clang-swap'
MUTATION to SOFTWARE.
* MUTATION defines targets of the swap operation
* SOFTWARE object to be modified by the mutation
"
  ;; This was specialized from parseable-swap because we may need to
  ;; add or remove semicolons when swapping expressions and statements

  (declare (ignorable software))
  (let* ((targets (targets mutation))
         (s1 (aget :stmt1 targets))
         (s2 (aget :stmt2 targets))
         (s1-full? (full-stmt-p software s1))
         (s2-full? (full-stmt-p software s2))
         (s1-semi? (has-trailing-semicolon-p s1))
         (s2-semi? (has-trailing-semicolon-p s2)))
    `((:set (:stmt1 . ,s1)
            (:stmt2 . ,(if (or s1-full? s1-semi?)
                           s2 ;; depending on fixup-mutations to add semis
                           (remove-semicolon s2))))
      (:set (:stmt1 . ,s2)
            (:stmt2 . ,(if (or s2-full? s2-semi?)
                           s1 ;; depending on fixup-mutations to add semis
                           (remove-semicolon s1)))))))

(define-mutation clang-swap-full (clang-swap)
  ((targeter :initform {pick-bad-bad _ :filter #'full-stmt-filter}))
  (:documentation "Perform a swap operation on a clang software object,
only swapping full statements."))


(define-mutation clang-swap-same (clang-swap)
  ((targeter :initform {pick-bad-bad _ :filter #'same-class-filter}))
  (:documentation "Perform a swap operation on a clang software object,
only swapping statements of the same AST class."))

(define-mutation clang-swap-full-same (clang-swap)
  ((targeter :initform {pick-bad-good _ :filter «and #'full-stmt-filter
                                                     #'same-class-filter»}))
  (:documentation "Perform a swap operation on a clang software object,
only full statements of the same AST class.")
  )

;;; Move
(define-mutation clang-move (clang-mutation parseable-move)
  ((targeter :initform #'pick-bad-bad))
  (:documentation "Perform a move operation on a clang software object."))

;;; Replace
(define-mutation clang-replace (clang-mutation parseable-replace)
  ((targeter :initform #'pick-bad-good))
  (:documentation "Perform a replace operation on a clang software object."))

(define-mutation clang-replace-full (clang-replace)
  ((targeter :initform {pick-bad-good _ :filter #'full-stmt-filter}))
  (:documentation "Perform a replace operation on a clang software object,
only replacing full statements."))

(define-mutation clang-replace-same (clang-replace)
  ((targeter :initform {pick-bad-good _ :filter #'same-class-filter}))
  (:documentation "Perform a replace operation on a clang software object,
only replacing statements of the same AST class."))

(define-mutation clang-replace-full-same (clang-replace)
  ((targeter :initform {pick-bad-good _ :filter «and #'full-stmt-filter
                                            	     #'same-class-filter»}))
  (:documentation "Perform a replace operation on a clang software object,
only replacing full statements of the same AST class."))

;;; Cut
(define-mutation clang-cut (clang-mutation parseable-cut)
  ((targeter :initform #'pick-bad-only))
  (:documentation "Perform a cut operation on a clang software object."))

(define-mutation clang-cut-full (clang-cut)
  ((targeter :initform {pick-bad-only _ :filter #'full-stmt-filter}))
  (:documentation "Perform a cut operation on a clang software object,
only cutting full statements."))

;;; Nop
(define-mutation clang-nop (clang-mutation parseable-nop)
  ()
  (:documentation "Perform a nop on a clang software object."))

(defmethod build-op ((mutation clang-nop) software)
  (declare (ignorable software mutation))
  nil)

(define-mutation clang-promote-guarded (clang-mutation)
  ((targeter :initform #'pick-guarded-compound))
  (:documentation "Promote a guarded compound statement in a clang
software object."))

(defgeneric pick-guarded-compound (software)
  (:documentation "Pick a guarded compound statement in SOFTWARE."))

(define-constant +clang-guarded-classes+
    '(:IfStmt :ForStmt :WhileStmt :DoStmt)
  :test #'equalp
  :documentation "Statement classes with guards")

(defmethod pick-guarded-compound ((obj clang-base))
  "Return a guarded statement in OBJ from the `bad-stmts' pool.
* OBJ software object to pick from
"
  (aget :stmt1
        (pick-bad-only obj :filter [{member _ +clang-guarded-classes+}
                                    #'ast-class])))

(defmethod build-op ((mutation clang-promote-guarded) software
                     &aux (guarded (targets mutation)))
  "Return an association list with the operations to apply a
`clang-promote-guarded' MUTATION to SOFTWARE.
* MUTATION defines the targets of the cut operation
* SOFTWARE object to be modified by the mutation
"
  (labels
      ((compose-children (&rest parents)
         (-<>> (iter (for p in parents)
                     ;; In case of an unbraced if/loop body, include
                     ;; the body directly.
                     (if (eq :CompoundStmt (ast-class p))
                         (appending (get-immediate-children software p))
                         (collecting p)))
	   (interleave <> (format nil "~%")))))

      (let ((children
          (switch ((ast-class guarded))
            (:DoStmt
             (compose-children
              (first (get-immediate-children software guarded))))
            (:WhileStmt
             (compose-children
              (second (get-immediate-children software guarded))))
            (:ForStmt
             (compose-children
              (lastcar (get-immediate-children software guarded))))
            (:IfStmt
             (let ((children (get-immediate-children software guarded)))
               (if (= 2 (length children))
                   ;; If with only one branch.
                   (compose-children (second children))
                   ;; If with both branches.
                   (cond
                     ((null             ; Then branch is empty.
                       (get-immediate-children software (second children)))
                      (compose-children (third children)))
                     ((null             ; Else branch is empty.
                       (get-immediate-children software (third children)))
                      (compose-children (second children)))
                     (t                 ; Both branches are populated.
                      (if (random-bool) ; Both or just one.
                          (compose-children (second children) (third children))
                          (if (random-bool) ; Pick a branch randomly.
                              (compose-children (second children))
                              (compose-children (third children)))))))))
            (t (warn "`clang-promote-guarded' unimplemented for ~a"
                     (ast-class guarded))))))

        `((:splice (:stmt1 . ,guarded) (:value1 . ,children))))))

;;; Explode and coalescing mutations over for and while loops.
(define-mutation explode-for-loop (clang-mutation)
  ((targeter :initform #'pick-for-loop))
  (:documentation
   "Select a 'for' loop and explode it into it's component parts.
This mutation will transform 'for(A;B;C)' into 'A;while(B);C'."))

(defgeneric pick-for-loop (software)
  (:documentation "Pick and return a 'for' loop in SOFTWARE."))

(defmethod pick-for-loop ((obj clang-base))
  "Return a for loop in OBJ from the `bad-stmts' pool.
* OBJ software object to pick from
"
  (pick-bad-only obj :filter [{eq :ForStmt} #'ast-class]))

(defmethod build-op ((mutation explode-for-loop) (obj clang-base))
  "Return an association list with the operations to apply an
`explode-for-loop' MUTATION to OBJ.
* MUTATION defines the targets of the explode-for-loop operation
* OBJ object to be modified by the mutation
"
  (labels ((is-initialization-ast (ast)
             (and (eq :BinaryOperator (ast-class ast))
                  (equal "=" (ast-opcode ast))))
           (is-condition-ast (ast)
             (or (eq :ImplicitCastExpr (ast-class ast))
                 (and (eq :BinaryOperator (ast-class ast))
                      (not (equal "=" (ast-opcode ast))))))
           (destructure-for-loop (ast)
             ;; Return the initialization, conditional, increment, and body
             ;; ASTS of the for-loop AST identified by ID as VALUES.
             ;;
             ;; This is an imperfect solution based on heuristics as to
             ;; probable ASTs for each part of a for loop.  These heuristics
             ;; undoubtedly will fail for some cases, and a non-compiling
             ;; individual will be created as a result.
             (let ((children (get-immediate-children obj ast)))
               (case (length children)
                 (4 (values-list children))
                 (3 (if (is-initialization-ast (first children))
                        (if (is-condition-ast (second children))
                            (values (first children)
                                    (second children)
                                    nil
                                    (third children))
                            (values (first children)
                                    nil
                                    (second children)
                                    (third children)))
                        (values nil
                                (first children)
                                (second children)
                                (third children))))
                 (2 (if (is-initialization-ast (first children))
                        (values (first children)
                                nil
                                nil
                                (second children))
                        (if (is-condition-ast (first children))
                            (values nil
                                    (first children)
                                    nil
                                    (second children))
                            (values nil
                                    nil
                                    (first children)
                                    (second children)))))
                 (1 (values nil nil nil (first children))) ; Always assume body
                 (otherwise (values nil nil nil nil))))))
    (let ((ast (aget :stmt1 (targets mutation))))
      (multiple-value-bind (initialization condition increment body)
        (destructure-for-loop ast)
        (let* ((condition (or condition (make-literal 1)))
               (body (make-block (if increment
                                     (list body increment ";")
                                     (list body)))))
         `((:set (:stmt1 . ,ast)
                 (:literal1 . ,(make-while-stmt (ast-syn-ctx ast)
                                                condition
                                                body)))
           .
           ,(when initialization
                  `((:insert (:stmt1 . ,ast)
                             (:literal1 . ,initialization))))))))))

(define-mutation coalesce-while-loop (clang-mutation)
  ((targeter :initform #'pick-while-loop))
  (:documentation
   "Select a 'while' loop and coalesce it into a 'for' loop.
This mutation will transform 'A;while(B);C' into 'for(A;B;C)'."))

(defgeneric pick-while-loop (software)
  (:documentation "Pick and return a 'while' loop in SOFTWARE."))

(defmethod pick-while-loop ((obj clang-base))
  "Return a while loop statement in OBJ from the `bad-stmts' pool.
* OBJ software object to pick from
"
  (pick-bad-only obj :filter [{eq :WhileStmt} #'ast-class]))

(defmethod build-op ((mutation coalesce-while-loop) (obj clang-base))
  "Return an association list with the operations to apply a
`coalesce-while-loop' MUTATION to SOFTWARE.
* MUTATION defines the targets of the coalesce-while-loop operation
* OBJ object to be modified by the mutation
"
  (let ((ast (aget :stmt1 (targets mutation))))
    (destructuring-bind (condition body)
        (get-immediate-children obj ast)
      (let ((precedent (block-predeccessor obj ast)))
        `((:set (:stmt1 . ,ast)
                ,(let ((children (get-immediate-children obj body)))
                      (cons :literal1
                            (make-for-stmt (ast-syn-ctx ast)
                                           (remove-semicolon precedent)
                                           condition
                                           (some->> children
					     (lastcar)
					     (remove-semicolon))
                                           (some->> children
                                                    (butlast)
                                                    (make-block))))))
          ;; Possibly consume the preceding full statement.
          ,@(when precedent
                  ;; Delete precedent
                  `((:cut (:stmt1 . ,precedent)))))))))

;;; Cut Decl
(define-mutation cut-decl (clang-mutation)
  ((targeter :initform #'pick-cut-decl))
  (:documentation
   "Perform a cut operation on a DeclStmt AST in a clang software object."))

(defun pick-cut-decl (clang)
  "Return a DeclStmt AST in CLANG from the `bad-stmts' pool.
* CLANG software object to pick from"
  (pick-bad-only clang :filter [{eq :DeclStmt} #'ast-class]))

(defmethod build-op ((mutation cut-decl) clang)
  "Return an association list with the operations to apply a `cut-decl'
MUTATION to CLANG.
* MUTATION defines the targets of the cut-decl operation
* CLANG object to be modified by the mutation
"
  (let* ((decl (aget :stmt1 (targets mutation)))
         (the-block (enclosing-block clang decl))
         (old-names (ast-declares decl))
         (uses (mappend (lambda (x) (get-children-using clang x the-block))
                        old-names))
         (vars (remove-if {find _ old-names :test #'name=}
                          (mapcar {aget :name}
                                  (get-vars-in-scope clang
                                    (if uses (car uses) the-block)))))
         (var (mapcar (lambda (old-name)
                        (declare (ignorable old-name))
                        (if vars
                            (random-elt vars)
                            "/* no vars before first use of cut-decl */"))
                      old-names)))
    (delete-decl-stmts clang the-block `((,decl . ,var)))))

;;; Swap Decls
(define-mutation swap-decls (clang-swap)
  ((targeter :initform #'pick-swap-decls))
  (:documentation "Swap two DeclStmt ASTs in a clang software object."))

(defun pick-swap-decls (clang)
  "Return two DeclStmt AST in CLANG from the `bad-stmts' pool.
* CLANG software object to pick from
"
  (labels
    ((is-decl (ast)
       (eq :DeclStmt (ast-class ast)))
     (pick-another-decl-in-block (ast)
       (some->> (enclosing-block clang ast)
                (get-immediate-children clang)
                (remove-if-not [{eq :DeclStmt} #'ast-class])
                (remove-if {equalp ast})
                (random-elt))))
    (if-let ((decl (some-> (bad-mutation-targets clang
                                                 :filter «and #'is-decl #'pick-another-decl-in-block»)
                           (random-elt))))
            `((:stmt1 . ,decl)
              (:stmt2 . ,(pick-another-decl-in-block decl))))))

;;; Rename variable
(define-mutation rename-variable (clang-mutation)
  ((targeter :initform #'pick-rename-variable))
  (:documentation
   "Replace a variable in a statement with another in scope variable name."))

(defun pick-rename-variable (clang)
  "Pick a statement in CLANG with a variable and replace with another in scope."
  (let* ((stmt (random-elt (bad-mutation-targets clang
                             :filter {get-used-variables clang})))
         (used (get-used-variables clang stmt))
         (old-var (random-elt used))
         (new-var (random-elt
                   (or (remove-if {equal old-var}
                                  (mapcar {aget :name}
                                          (get-vars-in-scope clang stmt)))
                       (list old-var))))
         (stmt1 (enclosing-full-stmt clang stmt)))
    `((:stmt1 . ,stmt1) (:old-var . ,old-var) (:new-var . ,new-var))))

(defmethod build-op ((mutation rename-variable) software)
  "Return an association list with the operations to apply a `rename-variable'
MUTATION to SOFTWARE.
* MUTATION defines the targets of the rename-variable operation
* SOFTWARE object to be modified by the mutation
"
  (declare (ignorable software))
  (let ((stmt1 (aget :stmt1 (targets mutation)))
        (old-var (aget :old-var (targets mutation)))
        (new-var (aget :new-var (targets mutation))))
    `((:set
       (:stmt1 . ,stmt1)
       (:literal1 . ,(rebind-vars stmt1
                                  (list (list (unpeel-bananas old-var)
                                              (unpeel-bananas new-var)))
                                  nil))))))

;;; Expand compound assignment or increment/decrement
(define-mutation expand-arithmatic-op (clang-replace)
  ((targeter :initform #'pick-expand-arithmatic-op))
  (:documentation "Expand a compound assignment or increment/decrement operation
in a clang software object."))

(defun pick-expand-arithmatic-op (clang)
  "Pick a compound assignment or increment/decrement operation in CLANG
to expand.
* CLANG software object to pick from
"
  (labels ((compound-assign-op (ast) (->> (ast-class ast)
                                          (eq :CompoundAssignOperator)))
           (increment-op (ast) (and (->> (ast-class ast)
                                         (eq :UnaryOperator))
                                    (->> (ast-opcode ast)
                                         (equal "++"))))
           (decrement-op (ast) (and (->> (ast-class ast)
                                         (eq :UnaryOperator))
                                    (->> (ast-opcode ast)
                                         (equal "--")))))
    (let ((ast (some-> (bad-mutation-targets clang
                                             :filter «or #'compound-assign-op
                                                         #'increment-op
                                                         #'decrement-op»)
                       (random-elt))))
      `((:stmt1 . ,ast)
        (:literal1 .
           ,(let* ((children (get-immediate-children clang ast))
                   (lhs (first children))
                   (rhs (second children))
                   (one (make-literal 1)))
              (cond
               ((increment-op ast)
                (make-operator (ast-syn-ctx ast) "="
                               (list lhs (make-operator (ast-syn-ctx lhs)
                                                        "+"
                                                        (list lhs one)))))
               ((decrement-op ast)
                (make-operator (ast-syn-ctx ast) "="
                               (list lhs (make-operator (ast-syn-ctx lhs)
                                                        "-"
                                                        (list lhs one)))))
               (t (make-operator
                   (ast-syn-ctx ast) "="
                   (list lhs
                         (make-operator (ast-syn-ctx rhs)
                                        (string-trim "="
                                                     (ast-opcode ast))
                                        (list lhs rhs))))))))))))


;;; Clang methods
(defgeneric stmts (software)
  (:documentation "Return a list of all statement asts in SOFTWARE."))

(defgeneric good-stmts (software)
  (:documentation "Return a list of all good statement asts in SOFTWARE."))

(defgeneric bad-stmts (software)
  (:documentation "Return a list of all bad statement asts in SOFTWARE."))

(defvar *clang-max-json-size* 104857600
  "Maximum size of output accepted from `clang-mutate'.")

(defvar *clang-json-required-fields*
  '(:class              :counter           :unbound-vals
    :unbound-funs       :types             :syn-ctx
    :parent-counter     :macros            :guard-stmt
    :full-stmt          :begin-addr        :end-addr
    :includes           :declares          :is-decl
    :opcode             :children          :begin-off
    :end-off            :size              :in-macro-expansion)
  "JSON database entry fields required for clang software objects.")

(defvar *clang-json-required-aux*
  '(:asts :types :macros)
  "JSON database AuxDB entries required for clang software objects.")

(defvar *clang-ast-aux-fields* nil
  "Extra fields to read from clang-mutate into ast-aux-data.")

(defvar *clang-mutate-additional-args* nil
  "Extra arguments to pass to clang-mutate when parsing")

(defun asts->tree (genome asts)
  "Convert the list of ASTs into an applicative AST tree to return.
* GENOME source code parsed into ASTs
* ASTS list of ASTs in GENOME as identified by clang-mutate
"
  (let ((roots (mapcar {aget :counter}
                       (remove-if-not [{eql 0} {aget :parent-counter}] asts)))
        (ast-vector (coerce asts 'vector))
        ;; Find all multi-byte characters in the genome for adjusting
        ;; offsets later.
        (byte-offsets
         (iter (for c in-string genome)
               (with byte = 0)
               (for length = (->> (make-string 1 :initial-element c)
                                  (babel:string-size-in-octets)))
               (incf byte length)
               (when (> length 1)
                 (collecting (cons byte (1- length)))))))
   (labels
       ((get-ast (id)
          (aref ast-vector (1- id)))
        (byte-offset-to-chars (offset)
          (if (eq offset :end)
              ;; Special case for end of top-level AST.
              (- (length genome) 1)

              ;; Find all the multi-byte characters at or before this
              ;; offset and accumulate the byte->character offset
              (- offset
                 (iter (for (pos . incr) in byte-offsets)
                       (while (<= pos offset))
                       (summing incr)))))
        (begin-offset (ast-alist)
          (byte-offset-to-chars (aget :begin-off ast-alist)))
        (end-offset (ast-alist)
          (byte-offset-to-chars (aget :end-off ast-alist)))
        (safe-subseq (seq start end)
          (subseq seq start (if (<= end start) start end)))
        (collect-children (ast-alist)
          ;; Find child ASTs and sort them in textual order.
          (let ((children (sort (mapcar #'get-ast (aget :children ast-alist))
                                (lambda (a b)
                                  (let ((a-begin (aget :begin-off a))
                                        (b-begin (aget :begin-off b)))
                                    ;; If ASTs start at the same place, put the
                                    ;; larger one first so parent-child munging
                                    ;; below works nicely.
                                    (if (= a-begin b-begin)
                                        (> (aget :end-off a) (aget :end-off b))
                                        (< a-begin b-begin)))))))
            ;; clang-mutate can produce siblings with overlapping source
            ;; ranges. In this case, move one sibling into the child list of the
            ;; other. See the typedef-workaround test for an example.
            ;;
            ;; NOTE: This next bit of code may be too clever by half.
            ;;       It holds a pointer named `prev' into the most
            ;;       recently collected `c'.  It then mutates the
            ;;       previously collected `c' based on processing of
            ;;       the subsequent `c' in the list.  Because of the
            ;;       mechanics of `(setf aget)' this only actually
            ;;       adds to the :children element of a list of that
            ;;       element exists *before* the setf call.  Hence the
            ;;       necessity for the `(unless (assoc :children c) ...)'
            ;;       bit before collecting `c'.
            (iter (for c in children)
                  (with prev)
                  (if (and prev
                           (< (aget :begin-off c) (aget :end-off prev)))
                      (progn (setf (aget :end-off prev)
                                   (max (aget :end-off prev)
                                        (aget :end-off c)))
                             (push (aget :counter c) (aget :children prev)))
                      (progn (unless (assoc :children c)
                               (setf c (cons (list :children) c)))
                             (setf prev c)
                             (collect c))))))
        (make-children (ast-alist child-ast-alists)
          (let ((start (begin-offset ast-alist)))
            ;; In macro expansions, the mapping to source text is sketchy and
            ;; it's impossible to build a proper hierarchy. So don't recurse
            ;; into them. And change the class so other code won't get
            ;; confused by the lack of children.
            (when (aget :in-macro-expansion ast-alist)
              (setf (aget :class ast-alist) "MacroExpansion")
              (setf (aget :syn-ctx ast-alist)
                    (if (string= "Braced" (aget :syn-ctx ast-alist))
                        "FullStmt"
                        (aget :syn-ctx ast-alist))))

            (if (and child-ast-alists (not (aget :in-macro-expansion ast-alist)))
                ;; Interleave child asts and source text
                (iter (for subtree in child-ast-alists)
                      (for c = (car subtree))
                      (for i upfrom 0)
                      ;; Collect text
                      (collect (safe-subseq genome
                                            start
                                            (begin-offset c))
                        into children)
                      ;; Collect child, converted to AST struct
                      (collect (cons (from-alist 'clang-ast-node c)
                                     (cdr subtree))
                        into children)
                      (setf start (+ 1 (end-offset c)))
                      (finally
                       (return
                         (append children
                                 (list (safe-subseq
                                        genome
                                        start
                                        (+ 1 (end-offset ast-alist))))))))
                ;; No children: create a single string child with source text
                (let ((text (safe-subseq genome
                                         (begin-offset ast-alist)
                                         (+ 1 (end-offset ast-alist)))))
                  (when (not (emptyp text))
                    (list (cond ((string= "DeclRefExpr"
                                          (aget :class ast-alist))
                                 (unpeel-bananas text))
                                ((and (string= "MacroExpansion"
                                               (aget :class ast-alist))
                                      (or (->> (aget :parent-counter ast-alist)
                                               (zerop))
                                          (->> (aget :parent-counter ast-alist)
                                               (get-ast)
                                               (aget :in-macro-expansion)
                                               (not))))
                                 (reduce
                                   (lambda (new-text unbound)
                                     (regex-replace-all
                                       (format nil "(^|[^A-Za-z0-9_]+)~
                                                    (~a)~
                                                    ([^A-Za-z0-9_]+|$)"
                                               unbound)
                                       new-text
                                       (format nil "\\1~a\\3"
                                               (unpeel-bananas unbound))))
                                   (append (unbound-vals ast-alist)
                                           (unbound-funs ast-alist))
                                   :initial-value text))
                                (t text))))))))
        (unbound-vals (ast-alist)
          (mapcar #'peel-bananas (aget :unbound-vals ast-alist)))
        (unbound-funs (ast-alist)
          (mapcar [#'peel-bananas #'car] (aget :unbound-funs ast-alist)))
        (unaggregate-ast (ast-alist children)
          (if (aget :in-macro-expansion ast-alist)
              ;; Peel bananas from variable names
              (setf (aget :unbound-vals ast-alist) (unbound-vals ast-alist))

              ;; clang-mutate aggregates types, unbound-vals, and unbound-funs
              ;; from children into parents. Undo that so it's easier to
              ;; update these properties after mutation.
              (iter (for c in children)
                    (appending (aget :types c) into child-types)
                    (appending (aget :unbound-vals c) into child-vals)
                    (appending (aget :unbound-funs c) into child-funs)

                    (finally
                     (unless (member (aget :class ast-alist)
                                     '("Var" "ParmVar")
                                     :test #'string=)
                       (setf (aget :types ast-alist)
                             (remove-if {member _ child-types}
                                        (aget :types ast-alist))))

                     (setf (aget :unbound-vals ast-alist)
                           (->> (remove-if {member _ child-vals :test #'string=}
                                           (aget :unbound-vals ast-alist))
                                (mapcar #'peel-bananas))

                           (aget :unbound-funs ast-alist)
                           (remove-if {member _ child-funs :test #'equalp}
                                      (aget :unbound-funs ast-alist))))))
          ast-alist)
        (make-tree (ast-alist &aux (stack nil))
          ;; Iterative replacement for the following recursive algorithm.
          ;; Uses an explicit stack for operations.
          ;;
          ;; (make-tree (ast &aux (children (collect-children ast))
          ;;                         (new-ast (unaggregate-ast ast children)))
          ;;  (cons new-ast (make-children new-ast
          ;;                               (mapcar #'make-tree children))))
          (pushnew (cons nil (list ast-alist)) stack)

          (iter (while (or (not (= 1 (length stack)))
                           (null (first (first stack)))))
                (let ((top (or (car (cdr (first stack)))
                               (car (cdr (second stack))))))
                  (cond ((not (null (cdr (first stack))))
                         (let ((children (collect-children top)))
                            (setf top (unaggregate-ast top children))
                            (push (cons nil children) stack)))
                        (t
                         (let ((new-children (reverse (car (pop stack)))))
                           (push (cons top (make-children top
                                                          new-children))
                                 (car (first stack)))
                           (pop  (cdr (first stack)))))))
                (finally (return (first (first (pop stack)))))))
        (create-clang-asts (tree &optional path)
          (make-clang-ast
            :path (reverse path)
            :node (car tree)
            :children (iter (for c in (cdr tree))
                            (for i upfrom 0)
                            (collect (if (listp c)
                                         (create-clang-asts c (cons i path))
                                         c))))))

     (destructuring-bind (root . children)
         (make-tree `((:class . :TopLevel)
                      (:counter . 0)
                      (:children . ,roots)
                      (:begin-off . 0)
                      (:end-off . :end)))
       (create-clang-asts (cons (from-alist 'clang-ast-node root)
                                children))))))

(defun types->hashtable (types)
  "Return a hashtable mapping type-hash -> type for each
type in TYPES.
* TYPES list of source types
"
  (iter (for type in types)
        (with hashtable = (make-hash-table :test #'equal))
        (setf (gethash (type-hash type) hashtable) type)
        (finally (return hashtable))))

(defmethod parse-asts ((obj clang))
  "Parse the genome of OBJ and update its ASTs.
* OBJ object to update
"
  (clang-mutate obj
                (list* :sexp
                       (cons :fields
                             (append *clang-ast-aux-fields*
                                     *clang-json-required-fields*))
                       (cons :aux *clang-json-required-aux*)
                       *clang-mutate-additional-args*)))

(defmethod update-asts ((obj clang))
  "Parse and return the ASTs in OBJ using `clang-mutate'.
* OBJ object to parse
"
  (with-slots (ast-root macros types genome) obj
    (unless genome     ; get genome from existing ASTs if necessary
      (setf genome (genome obj)
            ast-root nil))

    ;; Incorporate ASTs.
    (iter (for ast in (restart-case
                          (parse-asts obj)
                        (nullify-asts ()
                          :report "Nullify the clang software object."
                          nil)))
          (cond ((and (aget :hash ast)
                      (aget :name ast)
                      (aget :storage-class ast))
                  ;; Types
                  (collect (from-alist 'clang-type ast) into m-types))
                ((and (aget :name ast)
                      (aget :body ast)
                      (aget :hash ast))
                 ;; Macros
                 (collect (from-alist 'clang-macro ast) into m-macros))
                 ;; ASTs
                ((aget :counter ast)
                 (collect ast into body))
                (t (error "Unrecognized ast.~%~S" ast)))
          (finally
           (setf ast-root (fix-semicolons (asts->tree genome body))
                 types (types->hashtable m-types)
                 macros m-macros
                 genome nil))))

  obj)

(defmethod update-caches ((obj clang-base))
  "Update cached fields of OBJ, including `asts', `stmt-asts', `non-stmt-asts',
`functions', and `prototypes', return OBJ
* OBJ object to update caches for
"
  (call-next-method)

  (with-slots (asts stmt-asts non-stmt-asts functions prototypes) obj

    (iter (for ast in asts)
          (with last-proto = nil)
          (when (function-decl-p ast)
            (collect ast into protos)
            (when (function-body obj ast)
              (collect ast into funs))
            (setf last-proto ast))
          ;; stmt-asts are only collected in function bodies and
          ;; non-stmt-asts are only collected outside of function bodies
          (if (and last-proto (starts-with-subseq (ast-path last-proto)
                                                  (ast-path ast)))
              (unless (or (eq :ParmVar (ast-class ast))
                          (function-decl-p ast))
                (collect ast into my-stmts))
              (collect ast into my-non-stmts))

          (finally
           (setf stmt-asts my-stmts
                 non-stmt-asts my-non-stmts
                 functions funs
                 prototypes protos))))
  obj)

(defmethod update-caches ((obj clang))
  "Update the cached `includes` field on OBJ
* OBJ object to update caches for
"
  (call-next-method)
  (iter (for ast in (slot-value obj 'asts))
        (mapc (lambda (include)
                (adjoining include into m-includes test #'string=))
              (ast-includes ast))
        (finally (setf (slot-value obj 'includes) m-includes))))

(defmethod clear-caches ((obj clang-base))
  "Clear cached fields on OBJ, including `stmt-asts', `non-stmt-asts',
`functions', `prototypes', and `includes'.
* OBJ object to clear caches for.
"
  (with-slots (stmt-asts non-stmt-asts functions prototypes includes) obj
    (setf stmt-asts nil
          non-stmt-asts nil
          functions nil
          prototypes nil
          includes nil))
  (call-next-method))

(defmethod (setf genome) :before (new (obj clang-base))
  "Clear types and macros prior to updating the NEW genome."
  (declare (ignorable new))
  (with-slots (types macros) obj
    (setf types (make-hash-table :test 'equal)
          macros nil)))

(defmethod        macros :before ((obj clang-base))
  "Ensure the `macros' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defmethod      includes :before ((obj clang-base))
  "Ensure the `includes' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defmethod         types :before ((obj clang-base))
  "Ensure the `types' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defmethod     stmt-asts :before ((obj clang-base))
  "Ensure the `stmt-asts' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defmethod non-stmt-asts :before ((obj clang-base))
  "Ensure the `non-stmt-asts' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defmethod     functions :before ((obj clang-base))
  "Ensure the `functions' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defmethod    prototypes :before ((obj clang-base))
  "Ensure the `prototypes' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defmethod recontextualize ((clang clang-base) (ast ast) (pt ast))
  "Bind free variables and function in AST to concrete values
required for successful mutation in CLANG at PT
* CLANG object to be mutated
* AST node to be mutated into CLANG
* PT node where mutation is to occur
"
  (bind-free-vars clang ast pt))

(defmethod get-parent-decls ((clang clang-base) ast)
  "Return parents of AST in CLANG which are decl ASTs.
* CLANG software object to query
* AST ast to begin query from
"
  (remove-if-not #'ast-is-decl (get-parent-asts clang ast)))

(defmethod good-stmts ((clang clang-base))
  "Return a list of all good statement ASTs in CLANG."
  (stmt-asts clang))

(defmethod bad-stmts ((clang clang-base))
  "Return a list of all bad statement ASTs in CLANG."
  (stmt-asts clang))

(defmethod pick-good ((clang clang-base))
  "Pick a random AST in CLANG from the `good-stmt' pool."
  (random-elt (good-mutation-targets clang)))

(defmethod pick-bad ((clang clang-base))
  "Pick a random AST in CLANG from the `bad-stmt' pool."
  (random-elt (bad-mutation-targets clang)))

(defmethod pick-bad-good ((clang clang-base) &key filter
                          (bad-pool #'bad-stmts) (good-pool #'good-stmts))
  "Pick two ASTs from CLANG, both from the `bad-asts' pool,
excluding those ASTs removed by FILTER.
* CLANG object to perform picks for
* FILTER function taking two AST parameters and returning non-nil if the
second should be included as a possible pick
* BAD-POOL function returning a pool of 'bad' ASTs in SOFTWARE
* GOOD-POOL function returning a pool of 'good' ASTs in SOFTWARE
"
  (call-next-method clang
                    :filter filter
                    :bad-pool bad-pool
                    :good-pool good-pool))

(defmethod pick-bad-bad ((clang clang-base) &key filter (bad-pool #'bad-stmts))
  "Pick two ASTs from CLANG, both from the `bad-asts' pool,
excluding those ASTs removed by FILTER.
* CLANG object to perform picks for
* FILTER function taking two AST parameters and returning non-nil if the
second should be included as a possible pick
* BAD-POOL function returning a pool of 'bad' ASTs in SOFTWARE
"
  (call-next-method clang :filter filter :bad-pool bad-pool))

(defmethod pick-bad-only ((clang clang-base) &key filter
                          (bad-pool #'bad-stmts))
  "Pick a single AST from CLANG from `bad-pool',
excluding those ASTs removed by FILTER.
* CLANG object to perform picks for
* FILTER function taking two AST parameters and returning non-nil if the
second should be included as a possible pick
* BAD-POOL function returning a pool of 'bad' ASTs in SOFTWARE
"
  (call-next-method clang :filter filter :bad-pool bad-pool))

(defmethod good-mutation-targets ((clang clang-base) &key filter)
  "Return a list of all good statement ASTs in CLANG matching FILTER.
* CLANG software object to query for good statements
* FILTER predicate taking an AST parameter to allow for filtering
"
  (mutation-targets clang :filter filter :stmt-pool #'good-stmts))

(defmethod bad-mutation-targets ((clang clang-base) &key filter)
  "Return a list of all bad statement ASTs in CLANG matching FILTER.
* CLANG software object to query for bad statements
* FILTER predicate taking an AST parameter to allow for filtering
"
  (mutation-targets clang :filter filter :stmt-pool #'bad-stmts))

(defmethod mutation-targets ((clang clang-base) &key (filter nil)
                                                  (stmt-pool #'stmt-asts))
  "Return a list of target ASTs from STMT-POOL for mutation, throwing
a 'no-mutation-targets exception if none are available.

* CLANG software object to query for mutation targets
* FILTER filter AST from consideration when this function returns nil
* STMT-POOL method on CLANG returning a list of ASTs"
  (call-next-method clang :filter filter :stmt-pool stmt-pool))

(defvar *free-var-decay-rate* 0.3
  "The decay rate for choosing variable bindings.")

(defvar *matching-free-var-retains-name-bias* 0.75
  "The probability that if a free variable's original name matches a name
already in scope, it will keep that name.")

(defvar *matching-free-function-retains-name-bias* 0.95
  "The probability that if a free functions's original name matches a name
already in scope, it will keep that name.")

(defvar *clang-mutation-types*
  (cumulative-distribution
   (normalize-probabilities
    '((cut-decl                .  5)    ; All values are /100 total.
      (swap-decls              .  5)
      (rename-variable         .  5)
      (clang-promote-guarded   .  2)
      (explode-for-loop        .  1)
      (coalesce-while-loop     .  1)
      (expand-arithmatic-op    .  1)
      (clang-cut               .  5)
      (clang-cut-full          . 15)
      (clang-insert            .  1)
      (clang-insert-same       .  4)
      (clang-insert-full       .  4)
      (clang-insert-full-same  . 11)
      (clang-swap              .  1)
      (clang-swap-same         .  4)
      (clang-swap-full         .  4)
      (clang-swap-full-same    .  6)
      (clang-move              .  5)
      (clang-replace           .  1)
      (clang-replace-same      .  4)
      (clang-replace-full      .  4)
      (clang-replace-full-same . 11))))
  "Cumulative distribution of normalized probabilities of weighted mutations.")

(defmethod pick-mutation-type ((obj clang-base))
  "Select type of mutation to apply to OBJ."
  (random-pick *clang-mutation-types*))

(defmethod mutate ((clang clang-base))
  "Select a random mutation and mutate CLANG."
  (unless (stmt-asts clang)
    (error (make-condition 'mutate :text "No valid statements" :obj clang)))
  (let ((mutation (make-instance (pick-mutation-type clang) :object clang)))
    (apply-mutation clang mutation)
    (values clang mutation)))

(defun apply-clang-mutate-ops (software ops &aux (tu 0))
  "Run clang-mutate with a list of mutation operations, and update the genome."
  ;; If we multiplex multiple software objects onto one clang-mutate
  ;; invocation, they will need to track their own TU ids.  With one
  ;; software object, it will always be TU 0.
  (setf (genome software)
        (clang-mutate software '(:scripted) :script
                      (format nil "reset ~a; ~{~a; ~}preview ~a"
                              tu
                              (mapcar {mutation-op-to-cmd tu} ops)
                              tu)))
  software)

(defmethod apply-mutation ((software clang-base)
                           (mutation clang-mutation))
  "Apply MUTATION to SOFTWARE, returning the resulting SOFTWARE.
* SOFTWARE object to be mutated
* MUTATION mutation to be performed
"
  (restart-case
      (apply-mutation-ops software
                          ;; Sort operations latest-first so they
                          ;; won't step on each other.
                          (sort (recontextualize-mutation software mutation)
                                #'ast-later-p :key [{aget :stmt1} #'cdr]))
    (skip-mutation ()
      :report "Skip mutation and return nil"
      (values nil 1))
    (retry-mutation ()
      :report "Retry the mutation"
      (apply-mutation software mutation))
    (tidy ()
      :report "Call clang-tidy before re-attempting mutation"
      (clang-tidy software)
      (apply-mutation software mutation))
    (mutate ()
      :report "Apply another mutation before re-attempting mutations"
      (mutate software)
      (apply-mutation software mutation))))

(defmethod mutation-key ((obj clang-base) op)
  "Return key used to organize mutations in *mutation-stats* hashtable.
* OBJ object mutation is to be applied to
* OP operation to be performed
"
  ;; Return a list of the mutation type, and the classes of any stmt1 or
  ;; stmt2 arguments.
  (cons
   (type-of op)
   (mapcar [#'ast-class {get-ast obj} #'cdr]
           (remove-if-not [#'numberp #'cdr]
                          (remove-if-not [{member _ (list :stmt1 :stmt2)} #'car]
                                         (remove-if-not #'consp (targets op)))))))

(defun mutation-op-to-cmd (tu op)
  "Translate OP to be performed on the translate unit TU to an argument for
`clang-mutate'.
* TU translation unit to be mutated
* OP operation to be performed
"
  (labels ((ast (tag) (format nil "~a.~a" tu (aget tag (cdr op))))
           (str (tag) (encode-json-to-string (aget tag (cdr op)))))
    (ecase (car op)
      (:cut
       (format nil "cut ~a" (ast :stmt1)))
      (:insert
       (format nil "get ~a as $stmt; before ~a $stmt"
               (ast :stmt1) (ast :stmt2)))
      (:insert-value
       (format nil "before ~a ~a" (ast :stmt1) (str :value1)))
      (:insert-value-after
       (format nil "after ~a ~a" (ast :stmt1) (str :value1)))
      (:swap
       (format nil "swap ~a ~a" (ast :stmt1) (ast :stmt2)))
      (:set
       (format nil "set ~a ~a" (ast :stmt1) (str :value1)))
      (:set2
       (format nil "set ~a ~a ~a ~a"
               (ast :stmt1) (str :value1)
               (ast :stmt2) (str :value2)))
      (:set-range
       (format nil "set-range ~a ~a ~a"
               (ast :stmt1) (ast :stmt2) (str :value1)))
      (:set-func
       (format nil "set-func ~a ~a" (ast :stmt1) (str :value1)))
      (:ids
       (format nil "ids ~a" tu))
      (:list
       (format nil "list ~a" tu))
      (:sexp
       (let ((aux (if (aget :aux (cdr op))
                      (format nil "aux=~{~a~^,~}" (aget :aux (cdr op)))
                      ""))
             (fields (if (aget :fields (cdr op))
                         (format nil "fields=~{~a~^,~}" (aget :fields (cdr op)))
                         "")))
         (if (aget :stmt1 (cdr op))
             (format nil "ast ~a ~a" (ast :stmt1) fields)
             (format nil "sexp ~a ~a ~a" (ast :stmt1) fields aux)))))))

;;; This readtable tweak causes SBCL to read strings as base strings
;;; when possible.  This can greatly reduce memory usage.
;;; TODO: determine if this should be generalized to all software.
(defparameter *clang-mutate-readtable*
  (let ((rt (copy-readtable nil)))
    #+sbcl (setf (sb-ext:readtable-base-char-preference rt) :both)
    rt))

(defmethod clang-mutate ((obj clang-base) op
                         &key script
                         &aux value1-file value2-file)
  "DOCFIXME
* OBJ Clang software object
* OP List of (Action . Options) for the clang-mutate command line.
* SCRIPT DOCFIXME
* VALUE1-FILE DOCFIXME
* VALUE2-FILE DOCFIXME
"
  (assert (ext obj) (obj)
          "Software object ~a has no extension, required by clang-mutate."
          obj)
  (with-temp-file-of (src-file (ext obj)) (genome obj)
    (labels ((command-opt (command)
               (ecase command
                 (:cut "-cut")
                 (:insert "-insert")
                 (:insert-value "-insert-value")
                 (:swap "-swap")
                 (:set "-set")
                 (:set2 "-set2")
                 (:set-range "-set-range")
                 (:set-func  "-set-func")
                 (:ids "-ids")
                 (:list "-list")
                 (:sexp "-sexp")
                 (:scripted "-interactive -silent")))
             (option-opt (pair)
               (let ((option (car pair))
                     (value (cdr pair)))
                 (ecase option
                   (:stmt1 (format nil "-stmt1=~d" value))
                   (:stmt2 (format nil "-stmt2=~d" value))
                   (:fields (format nil "-fields=~a"
                                    (mapconcat #'field-opt value ",")))
                   (:aux (format nil "-aux=~a"
                                 (mapconcat #'aux-opt value ",")))
                   (:value1
                    (setf value1-file (temp-file-name))
                    (string-to-file value value1-file)
                    (format nil "-file1=~a" value1-file))
                   (:value2
                    (setf value2-file (temp-file-name))
                    (string-to-file value value2-file)
                    (format nil "-file2=~a" value2-file))
                   (:bin (format nil "-binary=~a" value))
                   (:dwarf-src-file-path
                    (format nil "-dwarf-filepath-mapping=~a=~a"
                            value src-file))
                   (:cfg "-cfg")
                   (:build-path (format nil "-p=~a" value)))))
             (field-opt (field)
               (ecase field
                 (:counter "counter")
                 (:declares "declares")
                 (:is-decl "is_decl")
                 (:parent-counter "parent_counter")
                 (:class "class")
                 (:src-file-name "src_file_name")
                 (:begin-src-line "begin_src_line")
                 (:begin-src-col "begin_src_col")
                 (:end-src-line "end_src_line")
                 (:end-src-col "end_src_col")
                 (:src-text "src_text")
                 (:guard-stmt "guard_stmt")
                 (:full-stmt "full_stmt")
                 (:unbound-vals "unbound_vals")
                 (:unbound-funs "unbound_funs")
                 (:macros "macros")
                 (:types "types")
                 (:stmt-list "stmt_list")
                 (:binary-file-path "binary_file_path")
                 (:scopes "scopes")
                 (:begin-addr "begin_addr")
                 (:end-addr "end_addr")
                 (:includes "includes")
                 (:opcode "opcode")
                 (:children "children")
                 (:successors "successors")
                 (:begin-off "begin_off")
                 (:end-off "end_off")
                 (:begin-norm-off "begin_norm_off")
                 (:end-norm-off "end_norm_off")
                 (:orig-text "orig_text")
                 (:binary-contents "binary_contents")
                 (:in-macro-expansion "in_macro_expansion")
                 (:expr-type "expr_type")
                 (:syn-ctx "syn_ctx")
                 (:size "size")))
             (aux-opt (aux)
               (ecase aux
                 (:types "types")
                 (:asts "asts")
                 (:decls "decls")
                 (:macros "macros")
                 (:none "none"))))
    (let ((*identifier-name-to-key* 'se-json-identifier-name-to-key)
          (cmd-fmt "clang-mutate ~a ~{~a~^ ~} ~a -- ~{~a~^ ~}"))
      (unwind-protect
           (multiple-value-bind (stdout stderr exit)
            (shell cmd-fmt
                   (command-opt (car op))
                   (mapcar #'option-opt (cdr op))
                   src-file
                   (flags obj)
                   :input script)
          ;; NOTE: The clang-mutate executable will sometimes produce
          ;;       usable output even on a non-zero exit, e.g., usable
          ;;       json or successful mutations but an exit of 1
          ;;       because of compiler errors.  To ensure these cases
          ;;       are still usable, we only signal mutation errors on
          ;;       specific exit values.
          (when (find exit '(131 132 134 136 139))
            (error
             (make-condition 'mutate
               :text (format nil "clang-mutate core dump with ~d, ~s"
                             exit stderr)
               :obj obj :op op)))
          (restart-case
              (unless (zerop exit)
                (error
                 (make-condition 'mutate
                   :text (format nil "clang-mutate exit ~d~%cmd:~s~%stderr:~s"
                                 exit
                                 (format nil cmd-fmt
                                         (command-opt (car op))
                                         (mapcar #'option-opt (cdr op))
                                         src-file
                                         (flags obj))
                                 stderr)
                   :obj obj :op op)))
            (keep-partial-asts ()
              :report "Ignore error retaining partial ASTs for software object."
              nil))
          ;; NOTE: If clang-mutate output exceeds 10 MB, this is likely due
          ;; to an insertion which is technically legal via the standard,
          ;; but is actually meaningless.  This tends to happen with array
          ;; initialization forms (e.g { 254, 255, 256 ... }) being inserted
          ;; and interpreted as a block.  Throw an error to clear the genome.
          (when (> (length stdout) *clang-max-json-size*)
            (error (make-condition 'mutate
                     :text (format nil "clang-mutate output exceeds ~a MB."
                                   (floor (/ *clang-max-json-size*
                                             1048576)))
                     :obj obj :op op)))
          (values
           (case (car op)
             (:sexp (let ((*readtable* *clang-mutate-readtable*))
                      (read-from-string stdout)))
             (t stdout))
           exit))
        ;; Cleanup forms.
        (when (and value1-file (probe-file value1-file))
        (delete-file value1-file))
      (when (and value2-file (probe-file value2-file))
        (delete-file value2-file)))))))

(defmethod fixup-mutation (operation (current clang-ast)
                           before ast after)
  (clang-fixup-mutation operation current before ast after))

(defun clang-fixup-mutation (operation current before ast after)
  "Adjust mutation result according to syntactic context.

Adds and removes semicolons, commas, and braces.

* OPERATION mutation operation performed (:cut, :set, :insert,
:insert-after, :splice)
* CURRENT AST node to be replaced
* BEFORE string or ast prior to the mutation point
* AST replacement ast in the mutation operation
* AFTER string or ast following the mutation point
"
  (when ast
    (setf ast (copy ast :syn-ctx (ast-syn-ctx current)
                    :full-stmt (ast-full-stmt current))))
  (labels
      ((no-change ()
         (list before ast after))
       (add-semicolon-if-unbraced ()
         (if (or (null ast) (ends-with "};"
				       (trim-whitespace (source-text ast))
				       :test #'find))
             (if (and (stringp after) (starts-with #\; (trim-whitespace after)))
                 (list before ast (subseq after (1+ (position #\; after))))
                 (no-change))
             (add-semicolon)))
       (add-semicolon-before-if-unbraced ()
         (if (or (null ast)
                 (starts-with #\{ (trim-whitespace (source-text ast)))
		 (and before (ends-with #\; (trim-whitespace (source-text before)))))
             (no-change)
             (list before ";" ast after)))
       (add-semicolon ()
         (if (or (ends-with #\; (trim-whitespace (source-text ast)))
                 (starts-with #\; (trim-whitespace (source-text after))))
             (no-change)
             (list before ast ";" after)))
       (add-comma ()
         (list before ast "," after))
       (add-leading-comma ()
         (list before "," ast after))
       (wrap-with-block-if-unbraced ()
         ;; Wrap in a CompoundStmt and also add semicolon -- this
         ;; never hurts and is sometimes necessary (e.g. for loop
         ;; bodies).
         (let ((text (trim-whitespace (source-text ast))))
           (if (and (starts-with #\{ text) (ends-with #\} text))
               (no-change)
               (list before (make-block (list ast ";"))
                     after))))
       (add-null-stmt ()
         ;; Note: clang mutate will generate a NullStmt with ";" as
         ;; its text, but here the semicolon already exists in a
         ;; parent AST.
         (list before
               (make-statement :NullStmt :unbracedbody nil)))
       (add-null-stmt-and-semicolon ()
         (list before
               (make-statement :NullStmt :unbracedbody '(";")))))
    (remove nil
            (ecase (ast-syn-ctx current)
              (:generic (no-change))
              (:fullstmt (ecase operation
                           (:before (add-semicolon-if-unbraced))
                           (:instead (add-semicolon-if-unbraced))
                           (:remove (add-semicolon-if-unbraced))
                           (:after (add-semicolon-before-if-unbraced))))
              (:listelt (ecase operation
                          (:before (add-comma))
                          (:after (add-comma))
                          (:instead (no-change))
                          (:remove (list before
                                         (if (starts-with #\, after)
                                             (subseq after 1)
                                             after)))))
              (:finallistelt (ecase operation
                               (:before (add-comma))
                               (:after (add-leading-comma))
                               (:instead (no-change))
                               (:remove (list after))))
              (:braced
               (ecase operation
                         (:before (no-change))
                         (:after (add-semicolon-if-unbraced))
                         ;; When cutting a free-floating block, we don't need a
                         ;; semicolon, but it's harmless. When cutting a braced
                         ;; loop/function body, we do need the semicolon. Since
                         ;; we can't easily distinguish these case, always add
                         ;; the semicolon.
                         (:remove (add-null-stmt-and-semicolon))
                         (:instead (wrap-with-block-if-unbraced))))
              (:unbracedbody
               (ecase operation
                 (:before (add-semicolon-if-unbraced))
                 (:after (no-change))
                 (:remove (add-null-stmt))
                 (:instead (add-semicolon-if-unbraced))))
              (:field (ecase operation
                        (:before (add-semicolon))
                        (:after (add-semicolon))
                        (:instead (add-semicolon))
                        (:remove (no-change))))
              (:toplevel (add-semicolon-if-unbraced))))))

(defmethod ends-with-semicolon ((str string))
  (let ((trimmed (trim-right-whitespace str)))
    (when (ends-with #\; trimmed)
      (subseq trimmed 0 (1- (length trimmed))))))

(defmethod ends-with-semicolon (obj)
  (declare (ignorable obj))
  nil)

(defgeneric remove-semicolon (obj)
  (:documentation "Removes the trailing semicolon from an AST, returning a new AST node.
If there is no trailing semicolon, return the AST unchanged."))

(defmethod remove-semicolon ((obj ast))
  (let* ((children (ast-children obj))
         (last (lastcar children)))
    (if-let ((trimmed (ends-with-semicolon last)))
            (copy obj :children (append (butlast children) (list trimmed)))
            obj)))

(defmethod remove-semicolon ((obj string))
  (or (ends-with-semicolon obj) obj))

(defmethod remove-semicolon ((obj null)) nil)


;;; AST Utility functions
(defgeneric function-decl-p (ast)
  (:documentation
   "Is AST a function (or method/constructor/destructor) decl?")
  (:method ((ast ast))
    (member (ast-class ast)
            '(:Function :CXXMethod :CXXConstructor :CXXDestructor)))
  (:method (x) (declare (ignorable x)) nil))

(defgeneric function-body (software ast)
  (:documentation
   "If AST is a function, return the AST representing its body.
* SOFTWARE software object containing AST and its children
* AST potential function AST to query for its body
")
  (:method ((software clang-base) (ast ast))
    (when (function-decl-p ast)
      (find-if [{eq :CompoundStmt} #'ast-class]
               (get-immediate-children software ast)))))

(defgeneric get-parent-full-stmt (software ast)
  (:documentation
   "Return the first ancestor of AST in SOFTWARE which is a full stmt.
Returns nil if no full-stmt parent is found."))

(defmethod get-parent-full-stmt ((clang clang-base) (ast clang-ast-base))
  "Return the first ancestor of AST in SOFTWARE which is a full stmt.
Returns nil if no full-stmt is found.
* CLANG software object containing AST and its parents
* AST to find the parent full statement of if not already a full statement
"
  (cond ((ast-full-stmt ast) ast)
        (ast (get-parent-full-stmt clang (get-parent-ast clang ast)))))

(defmethod stmt-range ((software clang-base) (function ast))
  "DOCFIXME
* SOFTWARE DOCFIXME
* FUNCTION DOCFIXME
"
  (labels
      ((rightmost-child (ast)
         (if-let ((children (get-immediate-children software ast)))
           (rightmost-child (lastcar children))
           ast)))
    (when-let ((body (function-body software function)))
      (mapcar {index-of-ast software}
              (list body (rightmost-child body))))))

(defgeneric wrap-ast (software ast)
  (:documentation "Wrap AST in SOFTWARE in a compound statement.
Known issue with ifdefs -- consider this snippet:

    if (x) {
      var=1;
    #ifdef SOMETHING
    } else if (y) {
      var=2;
    #endif
    }

it will transform this into:

    if (x) {
      var=1;
    #ifdef SOMETHING
    } else {
        if (y) {
          var=2;
    #endif
        }  // spurious -- now won't compile.
    }"))

(defmethod wrap-ast ((obj clang-base) (ast ast))
  "DOCFIXME
* OBJ DOCFIXME
* AST DOCFIXME
"
  (apply-mutation obj
                  `(clang-replace (:stmt1 . ,ast)
                                  (:literal1 . ,(make-block (list ast ";")))))
  obj)

(define-constant +clang-wrapable-parents+
    '(:WhileStmt :IfStmt :ForStmt :DoStmt :CXXForRangeStmt)
  :test #'equalp
  :documentation "Types which can be wrapped.")

(defgeneric wrap-child (software ast index)
  (:documentation "Wrap INDEX child of AST in SOFTWARE in a compound stmt."))

(defmethod wrap-child ((obj clang-base) (ast ast) (index integer))
  "DOCFIXME
* OBJ DOCFIXME
* AST DOCFIXME
* INDEX DOCFIXME
"
  (if (member (ast-class ast) +clang-wrapable-parents+)
      (wrap-ast obj (nth index (get-immediate-children obj ast)))
      (error "Will not wrap children of type ~a, only useful for ~a."
             (ast-class ast) +clang-wrapable-parents+))
  obj)

(defmethod can-be-made-traceable-p ((obj clang-base) (ast ast))
  "DOCFIXME
* OBJ DOCFIXME
* AST DOCFIXME
"
  (or (traceable-stmt-p obj ast)
      (unless (or (ast-guard-stmt ast) ; Don't wrap guard statements.
                  (eq :CompoundStmt ; Don't wrap CompoundStmts.
                      (ast-class ast)))
        (when-let ((parent (get-parent-ast obj ast)))
          ;; Is a child of a statement which might have a hanging body.
          (member (ast-class parent) +clang-wrapable-parents+)))))

(defmethod enclosing-traceable-stmt ((obj clang-base) (ast ast))
  "DOCFIXME
* OBJ DOCFIXME
* AST DOCFIXME
"
  (cond
    ((traceable-stmt-p obj ast) ast)
    ;; Wrap AST in a CompoundStmt to make it traceable.
    ((can-be-made-traceable-p obj ast) ast)
    (:otherwise
     (some->> (get-parent-ast obj ast)
              (enclosing-traceable-stmt obj)))))

(defmethod traceable-stmt-p ((obj clang-base) (ast ast))
  "DOCFIXME
* OBJ DOCFIXME
* AST DOCFIXME
"
  (and (ast-full-stmt ast)
       (not (function-decl-p ast))
       (not (ast-in-macro-expansion ast))
       (not (eq :NullStmt (ast-class ast)))
       (get-parent-ast obj ast)
       (eq :CompoundStmt (ast-class (get-parent-ast obj ast)))))

(defmethod nesting-depth ((clang clang-base) stmt &optional orig-depth)
  "DOCFIXME
* CLANG DOCFIXME
* STMT DOCFIXME
* ORIG-DEPTH DOCFIXME
"
  (let ((depth (or orig-depth 0)))
    (if (null stmt)
        depth
        (nesting-depth clang (enclosing-block clang stmt) (1+ depth)))))

(defmethod enclosing-block ((clang clang-base) (ast ast))
  "DOCFIXME
* CLANG DOCFIXME
* AST DOCFIXME
"
  ;; First parent AST is self, skip over that.
  (find-if {block-p clang} (cdr (get-parent-asts clang ast))))

(defgeneric full-stmt-p (software statement)
  (:documentation "Check if STATEMENT is a full statement in SOFTWARE."))

(defmethod full-stmt-p ((obj clang-base) (stmt ast))
  "Check if STMT is a full statement in clang software OBJ.
This may depend on context."
  (declare (ignorable obj))
  (ast-full-stmt stmt))

(defgeneric guard-stmt-p (software statement)
  (:documentation "Check if STATEMENT is a guard statement in SOFTWARE."))

(defmethod guard-stmt-p ((obj clang-base) (stmt ast))
  "DOCFIXME
* SOFTWARE DOCFIXME
* STATEMENT DOCFIXME
"
  (declare (ignorable obj))
  (ast-guard-stmt stmt))

(defgeneric block-p (software statement)
  (:documentation "Check if STATEMENT is a block in SOFTWARE."))

(defmethod block-p ((obj clang-base) (stmt ast))
  "DOCFIXME
* OBJ DOCFIXME
* STMT DOCFIXME
"
  (or (eq :CompoundStmt (ast-class stmt))
      (and (member (ast-class stmt) +clang-wrapable-parents+)
           (not (null (->> (get-immediate-children obj stmt)
                           (remove-if «or {guard-stmt-p obj}
                                          [{eq :CompoundStmt}
                                           #'ast-class]»)))))))

(defgeneric enclosing-full-stmt (software stmt)
  (:documentation
   "Return the first full statement in SOFTWARE holding STMT."))

(defmethod enclosing-full-stmt ((obj clang-base) (stmt ast))
  "DOCFIXME
* OBJ DOCFIXME
* STMT DOCFIXME
"
  (find-if #'ast-full-stmt (get-parent-asts obj stmt)))

(defun get-entry-after (item list)
  "DOCFIXME
* ITEM DOCFIXME
* LIST DOCFIXME
"
  (cond ((null list) nil)
        ((not (equalp (car list) item)) (get-entry-after item (cdr list)))
        ((null (cdr list)) nil)
        (t (cadr list))))

(defun get-entry-before (item list &optional saw)
  "DOCFIXME
* ITEM DOCFIXME
* LIST DOCFIXME
* SAW DOCFIXME
"
  (cond ((null list) nil)
        ((equalp (car list) item) saw)
        (t (get-entry-before item (cdr list) (car list)))))

(defmethod block-successor ((clang clang-base) ast)
  "DOCFIXME
* CLANG DOCFIXME
* AST DOCFIXME
"
  (let* ((full-stmt (enclosing-full-stmt clang ast))
         (the-block (enclosing-block clang full-stmt))
         (the-stmts (remove-if-not «or {block-p clang}
                                       {full-stmt-p clang}»
                                   (get-immediate-children clang the-block))))
    (get-entry-after full-stmt the-stmts)))

(defmethod block-predeccessor ((clang clang-base) ast)
  "DOCFIXME
* CLANG DOCFIXME
* AST DOCFIXME
"
  (let* ((full-stmt (enclosing-full-stmt clang ast))
         (the-block (enclosing-block clang full-stmt))
         (the-stmts (remove-if-not «or {block-p clang}
                                       {full-stmt-p clang}»
                                   (get-immediate-children clang the-block))))
    (get-entry-before full-stmt the-stmts)))

(defmethod full-stmt-predecessors ((clang clang-base) ast &optional acc blocks)
  "All full statements and blocks preceeding AST.

Predecessors are listed starting from the beginning of the containing
function, and grouped by nesting level. The last statement of each
sublist is the parent of the statements in the next sublist.

Ends with AST.
"

  (if (not (ast-full-stmt ast))
      ;; Reached a non-full statement. Go up to the enclosing
      ;; statement.
      (full-stmt-predecessors clang
                              (enclosing-full-stmt clang ast)
                              nil
                              (cons (cons ast acc)
                                    blocks))
      (if (null (enclosing-block clang ast))
          ;; We've made it to the top-level scope; return the accumulator
          (if (null acc)
              blocks
              (cons acc blocks))
          ;; Not at top level yet
          (let ((prev-stmt (block-predeccessor clang ast))
                (new-acc (cons ast acc)))
            (if prev-stmt
                ;; Middle of block. Accumulate and keep going.
                (full-stmt-predecessors clang
                                        prev-stmt
                                        new-acc
                                        blocks)
                ;; Last statement in block. Move up a scope and push
                ;; the accumulated statements onto the block stack.
                (full-stmt-predecessors clang
                                        (enclosing-block clang ast)
                                        nil
                                        (cons new-acc blocks)))))))

(defmethod tree-successors (ast ancestor &key include-ast)
  "Find all successors of AST within subtree at ANCESTOR.

Returns ASTs and text snippets, grouped by depth. AST itself is
included as the first successor."
  (labels
      ((successors (ast path)
         (bind (((head . tail) path)
                (children (ast-children ast)))
           (if tail
               (cons (subseq children (1+ head))
                     (successors (nth head children) tail))
               (list (subseq children (if include-ast head (1+ head))))))))
    (reverse (successors ancestor
                         (last (ast-path ast)
                               (- (length (ast-path ast))
                                  (length (ast-path ancestor))))))))

(defmethod update-headers-from-snippet ((clang clang-base) snippet database)
  "DOCFIXME
* CLANG DOCFIXME
* SNIPPET DOCFIXME
* DATABASE DOCFIXME
"
  (mapc {add-include clang}
        (reverse (aget :includes snippet)))
  (mapc [{add-macro clang} {find-macro database}]
        (reverse (aget :macros snippet)))
  (mapc [{add-type clang} {find-type database}]
        (reverse (aget :types snippet)))
  snippet)

(defmethod begins-scope ((ast clang-ast))
  "True if AST begins a new scope."
  (begins-scope* ast))

(defun begins-scope* (ast)
  (member (ast-class ast)
          '(:CompoundStmt :Block :Captured :Function :CXXMethod)))

(defmethod enclosing-scope ((software clang-base) (ast ast))
  "DOCFIXME
* SOFTWARE DOCFIXME
* AST DOCFIXME
"
  (or (find-if #'begins-scope
               (cdr (get-parent-asts software ast)))
      ;; Global scope
      (ast-root software)))

(defmethod nth-enclosing-scope ((software clang-base) depth (ast ast))
  "DOCFIXME
* SOFTWARE DOCFIXME
* DEPTH DOCFIXME
* AST DOCFIXME
"
  (let ((scope (enclosing-scope software ast)))
    (if (>= 0 depth) scope
        (nth-enclosing-scope software (1- depth) scope))))

(defmethod scopes ((software clang-base) (ast ast))
  "DOCFIXME
* SOFTWARE DOCFIXME
* AST DOCFIXME
"
  ;; Stop at the root AST
  (when (not (eq :TopLevel (ast-class ast)))
    (let ((scope (enclosing-scope software ast)))
      (cons (->> (iter (for c in
                            (get-immediate-children software scope))
                       (while (ast-later-p ast c))
                       (collect c))
                 ; expand decl statements
                 (mappend
                  (lambda (ast)
                    (cond ((eq :DeclStmt (ast-class ast))
                           (get-immediate-children software ast))
                          (t (list ast)))))
                 ; remove type and function decls
                 (remove-if-not [{member _ '(:Var :ParmVar)}
                                 #'ast-class])
                 ; build result
                 (mappend
                  (lambda (ast)
                    (mapcar
                     (lambda (name)
                       `((:name . ,name)
                         (:decl . ,ast)
                         (:type . ,(car (ast-types ast)))
                         (:scope . ,scope)))
                     (or (ast-declares ast)
                         (ast-unbound-vals ast)))))
                 ; drop nils and empty strings
                 (remove-if #'emptyp)
                 (reverse))
            (scopes software scope)))))

(defmethod get-ast-types ((software clang-base) (ast ast))
  "Compute all the types mentioned in AST.  AST-TYPES is
the types used at a node; this function closes over all the nodes
in the AST.  SOFTWARE is the software object to which AST belongs."
  (remove-duplicates (apply #'append (ast-types ast)
                            (mapcar {get-ast-types software}
                                    (get-immediate-children software ast)))))

(defmethod get-unbound-funs ((software clang-base) (ast ast))
  "Compute all the unbound funs in AST.   AST-UNBOUND-FUNS is
the unbound funs at a node; this function closes over all the nodes
in the AST.  SOFTWARE is the software object to which AST belongs."
  (remove-duplicates (apply #'append (ast-unbound-funs ast)
                            (mapcar {get-unbound-funs software}
                                    (get-immediate-children software ast)))
                     :test #'equal))

(defmethod get-unbound-funs ((software clang-base) ast)
  "DOCFIXME
* SOFTWARE DOCFIXME
* AST DOCFIXME
"
  (declare (ignorable software))
  (ast-unbound-funs ast))

(defmethod get-unbound-vals ((software clang-base) (ast ast))
  "DOCFIXME
* SOFTWARE DOCFIXME
* AST DOCFIXME
"
  (labels
      ((in-scope (var scopes)
         (some (lambda (s) (member var s :test #'name=))
               scopes))
       (walk-scope (ast unbound scopes)
         ;; Enter new scope
         (when (begins-scope ast)
           (push nil scopes))

         ;; Add definitions to scope
         (setf (car scopes)
               (append (ast-declarations ast) (car scopes)))

         ;; Find unbound values
         (iter (for name in (ast-unbound-vals ast))
               (unless (in-scope name scopes)
                 (push name unbound)))

         ;; Walk children
         (iter (for c in (get-immediate-children software ast))
               (multiple-value-bind (new-unbound new-scopes)
                   (walk-scope c unbound scopes)
                 (setf unbound new-unbound
                       scopes new-scopes)))

         ;; Exit scope
         (when (begins-scope ast)
           (setf scopes (cdr scopes)))

         (values unbound scopes)))
    ;; Walk this tree, finding all values which are referenced, but
    ;; not defined, within it
    (let ((in-scope (get-vars-in-scope software ast)))
      (-<>> (walk-scope ast nil (list nil))
            (remove-duplicates <> :test #'name=) ;; potential bad scaling
            (mapcar (lambda (name)
                      (or (find name in-scope :test #'name= :key {aget :name})
                          `((:name . ,name)))))))))

(defmethod get-unbound-vals ((software clang-base) ast)
  "DOCFIXME
* SOFTWARE DOCFIXME
* AST DOCFIXME
"
  (declare (ignorable software))
  (ast-unbound-vals ast))

(defun random-function-name (protos &key original-name arity)
  "DOCFIXME
* PROTOS DOCFIXME
* ORIGINAL-NAME DOCFIXME
* ARITY DOCFIXME
"
  (let ((matching '())
        (variadic '())
        (others   '())
        (saw-orig nil))
    (loop :for proto :in protos
       :do (let ((name (ast-name proto))
                 (args (length (ast-args proto))))
             (when (name= name original-name)
               (setf saw-orig t))
             (cond
               ((= args arity) (push name matching))
               ((and (< args arity)
                     (ast-varargs proto)) (push name variadic))
               (t (push name others)))))
    (if (and saw-orig (< (random 1.0) *matching-free-function-retains-name-bias*))
        original-name
        (random-elt (or matching variadic others '(nil))))))

(defun random-function-info (protos &key original-name arity)
  "Returns function info in the same format as unbound-funs.
* PROTOS DOCFIXME
* ORIGINAL-NAME DOCFIXME
* ARITY DOCFIXME
"
  (when-let* ((name (random-function-name protos
                                          :original-name original-name
                                          :arity arity))
              (decl (find-if [{name= name} #'ast-name] protos)))
    ;; fun is (name, voidp, variadicp, arity)
    (list (format nil "(|~a|)" (ast-name decl))
          nil ; (ast-void-ret decl)
          (ast-varargs decl)
          (length (ast-args decl)))))

(defun binding-for-var (obj in-scope name)
  "DOCFIXME
* OBJ DOCFIXME
* IN-SCOPE DOCFIXME
* NAME DOCFIXME
"
  ;; If the variable's original name matches the name of a variable in scope,
  ;; keep the original name with probability equal to
  ;; *matching-free-var-retains-name-bias*
  (or (when (and (< (random 1.0)
                    *matching-free-var-retains-name-bias*)
                 (find name in-scope :test #'name=))
        name)
      (random-elt-with-decay
       in-scope *free-var-decay-rate*)
      (error (make-condition 'mutate
                             :text "No bound vars in scope."
                             :obj obj))))

(defgeneric binding-for-function (obj functions name arity)
  (:documentation
  "DOCFIXME
* OBJ DOCFIXME
* FUNCTIONS DOCFIXME
* NAME DOCFIXME
* ARITY DOCFIXME
"))

(defmethod binding-for-function ((obj clang) functions name arity)
  (or (random-function-info functions
                            :original-name name
                            :arity arity)
      (error (make-condition 'mutate
                             :text "No funs found."
                             :obj obj))))

(defmethod bind-free-vars ((clang clang-base) (ast ast) (pt ast))
  "DOCFIXME
* CLANG DOCFIXME
* AST DOCFIXME
* PT DOCFIXME
"
  (let* ((in-scope (mapcar {aget :name} (get-vars-in-scope clang pt)))
         (var-replacements
          (mapcar (lambda (var)
                    (let ((name (aget :name var)))
                      (mapcar #'unpeel-bananas
                              (list name (binding-for-var clang in-scope
                                                          name)))))
                  (get-unbound-vals clang ast)))
         (fun-replacements
          (mapcar
           (lambda (fun)
             (list fun
                   (binding-for-function clang
                                         (prototypes clang)
                                         (first fun)
                                         (fourth fun))))
           (get-unbound-funs clang ast))))
    (values
     (rebind-vars ast var-replacements fun-replacements)
     var-replacements
     fun-replacements)))

(defmethod rebind-vars ((ast clang-ast)
                        var-replacements fun-replacements)
  "Replace variable and function references, returning a new AST.
* AST node to rebind variables and function references for
* VAR-REPLACEMENTS list of old-name, new-name pairs defining the rebinding
* FUN-REPLACEMENTS list of old-function-info, new-function-info pairs defining
the rebinding
"
  ;; var-replacements looks like:
  ;; ( (("(|old-name|)" "(|new-name|)") ... )
  ;; These name/depth pairs can come directly from ast-unbound-vals.

  ;; fun-replacements are similar, but the pairs are function info
  ;; lists taken from ast-unbound-funs.

  (make-clang-ast
    :path (ast-path ast)
    :node (copy (ast-node ast)
                :unbound-vals
                (unbound-vals-in-rebinding ast var-replacements))
    :children (mapcar {rebind-vars _ var-replacements fun-replacements}
                      (ast-children ast))))

(defgeneric unbound-vals-in-rebinding (ast var-replacements)
  (:documentation "Recomputation of UNBOUND-VALS in REBIND-VARS")
  (:method ((ast clang-ast) var-replacements)
    (remove-duplicates
     (mapcar (lambda (v)
               (or (some->> (find-if [{equal v} #'peel-bananas
                                      #'car]
                                     var-replacements)
                            (second)
                            (peel-bananas))
                   v))
             (ast-unbound-vals ast))
     :test #'equal)))

(defgeneric delete-decl-stmts (software block replacements)
  (:documentation
   "Return mutation ops applying REPLACEMENTS to BLOCK in SOFTWARE.
REPLACEMENTS is a list holding lists of an ID to replace, and the new
variables to replace use of the variables declared in stmt ID."))

(defmethod delete-decl-stmts ((obj clang-base) (block ast) (replacements list))
  "DOCFIXME
* OBJ DOCFIXME
* BLOCK DOCFIXME
* REPLACEMENTS DOCFIXME
"
  (append
   ;; Rewrite those stmts in the BLOCK which use an old variable.
   (let* ((old->new      ; First collect a map of old-name -> new-name.
           (mappend (lambda (pair)
                      (destructuring-bind (id . replacements) pair
                        (mapcar #'list
                                (mapcar #'unpeel-bananas (ast-declares id))
                                (mapcar #'unpeel-bananas replacements))))
                    replacements))
          (old (mapcar [#'peel-bananas #'car] old->new)))
     ;; Collect statements using old
     (-<>> (get-immediate-children obj block)
           (remove-if-not (lambda (ast)      ; Only Statements using old.
                            (intersection
                             (get-used-variables obj ast)
                             old :test #'name=)))
           (mapcar (lambda (ast)
                     (list :set (cons :stmt1 ast)
                           (cons :literal1
                                 (rebind-vars ast old->new nil)))))))
   ;; Remove the declaration.
   (mapcar [{list :cut} {cons :stmt1} #'car] replacements)))

(defmethod get-declared-variables ((clang clang-base) the-block)
  "DOCFIXME
* CLANG DOCFIXME
* THE-BLOCK DOCFIXME
"
  (mappend #'ast-declares (get-immediate-children clang the-block)))

(defmethod get-used-variables ((clang clang-base) stmt)
  "DOCFIXME
* CLANG DOCFIXME
* STMT DOCFIXME
"
  (mapcar {aget :name} (get-unbound-vals clang stmt)))

(defmethod get-children-using ((clang clang-base) var the-block)
  "DOCFIXME
* CLANG DOCFIXME
* VAR DOCFIXME
* THE-BLOCK DOCFIXME
"
  (remove-if-not [(lambda (el) (find var el :test #'equal))
                  {get-used-variables clang}]
                 (get-immediate-children clang the-block)))

(defmethod nth-enclosing-block ((clang clang-base) depth stmt)
  "DOCFIXME
* CLANG DOCFIXME
* DEPTH DOCFIXME
* STMT DOCFIXME
"
  (let ((the-block (enclosing-block clang stmt)))
    (if (>= 0 depth) the-block
        (nth-enclosing-block clang (1- depth) the-block))))

(defmethod ast-declarations ((ast clang-ast))
  "Names of variables or parameters declared by AST"
  (ast-declarations* ast))

(defun ast-declarations* (ast)
  (cond
    ((member (ast-class ast) '(:Var :ParmVar :DeclStmt)) ; Var or function arg
     (ast-declares ast))
    ((function-decl-p ast) ; Function declaration.
     (mapcar #'car (ast-args ast)))  ; Does not need the hash codes
    (:otherwise nil)))

(defmethod ast-var-declarations ((ast clang-ast))
  "Names of the variables that AST declares."
  (ast-var-declarations* ast))

(defun ast-var-declarations* (ast)
  (when (member (ast-class ast) '(:Var :ParmVar :DeclStmt))
    (ast-declares ast)))

;; These generalized comparison functions are so
;; we can use something other than strings as names
;; and something other than hash codes as types.
(defmethod name= ((n1 string) n2)
  (string= n1 (ast-name n2)))
(defmethod name= (n1 (n2 string))
  (string= (ast-name n1) n2))
(defmethod name= (n1 n2)
  (or (eql n1 n2)
      (string= (ast-name n1) (ast-name n2))))

(defun ast-arg-equal (arg1 arg2)
  (and (name= (first arg1) (first arg2))
       (equalp (second arg1) (second arg2))))

(defun ast-args-equal (args1 args2)
  "Compare two lists as returned by AST-ARGS"
  (and (= (length args1) (length args2))
       (every #'ast-arg-equal args1 args2)))


;;; Crossover functions
(defun create-crossover-context (clang outer start &key include-start)
  "Create the context for a crossover AST.

Start at the outer AST and proceed forward/inward, copying all
children before the start point of the crossover. This collects
everything within the outer AST that will not be replaced by the
crossover.

Returns a list of parent ASTs from outer to inner.
"
  (labels
      ((copy-predecessors (root statements)
         (if (eq root start)
             (values nil (when include-start
                           (list root)))
             (bind ((children (ast-children root))
                    ;; Last child at this level
                    (last-child (lastcar (car statements)))
                    ;; Position of last child in real AST child list
                    (last-index (position-if {equalp last-child}
                                             children))
                    ((:values stack new-child)
                     (copy-predecessors last-child (cdr statements)))
                    (new-ast (copy root
                                   :children
                                   ;; keep all but last, including text
                                   (append (subseq children 0 last-index)
                                           ;; copy last and update children
                                           new-child))))
               (values (cons new-ast stack) (list new-ast))))))

    (let ((predecessors (full-stmt-predecessors clang start)))
      (iter (until (or (some [{equalp outer} {get-parent-ast clang}]
                             (car predecessors))
                       (null predecessors)))
            (pop predecessors))
      (when predecessors
        (copy-predecessors outer predecessors)))))

(defun fill-crossover-context (context statements)
  "Fill in context with ASTs from the other genome.

Each element of CONTEXT is an incomplete AST which is missing some
trailing children. Each element of STATEMENTS is a corresponding list
of children.

Returns outermost AST of context.
"
  ;; Reverse context so we're proceeding from the innermost AST
  ;; outward. This ensures that the levels line up.
  (lastcar (iter (for parent in (reverse context))
                 (for children in statements)
                 (collect (copy parent :children (nconcf (ast-children parent)
                                                         children))))))

;; Perform 2-point crossover. The second point will be within the same
;; function as the first point, but may be in an enclosing scope.
;; The number of scopes exited as you go from the first crossover point
;; to the second crossover point will be matched between a and b.
;; Free variables are rebound in such a way as to ensure that they are
;; bound to variables that are declared at each point of use.
;;
;; Modifies parameter A.
;;
(defmethod crossover-2pt-outward
    ((a clang-base) (b clang-base) a-begin a-end b-begin b-end)
  "DOCFIXME
* A DOCFIXME
* B DOCFIXME
* A-BEGIN DOCFIXME
* A-END DOCFIXME
* B-BEGIN DOCFIXME
* B-END DOCFIXME
"
  (let* ((outer (common-ancestor a a-begin a-end))
         (context (create-crossover-context a outer a-begin :include-start nil))
         (b-stmts (-<>> (common-ancestor b b-begin b-end)
                       (get-parent-ast b)
                       (tree-successors b-begin <> :include-ast t)))
         (value1 (-<>> (fill-crossover-context context b-stmts)
                       ;; Special case if replacing a single statement
                       (or <> ( car (car b-stmts)))
                       (recontextualize a <> a-begin))))

    `((:stmt1  . ,outer)
      (:value1 . ,value1))))

;; Perform 2-point crossover. The second point will be within the same
;; function as the first point, but may be in an inner scope. The
;; number of scopes entered as you go from the first crossover point
;; to the second crossover point will be matched between a and b.
;; Free variables are rebound in such a way as to ensure that they are
;; bound to variables that are declared at each point of use.
;;
;; Modifies parameter A.
;;
(defmethod crossover-2pt-inward
    ((a clang-base) (b clang-base) a-begin a-end b-begin b-end)
  "DOCFIXME
* A DOCFIXME
* B DOCFIXME
* A-BEGIN DOCFIXME
* A-END DOCFIXME
* B-BEGIN DOCFIXME
* B-END DOCFIXME
"
  (labels
      ((child-index (parent child)
         "Position of CHILD within PARENT."
         (assert (equal (ast-path parent)
                        (butlast (ast-path child))))
         (lastcar (ast-path child)))
       (outer-ast (obj begin end)
         "AST which strictly encloses BEGIN and END."
         (let ((ancestor (common-ancestor obj begin end)))
           (if (equalp ancestor begin)
               (get-parent-ast obj ancestor)
               ancestor)))
       (splice-ast (a-outer b-outer b-inner b-ast)
         ;; Splice b-ast into a-outer.
         (bind ((children (ast-children a-outer))
                (a-index1 (child-index a-outer a-begin))
                (a-index2 (1+ (child-index a-outer
                                           (ancestor-after a a-outer a-end))))
                (b-index1 (child-index b-outer b-begin))
                (b-index2 (child-index b-outer b-inner)))
               (let ((new-children
                      (append
                         ;; A children before the crossover
                         (subseq children 0 a-index1)
                         ;; B children up to the inner ast
                         (subseq (ast-children b-outer)
                                 b-index1
                                 (if b-ast b-index2 (1+ b-index2)))
                         ;; The inner ast if it exists
                         (when b-ast (list b-ast))
                         ;; A children after the crossover
                         (subseq children a-index2))))
                 (copy a-outer
                       :path nil
                       :children new-children)
                 #+nil
                 (make-clang-ast
                  :path nil
                  :node (ast-node a-outer)
                  :children new-children)))))
    (let* ((a-outer (outer-ast a a-begin a-end))
           (b-outer (outer-ast b b-begin b-end))
           (b-inner (ancestor-after b b-outer b-end))
           (context (create-crossover-context b b-inner b-end :include-start t))
           (a-stmts (->> (common-ancestor a a-begin a-end)
                         (get-parent-ast a)
                         (tree-successors a-end)))
           ;; Build ast starting a b-outer.
           (b-ast (fill-crossover-context context a-stmts))
           ;; Splice into a-outer to get complete ast
           (whole-ast (splice-ast a-outer b-outer b-inner b-ast)))

      `((:stmt1  . ,a-outer)
        (:value1 . ,(recontextualize a whole-ast a-begin))))))


(defun combine-crossover-targets (obj inward-target outward-target)
  "DOCFIXME
* OBJ DOCFIXME
* INWARD-SNIPPET DOCFIXME
* OUTWARD-SNIPPET DOCFIXME
"
  (let* ((outward-stmt1 (aget :stmt1 outward-target))
         (outward-value1 (aget :value1 outward-target))
         (inward-stmt1 (aget :stmt1 inward-target))
         (inward-value1 (aget :value1 inward-target)))
   (flet
       ((replace-in-target (outer-stmt inner-stmt value)
          (assert (not (equalp outer-stmt inner-stmt)))
          (let* ((inner-path (ast-path inner-stmt))
                 (outer-path (ast-path outer-stmt))
                 (rel-path (last inner-path
                                 (- (length inner-path) (length outer-path)))))
            (setf outer-stmt (sel/sw/ast::replace-ast outer-stmt rel-path value)))))

     (cond
       ((null inward-target) outward-target)
       ((null outward-target) inward-target)
       ;; Insert value for outward target into inward target
       ((ancestor-of obj inward-stmt1 outward-stmt1)
        (replace-in-target inward-stmt1 outward-stmt1 outward-value1)
        inward-target)

       ;; Insert value for inward target into outward target
       ((ancestor-of obj outward-stmt1 inward-stmt1)
        (replace-in-target outward-stmt1 inward-stmt1 inward-value1)
        outward-target)

       (t
        (let* ((ancestor (common-ancestor obj outward-stmt1 inward-stmt1))
               (value1 (copy ancestor)))
          (replace-in-target value1 inward-stmt1 inward-value1)
          (replace-in-target value1 outward-stmt1 outward-value1)
          `((:stmt1 . ,ancestor) (:value1 . ,value1))))))))

(defmethod update-headers-from-ast ((clang clang-base) (ast ast) database)
  "Walk the ast AST in clang object CLANG, adding includes, macros, and types
that are mentioned at nodes of the AST.  DATABASE is the associated macro/type
database."
  (labels
      ((update (ast)
         (mapc {add-include clang}
               (reverse (ast-includes-in-obj clang ast)))
         (mapc [{add-macro clang} {find-macro database}]
               (reverse (ast-macros ast)))
         (mapc [{add-type clang} {find-type database}]
               (reverse (ast-types ast)))
         (mapc #'update (remove-if-not #'ast-p (ast-children ast)))))
    (update ast)))

;; Find the ancestor of STMT that is a child of ANCESTOR.
;; On failure, just return STMT again.
(defmethod ancestor-after ((clang clang-base) (ancestor ast) (stmt ast))
  "DOCFIXME
* CLANG DOCFIXME
* ANCESTOR DOCFIXME
* STMT DOCFIXME
"
  (or (->> (get-parent-asts clang stmt)
           (find-if [{equalp ancestor} {get-parent-ast clang}]))
      stmt))

(defmethod common-ancestor ((clang clang-base) x y)
  "DOCFIXME
* CLANG DOCFIXME
* X DOCFIXME
* Y DOCFIXME
"
  (let* ((x-ancestry (get-parent-asts clang x))
         (y-ancestry (get-parent-asts clang y))
         (last 0))
    (loop
       :for xp :in (reverse x-ancestry)
       :for yp :in (reverse y-ancestry)
       :when (equalp xp yp)
       :do (setf last xp))
    last))

(defmethod ancestor-of ((clang clang-base) x y)
  "DOCFIXME
* CLANG DOCFIXME
* X DOCFIXME
* Y DOCFIXME
"
  (equalp (common-ancestor clang x y) x))

(defmethod scopes-between ((clang clang-base) stmt ancestor)
  "DOCFIXME
* CLANG DOCFIXME
* STMT DOCFIXME
* ANCESTOR DOCFIXME
"
  (iter (for ast in (get-parent-asts clang stmt))
        (counting (block-p clang ast))
        (until (equalp ast ancestor))))

(defmethod nesting-relation ((clang clang-base) x y)
  "DOCFIXME
* CLANG DOCFIXME
* X DOCFIXME
* Y DOCFIXME
"
  (if (or (null x) (null y)) nil
      (let* ((ancestor (common-ancestor clang x y)))
        (cond
          ((equalp x ancestor) (cons 0 (scopes-between clang y ancestor)))
          ((equalp y ancestor) (cons (scopes-between clang x ancestor) 0))
          (t
           ;; If the two crossover points share a CompoundStmt as the
           ;; common ancestor, then you can get from one to the other
           ;; without passing through the final set of braces.  To
           ;; compensate, we subtract one from the number of scopes
           ;; that must be traversed to get from X to Y.
           (let ((correction (if (eq (ast-class ancestor) :CompoundStmt)
                                 1 0)))
             (cons (- (scopes-between clang x ancestor) correction)
                   (- (scopes-between clang y ancestor) correction))))))))

;; Split the path between two nodes into the disjoint union of
;; a path appropriate for across-and-out crossover, followed by a
;; path approppriate for across-and-in.  Returns the pair of
;; path descriptions, or NIL for a path that is not needed.
(defmethod split-vee ((clang clang-base) x y)
  "DOCFIXME
* CLANG DOCFIXME
* X DOCFIXME
* Y DOCFIXME
"
  (let* ((ancestor (common-ancestor clang x y))
         (stmt (ancestor-after clang ancestor x)))
    (cond
      ((equalp x y)
       (values nil (cons x y)))
      ((equalp y ancestor)
       (values (cons x y) nil))
      ((equalp x ancestor)
       (values nil (cons x y)))
      ((equalp x stmt)
       (values nil (cons x y)))
      (t
       (values (cons x stmt)
               (cons (block-successor clang stmt) y))))))

(defmethod match-nesting ((a clang-base) xs (b clang-base) ys)
  "DOCFIXME
* A DOCFIXME
* XS DOCFIXME
* B DOCFIXME
* YS DOCFIXME
"
  (let* (;; Nesting relationships for xs, ys
         (x-rel (nesting-relation a (car xs) (cdr xs)))
         (y-rel (nesting-relation b (car ys) (cdr ys)))
         ;; Parent statements of points in xs, ys
         (xps (cons (enclosing-full-stmt a (get-parent-ast a (car xs)))
                    (enclosing-full-stmt a (get-parent-ast a (cdr xs)))))
         (yps (cons (enclosing-full-stmt b (get-parent-ast b (car ys)))
                    (enclosing-full-stmt b (get-parent-ast b (cdr ys))))))
    ;; If nesting relations don't match, replace one of the points with
    ;; its parent's enclosing full statement and try again.
    (cond
      ((< (car x-rel) (car y-rel))
       (match-nesting a xs b (cons (car yps) (cdr ys))))
      ((< (cdr x-rel) (cdr y-rel))
       (match-nesting a xs b (cons (car ys) (cdr yps))))
      ((> (car x-rel) (car y-rel))
       (match-nesting a (cons (car xps) (cdr xs)) b ys))
      ((> (cdr x-rel) (cdr y-rel))
       (match-nesting a (cons (car xs) (cdr xps)) b ys))
      (t
       (multiple-value-bind (a-out a-in)
           (split-vee a (car xs) (cdr xs))
         (multiple-value-bind (b-out b-in)
             (split-vee b (car ys) (cdr ys))
           (values a-out b-out a-in b-in)))))))

(defmethod intraprocedural-2pt-crossover ((a clang-base) (b clang-base)
                                          a-begin a-end
                                          b-begin b-end)
  "DOCFIXME
* A DOCFIXME
* B DOCFIXME
* A-BEGIN DOCFIXME
* A-END DOCFIXME
* B-BEGIN DOCFIXME
* B-END DOCFIXME
"
  (let ((variant (copy a)))
    (multiple-value-bind (a-out b-out a-in b-in)
        (match-nesting a (cons a-begin a-end)
                       b (cons b-begin b-end))

      (let* ((outward-target
              (when (and a-out b-out)
                (crossover-2pt-outward variant b
                                       (car a-out) (cdr a-out)
                                       (car b-out) (cdr b-out))))
             (inward-target
              (when (and (car a-in) (car b-in))
                (crossover-2pt-inward variant b
                                       (car a-in) (cdr a-in)
                                       (car b-in) (cdr b-in))))
             (complete-target
               (combine-crossover-targets a
                                          inward-target
                                          outward-target)))


        (update-headers-from-ast a (aget :value1 complete-target) b)

        (apply-mutation-ops
         variant
         `((:set (:stmt1  . ,(aget :stmt1 complete-target))
                 (:value1 . ,(aget :value1 complete-target)))))

        (values variant
                (cons a-begin a-end)
                (cons b-begin b-end)
                t
                (cons (or (car a-out) (car a-in))
                      (or (cdr a-in) (cdr a-out))))))))

(defgeneric adjust-stmt-range (software start end)
  (:documentation
   "Adjust START and END so that they represent a valid range for set-range.
The values returned will be STMT1 and STMT2, where STMT1 and STMT2 are both
full statements"))

(defmethod adjust-stmt-range ((clang clang-base) start end)
  "DOCFIXME
* CLANG DOCFIXME
* START DOCFIXME
* END DOCFIXME
"
  (when (and start end)
    (let* ((stmt1 (enclosing-full-stmt clang (ast-at-index clang start)))
           (stmt2 (enclosing-full-stmt clang (ast-at-index clang end)))
           (position1 (index-of-ast clang stmt1))
           (position2 (index-of-ast clang stmt2)))
      (cond ((not (and stmt1 stmt2))
             ;; If either of STMT1 or STMT2 are nil, then most likely
             ;; START or END aren't valid stmt-asts.  In this case we
             ;; will imagine that the caller has made a mistake, and
             ;; simply return STMT1 and STMT2.
             (warn "Unable to find enclosing full statements for ~a and/or ~a."
                   start end)
             (values position1 position2))
            ((or (ancestor-of clang stmt1 stmt2)
                 (ancestor-of clang stmt2 stmt1))
             (values position1 position2))
            ((< position2 position1)
             (values position2 position1))
            (t
             (values position1 position2))))))

(defgeneric random-point-in-function (software prototype)
  (:documentation
   "Return the index of a random point in PROTOTYPE in SOFTWARE.
If PROTOTYPE has an empty function body in SOFTWARE return nil."))

(defmethod random-point-in-function ((clang clang-base) function)
  "DOCFIXME
* CLANG DOCFIXME
* FUNCTION DOCFIXME
"
  (destructuring-bind (first last) (stmt-range clang function)
    (if (equal first last) nil
        (+ (1+ first) (random (- last first))))))

(defgeneric select-intraprocedural-pair (software)
  (:documentation
   "Randomly select an AST within a function body and then select
another point within the same function.  If there are no ASTs
within a function body, return null."))

(defmethod select-intraprocedural-pair ((clang clang-base))
  "DOCFIXME
* CLANG DOCFIXME
"
  (when-let (stmt1 (some->> (remove-if {function-body-p clang} (stmt-asts clang))
                            (random-elt)))
    (values (index-of-ast clang stmt1)
            (random-point-in-function
             clang
             (function-containing-ast clang stmt1)))))

(defmethod select-crossover-points ((a clang-base) (b clang-base))
  "DOCFIXME
* A DOCFIXME
* B DOCFIXME
"
  (multiple-value-bind (a-stmt1 a-stmt2)
      (select-intraprocedural-pair a)
    (multiple-value-bind (b-stmt1 b-stmt2)
        (select-intraprocedural-pair b)
      (values a-stmt1 a-stmt2 b-stmt1 b-stmt2))))

(defmethod select-crossover-points-with-corrections ((a clang-base) (b clang-base))
  "DOCFIXME
* A DOCFIXME
* B DOCFIXME
"
  (multiple-value-bind (a-pt1 a-pt2 b-pt1 b-pt2)
      ;; choose crossover points
      (select-crossover-points a b)
    (multiple-value-bind (a-stmt1 a-stmt2)
        ;; adjust ranges to be valid for use with set-range
        (adjust-stmt-range a a-pt1 a-pt2)
      (multiple-value-bind (b-stmt1 b-stmt2)
          (adjust-stmt-range b b-pt1 b-pt2)
        (values a-stmt1 a-stmt2 b-stmt1 b-stmt2)))))

(defmethod crossover ((a clang-base) (b clang-base))
  "DOCFIXME
* A DOCFIXME
* B DOCFIXME
"
  (multiple-value-bind (a-stmt1 a-stmt2 b-stmt1 b-stmt2)
      (select-crossover-points-with-corrections a b)
    (if (and a-stmt1 a-stmt2 b-stmt1 b-stmt2)
        (multiple-value-bind (crossed a-point b-point changedp)
            (intraprocedural-2pt-crossover
             a b
             (ast-at-index a a-stmt1) (ast-at-index a a-stmt2)
             (ast-at-index b b-stmt1) (ast-at-index b b-stmt2))
          (if changedp
              (values crossed a-point b-point)
              (values crossed nil nil)))
        ;; Could not find crossover point
        (values (copy a) nil nil))))

(defgeneric function-containing-ast (object ast)
  (:documentation "Return the ast for the function containing AST in OBJECT."))

(defmethod function-containing-ast ((clang clang-base) (ast ast))
  "Return the function in CLANG containing AST.
* CLANG software object containing AST and its parent function
* AST ast to search for the parent function of
"
  (find-if #'function-decl-p (get-parent-asts clang ast)))

(defmethod function-body-p ((clang clang-base) stmt)
  "Return true if stmt AST if a function body, nil otherwise.
* CLANG software object containing STMT
* STMT ast to test if a function body
"
  (find-if [{equalp stmt} {function-body clang}] (functions clang)))


;;; Implement the generic format-genome method for clang objects.
(defmethod format-genome ((obj clang-base) &key)
  "Apply Artistic Style to OBJ to format the software."
  (astyle obj))


;;; Support for parsing a string directly into free-floating ASTs.
(defun parse-source-snippet-clang-common (type snippet
                                          &key unbound-vals includes macros
                                            preamble top-level keep-comments)
  "Build ASTs for SNIPPET, returning a list of root asts.  This function
contains logic common to both clang and new-clang implementations.
Arguments are identical to PARSE-SOURCE-SNIPPET."
  (labels ((has-comment-p (text)
             (and (stringp text)
                  (or (and (search "/*" text)
                           (search "*/" text))
                      (search "//" text))))
           (trim-stmt-end (text)
             (string-left-trim '(#\Space #\Backspace #\Tab #\;)
                                text))
           (prepend-text (ast text &aux (children (ast-children ast)))
             (copy ast
                   :children (if (stringp (first children))
                                 (cons (concatenate 'string text
                                                            (first children))
                                       (cdr children))
                                 (cons text children)))))
    (handler-case
        (let* ((dependency-source
                (format nil "
/* generated includes */
~{#include ~a~&~}
~{#define ~a~&~}
/* generated declarations */
~:{~a ~a;~%~}~%
/* preamble */
~a
"
                        includes
                        (mapcar #'macro-body macros)
                        (mapcar «list [#'type-decl-string #'second] #'first»
                                unbound-vals)
                        (or preamble "")))
               (wrapped (format nil
                                (if top-level
                                    "int __snippet_marker;~%~a~%"
                                    "void main() {int __snippet_marker; ~a;~%}")
                                snippet))
               (obj (make-instance (find-symbol (symbol-name type))
                      :flags (list "-Wno-everything")
                      :genome (concatenate 'string
                                           dependency-source
                                           wrapped)))
               (block-children
                 (if top-level
                     (mapcar #'copy (get-immediate-children obj (ast-root obj)))
                     (iter (for child in (->> (functions obj)
                                              (lastcar)
                                              (function-body obj)
                                              (ast-children)))
                           (for prev previous child)
                           (when (ast-p child)
                             (if (and keep-comments (has-comment-p prev))
                                 (collect (prepend-text child
                                                        (trim-stmt-end prev)))
                                 (collect (copy child))))))))
          (subseq block-children
                  (1+ (position-if [{name= "__snippet_marker"} #'car
                                    #'ast-declares]
                                   block-children))
                  (if (equal :NullStmt (ast-class (lastcar block-children)))
                      (1- (length block-children))
                      (length block-children))))
      ;; If error parsing simply return nil.
      (mutate (e) (declare (ignorable e)) nil))))

(defmethod parse-source-snippet ((type (eql :clang))
                                 (snippet string)
                                 &key unbound-vals includes macros preamble
                                   top-level keep-comments)
  "Build ASTs for SNIPPET, returning a list of root asts.

* SNIPPET may include one or more full statements. It should compile in
  a context where all UNBOUND-VALS are defined and all INCLUDES are
  included.

* UNBOUND-VALS should have the form ((name clang-type) ... )

* INCLUDES is a list of files to include.

* MACROS is a list of macros to define

* PREAMBLE source to add prior to snippet

* TOP-LEVEL indicates that the snippet is a construct which can exist
  outside a function body, such as a type or function declaration.

* KEEP-COMMENTS indicates comments should be retained
"
  (parse-source-snippet-clang-common type snippet
                                     :unbound-vals unbound-vals
                                     :includes includes
                                     :macros macros
                                     :preamble preamble
                                     :top-level top-level
                                     :keep-comments keep-comments))


;;; Function to find the position of a semicolon, if any
;;; Skip over C and C++ style comments

(defun position-of-leading-semicolon (str)
  (assert (stringp str))
  (let ((len (length str))
	(i -1))
    (loop
       (flet ((%step () (when (>= (incf i) len) (return))))
	 (declare (inline %step))
	 (%step)
	 (let ((c (char str i)))
	   (unless (or (eql c #\Space) (not (graphic-char-p c)))
	     (case c
	       (#\/
		(%step)
		(setf c (char str i))
		(%step)
		(case c
		  (#\/
		   ;; C++ style comment; skip to end of line
		   (let ((next (position #\Newline str :start i)))
		     (unless next (return))
		     (setf i next)))
		  ;; C style comment; search for closing characters
		  (#\*
		   (let ((next (search "*/" str :start2 i)))
		     (unless next (return))
		     (setf i (1+ next))))
                  (t (return nil))))
               (#\;
                (return (values i (subseq str 0 (1+ i)) (subseq str (1+ i)))))
	       (t (return nil)))))))))

;; This isn't right (consider // comments or non-symmetric effects of
;; backslash)
(defun position-of-trailing-semicolon (str)
  (let ((pos (position-of-leading-semicolon (reverse str))))
    (and pos (- (length str) pos 1))))


;;; Process a clang ast to move semicolons down to appropriate places

(defun move-semicolons-into-full-stmts (children)
  "Given a list of children of an AST node, move semicolons from
strings in the list they are in into preceding full stmt nodes.
Applied in preorder, this can migrate semicolons multiple levels down
the tree."
  (let ((p children))
    (loop
       (unless p (return))
       (let ((e (car p)))
         (when (and (ast-p e)
                    (ast-full-stmt e)
                    (stringp (cadr p)))
           (let ((e-children (ast-children e)))
             (when (stringp (lastcar e-children))
               (multiple-value-bind (found? prefix suffix)
                   (position-of-leading-semicolon (cadr p))
                 (when found?
                   (append-string-to-node e prefix)
                   (setf (cadr p) suffix)))))))
       (pop p))))

(defun move-semicolons-into-expr-stmts (ast)
  "CALLEXPRs don't necessarily have their semicolons in them.
Move the semicolon in just one level, but no further"
  ;;; Previously this was just for call-exprs in compoundstmts,
  ;;; but new clang needs more
  (let* ((children (ast-children ast))
         (p children))
    (loop (unless p (return))
       (let ((e (car p)))
         (when (and (ast-p e)
                    (stringp (cadr p))
                    (or (ast-full-stmt e) (eql (ast-class e) :field))
                    (stringp (lastcar (ast-children e))))
           (multiple-value-bind (found? prefix suffix)
               (position-of-leading-semicolon (cadr p))
             (when found?
               (append-string-to-node e prefix)
               (setf (cadr p) suffix)))))
       (pop p))))

;;; This is made generic because SOURCE-TEXT and AST-TEXT may
;;; be specialized for particular node types.
(defgeneric append-string-to-node (a str)
  (:documentation "Attach STR to the end of the text for a node.
This is done without copying.")
  (:method ((a ast) (str string))
    ;; default method
    (let ((c (ast-children a)))
      (if (null c)
          (setf (ast-children a) (list str))
          (let ((lc (last c)))
            (if (stringp (car lc))
                (setf (car lc) (concatenate 'string (car lc) str))
                (setf (cdr lc) (list str))))))))

;;; The following was moved from clang-instrument, because we need
;;; to specialize them for macroexpansion nodes in new-clang
;;; There is a name collision with the labels functionn ADD-SEMICOLON
;;; in clang-fixup-mutation.  TODO: change one of these names
(defgeneric add-semicolon (ast semi-position)
  (:documentation "Nondestructuvely add a semicolon before, after or
on both sides of AST.  AST is a string or ast node.  SEMI-POSITION is
:BEFORE, :AFTER, :BOTH, or some other value (which means no change.")
  (:method ((ast null) pos)
    (declare (ignorable ast pos))
    ";")
  (:method ((ast string) (pos (eql :before)))
    (declare (ignorable pos))
    (concatenate 'string '(#\;) ast))
  (:method ((ast string) (pos (eql :after)))
    (declare (ignorable pos))
    (concatenate 'string ast '(#\;)))
  (:method ((ast ast) (pos (eql :before)))
    (declare (ignorable pos))
    (let ((children (ast-children ast)))
      (copy ast :children
            (cons (add-semicolon (car children) :before)
                  (cdr children)))))
  (:method ((ast ast) (pos (eql :after)))
    (declare (ignorable pos))
    (copy ast :children
          (append (ast-children ast) (list ";"))))
  (:method (ast (pos (eql :both)))
    (declare (ignorable pos))
    (add-semicolon (add-semicolon ast :before) :after))
  (:method (ast pos)
    (declare (ignorable pos))
    ast))

(defun has-trailing-semicolon-p (ast)
  (typecase ast
    (string
     (position-of-trailing-semicolon ast))
    (ast
     (has-trailing-semicolon-p (lastcar (ast-children ast))))
    (t nil)))

(defun fix-semicolons-ast (ast)
  "Move semicolons into the appropriate stmt nodes in the children of node AST"
  (move-semicolons-into-expr-stmts ast)
  (move-semicolons-into-full-stmts (ast-children ast))
  ast)

(defun fix-semicolons (ast)
  "Move semicolons into appropriate stmt nodes in the tree rooted at AST"
  (map-ast ast #'fix-semicolons-ast)
  ast)

(defun stmt-p (ast)
  ;; Try to figure out if this node is pretending to be a statement, either
  ;; because it's truly a statement, or is an expression ending in a semicolon
  (or (ast-full-stmt ast)
      (has-trailing-semicolon-p ast)))
