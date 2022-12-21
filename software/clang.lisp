;;; clang.lisp --- clang software representation
;;;
;;; DOCFIXME Need a page or so introduction to clang software objects.
;;;
;;; @texi{clang}
(defpackage :software-evolution-library/software/clang
  (:nicknames :sel/software/clang :sel/sw/clang)
  (:use :gt/full
        :metabang-bind
        :uiop/pathname
        :software-evolution-library
        :software-evolution-library/utility/json
        :software-evolution-library/software/compilable
        :software-evolution-library/software/parseable
        :software-evolution-library/components/file
        :software-evolution-library/components/formatting
        :software-evolution-library/components/searchable
        :software-evolution-library/components/fodder-database)
  (:import-from :uiop/run-program :escape-shell-token)
  (:import-from :babel :string-size-in-octets)
  (:import-from :arrow-macros :some->>) ; FIXME: Remove.
  (:import-from :jsown)
  (:import-from :functional-trees :path-later-p)
  (:export :clang
           :clang-ast
           :headers
           :macros
           :includes
           :types
           :globals
           :symbol-table
           :name-symbol-table
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
           :get-entry
           :stmt-range
           :adjust-stmt-range
           :random-point-in-function
           :select-intraprocedural-pair
           :update-headers-from-snippet
           :update-headers-from-ast
           :prototypes
           :functions
           :stmt-asts
           :non-stmt-asts
           :good-stmts
           :bad-stmts
           :wrap-ast
           :wrap-child
           :+clang-wrapable-parents+
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
           :find-var-type
           :typedef-type
           :random-function-name
           :*clang-mutation-types*
           :*free-var-decay-rate*
           :*matching-free-var-retains-name-bias*
           :*matching-free-function-retains-name-bias*
           :delete-decl-stmts
           :ancestor-after
           :common-ancestor
           :ancestor-of
           :scopes-between
           :begins-scope
           :nesting-depth
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
           :force-include
           :add-type
           ;; :find-type
           :find-or-add-type
           :type-decl-string
           :type-trace-string
           :type-from-trace-string
           :trace-string-to-type-alist
           :add-macro
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
           ;; AST structures.
           :ast-class
           :ast-args
           :ast-args-equal
           :ast-declares
           :ast-expr-type
           :ast-full-stmt
           :ast-guard-stmt
           :ast-in-macro-expansion
           :ast-includes
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
           :ast-id
           :ast-type
           :ast-range
           :ast-i-file
           :ast-referenceddecl
           :ast-declarations
           :clang-type
           :ct+
           :ct+-type
           :copy-clang-type
           :type-qual
           :type-desugared
           :type-array
           :type-decl
           :type-hash
           :type-i-file
           :type-modifiers
           :type-pointer
           :type-const
           :type-volatile
           :type-restrict
           :type-storage-class
           :type-reqs
           :type-name
           :+pointer+
           :+const+
           :+volatile+
           :+restrict+
           :clang-macro
           :make-clang-macro
           :macro-name
           :macro-body
           :macro-hash
           :macro-i-file
           ;; FIXME: Clang literal building.
           :make-statement
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
           ;; AST names
           :name=
           :name-emptyp
           :equal-with-name=
           ;; FIXME
           :clang-range-begin
           :clang-loc-line
           :within-ast-range
           :ast-range-str
           :add-semicolon
           :cpp-scan))
(in-package :software-evolution-library/software/clang)
(in-readtable :curry-compose-reader-macros)


;;; Global variables
(declaim (special *aget-cache*))
(declaim (special *canonical-string-table*))
(declaim (special *canonical-clang-type-table*))
(declaim (special *clang-json-file*))

#-windows
(defun get-clang-default-includes ()
  "Retrieve the paths on the default clang system include search path."
  (nest
   (when (which "clang"))
   (with-temporary-file-of (:pathname bin :type "cpp") "")
   (multiple-value-bind (stdout stderr exit)
       (shell "clang -v ~a" bin)
     (declare (ignorable stdout exit))
     (register-groups-bind (include-search-paths)
         ("(?s)include <...> search starts here:(.*)End of search list"
          stderr)
       (nest (mapcar [#'namestring
                      #'ensure-directory-pathname
                      #'canonical-pathname])
             (mapcar #'trim-whitespace)
             (split-sequence #\Newline include-search-paths
                             :remove-empty-subseqs t))))))

#+windows
(defun get-clang-default-includes ()
  "Retrieve the paths on the default clang system include search path."
  (nest
   (when (which "clang-cl.exe"))
   (with-temporary-file-of (:pathname bin :type "cpp") "")
   (multiple-value-bind (stdout stderr exit)
       (shell "clang-cl.exe -v ~a" bin)
     (declare (ignorable stdout exit))
     (register-groups-bind (include-search-paths)
         ("(?s)include <...> search starts here:(.*)End of search list"
          stderr)
       (remove ""
               (nest (mapcar [#'namestring
                              #'ensure-directory-pathname
                              #'canonical-pathname])
                     (mapcar #'trim-whitespace)
                     (split-sequence #\Newline include-search-paths
                                     :remove-empty-subseqs t))
               :test 'equal)))))

(defvar *clang-default-includes* (get-clang-default-includes)
  "List of paths representing the default clang system includes search path.
These are required as -I flags as invoking clang -cc1 (required for ast-dump)
only invokes the clang front-end.
See also: https://clang.llvm.org/docs/FAQ.html#id2.")


;;; clang data structure definitions

(define-software clang (parseable compilable)
  ((asts
    :initarg :asts :reader asts :initform nil :copier :direct
    :type list :documentation "Deprecated: List of all ASTs.")
   (includes
    :initarg :includes :accessor includes
    :initform nil :copier :direct
    :type list ;; (list string *)
    :documentation "Names of headers included.")
   (types
    :initarg :types :reader types
    :initform (make-hash-table)
    :copier copy-hash-table
    :type hash-table
    :documentation "Hash table of types keyed by HASH id.")
   (macros
    :initarg :macros :accessor macros
    :initform nil :copier :direct
    :type list
    :documentation "List of macros.")
   (stmt-asts
    :initarg :stmt-asts :reader stmt-asts
    :initform nil :copier :direct
    :type list ;; (list (cons keyword *) *)
    :documentation
    "List of statement ASTs which exist within a function body.")
   ;; TODO: We should split non-statement ASTs into typedefs,
   ;;       structs/classes, and global variables, all of which should
   ;;       have different mutation types defined.  This needs more design.
   (non-stmt-asts
    :initarg :non-stmt-asts :reader non-stmt-asts
    :initform nil :copier :direct
    :type list ;;  (list (cons keyword *) *)
    :documentation
    "List of global AST which live outside of any function.")
   (functions
    :initarg :functions :reader functions
    :initform nil :copier :direct
    :type list ;; (list (cons keyword *) *)
    :documentation "Complete functions with bodies.")
   (prototypes
    :initarg :prototypes :reader prototypes
    :initform nil :copier :direct
    :type list ;; (list (cons keyword *) *)
    :documentation "Function prototypes.")
   (symbol-table
    :initarg :symbol-table :reader symbol-table
    :initform (make-hash-table :test #'equal)
    :copier copy-hash-table
    :type hash-table
    :documentation "Map from IDs to objects")
   (name-symbol-table
    :initarg :name-symbol-table :reader name-symbol-table
    :initform (make-hash-table :test #'equal)
    :copier copy-hash-table
    :type hash-table
    :documentation "Map from name strings to declaration objects."))
  (:documentation
   "C language (C, C++, C#, etc...) ASTs using Clang, C language frontend
   for LLVM.  See http://clang.llvm.org/.  This is for ASTs from Clang 9+."))

(eval-always
  (defclass clang-ast (ast stored-hash)
    ((path
      :initarg :path :initform nil :type list)
     (children
      :initarg :children :initform nil
      :accessor children :type list)
     (class
      :initarg :class :initform nil
      :accessor ast-class :type (or null symbol))
     (type
      :initarg :type :initform nil
      :accessor ast-type :type (or null clang-type ct+))
     (range
      :initarg :range :initform nil
      :accessor ast-range :type (or null clang-range))
     (id
      :initarg :id :initform nil
      :accessor ast-id :type (or null integer))
     (syn-ctx
      :initarg :syn-ctx :initform nil
      :accessor ast-syn-ctx :type (or null symbol))
     (stored-hash
      :initarg :stored-hash :initform nil :type (or null fixnum))
     (annotations
      :initarg :annotations :initform nil
      :accessor ast-annotations :type list)))

  (defclass clang-type ()
    ((qual :reader type-qual
           :initform nil
           :initarg :qual
           :documentation "Translation of the qualType attribute
  of clang json type objects")
     (desugared :reader type-desugared
                :initform nil
                :initarg :desugared
                :documentation "Translation of the desugaredQualType
  attribute of clang json objects")
     ;; Slots filled in by parsing the qual or desugred type
     (modifiers :initarg :modifiers
                :type integer
                :reader type-modifiers)
     (array :initarg :array
            :type string
            :reader type-array)
     ;; Name is the underlying name sans the modifiers and array
     (name :initarg :name
           :type string
           :reader type-name)
     ;; Slots populated from the type declaration AST.
     (i-file :reader type-i-file
             :initarg :i-file
             :initform nil
             :type (or null string)
             :documentation "Header file where the type is located.")
     (reqs :reader type-reqs
           :initarg :reqs
           :initform nil
           :type list ;; of clang-type objects
           :documentation "List of types that are required to understand
  this type.")
     ;; TODO: This field was carried forward from old clang types.
     ;; Perhaps this should be an AST instead of a string.
     (decl :reader type-decl
           :initarg :decl
           :initform ""
           :type string
           :documentation "Source text of the type declaration."))
    (:documentation "Objects representing C/C++ types.  Canonicalized
  on QUAL and DESUGARED slots."))

  (defclass ct+ ()
    ((type :initarg :type
           :reader ct+-type
           :type clang-type)
     (storage-class :initarg :storage-class
                    :reader type-storage-class
                    :initform :none
                    :type (member :none :auto :static :register
                                  :extern :__private_extern__)))
    (:documentation "Wrapper object that is intended to behave like
  SEL/SW/CLANG:CLANG-TYPE.  This means it must have some information
  that is not strictly speaking about types at all (storage class).")))

(defstruct (clang-macro (:conc-name :macro-))
  "Representation of a macro in software object including the header
the macro is defined within."
  (hash   nil :type (or number null))
  (name   nil :type (or string null))
  (body   nil :type (or string null))
  (i-file nil :type (or string null) :read-only t))

(defmethod print-object ((obj clang-ast) stream)
  "Print a representation of the clang-ast OBJ to STREAM.
* OBJ clang-ast to print
* STREAM stream to print OBJ to
"
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t)
        (format stream "~a~@[ ~a~]" (ast-class obj) (ast-name obj)))))

(defmethod print-object ((obj clang-type) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t)
        (format stream "~{~a~^ ~}"
                (append (let ((qual (type-qual obj)))
                          (when qual (list ":QUAL" qual)))
                        (let ((desugared (type-desugared obj)))
                          (when desugared (list ":DESUGARED" desugared))))))))



;;; FSet overrides
;;; NOTE: these are here to maintain previous functionality for methods that
;;;       have changed in parseable.
;;; FSet tree manipulations pass through to genome.
(eval-always
  (defun tree-manipulation-passthrough-for (name)
    "Return a defmethod form to define a passthrough method for NAME.
When NAME is called on a software object it will then be invoked on
the `genome' of the software object."
    (let ((lambda-list (generic-function-lambda-list (ensure-function name))))
      `(defmethod ,name ((obj clang) ,@(cdr lambda-list))
         (setf (genome obj)
               (,name (genome obj)
                      ,(second lambda-list)
                      ,@(nest (mapcar (lambda (param) `(tree-copy ,param)))
                              (remove '&optional)
                              (cddr lambda-list))))
         obj))))

(defmacro write-tree-manipulation-function-parseable-methods (&rest names)
  "Write tree-manipulation passthrough methods for NAMES using `tree-manipulation-passthrough-for'."
  `(progn ,@(mapcar #'tree-manipulation-passthrough-for names)))

(write-tree-manipulation-function-parseable-methods
 with
 insert
 splice)

(defmethod less ((obj clang) value1 &optional value2)
  (declare (ignorable value2))
  (setf (genome obj) (less (genome obj) value1))
  obj)


;;; Object creation, serialization, and copying.
(defmethod to-alist ((ast clang-ast))
  (flet ((%p (key fn)
           (list (cons key (funcall fn ast))))
         (%type (tp)
           (and tp (to-alist tp)))
         (%annotations (annotations)
           (append (when (aget :referenceddecl annotations)
                     (list (cons :referenceddecl
                                 (to-alist (aget :referenceddecl
                                                 annotations)))))
                   (when (aget :macro-child-segment annotations)
                     (list (cons :macro-child-segment
                                 (mapcar #'to-alist
                                         (aget :macro-child-segment
                                               annotations)))))
                   (when (aget :argtype annotations)
                     (list (cons :argtype
                                 (to-alist (aget :argtype annotations)))))
                   (adrop (list :type :argtype
                                :referenceddecl :macro-child-segment)
                          annotations))))
    (append (%p ':class #'ast-class)
            (%p ':id #'ast-id)
            (%p ':syn-ctx #'ast-syn-ctx)
            (%p ':annotations [#'%annotations #'ast-annotations])
            (%p ':range #'ast-range)
            (%p ':type [#'%type #'ast-type]))))

(defmethod from-alist ((obj (eql 'clang-ast)) alist)
  (flet ((%type (alist)
           (and alist (from-alist 'clang-type alist)))
         (%annotations (annotations)
           (append (when (aget :referenceddecl annotations)
                     (list (cons :referenceddecl
                                 (nest (from-alist 'clang-ast)
                                       (aget :referenceddecl annotations)))))
                   (when (aget :macro-child-segment annotations)
                     (list (cons :macro-child-segment
                                 (nest (mapcar {from-alist 'clang-ast})
                                       (aget :macro-child-segment
                                             annotations)))))
                   (when (aget :type annotations)
                     (list (cons :type
                                 (nest (from-alist 'clang-type)
                                       (aget :type annotations)))))
                   (when (aget :argtype annotations)
                     (list (cons :argtype
                                 (nest (from-alist 'clang-type)
                                       (aget :argtype annotations)))))
                   (adrop (list :type :argtype
                                :referenceddecl :macro-child-segment)
                          annotations))))
    (make-instance 'clang-ast
                   :class (aget :class alist)
                   :id (aget :id alist)
                   :syn-ctx (aget :syn-ctx alist)
                   :annotations (%annotations (aget :annotations alist))
                   :range (aget :range alist)
                   :type (%type (aget :type alist)))))

(defmethod copy ((ast clang-ast) &rest args
                 &key
                   referenceddecl
                   path
                   (children (children ast))
                   (class (ast-class ast))
                   (type (ast-type ast))
                   (range (ast-range ast))
                   (id (ast-id ast))
                   (syn-ctx (ast-syn-ctx ast))
                   (annotations (ast-annotations ast) annotations-p)
                   &allow-other-keys)
  ;; The value of REFERENCEDDECL is not otherwise explicitly
  ;; used in this function, but it gets used as part of ARGS
  (unless (or (null referenceddecl) (typep referenceddecl 'clang-ast))
    (error "Referenceddecl not an AST: ~s~%" referenceddecl))
  (let (new-annotations)
    (let ((args2 args))
      (iter (while args2)
            (let ((key (pop args2))
                  (arg (pop args2)))
              (case key
                ((:path :children :class :id :syn-ctx) nil)
                ((:annotations)
                 (unless annotations-p
                   (setf annotations-p t
                         annotations arg)))
                (t
                 ;; Otherwise, it's an annotation
                 (push (cons key arg) new-annotations))))))
    (iter (for (key . arg) in new-annotations)
          (setf annotations (areplace key arg annotations)))

    (make-instance 'clang-ast
                   :path path :children children
                   :class class :type type :range range :id id
                   :syn-ctx syn-ctx :annotations annotations)))

(defmethod to-alist ((clang-type clang-type))
  (flet ((%p (key fn)
           (list (cons key (funcall fn clang-type)))))
    (append (%p ':qual #'type-qual)
            (%p ':desugared #'type-desugared)
            (%p ':modifiers #'type-modifiers)
            (%p ':array #'type-array)
            (%p ':name #'type-name)
            (%p ':i-file #'type-i-file)
            (%p ':reqs [{mapcar #'to-alist} #'type-reqs])
            (%p ':decl #'type-decl))))

(defmethod to-alist ((nct ct+))
  (flet ((%p (key fn)
           (when-let ((v (funcall fn nct)))
             (list (cons key v)))))
    (append (%p ':type [#'to-alist #'ct+-type])
            (%p ':storage-class #'type-storage-class))))

(defmethod from-alist ((obj (eql 'clang-type)) alist)
  (make-instance 'clang-type
    :qual (aget :qual alist)
    :desugared (aget :desugared alist)
    :modifiers (aget :modifiers alist)
    :array (aget :array alist)
    :name (aget :name alist)
    :i-file (aget :i-file alist)
    :reqs (mapcar {from-alist 'clang-type}
                  (aget :reqs alist))
    :decl (aget :decl alist)))

(defmethod from-alist ((ct+ (eql 'ct+)) alist)
  (make-instance 'ct+
    :type (from-alist 'clang-type (aget :type alist))
    :storage-class (aget :storage-class alist)))

(defmethod copy ((tp clang-type)
                 &key (qual nil qual-supplied-p)
                   (desugared nil desugared-supplied-p)
                   (modifiers nil modifiers-supplied-p)
                   (array nil array-supplied-p)
                   (name nil name-supplied-p)
                   (i-file nil i-file-supplied-p)
                   (reqs nil reqs-supplied-p)
                   (decl nil decl-supplied-p))
  (make-instance 'clang-type
    :qual (if qual-supplied-p qual (type-qual tp))
    :desugared (if desugared-supplied-p desugared (type-desugared tp))
    :modifiers (if modifiers-supplied-p modifiers (type-modifiers tp))
    :array (if array-supplied-p array (type-array tp))
    :name (if name-supplied-p name (type-name tp))
    :i-file (if i-file-supplied-p i-file (type-i-file tp))
    :reqs (if reqs-supplied-p reqs (type-reqs tp))
    :decl (if decl-supplied-p decl (type-decl tp))))

(defmethod copy ((tp+ ct+)
                 &key (type nil type-supplied-p)
                   (storage-class nil storage-class-supplied-p))
  (make-instance 'ct+
    :type (if type-supplied-p type (ct+-type tp+))
    :storage-class (if storage-class-supplied-p
                       storage-class
                       (type-storage-class tp+))))

(defmethod copy ((macro clang-macro)
                 &key (hash nil hash-supplied-p)
                   (name nil name-supplied-p)
                   (body nil body-supplied-p)
                   (i-file nil i-file-supplied-p))
  (make-clang-macro
   :hash (if hash-supplied-p hash (macro-hash macro))
   :name (if name-supplied-p name (macro-name macro))
   :body (if body-supplied-p body (macro-body macro))
   :i-file (if i-file-supplied-p i-file (macro-i-file macro))))

(defmethod to-alist ((macro clang-macro))
  (flet ((%p (key fn)
           (list (cons key (funcall fn macro)))))
    (append (%p ':name #'macro-name)
            (%p ':body #'macro-body)
            (%p ':hash #'macro-hash)
            (%p ':i-file #'macro-i-file))))

(defmethod from-alist ((obj (eql 'clang-macro)) alist)
  (make-clang-macro :name (aget :name alist)
                    :body (aget :body alist)
                    :hash (aget :hash alist)
                    :i-file (aget :i-file alist)))


;;; Custom type and equivalence methods for AST names.
(deftype clang-name () 'string)

(defgeneric name= (n1 n2)
  (:documentation "Generalized name equality for AST names")
  (:method ((n1 string) (n2 string))
    (string= n1 n2))
  (:method ((n1 string) n2)
    (string= n1 (ast-name n2)))
  (:method (n1 (n2 string))
    (string= (ast-name n1) n2))
  (:method (n1 n2)
    (or (eql n1 n2)
        (string= (ast-name n1) (ast-name n2)))))

(defgeneric name-emptyp (n)
  (:documentation "Generalized name emptiness check")
  (:method ((n sequence)) (emptyp n))
  (:method (n) (declare (ignorable n)) nil))

(defun equal-with-name= (n1 n2)
  (if (consp n1)
      (and (consp n2)
           (equal-with-name= (car n1) (car n2))
           (equal-with-name= (cdr n1) (cdr n2)))
      (name= n1 n2)))


;;; clang-specific overrides of software object creation
;;; and slot reader/writer routines to ensure (1) paths
;;; in flags field are represented in canonicalized,
;;; absolute form; (2) the default compiler is 'clang';
;;; and (3) the default file extension is '.c'.
(defun normalize-flags (dir flags)
  "Normalize the list of compiler FLAGS so all search paths are fully
expanded relative to DIR.

* DIR base directory for all relative paths
* FLAGS list of compiler flags
"
  (labels ((split-flags (flags)
             (nest (remove-if #'emptyp)
                   (mapcar #'trim-whitespace)
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

(defmethod initialize-instance :after ((obj clang) &key &allow-other-keys)
  "Wrapper after the constructor to ensure all AST paths are populated,
flags are in a normalized form with absolute, canonical paths, and the
compiler is set to a default value if none is provided."
  (with-slots (genome flags compiler) obj
    (setf genome (if (stringp genome) genome (update-paths genome)))
    (setf flags (normalize-flags (original-directory obj)
                                 (flags obj)))
    (setf compiler (or compiler "clang")))
  obj)

(defmethod copy :before ((obj clang)
                         &key (genome nil genome-supplied-p) &allow-other-keys)
  "Wrapper before copy to ensure all AST paths are populated in the
genome supplied as a keyword."
  (setf genome
        (if (and genome-supplied-p (typep genome 'ast))
            (update-paths genome)
            genome)))

(defmethod (setf flags) :after ((flags list) (obj clang))
  "Wrapper after the flags setf to ensure the flags are in a
normalized form with absolute, canonical paths."
  (setf (slot-value obj 'flags)
        (normalize-flags (original-directory obj)
                         (flags obj))))

(defmethod from-file ((obj clang) path)
  "Initialize OBJ with the contents of PATH."
  (setf path (if (absolute-pathname-p path)
                 (namestring path)
                 (namestring (truename path))))
  (setf (genome obj) (file-to-string path))
  (setf (flags obj) (nest (normalize-flags (pathname-directory-pathname path))
                          (cons (format nil "-I~a"
                                        (pathname-directory-pathname path)))
                          (flags obj)))
  obj)

(defmethod ext :around ((obj clang))
  "Ensure the default extension for clang software objects is .c"
  (or (call-next-method) "c"))


;;; Legacy AST creation routines (deprecated).  Please use `to-ast` instead.
(defun make-statement (class syn-ctx children
                       &key full-stmt guard-stmt opcode declares annotations
                         &allow-other-keys)
  "Create a statement AST of the NEW-CLANG type.

* CLASS class name of the AST node
* SYN-CTX surrounding syntactic context of the AST node
* CHILDREN children of the AST node
* FULL-STMT boolean indicating if the AST represents a complete statement
* GUARD-STMT  boolean indicating if the AST is a control-flow predicate
* OPCODE name of the operation for Unary/BinaryOp AST nodes
* DECLARES identifiers declared by the AST node

Other keys are allowed but are silently ignored.
"
  (macrolet ((%push (k v)
               `(when ,v (push (cons ,k ,v) annotations))))
    (%push :full-stmt full-stmt)
    (%push :guard-stmt guard-stmt)
    (%push :opcode opcode)
    (%push :name (when (length= declares 1)
                   ;; clang name attribute is not aggregated
                   (ast-name (first declares))))
    (make-instance 'clang-ast
                   :path nil
                   :syn-ctx syn-ctx
                   :class class
                   :annotations annotations
                   :children children)))

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

(defun make-operator (syn-ctx opcode child-asts
                      &rest rest &key full-stmt &allow-other-keys)
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
* REST optional additional arguments to `make-statement'`
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

(defun make-var-reference (name type &rest rest)
  "Create a variable reference AST.
* NAME name of the variable to reference
* TYPE type of the variable to reference
* REST optional additional arguments to `make-statement'
"
  (apply #'make-statement :ImplicitCastExpr :generic
         (list (make-statement :DeclRefExpr :generic
                               (list name)
                               :expr-type (when type (ct+-type type))))
         :expr-type (when type (ct+-type type))
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
                                                  (list name))))
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


;;; Handling include, type, and macro information caches on clang
;;; software objects (formerly mitochondria).
(defgeneric add-type (software type)
  (:documentation "Add TYPE to `types' of SOFTWARE, unique by hash.")
  (:method ((obj clang) (type null)) nil)
  (:method ((obj clang) (type ct+))
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
              (prepend-text-to-genome obj td))))
      ;; always add type with new hash to types hashtable
      (setf (gethash (type-hash type) (types obj)) type))
    type))

(defmethod find-type ((obj clang) (hash integer))
  "Return the type in OBJ with the given type HASH.
* OBJ clang object to search for HASH
* HASH type hash to search for"
  (gethash hash (types obj)))

(defmethod find-type ((obj clang) (tp ct+))
  ;; This looks like a stub, but isn't.
  ;; What's happening here is that while in old clang
  ;; find-type was used to look up types from hashes,
  ;; in the new front end the type objects are there directly.
  ;; The lookup function just returns the object in that case.
  tp)

(defmethod find-type ((obj clang) (tp null))
  nil)

(defgeneric find-or-add-type (obj trace-name &rest args &key &allow-other-keys)
  (:documentation "Find the type with the given TRACE-NAME representation in
a execution trace in OBJ.")
  (:method ((obj clang) (trace-name string)
            &rest args &key &allow-other-keys
            &aux (name (apply #'trace-string-to-clang-json-string
                              trace-name args)))
    ;; Trace names have different format, with * and [...] before the type
    (or (first (remove-if-not {string= name}
                              (hash-table-values (types obj))
                              :key [#'type-qual #'ct+-type]))
        (add-type obj (make-instance 'ct+
                        :type (make-instance 'clang-type :qual name))))))

(defgeneric find-var-type (software variable)
  (:documentation "Return the type of VARIABLE in SOFTWARE.")
  (:method ((obj clang) (variable list))
    "Return the type of VARIABLE in SOFTWARE"
    (some->> (aget :type variable)
             (find-type obj))))

(defgeneric add-macro (software macro)
  (:documentation "Add MACRO to `macros' of SOFTWARE, unique by hash.")
  (:method ((obj clang) (macro clang-macro))
    (unless (find-macro obj (macro-hash macro))
      (prepend-text-to-genome obj (format nil "#define ~a~&"
                                          (macro-body macro)))
      (push macro (macros obj)))
    obj))

(defmethod find-macro ((obj clang) (hash integer))
  "Return the macro in OBJ with the given HASH.
* OBJ object to search for HASH
* HASH macro hash to find
"
  (find-if {= hash} (macros obj) :key #'macro-hash))

(defmethod find-macro ((obj clang) (macro clang-macro))
  ;; This looks like a stub, but isn't.
  ;; What's happening here is that while in old clang
  ;; find-macro was used to look up macros from hashes,
  ;; in the new front end the macro objects are there directly.
  ;; The lookup function just returns the object in that case.
  macro)

(defgeneric add-include (software include)
  (:documentation "Add an #include directive for an INCLUDE to SOFTWARE.")
  (:method ((obj clang) (include string))
    (unless (member include (includes obj) :test #'string=)
      (prepend-text-to-genome obj (format nil "#include ~a~&" include))
      (push include (includes obj)))
    obj))

(defgeneric force-include (obj include)
  (:documentation "Add an #include directive for an INCLUDE to OBJ
even if such an INCLUDE already exists in OBJ.")
  (:method ((obj clang) (include string))
    (prepend-text-to-genome obj (format nil "#include ~a~&" include))
    (unless (member include (includes obj) :test #'string=)
      (push include (includes obj)))
    obj))


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
         (s1-full? (ast-full-stmt s1))
         (s2-full? (ast-full-stmt s2))
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

(defmethod pick-guarded-compound ((obj clang))
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
  (declare (ignorable software))
  (labels
      ((compose-children (&rest parents)
         (interleave (iter (for p in parents)
                           ;; In case of an unbraced if/loop body, include
                           ;; the body directly.
                           (if (eq :CompoundStmt (ast-class p))
                               (appending (child-asts p))
                               (collecting p)))
                      (format nil "~%"))))
      (let ((children
          (switch ((ast-class guarded))
            (:DoStmt
             (compose-children
              (first (child-asts guarded))))
            (:WhileStmt
             (compose-children
              (second (child-asts guarded))))
            (:ForStmt
             (compose-children
              (lastcar (child-asts guarded))))
            (:IfStmt
             (let ((children (child-asts guarded)))
               (if (length= 2 children)
                   ;; If with only one branch.
                   (compose-children (second children))
                   ;; If with both branches.
                   (cond
                     ((null             ; Then branch is empty.
                       (child-asts (second children)))
                      (compose-children (third children)))
                     ((null             ; Else branch is empty.
                       (child-asts (third children)))
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

(defmethod pick-for-loop ((obj clang))
  "Return a for loop in OBJ from the `bad-stmts' pool.
* OBJ software object to pick from
"
  (pick-bad-only obj :filter [{eq :ForStmt} #'ast-class]))

(defmethod build-op ((mutation explode-for-loop) (obj clang))
  "Return an association list with the operations to apply an
`explode-for-loop' MUTATION to OBJ.
* MUTATION defines the targets of the explode-for-loop operation
* OBJ object to be modified by the mutation
"
  (declare (ignorable obj))
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
             (let ((children (child-asts ast)))
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

(defmethod pick-while-loop ((obj clang))
  "Return a while loop statement in OBJ from the `bad-stmts' pool.
* OBJ software object to pick from
"
  (pick-bad-only obj :filter [{eq :WhileStmt} #'ast-class]))

(defmethod build-op ((mutation coalesce-while-loop) (obj clang))
  "Return an association list with the operations to apply a
`coalesce-while-loop' MUTATION to SOFTWARE.
* MUTATION defines the targets of the coalesce-while-loop operation
* OBJ object to be modified by the mutation
"
  (let ((ast (aget :stmt1 (targets mutation))))
    (destructuring-bind (condition body)
        (child-asts ast)
      (let ((precedent (block-predeccessor obj ast)))
        `((:set (:stmt1 . ,ast)
                ,(let ((children (child-asts body)))
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
                (child-asts)
                (remove-if-not [{eq :DeclStmt} #'ast-class])
                (remove-if {equalp ast})
                (random-elt))))
    (if-let ((decl (some->> (bad-mutation-targets clang
                              :filter «and #'is-decl
                                           #'pick-another-decl-in-block»)
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
                                  (list (list old-var new-var))
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
  (labels ((compound-assign-op (ast)
             (eq (ast-class ast) :CompoundAssignOperator))
           (increment-op (ast)
             (and (eq (ast-class ast) :UnaryOperator)
                  (equal (ast-opcode ast) "++")))
           (decrement-op (ast)
             (and (eq (ast-class ast) :UnaryOperator)
                  (equal (ast-opcode ast) "--"))))
    (let ((ast (some->> (bad-mutation-targets clang
                                              :filter «or #'compound-assign-op
                                                          #'increment-op
                                                          #'decrement-op»)
                        (random-elt))))
      `((:stmt1 . ,ast)
        (:literal1 .
           ,(let* ((children (child-asts ast))
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

(defmethod (setf genome) :before ((new t) (obj clang))
  "Clear caches prior to updating to the NEW genome."
  (clear-caches obj))

(defmethod (setf genome) :before ((new string) (obj clang))
  "When the genome is set to a string and must be re-parsed, clear the
macros and symbol table fields in addition to the normal caches.  These
fields are re-populated when parsing ASTs."
  (setf (slot-value obj 'macros) nil)
  (setf (slot-value obj 'symbol-table) (make-hash-table :test #'equal)))

(defmethod (setf genome) :around ((new ast) (obj clang))
  "Upon setting the genome, if the new genome is composed of ASTS,
create a deep copy of NEW to ensure ASTs are not shared between
software objects after modification."
  (labels ((deep-copy (ast)
             (copy ast :children (mapcar (lambda (c)
                                           (if (typep c 'clang-ast)
                                               (deep-copy c)
                                               c))
                                         (children ast)))))
    (call-next-method (update-paths (deep-copy new)) obj)))

(defmethod (setf genome) :after ((new ast) (obj clang))
  "After setting the genome, update the symbol table and then update the
:REFERENCEDDECL field on the ASTs in NEW to point to the entries
in the symbol table."
  (with-slots (symbol-table) obj
    (setf symbol-table
          (update-symbol-table (clear-symbol-table symbol-table) new))
    (setf new
          (update-referenceddecl-from-symbol-table new symbol-table))))

(defmethod        asts   :before ((obj clang))
  "Ensure the `asts` field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defmethod        macros :before ((obj clang))
  "Ensure the `macros' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defmethod      includes :before ((obj clang))
  "Ensure the `includes' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defmethod         types :before ((obj clang))
  "Ensure the `types' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defmethod     stmt-asts :before ((obj clang))
  "Ensure the `stmt-asts' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defmethod non-stmt-asts :before ((obj clang))
  "Ensure the `non-stmt-asts' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defmethod     functions :before ((obj clang))
  "Ensure the `functions' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defmethod    prototypes :before ((obj clang))
  "Ensure the `prototypes' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defmethod name-symbol-table :before ((obj clang))
  "Ensure the `name-symbol-table' field is set on OBJ prior to access."
  (update-caches-if-necessary obj))

(defgeneric update-caches-if-necessary (obj)
  (:documentation "Update cached fields of OBJ if these fields are not set.")
  (:method ((obj clang))
    (unless (slot-value obj 'asts) (update-caches obj))))

(defgeneric update-caches (obj)
  (:documentation "Update cached fields of OBJ, including `asts', `stmt-asts',
`non-stmt-asts', `functions', `prototypes', `includes`, `types`, `symbol-table`,
and `name-symbol-table`, returning OBJ.
* OBJ object to update caches for.")
  (:method ((obj clang))
    ;; Update AST-based caches
    (with-slots (asts stmt-asts non-stmt-asts functions prototypes) obj
      (let ((last-proto nil))
        (iter (for ast in (child-asts (genome obj) :recursive t))
              (collect ast into my-asts)
              (when (function-decl-p ast)
                (collect ast into protos)
                (when (function-body ast)
                  (collect ast into funs))
                (setf last-proto ast))
              ;; stmt-asts are only collected in function bodies and
              ;; non-stmt-asts are only collected outside of function bodies
              (if (and last-proto (starts-with-subseq (ast-path obj last-proto)
                                                      (ast-path obj ast)))
                  (unless (or (eq :ParmVar (ast-class ast))
                              (function-decl-p ast))
                    (collect ast into my-stmts))
                  (collect ast into my-non-stmts))
              (finally
               (setf asts my-asts
                     stmt-asts my-stmts
                     non-stmt-asts my-non-stmts
                     functions funs
                     prototypes protos)))))

    ;; Update non-AST caches
    (with-slots (includes types symbol-table name-symbol-table) obj
      (setf includes
            (ast-includes obj (genome obj)))
      (setf name-symbol-table
            (if (zerop (hash-table-count name-symbol-table))
                (update-name-symbol-table name-symbol-table symbol-table)
                name-symbol-table))
      (setf types
            (if (zerop (hash-table-count types))
                (update-type-table types symbol-table (genome obj))
                types)))
    obj))

(defgeneric clear-caches (obj)
  (:documentation "Clear cached fields on OBJ, including `asts', `stmt-asts',
`non-stmt-asts', `functions', `prototypes', `includes', `types', and
`name-symbol-table.'
* OBJ object to clear caches for.")
  (:method ((obj clang))
    ;; Clear AST-based caches
    (with-slots (asts stmt-asts non-stmt-asts functions prototypes includes) obj
      (setf asts nil
            stmt-asts nil
            non-stmt-asts nil
            functions nil
            prototypes nil
            includes nil))

    ;; Clear non-AST caches
    (with-slots (includes types name-symbol-table) obj
      (setf includes nil)
      (setf types (make-hash-table :test #'equal))
      (setf name-symbol-table (make-hash-table :test #'equal)))))

;; FIXME: When clang is converted to utilize functional trees,
;; this method specialization will no longer be required.
(defmethod size ((obj clang))
  "Return the number of non-root ASTs in OBJ."
  (length (asts obj)))

(defmethod lines ((obj clang) &rest args &key)
  "Return a list of lines of source in OBJ"
  (apply #'lines (genome-string obj) args))

(defmethod (setf lines) (new (obj clang))
  "Set the genome of OBJ from a list of lines of source"
  (setf (genome obj) (format nil "~{~a~^~%~}" new)))

(defmethod recontextualize ((clang clang) (ast clang-ast) (pt clang-ast))
  "Bind free variables and function in AST to concrete values
required for successful mutation in CLANG at PT
* CLANG object to be mutated
* AST node to be mutated into CLANG
* PT node where mutation is to occur
"
  (bind-free-vars clang ast pt))

(defmethod get-parent-decls ((clang clang) (ast clang-ast))
  "Return parents of AST in CLANG which are decl ASTs.
* CLANG software object to query
* AST ast to begin query from
"
  (remove-if-not #'ast-is-decl (get-parent-asts clang ast)))

(defmethod good-stmts ((clang clang))
  "Return a list of all good statement ASTs in CLANG."
  (stmt-asts clang))

(defmethod bad-stmts ((clang clang))
  "Return a list of all bad statement ASTs in CLANG."
  (stmt-asts clang))

(defmethod pick-good ((clang clang))
  "Pick a random AST in CLANG from the `good-stmt' pool."
  (random-elt (good-mutation-targets clang)))

(defmethod pick-bad ((clang clang))
  "Pick a random AST in CLANG from the `bad-stmt' pool."
  (random-elt (bad-mutation-targets clang)))

(defmethod pick-bad-good ((clang clang) &key filter
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

(defmethod pick-bad-bad ((clang clang) &key filter (bad-pool #'bad-stmts))
  "Pick two ASTs from CLANG, both from the `bad-asts' pool,
excluding those ASTs removed by FILTER.
* CLANG object to perform picks for
* FILTER function taking two AST parameters and returning non-nil if the
second should be included as a possible pick
* BAD-POOL function returning a pool of 'bad' ASTs in SOFTWARE
"
  (call-next-method clang :filter filter :bad-pool bad-pool))

(defmethod pick-bad-only ((clang clang) &key filter
                          (bad-pool #'bad-stmts))
  "Pick a single AST from CLANG from `bad-pool',
excluding those ASTs removed by FILTER.
* CLANG object to perform picks for
* FILTER function taking two AST parameters and returning non-nil if the
second should be included as a possible pick
* BAD-POOL function returning a pool of 'bad' ASTs in SOFTWARE
"
  (call-next-method clang :filter filter :bad-pool bad-pool))

(defmethod good-mutation-targets ((clang clang) &key filter)
  "Return a list of all good statement ASTs in CLANG matching FILTER.
* CLANG software object to query for good statements
* FILTER predicate taking an AST parameter to allow for filtering
"
  (mutation-targets clang :filter filter :stmt-pool #'good-stmts))

(defmethod bad-mutation-targets ((clang clang) &key filter)
  "Return a list of all bad statement ASTs in CLANG matching FILTER.
* CLANG software object to query for bad statements
* FILTER predicate taking an AST parameter to allow for filtering
"
  (mutation-targets clang :filter filter :stmt-pool #'bad-stmts))

(defmethod mutation-targets ((clang clang) &key (filter nil)
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

(defmethod pick-mutation-type ((obj clang))
  "Select type of mutation to apply to OBJ."
  (random-pick *clang-mutation-types*))

(defmethod mutate ((clang clang))
  "Select a random mutation and mutate CLANG."
  (unless (stmt-asts clang)
    (error (make-condition 'mutate :text "No valid statements" :obj clang)))
  (let ((mutation (make-instance (pick-mutation-type clang) :object clang)))
    (apply-mutation clang mutation)
    (values clang mutation)))

(defmethod apply-mutation ((software clang)
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
                                {path-later-p software}
                                :key [{aget :stmt1} #'cdr]))
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

(defmethod mutation-key ((obj clang) op)
  "Return key used to organize mutations in *mutation-stats* hashtable.
* OBJ object mutation is to be applied to
* OP operation to be performed
"
  ;; Return a list of the mutation type, and the classes of any stmt1 or
  ;; stmt2 arguments.
  (cons
   (type-of op)
   (mapcar [#'ast-class {lookup obj} #'cdr]
           (remove-if-not [#'numberp #'cdr]
                          (remove-if-not [{member _ (list :stmt1 :stmt2)} #'car]
                                         (remove-if-not #'consp (targets op)))))))

;; FIXME: When clang is converted to utilize functional trees,
;; these method specializations will no longer be required.
(defmethod with ((tree clang-ast) (location t) &optional replacement)
  (with tree (ast-path tree location) replacement))

(defmethod with ((tree clang-ast) (location list) &optional replacement)
  (labels        ; TODO: Remove or relocate all of this "fixup" logic.
      ((non-empty (str)
         "Return STR only if it's not empty.

        asts->tree tends to leave dangling empty strings at the ends of child
        list, and we want to treat them as NIL in most cases."
         (when (not (equalp str "")) str))
       (helper (tree path next)
         (bind (((head . tail) path)
                (children (children tree)))
               (if tail
                   ;; The insertion may need to modify text farther up the
                   ;; tree. Pass down the next bit of non-empty text and
                   ;; get back a new string.
                   (multiple-value-bind (child new-next)
                       (helper (nth head children)
                               tail
                               (or (non-empty (nth (1+ head) children))
                                   next))
                     (if (and new-next (non-empty (nth (1+ head) children)))
                         ;; The modified text belongs here. Insert it.
                         (values (copy tree
                                       :children (nconc (subseq children
                                                                0 head)
                                                        (list child new-next)
                                                        (subseq children
                                                                (+ 2 head))))
                                 nil)

                         ;; Otherwise keep passing it up the tree.
                         (values (replace-nth-child tree head child)
                                 new-next)))
                   (let* ((after (nth (1+ head) children))
                          (fixed (fixup-mutation :instead
                                                 (nth head children)
                                                 (if (positive-integer-p head)
                                                     (nth (1- head) children)
                                                     "")
                                                 replacement
                                                 (or (non-empty after) next))))

                     (if (non-empty after)
                         ;; fixup-mutation can change the text after the
                         ;; insertion (e.g. to remove a semicolon). If
                         ;; that text is part of this AST, just include it
                         ;; in the list.
                         (values
                          (copy tree
                                :children (nconc (subseq children
                                                         0 (max 0 (1- head)))
                                                 fixed
                                                 (nthcdr (+ 2 head) children)))
                          nil)

                         ;; If the text we need to modify came from
                         ;; farther up the tree, return it instead of
                         ;; inserting it here.
                         (values
                          (copy tree
                                :children (nconc (subseq children
                                                         0 (max 0 (1- head)))
                                                 (butlast fixed)
                                                 (or (nthcdr (+ 2 head) children)
                                                     '(""))))
                          (lastcar fixed))))))))
    (if location
        (helper tree location nil)
        replacement)))

;; FIXME: When clang is converted to utilize functional trees,
;; these method specializations will no longer be required.
(defmethod less ((tree clang-ast) (location t) &optional arg2)
  (declare (ignorable arg2))
  (less tree (ast-path tree location)))

(defmethod less ((tree clang-ast) (location list) &optional arg2)
  (declare (ignorable arg2))
  (labels
      ((helper (tree path)
         (bind (((head . tail) path)
                (children (children tree)))
               (if tail
                   ;; Recurse into child
                   (replace-nth-child tree head (helper (nth head children) tail))

                   ;; Remove child
                   (copy tree
                         :children (nconc (subseq children 0 (max 0 (1- head)))
                                          (fixup-mutation
                                           :remove
                                           (nth head children)
                                           (if (positive-integer-p head)
                                               (nth (1- head) children)
                                               "")
                                           nil
                                           (or (nth (1+ head) children) ""))
                                          (nthcdr (+ 2 head) children)))))))
    (helper tree location)))

;; FIXME: When clang is converted to utilize functional trees,
;; these method specializations will no longer be required.
(defmethod insert ((tree clang-ast) (location t) (replacement t))
  (insert tree (ast-path tree location) replacement))

(defmethod insert ((tree clang-ast) (location list) (replacement t))
  (labels
      ((helper (tree path)
         (bind (((head . tail) path)
                (children (children tree)))
               (assert (>= head 0))
               (assert (length< head children))
               (if tail
                   ;; Recurse into child
                   (replace-nth-child tree head (helper (nth head children) tail))

                   ;; Insert into children
                   (copy tree
                         :children (nconc (subseq children 0 (max 0 (1- head)))
                                          (fixup-mutation
                                           :before
                                           (nth head children)
                                           (if (positive-integer-p head)
                                               (nth (1- head) children)
                                               "")
                                           replacement
                                           (or (nth head children) ""))
                                          (nthcdr (1+ head) children)))))))
    (helper tree location)))

;; FIXME: When clang is converted to utilize functional trees,
;; these method specializations will no longer be required.
(defmethod splice ((tree clang-ast) (location t) (values list))
  (splice tree (ast-path tree location) values))

(defmethod splice ((tree clang-ast) (location list) (values list))
  (labels
      ((helper (tree path)
         (bind (((head . tail) path)
                (children (children tree)))
               (if tail
                   ;; Recurse into child
                   (replace-nth-child tree head
                                      (helper (nth head children) tail))
                   ;; Splice into children
                   (copy tree
                         :children (nconc (subseq children 0 head)
                                          values
                                          (nthcdr (1+ head) children)))))))
    (helper tree location)))

;; FIXME: When clang is converted to utilize functional trees,
;; this method specialization will no longer be required.
(defgeneric replace-nth-child (ast n replacement)
  (:documentation "Return AST with the nth child of AST replaced with
REPLACEMENT.

* AST tree to modify
* N child to modify
* REPLACEMENT replacement for the nth child")
  (:method ((ast clang-ast) (n integer) replacement)
    (replace-nth-child ast n (list replacement)))
  (:method ((ast clang-ast) (n integer) (replacement list))
    (copy ast
          :children (append (subseq (children ast) 0 n)
                            replacement
                            (subseq (children ast) (+ 1 n))))))

(defgeneric fixup-mutation (operation current before ast after)
  (:documentation "Adjust mutation result according to syntactic context.")
  (:method (operation (current clang-ast) before ast after)
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
              (:toplevel (add-semicolon-if-unbraced)))))))

;; FIXME: When clang is converted to utilize functional trees,
;; these method specializations will no longer be required.
(defmethod lookup ((ast clang-ast) (path list))
  (lookup (lookup ast (car path)) (cdr path)))

(defmethod lookup ((ast clang-ast) (i integer))
  (elt (children ast) i))

(defmethod lookup ((ast clang-ast) (path null)) ast)

(defmethod ends-with-semicolon ((str string))
  (let ((trimmed (string-right-trim (list #\Space #\Tab #\Newline
                                          #\Return #\Linefeed #\Page)
                                    str)))
    (when (ends-with #\; trimmed)
      (subseq trimmed 0 (1- (length trimmed))))))

(defmethod ends-with-semicolon (obj)
  (declare (ignorable obj))
  nil)

(defgeneric remove-semicolon (obj)
  (:documentation "Removes the trailing semicolon from an AST, returning a new AST node.
If there is no trailing semicolon, return the AST unchanged."))

(defmethod remove-semicolon ((obj clang-ast))
  (let* ((children (children obj))
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
  (:method ((ast clang-ast))
    (member (ast-class ast)
            '(:Function :CXXMethod :CXXConstructor :CXXDestructor)))
  (:method (x) (declare (ignorable x)) nil))

(defgeneric function-body (ast)
  (:documentation
   "If AST is a function, return the AST representing its body.
* AST potential function AST to query for its body
")
  (:method ((ast clang-ast))
    (when (function-decl-p ast)
      (find-if [{eq :CompoundStmt} #'ast-class]
               (child-asts ast)))))

(defgeneric get-entry (software)
  (:documentation
   "Return the AST of the entry point (main function) in SOFTWARE,
or NIL if there is no entry point.")
  (:method ((soft software)) nil)
  (:method ((obj clang))
    (when-let* ((main (find-if [{name= "main"} {ast-name}]
                               (functions obj)))
                (return-type (find-type obj (ast-ret main)))
                (_ (and (equal :Function (ast-class main))
                        (member (type-name return-type)
                                '("int" "void") :test #'name=))))
      (function-body main))))

(defmethod get-parent-full-stmt ((clang clang) (ast clang-ast))
  "Return the first ancestor of AST in SOFTWARE which is a full stmt.
Returns nil if no full-stmt is found.
* CLANG software object containing AST and its parents
* AST to find the parent full statement of if not already a full statement
"
  (cond ((ast-full-stmt ast) ast)
        (ast (get-parent-full-stmt clang (get-parent-ast clang ast)))))

(defgeneric stmt-range (software function)
  (:documentation
   "The indices of the first and last statements in a function.
Return as a list of (first-index last-index). Indices are positions in
the list returned by (asts software).")
  (:method ((software clang) (function clang-ast))
    (labels
        ((rightmost-child (ast)
           (if-let ((children (child-asts ast)))
             (rightmost-child (lastcar children))
             ast)))
      (when-let ((body (function-body function)))
        (mapcar {index-of-ast software}
                (list body (rightmost-child body)))))))

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

(defmethod wrap-ast ((obj clang) (ast clang-ast))
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

(defmethod wrap-child ((obj clang) (ast clang-ast) (index integer))
  "DOCFIXME
* OBJ DOCFIXME
* AST DOCFIXME
* INDEX DOCFIXME
"
  (if (member (ast-class ast) +clang-wrapable-parents+)
      (wrap-ast obj (nth index (child-asts ast)))
      (error "Will not wrap children of type ~a, only useful for ~a."
             (ast-class ast) +clang-wrapable-parents+))
  obj)

(defmethod nesting-depth ((clang clang) stmt &optional orig-depth)
  "DOCFIXME
* CLANG DOCFIXME
* STMT DOCFIXME
* ORIG-DEPTH DOCFIXME
"
  (let ((depth (or orig-depth 0)))
    (if (null stmt)
        depth
        (nesting-depth clang (enclosing-block clang stmt) (1+ depth)))))

(defmethod enclosing-block ((clang clang) (ast clang-ast))
  "DOCFIXME
* CLANG DOCFIXME
* AST DOCFIXME
"
  ;; First parent AST is self, skip over that.
  (find-if #'block-p (cdr (get-parent-asts clang ast))))

(defgeneric block-p (statement)
  (:documentation "Check if STATEMENT is a block."))

(defmethod block-p ((stmt clang-ast))
  "DOCFIXME
* OBJ DOCFIXME
* STMT DOCFIXME
"
  (or (eq :CompoundStmt (ast-class stmt))
      (and (member (ast-class stmt) +clang-wrapable-parents+)
           (not (null (nest (remove-if «or #'ast-guard-stmt
                                           [{eq :CompoundStmt}
                                            #'ast-class]»)
                            (child-asts stmt)))))))

(defgeneric enclosing-full-stmt (software stmt)
  (:documentation
   "Return the first full statement in SOFTWARE holding STMT."))

(defmethod enclosing-full-stmt ((obj clang) (stmt clang-ast))
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

(defmethod block-successor ((clang clang) (ast clang-ast))
  "DOCFIXME
* CLANG DOCFIXME
* AST DOCFIXME
"
  (let* ((full-stmt (enclosing-full-stmt clang ast))
         (the-block (enclosing-block clang full-stmt))
         (the-stmts (remove-if-not «or #'block-p #'ast-full-stmt»
                                   (child-asts the-block))))
    (get-entry-after full-stmt the-stmts)))

(defmethod block-predeccessor ((clang clang) (ast clang-ast))
  "DOCFIXME
* CLANG DOCFIXME
* AST DOCFIXME
"
  (let* ((full-stmt (enclosing-full-stmt clang ast))
         (the-block (enclosing-block clang full-stmt))
         (the-stmts (remove-if-not «or #'block-p #'ast-full-stmt»
                                   (child-asts the-block))))
    (get-entry-before full-stmt the-stmts)))

(defmethod full-stmt-predecessors ((clang clang) (ast clang-ast)
                                   &optional acc blocks)
  "All full statements and blocks preceding AST.

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

(defmethod tree-successors ((obj clang) (ast clang-ast) (ancestor clang-ast)
                            &key include-ast)
  "Find all successors of AST within subtree at ANCESTOR.

Returns ASTs and text snippets, grouped by depth. AST itself is
included as the first successor."
  (labels
      ((successors (ast path)
         (bind (((head . tail) path)
                (children (children ast)))
           (if tail
               (cons (subseq children (1+ head))
                     (successors (nth head children) tail))
               (list (subseq children (if include-ast head (1+ head))))))))
    (nest (reverse)
          (successors ancestor)
          (subseq (ast-path obj ast) (length (ast-path obj ancestor))))))

(defmethod update-headers-from-snippet ((clang clang) snippet database)
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
  (member (ast-class ast)
          '(:CompoundStmt :Block :Captured :Function :CXXMethod)))

(defmethod enclosing-scope ((software clang) (ast clang-ast))
  "DOCFIXME
* SOFTWARE DOCFIXME
* AST DOCFIXME
"
  (or (find-if #'begins-scope
               (cdr (get-parent-asts software ast)))
      ;; Global scope
      (genome software)))

(defmethod nth-enclosing-scope ((software clang)
                                (depth integer)
                                (ast clang-ast))
  "DOCFIXME
* SOFTWARE DOCFIXME
* DEPTH DOCFIXME
* AST DOCFIXME
"
  (let ((scope (enclosing-scope software ast)))
    (if (>= 0 depth) scope
        (nth-enclosing-scope software (1- depth) scope))))

(defmethod scopes ((software clang) (ast clang-ast))
  "DOCFIXME
* SOFTWARE DOCFIXME
* AST DOCFIXME
"
  ;; Stop at the root AST
  (when (not (eq :TopLevel (ast-class ast)))
    (let ((scope (enclosing-scope software ast)))
      (cons (nest (reverse)
                  ; drop nils and empty strings
                  (remove-if #'emptyp)
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
                  ; remove type and function decls
                  (remove-if-not [{member _ '(:Var :ParmVar)}
                                  #'ast-class])
                  ; expand decl statements
                  (mappend
                   (lambda (ast)
                     (cond ((eq :DeclStmt (ast-class ast))
                            (child-asts ast))
                           (t (list ast)))))
                  ; get children in scope
                  (iter (for c in (child-asts scope))
                        (while (path-later-p software ast c))
                        (collect c)))
            (scopes software scope)))))

(defmethod get-vars-in-scope :around ((obj clang) (ast clang-ast)
                                      &optional (keep-globals t))
  "Return all variables in enclosing scopes.
* OBJ clang software object containing AST and its enclosing scopes
* AST node to find variables in scope for"
  ;; Remove duplicate variable names from outer scopes. Only the inner variables
  ;; are accessible.
  (remove-duplicates (call-next-method obj ast keep-globals)
                     :from-end t
                     :key {aget :name}
                     :test #'name=))

(defmethod get-ast-types ((software clang) (ast clang-ast))
  "Compute all the types mentioned in AST.  AST-TYPES is
the types used at a node; this function closes over all the nodes
in the AST.  SOFTWARE is the software object to which AST belongs."
  (remove-duplicates (apply #'append (ast-types ast)
                            (mapcar {get-ast-types software}
                                    (child-asts ast)))
                     :key #'type-hash))

(defmethod get-unbound-funs ((software clang) (ast clang-ast))
  "Compute all the unbound funs in AST.   AST-UNBOUND-FUNS is
the unbound funs at a node; this function closes over all the nodes
in the AST.  SOFTWARE is the software object to which AST belongs."
  (remove-duplicates (apply #'append (ast-unbound-funs ast)
                            (mapcar {get-unbound-funs software}
                                    (child-asts ast)))
                     :test #'equal))

(defmethod get-unbound-vals ((software clang) (ast clang-ast) &key)
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
         (iter (for c in (child-asts ast))
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
      (nest (mapcar (lambda (name)
                      (or (find name in-scope :test #'name= :key {aget :name})
                          `((:name . ,name)))))
            (remove-duplicates (walk-scope ast nil (list nil))
                               :test #'name=)))))

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
  "Returns funmction info in the same format as unbound-funs"
  (when-let ((name (random-function-proto protos :original-name original-name
                                          :arity arity)))
    (list name nil (ast-varargs name) (length (ast-args name)))))

(defun random-function-proto (protos &key original-name arity)
  (let ((matching '())
        (variadic '())
        (others   '())
        (saw-orig nil))
    (loop :for proto :in protos
       :do (let ((args (length (ast-args proto))))
             (when (name= proto original-name)
               (setf saw-orig t))
             (cond
               ((= args arity) (push proto matching))
               ((and (< args arity)
                     (ast-varargs proto))
                (push proto variadic))
               (t (push proto others)))))
    (if (and saw-orig (< (random 1.0) *matching-free-function-retains-name-bias*))
        original-name
        (random-elt (or matching variadic others '(nil))))))

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

(defmethod bind-free-vars ((clang clang) (ast clang-ast) (pt clang-ast))
  "DOCFIXME
* CLANG DOCFIXME
* AST DOCFIXME
* PT DOCFIXME
"
  (let* ((in-scope (mapcar {aget :name} (get-vars-in-scope clang pt)))
         (var-replacements
          (mapcar (lambda (var)
                    (let ((name (aget :name var)))
                      (list name (binding-for-var clang in-scope
                                                  name))))
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
  (case (ast-class ast)
    (:macroexpansion
     ;; Revert back to string-based rebinding
     (let ((new-children
            (mapcar (lambda (s)
                      (reduce
                       (lambda-bind (text (old new))
                         (regex-replace-all
                          (format nil "(^|[^A-Za-z0-9_]+)~
                                         (~a)~
                                         ([^A-Za-z0-9_]+|$)"
                                  (ast-name old))
                          text
                          (format nil "\\1~a\\3" (ast-name new))))
                       (append var-replacements
                               (mapcar (lambda-bind ((oldf newf))
                                         (list (first oldf) (first newf)))
                                       fun-replacements))
                       :initial-value s))
                    (children ast))))
       (if (equal (children ast) new-children)
           ast
           (copy ast :children new-children))))
    (:DeclRefExpr
     (iter (for (old new) in var-replacements)
           (when (eql (ast-referenceddecl ast) old)
             (setf ast (copy ast :referenceddecl new
                             :children (list (ast-name new))))))
     (iter (for (oldf newf) in fun-replacements)
           (when (eql (ast-referenceddecl ast) (first oldf))
             (setf ast (copy ast :referenceddecl (first newf)))))
     ast)
    (t (let ((c (mapcar (lambda (c)
                          (cond ((stringp c) c)
                                (t (rebind-vars c var-replacements
                                                fun-replacements))))
                        (children ast))))
         (if (every #'eql c (children ast))
             ast
             (copy ast :children c))))))

(defgeneric delete-decl-stmts (software block replacements)
  (:documentation
   "Return mutation ops applying REPLACEMENTS to BLOCK in SOFTWARE.
REPLACEMENTS is a list holding lists of an ID to replace, and the new
variables to replace use of the variables declared in stmt ID."))

(defmethod delete-decl-stmts ((obj clang) (block clang-ast) (replacements list))
  "DOCFIXME
* OBJ DOCFIXME
* BLOCK DOCFIXME
* REPLACEMENTS DOCFIXME
"
  (append
   ;; Rewrite those stmts in the BLOCK which use an old variable.
   (let* ((old->new      ; First collect a map of old-name -> new-name.
           (mappend (lambda-bind ((id . replacements))
                      (mapcar #'list (ast-declares id) replacements))
                    replacements)))
     ;; Collect statements using old
     (nest (mapcar (lambda (ast)
                     (list :set (cons :stmt1 ast)
                           (cons :literal1
                                 (rebind-vars ast old->new nil)))))
           (remove-if-not (lambda (ast)      ; Only Statements using old.
                            (intersection
                             (get-used-variables obj ast)
                             (mapcar #'car old->new)
                             :test #'name=)))
           (child-asts block)))
   ;; Remove the declaration.
   (mapcar [{list :cut} {cons :stmt1} #'car] replacements)))

(defmethod get-declared-variables ((the-block clang-ast))
  "DOCFIXME
* CLANG DOCFIXME
* THE-BLOCK DOCFIXME
"
  (mappend #'ast-declares (child-asts the-block)))

(defmethod get-used-variables ((clang clang) (stmt clang-ast))
  "DOCFIXME
* CLANG DOCFIXME
* STMT DOCFIXME
"
  (mapcar {aget :name} (get-unbound-vals clang stmt)))

(defmethod get-children-using ((clang clang) var the-block)
  "DOCFIXME
* CLANG DOCFIXME
* VAR DOCFIXME
* THE-BLOCK DOCFIXME
"
  (remove-if-not [(lambda (el) (find var el :test #'equal))
                  {get-used-variables clang}]
                 (child-asts the-block)))

(defmethod nth-enclosing-block ((clang clang) (depth integer) (stmt clang-ast))
  "DOCFIXME
* CLANG DOCFIXME
* DEPTH DOCFIXME
* STMT DOCFIXME
"
  (let ((the-block (enclosing-block clang stmt)))
    (if (>= 0 depth) the-block
        (nth-enclosing-block clang (1- depth) the-block))))

(defmethod get-function-from-function-call ((obj clang) (callexpr clang-ast))
  (match callexpr
    ((clang-ast
      (ast-class :callexpr)
      (children
       (list* (type string)
              (clang-ast
               (ast-class :implicitcastexpr)
               (children
                (list* (type string)
                       declref
                       _)))
              _)))
     (and (eq :declrefexpr (ast-class declref))
          (ast-referenceddecl declref)))))


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
             (bind ((children (children root))
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
                 (collect (copy parent :children (nconcf (children parent)
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
    ((a clang) (b clang)
     (a-begin clang-ast) (a-end clang-ast)
     (b-begin clang-ast) (b-end clang-ast))
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
         (b-stmts (tree-successors b b-begin
                                   (nest (get-parent-ast b)
                                         (common-ancestor b b-begin b-end))
                                   :include-ast t))
         (value1 (recontextualize a
                                  (or (fill-crossover-context context b-stmts)
                                      (car (car b-stmts)))
                                  a-begin)))

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
    ((a clang) (b clang)
     (a-begin clang-ast) (a-end clang-ast)
     (b-begin clang-ast) (b-end clang-ast))
  "DOCFIXME
* A DOCFIXME
* B DOCFIXME
* A-BEGIN DOCFIXME
* A-END DOCFIXME
* B-BEGIN DOCFIXME
* B-END DOCFIXME
"
  (labels
      ((child-index (obj parent child)
         "Position of CHILD within PARENT."
         (assert (equal (ast-path obj parent)
                        (butlast (ast-path obj child))))
         (lastcar (ast-path obj child)))
       (outer-ast (obj begin end)
         "AST which strictly encloses BEGIN and END."
         (let ((ancestor (common-ancestor obj begin end)))
           (if (equalp ancestor begin)
               (get-parent-ast obj ancestor)
               ancestor)))
       (splice-ast (a-outer b-outer b-inner b-ast)
         ;; Splice b-ast into a-outer.
         (bind ((children (children a-outer))
                (a-index1 (child-index a a-outer a-begin))
                (a-index2 (1+ (child-index a a-outer
                                           (ancestor-after a a-outer a-end))))
                (b-index1 (child-index b b-outer b-begin))
                (b-index2 (child-index b b-outer b-inner)))
               (let ((new-children
                      (append
                         ;; A children before the crossover
                         (subseq children 0 a-index1)
                         ;; B children up to the inner ast
                         (subseq (children b-outer)
                                 b-index1
                                 (if b-ast b-index2 (1+ b-index2)))
                         ;; The inner ast if it exists
                         (when b-ast (list b-ast))
                         ;; A children after the crossover
                         (subseq children a-index2))))
                 (copy a-outer
                       :path nil
                       :children new-children)))))
    (let* ((a-outer (outer-ast a a-begin a-end))
           (b-outer (outer-ast b b-begin b-end))
           (b-inner (ancestor-after b b-outer b-end))
           (context (create-crossover-context b b-inner b-end :include-start t))
           (a-stmts (nest (tree-successors a a-end)
                          (get-parent-ast a)
                          (common-ancestor a a-begin a-end)))
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
          (let* ((inner-path (ast-path obj inner-stmt))
                 (outer-path (ast-path obj outer-stmt))
                 (rel-path (subseq inner-path (length outer-path))))
            (setf outer-stmt (with outer-stmt rel-path value)))))

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

(defmethod update-headers-from-ast ((clang clang) (ast clang-ast) database)
  "Walk the ast AST in clang object CLANG, adding includes, macros, and types
that are mentioned at nodes of the AST.  DATABASE is the associated macro/type
database."
  (labels
      ((update (ast)
         (mapc {add-include clang}
               (reverse (ast-includes clang ast)))
         (mapc [{add-macro clang} {find-macro database}]
               (reverse (ast-macros ast)))
         (mapc [{add-type clang} {find-type database}]
               (reverse (ast-types ast)))
         (mapc #'update (remove-if-not {typep _ 'clang-ast}
                                       (children ast)))))
    (update ast)))

;; Find the ancestor of STMT that is a child of ANCESTOR.
;; On failure, just return STMT again.
(defmethod ancestor-after ((clang clang) (ancestor clang-ast) (stmt clang-ast))
  "DOCFIXME
* CLANG DOCFIXME
* ANCESTOR DOCFIXME
* STMT DOCFIXME
"
  (or (nest (find-if [{equalp ancestor} {get-parent-ast clang}])
            (get-parent-asts clang stmt))
      stmt))

(defmethod common-ancestor ((clang clang) (x clang-ast) (y clang-ast))
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

(defmethod ancestor-of ((clang clang) (x clang-ast) (y clang-ast))
  "DOCFIXME
* CLANG DOCFIXME
* X DOCFIXME
* Y DOCFIXME
"
  (equalp (common-ancestor clang x y) x))

(defmethod scopes-between ((clang clang) (stmt clang-ast) (ancestor clang-ast))
  "DOCFIXME
* CLANG DOCFIXME
* STMT DOCFIXME
* ANCESTOR DOCFIXME
"
  (iter (for ast in (get-parent-asts clang stmt))
        (counting (block-p ast))
        (until (equalp ast ancestor))))

(defmethod nesting-relation ((clang clang) x y)
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
(defmethod split-vee ((clang clang) (x clang-ast) (y clang-ast))
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

(defmethod match-nesting ((a clang) xs (b clang) ys)
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

(defmethod intraprocedural-2pt-crossover ((a clang) (b clang)
                                          (a-begin clang-ast) (a-end clang-ast)
                                          (b-begin clang-ast) (b-end clang-ast))
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

(defmethod adjust-stmt-range ((clang clang) start end)
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

(defmethod random-point-in-function ((clang clang) (function clang-ast))
  "DOCFIXME
* CLANG DOCFIXME
* FUNCTION DOCFIXME
"
  (destructuring-bind (&optional first last) (stmt-range clang function)
    (if (equal first last) nil
        (+ (1+ first) (random (- last first))))))

(defgeneric select-intraprocedural-pair (software)
  (:documentation
   "Randomly select an AST within a function body and then select
another point within the same function.  If there are no ASTs
within a function body, return null."))

(defmethod select-intraprocedural-pair ((clang clang))
  "DOCFIXME
* CLANG DOCFIXME
"
  (when-let (stmt1 (some->> (remove-if {function-body-p clang} (stmt-asts clang))
                            (random-elt)))
    (values (index-of-ast clang stmt1)
            (random-point-in-function
             clang
             (function-containing-ast clang stmt1)))))

(defmethod select-crossover-points ((a clang) (b clang))
  "DOCFIXME
* A DOCFIXME
* B DOCFIXME
"
  (multiple-value-bind (a-stmt1 a-stmt2)
      (select-intraprocedural-pair a)
    (multiple-value-bind (b-stmt1 b-stmt2)
        (select-intraprocedural-pair b)
      (values a-stmt1 a-stmt2 b-stmt1 b-stmt2))))

(defmethod select-crossover-points-with-corrections ((a clang) (b clang))
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

(defmethod crossover ((a clang) (b clang))
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

(defmethod function-containing-ast ((clang clang) (ast clang-ast))
  "Return the function in CLANG containing AST.
* CLANG software object containing AST and its parent function
* AST ast to search for the parent function of
"
  (find-if #'function-decl-p (get-parent-asts clang ast)))

(defmethod function-body-p ((clang clang) (stmt clang-ast))
  "Return true if stmt AST if a function body, nil otherwise.
* CLANG software object containing STMT
* STMT ast to test if a function body
"
  (find-if [{equalp stmt} #'function-body] (functions clang)))


;;; Implement the generic format-genome method for clang objects.
(defmethod format-genome ((obj clang) &key)
  "Apply Artistic Style to OBJ to format the software."
  (astyle obj))


;;; Support for parsing a string directly into free-floating ASTs.
(defmethod convert ((to-type (eql 'clang-ast)) (collection string)
                    &key unbound-vals includes macros preamble
                      top-level keep-comments)
  "Build ASTs for COLLECTION, returning a list of root asts.

* COLLECTION snippet including one or more full statements. It should compile
  in a context where all UNBOUND-VALS are defined and all INCLUDES are
  included.

* UNBOUND-VALS should have the form ((name clang-type) ... )

* INCLUDES is a list of files to include.

* MACROS is a list of macros to define

* PREAMBLE source to add prior to collection

* TOP-LEVEL indicates that the snippet is a construct which can exist
  outside a function body, such as a type or function declaration.

* KEEP-COMMENTS indicates comments should be retained
"
  (labels ((has-comment-p (text)
             (and (stringp text)
                  (or (and (search "/*" text)
                           (search "*/" text))
                      (search "//" text))))
           (trim-stmt-end (text)
             (string-left-trim '(#\Space #\Backspace #\Tab #\;)
                                text))
           (prepend-text (ast text &aux (children (children ast)))
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
                                collection))
               (obj (make-instance 'clang
                      :flags (list "-Wno-everything")
                      :genome (concatenate 'string
                                           dependency-source
                                           wrapped)))
               (block-children
                 (if top-level
                     (mapcar #'copy (child-asts (genome obj)))
                     (iter (for child in (nest (children)
                                               (function-body)
                                               (lastcar)
                                               (functions obj)))
                           (for prev previous child)
                           (when (typep child 'clang-ast)
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

(defmethod convert ((to-type (eql 'clang-ast)) (spec list)
                    &key &allow-other-keys)
  "Create a clang AST from the SPEC (specification) list.

SPEC: List specification of an AST.  A SPEC should have the form

  (ast-class <optional-keyword-args-to-`make-instance <AST-TYPE>'>
             CHILDREN)

where CHILDREN may themselves be specifications suitable for passing
to `convert`"
  (convert-list-to-ast-helper spec
                              (lambda (class keys children)
                                (apply #'make-instance 'clang-ast
                                 :class class
                                 :children children
                                 keys))))

(defun convert-list-to-ast-helper (spec fn)
  "Helper function for converting a list SPECification of an AST to an
AST using FN to create the AST.

SPEC: List specification of an AST.  A SPEC should have the form

  (ast-class <optional-keyword-args-to-`make-instance <AST-TYPE>'>
             CHILDREN)

where CHILDREN may themselves be specifications suitable for passing
to `convert-list-to-ast-helper`

FN: Function taking three arguments (class, keys, and children) and
returning a newly created AST."
  (destructuring-bind (class &rest options-and-children) spec
    (multiple-value-bind (keys children)
        (let ((previous nil))
          (iter (for item in options-and-children)
                (if (or (keywordp previous)
                        (keywordp item))
                    ;; Collect keyword arguments.
                    (collect item into keys)
                    ;; Process lists as new AST nodes.
                    (if (listp item)
                        (collect (convert-list-to-ast-helper item fn)
                                 into children)
                        (collect item into children)))
                (setf previous item)
                (finally (return (values keys children)))))
      (funcall fn class keys children))))


;;; Structures and functions relating to genome locations.

;;; TODO: I would like to re-work all of this to inherit from the
;;;       existing source ranges.
(defstruct clang-loc
  "Structure used to represent a location within a clang-parseable file."
  (file nil :type (or null string))
  (included-from nil :type (or null string))
  (line nil :type (or null integer))
  (presumed-line nil :type (or null integer))
  (col nil :type (or null integer))
  (offset 0 :type (or null integer))
  (tok-len 0 :type (or null integer)))

(defstruct clang-macro-loc
  "Structure used to represent :begin/:end entries for
things in macro expansion.  SPELLING-LOC is the location
in the macro defn, EXPANSION-LOC is at the macro use."
  (spelling-loc nil :type (or null clang-loc))
  (expansion-loc nil :type (or null clang-loc))
  (is-macro-arg-expansion nil :type boolean))

(defstruct clang-range
  "Structure used to represent the begin and end location of an AST."
  (begin nil :type (or null clang-loc clang-macro-loc))
  (end nil :type (or null clang-loc clang-macro-loc)))

(defmethod copy ((obj clang-loc)
                 &key (file nil file-supplied-p)
                   (included-from nil included-from-supplied-p)
                   (line nil line-supplied-p)
                   (presumed-line nil presumed-line-supplied-p)
                   (col nil col-supplied-p)
                   (offset 0 offset-supplied-p)
                   (tok-len 0 tok-len-supplied-p))
  (make-clang-loc
   :file (if file-supplied-p file (copy-seq (clang-loc-file obj)))
   :included-from (if included-from-supplied-p
                      included-from
                      (clang-loc-included-from obj))
   :line (if line-supplied-p line (clang-loc-line obj))
   :presumed-line (if presumed-line-supplied-p
                      presumed-line
                      (clang-loc-presumed-line obj))
   :col (if col-supplied-p col (clang-loc-col obj))
   :offset (if offset-supplied-p offset (clang-loc-offset obj))
   :tok-len (if tok-len-supplied-p tok-len (clang-loc-tok-len obj))))

(defmethod copy ((obj clang-macro-loc)
                 &key (spelling-loc nil spelling-loc-supplied-p)
                   (expansion-loc nil expansion-loc-supplied-p)
                   (is-macro-arg-expansion nil
                                           is-macro-arg-expansion-supplied-p))
  (make-clang-macro-loc
   :spelling-loc (if spelling-loc-supplied-p
                     spelling-loc
                     (copy (clang-macro-loc-spelling-loc obj)))
   :expansion-loc (if expansion-loc-supplied-p
                      expansion-loc
                      (copy (clang-macro-loc-expansion-loc obj)))
   :is-macro-arg-expansion (if is-macro-arg-expansion-supplied-p
                               is-macro-arg-expansion
                               (clang-macro-loc-is-macro-arg-expansion
                                obj))))

(defmethod copy ((obj clang-range)
                 &key (begin nil begin-supplied-p)
                   (end nil end-supplied-p))
  (make-clang-range
   :begin (if begin-supplied-p begin (copy (clang-range-begin obj)))
   :end (if end-supplied-p end (copy (clang-range-end obj)))))

(defgeneric spelling-loc-has-source-text-p (loc)
  (:documentation "Return TRUE if loc represents a macro location where
the source text may be found in the spelling location instead of the
expansion location.  This is the case for macro arguments where the
spelling location comes after the expansion location.")
  (:method ((loc clang-macro-loc))
    (and (clang-macro-loc-is-macro-arg-expansion loc)
         (< (offset (clang-macro-loc-expansion-loc loc))
            (offset (clang-macro-loc-spelling-loc loc))))))

;;; TODO: This method should be removed when we unify with sel/utility/range:range
;;;       as its name conflicts with the line method there.
(defmethod line ((obj clang-loc))
  (clang-loc-line obj))
(defmethod line ((obj clang-macro-loc))
  (line (if (and (clang-macro-loc-is-macro-arg-expansion obj)
                 (< (offset (clang-macro-loc-expansion-loc obj))
                    (offset (clang-macro-loc-spelling-loc obj))))
            (clang-macro-loc-spelling-loc obj)
            (clang-macro-loc-expansion-loc obj))))

(defgeneric offset (obj)
  (:method ((obj clang-loc))
    (clang-loc-offset obj))
  (:method ((obj clang-macro-loc))
    (if (spelling-loc-has-source-text-p obj)
        (offset (clang-macro-loc-spelling-loc obj))
        (offset (clang-macro-loc-expansion-loc obj)))))

(defgeneric (setf offset) (offset obj)
  (:method ((offset integer) (obj clang-loc))
    (setf (clang-loc-offset obj) offset))
  (:method ((offset integer) (obj clang-macro-loc))
    (setf (offset (if (spelling-loc-has-source-text-p obj)
                      (clang-macro-loc-spelling-loc obj)
                      (clang-macro-loc-expansion-loc obj))) offset)))

(defgeneric tok-len (obj)
  (:method ((obj clang-loc)) (clang-loc-tok-len obj))
  (:method ((obj clang-macro-loc))
    (tok-len (if (spelling-loc-has-source-text-p obj)
                 (clang-macro-loc-spelling-loc obj)
                 (clang-macro-loc-expansion-loc obj)))))

(defgeneric (setf tok-len) (tok-len obj)
  (:method ((tok-len integer) (obj clang-loc))
    (setf (clang-loc-tok-len obj) tok-len))
  (:method ((tok-len integer) (obj clang-macro-loc))
    (setf (tok-len (if (spelling-loc-has-source-text-p obj)
                       (clang-macro-loc-spelling-loc obj)
                       (clang-macro-loc-expansion-loc obj))) tok-len)))

(defgeneric begin-offset (obj)
  (:method ((obj clang-ast))
    (when-let ((range (ast-range obj)))
      (begin-offset range)))
  (:method ((obj clang-range))
    (offset (clang-range-begin obj))))

(defgeneric (setf begin-offset) (offset obj)
  (:method ((offset integer) (obj clang-ast))
    (setf (begin-offset (ast-range obj)) offset))
  (:method ((offset integer) (obj clang-range))
    (setf (offset (clang-range-begin obj)) offset)))

(defgeneric begin-tok-len (obj)
  (:method ((obj clang-ast))
    (when-let ((range (ast-range obj)))
      (begin-tok-len range)))
  (:method ((obj clang-range))
    (tok-len (clang-range-begin obj))))

(defgeneric (setf begin-tok-len) (tok-len obj)
  (:method ((tok-len integer) (obj clang-ast))
    (setf (begin-tok-len (ast-range obj)) tok-len))
  (:method ((tok-len integer) (obj clang-range))
    (setf (tok-len (clang-range-begin obj)) tok-len)))

(defgeneric end-offset (obj)
  (:method ((obj clang-ast))
    (when-let ((range (ast-range obj)))
      (end-offset range)))
  (:method ((obj clang-range))
    (offset (clang-range-end obj))))

(defgeneric (setf end-offset) (offset obj)
  (:method ((offset integer) (obj clang-ast))
    (setf (end-offset (ast-range obj)) offset))
  (:method ((offset integer) (obj clang-range))
    (setf (offset (clang-range-end obj)) offset)))

(defgeneric end-tok-len (obj)
  (:method ((obj clang-ast))
    (when-let ((range (ast-range obj)))
      (end-tok-len range)))
  (:method ((obj clang-range))
    (tok-len (clang-range-end obj))))

(defgeneric (setf end-tok-len) (tok-len obj)
  (:method ((tok-len integer) (obj clang-ast))
    (setf (end-tok-len (ast-range obj)) tok-len))
  (:method ((tok-len integer) (obj clang-range))
    (setf (tok-len (clang-range-end obj)) tok-len)))

;;; The end offset is one past the last character in the source text
;;; for the ast
(defgeneric begin-and-end-offsets (x)
  (:method ((obj clang-ast))
    (when-let ((range (ast-range obj)))
      (begin-and-end-offsets range)))
  (:method ((obj clang-range))
    (values (begin-offset obj)
            (+ (end-offset obj) (end-tok-len obj)))))

;;; TODO: Shouldn't this all be implemented on parseable?  I imagine
;;;       we would like to have similar functionality for JavaScript
;;;       (and friends) as well?
(defgeneric file (obj &optional macro?)
  (:documentation "Return the file name associated with OBJ.
If MACRO? is non-nil, return the file name associated with the macro
definition, if applicable.")
  (:method ((obj clang-ast) &optional macro?)
    (when-let ((range (ast-range obj)))
      (file range macro?)))
  (:method ((obj clang-range) &optional macro?)
    (file (clang-range-begin obj) macro?))
  (:method ((obj clang-loc) &optional macro?)
    (declare (ignorable macro?))
    (clang-loc-file obj))
  (:method ((obj clang-macro-loc) &optional macro?)
    (file (if (if (spelling-loc-has-source-text-p obj)
                  (not macro?)
                  macro?)
              (clang-macro-loc-spelling-loc obj)
              (clang-macro-loc-expansion-loc obj))
          macro?))
  (:method (obj &optional macro?)
    (declare (ignorable obj macro?)) nil))

(defgeneric included-from (obj &optional macro?)
  (:documentation "Return the file name which included the header containing
OBJ.  If MACRO? is non-nil, return the file name associated with the macro
definition, if applicable.")
  (:method ((obj clang-ast) &optional macro?)
    (when-let ((range (ast-range obj)))
      (included-from range macro?)))
  (:method ((obj clang-range) &optional macro?)
    (included-from (clang-range-begin obj) macro?))
  (:method ((obj clang-loc) &optional macro?)
    (declare (ignorable macro?))
    (clang-loc-included-from obj))
  (:method ((obj clang-macro-loc) &optional macro?)
    (included-from (if (if (spelling-loc-has-source-text-p obj)
                           (not macro?)
                           macro?)
                       (clang-macro-loc-spelling-loc obj)
                       (clang-macro-loc-expansion-loc obj))
                   macro?))
  (:method (obj &optional macro?)
    (declare (ignorable obj macro?)) nil))


;;; AST fields

(defmethod ast-name ((x null)) nil)
(defmethod ast-name ((s string)) s)
(defmethod ast-name ((obj clang-ast)) (ast-annotation obj :name))

(defmethod ast-in-macro-expansion ((obj clang-ast))
  (eql (ast-class obj) :macroexpansion))

(defgeneric ast-is-implicit (ast)
  (:method ((ast t)) nil)
  (:method ((ast clang-ast))
    (or (ast-annotation ast :isimplicit)
        (ast-annotation ast :implicit))))

(defgeneric ast-is-class (ast key)
  (:method ((ast t) (class t)) nil)
  (:method ((ast clang-ast) (key symbol))
     (eql (ast-class ast) key)))

(defmethod ast-unbound-vals ((ast clang-ast))
  (ast-unbound-vals* ast (ast-class ast)))

(defmethod ast-unbound-vals ((str string))
  (declare (ignore str))
  nil)

(defgeneric ast-unbound-vals* (ast class)
  (:documentation "Implementation function for ast-unbound-vals,
where class = (ast-class ast)."))

(defmethod ast-unbound-vals* ((ast clang-ast) (class (eql :declrefexpr)))
  (when-let ((obj (ast-referenceddecl ast)))
    (when (member (ast-class obj) '(:Var :ParmVar))
      (list obj))))

(defmethod ast-unbound-vals* ((ast clang-ast) (class (eql :macroexpansion)))
  (let ((children (ast-annotation ast :macro-child-segment))
        (bound nil)
        (unbound nil))
    (dolist (c children)
      (map-ast c (lambda (a)
                   (setf bound (append (ast-declarations a) bound))
                   (setf unbound (append (ast-unbound-vals a) unbound)))))
    (set-difference (remove-duplicates unbound)
                    (remove-duplicates bound))))

(defmethod ast-unbound-vals* ((ast clang-ast) (class t))
  nil)

(defgeneric ast-bound-vals (ast)
  (:documentation "Vars that are bound by an AST")
  (:method ((x string)) (declare (ignore x)) nil)
  (:method ((ast clang-ast))
    (ast-bound-vals* ast (ast-class ast))))

(defgeneric ast-bound-vals* (ast class)
  (:documentation "Implementation funtion for ast-bound-vals,
where class = (ast-class ast).")
  (:method ((ast clang-ast) (c t))
    ;; default method
    nil)
  (:method ((ast clang-ast) (c (eql :var)))
    (list ast))
  (:method ((ast clang-ast) (c (eql :declstmt)))
    (remove-if-not (lambda (a) (and (typep a 'clang-ast)
                                    (eql (ast-class a) :var)))
                   (children ast))))

(defmethod ast-unbound-funs ((ast clang-ast))
  (ast-unbound-funs* ast (ast-class ast)))

(defmethod ast-unbound-funs ((str string))
  (declare (ignore str))
  nil)

(defgeneric ast-unbound-funs* (ast class)
  (:documentation "Implementation funtion for ast-unbound-funs,
where class = (ast-class ast).")
  (:method ((ast t) (class t)) nil)
  (:method ((ast clang-ast) (class t)) nil)
  (:method ((ast clang-ast) (class (eql :declrefexpr)))
    (when-let* ((obj (ast-referenceddecl ast)))
      (when (eql (ast-class obj) :function)
        (list (list obj (ast-void-ret obj) (ast-varargs obj)
                    (count-if (lambda (a) (and (typep a 'clang-ast)
                                               (eql (ast-class a) :ParmVar)))
                              (children obj))))))))

(defmethod ast-includes ((obj null) (ast clang-ast))
  (ast-includes (make-instance 'clang) ast))

(defmethod ast-includes ((obj clang) (ast clang-ast))
  (ast-includes* obj ast (ast-class ast)))

(defmethod ast-includes ((obj clang) (ignored t)) nil)

(defmethod ast-includes* ((obj clang)
                          (ast clang-ast)
                          (class t))
  (remove-duplicates (apply #'append
                            (ast-includes-in-current-ast obj ast)
                            (mapcar {ast-includes obj}
                                    (children ast)))
                     :test #'equal))

(defmethod ast-includes* ((obj clang)
                          (ast clang-ast)
                          (class (eql :macroexpansion)))
  (remove-duplicates (apply #'append
                            (ast-includes-in-current-ast obj ast)
                            (mapcar {ast-includes obj}
                                    (ast-annotation ast :macro-child-segment)))
                     :test #'equal))

(defmethod ast-includes-in-current-ast ((obj clang) (ast clang-ast))
  (nest (remove-if #'null)
        (append (mapcar {ast-i-file obj} (list ast (ast-referenceddecl ast)))
                (when (ast-type ast)
                  (list (type-i-file (ast-type ast))))
                (when (ast-annotation ast :macro)
                  (list (macro-i-file (ast-annotation ast :macro)))))))

(defmethod ast-macros ((ast clang-ast))
  (ast-macros* ast (ast-class ast)))

(defmethod ast-macros ((ast string)) nil)

(defmethod ast-macros* ((ast clang-ast) (class t))
  (remove-duplicates (apply #'append
                            (when (and (ast-annotation ast :macro)
                                       (null (nest (macro-i-file)
                                                   (ast-annotation ast :macro))))
                              (list (ast-annotation ast :macro)))
                            (mapcar #'ast-macros (children ast)))))

(defmethod ast-macros* ((ast clang-ast) (class (eql :toplevel)))
  nil)

(defmethod ast-macros* ((ast clang-ast) (class (eql :macroexpansion)))
  (remove-duplicates (apply #'append
                            (when (and (ast-annotation ast :macro)
                                       (null (nest (macro-i-file)
                                                   (ast-annotation ast :macro))))
                              (list (ast-annotation ast :macro)))
                            (mapcar #'ast-macros
                                    (ast-annotation ast :macro-child-segment)))))

(defmethod ast-types ((ast string)) nil)
(defmethod ast-types ((ast clang-ast))
  (ast-types* ast (ast-class ast)))
(defun ast-types*-on-decl (ast)
  (when-let ((tp (ast-type ast))
             (storage-class (or (ast-annotation ast :storageclass) :none)))
    (list (make-instance 'ct+ :type tp :storage-class storage-class))))

(defgeneric ast-types* (ast class)
  (:documentation "Dispatch function for computing AST-TYPES
on various ast classes"))

(defmethod ast-types* ((ast clang-ast) (ast-class (eql :ParmVar)))
  (ast-types*-on-decl ast))

(defmethod ast-types* ((ast clang-ast) (ast-class (eql :Var)))
  ;; For :Var nodes, we must also include the types in the
  ;; initializer, if present
  (remove-duplicates (apply #'append
                            (ast-types*-on-decl ast)
                            (mapcar #'ast-types
                                    (remove ast (ast-nodes-in-subtree ast))))
                     :key #'type-hash))

(defmethod ast-types* ((ast clang-ast) (ast-class (eql :Macroexpansion)))
  (remove-duplicates (apply #'append
                            (ast-types*-on-decl ast)
                            (nest (mapcar #'ast-types)
                                  (mapcan #'ast-nodes-in-subtree
                                          (ast-annotation ast :macro-child-segment))))
                     :key #'type-hash))

(defmethod ast-types* ((ast clang-ast) (ast-class (eql :UnaryExprOrTypeTraitExpr)))
  (let ((argtype (ast-annotation ast :argtype))
        (types (ast-types*-on-decl ast)))
    (if argtype
        (adjoin (make-instance 'ct+ :type argtype) types :key #'type-hash)
        types)))

(defmethod ast-types* ((ast clang-ast) (ast-class (eql :Typedef)))
  (ast-types*-on-decl ast))

(defmethod ast-types* ((ast clang-ast) (ast-class symbol))
  (case ast-class
    ((:CstyleCastExpr
      :CXXFunctionalCastExpr
      :CXXReinterpretCastExpr)
     (ast-types*-on-decl ast))
    (t nil)))

(defmethod ast-declarations ((ast clang-ast))
  (cond
    ((member (ast-class ast) '(:Var :ParmVar :DeclStmt)) ; Var or function arg
     (ast-declares ast))
    ((function-decl-p ast) ; Function declaration.
     (mapcar #'car (ast-args ast)))  ; Does not need the hash codes
    (:otherwise nil)))

;; returns ast nodes, not strings
(defmethod ast-args ((obj clang-ast))
  (mapcar
   (lambda (o) (list o (ast-type o)))
   (remove-if-not (lambda (c) (ast-is-class c :ParmVar))
                  (children obj))))

(defun ast-arg-equal (arg1 arg2)
  (and (name= (first arg1) (first arg2))
       (equalp (second arg1) (second arg2))))

(defun ast-args-equal (args1 args2)
  "Compare two lists as returned by AST-ARGS"
  (and (length= args1 args2)
       (every #'ast-arg-equal args1 args2)))

(defmethod ast-declares ((c string)) nil)
(defmethod ast-declares ((c null)) nil)
(defmethod ast-declares ((obj clang-ast))
  (case (ast-class obj)
    (:DeclStmt
     (reduce #'append (children obj)
             :key #'ast-declares :initial-value nil))
    ((:ParmVar :Function :Var :Field :Record :TypeDef)
     (when (ast-name obj)
       (list obj)))
    ;; More cases here
    (t nil)))

(defmethod ast-expr-type ((obj clang-ast))
  (ast-type obj))

;;; Legacy AST full statement check (deprecated).  Please use `is-stmt-p` instead.
(defmethod ast-full-stmt ((obj clang-ast))
  (is-stmt-p obj))

;; This field should be filled in by a pass
;; that marks AST nodes that are full statements
;; (and that might not otherwise be)
(defmethod is-stmt-p ((obj clang-ast))
  (ast-annotation obj :full-stmt))

;; This field should be filled in by a pass
;; that marks AST nodes that are guard statements
;; (and that might not otherwise be)
(defmethod ast-guard-stmt ((obj clang-ast))
  (ast-annotation obj :guard-stmt))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *clang-decl-kinds*
    ;; Comment indicates the symbol obtained from
    ;; the raw json input
    '(:AccessSpec ;; :AccessSpecDecl
      :Binding ;; :BindingDecl
      :Block ;; :BlockDecl
      :BuiltinTemplate ;; :BuiltinTemplateDecl
      :Captured ;; :CapturedDecl
      :ClassScopeFunctionSpecialization ;;
      :ClassTemplate ;; :ClassTemplateDecl
      :ClassTemplatePartialSpecializationDecl
      :ClassTemplateSpecializationDecl ;;
      :ConstructorUsingShadowDecl ;;
      :CXXConstructor ;; :CXXConstructorDecl
      :CXXConversion ;; :CXXConversionDecl
      :CXXDestructor ;; :CXXDestructorDecl
      :CXXMethod ;; :CXXMethodDecl
      :CXXRecord ;; :CXXRecordDecl
      ;; :DeclRefExpr
      ;; :DeclStmt
      :DecltypeType
      :Decomposition ;; :DecompositionDecl
      ;; :DependentScopeDeclRefExpr
      :EmptyDecl
      :EnumConstant ;; :EnumConstantDecl
      :Enum ;; :EnumDecl
      :Field ;; :FieldDecl
      :FileScopeAsm ;; :FileScopeAsmDecl
      :Friend ;; :FriendDecl
      :Function ;; :FunctionDecl
      :FunctionTemplate ;; :FunctionTemplateDecl
      :ImplicitParam ;; :ImplicitParamDecl
      :IndirectField ;; :IndirectFieldDecl
      :Label ;; :LabelDecl
      :LinkageSpec ;; :LinkageSpecDecl
      :NamespaceAlias ;; :NamespaceAliasDecl
      :Namespace ;;  :NamespaceDecl
      :NonTypeTemplateParm ;; :NonTypeTemplateParmDecl
      :ParmVar  ;; :ParmVarDecl
      :PragmaComment ;; :PragmaCommentDecl
      :Record  ;; :RecordDecl
      :StaticAssert ;; :StaticAssertDecl
      :TemplateTemplateParm ;; :TemplateTemplateParmDecl
      :TemplateTypeParm ;; :TemplateTypeParmDecl
      :TopLevel ;; :TranslationUnitDecl
      :TypeAlias ;; :TypeAliasDecl
      :TypeAliasTemplate ;; :TypeAliasTemplateDecl
      :Typedef ;; :TypedefDecl
      :UnresolvedUsingTypename ;; :UnresolvedUsingTypenameDecl
      :UnresolvedUsingValue ;; :UnresolvedUsingValueDecl
      :Using ;; :UsingDecl
      :UsingDirective ;; :UsingDirectiveDecl
      :UsingPack ;; :UsingPackDecl
      :UsingShadow ;; :UsingShadowDecl
      :Var ;; :VarDecl
      :VarTemplate ;; :VarTemplateDecl
      :VarTemplatePartialSpecialization ;; :VarTemplatePartialSpecializationDecl
      :VarTemplateSpecialization))) ;; :VarTemplateSpecializationDecl

(defmethod ast-is-decl ((obj clang-ast))
  (case (ast-class obj)
    (#.*clang-decl-kinds* t)
    (t nil)))

(defmethod ast-opcode ((obj clang-ast))
  (ast-annotation obj :opcode))

(defmethod ast-ret ((obj clang-ast))
  (case (ast-class obj)
    (:Function
     (let ((type (type-qual (ast-type obj))))
       (ret-type-of-function-type type)))
    ;; Others?
    (t nil)))

(defmethod ast-void-ret ((obj clang-ast))
  (equal "void" (type-qual (ct+-type (ast-ret obj)))))

(defmethod ast-varargs ((obj clang-ast))
  ;; Should just be :FunctionDecl objects
  (ast-annotation obj :variadic))

(defgeneric ast-referenceddecl (ast)
  (:documentation "The declaration referenced by AST.")
  (:method ((ast clang-ast))
    (ast-annotation ast :referenceddecl)))

(defgeneric (setf ast-annotation) (v ast annotation)
  (:documentation "Set the given AST ANNOTATION to V.")
  (:method (v (ast clang-ast) (annotation symbol))
    (setf (ast-annotations ast)
          (cons (cons annotation v)
                (adrop (list annotation) (ast-annotations ast))))
    v))

;; FIXME: When clang is converted to utilize functional trees,
;; these method specializations will no longer be required.
(defmethod ast-path ((obj t) (ast clang-ast))
  (declare (ignorable obj))
  (slot-value ast 'path))

(defmethod ast-path ((obj clang) (ast functional-tree-ast))
  (ast-path (genome obj) ast))

;; FIXME: When clang is converted to utilize functional trees,
;; this method specialization will no longer be required.
(defmethod ast-hash ast-combine-hash-values ((ast clang-ast))
  (ast-hash (cons (ast-class ast) (children ast))))

;; FIXME: When clang is converted to utilize functional trees,
;; this method specialization will no longer be required.
(defmethod equal? :around ((ast-a clang-ast) (ast-b clang-ast))
  (and (eq (ast-class ast-a) (ast-class ast-b))
       (call-next-method)))

(defun ret-type-of-function-type (s)
  "Returns a string that is the return type of the function type
given in the string S. Return nil if the return type cannot be
determined."
  ;; This is grossly incomplete now, and will fail on some
  ;; hairy types
  (let ((pos (position #\( s)))
    (when (and pos (> pos 0))
      (make-instance 'ct+
        :type (make-instance 'clang-type
                :qual (trim-whitespace (subseq s 0 (1- pos))))))))

(defun flags-to-include-dirs (flags)
  "Return the listing of include search paths in FLAGS

* FLAGS: list of normalized compiler flags"
  (iter (for f in flags)
        (for p previous f)
        (when (string= p "-I")
          (collect f))))

;;; Question on this: are IDs unique between files?
;;; Or do we need keys that include file names?

;;; NOTE: will need to make sure this works on Windows also
;;;  Perhaps it should work on pathnames, not namestrings?

;;; NOTE: this assumes "..." does NOT search the include path
;;;  In Clang, this behavior is controlled by command line options,
;;;  which we'll need to recognize.

(defun normalize-file-for-include (obj file-string)
  "Returns the normalized version of file-string relative to OBJ's include-dirs,
and a value that is T if the string should be in #include \"...\", NIL if in
#include <...>"
  (cond
    ;; Empty string is erroneous
    ((emptyp file-string)
     (error "normalize-file-for-include given an empty string"))
    ;; If it starts with ./, it's a local file
    ((eql (search "./" file-string) 0)
     (values (subseq file-string 2) t))
    ;; If it does not start with /, it's a local file
    ((not (eql (elt file-string 0) #\/))
     (values file-string t))
    (t
     ;; Otherwise, try to find longest prefix for include-dirs
     ;; Assumes include-dirs is in normal form
     (let ((include-dirs (append (flags-to-include-dirs (flags obj))
                                 *clang-default-includes*))
           (file-len (length file-string))
           (max-match 0)
           (dir nil))
       (flet ((%match (ind)
                "Attempt to match FILE-STRING against IND.  Returns
the match length if sucessful, NIL if not."
                (let ((ind-len (length ind)))
                  (when (< ind-len file-len)
                    (let ((mm (mismatch ind file-string)))
                      (when (= mm ind-len) ind-len))))))
         (iter (for ind in include-dirs)
               (let ((mm (%match ind)))
                 (when (and mm (> mm max-match))
                   (setf max-match mm
                         dir ind))))
         (if (find dir *clang-default-includes* :test #'equal)
             (values (concatenate 'string
                                  "<" (subseq file-string max-match) ">")
                     nil)
             (values (concatenate 'string
                                  "\"" (subseq file-string max-match) "\"")
                     t)))))))

(defun ast-i-file (obj ast)
  "Return the file AST is located within in a format suitable for use
in a #include."
  (when-let ((file (nest (first)
                         (remove-if «or #'null
                                        {equal "<built-in>"}
                                        {equal "<scratch space>"}
                                        [{find-if {equalp "bits"}}
                                         #'pathname-directory]»)
                         (list (file ast nil)
                               (file ast t)
                               (included-from ast nil)
                               (included-from ast t)))))
    (normalize-file-for-include obj file)))

;;; TODO: This function should be removed when we unify
;;;       clang-range with sel/utility/range:range.
(defun within-ast-range (range line)
  "Test whether the supplied line is within a range."
  (and (>= line (clang-loc-line (clang-range-begin range)))
       (<= line (clang-loc-line (clang-range-end range)))))

;;; TODO: This function should be removed when we unify
;;;       clang-range with sel/utility/range:range.
(defun ast-range-str (range)
  "Return a short string-rep for the supplied range."
  (format nil "[~a, ~a]" (clang-loc-line (clang-range-begin range))
          (clang-loc-line (clang-range-end range))))


;;; Type-related functions
;;;
;;; NOTE: NEW-CLANG-TYPE is not a drop-in replacement for
;;;  SEL/SW/CLANG:CLANG-TYPE.  The latter contains additional
;;;  information that is not properly part of a type at all.
;;;

(defmethod type-i-file ((tp+ ct+))
  (type-i-file (ct+-type tp+)))

(defmethod type-reqs ((tp+ ct+))
  (mapcar {make-instance 'ct+ :type}
          (type-reqs (ct+-type tp+))))

(defmethod type-hash ((tp+ ct+))
  (sxhash (concatenate 'string
                       (type-qual (ct+-type tp+))
                       (or (type-desugared (ct+-type tp+)) "")
                       (symbol-name (type-storage-class tp+)))))
(defmethod type-hash ((tp clang-type))
  (sxhash (concatenate 'string
                       (type-qual tp)
                       (or (type-desugared tp) ""))))

(defmethod type-decl ((tp+ ct+))
  (type-decl (ct+-type tp+)))

;;; Pointer, const, volatile, and restrict are indicated by integers
;;;  in the modifiers slot.

(defconstant +pointer+ 1)
(defconstant +const+ 2)
(defconstant +volatile+ 4)
(defconstant +restrict+ 8)

(defmethod type-name ((tp+ ct+))
  (type-name (ct+-type tp+)))

(defmethod type-array ((tp+ ct+))
  (type-array (ct+-type tp+)))

(defmethod type-pointer ((tp+ ct+))
  (type-pointer (ct+-type tp+)))
(defmethod type-pointer ((tp clang-type))
  (if (logtest +pointer+ (type-modifiers tp)) t nil))

(defmethod type-const ((tp+ ct+))
  (type-const (ct+-type tp+)))
(defmethod type-const ((tp clang-type))
  (if (logtest +const+ (type-modifiers tp)) t nil))

(defmethod type-volatile ((tp+ ct+))
  (type-volatile (ct+-type tp+)))
(defmethod type-volatile ((tp clang-type))
  (if (logtest +volatile+ (type-modifiers tp)) t nil))

(defmethod type-restrict ((tp+ ct+))
  (type-restrict (ct+-type tp+)))
(defmethod type-restrict ((tp clang-type))
  (if (logtest +restrict+ (type-modifiers tp)) t nil))

(defmethod slot-unbound ((class t) (obj clang-type) (slot (eql 'array)))
  (compute-clang-type-slots obj)
  (slot-value obj slot))
(defmethod slot-unbound ((class t) (obj clang-type) (slot (eql 'name)))
  (compute-clang-type-slots obj)
  (slot-value obj slot))
(defmethod slot-unbound ((class t) (obj clang-type) (slot (eql 'modifiers)))
  (compute-clang-type-slots obj)
  (slot-value obj slot))

(defgeneric compute-clang-type-slots (tp)
  (:method ((tp clang-type))
    ;; Fill in various slots in clang-type object
    (multiple-value-bind (pointer const volatile restrict n a)
        (compute-type-properties (type-qual tp))
      (with-slots (array name modifiers) tp
        (setf array a
              name n
              modifiers
              (pack-type-modifiers
               pointer const volatile restrict))))))

(defun pack-type-modifiers (pointer const volatile restrict)
  (logior
   (if pointer +pointer+ 0)
   (if const +const+ 0)
   (if volatile +volatile+ 0)
   (if restrict +restrict+ 0)))

(defun compute-type-properties (name)
  (multiple-value-bind (name suffix-list)
      (trim-array-suffixes name)
    (let ((const nil) const2 volatile volatile2
          restrict restrict2 pointer)
      (setf (values const volatile restrict name)
            (trim-prefix-modifiers name))
      (setf (values const2 volatile2 restrict2 name)
            (trim-suffix-modifiers name))
      (let ((l (length name)))
        (when (and (> l 0) (eql (elt name (1- l)) #\*))
          (setf pointer t)
          (setf name (subseq name 0 (1- l)))))
      (values pointer
              (or const const2)
              (or volatile volatile2)
              (or restrict restrict2)
              (string-trim " " name)
              (format nil "~{[~a]~}" suffix-list)))))

(defun trim-prefix-modifiers (str)
  "Trim const, volatile, restrict, and keyword (class, struct, etc.)
modifiers from a type name"
  (let ((const nil) (volatile nil) (restrict nil)
        (pos 0) (strlen (length str)))
    (flet ((is-prefix (s)
             (let ((l (length s)))
               (when (and (< l (- strlen pos))
                          (let ((c (elt str (+ pos l))))
                            (and (not (alphanumericp c))
                                 (not (eql c #\_))))
                          (let ((m (string/= s str :start2 pos)))
                            (or (null m) (eql m l))))
                 (incf pos l)))))
      (loop
         (cond
           ((>= pos strlen) (return))
           ((whitespacep (elt str pos))
            (incf pos))
           ((is-prefix "const") (setf const t))
           ((is-prefix "volatile") (setf volatile t))
           ((is-prefix "restrict") (setf restrict t))
           ((is-prefix "struct") t)
           ((is-prefix "typedef") t)
           ((is-prefix "class") t)
           ((is-prefix "union") t)
           (t (return)))))
    (values const volatile restrict
            (if (= pos 0) str (subseq str pos)))))

(defun trim-suffix-modifiers (str)
  (let* (const volatile restrict
               (strlen (length str))
               (pos strlen))
    (flet ((is-suffix (s)
             (let ((l (length s)))
               (when (and (< l pos)
                          (let ((c (elt str (- pos l 1))))
                            (and (not (alphanumericp c))
                                 (not (eql c #\_))))
                          (let ((m (string/= s str :start2 (- pos l))))
                            (or (null m) (eql m l))))
                 (decf pos l)))))
      (loop
         (cond
           ((<= pos 0) (return))
           ((whitespacep (elt str (1- pos)))
            (decf pos))
           ((is-suffix "const") (setf const t))
           ((is-suffix "volatile") (setf volatile t))
           ((is-suffix "restrict") (setf restrict t))
           (t (return)))))
    (values const volatile restrict
            (if (= pos strlen) str (subseq str 0 pos)))))

(defun trim-array-suffixes (str)
  (let* ((suffixes nil)
         (len (length str))
         (pos len)
         (last-suffix-start len))
    (block done
      (iter (while (> pos 0))
            (decf pos)
            (while (eql (elt str pos) #\]))
            (let ((end pos))
              (iter (unless (> pos 0)
                      (return-from done))
                    (decf pos)
                    (when (eql (elt str pos) #\[)
                      (setq last-suffix-start pos)
                      (push (subseq str (1+ pos) end)
                            suffixes)
                      (return))))))
    (if (null suffixes)
        (values str nil)
        (values (string-right-trim " " (subseq str 0 last-suffix-start))
                suffixes))))

(defgeneric type-decl-ast (obj type)
  (:documentation "Return the AST in OBJ declaring TYPE.")
  (:method ((obj clang) (tp+ ct+))
    (type-decl-ast obj (ct+-type tp+)))
  (:method ((obj clang) (tp clang-type))
    (type-decl-ast (name-symbol-table obj) tp))
  (:method ((name-symbol-table hash-table) (tp+ ct+))
    (type-decl-ast name-symbol-table (ct+-type tp+)))
  (:method ((name-symbol-table hash-table) (tp clang-type))
    (when-let* ((qual (type-qual tp))
                (ast-classes (cond ((or (starts-with-subseq "struct " qual)
                                        (starts-with-subseq "class " qual))
                                    (list :CXXRecord :Record))
                                   ((starts-with-subseq "union " qual)
                                    (list :Union))
                                   (t (list :Typedef)))))
      (first (remove-if-not [{member _ ast-classes} #'ast-class]
                            (gethash (type-name tp) name-symbol-table))))))

(defgeneric typedef-type (software type)
  (:documentation "Return the underlying type if TYPE is a typedef")
  (:method ((obj clang) (ct ct+)
            &aux (mods (type-modifiers (ct+-type ct)))
              (array (type-array (ct+-type ct))))
    (labels ((system-ast-p (obj ast)
               "Return T if AST is in a system header file."
               (or (and (null (file ast nil))
                        (null (file ast t)))
                   (nth-value 1 (ast-i-file obj ast))))
             (typedef-type-helper (ct)
               (if-let* ((typedef-ast (type-decl-ast obj ct))
                         (typedef-ct
                          (when (and (system-ast-p obj typedef-ast)
                                     (ast-type typedef-ast))
                            (make-instance 'ct+ :type (ast-type typedef-ast)))))
                 (typedef-type-helper typedef-ct)
                 (copy ct
                       :type (copy (ct+-type ct)
                                   :modifiers (logior mods
                                                      (type-modifiers
                                                       (ct+-type ct)))
                                   :array (concatenate 'string
                                                       (type-array (ct+-type ct))
                                                       array))))))
      (typedef-type-helper ct))))

(defgeneric type-decl-string (type)
  (:documentation "The source text used to declare variables of TYPE.
This will have stars on the right, e.g. char**. ")
  (:method ((obj clang-type))
    (type-qual obj))
  (:method ((obj ct+))
    (concatenate 'string
                 (unless (eq (type-storage-class obj) :none)
                   (format nil "~a " (nest (string-downcase)
                                           (symbol-name)
                                           (type-storage-class obj))))
                 (type-decl-string (ct+-type obj)))))

(defgeneric type-trace-string (type &key qualified)
  (:documentation "The text used to describe TYPE in an execution trace.

This will have stars on the left, e.g **char.")
  (:method ((type ct+) &key (qualified t))
    (concatenate 'string
                 (when (type-pointer type) "*")
                 (when (not (emptyp (type-array type))) (type-array type))
                 (when (and qualified (type-const type)) "const ")
                 (when (and qualified (type-volatile type)) "volatile ")
                 (when (and qualified (type-restrict type)) "restrict ")
                 (when (and qualified
                            (not (eq :None (type-storage-class type))))
                   (format nil "~a " (nest (string-downcase)
                                           (symbol-name)
                                           (type-storage-class type))))
                 (type-name type))))

(defgeneric type-from-trace-string (trace-string)
  (:documentation
   "Create a clang-type from a name used in an execution trace.
The resulting type will not be added to any clang object and will not have a
valid hash.
* TRACE-STRING type name as expressed in an execution trace.")
  (:method ((trace-string string))
    (let ((alist (trace-string-to-type-alist trace-string)))
      (nest (make-instance 'ct+ :storage-class (aget :storage-class alist) :type)
            (make-instance 'clang-type
              :qual (trace-string-to-clang-json-string trace-string)
              :modifiers (pack-type-modifiers (aget :pointer alist)
                                              (aget :const alist)
                                              (aget :volatile alist)
                                              (aget :restrict alist))
              :array (aget :array alist)
              :name (aget :name alist))))))

(defun trace-string-to-type-alist (trace-string)
  (list (cons :pointer (not (null (find #\* trace-string))))
        (cons :array (if (find #\[ trace-string)
                         (scan-to-strings "\\[[^\\[\\]]*\\]" trace-string)
                         ""))
        (cons :const (not (null (search "const" trace-string))))
        (cons :volatile (not (null (search "volatile" trace-string))))
        (cons :restrict (not (null (search "restrict" trace-string))))
        (cons :storage-class
              (or (register-groups-bind (storage-class)
                      ("(extern|static|__private_extern__|auto|register)"
                       trace-string)
                    (make-keyword (string-upcase storage-class)))
                  :None))
        (cons :name
              (regex-replace
               (format nil
                       "^(\\*|\\[[^\\[\\]]*\\]|const |volatile |restrict |~
                           extern |static |__private_extern__ |auto |~
                           register )*")
               trace-string ""))))

(defun trace-string-to-clang-json-string
    (trace-string &key storage-class const pointer volatile restrict name array
                    &allow-other-keys)
  (let ((alist (trace-string-to-type-alist trace-string)))
    (string-right-trim
     " "
     (format
      nil
      "~@[~(~a~) ~]~:[~;const ~]~:[~;volatile ~]~:[~;restrict ~]~a ~:[~;*~]~@[~a~]"
      (let ((sc (or storage-class (aget :storage-class alist))))
        (if (eql sc :none) nil sc))
      (or const (aget :const alist))
      (or volatile (aget :volatile alist))
      (or restrict (aget :restrict alist))
      (or name (aget :name alist))
      (or pointer (aget :pointer alist))
      (or array (aget :array alist))))))


;;; Invocation of clang to get json

(defmethod clang-json ((obj clang) &aux (source-text (genome-string obj)))
  (with-temporary-file-of (:pathname src-file :type (ext obj)) source-text
    (let ((cmd-fmt "clang -cc1 -ast-dump=json ~
                          -fgnuc-version=4.2.1 ~
                          -fcxx-exceptions ~
                          ~{~a~^ ~} ~a ~a")
          (filter "| sed -e \"s/  *//\" ; exit ${PIPESTATUS[0]}")
          (flags (nest (append (clang-frontend-flags (flags obj)))
                       (mappend {list "-isystem"})
                       (mapcar #'escape-shell-token)
                       *clang-default-includes*)))
      (multiple-value-bind (stdout stderr exit)
          (let ((*trace-output* *standard-output*))
            (if (boundp '*clang-json-file*)
                (shell "cat ~a ~a;"
                       (namestring *clang-json-file*)
                       filter)
                (shell cmd-fmt
                       flags
                       src-file
                       filter
                       :bash t)))
        (when (find exit '(131 132 134 136 139))
          (error
           (make-condition 'mutate
                           :text (format nil "clang core dump with ~d, ~s"
                                         exit stderr)
                           :obj obj)))
        (restart-case
            (unless (zerop exit)
              (error
               (make-condition 'mutate
                               :text (format nil
                                             "clang exit ~d~%cmd:~s~%stderr:~s"
                                             exit
                                             (format nil cmd-fmt
                                                     flags
                                                     src-file
                                                     filter)
                                             stderr)
                               :obj obj)))
          (keep-partial-asts ()
            :report "Ignore error retaining partial ASTs for software object."
            nil))
        (values (convert-clang-jsown-tree (jsown:parse stdout))
                src-file
                (length source-text))))))

(defun convert-clang-jsown-tree (jt)
  (convert-jsown-tree jt #'jsown-str-to-clang-keyword))

;;; The STRING-CASE macro is much faster than just calling INTERN
;;; on the string, when one of these common arguments is seen.
(defun jsown-str-to-clang-keyword (str)
  (string-case-to-keywords ("id" "tokLen" "col" "kind" "qualType"
                                 "type" "file" "range" "end" "begin"
                                 "includedFrom" "line" "valueCategory"
                                 "inner" "name" "loc" "castKind"
                                 "referencedDecl" "spellingLoc" "offset"
                                 "expansionLoc" "desugaredQualType")
                           str))

;;; Json conversion

(defun clang-convert-json-for-file (json file genome-len)
  ;; The aget cache is used to record values of elided
  ;; json attributes, that are assumed to be equal to the previous
  ;; occurrence of such an attribute.  This res the json
  ;; be converted left to right.  cl-json produces alists
  ;; in the same order they appear in the json, fortunately.
  (let* ((*aget-cache* nil)
         (ast (clang-convert-json json)))
    (setf (ast-range ast)
          (make-clang-range
           :begin (make-clang-loc
                   :file file
                   :offset 0)
           :end (make-clang-loc
                 :file file
                 :offset genome-len)))
    ast))

(defun clang-convert-json (json)
  "Convert json data in list form to data structures using NEW-CLANG-AST"
  (typecase json
    (null nil)
    (cons
     (let* ((json-kind (aget :kind json))
            (json-kind-symbol (if json-kind
                                  (json-kind-to-keyword json-kind)
                                  :unknown)))
       (unless (keywordp json-kind-symbol)
         (error "Cannot convert ~a to a json-kind keyword" json-kind))
       (j2ck json json-kind-symbol)))
    (string (canonicalize-string json))
    (t json)))

(defgeneric j2ck (json json-kind-symbol)
  (:documentation "Generic function for converting a json node
to a clang-node.  The purpose of this is to enable dispatch
on json-kind-symbol when special subclasses are wanted."))

(defmethod j2ck (json (json-kind-symbol t))
  (let ((obj (make-instance 'clang-ast)))
    (store-slots obj json)))

(defmethod j2ck :around ((json t) (json-kind-symbol (eql :forstmt)))
  ;; Clang's json has {} for missing for clauses
  ;; cl-json converts these to NIL.  Just remove then,
  ;; as the old clang front end does.
  (let ((obj (call-next-method)))
    (setf (children obj) (remove nil (children obj)))
    obj))

(defmethod j2ck :around ((json t) (json-kind-symbol (eql :ImplicitListExpr)))
  ;; We remove :ImplicitValueInitExprs, turning them to NIL.
  ;; Here, remove those NILs.
  (let ((obj (call-next-method)))
    (setf (children obj) (remove nil (children obj)))
    obj))

(defmethod j2ck :around ((json t) (json-kind-symbol (eql :typedef)))
  (let ((obj (call-next-method)))
    (pop (children obj))
    obj))

(defmethod j2ck ((json t) (json-kind-symbol (eql :ImplicitValueInitExpr)))
  nil)

(defmethod j2ck ((json t) (json-kind-symbol (eql :TextComment)))
  nil)

(defmethod j2ck ((json t) (json-kind-symbol (eql :ParagraphComment)))
  nil)

(defmethod j2ck ((json t) (json-kind-symbol (eql :FullComment)))
  nil)

(defmethod j2ck ((json t) (json-kind-symbol (eql :InlineCommandComment)))
  nil)

(defmethod j2ck ((json t) (json-kind-symbol (eql :BlockCommandComment)))
  nil)

(defmethod j2ck ((json t) (json-kind-symbol (eql :ParamCommandComment)))
  nil)

(defmethod j2ck ((json t) (json-kind-symbol null))
  ;; If there is no :kind field, the value is nil and this method applies
  nil)

;; The ctor intiializer is not give range information, just the initializer
;; expression.
(defmethod j2ck ((json t) (json-kind-symbol (eql :CxxCtorInitializer)))
  nil)

;;; This special rule handles catch (...).   The ... shows up
;;; as a json entry with id 0x0.
(defmethod j2ck (json (json-kind-symbol (eql :CxxCatchStmt)))
  (let* ((inner (aget :inner json)))
    (flet ((%r (a) (aget :range a)))
      (if (notevery #'%r inner)
          ;; The field for ... doesn't have any :kind
          ;; Don't try to translate it
          (j2ck (iter (for e in json)
                      (if (eql (car e) :inner)
                          (collect (cons :inner (remove-if-not #'%r (cdr e))))
                          (collect e)))
                json-kind-symbol)
          (call-next-method)))))

(defmethod j2ck (json (json-kind-symbol (eql :GenericSelectionExpr)))
  (let* ((inner (aget :inner json)))
    (if (notevery (lambda (a) (aget :kind a)) inner)
        ;; Rewrite and try again
        (let* ((new-inner
                (cons
                 (car inner)
                 (iter (for x in (cddr inner))
                       (cond
                         ((aget :kind x) (collect x))
                         ((aget :associationkind x)
                          (let ((inner2 (aget :inner x)))
                            (collect (cadr inner2))))))))
               (new-json
                (iter (for x in json)
                      (collect
                       (if (and (consp x) (eql (car x) :inner))
                           `(:inner ,@new-inner)
                           x)))))
          (j2ck new-json json-kind-symbol))
        (call-next-method))))

(defgeneric store-slots (obj json)
  (:documentation "Store values in the json into obj.
Return the object, or another object to be used in
its place."))

(defmethod store-slots ((obj clang-ast) (json list))
  (dolist (x json)
    (destructuring-bind (slot . value) x
      (setf obj (store-slot obj slot value))))
  obj)

(defgeneric store-slot (obj slot value)
  (:documentation "Converts json VALUE into appropriate internal
form for SLOT, and stores into OBJ.  Returns OBJ or its replacement."))

(defmethod store-slot ((obj clang-ast) (slot symbol) value)
  ;; Generic case
  (let ((annotations (ast-annotations obj)))
    (assert (null (aget slot annotations)) () "Duplicate slot ~a" slot)
    (when-let ((converted-value (convert-slot-value obj slot value)))
      (setf (ast-annotations obj)
            (append annotations `((,slot . ,converted-value))))))
  obj)

(defmethod store-slot ((obj clang-ast) (slot (eql :type)) value)
  (assert (null (ast-type obj)))
  (setf (ast-type obj) (convert-slot-value obj slot value))
  obj)

(defmethod store-slot ((obj clang-ast) (slot (eql :kind)) value)
  (setf (ast-class obj) (json-kind-to-keyword value))
  obj)

(defmethod store-slot ((obj clang-ast) (slot (eql :range)) value)
  (assert (null (ast-range obj)))
  (setf (ast-range obj) (convert-slot-value obj slot value))
  obj)

(macrolet ((ignore-slot (slot-name)
             `(defmethod store-slot ((obj clang-ast)
                                     (slot (eql ',slot-name))
                                     (value t))
                obj)))
  (ignore-slot :definitionData)
  (ignore-slot :bases)
  (ignore-slot :foundReferencedDecl)
  (ignore-slot :path)
  (ignore-slot :lookups)
  (ignore-slot :valueCategory)
  (ignore-slot :ctorType)
  (ignore-slot :hadMultipleCandidates)
  (ignore-slot :constructionKind)
  (ignore-slot :inline)
  (ignore-slot :constexpr)
  (ignore-slot :explicitlyDefaulted)
  (ignore-slot :isUsed)
  (ignore-slot :completeDefinition)
  (ignore-slot :canOverflow))

(defmethod store-slot ((obj clang-ast) (slot (eql :id)) value)
  (setf (ast-id obj) (convert-slot-value obj slot value))
  obj)

(defmethod store-slot ((obj clang-ast) (slot (eql :inner)) value)
  (setf (children obj)
        (remove nil (mapcar (lambda (o) (clang-convert-json o)) value)))
  obj)

(defmethod store-slot ((obj clang-ast) (slot (eql :array_filler)) (value t))
  obj)

(defgeneric convert-slot-value (obj slot value)
  (:documentation "Convert a value in the context of a specific slot.  Return of
NIL indicates no value."))

(defmethod convert-slot-value ((obj clang-ast) (slot symbol) value)
  ;; Default to a context-independent conversion
  (clang-convert-json value))

(defmethod convert-slot-value ((obj clang-ast) (slot (eql :referenceddecl)) value)
  (clang-convert-json value))

(defmethod convert-slot-value ((obj clang-ast) (slot (eql :decl)) value)
  (clang-convert-json value))

(defmethod convert-slot-value ((obj clang-ast) (slot (eql :templateparams)) value)
  (mapcar #'clang-convert-json value))

(defmethod convert-slot-value ((obj clang-ast) (slot (eql :explicittemplateargs)) value)
  (declare (ignorable obj slot))
  (mapcar #'clang-convert-json value))

(defmethod convert-slot-value ((obj clang-ast) (slot (eql :id)) value)
  (read-c-integer value))

(defmethod convert-slot-value ((obj clang-ast) (slot (eql :previousdecl)) value)
  (read-c-integer value))

(defmethod convert-slot-value ((obj clang-ast) (slot (eql :name)) value)
  (and (not (equal value "")) (call-next-method)))

(defmethod convert-slot-value ((obj clang-ast) (slot (eql :tagused)) value)
  (cond
    ((equal value "struct") :struct)
    ((equal value "union") :union)
    ((equal value "class") :class)
    ((equal value "typename") :typename)
    (t (call-next-method))))

(defmethod convert-slot-value ((obj clang-ast) (slot (eql :storageClass)) value)
  (cond
    ((equal value "auto") :auto)
    ((equal value "static") :static)
    ((equal value "extern") :extern)
    ((equal value "register") :register)
    ((equal value "__private_extern__") :__PRIVATE_EXTERN__)
    (t (call-next-method))))

(defmethod convert-slot-value ((obj clang-ast) (slot (eql :castKind)) (value string))
  (cond
    ((equal value "LValueToRValue") :LValueToRValue)
    ((equal value "FunctionToPointerDecay") :FunctionToPointerDecay)
    ((equal value "NullToPointer") :NullToPointer)
    ((equal value "ArrayToPointerDecay") :ArrayToPointerDecay)
    ((equal value "BitCast") :BitCast)
    ((equal value "IntegralCase") :IntegralCast)
    ((equal value "NoOp") :NoOp)
    (t (intern (string-upcase value) :keyword))))

;; More conversions

(defun convert-loc-json (loc-json)
  "Special handler for values of loc attributes"
  (if (aget :spellingloc loc-json)
      (convert-macro-loc-json loc-json)
      (make-clang-loc
       :file (canonicalize-string (cached-aget :file loc-json))
       :included-from (nest (canonicalize-string)
                            (aget :file)
                            (aget :includedfrom loc-json))
       :line (cached-aget :line loc-json)
       :presumed-line (cached-aget :presumedline loc-json)
       :col (cached-aget :col loc-json)
       :tok-len (cached-aget :toklen loc-json))))

(defun convert-macro-loc-json (loc-json)
  "This is the special case of a LOC that has spelling and expansion locs"
  (let* ((spelling-loc (convert-loc-json (aget :spellingloc loc-json)))
         (expansion-loc-json (aget :expansionloc loc-json))
         (expansion-loc (convert-loc-json expansion-loc-json))
         (is-macro-arg-expansion (aget :ismacroargexpansion expansion-loc-json)))
    (when (or spelling-loc expansion-loc)
      (make-clang-macro-loc
       :spelling-loc spelling-loc
       :expansion-loc expansion-loc
       :is-macro-arg-expansion is-macro-arg-expansion))))

(defun convert-range-json (range-json)
  "Special handler for values of range attributes"
  (let ((begin (convert-loc-json (aget :begin range-json)))
        (end (convert-loc-json (aget :end range-json))))
    (when (or begin end)
      (make-clang-range :begin begin :end end))))

(defmethod convert-slot-value ((obj clang-ast) (slot (eql :loc)) value)
  (convert-loc-json value))

(defmethod convert-slot-value ((obj clang-ast) (slot (eql :range)) value)
  (convert-range-json value))

(defmethod convert-slot-value ((obj clang-ast) (slot (eql :type)) (value list))
  (convert-type-slot-value obj slot value))

(defmethod convert-slot-value ((obj clang-ast) (slot (eql :argtype)) (value list))
  (convert-type-slot-value obj slot value))

(defun convert-type-slot-value (obj slot value)
  (declare (ignore obj slot))
  ;; These should be strings, but convert anyway to canonicalize them
  (canonicalize-type (clang-convert-json (aget :qualtype value))
                     (clang-convert-json (aget :desugaredqualtype value))))

;; Helpers for JSON conversion
;; TODO:  string-case this?
(defun json-kind-to-keyword (json-kind)
  (when (stringp json-kind)
    (let ((sym (intern (string-upcase json-kind) :keyword)))
      (case sym
        ((:AccessSpecDecl) :AccessSpec)
        ((:BindingDecl) :Binding)
        ((:BlockDecl) :Block)
        ((:BuiltinTemplateDecl) :BuiltinTemplate)
        ((:CXXConstructorDecl) :CXXConstructor)
        ((:CXXConversionDecl) :CXXConversion)
        ((:CXXDestructorDecl) :CXXDestructor)
        ((:CXXMethodDecl) :CXXMethod)
        ((:CXXRecordDecl) :CXXRecord)
        ((:CapturedDecl) :Captured)
        ((:ClassScopeFunctionSpecializationDecl) :ClassScopeFunctionSpecialization)
        ((:ClassTemplateDecl) :ClassTemplate)
        ((:ClassTemplateSpecializationDecl) :ClassTemplateSpecialization)
        ((:ConstructorUsingShadowDecl) :ConstructorUsingShadow)
        ((:DecompositionDecl) :Decomposition)
        ;; ((:EmptyDecl) :Empty)
        ((:EnumConstantDecl) :EnumConstant)
        ((:EnumDecl) :Enum)
        ;; ((:ExternCContextDecl) :ExternCContext)
        ((:FieldDecl) :Field)
        ((:FileScopeAsmDecl) :FileScopeAsm)
        ((:FriendDecl) :Friend)
        ((:FriendTemplateDecl) :FriendTemplate)
        ((:FunctionDecl) :Function)
        ((:FunctionTemplateDecl) :FunctionTemplate)
        ((:ImplicitParamDecl) :ImplicitParam)
        ;; ((:ImportDecl) :Import)
        ((:IndirectFieldDecl) :IndirectField)
        ((:LabelDecl) :Label)
        ((:LinkageSpecDecl) :LinkageSpec)
        ((:NameSpaceAliasDecl) :NameSpaceAlias)
        ((:NamespaceDecl) :Namespace)
        ((:NonTypeTemplateParmDecl) :NonTypeTemplateParm)
        ((:ParmVarDecl) :ParmVar)
        ((:PragmaCommentDecl) :PragmaComment)
        ((:RecordDecl) :Record)
        ((:StaticAssertDecl) :StaticAssert)
        ((:TemplateTemplateParmDecl) :TemplateTemplateParm)
        ((:TemplateTypeParmDecl) :TemplateTypeParm)
        ((:TranslationUnitDecl) :TopLevel)
        ((:TypeAliasDecl) :TypeAlias)
        ((:TypeAliasTemplateDecl) :TypeAliasTemplate)
        ((:TypedefDecl) :Typedef)
        ((:UnresolvedUsingTypenameDecl) :UnresolvedUsingTypename)
        ((:UnresolvedUsingValueDecl) :UnresolvedUsingValue)
        ((:UsingDecl) :Using)
        ((:UsingDirectiveDecl) :UsingDirective)
        ((:UsingPackDecl) :UsingPack)
        ((:UsingShadowDecl) :UsingShadow)
        ((:VarDecl) :Var)
        ((:VarTemplateDecl) :VarTemplate)
        ((:VarTemplatePartialSpecializationDecl) :VarTemplatePartialSpecialization)
        ((:VarTemplateSpecializationDecl) :VarTemplateSpecialization)
        (t sym)))))

;;; We cache the last lookup of certain slots, so that repeat values
;;; can be omitted in the json.  A special accessor maintains this cache

(defun cached-aget (key alist)
  "Cached aget looks up the value, then uses the cached valued
if no value was found."
  (if (boundp '*aget-cache*)
      (let ((value (aget key alist))
            (cache *aget-cache*))
        (if value
            (let ((p (assoc key cache)))
              (if p (setf (cdr p) value)
                  (setf *aget-cache* (cons (cons key value) cache)))
              value)
            (aget key cache)))
      (let ((*aget-cache* nil))
        (cached-aget key alist))))

(defun canonicalize-string (str)
  (if (boundp '*canonical-string-table*)
      (let ((table *canonical-string-table*))
        (or (gethash str table)
            (setf (gethash str table) str)))
      str))

(defun canonicalize-type (qual desugared)
  (if (boundp '*canonical-clang-type-table*)
      (or (gethash qual *canonical-clang-type-table*)
          (setf (gethash qual *canonical-clang-type-table*)
                (make-instance 'clang-type
                  :qual qual
                  :desugared desugared)))
      (make-instance 'clang-type
        :qual qual
        :desugared desugared)))

(defun read-c-integer (str)
  ;; Does not handle U, L
  (assert (string str))
  (let ((len (length str)))
    (assert (> len 0))
    (if (equal str "0")
        0
        (multiple-value-bind (base skip)
            (case (elt str 0)
              (#\0
               (if (and (> len 1) (find (elt str 1) "xX"))
                   (values 16 2)
                   (values 8 1)))
              ((#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
               (values 10 0))
              (t (error "Invalid integer literal: ~a" str)))
          ;; Find end
          (let ((end skip))
            (loop while (< end len)
               while (digit-char-p (elt str end) base)
               do (incf end))
            (if (eql skip end)
                (if (eql base 8)
                    (read-from-string str t nil :end end)
                    (error "Invalid integer literal: ~a" str))
                (let ((*read-base* base))
                  (read-from-string str t nil :start skip :end end))))))))

(defun int-to-c-hex (x)
  (if (< x 0)
      (format nil "-0x~(~x~)" (- x))
      (format nil "0x~(~x~)" x)))


;;; Massaging ASTs into proper form after parsing

(defgeneric remove-non-program-asts (ast-root file)
  (:documentation "Remove ASTs from ast-root that are not from the
actual source file")
  (:method ((ast-root clang-ast) (file string))
    ;; Minor performance optimization.  First remove the
    ;; top-level ASTs which are included from another file.
    ;; Then remove the sub-ASTs included from another file.
    (setf (children ast-root)
          (remove-if #'included-from (children ast-root)))
    (remove-asts-if ast-root #'included-from)
    ast-root))

(defun remove-asts-in-classes (ast classes)
  (remove-asts-if ast (lambda (o) (member (ast-class o) classes))))

(defun remove-template-expansion-asts (ast-root)
  (map-ast ast-root
           (lambda (a)
             (flet ((%remove-all-but (kind count)
                      (flet ((%is-kind (c)
                               (and (typep c 'clang-ast)
                                    (eql (ast-class c) kind))))
                        (let* ((children (children a))
                               (num (count-if #'%is-kind children)))
                          (when (> num count)
                            (setf (children a)
                                  (remove-if #'%is-kind children
                                             :from-end t
                                             :count (- num count))))))))
               (case (ast-class a)
                 ;; :TypeAliasTemplate does not cause problems, as the json
                 ;; does not have the expansions in it
                 (:ClassTemplate
                  (%remove-all-but :ClassTemplateSpecialization 0))
                 (:FunctionTemplate
                  (%remove-all-but :Function 1)))))))

(defun remove-attribute-asts (ast-root)
  "Remove any ASTs representing an __attribute__ specification from ast-root."
  (remove-asts-if ast-root
                  [{ends-with-subseq "ATTR"}
                   #'string-upcase
                   #'symbol-name
                   #'ast-class]))

(defun fix-line-directives (ast-root tmp-file)
  "Fix the `file` attribute for ASTs in AST-ROOT which appear after
a #line directive."
  (labels
      ((fix-line-directive (loc)
         "Fix the FILE field on LOC if LOC appears after a #line directive."
         (cond ((typep loc 'clang-macro-loc)
                (copy loc
                      :spelling-loc
                      (fix-line-directive (clang-macro-loc-spelling-loc loc))
                      :expansion-loc
                      (fix-line-directive (clang-macro-loc-expansion-loc loc))))
               ((and (typep loc 'clang-loc)
                     (clang-loc-presumed-line loc)
                     (null (included-from loc))
                     (not (member (file loc)
                                  (list "<built-in>" "<scratch space>" tmp-file)
                                  :test #'equal)))
                ;; When the presumed-line field is set for the location,
                ;; the location is not included from another file, and
                ;; the file exists on the disk and is not equal to tmp-file,
                ;; loc appears after a #line directive and the file
                ;; attribute needs to be touched up.
                (copy loc :file tmp-file :presumed-line nil))
               (t loc))))
    (map-ast ast-root
             (lambda (ast)
               (when-let ((range (ast-range ast)))
                 (setf (ast-range ast)
                       (make-clang-range
                        :begin (fix-line-directive (clang-range-begin range))
                        :end (fix-line-directive (clang-range-end range)))))))))

(defun remove-file-from-asts (ast-root tmp-file)
  "Remove the file attribute from the ASTs in AST-ROOT which are located
in TMP-FILE (the original genome)."
  (labels
      ((remove-file (loc)
         "Remove the file attribute from LOC when the file is equal to TMP-FILE."
         (cond ((typep loc 'clang-macro-loc)
                (copy loc
                      :spelling-loc
                      (remove-file (clang-macro-loc-spelling-loc loc))
                      :expansion-loc
                      (remove-file (clang-macro-loc-expansion-loc loc))))
               ((typep loc 'clang-loc)
                (copy loc
                      :file (unless (equal tmp-file (file loc))
                              (file loc))
                      :included-from (unless (equal tmp-file (included-from loc))
                                       (included-from loc))))
               (t loc))))
    (map-ast ast-root
             (lambda (ast)
               (when-let ((range (ast-range ast))
                          (_ (equal (file ast) tmp-file)))
                 (setf (ast-range ast)
                       (make-clang-range
                        :begin (remove-file (clang-range-begin range))
                        :end (remove-file (clang-range-end range)))))))))

(defun line-offsets (str &aux (byte 0))
  "Return a list with containing the byte offsets of each new line in STR."
  (cons 0 (iter (for c in-string str)
                (incf byte (string-size-in-octets (make-string 1 :initial-element c)))
                (when (eq c #\Newline)
                  (collect byte)))))

(defun convert-line-and-col-to-byte-offsets
    (ast-root source-text &aux (line-offsets (line-offsets source-text)))
  "Convert AST range begin/ends in AST-ROOT from line and column pairs to
byte offsets.

* AST-ROOT root of the AST tree for SOURCE-TEXT
* SOURCE-TEXT string with the source text of the program"
  (labels
      ((to-byte-offset (line col)
         "Convert the given LINE and COL to a byte offset."
         (+ (1- col) (nth (1- line) line-offsets)))
       (convert-loc (loc)
         "Populate the given LOC's offset field with the byte offset."
         (cond ((typep loc 'clang-macro-loc)
                (copy loc
                      :spelling-loc
                      (convert-loc (clang-macro-loc-spelling-loc loc))
                      :expansion-loc
                      (convert-loc (clang-macro-loc-expansion-loc loc))))
               ((null (file loc))
                (make-clang-loc
                 :line (clang-loc-line loc)
                 :offset (to-byte-offset (clang-loc-line loc)
                                         (clang-loc-col loc))
                 :tok-len (clang-loc-tok-len loc)))
               (t loc))))
    (map-ast ast-root
             (lambda (ast)
               (when-let* ((range (ast-range ast))
                           (_ (and (not (eq :TopLevel (ast-class ast)))
                                   (null (file ast)))))
                 (setf (ast-range ast)
                       (make-clang-range
                        :begin (convert-loc (clang-range-begin range))
                        :end (convert-loc (clang-range-end range)))))))))

(defun multibyte-characters (str &aux (byte 0))
  "Return a listing of multibyte character byte offsets and their length in STR."
  (iter (for c in-string str)
        (for len = (string-size-in-octets (make-string 1 :initial-element c)))
        (incf byte len)
        (when (> len 1)
          (collecting (cons byte (1- len))))))

(defun fix-multibyte-characters
    (ast-root source-text &aux (mb-chars (multibyte-characters source-text)))
  "Convert AST range begin/ends in AST-ROOT from byte offsets to character
offsets to support source text with multibyte characters.

* AST-ROOT root of the AST tree for SOURCE-TEXT
* SOURCE-TEXT string with the source text of the program"
  (labels
      ((byte-offset-to-chars (offset)
         "Convert the given byte OFFSET to a character offset."
         (- offset
            (iter (for (pos . incr) in mb-chars)
                  (while (<= pos offset))
                  (summing incr))))
       (fix-mb-chars (loc)
         "Convert the given LOC using byte offsets to one using character
          offsets."
         (cond ((typep loc 'clang-macro-loc)
                (copy loc
                      :spelling-loc
                      (fix-mb-chars (clang-macro-loc-spelling-loc loc))
                      :expansion-loc
                      (fix-mb-chars (clang-macro-loc-expansion-loc loc))))
               ((null (file loc))
                (make-clang-loc
                 :line (clang-loc-line loc)
                 :offset (byte-offset-to-chars (clang-loc-offset loc))
                 :tok-len (- (byte-offset-to-chars
                              (+ (clang-loc-offset loc)
                                 (clang-loc-tok-len loc)))
                             (byte-offset-to-chars (clang-loc-offset loc)))))
               (t loc))))
    (map-ast ast-root
             (lambda (ast)
               (when-let* ((range (ast-range ast))
                           (_ (and (not (eq :TopLevel (ast-class ast)))
                                   (null (file ast)))))
                 (setf (ast-range ast)
                       (make-clang-range
                        :begin (fix-mb-chars (clang-range-begin range))
                        :end (fix-mb-chars (clang-range-end range)))))))))

(defun remove-loc-attribute (ast-root)
  "Remove the :LOC attribute from ASTs in AST-ROOT."
  ;; Note: Removing the convert-slot-value method for :loc slots
  ;; leads to errors as the AST file and line may be specified
  ;; in the :loc slot and elided in the later :range slot.
  ;; We need to call `cached-aget` with the fields in the :loc
  ;; to allow them to be properly set on the :range later.
  (map-ast ast-root
           (lambda (ast)
             (setf (ast-annotations ast)
                   (adrop (list :loc) (ast-annotations ast))))))

;;; Macro-related code

(defun build-macro (str &key i-file)
  "Create a NEW-CLANG-MACRO structure from the macro definition in STR
and the given I-FILE where the macro definition may be found."
  (let ((slen (length str)))
    (assert (>= slen 7))
    (assert (string= "#define" str :end2 7))
    (let ((pos 7))
      ;; Skip whitespace
      (iter (while (< pos slen))
            (while (whitespacep (elt str pos)))
            (incf pos))
      ;; get name
      (let ((name-start pos) c)
        (iter (while (< pos slen))
              (setf c (elt str pos))
              (while (or (eql c #\_) (alphanumericp c)))
              (incf pos))
        ;; [name-start,pos) is the name
        (let* ((name (subseq str name-start pos))
               (body (subseq str name-start))
               (hash (sxhash body)))  ;; improve this hash
          (make-clang-macro :hash hash
                            :body body
                            :name name
                            :i-file i-file))))))

(defparameter *clang-frontend-flags-whitelist*
    (list "-fcxx-exceptions"
          "-fgnuc-version"
          "-Wno-everything"
          "-Wno-return-type"
          "-std"
          "-ansi")
  "Whitelist of compiler flags to pass thru to the clang frontend.")

(defun clang-frontend-flags (flags)
  "Return the subset of flags required for parsing a source file
using the clang front-end.

* FLAGS: list of normalized compiler flags"
  (iter (for f in flags)
        (for p previous f)
        ;; Include file paths or macro definitions
        (when (or (string= p "-I") (string= p "-D"))
          (appending (list p f)))
        ;; Macro definition without whitespace
        (when (and (not (string= f "-D"))
                   (not (string= f "\"-D\""))
                   (or (starts-with-subseq "-D" f)
                       (starts-with-subseq "\"-D" f)))
          (appending (list f)))
        ;; Special cases
        (when (member f *clang-frontend-flags-whitelist*
                      :test (flip #'starts-with-subseq))
          (appending (list f)))))

(defun dump-preprocessor-macros (obj)
  "Return a list of CLANG-MACRO structures with the macro definitions
in OBJ's source-text.  The macros are populated after evaluating pre-processor
if/else clauses."
  (with-temporary-file-of (:pathname src-file :type (ext obj))
    (genome-string obj)
    (let ((file nil)
          (file-line-scanner (create-scanner "^# [0-9]+ \"(.*)\"")))
      (iter (for line in (nest (remove-if #'emptyp)
                               (mapcar #'trim-whitespace)
                               (split-sequence #\Newline)
                               (shell "clang -dD -E ~{~a~^ ~} ~a"
                                      (clang-frontend-flags (flags obj))
                                      src-file)))
            (when (starts-with #\# line)
              (if (starts-with-subseq "#define" line)
                  (collect (build-macro line
                                        :i-file (unless (equal file src-file)
                                                  (normalize-file-for-include
                                                   obj file))))
                  (register-groups-bind (new-file)
                      (file-line-scanner line)
                    (when (and new-file
                               (not (find-if {equalp "bits"}
                                             (pathname-directory new-file))))
                      (setf file new-file)))))))))

(defgeneric compute-macro-extent (source-text off len)
  (:documentation "Compute the length of a macro occurrence in
the source-text.  SOURCE-TEXT is the source string, OFF the starting offset,
LEN the length of the macro name.")
  (:method (source-text off len &aux (source-text-len (length source-text)))
    (assert (<= 0 off))
    (assert (< 0 len))
    (assert (<= (+ off len) source-text-len))
    (let ((i (+ off len)))
      ;; Skip over whitespace after macro
      (iter (while (< i source-text-len))
            (while (whitespacep (elt source-text i)))
            (incf i))
      (if (or (>= i source-text-len)
              (not (eql (elt source-text i) #\()))
          len ;; give up; could not find macro arguments
          (let ((end (cpp-scan source-text (constantly t)
                               :start i :skip-first t)))
            (- end off))))))

(defgeneric encapsulate-macro-expansions-cheap (ast-root macros source-text)
  (:documentation "Replace macro expansions with :MACROEXPANSION nodes.")
  (:method ((ast-root clang-ast) (macros list) (source-text string))
    (map-ast ast-root
             {encapsulate-macro-expansion-cheap-below-node _
                                                           macros
                                                           source-text})))

(defgeneric encapsulate-macro-expansion-cheap-below-node (ast-root
                                                          macros
                                                          source-text)
  (:documentation "Walk over the children of AST, combining those that are
from the same macroexpansion into a single macroexpansion node.")
  (:method ((ast clang-ast) (macros list) (source-text string))
    (labels ((%is-macro-child-segment-ast (ast macro-child-segment)
               "Return true if the given AST should be grouped with the
               existing nodes in the MACRO-CHILD-SEGMENT.  The AST is
               part of the MACRO-CHILD-SEGMENT if it is a macro
               expansion node with the same expansion offset as the existing
               nodes in MACRO-CHILD-SEGMENT."
               (when-let* ((range (ast-range ast))
                           (begin (clang-range-begin range))
                           (end (clang-range-end range)))
                 (and (typep begin 'clang-macro-loc)
                      (typep end 'clang-macro-loc)
                      (= (offset (clang-macro-loc-expansion-loc begin))
                         (offset (clang-macro-loc-expansion-loc end)))
                      (or (null macro-child-segment)
                          (= (offset (clang-macro-loc-expansion-loc begin))
                             (nest (offset)
                                   (clang-macro-loc-expansion-loc)
                                   (clang-range-begin)
                                   (ast-range)
                                   (car macro-child-segment)))))))
             (%find-macro (name)
               "Return the macro with the given NAME in MACROS."
               (find name macros :test #'equal :key #'macro-name))
             (%get-macro-name (loc)
               "Return the name of the macro in the source text of OBJ at LOC."
               (let ((e-loc (clang-macro-loc-expansion-loc loc)))
                 (subseq source-text
                         (offset e-loc)
                         (+ (offset e-loc) (tok-len e-loc)))))
             (%function-like-macro-p (macro &optional seen)
               "Return true if MACRO is a function-like macro with arguments."
               (when (and macro (not (member macro seen)))
                 (let ((body-wo-name
                        (nest (trim-whitespace)
                              (subseq (macro-body macro)
                                      (length (macro-name macro))))))
                   ;; This is a function-like macro if the macro body
                   ;; (1) starts with an arguments list or
                   ;; (2) is itself a function-like macro
                   (or (and (starts-with #\( body-wo-name)
                            (cpp-scan body-wo-name (constantly t)
                                      :skip-first t))
                       (%function-like-macro-p (%find-macro body-wo-name)
                                               (cons macro seen))))))
             (%create-macro-loc-end (macro loc)
               "Create the end location for the macro expansion node."
               (cond ((typep loc 'clang-macro-loc)
                      ;; mark this as a not a macro arg expansion
                      ;; and recompute the expansion location end
                      (copy loc
                            :expansion-loc
                            (nest (%create-macro-loc-end macro)
                                  (clang-macro-loc-expansion-loc loc))
                            :is-macro-arg-expansion nil))
                     ((typep loc 'clang-loc)
                      ;; compute the macro end offset by adding the
                      ;; the current offset to the length of the
                      ;; macro including macro arguments for
                      ;; function-like macros
                      (copy loc
                            :offset (+ (offset loc)
                                       (if (%function-like-macro-p macro)
                                           (compute-macro-extent source-text
                                                                 (offset loc)
                                                                 (tok-len loc))
                                           (tok-len loc)))
                            :tok-len 0))))
             (%create-macro-ast (macro-child-segment)
               "Create a single macroexpansion AST node from the given
               MACRO-CHILD-SEGMENT nodes which the macro is composed of.
               These nodes will be stored in the :macro-child-segment
               attribute of the macroexpansion AST."
               (let* ((b-loc (nest (clang-range-begin)
                                   (ast-range)
                                   (car macro-child-segment)))
                      (e-loc (nest (clang-range-end)
                                   (ast-range)
                                   (lastcar macro-child-segment)))
                      (macro (%find-macro (%get-macro-name b-loc))))
                 (make-instance 'clang-ast
                  :class :macroexpansion
                  :range (make-clang-range
                          :begin (copy b-loc :is-macro-arg-expansion nil)
                          :end (%create-macro-loc-end macro e-loc))
                  :annotations
                  (list (cons :macro-child-segment macro-child-segment)
                        (cons :macro macro))))))
      (unless (eql (ast-class ast) :macroexpansion)
        ;; Scan the children of ast, grouping those that are macro expansion
        ;; nodes of the same offset.
        (setf (children ast)
              (let ((macro-child-segment nil))
                (iter (for child in (children ast))
                      (when (and macro-child-segment
                                 (not (%is-macro-child-segment-ast
                                       child
                                       macro-child-segment)))
                        (collect (%create-macro-ast macro-child-segment)
                                 into results)
                        (setf macro-child-segment nil))
                      (if (%is-macro-child-segment-ast child macro-child-segment)
                          (push child macro-child-segment)
                          (collect child into results))
                      (finally
                       (when macro-child-segment
                         (appendf results
                                  `(,(%create-macro-ast macro-child-segment))))
                       (return results)))))))))

(defun fix-overlapping-declstmt-children (ast source-text)
  (map-ast ast
           (lambda (a)
             (when (eq :declstmt (ast-class a))
               (fix-overlapping-declstmt-children-at-node a source-text)))))

(defun fix-overlapping-declstmt-children-at-node (ast source-text)
  "Separate consecutive, overlapping decl children in a :DeclStmt node
so their text ranges in the source do not overlap, if possible.  This
mimics the previous behavior within clang-mutate."
  (let ((child-asts (children ast)))
    (let (prev pos)
      (when (and (typep (car child-asts) 'clang-ast)
                 (member (ast-class (car child-asts)) *clang-decl-kinds*))
        (setf prev (car child-asts))
        (setf pos (begin-offset prev)))
      (do* ((e (cdr child-asts) (cdr e))
            (c (car e) (car e)))
           ((null e))
        (if (typep c 'clang-ast)
            (let ((next-pos (begin-offset c))
                  (end (end-offset c)))
              (if (member (ast-class c) *clang-decl-kinds*)
                  (progn
                    (if prev
                        (if (and next-pos end)
                            (if (< (end-offset prev) next-pos)
                                ;; things are fine -- no overlap
                                (setf prev c pos next-pos)
                                ;; There is overlap -- find the next
                                ;; position
                                (let ((comma-pos
                                       (cpp-scan source-text
                                                 (lambda (c) (eql c #\,))
                                                 :start pos
                                                 :end (1+ end))))
                                  (if comma-pos
                                      (setf pos (1+ comma-pos)
                                            (begin-offset c) pos
                                            prev c)
                                      ;; Failed to find comma; change nothing
                                      (setf prev c
                                            pos next-pos))))
                            (setf prev c
                                  pos next-pos))))
                  (setf prev c
                        pos next-pos)))
            (setf prev nil pos nil)))))
  ast)

(defun fix-ancestor-ranges (ast)
  "Normalize the ast so the range of each node is a superset
of the ranges of its children"
  (let (changed?)
    (flet ((%normalize (a)
             (multiple-value-bind (begin end)
                 (begin-and-end-offsets a)
               (let ((min-begin begin)
                     (max-end end))
                 (iter (for c in (children a))
                       (when (and (typep c 'clang-ast)
                                  (equal (file a) (file c)))
                         (multiple-value-bind (cbegin cend)
                             (begin-and-end-offsets c)
                           (when (and cbegin
                                      (or (null min-begin)
                                          (> min-begin cbegin)))
                             (setf min-begin cbegin))
                           (when (and cend
                                      (or (null max-end)
                                          (< max-end cend)))
                             (setf max-end cend)))))
                 (unless (and (eql min-begin begin)
                              (eql max-end end))
                   (setf changed? t)
                   (setf (ast-range a)
                         (make-clang-range
                          :begin (make-clang-loc
                                  :file (file a)
                                  :line (nest (line)
                                              (clang-range-begin)
                                              (ast-range a))
                                  :offset min-begin)
                          :end (make-clang-loc
                                :file (file a)
                                :line (nest (line)
                                            (clang-range-end)
                                            (ast-range a))
                                :offset max-end))))))))
      ;; Fixpoint for normalization of ranges
      (loop
         (setf changed? nil)
         (map-ast ast #'%normalize)
         (map-ast-postorder ast #'%normalize)
         (unless changed? (return ast))))))

(defun combine-overlapping-siblings (ast-root)
  (labels ((%sorted-children (children)
             "Sort the children in textual order."
             ;; This is required to match the behavior of old-clang
             ;; in `collect-children`.
             (stable-sort
              children
              (lambda (a b)
                (bind (((:values a-begin a-end)
                        (begin-and-end-offsets a))
                       ((:values b-begin b-end)
                        (begin-and-end-offsets b)))
                      ;; If ASTs start at the same place, put the
                      ;; larger one first so parent-child combining
                      ;; below works nicely.
                      (cond ((or (null b-begin) (null b-end)) t)
                            ((or (null a-begin) (null a-end)) nil)
                            ((= a-begin b-begin) (> a-end b-end))
                            (t (< a-begin b-begin)))))))
            (%combine-overlapping-siblings-p (prev child)
              "Return T if the AST siblings PREV and CHILD overlap
               and should be combined into a single node."
              (and prev (ast-range prev) (ast-range child)
                   (< (begin-offset child)
                      (+ (end-offset prev) (end-tok-len prev))))))
    (map-ast ast-root
             (lambda (ast)
               (setf (children ast)
                     (let ((prev nil))
                       (iter (for child in (%sorted-children (children ast)))
                             (if (%combine-overlapping-siblings-p prev child)
                                 (progn
                                   (setf (end-offset prev)
                                         (max (+ (end-offset prev)
                                                 (end-tok-len prev))
                                              (+ (end-offset child)
                                                 (end-tok-len child)))
                                         (end-tok-len prev) 0)
                                   (push child (children prev)))
                                 (progn
                                   (setf prev child)
                                   (collect child))))))))))

(defun decorate-ast-with-strings (ast source-text)
  (labels
      ((%assert1 (i cbegin c)
         (assert (>= cbegin i) ()
                 "Offsets out of order: i = ~a,~
                  cbegin = ~a, c = ~a, range = ~a"
                 i cbegin c
                 (ast-range c)))
       (%safe-subseq (seq start end)
         (subseq seq start (if (<= end start) start end)))
       (%decorate (a)
         ;; At ast node A, split the parts of the source
         ;; that are not in the children into substrings
         ;; that are placed between the children.  Do not
         ;; place strings for children for whom offsets
         ;; cannot be computed
         (let ((children (children a)))
           (multiple-value-bind (begin end)
               (begin-and-end-offsets a)
             (when (and begin end (null (file a)))
               (let ((i begin))
                 (setf
                  (children a)
                  (nconc
                   (iter
                    (for c in children)
                    (when (and (typep c 'clang-ast) (null (file c)))
                      (multiple-value-bind (cbegin cend)
                          (begin-and-end-offsets c)
                        (when cbegin
                          (%assert1 i cbegin c)
                          (collect (%safe-subseq source-text i cbegin))
                          (when (and cend (< i cend))
                            (setf i cend)))))
                    (collect c))
                   (list (%safe-subseq source-text i end))))))))))
    (map-ast ast #'%decorate))
  ast)

(defgeneric compute-full-stmt-annotation (obj ancestors)
  (:documentation "Fills in the :FULL-STMT annotation on clang ast
nodes, as needed.")
  (:method ((obj clang-ast) ancestors)
    (let ((parent (car ancestors)))
      (let ((parent-class (and parent (ast-class parent)))
            (obj-class (ast-class obj)))
        (when
            (case parent-class
              ((nil :TopLevel) (ast-is-decl obj))
              (:CompoundStmt (not (eql obj-class :CompoundStmt)))
              ;; (:DefaultStmt t)
              (:LabelStmt t)
              (:Function (eql obj-class :CompoundStmt))
              ;; first child of a CastStmt is the case expression
              (:CaseStmt (not (eql-nth-ast-child obj parent 0)))
              (:DoStmt
               (and (not (eql obj-class :CompoundStmt))
                    (eql-nth-ast-child obj parent 0)))
              (:WhileStmt
               (and (not (eql obj-class :CompoundStmt))
                    (eql-nth-ast-child obj parent 1)))
              (:ForStmt
               (and (not (eql obj-class :CompoundStmt))
                    (eql-nth-ast-child obj parent 3)))
              ;; Case for :CXXForRangeStmt here
              (:IfStmt
               (and (not (eql obj-class :CompoundStmt))
                    (not (eql-nth-ast-child obj parent 0))))
              (t nil))
          (setf (ast-annotation obj :full-stmt) t))))
    obj))

(defun compute-full-stmt-annotations (ast)
  (map-ast-with-ancestors ast #'compute-full-stmt-annotation))

(defgeneric compute-guard-stmt-annotation (obj ancestors)
  (:documentation "Fills in the :GUARD-STMT annotation on clang
ast nodes, as needed")
  (:method ((obj clang-ast) ancestors)
    (when ancestors
      (let ((parent (car ancestors)))
        (when (not (is-single-line-stmt obj parent))
          (case (ast-class parent)
            ((:CapturedStmt :CompoundStmt :CXXCatchStmt :DoStmt
                            :ForStmt :IfStmt :SwitchStmt :WhileStmt)
             (setf (ast-annotation obj :Guard-Stmt) t))))))
    obj))

(defun compute-guard-stmt-annotations (ast)
  (map-ast-with-ancestors ast #'compute-guard-stmt-annotation))

(defgeneric compute-syn-ctx (ast ancestors)
  (:documentation "Fill in the syn-ctx slot")
  (:method ((obj clang-ast) ancestors)
    (let* ((parent (car ancestors))
           (obj-class (ast-class obj))
           (syn-ctx
            (cond
              ((null parent) nil)
              ((and parent (null (cdr ancestors))) :toplevel)
              ((eql obj-class :Field) :Field)
              ((eql obj-class :CompoundStmt) :Braced)
              ((is-loop-or-if-body obj parent) :UnbracedBody)
              ((ast-full-stmt obj) :FullStmt)
              (t :Generic))))
      (setf (ast-syn-ctx obj) syn-ctx))
    obj))

(defgeneric fix-var-syn-ctx (ast)
  (:documentation "Fix the syn-ctx of Var and ParmVar nodes")
  (:method ((obj clang-ast))
    (let ((prev nil)
          (prev-var? nil))
      (unless (eql (ast-class obj) :toplevel)
        (iter (for c in (children obj))
              (when (typep c 'clang-ast)
                (case (ast-class c)
                  ((:Var :ParmVar)
                   ;; This logic makes single element ParmVar lists
                   ;; be :Generic.  Weird, but that's what clang-mutate
                   ;; did
                   (when prev-var?
                     (setf (ast-syn-ctx prev) :ListElt
                           (ast-syn-ctx  c) :FinalListElt))
                   (setf
                    prev c
                    prev-var? t))
                  (t (setf prev-var? nil prev nil)))))))
    obj))

(defun compute-syn-ctxs (ast)
  (map-ast-with-ancestors ast #'compute-syn-ctx)
  (map-ast ast #'fix-var-syn-ctx))

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

(defun move-semicolons-into-full-stmts (children)
  "Given a list of children of an AST node, move semicolons from
strings in the list they are in into preceding full stmt nodes.
Applied in preorder, this can migrate semicolons multiple levels down
the tree."
  (let ((p children))
    (loop
       (unless p (return))
       (let ((e (car p)))
         (when (and (typep e 'clang-ast)
                    (ast-full-stmt e)
                    (stringp (cadr p)))
           (let ((e-children (children e)))
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
  (let* ((children (children ast))
         (p children))
    (loop (unless p (return))
       (let ((e (car p)))
         (when (and (typep e 'clang-ast)
                    (stringp (cadr p))
                    (or (ast-full-stmt e) (eql (ast-class e) :field))
                    (stringp (lastcar (children e))))
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
  (:method ((a clang-ast) (str string))
    ;; default method
    (let ((c (children a)))
      (if (null c)
          (setf (children a) (list str))
          (let ((lc (lastcar c)))
            (if (stringp lc)
                (setf (children a)
                      (append (butlast c) (list (format nil "~a~a" lc str))))
                (setf (children a)
                      (append c (list str)))))))))

;;; There is a name collision with the labels functionn ADD-SEMICOLON
;;; in fixup-mutation.  TODO: change one of these names
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
  (:method ((ast clang-ast) (pos (eql :before)))
    (declare (ignorable pos))
    (let ((children (children ast)))
      (copy ast :children
            (cons (add-semicolon (car children) :before)
                  (cdr children)))))
  (:method ((ast clang-ast) (pos (eql :after)))
    (declare (ignorable pos))
    (copy ast :children
          (append (children ast) (list ";"))))
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
     (has-trailing-semicolon-p (lastcar (children ast))))
    (t nil)))

(defun fix-semicolons-ast (ast)
  "Move semicolons into the appropriate stmt nodes in the children of node AST"
  (move-semicolons-into-expr-stmts ast)
  (move-semicolons-into-full-stmts (children ast))
  ast)

(defun fix-semicolons (ast)
  "Move semicolons into appropriate stmt nodes in the tree rooted at AST"
  (map-ast ast #'fix-semicolons-ast)
  ast)

(defun populate-type-fields-from-symbol-table (obj name-symbol-table types)
  "Populate the `i-file`, `reqs`, and `decl` fields for clang-type
objects in TYPES using NAME-SYMBOL-TABLE."
  (labels ((populate-type-i-file (obj tp decl)
             (setf (slot-value tp 'i-file)
                   (ast-i-file obj decl)))
           (populate-type-reqs (tp decl)
             (setf (slot-value tp 'reqs)
                   (unless (type-i-file tp)
                     (nest (remove tp)
                           (remove nil)
                           (remove-duplicates)
                           (mapcar #'ast-type)
                           (cons decl (child-asts decl :recursive t))))))
           (populate-type-decl (tp decl)
             (setf (slot-value tp 'decl)
                   (source-text decl))))
    (iter (for tp in (nest (remove-duplicates)
                           (mapcar #'ct+-type)
                           (hash-table-values types)))
          (unless (and (type-i-file tp) (type-reqs tp))
            (when-let ((decl (type-decl-ast name-symbol-table tp)))
              (populate-type-i-file obj tp decl)
              (populate-type-reqs tp decl)
              (populate-type-decl tp decl)))
          (finally (return types)))))

(defun update-symbol-table (symbol-table ast-root)
  "Populate SYMBOL-TABLE with a mapping of AST ID -> AST(s) for all of
the decl ASTs in AST-ROOT."
  (labels ((symbol-ast-p (ast)
             "Return TRUE is AST should be included in the symbol table."
             (and (typep ast 'clang-ast)
                  (ast-is-decl ast)
                  (not (eq :TopLevel (ast-class ast))))))
    (map-ast ast-root
             (lambda (ast)
               (when (symbol-ast-p ast)
                 (setf (gethash (ast-id ast) symbol-table)
                       (list ast)))
               (when (and (symbol-ast-p (ast-referenceddecl ast))
                          (null (gethash (ast-id (ast-referenceddecl ast))
                                         symbol-table)))
                 (setf (gethash (ast-id (ast-referenceddecl ast)) symbol-table)
                       (list (ast-referenceddecl ast))))))
    symbol-table))

(defun update-name-symbol-table (name-symbol-table symbol-table)
  "Populate NAME-SYMBOL-TABLE with a mapping of AST name -> symbol ASTs
using the existing SYMBOL-TABLE."
  (iter (for (id asts) in-hashtable symbol-table)
        (declare (ignorable id))
        (iter (for ast in asts)
              (when (and ast (ast-name ast))
                (push ast (gethash (ast-name ast) name-symbol-table))))
        (finally (return name-symbol-table))))

(defun clear-symbol-table (symbol-table)
  "Remove entries for the current file from the SYMBOL-TABLE."
  (maphash (lambda (k asts)
             (setf (gethash k symbol-table)
                   (remove-if [#'null #'file] asts))
             (when (null (gethash k symbol-table))
               (remhash k symbol-table)))
           symbol-table)
  symbol-table)

(defun update-referenceddecl-from-symbol-table (ast-root symbol-table)
  "Update the :AST-REFERENCEDDECL field on ASTs in AST-ROOT to point to decls
in the SYMBOL-TABLE."
  (map-ast ast-root
           (lambda (ast)
             (when-let* ((_ (typep ast 'clang-ast))
                         (old-ref (ast-referenceddecl ast))
                         (new-ref (find old-ref
                                        (gethash (ast-id old-ref) symbol-table)
                                        :key #'ast-name :test #'name=)))
               (setf (ast-annotation ast :referenceddecl) new-ref))))
  ast-root)

(defun update-type-table (types symbol-table ast-root)
  "Populate TYPES with a mapping of type-hash -> NCT+ objects using the
ASTs in the existing SYMBOL-TABLE and AST-ROOT tree."
  (labels ((get-ct+-type (ast)
             (when-let* ((_ (typep ast 'clang-ast))
                         (tp (ast-type ast))
                         (storage-class (or (ast-annotation ast :storage-class)
                                            :none))
                         (tp+ (make-instance 'ct+
                                :type tp
                                :storage-class storage-class)))
               tp+)))
    ;; Populate from the symbol table containing decls in header files
    ;; outside the current file.
    (maphash (lambda (id asts)
               (declare (ignorable id))
               (iter (for ast in asts)
                     (when-let* ((tp+ (get-ct+-type ast))
                                 (_ (null (gethash (type-hash tp+) types))))
                       (setf (gethash (type-hash tp+) types) tp+))))
             symbol-table)
    ;; Populate from the AST ROOT all types in the current file.
    (map-ast ast-root
             (lambda (ast)
               (when-let* ((tp+ (get-ct+-type ast))
                           (_ (null (gethash (type-hash tp+) types))))
                 (setf (gethash (type-hash tp+) types) tp+))))
    types))

;; FIXME: When clang is converted to utilize functional trees,
;; this method should no longer be required.
(defmethod update-paths ((ast ast))
  "Modify AST in place with all paths updated to begin at the root AST."
  (labels ((update-paths-helper (ast &optional path)
             (if (typep ast 'clang-ast)
                 ;; clang AST, recurse into children
                 (setf (slot-value ast 'path) (reverse path)
                       (slot-value ast 'children)
                       (iter (for c in (children ast))
                             (for i upfrom 0)
                             (collect (if (typep c 'ast)
                                          (update-paths-helper c (cons i path))
                                          c)))))
             ast))
    (update-paths-helper ast)))

(defmethod parse-asts ((obj clang) &optional (source-text (genome-string obj)))
  (let ((*canonical-string-table* (make-hash-table :test 'equal))
        (*canonical-clang-type-table* (make-hash-table :test 'equal)))
    (with-slots (macros types symbol-table name-symbol-table) obj
      (multiple-value-bind (json tmp-file text-len)
          (clang-json obj)
        (let ((ast-root (clang-convert-json-for-file json tmp-file text-len))
              (macro-dump (dump-preprocessor-macros obj)))
          ;; Populate and massage auxilliary fields such as symbol tables
          ;; and types.
          (update-symbol-table symbol-table ast-root)
          (update-name-symbol-table name-symbol-table symbol-table)
          (remove-non-program-asts ast-root tmp-file)
          (update-referenceddecl-from-symbol-table ast-root symbol-table)
          (update-type-table types symbol-table ast-root)
          (setf macros (remove-if #'macro-i-file macro-dump))

          ;; Massage the ASTs identified by clang.
          (remove-asts-if ast-root #'ast-is-implicit)
          (remove-template-expansion-asts ast-root)
          (remove-attribute-asts ast-root)
          (fix-line-directives ast-root tmp-file)
          (remove-file-from-asts ast-root tmp-file)
          (convert-line-and-col-to-byte-offsets ast-root source-text)
          (fix-multibyte-characters ast-root source-text)
          (remove-loc-attribute ast-root)
          (encapsulate-macro-expansions-cheap ast-root macro-dump source-text)
          (fix-overlapping-declstmt-children ast-root source-text)
          (fix-ancestor-ranges ast-root)
          (combine-overlapping-siblings ast-root)
          (decorate-ast-with-strings ast-root source-text)
          (compute-full-stmt-annotations ast-root)
          (compute-guard-stmt-annotations ast-root)
          (compute-syn-ctxs ast-root)
          (fix-semicolons ast-root)
          (update-paths ast-root)
          (populate-type-fields-from-symbol-table obj name-symbol-table types)

          ast-root)))))


;;; Helper methods for computing attributes on new clang ast nodes

(defgeneric nth-ast-child (obj n)
  (:documentation
   "Returns the Nth child of OBJ that is an AST, starting
at zero, or NIL if there is none."))

(defmethod nth-ast-child ((obj clang-ast) n)
  (declare (type (and fixnum (integer 0)) n))
  (let ((children (children obj)))
    (loop
       (unless children (return nil))
       (let ((next-child (pop children)))
         (when (or (null next-child) ;; NIL is considered an AST
                   (typep next-child 'ast))
           (if (<= n 0)
               (return next-child)
               (decf n)))))))

(defgeneric pos-ast-child (child obj &key child-fn test)
  (:documentation
   "Returns the position of CHILD in the child list of OBJ (with children
failing the CHILD-FN test omitted, or NIL if CHILD does not satisfy
the test or is not present."))

(defmethod pos-ast-child (child (obj clang-ast)
                          &key (child-fn (complement #'stringp))
                            (test #'eql))
  (let ((pos 0)
        (children (children obj)))
    (loop
       (unless children (return nil))
       (let ((next-child (pop children)))
         (when (funcall child-fn next-child)
           (when (funcall test child next-child)
             (return pos))
           (incf pos))))))

(defun eql-nth-ast-child (obj parent n)
  (eql obj (nth-ast-child parent n)))

(defun is-single-line-stmt (s p)
  ;; ported from clang mutate, where there is this comment:
  ;;  Return true if the clang::Stmt is a statement in the C++ grammar
  ;;  which would typically be a single line in a program.
  ;;  This is done by testing if the parent of the clang::Stmt
  ;;  is an aggregation type.  The immediate children of an aggregation
  ;;  type are all valid statements in the C/C++ grammar.
  (and s p
       (let ((pc (ast-class p)))
         (flet ((%e () (error "Not handled yet in is-single-line-stmt: ~a" pc)))
           (case pc
             (:CompoundStmt t)
             (:CapturedStmt (%e))
             (:CXXForRangeStmt (%e))
             (:DoStmt (eql-nth-ast-child s p 0))
             (:ForStmt
              (eql s (nest (lastcar)
                           (remove-if-not {typep _ 'ast})
                           (children p))))
             ((:WhileStmt :SwitchStmt :CxxCatchStmt)
              (eql-nth-ast-child s p 1))
             ((:IfStmt)
              (let ((pos (pos-ast-child s p)))
                (or (eql pos 1) (eql pos 2))))
             (t nil))))))

(defun is-loop-or-if-body (s p)
  ;; Ported from clang-mutate
  (and (is-single-line-stmt s p)
       (case (ast-class p)
         ((:IfStmt :WhileStmt :ForStmt :DoStmt) t)
         (t nil))))


;;; Map over the nodes of an AST
;; FIXME: When clang is converted to utilize functional trees,
;; these methods should no longer be required.

(defgeneric map-ast (tree fn)
  (:documentation "Apply FN to each node of AST, in preorder.")
  (:method ((tree clang-ast) fn)
    (funcall fn tree)
    (dolist (c (children tree))
      (when (typep c 'clang-ast) (map-ast c fn)))
    tree)
  (:method (tree fn)
    (declare (ignorable tree fn))
    nil))

(defgeneric map-ast-with-ancestors (tree fn &optional ancestors)
  (:documentation "Apply FN to each node of the AST, and its list of ancestors,
in preorder.  The ancestor list is in decreasing order of depth in the AST.")
  (:method ((tree clang-ast) fn &optional ancestors)
    (funcall fn tree ancestors)
    (let ((ancestors (cons tree ancestors)))
      (dolist (c (children tree))
        (when (typep c 'clang-ast) (map-ast-with-ancestors c fn ancestors))))
    tree)
  (:method (tree fn &optional ancestors)
    (declare (ignore tree fn ancestors))
    nil))

(defgeneric map-ast-postorder (tree fn)
  (:documentation "Apply FN to each node of AST, in postorder.")
  (:method ((tree clang-ast) fn)
    (dolist (c (children tree))
      (when (typep c 'clang-ast) (map-ast-postorder c fn)))
    (funcall fn tree)
    tree))

(defgeneric map-ast-while (a fn)
  (:documentation "Apply FN to the nodes of AST A, stopping
the descent when FN returns NIL.")
  (:method ((a clang-ast) fn)
    (when (funcall fn a)
      (dolist (c (children a))
        (when (typep c 'clang-ast) (map-ast-while c fn))))))

(defgeneric map-ast-sets (ast fn &key key test)
  (:documentation "Evaluates FN at the nodes of AST, returning a list of
objects.  Returns the union of this list and the value computed at the
children.")
  (:method (ast fn &key (key 'identity) (test 'eql))
    (labels ((%recurse (a)
               ;; This is slow
               ;; In the future, memoize and use better data structures
               (let ((here (funcall fn a))
                     (child-sets
                      (iter (for c in (children a))
                            (when (typep c 'clang-ast)
                              (collect (%recurse c))))))
                 (reduce (lambda (s1 s2)
                           (union s1 s2 :key key :test test))
                         child-sets :initial-value here))))
      (%recurse ast))))

(defgeneric remove-asts-if (ast fn)
  (:documentation "Remove all subasts for which FN is true.")
  (:method ((ast clang-ast) fn)
    (let* ((children (children ast))
           (new-children (mapcar (lambda (a) (remove-asts-if a fn))
                                 (remove-if fn children))))
      (unless (and (length= children new-children)
                   (every #'eql children new-children))
        (setf (children ast) new-children)))
    ast)
  (:method (ast (fn t)) ast))

(defmethod find-if (predicate (ast clang-ast) &key key from-end end start)
  (declare (ignorable key from-end end start))
  (cond
    ((funcall predicate ast) ast)
    ((children ast)
     (iter (for child in (children ast))
           (when-let ((satisfied-p
                       (and (typep child 'clang-ast)
                            (find-if predicate child))))
             (return satisfied-p))))))

(defmethod find-if-not (predicate (ast clang-ast) &key key)
  (declare (ignorable key))
  (find-if (complement predicate) ast :key key))

(defun ast-nodes-in-subtree (ast)
  (let ((result nil))
    (map-ast ast (lambda (a) (push a result)))
    (nreverse result)))

(defmethod substitute-if (new-item predicate (ast clang-ast)
                          &key key copy start end from-end count
                          &allow-other-keys)
  (declare (ignorable key copy start end from-end count))
  (if (funcall predicate ast)
      new-item
      (let ((new-children (mapcar
                           (lambda (child)
                             (if (typep child 'clang-ast)
                                 (substitute-if new-item predicate child)
                                 child))
                           (children ast))))
        (if (every #'eql new-children (children ast))
            ast
            (copy ast :children new-children)))))

(defmethod mapcar (function (ast clang-ast) &rest more)
  (declare (ignorable more))
  (when-let ((value (funcall function ast)))
    (let ((new-children (and (typep value 'clang-ast)
                             (mapcar
                              (lambda (child)
                                (if (typep child 'clang-ast)
                                    (mapcar function child)
                                    child))
                              (children value)))))
      (if (every #'eql new-children (children value))
          value
          (copy value :children new-children)))))

(defmethod reduce (fn (ast clang-ast)
                   &key key initial-value ;; start end from-end
                   &allow-other-keys
                   &aux (accumulator initial-value))
  (setf accumulator
        (funcall fn accumulator (if key (funcall key ast) ast)))

  (dolist (c (children ast))
    (when (typep c 'ast)
      (setf accumulator
            (reduce fn c :key key :initial-value accumulator))))

  accumulator)


(defun cpp-scan (str until-fn &key (start 0) (end (length str))
                                (skip-first nil)
                                (angle-brackets))
  "Scan string STR from START to END, skipping over parenthesized
C/C++ things, and respecting C/C++ comments and tokens, until
either the end is reached, or a substring satisfying UNTIL-FN
is found.  Returns NIL on no match, or the satisfying position
of the match.  If ANGLE-BRACKETS is true then try to handle
template brackets < and >."
  ;;
  ;; The typical use case for this is scanning over a vardecl
  ;; looking for either a comma or a semicolon
  ;;
  ;; Handling < > is tricky in C++, since telling template
  ;; brackets apart from comparison or shift operators is difficult.
  ;; This code makes only a partial attempt to get it right.
  ;;
  ;; Improvements that could be made:
  ;;  -- Better handle improperly nested parens/brackets.  For
  ;;     example,   ( ... < ... ) should close the paren, since
  ;;     this must mean the < wasn't a bracket
  ;;  -- Exclude < from processing as a bracket when from the
  ;;     preceding context it could not be a template bracket.
  ;;
  (let ((pos start))
    (labels ((inc? (&optional (l 1))
               (setf skip-first nil)
               (when (>= (incf pos l) end)
                 (return-from cpp-scan nil)))
             (cpp-scan* (closing-char)
               ;; If CLOSING-CHAR is not NIL, it is the closing character
               ;; of a pair of matching parens/brackets/braces.
               (loop
                  (let ((c (elt str pos)))
                    (when (and (not closing-char)
                               (not skip-first)
                               (funcall until-fn c))
                      (return-from cpp-scan pos))
                    (if (whitespacep c)
                        (inc?)
                        (case c
                          ((#\() (inc?) (cpp-scan* #\)))
                          ((#\[) (inc?) (cpp-scan* #\]))
                          ((#\{) (inc?) (cpp-scan* #\}))
                          ((#\")
                           ;; Skip a string constant
                           (cpp-scan-string-constant))
                          ((#\') (cpp-scan-char-constant))
                          ;;
                          ((#\/)
                           (inc?)
                           (case (elt str pos)
                             ((#\/) (cpp-scan-//-comment))
                             ((#\*) (cpp-scan-/*-comment))))
                          (t
                           (cond ((and (eql c #\<)
                                       angle-brackets)
                                  (inc?)
                                  (cpp-scan* #\>))
                                 ((eql closing-char c)
                                  (inc?)
                                  (return))
                                 #|
                                 ((and (not closing-char)
                                 (not skip-first)
                                 (funcall until-fn c))
                                 (return-from cpp-scan pos))
                                 |#
                                 (t
                                  (inc?)))))))))
             (cpp-scan-string-constant ()
               (loop
                  (inc?)
                  (case (elt str pos)
                    ((#\") (inc?) (return))
                    ;; We don't have to parse \X, \U, etc.
                    ((#\\) (inc?)))))
             (cpp-scan-char-constant ()
               (loop
                  (inc?)
                  (case (elt str pos)
                    ((#\') (inc?) (return))
                    ((#\\) (inc?)))))
             (cpp-scan-//-comment ()
               (inc?)
               ;; NOTE: Windows
               (cpp-scan-until (string #\Newline)))
             (cpp-scan-/*-comment ()
               (inc?)
               (cpp-scan-until "*/"))
             (cpp-scan-until (s)
               "Scan until a substring equal to S is found"
               (let ((l (length s)))
                 (iter (until (string= str s :start1 pos :end1 (+ pos l)))
                       (inc?))
                 (inc? l))))
      (when (< pos end)
        (cpp-scan* nil)))))


;;; Language to symbol mapping
#-TREE-SITTER-C
(define-language-alias-mappings c ("c"))

#-TREE-SITTER-CPP
(define-language-alias-mappings
    cpp ("c plus plus" "c++" "c-plus-plus" "cc" "cp" "cpp" "cxx" "hpp"))
