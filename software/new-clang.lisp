;;; new-clang.lisp --- clang software representation
;;;
;;; DOCFIXME Need a page or so introduction to new clang
;;;   software objects.  Also, update software-evolution.texi
;;;   to describe the new class.
;;;
;;; @texi{new-clang}
(defpackage :software-evolution-library/software/new-clang
  (:nicknames :sel/software/new-clang :sel/sw/new-clang)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :metabang-bind
        :iterate
        :split-sequence
        :cl-ppcre
        :uiop/pathname
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/ast
        :software-evolution-library/software/source
        :software-evolution-library/software/parseable
        :software-evolution-library/components/formatting
        :software-evolution-library/components/searchable
        :software-evolution-library/components/fodder-database
        :software-evolution-library/software/clang)
  (:import-from :uiop :nest)
  (:import-from :anaphora :awhen :it)
  (:import-from :jsown)
  (:import-from :string-case :string-case)
  (:import-from :babel :string-size-in-octets)
  (:export :new-clang
           :new-clang-ast
           :new-clang-range-begin
           :new-clang-loc-line
           :make-new-clang-ast
           :ast-id
           :ast-attr
           :ast-file
           :new-clang-ast-attrs
           :symbol-table
           :name-symbol-table
           :combine-overlapping-siblings
           :decorate-ast-with-strings
           :clang-convert-json-for-file
           :make-statement-new-clang
           :*new-clang?*
           :nct+
           :nct+-type
           :cpp-scan
           :ast-attr
           :within-ast-range
           :ast-range-str))
(in-package :software-evolution-library/software/new-clang)
(in-readtable :curry-compose-reader-macros)


;;; Code for adapting tests to use old or new clang front
;;; ends, controllably

;; This is DEFVAR so I can set the var and reload this file
;; without losing that value, which is useful for debugging.
(defvar *new-clang?* nil "When true, use NEW-CLANG instead of CLANG")


;;; Global variables
(declaim (special *aget-cache*))
(declaim (special *canonical-string-table*))
(declaim (special *canonical-new-clang-type-table*))
(declaim (special *new-clang-json-file*))

#-windows
(defun get-clang-default-includes ()
  "Retrieve the paths on the default clang system include search path."
  (nest
   (when (which "clang"))
   (with-temp-file-of (bin "cpp") "")
   (multiple-value-bind (stdout stderr exit)
       (shell "clang -v ~a" bin)
     (declare (ignorable stdout exit))
     (register-groups-bind (include-search-paths)
         ("(?s)include <...> search starts here:(.*)End of search list"
          stderr)
       (->> (split-sequence #\Newline include-search-paths
                            :remove-empty-subseqs t)
            (mapcar #'trim-whitespace)
            (mapcar [#'namestring
                     #'ensure-directory-pathname
                     #'canonical-pathname]))))))

#+windows
(defun get-clang-default-includes ()
  "Retrieve the paths on the default clang system include search path."
  (nest
   (when (which "clang-cl.exe"))
   (with-temp-file-of (bin "cpp") "")
   (multiple-value-bind (stdout stderr exit)
       (shell "clang-cl.exe -v ~a" bin)
     (declare (ignorable stdout exit))
     (register-groups-bind (include-search-paths)
         ("(?s)include <...> search starts here:(.*)End of search list"
          stderr)
       (remove ""
               (->> (split-sequence #\Newline include-search-paths
                                    :remove-empty-subseqs t)
                    (mapcar #'trim-whitespace)
                    (mapcar [#'namestring
                             #'ensure-directory-pathname
                             #'canonical-pathname]))
               :test 'equal)))))

(defvar *clang-default-includes* (get-clang-default-includes)
  "List of paths representing the default clang system includes search path.
These are required as -I flags as invoking clang -cc1 (required for ast-dump)
only invokes the clang front-end.
See also: https://clang.llvm.org/docs/FAQ.html#id2.")


;;; new-clang software objects

(define-software new-clang (clang-base)
  ((symbol-table
    :initarg :symbol-table :accessor symbol-table
    :initform (make-hash-table :test #'equal)
    :copier copy-hash-table
    :type hash-table
    :documentation "Map from IDs to objects")
   (name-symbol-table
    :initarg :name-symbol-table :accessor name-symbol-table
    :initform (make-hash-table :test #'equal)
    :copier copy-hash-table
    :type hash-table
    :documentation "Map from name strings to declaration objects."))
  (:documentation
   "C language (C, C++, C#, etc...) ASTs using Clang, C language frontend
   for LLVM.  See http://clang.llvm.org/.  This is for ASTs from Clang 9+."))

(defstruct (new-clang-ast (:include clang-ast-base)
                          (:conc-name new-clang-ast-))
  (path nil :type list)          ;; Path to subtree from root of tree.
  (children nil :type list)      ;; Remainder of subtree.
  (stored-hash nil :type (or null fixnum))
  ;; Class symbol for this ast node
  (class nil :type symbol)
  ;; Association list of attr name -> value pairs
  (attrs nil :type list)
  ;; Hashed id number from Clang
  (id nil :type (or null integer))
  ;; Syntactic context
  (syn-ctx nil :type symbol)
  ;; aux data
  (aux-data nil :type list))

;; Special subclass for :CXXOperatorCallExpr nodes
(defstruct (cxx-operator-call-expr (:include new-clang-ast))
  ;; POS is the "actual" position of the operator in the
  ;; list of child ASTs (ignoring non-AST children).
  ;; When computing ranges, and when computing
  ;; source text, put it there.
  ;; The type FIXNUM is too big; figure out how much
  ;; smaller this can be made.
  (pos nil :type (or null fixnum)))

(defclass new-clang-type ()
  ((qual :reader new-clang-type-qual
         :initform nil
         :initarg :qual
         :documentation "Translation of the qualType attribute
of clang json type objects")
   (desugared :reader new-clang-type-desugared
              :initform nil
              :initarg :desugared
              :documentation "Translation of the desugaredQualType
attribute of clang json objects")
   ;; Slots filled in by parsing the qual or desugred type
   (modifiers :initarg :modifiers
              :type integer
              :reader new-clang-type-modifiers)
   (array :initarg :array
          :type string
          :reader type-array)
   (i-file :initarg :i-file
           :type (or null string)
           :reader new-clang-type-i-file
           :initform nil)
   ;; Name is the underlying name sans the modifiers and array
   (name :initarg :name
         :type string
         :reader type-name)
   (reqs :accessor type-reqs
         :initarg :reqs
         :type list ;; of new-clang-type objects
         :documentation "List of types that are required to understand
this type."))
  (:documentation "Objects representing C/C++ types.  Canonicalized
on QUAL and DESUGARED slots."))

(defclass nct+ ()
  ((type :initarg :type
         :reader nct+-type
         :type new-clang-type)
   (storage-class :initarg :storage-class
                  :reader type-storage-class
                  :initform :none
                  :type (member :none :static :extern :register :__private_extern__)))
  (:documentation "Wrapper object that is intended to behave like
SEL/SW/CLANG:CLANG-TYPE.  This means it must have some information
that is not strictly speaking about types at all (storage class)."))

(defmethod print-object ((obj new-clang-ast) stream)
  "Print a representation of the clang-ast-node OBJ to STREAM.
* OBJ clang-ast to print
* STREAM stream to print OBJ to
"
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t)
        (format stream "~a~@[ ~a~] ~a" (ast-class obj) (ast-name obj)
                (ast-path obj)))))

(defmethod print-object ((obj new-clang-type) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t)
        (format stream "~{~a~^ ~}"
                (append (let ((qual (new-clang-type-qual obj)))
                          (when qual (list ":QUAL" qual)))
                        (let ((desugared (new-clang-type-desugared obj)))
                          (when desugared (list ":DESUGARED" desugared))))))))


;;; new-clang software interface

(defmethod initialize-instance :after ((obj new-clang) &key &allow-other-keys)
  "Wrapper after the constructor to ensure the flags are in a normalized form
with absolute, canonical paths."
  (setf (slot-value obj 'flags)
        (normalize-flags (original-directory obj)
                         (flags obj)))
  obj)

(defmethod (setf flags) :after ((flags list) (obj new-clang))
  "Wrapper after the flags setf to ensure the flags are in a
normalized form with absolute, canonical paths."
  (declare (ignorable flags))
  (setf (slot-value obj 'flags)
        (normalize-flags (original-directory obj)
                         (flags obj))))

(defun flags-to-include-dirs (flags)
  "Return the listing of include search paths in FLAGS

* FLAGS: list of normalized compiler flags"
  (iter (for f in flags)
        (for p previous f)
        (when (string= p "-I")
          (collect f))))

(defun clang-frontend-flags (flags)
  "Return the subset of flags required for parsing a source file
using the clang front-end.

* FLAGS: list of normalized compiler flags"
  (iter (for f in flags)
        (for p previous f)
        (when (or (string= p "-I") (string= p "-D"))
          (appending (list p f)))
        (when (and (not (string= f "-D"))
                   (not (string= f "\"-D\""))
                   (or (starts-with-subseq "-D" f)
                       (starts-with-subseq "\"-D" f)))
          (appending (list f)))))

(defmethod stmt-asts ((obj new-clang))
  (let ((stmt-asts nil))
    (map-ast-while
     (ast-root obj)
     (lambda (a)
       (if (function-decl-p a)
           (progn
             (map-ast a (lambda (b)
                          (unless (or (eql (ast-class b) :ParmVar)
                                      (function-decl-p b))
                            (push b stmt-asts))))
             nil)
           t)))
    (nreverse stmt-asts)))

(defmethod non-stmt-asts ((obj new-clang))
  "Collect a list of all ASTs (except the root) that are not
in or below function declarations"
  (let ((non-stmt-asts nil)
        (root (ast-root obj)))
    (map-ast-while
     root
     (lambda (a)
       (and (not (function-decl-p a))
            (progn
              (unless (eql a root) (push a non-stmt-asts))
              t))))
    (nreverse non-stmt-asts)))

(defmethod functions ((obj new-clang))
  (let ((functions nil))
    (map-ast (ast-root obj)
             (lambda (a)
               (when (and (function-decl-p a)
                          (function-body obj a))
                 (push a functions))))
    (nreverse functions)))

(defmethod prototypes ((obj new-clang))
  (let ((protos nil))
    (map-ast (ast-root obj)
             (lambda (a)
               (when (function-decl-p a)
                 (push a protos))))
    (nreverse protos)))

(defmethod binding-for-function ((obj new-clang) functions name arity)
  (or (random-function-info functions
                            :original-name name
                            :arity arity)
      (error (make-condition 'mutate
                             :text "No funs found."
                             :obj obj))))

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

(defmethod symbol-table :before ((obj new-clang))
  (update-caches-if-necessary obj))

(defmethod names-symbol-table :before ((obj new-clang))
  (update-caches-if-necessary obj))

(defmethod (setf ast-root) :after (new (obj new-clang))
  ;; Upon setting the AST root, update the symbol and then update the
  ;; :REFERENCEDDECL field on the ASTs in NEW to point to the entries
  ;; in the symbol table.
  (with-slots (symbol-table) obj
    (setf symbol-table
          (update-symbol-table (clear-symbol-table symbol-table) new))
    (setf new
          (update-referenceddecl-from-symbol-table new symbol-table))))

(defmethod update-caches ((obj new-clang))
  (call-next-method)
  (with-slots (includes macros types symbol-table name-symbol-table) obj
    (setf name-symbol-table
          (update-name-symbol-table name-symbol-table symbol-table))
    (setf includes
          (ast-includes-in-obj obj (ast-root obj)))
    (setf types
          (if (zerop (hash-table-count types))
              (update-type-table types symbol-table (ast-root obj))
              types))
    (setf macros
          (if (zerop (length macros))
              (find-macros-in-children (ast-children (ast-root obj)))
              macros)))
  obj)

(defmethod clear-caches ((obj new-clang))
  (with-slots (includes macros types name-symbol-table) obj
    (setf includes nil)
    (setf macros nil)
    (setf types (make-hash-table))
    (setf name-symbol-table (clear-symbol-table name-symbol-table)))
  (call-next-method))

(defmethod rebind-vars ((ast new-clang-ast)
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
                    (ast-children ast))))
       (if (equal (ast-children ast) new-children)
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
                        (ast-children ast))))
         (if (every #'eql c (ast-children ast))
             ast
             (copy ast :children c))))))

(defmethod begins-scope ((ast new-clang-ast))
  (begins-scope* ast))

(defmethod fixup-mutation (operation (current new-clang-ast)
                           before ast after)
  (clang-fixup-mutation operation current before ast after))

(defmethod find-type ((obj new-clang) (type nct+))
  ;; This looks like a stub, but isn't.
  ;; What's happening here is that while in old clang
  ;; find-type was used to look up types from hashes,
  ;; in the new front end the type objects are there directly.
  ;; The lookup function just returns the object in that case.
  type)

(defmethod add-type ((obj new-clang) (type nct+))
  (add-type* obj type))

(defmethod find-or-add-type
    ((obj new-clang) (trace-name string)
     &rest args &key &allow-other-keys
     &aux (name (apply #'trace-string-to-clang-json-string trace-name args)))
  ;; NAME is a trace name, not a name from clang json
  ;; Trace names have different format, with * and [...] before the type
  ;; name2
  (or (first (remove-if-not {string= name}
                            (hash-table-values (types obj))
                            :key [#'new-clang-type-qual #'nct+-type]))
      (add-type obj (make-instance 'nct+
                      :type (make-instance 'new-clang-type :qual name)))))

(defmethod get-ast-types :around ((software new-clang) (ast new-clang-ast))
  ;; new-clang returns actual types, not hashes.
  ;; Remove those types with the same hashes.
  (remove-duplicates (call-next-method) :key #'type-hash))

(defmethod find-macro ((obj new-clang) (macro clang-macro))
  ;; This looks like a stub, but isn't.
  ;; What's happening here is that while in old clang
  ;; find-macro was used to look up macros from hashes,
  ;; in the new front end the macro objects are there directly.
  ;; The lookup function just returns the object in that case.
  macro)

(defun find-macros-in-children (c)
  ;; Break C up into segments of actual strings
  (mapcan #'find-macros-in-string
          (iter (while c)
                (if (stringp (car c))
                    (collecting (iter (while (stringp (car c)))
                                      (concatenating (pop c))))
                    (pop c)))))

(defun find-macros-in-string (str)
  "Scan a string for things that look like macros.  Compute
macro objects from these, returning a list."
  (let* ((pos 0)
         (slen (length str))
         (slen7 (- slen 7))
         (macros nil))
    ;; At top of loop, we're at the start of a line
    (iter (while (<= pos slen7))
          (if (string= "#define" str :start2 pos :end2 (+ pos 7))
              ;; Scan for macro
              (let ((pos2 (+ pos 7)))
                (iter (when (>= pos2 slen)
                        (push (build-macro-from-string (subseq str pos pos2))
                              macros)
                        (return))
                      (case (elt str pos2)
                        ;; NOTE: Windows
                        (#\Newline
                         ;; End of macro
                         (push (build-macro-from-string (subseq str pos pos2))
                               macros)
                         (incf pos2)
                         (return))
                        (#\\
                         ;; Skip next character -- skips newline
                         (incf pos2 2))
                        (t
                         (incf pos2))))
                (setf pos pos2))
              ;; Skip to end
              (iter (while (< pos slen))
                    (let ((c (elt str pos)))
                      (incf pos)
                      ;; NOTE: Windows
                      (when (eql c #\Newline) (return))))))
    macros))

(defun build-macro-from-string (str)
  (let ((slen (length str)))
    (assert (>= slen 7))
    (assert (string= "#define" str :end2 7))
    (let ((pos 7))
      ;; Skip whitespace
      (iter (while (< pos slen))
            (while (case (elt str pos)
                     (#.+whitespace-chars+ t)
                     (t nil)))
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
          (make-clang-macro :hash hash :body body :name name))))))

(defun update-symbol-table (symbol-table ast-root)
  "Populate SYMBOL-TABLE with a mapping of AST ID -> AST(s) for all of
the decl ASTs in AST-ROOT."
  (labels ((symbol-ast-p (ast)
             "Return TRUE is AST should be included in the symbol table."
             (and ast
                  (ast-is-decl ast)
                  (not (eq :TopLevel (ast-class ast))))))
    (map-ast ast-root
             (lambda (ast &aux (referenced (ast-referenceddecl ast)))
               (when (symbol-ast-p ast)
                 (setf (gethash (ast-id ast) symbol-table)
                       (list ast)))
               (when (and (symbol-ast-p referenced)
                          (null (gethash (ast-id referenced) symbol-table)))
                 (setf (gethash (ast-id referenced) symbol-table)
                       (list referenced)))))
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
                   (remove-if [#'null #'ast-file] asts))
             (when (null (gethash k symbol-table))
               (remhash k symbol-table)))
           symbol-table)
  symbol-table)

(defun update-referenceddecl-from-symbol-table (ast-root symbol-table)
  "Update the :AST-REFERENCEDDECL field on ASTs in AST-ROOT to point to decls
in the SYMBOL-TABLE."
  (map-ast ast-root
           (lambda (ast)
             (when-let* ((old-ref (ast-referenceddecl ast))
                         (new-ref (find old-ref
                                        (gethash (ast-id old-ref) symbol-table)
                                        :key #'ast-name :test #'name=)))
               (setf (ast-attr ast :referenceddecl) new-ref))))
  ast-root)

(defun update-type-table (types symbol-table ast-root)
  "Populate TYPES with a mapping of type-hash -> NCT+ objects using the
ASTs in the existing SYMBOL-TABLE and AST-ROOT tree."
  (labels ((get-nct+-type (ast)
             (when-let* ((tp (ast-type ast))
                         (storage-class (or (ast-attr ast :storage-class)
                                            :none))
                         (tp+ (make-instance 'nct+
                                :type tp
                                :storage-class storage-class)))
               tp+)))
    ;; Populate from the symbol table containing decls in header files
    ;; outside the current file.
    (maphash (lambda (id asts)
               (declare (ignorable id))
               (iter (for ast in asts)
                     (when-let* ((tp+ (get-nct+-type ast))
                                 (_ (null (gethash (type-hash tp+) types))))
                       (setf (gethash (type-hash tp+) types) tp+))))
             symbol-table)
    ;; Populate from the AST ROOT all types in the current file.
    (map-ast ast-root
             (lambda (ast)
               (when-let* ((tp+ (get-nct+-type ast))
                           (_ (null (gethash (type-hash tp+) types))))
                 (setf (gethash (type-hash tp+) types) tp+))))
    types))


;;; Structures and functions relating to genome locations.

(defstruct new-clang-loc
  "Structure used to represent a location within a clang-parseable file."
  (file nil :type (or null string))
  (line nil :type (or null integer))
  (col nil :type (or null integer))
  (offset 0 :type (or null integer))
  (tok-len 0 :type (or null integer)))

(defstruct new-clang-macro-loc
  "Structure used to represent :begin/:end entries for
things in macro expansion.  SPELLING-LOC is the location
in the macro defn, EXPANSION-LOC is at the macro use."
  (spelling-loc nil :type (or null new-clang-loc))
  (expansion-loc nil :type (or null new-clang-loc))
  (is-macro-arg-expansion nil :type boolean))

(defstruct new-clang-range
  "Structure used to represent the begin and end location of an AST."
  (begin nil :type (or null new-clang-loc new-clang-macro-loc))
  (end nil :type (or null new-clang-loc new-clang-macro-loc)))

(defmethod copy ((obj new-clang-loc)
                 &key (file nil file-supplied-p)
                   (line nil line-supplied-p)
                   (col nil col-supplied-p)
                   (offset 0 offset-supplied-p)
                   (tok-len 0 tok-len-supplied-p))
  (make-new-clang-loc
   :file (if file-supplied-p file (copy-seq (new-clang-loc-file obj)))
   :line (if line-supplied-p line (new-clang-loc-line obj))
   :col (if col-supplied-p col (new-clang-loc-col obj))
   :offset (if offset-supplied-p offset (new-clang-loc-offset obj))
   :tok-len (if tok-len-supplied-p tok-len (new-clang-loc-tok-len obj))))

(defmethod copy ((obj new-clang-macro-loc)
                 &key (spelling-loc nil spelling-loc-supplied-p)
                   (expansion-loc nil expansion-loc-supplied-p)
                   (is-macro-arg-expansion nil
                                           is-macro-arg-expansion-supplied-p))
  (make-new-clang-macro-loc
   :spelling-loc (if spelling-loc-supplied-p
                     spelling-loc
                     (copy (new-clang-macro-loc-spelling-loc obj)))
   :expansion-loc (if expansion-loc-supplied-p
                      expansion-loc
                      (copy (new-clang-macro-loc-expansion-loc obj)))
   :is-macro-arg-expansion (if is-macro-arg-expansion-supplied-p
                               is-macro-arg-expansion
                               (new-clang-macro-loc-is-macro-arg-expansion
                                obj))))

(defmethod copy ((obj new-clang-range)
                 &key (begin nil begin-supplied-p)
                   (end nil end-supplied-p))
  (make-new-clang-range
   :begin (if begin-supplied-p begin (copy (new-clang-range-begin obj)))
   :end (if end-supplied-p end (copy (new-clang-range-end obj)))))

(defgeneric offset (obj)
  (:method ((obj new-clang-loc))
    (new-clang-loc-offset obj))
  (:method ((obj new-clang-macro-loc))
    (if (new-clang-macro-loc-is-macro-arg-expansion obj)
        (offset (new-clang-macro-loc-spelling-loc obj))
        (offset (new-clang-macro-loc-expansion-loc obj)))))

(defgeneric (setf offset) (offset obj)
  (:method ((offset integer) (obj new-clang-ast))
    (setf (offset (new-clang-range-begin (ast-attr obj :range))) offset))
  (:method ((offset integer) (obj new-clang-loc))
    (setf (new-clang-loc-offset obj) offset))
  (:method ((offset integer) (obj new-clang-macro-loc))
    (setf (offset (new-clang-macro-loc-expansion-loc obj)) offset)))

(defgeneric begin-offset (obj)
  (:method ((obj new-clang-ast))
    (begin-offset (aget :range (new-clang-ast-attrs obj))))
  (:method ((obj new-clang-range))
    (offset (new-clang-range-begin obj))))

(defgeneric begin-tok-len (obj)
  (:method ((obj new-clang-ast))
    (begin-tok-len (aget :range (new-clang-ast-attrs obj))))
  (:method ((obj new-clang-range))
    (tok-len (new-clang-range-begin obj))))

(defgeneric end-offset (obj)
  (:method ((obj new-clang-ast))
    (end-offset (aget :range (new-clang-ast-attrs obj))))
  (:method ((obj new-clang-range))
    (offset (new-clang-range-end obj))))

(defgeneric tok-len (x)
  (:method ((x new-clang-loc)) (new-clang-loc-tok-len x))
  (:method ((x new-clang-macro-loc))
    #|
    (if (not (new-clang-macro-loc-is-macro-arg-expansion x))
    (tok-len (new-clang-macro-loc-spelling-loc x))
    (tok-len (new-clang-macro-loc-expansion-loc x)))
    |#
    (tok-len (new-clang-macro-loc-expansion-loc x))))

;;; Compute beginning, ending offsets for an ast, other things
;;; The end offset is one past the last character in the ast-text
;;; for the ast
(defgeneric begin-and-end-offsets (x)
  (:method ((obj new-clang-ast))
    (begin-and-end-offsets (ast-attr obj :range)))
  (:method ((obj new-clang-range))
    (values (offset (new-clang-range-begin obj))
            (+ (offset (new-clang-range-end obj))
               (tok-len (new-clang-range-end obj))))))

(defun extended-end-offset (x)
  (nth-value 1 (begin-and-end-offsets x)))


;;; AST creation mechanisms

(defgeneric make-new-clang-ast* (class &rest args &key &allow-other-keys)
  (:documentation "Make a new-clang-ast node or a subclass of new-clang-ast,
depending on CLASS"))

(defmethod make-new-clang-ast* (class &rest args &key &allow-other-keys)
  (apply #'make-new-clang-ast :class class args))

(defmethod to-ast ((ast-type (eql 'clang)) s)
  (declare (ignorable ast-type))
  (to-ast (if *new-clang?* 'new-clang-ast 'clang-ast) s))

(defmethod to-ast ((ast-type (eql 'new-clang-ast)) spec)
  (to-ast* spec
           (lambda (class keys children)
             (apply
              #'make-new-clang-ast*
              class
              :children children
              :allow-other-keys t
              keys))))

(defmethod make-new-clang-ast* ((class (eql :cxxoperatorcallexpr)) &rest args
                                &key &allow-other-keys)
  (apply #'make-cxx-operator-call-expr :class class args))

(defmethod make-new-clang-ast* ((class (eql :macroexpansion)) &rest args
                                &key children false-children &allow-other-keys)
  ;; :FALSE-CHILDREN is the list of ersatz children below the macroexpansion node
  ;; They are not used for computing the source-text of the node
  (assert (listp children))
  (assert (= (length children) 1))
  (apply #'make-new-clang-ast
         :class :macroexpansion
         :children false-children
         :source-text (car children)
         :allow-other-keys t
         args))

(defmethod to-alist ((ast new-clang-ast))
  (flet ((%p (key fn)
           (let ((v (funcall fn ast)))
             (when v
               (list (cons key v)))))
         (%attrs (attrs)
           (append (when (aget :referenceddecl attrs)
                     `((:referenceddecl .
                                        ,(to-alist (aget :referenceddecl attrs)))))
                   (when (aget :macro-child-segment attrs)
                     `((:macro-child-segment .
                                             ,(mapcar #'to-alist
                                                      (aget :macro-child-segment attrs)))))
                   (when (aget :type attrs)
                     `((:type . ,(to-alist (aget :type attrs)))))
                   (when (aget :argtype attrs)
                     `((:argtype . ,(to-alist (aget :argtype attrs)))))
                   (adrop (list :type :argtype
                                :referenceddecl :macro-child-segment)
                          attrs))))
    (append (%p ':class #'new-clang-ast-class)
            (%p ':id #'new-clang-ast-id)
            (%p ':syn-ctx #'new-clang-ast-syn-ctx)
            (%p ':aux-data #'new-clang-ast-aux-data)
            ;; Always include :attrs, as it distinguishes
            ;; new-clang from (old) clang serialized asts
            (%p ':attrs [#'%attrs #'new-clang-ast-attrs]))))

(defmethod from-alist ((obj (eql 'new-clang-ast)) alist)
  (flet ((%attrs (attrs)
           (append (when (aget :referenceddecl attrs)
                     `((:referenceddecl .
                                        ,(from-alist 'new-clang-ast
                                                     (aget :referenceddecl attrs)))))
                   (when (aget :macro-child-segment attrs)
                     `((:macro-child-segment .
                                             ,(mapcar {from-alist 'new-clang-ast}
                                                      (aget :macro-child-segment attrs)))))
                   (when (aget :type attrs)
                     `((:type . ,(from-alist 'new-clang-type
                                             (aget :type attrs)))))
                   (when (aget :argtype attrs)
                     `((:argtype . ,(from-alist 'new-clang-type
                                                (aget :argtype attrs)))))
                   (adrop (list :type :argtype
                                :referenceddecl :macro-child-segment)
                          attrs))))
    (make-new-clang-ast :class (aget :class alist)
                        :id (aget :id alist)
                        :syn-ctx (aget :syn-ctx alist)
                        :aux-data (aget :aux-data alist)
                        :attrs (%attrs (aget :attrs alist)))))

(defun new-clang-ast-copy (ast fn &rest args
                           &key
                             referenceddecl
                             path
                             (children (new-clang-ast-children ast))
                             (class (new-clang-ast-class ast))
                             (attrs (new-clang-ast-attrs ast) attrs-p)
                             (id (new-clang-ast-id ast))
                             (syn-ctx (new-clang-ast-syn-ctx ast))
                             (aux-data (new-clang-ast-aux-data ast))
                             &allow-other-keys)
  ;; The value of REFERENCEDDECL is not otherwise explicitly
  ;; used in this function, but it gets used as part of ARGS
  (unless (or (null referenceddecl) (ast-p referenceddecl))
    (error "Referenceddecl not an AST: ~s~%" referenceddecl))
  (let (new-attrs)
    (let ((args2 args))
      (iter (while args2)
            (let ((key (pop args2))
                  (arg (pop args2)))
              (case key
                ((:path :children :class :id :syn-ctx :aux-data) nil)
                ((:attrs)
                 (unless attrs-p
                   (setf attrs-p t
                         attrs arg)))
                (t
                 ;; Otherwise, it's an attribute
                 (push (cons key arg) new-attrs))))))
    (iter (for (key . arg) in new-attrs)
          (setf attrs (areplace key arg attrs)))

    ;; This call includes :ALLOW-OTHER-KEYS T because
    ;; FN may be #'make-new-clang-ast, and we cannot
    ;; add &allow-other-keys to that.
    (funcall fn :allow-other-keys t
             :path path :children children
             :class class :attrs attrs :id id
             :syn-ctx syn-ctx :aux-data aux-data)))

(defmethod copy ((ast new-clang-ast) &rest args) ;  &key &allow-other-keys)
  (apply #'new-clang-ast-copy ast #'make-new-clang-ast args))

(defmethod copy ((ast cxx-operator-call-expr) &rest args) ;  &key &allow-other-keys)
  (apply #'new-clang-ast-copy ast #'make-cxx-operator-call-expr args))

(defun make-statement (&rest args)
  (apply (if *new-clang?*
             #'make-statement-new-clang
             #'sel/sw/clang::make-statement*)
         args))

(defun make-statement-new-clang (class syn-ctx children
                                 &key full-stmt guard-stmt opcode declares
                                   aux-data
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
  (let ((attrs nil))
    (macrolet ((%push (k v)
                 `(when ,v (push (cons ,k ,v) attrs))))
      (%push :full-stmt full-stmt)
      (%push :guard-stmt guard-stmt)
      (%push :opcode opcode)
      (%push :name (when (= (length declares) 1)
                     ;; new-clang name attribute is not aggregated
                     (car declares))))
    (make-new-clang-ast
     :path nil
     :children children
     :syn-ctx syn-ctx
     :class class
     :attrs attrs
     :children children
     :aux-data aux-data)))


;;; AST fields
;;;
;;; TODO: identify which of these generic accessors should be read only,
;;;  beyond those that refer to read-only fields in new-clang-ast.
;;;  For those, remove the SETF method, or at least make it error
;;;  when called.

;;; There are no separate 'node' objects for new-clang
(defmethod ast-node ((obj new-clang-ast)) obj)

(defmethod ast-id ((obj new-clang-ast))
  (new-clang-ast-id obj))
(defmethod ast-path ((obj new-clang-ast))
  (new-clang-ast-path obj))
(defmethod (setf ast-path) (value (obj new-clang-ast))
  (setf (new-clang-ast-path obj) value))
(defmethod ast-children ((obj new-clang-ast))
  (new-clang-ast-children obj))
(defmethod (setf ast-children) (value (obj new-clang-ast))
  (setf (new-clang-ast-children obj) value))
(defmethod ast-stored-hash ((obj new-clang-ast))
  (new-clang-ast-stored-hash obj))
(defmethod (setf ast-stored-hash) (value (obj new-clang-ast))
  (setf (new-clang-ast-stored-hash obj) value))
(defmethod ast-aux-data ((obj new-clang-ast))
  (new-clang-ast-aux-data obj))
(defmethod (setf ast-aux-data) (v (obj new-clang-ast))
  (setf (new-clang-ast-aux-data obj) v))
(defgeneric ast-attr (ast attr))
(defmethod ast-attr ((ast new-clang-ast) (attr symbol))
  (let ((attrs (new-clang-ast-attrs ast)))
    (aget attr attrs)))
(defgeneric (setf ast-attr) (v ast attr))
(defmethod (setf ast-attr) (v (ast new-clang-ast) (attr symbol))
  (let* ((attrs (new-clang-ast-attrs ast))
         (p (assoc attr attrs)))
    (if p (setf (cdr p) v)
        (setf (new-clang-ast-attrs ast)
              (cons (cons attr v) attrs)))
    v))

(defmethod ast-name ((s string)) s)
(defmethod ast-name ((obj new-clang-ast)) (ast-attr obj :name))

(defmethod ast-class ((obj new-clang-ast))
  (new-clang-ast-class obj))

(defmethod ast-syn-ctx ((obj new-clang-ast))
  (new-clang-ast-syn-ctx obj))

(defmethod ast-in-macro-expansion ((obj new-clang-ast))
  (eql (ast-class obj) :macroexpansion))

(defun ast-range (ast) (ast-attr ast :range))
(defun (setf ast-range) (val ast) (setf (ast-attr ast :range) val))

(defgeneric ast-is-implicit (ast)
  (:method (ast) (declare (ignorable ast)) nil)
  (:method ((ast new-clang-ast))
    (or (ast-attr ast :isimplicit)
        (ast-attr ast :implicit))))

(defgeneric ast-is-class (ast key)
  (:method (ast class) (declare (ignorable ast class)) nil)
  (:method ((ast new-clang-ast) (key symbol))
    (and (ast-p ast)
         (eql (ast-class ast) key))))

(defmethod ast-type ((ast new-clang-ast))
  (ast-attr ast :type))

(defmethod ast-unbound-vals ((ast new-clang-ast))
  (ast-unbound-vals* ast (ast-class ast)))

(defmethod ast-unbound-vals ((str string))
  (declare (ignore str))
  nil)

(defgeneric ast-unbound-vals* (ast class)
  (:documentation "Implementation function for ast-unbound-vals,
where class = (ast-class ast)."))

(defmethod ast-unbound-vals* ((ast new-clang-ast) (class (eql :declrefexpr)))
  (when-let ((obj (ast-referenceddecl ast)))
    (when (member (ast-class obj) '(:Var :ParmVar))
      (list obj))))

(defmethod ast-unbound-vals* ((ast new-clang-ast) (class (eql :macroexpansion)))
  (let ((children (ast-attr ast :macro-child-segment))
        (bound nil)
        (unbound nil))
    (dolist (c children)
      (map-ast c (lambda (a)
                   (setf bound (append (ast-declarations a) bound))
                   (setf unbound (append (ast-unbound-vals a) unbound)))))
    (set-difference (remove-duplicates unbound)
                    (remove-duplicates bound))))

(defmethod ast-unbound-vals* ((ast new-clang-ast) class)
  (declare (ignorable ast class))
  nil)

(defgeneric ast-bound-vals (ast)
  (:documentation "Vars that are bound by an AST")
  (:method ((x string)) (declare (ignore x)) nil)
  (:method ((ast new-clang-ast))
    (ast-bound-vals* ast (ast-class ast))))

(defgeneric ast-bound-vals* (ast class)
  (:documentation "Implementation funtion for ast-bound-vals,
where class = (ast-class ast).")
  (:method ((ast new-clang-ast) c)
    ;; default method
    (declare (ignorable c))
    nil)
  (:method ((ast new-clang-ast) (c (eql :var)))
    (list ast))
  (:method ((ast new-clang-ast) (c (eql :declstmt)))
    (remove-if-not (lambda (a) (and (ast-p a) (eql (ast-class a) :var)))
                   (ast-children ast))))

(defmethod ast-unbound-funs ((ast new-clang-ast))
  (ast-unbound-funs* ast (ast-class ast)))

(defmethod ast-unbound-funs ((str string))
  (declare (ignore str))
  nil)

(defgeneric ast-unbound-funs* (ast class)
  (:documentation "Implementation funtion for ast-unbound-funs,
where class = (ast-class ast).")
  (:method (ast class) (declare (ignorable ast class)) nil)
  (:method ((ast new-clang-ast) class) (declare (ignorable ast class)) nil)
  (:method ((ast new-clang-ast) (class (eql :declrefexpr)))
    (declare (ignorable class))
    (when-let* ((obj (ast-referenceddecl ast)))
      (when (eql (ast-class obj) :function)
        (list (list obj (ast-void-ret obj) (ast-varargs obj)
                    (count-if (lambda (a) (and (ast-p a) (eql (ast-class a) :ParmVar)))
                              (ast-children obj))))))))

(defmethod ast-includes-in-obj ((obj new-clang) (ast new-clang-ast)
                                &aux (includes nil))
  ;;;  This code is now assuming AST-FILE is not present (or NIL) if
  ;;;  the file was the same as the one this AST is in.
  ;;;
  ;;;  This is also needed to make the i-file slot of nct+ work.
  (labels ((ast-includes-in-child (child)
             (nest (mapcar (lambda (ast)
                             (ast-file-for-include obj ast)))
                   (remove-if-not (lambda (ast)
                                    (and ast
                                         (or (ast-file ast nil)
                                             (ast-file ast t)))))
                   (list child
                         (ast-referenceddecl child)
                         (when (ast-type child)
                           (type-decl-ast obj (ast-type child)))))))
    (map-ast ast
             (lambda (c)
               (dolist (f (ast-includes-in-child c))
                 (pushnew f includes :test #'equal))))
    (nreverse includes)))

(defmethod ast-macros ((ast new-clang-ast))
  (ast-macros* ast (ast-class ast)))

(defmethod ast-macros ((ast string)) (declare (ignorable ast)) nil)

(defmethod ast-macros* ((ast new-clang-ast) class)
  (declare (ignorable class))
  (remove-duplicates (apply #'append
                            (when (ast-attr ast :macro)
                              (list (ast-attr ast :macro)))
                            (mapcar #'ast-macros (ast-children ast)))))

(defmethod ast-macros* ((ast new-clang-ast) (class (eql :toplevel)))
  (declare (ignorable ast class))
  nil)

(defmethod ast-macros* ((ast new-clang-ast) (class (eql :macroexpansion)))
  (declare (ignorable class))
  (remove-duplicates (apply #'append
                            (when (ast-attr ast :macro)
                              (list (ast-attr ast :macro)))
                            (mapcar #'ast-macros
                                    (ast-attr ast :macro-child-segment)))))

(defmethod ast-types ((ast string)) nil)
(defmethod ast-types ((ast new-clang-ast))
  (ast-types* ast (ast-class ast)))
(defun ast-types*-on-decl (ast)
  (when-let ((tp (ast-attr ast :type))
             (storage-class (or (ast-attr ast :storageclass) :none)))
    (list (make-instance 'nct+ :type tp :storage-class storage-class))))

(defgeneric ast-types* (ast class)
  (:documentation "Dispatch function for computing AST-TYPES
on various ast classes"))

(defmethod ast-types* ((ast new-clang-ast) (ast-class (eql :ParmVar)))
  (ast-types*-on-decl ast))

(defmethod ast-types* ((ast new-clang-ast) (ast-class (eql :Var)))
  ;; For :Var nodes, we must also include the types in the
  ;; initializer, if present
  (let ((nodes (remove ast (ast-nodes-in-subtree ast))))
    (sort
     (reduce (lambda (a b) (union a b :key #'type-hash))
             (mapcar #'ast-types nodes)
             :initial-value (ast-types*-on-decl ast))
     #'string<
     :key #'type-name)))

(defmethod ast-types* ((ast new-clang-ast) (ast-class (eql :Macroexpansion)))
  (let ((nodes (remove ast (ast-nodes-in-subtree ast)))
        (macro-child-segment (mapcan #'ast-nodes-in-subtree
                                     (ast-attr ast :macro-child-segment))))
    (sort
     (reduce (lambda (a b) (union a b :key #'type-hash))
             (mapcar #'ast-types (append nodes macro-child-segment))
             :initial-value nil)
     #'string<
     :key #'type-name)))

(defmethod ast-types* ((ast new-clang-ast) (ast-class (eql :UnaryExprOrTypeTraitExpr)))
  (let ((argtype (ast-attr ast :argtype))
        (types (ast-types*-on-decl ast)))
    (if argtype
        (adjoin (make-instance 'nct+ :type argtype) types :key #'type-hash)
        types)))

(defmethod ast-types* ((ast new-clang-ast) (ast-class (eql :Typedef)))
  (ast-types*-on-decl ast))

(defmethod ast-types* ((ast new-clang-ast) (ast-class symbol))
  (case ast-class
    ((:CstyleCastExpr
      :CXXFunctionalCastExpr
      :CXXReinterpretCastExpr)
     (ast-types*-on-decl ast))
    (t nil)))

(defmethod ast-declarations ((ast new-clang-ast))
  (ast-declarations* ast))

(defmethod ast-var-declarations ((ast new-clang-ast))
  (ast-var-declarations* ast))

;;; AST-FILE was used for two things: to identify non-source
;;; code that clang stuck in, and to identify macros
;;; These two functions needed to be separated.
(defun ast-file (ast &optional macro?)
  (ast-file* ast macro?))

(defgeneric ast-file* (ast macro?)
  (:documentation "The file name associated with an AST node")
  (:method ((ast new-clang-ast) macro?)
    (ast-file* (ast-attr ast :range) macro?))
  (:method ((range new-clang-range) macro?)
    (ast-file* (new-clang-range-begin range) macro?))
  (:method ((loc new-clang-loc) macro?)
    (declare (ignore macro?))
    (new-clang-loc-file loc))
  (:method ((loc new-clang-macro-loc) macro?)
    (ast-file*
     (if (if (new-clang-macro-loc-is-macro-arg-expansion loc)
             (not macro?)
             macro?)
         (new-clang-macro-loc-spelling-loc loc)
         (new-clang-macro-loc-expansion-loc loc))
     macro?))
  (:method (obj macro?) (declare (ignorable obj macro?)) nil))

;; returns ast nodes, not strings
(defmethod ast-args ((obj new-clang-ast))
  (mapcar
   (lambda (o) (list o (ast-type o)))
   (remove-if-not (lambda (c) (ast-is-class c :ParmVar))
                  (ast-children obj))))

(defmethod ast-declares ((obj new-clang-ast))
  (case (ast-class obj)
    (:DeclStmt
     (reduce #'append (ast-children obj)
             :key #'ast-declares :initial-value nil))
    ((:ParmVar :Function :Var :Field :Record :TypeDef)
     (when (ast-name obj)
       (list obj)))
    ((:Combined)
     (reduce #'append (ast-children obj)
             :key #'ast-declares :initial-value nil))
    ;; More cases here
    (t nil)))

(defmethod ast-expr-type ((obj new-clang-ast))
  (ast-attr obj :type))

;; This field should be filled in by a pass
;; that marks AST nodes that are full statements
;; (and that might not otherwise be)
(defmethod ast-full-stmt ((obj new-clang-ast))
  (ast-attr obj :full-stmt))

(defmethod full-stmt-p ((obj new-clang) (ast new-clang-ast))
  (declare (ignorable obj))
  (ast-full-stmt ast))

;; This field should be filled in by a pass
;; that marks AST nodes that are guard statements
;; (and that might not otherwise be)
(defmethod ast-guard-stmt ((obj new-clang-ast))
  (ast-attr obj :guard-stmt))

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

(defmethod ast-is-decl ((obj new-clang-ast))
  (case (ast-class obj)
    (#.*clang-decl-kinds* t)
    (t nil)))

(defmethod ast-opcode ((obj new-clang-ast))
  (ast-attr obj :opcode))

(defmethod ast-ret ((obj new-clang-ast))
  (case (ast-class obj)
    (:Function
     (let ((type (new-clang-type-qual (ast-type obj))))
       (ret-type-of-function-type type)))
    ;; Others?
    (t nil)))

(defmethod ast-void-ret ((obj new-clang-ast))
  (equal "void" (new-clang-type-qual (nct+-type (ast-ret obj)))))

(defun ast-reference-decls (ast)
  (map-ast-sets ast #'reference-decls-at-ast :key #'new-clang-ast-id))

(defmethod ast-varargs ((obj new-clang-ast))
  ;; Should just be :FunctionDecl objects
  (ast-attr obj :variadic))

(defgeneric ast-referenceddecl (ast)
  (:documentation "The declaration referenced by AST.")
  (:method ((ast new-clang-ast))
    (ast-attr ast :referenceddecl)))

;; Helpers for the "ast-*" functions above
(defun reference-decls-at-ast (a)
  (let ((rd (ast-attr a :referencedDecl)))
    (when rd (list rd))))

(defun ret-type-of-function-type (s)
  "Returns a string that is the return type of the function type
given in the string S. Return nil if the return type cannot be
determined."
  ;; This is grossly incomplete now, and will fail on some
  ;; hairy types
  (let ((pos (position #\( s)))
    (when (and pos (> pos 0))
      (make-instance 'nct+
        :type (make-instance 'new-clang-type
                :qual (trim-right-whitespace (subseq s 0 (1- pos))))))))

;;; Question on this: are IDs unique between files?
;;; Or do we need keys that include file names?

;;; NOTE: will need to make sure this works on Windows also
;;;  Perhaps it should work on pathnames, not namestrings?

;;; NOTE: this assumes "..." does NOT search the include path
;;;  In Clang, this behavior is controlled by command line options,
;;;  which we'll need to recognize.

(defun normalize-file-for-include (file-string include-dirs)
  "Returns the normalized version of file-string relative to the include-dirs,
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
     (let ((file-len (length file-string))
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

(defun ast-file-for-include (obj ast)
  "Return the file AST is located within in a format suitable for use
in a #include."
  (normalize-file-for-include (or (ast-file ast nil) (ast-file ast t))
                              (append (flags-to-include-dirs (flags obj))
                                      *clang-default-includes*)))

(defmethod source-text ((ast new-clang-ast))
  (with-output-to-string (out)
    (if (eq :Combined (ast-class ast))
        ;; Combined nodes are a special case as the
        ;; child node source text ranges overlap
        (write-string (ast-attr ast :source-text) out)
        ;; For normal ASTs, just collect the child node source
        (mapc [{write-string _ out} #'source-text]
              (ast-children ast)))))

;;; TODO: This function should be removed when we unify
;;;       new-clang-range with sel/utility:range.
(defun within-ast-range (range line)
  "Test whether the supplied line is within a range."
  (and (>= line (new-clang-loc-line (new-clang-range-begin range)))
       (<= line (new-clang-loc-line (new-clang-range-end range)))))

;;; TODO: This function should be removed when we unify
;;;       new-clang-range with sel/utility:range.
(defun ast-range-str (range)
  "Return a short string-rep for the supplied range."
  (format nil "[~a, ~a]" (new-clang-loc-line (new-clang-range-begin range))
          (new-clang-loc-line (new-clang-range-end range))))


;;; Reimplementations of ast-* functions for nodes

(defgeneric map-ast-while (a fn)
  (:documentation "Apply FN to the nodes of AST A, stopping
the descent when FN returns NIL"))

(defmethod map-ast-while ((a ast) fn)
  (when (funcall fn a)
    (dolist (c (ast-children a))
      (when (ast-p c) (map-ast-while c fn)))))

(defgeneric map-ast-sets (ast fn &key key test)
  (:documentation
   "Evaluates FN at the nodes of AST, returning a list of
objects.  Returns the union of this list and the value
computed at the children"))

(defmethod map-ast-sets (ast fn &key (key 'identity) (test 'eql))
  (labels ((%recurse (a)
             ;; This is slow
             ;; In the future, memoize and use better data structures
             (let ((here (funcall fn a))
                   (child-sets
                    (iter (for c in (ast-children a))
                          (when (ast-p c)
                            (collect (%recurse c))))))
               (reduce (lambda (s1 s2)
                         (union s1 s2 :key key :test test))
                       child-sets :initial-value here))))
    (%recurse ast)))

(defgeneric remove-asts-if (ast fn)
  (:documentation "Remove all subasts for which FN is true"))

(defmethod remove-asts-if ((ast new-clang-ast) fn)
  (let* ((children (ast-children ast))
         (new-children (mapcar (lambda (a) (remove-asts-if a fn))
                               (remove-if fn children))))
    (unless (and (= (length children)
                    (length new-children))
                 (every #'eql children new-children))
      (setf (ast-children ast) new-children)))
  ast)

(defmethod remove-asts-if (ast fn) (declare (ignorable fn)) ast)


;;; Type-related functions
;;;
;;; NOTE: NEW-CLANG-TYPE is not a drop-in replacement for
;;;  SEL/SW/CLANG:CLANG-TYPE.  The latter contains additional
;;;  information that is not properly part of a type at all.
;;;

(defmethod to-alist ((new-clang-type new-clang-type))
  (flet ((%p (key fn)
           (when-let ((v (funcall fn new-clang-type)))
             (list (cons key v)))))
    (append (%p ':qual #'new-clang-type-qual)
            (%p ':desugared #'new-clang-type-desugared)
            (%p ':modifiers #'new-clang-type-modifiers)
            (%p ':array #'type-array)
            (%p ':i-file #'type-i-file)
            (%p ':name #'type-name))))

(defmethod to-alist ((nct nct+))
  (flet ((%p (key fn)
           (when-let ((v (funcall fn nct)))
             (list (cons key v)))))
    (append (%p ':type [#'to-alist #'nct+-type])
            (%p ':storage-class #'type-storage-class))))

(defmethod from-alist ((obj (eql 'new-clang-type)) alist)
  (make-instance 'new-clang-type
    :qual (aget :qual alist)
    :desugared (aget :desugared alist)
    :modifiers (aget :modifiers alist)
    :array (aget :array alist)
    :i-file (aget :i-file alist)
    :name (aget :name alist)))

(defmethod from-alist ((nct+ (eql 'nct+)) alist)
  (make-instance 'nct+
    :type (from-alist 'new-clang-type (aget :type alist))
    :storage-class (aget :storage-class alist)))

(defmethod copy ((tp new-clang-type)
                 &key (qual nil qual-supplied-p)
                   (desugared nil desugared-supplied-p)
                   (modifiers nil modifiers-supplied-p)
                   (array nil array-supplied-p)
                   (name nil name-supplied-p)
                   (i-file nil i-file-supplied-p)
                   (reqs nil reqs-supplied-p))
  (make-instance 'new-clang-type
    :qual (if qual-supplied-p qual (new-clang-type-qual tp))
    :desugared (if desugared-supplied-p desugared (new-clang-type-desugared tp))
    :modifiers (if modifiers-supplied-p modifiers (new-clang-type-modifiers tp))
    :array (if array-supplied-p array (type-array tp))
    :name (if name-supplied-p name (type-name tp))
    :i-file (if i-file-supplied-p i-file (type-i-file tp))
    :reqs (if reqs-supplied-p reqs (type-reqs tp))))

(defmethod copy ((tp+ nct+)
                 &key (type nil type-supplied-p)
                   (storage-class nil storage-class-supplied-p))
  (make-instance 'nct+
    :type (if type-supplied-p type (nct+-type tp+))
    :storage-class (if storage-class-supplied-p
                       storage-class
                       (type-storage-class tp+))))

(defmethod typedef-type ((obj new-clang) (nct nct+)
                         &aux (mods (new-clang-type-modifiers (nct+-type nct)))
                           (array (type-array (nct+-type nct))))
  (labels ((typedef-type-helper (nct)
             (if-let ((typedef-nct
                       (some-<>> (type-decl-ast obj nct)
                                 (ast-type)
                                 (make-instance 'nct+ :type))))
               (typedef-type-helper typedef-nct)
               (copy nct
                     :type (copy (nct+-type nct)
                                 :modifiers (logior mods
                                                    (new-clang-type-modifiers
                                                     (nct+-type nct)))
                                 :array (concatenate 'string
                                                     (type-array (nct+-type nct))
                                                     array))))))
    (typedef-type-helper nct)))

(defmethod type-i-file ((tp+ nct+))
  (type-i-file (nct+-type tp+)))
(defmethod type-i-file ((tp new-clang-type))
  (new-clang-type-i-file tp))

(defmethod type-hash ((tp+ nct+))
  (sxhash (concatenate 'string
                       (new-clang-type-qual (nct+-type tp+))
                       (or (new-clang-type-desugared (nct+-type tp+)) "")
                       (symbol-name (type-storage-class tp+)))))
(defmethod type-hash ((tp new-clang-type))
  (sxhash (concatenate 'string
                       (new-clang-type-qual tp)
                       (or (new-clang-type-desugared tp) ""))))

;;; Pointer, const, volatile, and restrict are indicated by integers
;;;  in the modifiers slot.

(defconstant +pointer+ 1)
(defconstant +const+ 2)
(defconstant +volatile+ 4)
(defconstant +restrict+ 8)

(defmethod type-name ((tp+ nct+))
  (type-name (nct+-type tp+)))

(defmethod type-array ((tp+ nct+))
  (type-array (nct+-type tp+)))

(defmethod type-pointer ((tp+ nct+))
  (type-pointer (nct+-type tp+)))
(defmethod type-pointer ((tp new-clang-type))
  (if (logtest +pointer+ (new-clang-type-modifiers tp)) t nil))

(defmethod type-const ((tp+ nct+))
  (type-const (nct+-type tp+)))
(defmethod type-const ((tp new-clang-type))
  (if (logtest +const+ (new-clang-type-modifiers tp)) t nil))

(defmethod type-volatile ((tp+ nct+))
  (type-volatile (nct+-type tp+)))
(defmethod type-volatile ((tp new-clang-type))
  (if (logtest +volatile+ (new-clang-type-modifiers tp)) t nil))

(defmethod type-restrict ((tp+ nct+))
  (type-restrict (nct+-type tp+)))
(defmethod type-restrict ((tp new-clang-type))
  (if (logtest +restrict+ (new-clang-type-modifiers tp)) t nil))

(defmethod type-reqs ((tp+ nct+))
  (mapcar {make-instance 'nct+ :type}
          (remove-duplicates
           (type-reqs (nct+-type tp+)))))
(defmethod slot-unbound (class (obj new-clang-type) (slot (eql 'reqs)))
  ;; Find all the types required by this type
  ;; Currently, this is a stub
  ;; The proper implemenetation must walk over the type
  (declare (ignorable class))
  (setf (slot-value obj slot) nil)) ;; stub

(defmethod slot-unbound (class (obj new-clang-type) (slot (eql 'array)))
  (declare (ignorable class))
  (compute-new-clang-type-slots obj)
  (slot-value obj slot))
(defmethod slot-unbound (class (obj new-clang-type) (slot (eql 'name)))
  (declare (ignorable class))
  (compute-new-clang-type-slots obj)
  (slot-value obj slot))
(defmethod slot-unbound (class (obj new-clang-type) (slot (eql 'modifiers)))
  (declare (ignorable class))
  (compute-new-clang-type-slots obj)
  (slot-value obj slot))

(defgeneric compute-new-clang-type-slots (tp)
  (:method ((tp new-clang-type))
    ;; Fill in various slots in new-clang-type object
    (multiple-value-bind (pointer const volatile restrict n a)
        (compute-type-properties (new-clang-type-qual tp))
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
  (let (const volatile restrict
              (pos 0)
              (strlen (length str)))
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
           ((case (elt str pos)
              (#.+whitespace-chars+ t)
              (t nil))
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
           ((case (elt str (1- pos))
              (#.+whitespace-chars+ t)
              (t nil))
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
  (:method ((obj new-clang) (tp+ nct+))
    (type-decl-ast obj (nct+-type tp+)))
  (:method ((obj new-clang) (tp new-clang-type))
    (when-let* ((qual (new-clang-type-qual tp))
                (ast-classes (cond ((or (starts-with-subseq "struct " qual)
                                        (starts-with-subseq "class " qual))
                                    (list :CXXRecord :Record))
                                   ((starts-with-subseq "union " qual)
                                    (list :Union))
                                   (t (list :Typedef)))))
      (car (remove-if-not [{member _ ast-classes} #'ast-class]
                          (gethash (type-name tp)
                                   (name-symbol-table obj)))))))

(defmethod type-decl* ((obj new-clang) (tp+ nct+))
  (type-decl* obj (nct+-type tp+)))
(defmethod type-decl* ((obj new-clang) (tp new-clang-type))
  (source-text (type-decl-ast obj tp)))

(defmethod type-decl-string ((obj new-clang-type) &key &allow-other-keys)
  (new-clang-type-qual obj))

(defmethod type-decl-string ((obj nct+) &key &allow-other-keys)
  (type-decl-string (nct+-type obj)))

(defmethod type-trace-string ((type nct+) &key qualified)
  (type-trace-string* type qualified))

(defun trace-string-to-clang-json-string
    (trace-string &key storage-class const pointer volatile restrict name array
                    &allow-other-keys)
  (let ((alist (type-from-trace-string*
                (lambda (&rest args) args) trace-string)))
    (string-right-trim
     " "
     (format
      nil
      "~@[~(~a~) ~]~:[~;const ~]~:[~;volatile ~]~:[~;restrict ~]~a ~:[~;*~]~@[~a~]"
      (let ((sc (or storage-class (getf alist :storage-class))))
        (if (eql sc :none) nil sc))
      (or const (getf alist :const))
      (or volatile (getf alist :volatile))
      (or restrict (getf alist :restrict))
      (or name (getf alist :name))
      (or pointer (getf alist :pointer))
      (or array (getf alist :array))))))


;;; Invocation of clang to get json

(defmethod clang-json ((obj new-clang) &key &allow-other-keys)
  (with-temp-file-of (src-file (ext obj)) (genome obj)
                     (let ((cmd-fmt "clang -cc1 -fgnuc-version=4.2.1 -ast-dump=json ~{~a~^ ~} ~a ~a")
                           (filter "| sed -e \"s/  *//\" ; exit ${PIPESTATUS[0]}")
                           (genome-len (length (genome obj)))
                           (flags (append (clang-frontend-flags (flags obj))
                                          (mappend {list "-I"} *clang-default-includes*))))
                       (multiple-value-bind (stdout stderr exit)
                           (let ((*trace-output* *standard-output*))
                             (if (boundp '*new-clang-json-file*)
                                 (shell "cat ~a ~a;"
                                        (namestring *new-clang-json-file*)
                                        filter)
                                 (shell cmd-fmt
                                        flags
                                        src-file
                                        filter
                                        :bash t)))
                         (when (find exit '(131 132 134 136 139))
                           (error
                            (make-condition 'located-mutate
                                            :text (format nil "clang core dump with ~d, ~s"
                                                          exit stderr)
                                            :obj obj)))
                         (restart-case
                             (unless (zerop exit)
                               (error
                                (make-condition 'located-mutate
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
                         (values (convert-jsown-tree (jsown:parse stdout))
                                 src-file
                                 genome-len)))))

(defun convert-jsown-tree (jt)
  "Converts the tree representation from JSOWN into something similar to
output from CL-JSON"
  (typecase jt
    ((cons (eql :obj) t)
     (convert-jsown-obj (cdr jt)))
    (cons
     (cons (convert-jsown-tree (car jt))
           (convert-jsown-tree (cdr jt))))
    (t jt)))

(defun convert-jsown-obj (key-alist)
  (iter (for (key . val) in key-alist)
        (collect (cons (jsown-str-to-keyword key)
                       (convert-jsown-tree val)))))

;;; The STRING-CASE macro is much faster than just calling INTERN
;;; on the string, when one of these common arguments is seen.
(defun jsown-str-to-keyword (str)
  (macrolet ((%m (s)
               (let ((names '("id" "tokLen" "col" "kind" "qualType"
                              "type" "file" "range" "end" "begin"
                              "includedFrom" "line" "valueCategory"
                              "inner" "name" "loc" "castKind"
                              "referencedDecl" "spellingLoc" "offset"
                              "expansionLoc" "desugaredQualType")))
                 `(string-case (,s)
                               ,@(iter (for n in names)
                                       (collect (list n (intern (string-upcase n)
                                                                :keyword))))
                               (t (intern (string-upcase ,s) :keyword))))))
    ;; Allow common cases to be optimized for particular
    ;; string types
    (typecase str
      (simple-base-string (%m str))
      #-ccl
      ((and simple-string
            (vector character))
       (%m str))
      (t (intern (string-upcase str) :keyword)))))


;;; Json conversion

(defun clang-convert-json-for-file (json file genome-len)
  ;; The aget cache is used to record values of elided
  ;; json attributes, that are assumed to be equal to the previous
  ;; occurrence of such an attribute.  This res the json
  ;; be converted left to right.  cl-json produces alists
  ;; in the same order they appear in the json, fortunately.
  (let* ((*aget-cache* nil)
         (ast (clang-convert-json json)))
    (setf (ast-attr ast :range)
          (make-new-clang-range
           :begin (make-new-clang-loc
                   :file file
                   :offset 0)
           :end (make-new-clang-loc
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

(defmethod j2ck (json json-kind-symbol)
  (declare (ignorable json-kind-symbol))
  (let ((obj (make-new-clang-ast)))
    (store-slots obj json)))

(defmethod j2ck :around (json (json-kind-symbol (eql :forstmt)))
  ;; Clang's json has {} for missing for clauses
  ;; cl-json converts these to NIL.  Just remove then,
  ;; as the old clang front end does.
  (declare (ignorable json json-kind-symbol))
  (let ((obj (call-next-method)))
    (setf (ast-children obj) (remove nil (ast-children obj)))
    obj))

(defmethod j2ck :around (json (json-kind-symbol (eql :ImplicitListExpr)))
  ;; We remove :ImplicitValueInitExprs, turning them to NIL.
  ;; Here, remove those NILs.
  (declare (ignorable json json-kind-symbol))
  (let ((obj (call-next-method)))
    (setf (ast-children obj) (remove nil (ast-children obj)))
    obj))

(defmethod j2ck :around (json (json-kind-symbol (eql :typedef)))
  (declare (ignorable json json-kind-symbol))
  (let ((obj (call-next-method)))
    (when-let ((typedef-type (pop (ast-children obj))))
      (setf (ast-attr obj :typedef-type) typedef-type))
    obj))

(defmethod j2ck (json (json-kind-symbol (eql :ImplicitValueInitExpr)))
  (declare (ignorable json json-kind-symbol))
  nil)

(defmethod j2ck (json (json-kind-symbol (eql :TextComment)))
  (declare (ignorable json json-kind-symbol))
  nil)

(defmethod j2ck (json (json-kind-symbol (eql :ParagraphComment)))
  (declare (ignorable json json-kind-symbol))
  nil)

(defmethod j2ck (json (json-kind-symbol (eql :FullComment)))
  (declare (ignorable json json-kind-symbol))
  nil)

(defmethod j2ck (json (json-kind-symbol (eql :InlineCommandComment)))
  (declare (ignorable json json-kind-symbol))
  nil)

(defmethod j2ck (json (json-kind-symbol (eql :BlockCommandComment)))
  (declare (ignorable json json-kind-symbol))
  nil)

(defmethod j2ck (json (json-kind-symbol (eql :ParamCommandComment)))
  (declare (ignorable json json-kind-symbol))
  nil)

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

(defmethod j2ck (json (json-kind-symbol (eql :CXXOperatorCallExpr)))
  ;; CXXOperatorCallExprs must be a special subclass, as the children
  ;; are out of order (the operator is put first even if it is not
  ;; first in the source file)
  (declare (ignorable json-kind-symbol))
  (store-slots (make-cxx-operator-call-expr) json))

(defgeneric store-slots (obj json)
  (:documentation "Store values in the json into obj.
Return the object, or another object to be used in
its place."))

(defmethod store-slots ((obj new-clang-ast) (json list))
  (dolist (x json)
    (destructuring-bind (slot . value) x
      (setf obj (store-slot obj slot value))))
  obj)

(defgeneric store-slot (obj slot value)
  (:documentation "Converts json VALUE into appropriate internal
form for SLOT, and stores into OBJ.  Returns OBJ or its replacement."))

(defmethod store-slot ((obj new-clang-ast) (slot symbol) value)
  ;; Generic case
  (let ((attrs (new-clang-ast-attrs obj)))
    (assert (null (aget slot attrs)) () "Duplicate slot ~a" slot)
    (when-let ((converted-value (convert-slot-value obj slot value)))
      (setf (new-clang-ast-attrs obj)
            (append attrs `((,slot . ,converted-value))))))
  obj)

(defmethod store-slot ((obj new-clang-ast) (slot (eql :kind)) value)
  (declare (ignorable slot))
  (setf (new-clang-ast-class obj) (json-kind-to-keyword value))
  obj)

(defmethod store-slot ((obj new-clang-ast) (slot (eql :definitiondata)) value)
  (declare (ignorable slot value))
  ;; Do not translate this attribute for now
  obj)

(defmethod store-slot ((obj new-clang-ast) (slot (eql :bases)) value)
  (declare (ignorable slot value))
  ;; Do not translate this attribute for now
  obj)

(defmethod store-slot ((obj new-clang-ast) (slot (eql :path)) value)
  (declare (ignorable slot value))
  ;; Do not translate this attribute for now
  obj)

(defmethod store-slot ((obj new-clang-ast) (slot (eql :id)) value)
  (setf (new-clang-ast-id obj) (convert-slot-value obj slot value))
  obj)

(defmethod store-slot ((obj new-clang-ast) (slot (eql :inner)) value)
  (declare (ignorable slot))
  (setf (new-clang-ast-children obj)
        (remove nil (mapcar (lambda (o) (clang-convert-json o)) value)))
  obj)

(defmethod store-slot ((obj new-clang-ast) (slot (eql :lookups)) value)
  (declare (ignorable slot))
  (setf (new-clang-ast-children obj)
        (remove nil (mapcar (lambda (o) (clang-convert-json o)) value)))
  obj)

(defmethod store-slot ((obj new-clang-ast) (slot (eql :array_filler)) value)
  (declare (ignorable slot))
  (setf (new-clang-ast-children obj)
        (remove nil (mapcar (lambda (o) (clang-convert-json o)) value)))
  obj)

(defgeneric convert-slot-value (obj slot value)
  (:documentation "Convert a value in the context of a specific slot.  Return of
NIL indicates no value."))

(defmethod convert-slot-value ((obj new-clang-ast) (slot symbol) value)
  ;; Default to a context-independent conversion
  (declare (ignorable obj slot))
  (clang-convert-json value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :referenceddecl)) value)
  (declare (ignorable obj slot))
  (clang-convert-json value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :decl)) value)
  (declare (ignorable obj slot))
  (clang-convert-json value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :templateparams)) value)
  (declare (ignorable obj slot))
  (mapcar #'clang-convert-json value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :id)) value)
  (declare (ignorable obj slot))
  (read-c-integer value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :previousdecl)) value)
  (declare (ignorable obj slot))
  (read-c-integer value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :name)) value)
  (declare (ignorable obj slot))
  (and (not (equal value "")) (call-next-method)))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :tagused)) value)
  (declare (ignorable obj slot))
  (cond
    ((equal value "struct") :struct)
    ((equal value "union") :union)
    (t (call-next-method))))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :storageClass)) value)
  (declare (ignorable obj slot))
  (cond
    ((equal value "auto") :auto)
    ((equal value "static") :static)
    ((equal value "extern") :extern)
    ((equal value "register") :register)
    ((equal value "__private_extern__") :__PRIVATE_EXTERN__)
    (t (call-next-method))))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :valueCategory)) (value string))
  (declare (ignorable obj slot))
  (cond
    ((equal value "rvalue") :rvalue)
    ((equal value "lvalue") :lvalue)
    (t (intern (string-upcase value) :keyword))))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :castKind)) (value string))
  (declare (ignorable obj slot))
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
      (make-new-clang-loc
       :file (canonicalize-string (cached-aget :file loc-json))
       :line (cached-aget :line loc-json)
       :col (cached-aget :col loc-json)
       :tok-len (cached-aget :toklen loc-json))))

(defun convert-macro-loc-json (loc-json)
  "This is the special case of a LOC that has spelling and expansion locs"
  (let* ((spelling-loc (convert-loc-json (aget :spellingloc loc-json)))
         (expansion-loc-json (aget :expansionloc loc-json))
         (expansion-loc (convert-loc-json expansion-loc-json))
         (is-macro-arg-expansion (aget :ismacroargexpansion expansion-loc-json)))
    (when (or spelling-loc expansion-loc)
      (make-new-clang-macro-loc
       :spelling-loc spelling-loc
       :expansion-loc expansion-loc
       :is-macro-arg-expansion is-macro-arg-expansion))))

(defun convert-range-json (range-json)
  "Special handler for values of range attributes"
  (let ((begin (convert-loc-json (aget :begin range-json)))
        (end (convert-loc-json (aget :end range-json))))
    (when (or begin end)
      (make-new-clang-range :begin begin :end end))))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :loc)) value)
  (declare (ignorable obj slot))
  (convert-loc-json value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :range)) value)
  (declare (ignorable obj slot))
  (convert-range-json value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :type)) (value list))
  (convert-type-slot-value obj slot value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :argtype)) (value list))
  (convert-type-slot-value obj slot value))

(defun convert-type-slot-value (obj slot value)
  (declare (ignore obj slot))
  ;; These should be strings, but convert anyway to canonicalize them
  (canonicalize-type (clang-convert-json (aget :qualtype value))
                     (clang-convert-json (aget :desugaredqualtype value))))

;; Helpers for JSON conversion
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
  (if (boundp '*canonical-new-clang-type-table*)
      (or (gethash qual *canonical-new-clang-type-table*)
          (setf (gethash qual *canonical-new-clang-type-table*)
                (make-instance 'new-clang-type
                  :qual qual
                  :desugared desugared)))
      (make-instance 'new-clang-type
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

(defgeneric remove-non-program-asts (ast file)
  (:documentation "Remove ASTs from ast that are not from the
actual source file"))

(defun is-non-program-ast (a file)
  (let ((f (ast-file a)))
    (and f (not (equal file f)))))

(defmethod remove-non-program-asts ((ast ast) (file string))
  (flet ((%non-program (a) (is-non-program-ast a file)))
    ;; Remove asts based on FILE only at the top level
    ;; This could fail on #include inside other forms,
    ;; but we have trouble with macroexpansion there.
    ;; TO BE FIXED
    (setf (ast-children ast)
          (remove-if #'%non-program (ast-children ast))))
  ast)

(defun remove-asts-in-classes (ast classes)
  (remove-asts-if ast (lambda (o) (member (ast-class o) classes))))

(defun remove-file-from-asts (ast-root tmp-file)
  "Remove the file attribute from the ASTs in AST-ROOT which are located
in TMP-FILE (the original genome)."
  (labels
      ((remove-file-from-loc (loc)
         (cond ((typep loc 'new-clang-macro-loc)
                (make-new-clang-macro-loc
                 :spelling-loc
                 (remove-file-from-loc (new-clang-macro-loc-spelling-loc loc))
                 :expansion-loc
                 (remove-file-from-loc (new-clang-macro-loc-expansion-loc loc))
                 :is-macro-arg-expansion
                 (new-clang-macro-loc-is-macro-arg-expansion loc)))
               ((and loc (equal tmp-file (ast-file loc)))
                (make-new-clang-loc
                 :line (new-clang-loc-line loc)
                 :col (new-clang-loc-col loc)
                 :offset (new-clang-loc-offset loc)
                 :tok-len (new-clang-loc-tok-len loc)))
               (t loc))))
    (map-ast ast-root
             (lambda (ast)
               (when (equal (ast-file ast) tmp-file)
                 (setf (ast-attr ast :range)
                       (make-new-clang-range
                        :begin (nest (remove-file-from-loc)
                                     (new-clang-range-begin)
                                     (ast-range ast))
                        :end (nest (remove-file-from-loc)
                                   (new-clang-range-end)
                                   (ast-range ast)))))))))

(defun line-offsets (str)
  "Return a list with containing the byte offsets of each new line in STR."
  (cons 0 (iter (with byte = 0)
                (for c in-string str)
                (incf byte (string-size-in-octets (make-string 1 :initial-element c)))
                (when (eq c #\Newline)
                  (collect byte)))))

(defun convert-line-and-col-to-byte-offsets
    (ast-root genome &aux (line-offsets (line-offsets genome)))
  "Convert AST range begin/ends in AST-ROOT from line and column pairs to
byte offsets.

* AST-ROOT root of the AST tree for GENOME
* GENOME string with the source text of the program"
  (labels
      ((to-byte-offset (line col)
         (+ (1- col) (nth (1- line) line-offsets)))
       (convert-to-byte-offsets (loc)
         (cond ((typep loc 'new-clang-macro-loc)
                (make-new-clang-macro-loc
                 :spelling-loc
                 (convert-to-byte-offsets (new-clang-macro-loc-spelling-loc loc))
                 :expansion-loc
                 (convert-to-byte-offsets (new-clang-macro-loc-expansion-loc loc))
                 :is-macro-arg-expansion
                 (new-clang-macro-loc-is-macro-arg-expansion loc)))
               ((null (ast-file loc))
                (make-new-clang-loc
                 :offset (to-byte-offset (new-clang-loc-line loc)
                                         (new-clang-loc-col loc))
                 :tok-len (new-clang-loc-tok-len loc)))
               (t loc))))
    (map-ast ast-root
             (lambda (ast)
               (when-let ((_ (and (not (eq :TopLevel (ast-class ast)))
                                  (null (ast-file ast))))
                          (begin (new-clang-range-begin (ast-range ast)))
                          (end (new-clang-range-end (ast-range ast))))
                 (setf (ast-attr ast :range)
                       (make-new-clang-range
                        :begin (convert-to-byte-offsets begin)
                        :end (convert-to-byte-offsets end))))))))

(defun multibyte-characters (str)
  "Return a listing of multibyte character byte offsets and their length in STR."
  (iter (for c in-string str)
        (with byte = 0)
        (for len = (string-size-in-octets (make-string 1 :initial-element c)))
        (incf byte len)
        (when (> len 1)
          (collecting (cons byte (1- len))))))

(defun fix-multibyte-characters (ast-root genome
                                 &aux (mb-chars (multibyte-characters genome)))
  "Convert AST range begin/ends in AST-ROOT from byte offsets to character
offsets to support source text with multibyte characters.

* AST-ROOT root of the AST tree for GENOME
* GENOME string with the source text of the program"
  (labels
      ((byte-offset-to-chars (offset)
         "Convert the given byte OFFSET to a character offset."
         (- offset
            (iter (for (pos . incr) in mb-chars)
                  (while (<= pos offset))
                  (summing incr))))
       (byte-loc-to-chars (loc)
         "Convert the given LOC using byte offsets to one using character offsets."
         (cond ((typep loc 'new-clang-macro-loc)
                (make-new-clang-macro-loc
                 :spelling-loc
                 (byte-loc-to-chars (new-clang-macro-loc-spelling-loc loc))
                 :expansion-loc
                 (byte-loc-to-chars (new-clang-macro-loc-expansion-loc loc))
                 :is-macro-arg-expansion
                 (new-clang-macro-loc-is-macro-arg-expansion loc)))
               ((null (ast-file loc))
                (make-new-clang-loc
                 :offset (byte-offset-to-chars (new-clang-loc-offset loc))
                 :tok-len (- (byte-offset-to-chars
                              (+ (new-clang-loc-offset loc)
                                 (new-clang-loc-tok-len loc)))
                             (byte-offset-to-chars (new-clang-loc-offset loc)))))
               (t loc))))
    (map-ast ast-root
             (lambda (ast)
               (when-let ((_ (and (not (eq :TopLevel (ast-class ast)))
                                  (null (ast-file ast))))
                          (begin (new-clang-range-begin (ast-range ast)))
                          (end (new-clang-range-end (ast-range ast))))
                 (setf (ast-attr ast :range)
                       (make-new-clang-range
                        :begin (byte-loc-to-chars begin)
                        :end (byte-loc-to-chars end))))))))

(defgeneric compute-operator-positions (ast)
  (:documentation "Compute positions of operators in
CXXOperatorCallExpr nodes")
  (:method ((ast new-clang-ast))
    (map-ast ast #'compute-operator-position)))

(defgeneric compute-operator-position (ast)
  (:documentation "Compute positions of operators at a
CXXOperatorCallExpr node.   Also, normalize postfix operator++/--
to remove dummy arg")
  (:method ((ast new-clang-ast)) nil)
  (:method ((ast cxx-operator-call-expr))
    (let* ((ac (ast-children ast))
           (op (first ac))
           (op-begin (begin-offset op)))
      ;; The last argument to a ++ or -- is dummy when
      ;; it's a postfix operator.  Remove it.
      (when (and (= (length ac) 3)
                 (let ((rds (ast-reference-decls op)))
                   (flet ((%m (s) (member s rds :key #'ast-name
                                          :test #'equal)))
                     (or (%m "operator++") (%m "operator--")))))
        (setf ac (setf (ast-children ast) (subseq ac 0 2))))
      ;; Position = # of child asts that are before
      ;; the operator in the source file
      (let ((rest-offsets (mapcar #'begin-offset (cdr ac))))
        (when (and op-begin (every #'identity rest-offsets))
          (setf (cxx-operator-call-expr-pos ast)
                (count op-begin rest-offsets :test #'>)))))))

(defgeneric put-operators-into-inner-positions (sw ast)
  (:documentation "Put operators into their inner positions
in CXXOperatorCallExpr nodes.")
  (:method ((sw new-clang) (ast new-clang-ast))
    (map-ast ast #'put-operator-into-inner-position)))

(defgeneric put-operator-into-inner-position (ast)
  (:documentation "Put operator into its inner position
in a CXXOperatorCallExpr node.")
  (:method ((ast new-clang-ast)) nil)
  (:method ((ast cxx-operator-call-expr))
    ;; This is pre-stringification, so there should only
    ;; be ast children
    (let* ((c (ast-children ast))
           (op (first c))
           (pos (cxx-operator-call-expr-pos ast)))
      (assert (every #'ast-p c))
      (when pos
        (assert (< pos (length c)))
        (setf (ast-children ast)
              (append (subseq c 1 (1+ pos))
                      (list op)
                      (subseq c (1+ pos))))))))

;;; Macro expansion code
(defgeneric find-macro-uses (obj a)
  (:documentation "Identify the locations at which macros occur in the
file, and the length of the macro token.  Returns a hash table mapping
macro offsets to a list with additional information on the macro.
The first element of the list is the position of the start of the
macro name, the second is the length, the third is true
when the macro has one or more macro parameters that became expressed
in the AST, and the fourth is the length of the macro expression in
genome."))

(defmethod find-macro-uses (obj a &aux (macro-table (make-hash-table)))
  (map-ast a (lambda (x) (find-macro-use-at-node obj x macro-table)))
  macro-table)

(defgeneric compute-macro-extent (obj off len is-arg)
  (:documentation "Compute the length of a macro occurrence in
the genome.  IS-ARG, when true, indicates this was a parameterized
macro.  OBJ is the software object, OFF the starting offset,
LEN the length of the macro name.")
  (:method (obj off len is-arg)
    (let* ((genome (genome obj))
           (glen (length genome)))
      (assert (<= 0 off))
      (assert (< 0 len))
      (assert (<= (+ off len) (length genome)))
      (if is-arg
          (let ((i (+ off len)))
            ;; Skip over whitespace after macro
            (iter (while (< i glen))
                  (while (member (elt genome i) +whitespace-chars+))
                  (incf i))
            (if (or (>= i glen)
                    (not (eql (elt genome i) #\()))
                len ;; give up; could not find macro arguments
                (let ((end (cpp-scan genome (constantly t)
                                     :start i :skip-first t)))
                  (- end off))))
          len))))

(defun find-macro-use-at-node (obj a table)
  (when-let ((range (ast-range a)))
    (flet ((%record (loc)
             (when (typep loc 'new-clang-macro-loc)
               (when-let* ((eloc (new-clang-macro-loc-expansion-loc loc)))
                 (when (null (new-clang-loc-file eloc))
                   (let* ((off (offset eloc))
                          (existing (gethash off table))
                          (len (or (first existing) (tok-len eloc)))
                          (is-arg (or (second existing)
                                      (new-clang-macro-loc-is-macro-arg-expansion
                                       loc)))
                          (extent (compute-macro-extent obj off len is-arg)))
                     (setf (gethash off table)
                           (list len is-arg extent))))))))
      (%record (new-clang-range-begin range))
      (%record (new-clang-range-end range)))))

(defgeneric encapsulate-macro-expansions-cheap (ast table)
  (:documentation "Replace macro expansions with :MACROEXPANSION
nodes.  This does not try to find macro arguments.  AST is the root of
the AST, and TABLE is a mapping from macro use offsets to a list
containing the length of the macro token and a flag that is T if the
macro was found to have arguments below it in the AST (so look for a
'(' character).  Uses cpp-scan to determine the extent of the
macro.")
  (:method (ast (table hash-table))
    (map-ast-while
     ast
     (lambda (a) (encapsulate-macro-expansion-cheap-below-node a table)))))

(defun %is-macro-expansion-node (x table)
  ;; The logic here:  determine if this entire AST comes from a macroexpansion
  ;; It does when (1) the start end end locations are macro locs, (2) they
  ;; come from the SAME macro expansion.  Without this check, and expression
  ;; like X*Y (where X and Y are macros) will not be handled properly.
  (assert (ast-p x))
  (when-let* ((range (ast-range x))
              (begin (new-clang-range-begin range))
              (end (new-clang-range-end range)))
    (when (and (typep begin 'new-clang-macro-loc)
               (typep end 'new-clang-macro-loc))
      (when-let* ((beloc (new-clang-macro-loc-expansion-loc begin))
                  (boff (offset beloc))
                  (eeloc (new-clang-macro-loc-expansion-loc end))
                  (eoff (offset eeloc))
                  (len/args (gethash boff table)))
        (when (eql boff eoff) (cons boff len/args))))))

(defgeneric encapsulate-macro-expansion-cheap-below-node (a table)
  ;; Walk over the children of A, combining those that are from the same
  ;; macroexpansion into a single macroexpansion node.
  (:method (a (table hash-table))
    ;; Scan the children of A, grouping those that are macro expansion
    ;; nodes of the same offset.
    (unless (eql (ast-class a) :macroexpansion)
      (let* ((last-offset nil)
             (m nil)
             (macro-child-segment nil)
             (c (ast-children a))
             (changed? nil)
             (new-children
              (iter (flet ((%collect ()
                             (when macro-child-segment
                               (collecting
                                (let ((b-loc (nest (new-clang-range-begin)
                                                   (ast-range)
                                                   (car macro-child-segment)))
                                      (e-loc (nest (new-clang-range-end)
                                                   (ast-range)
                                                   (lastcar macro-child-segment)))
                                      (new-begin-offset (first m))
                                      (new-begin-tok-len (second m))
                                      (new-end-offset (+ (first m) (fourth m))))
                                  (make-new-clang-ast
                                   :class :macroexpansion :attrs
                                   `((:range .
                                             ,(make-new-clang-range
                                               :begin (copy b-loc :expansion-loc
                                                            (make-new-clang-loc
                                                             :file (ast-file b-loc)
                                                             :offset new-begin-offset
                                                             :tok-len new-begin-tok-len)
                                                            :is-macro-arg-expansion nil)
                                               :end (copy e-loc :expansion-loc
                                                          (make-new-clang-loc
                                                           :file (ast-file e-loc)
                                                           :offset new-end-offset)
                                                          :is-macro-arg-expansion nil)))
                                     (:macro-child-segment .
                                                           ,macro-child-segment))))))
                             (setf changed? t
                                   m nil
                                   macro-child-segment nil
                                   last-offset nil)))
                      (if c
                          (let ((x (pop c)))
                            (let ((macro-info (%is-macro-expansion-node
                                               x table)))
                              (if macro-info
                                  (if (eql (car macro-info) last-offset)
                                      (setf macro-child-segment
                                            (append macro-child-segment
                                                    (list x)))
                                      ;; start new macro segment
                                      (progn
                                        (%collect)
                                        (setf m macro-info
                                              last-offset (car macro-info)
                                              macro-child-segment (list x))))
                                  (progn
                                    (%collect)
                                    (collecting x)))))
                          (progn
                            (%collect)
                            (finish)))))))
        (when changed?
          (setf (ast-children a) new-children))
        t))))

(defun populate-macro-attr-in-macro-expansion-nodes (ast-root genome macros)
  "For :MACROEXPANSION ASTs in AST-ROOT populate the :MACRO attribute
with the macro in MACROS corresponding to the current node."
  (map-ast ast-root
           (lambda (ast)
             (when-let ((is-macro-ast (eq :MacroExpansion (ast-class ast)))
                        (macro (find (subseq genome
                                             (begin-offset ast)
                                             (+ (begin-offset ast)
                                                (begin-tok-len ast)))
                                     macros :test #'equal :key #'macro-name)))
               (setf (ast-attr ast :macro) macro)))))

(defun fix-overlapping-vardecls (sw ast)
  (map-ast ast (lambda (a) (fix-overlapping-vardecls-at-node sw a))))

(defun fix-overlapping-vardecls-at-node (sw ast)
  "Separate consecutive, overlapping :VAR and :FIELD nodes so their
text ranges in the source do not overlap, if possible."
  ;; This does not exactly reproduce what the old clang
  ;; front end was doing.  There, the Var nodes were children of
  ;; each other in some cases.
  (let ((child-asts (ast-children ast)))
    (let (prev pos)
      (when (and (ast-p (car child-asts))
                 (member (ast-class (car child-asts))
                         '(:var :field)))
        (setf prev (car child-asts))
        (setf pos (begin-offset prev)))
      (do* ((e (cdr child-asts) (cdr e))
            (c (car e) (car e)))
           ((null e))
        (if (ast-p c)
            (let ((next-pos (begin-offset c))
                  (end (end-offset c)))
              (if (member (ast-class c) '(:var :field))
                  (progn
                    (if prev
                        (if (and next-pos end)
                            (if (< (end-offset prev) next-pos)
                                ;; things are fine -- no overlap
                                (setf prev c pos next-pos)
                                ;; There is overlap -- find the next
                                ;; position
                                (let ((comma-pos
                                       (cpp-scan (genome sw)
                                                 (lambda (c) (eql c #\,))
                                                 :start pos
                                                 :end (1+ end))))
                                  (if comma-pos
                                      (setf pos (1+ comma-pos)
                                            (offset c) pos
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
                 (iter (for c in (ast-children a))
                       (when (and (ast-p c) (equal (ast-file a) (ast-file c)))
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
                   #+(or)
                   (format t "Change range of ~a to ~a, ~a~%"
                           a min-begin max-end)
                   (setf (ast-attr a :range)
                         (make-new-clang-range
                          :begin (make-new-clang-loc
                                  :file (ast-file a)
                                  :offset min-begin)
                          :end (make-new-clang-loc
                                :file (ast-file a)
                                :offset max-end))))))))
      ;; Fixpoint for normalization of ranges
      (loop
         (setf changed? nil)
         (map-ast ast #'%normalize)
         (map-ast-postorder ast #'%normalize)
         (unless changed? (return ast))))))

;;; FIXME: refactor this into small functions, for
;;;  understandability and line length limits
(defun combine-overlapping-siblings (sw ast &aux (genome (genome sw)))
  "Group sibling nodes with overlapping or out of order
ranges into 'combined' nodes.  Warn when this happens."
  (flet ((%check (a)
           (let ((end 0)
                 changed? accumulator)
             (flet ((%sorted-children (children)
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
                    (%combine ()
                      (case (length accumulator)
                        (0)
                        (1 (list (pop accumulator)))
                        (t
                         (let ((new-begin (reduce #'min accumulator
                                                  :key #'begin-offset))
                               (new-end (reduce #'max accumulator
                                                :key #'extended-end-offset)))
                           (prog1
                               (unless (eql new-begin new-end)
                                 (setf changed? t)
                                 (if (and (= (length accumulator) 2)
                                          (eql (ast-class (car accumulator)) :typedef))
                                     ;; Special case: there are two nodes, and the first is a typedef
                                     ;; In that case, make the second a child of the first
                                     (progn
                                       (push (cadr accumulator) (ast-children (car accumulator)))
                                       (list (car accumulator)))
                                     ;; Otherwise, create a "Combined" node with the children as the
                                     ;; overlapping ASTs.  Previously, the overlapping children
                                     ;; where stored in a "subsumed" attribute instead of as
                                     ;; the children field.  However, this led to issue when writing
                                     ;; out the file as `source-text` relies on the children field.
                                     ;; Further, it required special case logic for all methods
                                     ;; which call `ast-children` (e.g. replace-in-ast,
                                     ;; replace-nth-child, remove-ast, replace-ast, insert-ast,
                                     ;; map-ast, map-ast-strings, map-ast-with-ancestor,
                                     ;; get-immediate-children, etc.).  By making the overlapping
                                     ;; nodes children of the combined node, we only need special
                                     ;; case logic when writing out the source text of the combined
                                     ;; AST which is less error-prone than the converse.
                                     (list (make-new-clang-ast
                                            :class :combined
                                            :children accumulator
                                            :attrs `((:range .
                                                             ,(make-new-clang-range
                                                               :begin (make-new-clang-loc
                                                                       :file (ast-file (car accumulator))
                                                                       :offset new-begin)
                                                               :end (make-new-clang-loc
                                                                     :file (ast-file (car accumulator))
                                                                     :offset new-end)))
                                                     (:source-text .
                                                                   ,(subseq genome
                                                                            new-begin
                                                                            new-end)))))))
                             (setf accumulator nil)))))))
               (unless (eq :combined (ast-class a))
                 (let ((new-children
                        (append
                         ;; Find child ASTs and sort them in textual order.
                         (iter (for c in (%sorted-children (ast-children a)))
                               (multiple-value-bind (cbegin cend)
                                   (begin-and-end-offsets c)
                                 (if (and cbegin end cend (< cbegin end)
                                          (equal (ast-file a) (ast-file c)))
                                     (setf accumulator
                                           (append accumulator (list c))
                                           end (max end cend))
                                     (progn
                                       (appending (%combine))
                                       (setf accumulator (list c)
                                             end cend)))))
                         (%combine))))
                   (when changed?
                     (setf (ast-children a) new-children))))))))
    (map-ast ast #'%check)))

(defmethod append-string-to-node ((a new-clang-ast) (str string))
  (if (eq (ast-class a) :combined)
      (setf (ast-attr a :source-text)
            (concatenate 'string (ast-attr a :source-text) str))
      (call-next-method)))

(defmethod remove-semicolon ((a new-clang-ast))
  (if (eq (ast-class a) :combined)
      (let* ((st (ast-attr a :source-text))
             (nst (remove-semicolon st)))
        (if (eq st nst)
            a
            (copy a :source-text nst)))
      (call-next-method)))

(defmethod add-semicolon ((a new-clang-ast) pos)
  (if (eq (ast-class a) :combined)
      (setf (ast-attr a :source-text)
            (add-semicolon (ast-attr a :source-text) pos))
      (call-next-method)))

(defun decorate-ast-with-strings (sw ast &aux (genome (genome sw)))
  (labels
      ((%assert1 (i cbegin c)
         (assert (>= cbegin i) ()
                 "Offsets out of order: i = ~a,~
                  cbegin = ~a, c = ~a, range = ~a"
                 i cbegin c
                 (ast-attr c :range)))
       (%safe-subseq (seq start end)
         (subseq seq start (if (<= end start) start end)))
       (%decorate (a)
         ;; At ast node A, split the parts of the source
         ;; that are not in the children into substrings
         ;; that are placed between the children.  Do not
         ;; place strings for children for whom offsets
         ;; cannot be computed
         (let ((children (ast-children a)))
           (multiple-value-bind (begin end)
               (begin-and-end-offsets a)
             (when (and begin end (null (ast-file a)))
               (let ((i begin))
                 (setf
                  (ast-children a)
                  (nconc
                   (iter
                    (for c in children)
                    (when (and (ast-p c) (null (ast-file c)))
                      (multiple-value-bind (cbegin cend)
                          (begin-and-end-offsets c)
                        (when cbegin
                          (unless (eq :Combined (ast-class a))
                            (%assert1 i cbegin c))
                          (collect (%safe-subseq genome i cbegin))
                          (setf i cend))))
                    (collect c))
                   (list (%safe-subseq genome i end))))))))))
    (map-ast ast #'%decorate))
  ast)

(defgeneric put-operators-into-starting-positions (sw ast)
  (:documentation "Put operators into their starting positions
in CXXOperatorCallExpr nodes.")
  (:method ((sw new-clang) (ast new-clang-ast))
    (map-ast ast #'put-operator-into-starting-position)))

(defgeneric put-operator-into-starting-position (ast)
  (:documentation "Put operator into their starting position
in a CXXOperatorCallExpr node.")
  (:method ((ast new-clang-ast)) (declare (ignorable ast)) nil)
  (:method ((ast cxx-operator-call-expr))
    ;; The AST will have been stringified here, so pos
    ;; is the position in (remove-if-not #'ast-p (ast-children))
    (let ((pos (cxx-operator-call-expr-pos ast))
          (c (ast-children ast)))
      (when pos
        (assert (<= 0 pos (1- (length c))))
        (when (> pos 0)
          (let ((actual-pos
                 (let ((count pos))
                   (position-if (lambda (e)
                                  (and (ast-p e) (zerop (decf count))))
                                c))))
            (setf (ast-children ast)
                  (append
                   (list (elt c actual-pos))
                   (subseq c 0 actual-pos)
                   (subseq c (1+ actual-pos))))))))))

(defgeneric compute-full-stmt-attr (obj ancestors)
  (:documentation "Fills in the :FULL-STMT attribute on clang ast
nodes, as needed.")
  (:method ((obj new-clang-ast) ancestors)
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
          (setf (ast-attr obj :full-stmt) t))))
    obj))

(defun compute-full-stmt-attrs (ast)
  (map-ast-with-ancestors ast #'compute-full-stmt-attr))

(defgeneric compute-guard-stmt-attr (obj ancestors)
  (:documentation "Fills in the :GUARD-STMT attribute on clang
ast nodes, as needed")
  (:method ((obj new-clang-ast) ancestors)
    (when ancestors
      (let ((parent (car ancestors)))
        (when (not (is-single-line-stmt obj parent))
          (case (ast-class parent)
            ((:CapturedStmt :CompoundStmt :CXXCatchStmt :DoStmt
                            :ForStmt :IfStmt :SwitchStmt :WhileStmt)
             (setf (ast-attr obj :Guard-Stmt) t))))))
    obj))

(defun compute-guard-stmt-attrs (ast)
  (map-ast-with-ancestors ast #'compute-guard-stmt-attr))

(defgeneric compute-syn-ctx (ast ancestors)
  (:documentation "Fill in the syn-ctx slot")
  (:method ((obj new-clang-ast) ancestors)
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
      (setf (new-clang-ast-syn-ctx obj) syn-ctx))
    obj))

(defgeneric fix-var-syn-ctx (ast)
  (:documentation "Fix the syn-ctx of Var and ParmVar nodes")
  (:method ((obj new-clang-ast))
    (let ((prev nil)
          (prev-var? nil))
      (unless (eql (ast-class obj) :toplevel)
        (iter (for c in (ast-children obj))
              (when (new-clang-ast-p c)
                (case (ast-class c)
                  ((:Var :ParmVar)
                   ;; This logic makes single element ParmVar lists
                   ;; be :Generic.  Weird, but that's what clang-mutate
                   ;; did
                   (when prev-var?
                     (setf (new-clang-ast-syn-ctx prev) :ListElt
                           (new-clang-ast-syn-ctx  c) :FinalListElt))
                   (setf
                    prev c
                    prev-var? t))
                  (t (setf prev-var? nil prev nil)))))))
    obj))

(defun compute-syn-ctxs (ast)
  (map-ast-with-ancestors ast #'compute-syn-ctx)
  (map-ast ast #'fix-var-syn-ctx))

(defmethod update-paths
    ((ast new-clang-ast) &optional path)
  "Modify AST in place with all paths updated to begin at PATH"
  (setf (ast-path ast) (reverse path)
        (ast-children ast)
        (iter (for c in (ast-children ast))
              (for i upfrom 0)
              (collect (if (typep c 'ast)
                           (update-paths c (cons i path))
                           c))))
  ast)

(defmethod update-asts ((obj new-clang))
  ;; Port of this method from clang.lsp, for new class
  (let ((*canonical-string-table* (make-hash-table :test 'equal))
        (*canonical-new-clang-type-table* (make-hash-table :test 'equal)))
    (with-slots (ast-root genome macros includes types
                          symbol-table name-symbol-table) obj
      (unless genome     ; get genome from existing ASTs if necessary
        (setf genome (genome obj)
              ast-root nil))

      (setf macros (find-macros-in-string genome)
            types (make-hash-table)
            symbol-table (make-hash-table :test #'equal)
            name-symbol-table (make-hash-table :test #'equal))

      (multiple-value-bind (json tmp-file genome-len)
          (clang-json obj)
        (let ((ast (clang-convert-json-for-file json tmp-file genome-len)))
          ;; Populate and massage auxilliary fields such as symbol tables
          ;; and types.
          (update-symbol-table symbol-table ast)
          (update-name-symbol-table name-symbol-table symbol-table)
          (remove-non-program-asts ast tmp-file)
          (update-referenceddecl-from-symbol-table ast symbol-table)
          (update-type-table types symbol-table ast)

          ;; Massage the ASTs identified by clang.
          (remove-asts-if ast #'ast-is-implicit)
          (remove-file-from-asts ast tmp-file)
          (convert-line-and-col-to-byte-offsets ast genome)
          (fix-multibyte-characters ast genome)
          (compute-operator-positions ast)
          (put-operators-into-inner-positions obj ast)
          (encapsulate-macro-expansions-cheap ast (find-macro-uses obj ast))
          (populate-macro-attr-in-macro-expansion-nodes ast genome macros)
          (fix-overlapping-vardecls obj ast) ; must be after macro encapsulation
          (fix-ancestor-ranges ast)
          (combine-overlapping-siblings obj ast)
          (decorate-ast-with-strings obj ast)
          (put-operators-into-starting-positions obj ast)
          (compute-full-stmt-attrs ast)
          (compute-guard-stmt-attrs ast)
          (compute-syn-ctxs ast)
          (fix-semicolons ast)
          (update-paths ast)
          (setf ast-root ast
                genome nil)
          obj)))))


;;; Helper methods for computing attributes on new clang ast nodes

(defgeneric nth-ast-child (obj n)
  (:documentation
   "Returns the Nth child of OBJ that is an AST, starting
at zero, or NIL if there is none."))

(defmethod nth-ast-child ((obj new-clang-ast) n)
  (declare (type (and fixnum (integer 0)) n))
  (let ((children (ast-children obj)))
    (loop
       (unless children (return nil))
       (let ((next-child (pop children)))
         (when (or (null next-child) ;; NIL is considered an AST
                   (ast-p next-child))
           (if (<= n 0)
               (return next-child)
               (decf n)))))))

(defgeneric pos-ast-child (child obj &key child-fn test)
  (:documentation
   "Returns the position of CHILD in the child list of OBJ (with children
failing the CHILD-FN test omitted, or NIL if CHILD does not satisfy
the test or is not present."))

(defmethod pos-ast-child (child (obj new-clang-ast)
                          &key (child-fn (complement #'stringp))
                            (test #'eql))
  (let ((pos 0)
        (children (ast-children obj)))
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
              (eql s (car (last (remove-if-not #'ast-p (ast-children p))))))
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


;;; Parsing a source code snippet

(defmethod parse-source-snippet ((type (eql :new-clang))
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


(defun cpp-scan (str until-fn &key (start 0) (end (length str))
                                (skip-first nil)
                                (angle-brackets))
  "Scan string STR from START to END, skipping over parenthesizd
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
                    (case c
                      (#.+whitespace-chars+ (inc?))
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
                              (inc?))))))))
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

