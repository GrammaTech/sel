;;; new-clang.lisp --- clang software representation
;;;
;;; DOCFIXME
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
        :cl-json
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
  (:export :new-clang
           :combine-overlapping-siblings
           :decorate-ast-with-strings
           :clang-convert-json-for-file
           :remove-non-program-asts
           :make-statement-new-clang
           :*new-clang?*
           :make-new-clang-macroexpand-hook))
(in-package :software-evolution-library/software/new-clang)
(in-readtable :curry-compose-reader-macros)

(declaim (optimize (debug 3)))

(declaim (special *soft*))

(defparameter *clang-binary*
  ;; "/pdietz/clang9-installed/bin/clang"
  "/clang9/bin/clang"
  "This is the location clang is installed in the clang9 Docker image")
;; Install locally by:
;;  sudo docker cp clang9:/clang9 /clang9
;; This takes several minutes and uses about 34GB of disk space

(define-software new-clang (clang-base genome-lines-mixin)
  ((functions :initarg :functions :reader functions
              :initform nil :copier :direct
              :type #+sbcl (list (cons keyword *) *) #-sbcl list
              :documentation "Complete functions with bodies.")
   (prototypes :initarg :prototypes :reader prototypes
               :initform nil :copier :direct
               :type #+sbcl (list (cons keyword *) *) #-sbcl list
               :documentation "Function prototypes.")
   (includes :initarg :includes :accessor includes
             :initform nil :copier :direct
             :type #+sbcl (list string *) #-sbcl list
             :documentation "Names of included includes.")
   (types :initarg :types :accessor types
          :initform (make-hash-table :test 'equal)
          :copier copy-hash-table
          :type #+sbcl hash-table #-sbcl hash-table
          :documentation "Association list of types keyed by HASH id.")
   (macros :initarg :macros :accessor macros
           :initform nil :copier :direct
           :type #+sbcl (list clang-macro *) #-sbcl list
           :documentation "Association list of Names and values of macros.")
   (tmp-file
    :initarg :tmp-file :accessor tmp-file
    :initform nil :copier :direct
    :type (or null string)
    :documentation "Full file name of the temporary file
on which clang was run to get the AST.")
   (symbol-table
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
    :documentation "Map from name strings to declaration objects.")
   (type-table
    :initarg :type-table
    :initform (make-hash-table :test #'equal)
    :accessor type-table
    :copier copy-hash-table
    :type hash-table
    :documentation "Mapping from qualtype/desugaredType pairs to new-clang-type
objects.  Used for canonicalization of these objects.")
   (include-dirs
    :initarg :include-dirs
    :accessor include-dirs
    :initform nil
    :type list
    :copier :direct
    :documentation "List of directories that are where include files are searched for
the -I arguments to the C compiler")
   (compiler :initform *clang-binary*)
   (hash-counter :initform 1 :initarg :hash-counter
                 :accessor hash-counter
                 :type integer
                 :documentation "A counter used to assign hash values to type objects and
possibly other things"))
  (:documentation
   "C language (C, C++, C#, etc...) ASTs using Clang, C language frontend for LLVM.
   See http://clang.llvm.org/.  This is for ASTs from Clang 9+."))

;;; The INCLUDE-DIRS slot should be in normal form
;;; Use various methods to ensure this happens

(defmethod initialize-instance :after ((obj new-clang) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (include-dirs obj) (normalize-include-dirs (include-dirs obj)))
  obj)

(defmethod (setf include-dirs) ((dirs list) (obj new-clang))
  (let ((additional-include-dirs
         (flags-to-include-dirs (flags obj))))
    (setf (slot-value obj 'include-dirs)
          (normalize-include-dirs
           (append additional-include-dirs dirs)))))

(defmethod (setf flags) :after ((flags list) (obj new-clang))
  (let ((additional-include-dirs
         (flags-to-include-dirs (flags obj))))
    (setf (slot-value obj 'include-dirs)
          (remove-duplicates
           (normalize-include-dirs
            (append additional-include-dirs
                    (include-dirs obj)))
           :test #'equal))))

(defun flags-to-include-dirs (flags)
  (iter (for e on flags)
        (let ((x (car e)))
          (cond
            ((equal x "-I")
             (collecting (cadr e)))
            ((and (equal (subseq x 0 2) "-I")
                  (> (length x) 2))
             (collecting (subseq x 2)))))))


;;; Code for adapting tests to use old or new clang front
;;; ends, controllably

;; This is DEFVAR so I can set the var and reload this file
;; without losing that value, which is useful for debugging.
(defvar *new-clang?* nil "When true, use NEW-CLANG instead of CLANG")

;;; shared with old clang

(defstruct (new-clang-ast (:include ast-stub)
                          (:conc-name new-clang-ast-))
  ;; Inherit path, children and stored-hash fields
  ;;  from ast-stub
  ;; Class symbol for this ast node
  (class nil :type symbol)
  ;; Association list of attr name -> value pairs
  (attrs nil :type list)
  ;; Hashed id number from Clang
  (id nil :type (or null integer))
  ;; Syntactic context
  (syn-ctx nil :type symbol)
  ;; aux data
  (aux-data nil :type symbol))

(defgeneric ast-attr (ast attr))
(defmethod ast-attr ((ast new-clang-ast) (attr symbol))
  (let ((attrs (new-clang-ast-attrs ast)))
    (aget attr attrs)))

(defun ast-attr-fn (attr)
  (lambda (obj) (ast-attr obj attr)))

(defgeneric (setf ast-attr) (v ast attr))
(defmethod (setf ast-attr) (v (ast new-clang-ast) (attr symbol))
  (let* ((attrs (new-clang-ast-attrs ast))
         (p (assoc attr attrs)))
    (if p (setf (cdr p) v)
        (setf (new-clang-ast-attrs ast)
              (cons (cons attr v) attrs)))
    v))

(defun ast-range (ast) (ast-attr ast :range))
(defun (setf ast-range) (val ast) (setf (ast-attr ast :range) val))

(defmethod ast-name ((obj new-clang-ast)) (ast-attr obj :name))

(defgeneric ast-is-implicit (ast)
  (:method (ast) nil)
  (:method ((ast new-clang-ast))
    (or (ast-attr ast :isimplicit)
        (ast-attr ast :implicit))))

(defgeneric ast-is-class (ast key)
  (:method (ast class) nil)
  (:method ((ast new-clang-ast) (key symbol))
    (and (ast-p ast)
         (eql (ast-class ast) key))))

(defmethod ast-type ((ast new-clang-ast))
  (ast-attr ast :type))

(defun ast-is-class-fun (key)
  (lambda (c) (ast-is-class c key)))

(defun %areplace (key val alist)
  (cons (cons key val) (remove key alist :key #'car)))

(defmethod copy ((ast new-clang-ast)
                 &key
                   path
                   (children nil children-p)
                   (class nil class-p)
                   (attrs nil attrs-p)
                   (id 0 id-p)
                   (syn-ctx nil syn-ctx-p)
                   (full-stmt nil full-stmt-p)
                   (guard-stmt nil guard-stmt-p)
                   (opcode nil opcode-p)
                   &allow-other-keys)
  (let ((children (if children-p children (new-clang-ast-children ast)))
        (class (if class-p class (new-clang-ast-class ast)))
        (attrs (if attrs-p attrs (new-clang-ast-attrs ast)))
        ;; ID should not be copied -- generate a new one?
        (id (if id-p id (new-clang-ast-id ast)))
        (syn-ctx (if syn-ctx-p syn-ctx (new-clang-ast-syn-ctx ast))))
    (flet () #+nil ((%areplace (key val alist)
                               (cons (cons key val) (remove key alist :key #'car))))
          (when full-stmt-p
            (setf attrs (%areplace :fullstmt full-stmt attrs)))
          (when guard-stmt-p
            (setf attrs (%areplace :guardstmt guard-stmt attrs)))
          (when opcode-p
            (setf attrs (%areplace :opcode opcode attrs)))
          (make-new-clang-ast :path path :children children
                              :class class :attrs attrs :id id
                              :syn-ctx syn-ctx))))

(defmethod ast-class ((obj new-clang-ast))
  (new-clang-ast-class obj))
(defmethod (setf ast-class) (val (obj new-clang-ast))
  (setf (new-clang-ast-class obj) val))
(defmethod ast-syn-ctx ((obj new-clang-ast))
  (new-clang-ast-syn-ctx obj))
(defmethod (setf ast-syn-ctx) (val (obj new-clang-ast))
  (setf (new-clang-ast-syn-ctx obj) val))
(defmethod source-text ((ast new-clang-ast))
  (with-output-to-string (out)
    (mapc [{write-string _ out} #'source-text] (ast-children ast))))

(defmethod stmt-asts ((obj new-clang))
  ;; Walk AST, recording all asts at or below :COMPOUNDSTMT asts
  (let ((result nil))
    (labels ((%r (a)
               (when (ast-p a)
                 (if (eql (ast-class a) :compoundstmt)
                     (map-ast a (lambda (b) (push b result)))
                     (mapc #'%r (ast-children a))))))
      (%r (ast-root obj)))
    (nreverse result)))

(defmethod non-stmt-asts ((obj new-clang))
  (let ((result nil))
    (labels ((%r (a)
               (when (ast-p a)
                 (unless (or (function-decl-p a)
                             (eql (ast-class a) :toplevel))
                   (push a result)
                   (mapc #'%r (ast-children a))))))
      (%r (ast-root obj)))
    (nreverse result)))

(defmethod prototypes ((obj new-clang))
  (let ((result nil))
    (labels ((%r (a)
               (when (ast-p a)
                 (if (function-decl-p a)
                     (push a result)
                     (mapc #'%r (ast-children a))))))
      (%r (ast-root obj))
      (nreverse result))))

(defmethod functions ((obj new-clang))
  (let ((result nil))
    (labels ((%r (a)
               (when (ast-p a)
                 (if (function-decl-p a)
                     (when (function-body obj a)
                       (push a result))
                     (mapc #'%r (ast-children a))))))
      (%r (ast-root obj))
      (nreverse result))))

#|
(defmethod includes ((obj new-clang))
(mapcan (lambda (c)
(when (stringp c)
(includes-in-string c)))
(ast-children (ast-root obj))))
|#

(defmethod ast-includes-in-obj ((obj new-clang) (ast new-clang-ast))
  (let ((*soft* obj)
        (tmp-file (tmp-file obj))
        (file (ast-file obj))
        (include-dirs (include-dirs obj))
        (od (original-directory obj)))
    (mapcar
     (lambda (s) (normalize-file-for-include s od include-dirs))
     (delete-duplicates
      (nconc
       (unless (or (not file) (equal tmp-file file))
         (list file))
       (if-let ((ref (ast-referenced-obj ast)))
         (if-let ((file (ast-file ref)))
           (unless (or (not file) (equal file tmp-file))
             (list file))))
       (if-let ((type (ast-type ast)))
         (let ((str (new-clang-type-qual type))
               (table (name-symbol-table obj)))
           ;; look up names in str
           (let ((names (names-in-str str)))
             (iter (for n in names)
                   (nconcing
                    (iter (for v in (gethash n table))
                          (when v
                            (nconcing (ast-include-from-type v tmp-file))))))))))
      :test #'equal))))

(defun ast-include-from-type (v tmp-file)
  (when (member (ast-class v) '(:typedef))
    (let ((file (ast-file v)))
      (unless (equal tmp-file file)
        (list file)))))

(defgeneric compute-includes (obj)
  (:documentation "Fill in the INCLUDES slot of OBJ"))

(defmethod compute-includes ((obj new-clang))
  (let ((includes nil)
        (*soft* obj))
    (map-ast (slot-value obj 'ast-root)
             (lambda (a)
               (dolist (f (ast-includes-in-obj obj a))
                 (pushnew f includes :test #'equal))))
    (setf (includes obj) (nreverse includes))))

(defmethod update-caches ((obj new-clang))
  (call-next-method)
  (compute-includes obj)
  obj)

(defmethod clear-caches ((obj new-clang))
  (with-slots (includes) obj
    (setf includes nil))
  (call-next-method))

(defmethod from-file :after ((obj new-clang) path)
  nil)

(defun names-in-str (str)
  "Find all substrings of STR that are C/C++ names"
  (split-sequence-if-not (lambda (c)
                           (and (typep c 'standard-char)
                                (or (alphanumericp c)
                                    (member c '(#\_ #\: #\< #\>)))))
                         str
                         :remove-empty-subseqs t))


(defmethod ast-unbound-vals ((ast new-clang-ast))
  ;; stub
  nil)

(defmethod ast-unbound-funs ((ast new-clang-ast))
  ;; stub
  nil)

(defmethod ast-includes ((ast new-clang-ast))
  ;; stub
  nil)

(defmethod ast-macros ((ast new-clang-ast))
  ;; stub
  nil)

(defmethod ast-types ((ast new-clang-ast))
  ;; stub
  nil)

(defmethod rebind-vars ((ast new-clang-ast)
                        var-replacements fun-replacements)
  ;; stub
  (copy ast))

(defmethod fixup-mutation (operation (current new-clang-ast)
                           before ast after)
  (clang-fixup-mutation operation current before ast after))

(defmethod begins-scope ((ast new-clang-ast))
  (begins-scope* ast))

(defmethod ast-declarations ((ast new-clang-ast))
  (ast-declarations* ast))

(defmethod ast-var-declarations ((ast new-clang-ast))
  (ast-var-declarations* ast))

;;; Structures for various attribute values

(defstruct new-clang-range begin end)
;; Locations may also be integers, which represent absolute
;; character positions in source genome
(defstruct new-clang-loc col file line tok-len)
(defstruct new-clang-macro-loc
  "Structure used to represent :begin/:end entries for
things in macro expansion.  SPELLING-LOC is the location
in the macro defn, EXPANSION-LOC is at the macro use."
  spelling-loc expansion-loc is-macro-arg-expansion)

;; (defstruct new-clang-type qual desugared)
(defclass new-clang-type ()
  ((qual :accessor new-clang-type-qual
         :initform nil
         :initarg :qual
         :documentation "Translation of the qualType attribute
of clang json type objects")
   (desugared :accessor new-clang-type-desugared
              :initform nil
              :initarg :desugared
              :documentation "Translation of the desugaredQualType
attribute of clang json objects")
   (hash :accessor new-clang-type-hash
         :initarg :hash
         :initform (incf (hash-counter *soft*))
         :documentation "A hash code assigned to type
objects, for compatibility with old clang"))
  (:documentation "Objects representing C/C++ types.  Canonicalized
on QUAL and DESUGARED slots."))

(defmethod initialize-instance :after ((obj new-clang-type) &rest initargs &key hash &allow-other-keys)
  (declare (ignore initargs))
  (when (boundp '*soft*)
    (setf (gethash hash (slot-value *soft* 'types)) obj))
  obj)

(defun make-new-clang-type (&rest keys &key qual desugared (hash (incf (hash-counter *soft*)))
                                         &allow-other-keys)
  (let* ((key (cons qual desugared))
         (sw *soft*)
         (table (when sw (type-table *soft*))))
    (flet ((%make () (apply #'make-instance 'new-clang-type
                            :hash hash keys)))
      (if table
          (or (gethash key table)
              (setf (gethash key table) (%make)))
          (%make)))))

;; Wrapper for values in referencedDecl attribute, when
;; storing them in (symbol-table *soft*)
(defstruct reference-entry obj)

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
    (new-clang-loc-file loc))
  (:method ((loc new-clang-macro-loc) macro?)
    (ast-file*
     (if (if (new-clang-macro-loc-is-macro-arg-expansion loc) (not macro?) macro?)
         (new-clang-macro-loc-spelling-loc loc)
         (new-clang-macro-loc-expansion-loc loc))
     macro?))
  (:method (obj macro?) nil))

(defun json-kind-to-keyword (json-kind)
  (when (stringp json-kind)
    (let ((sym (intern (string-upcase json-kind) :keyword)))
      (case sym
        ((:FunctionDecl) :Function)
        ((:VarDecl) :Var)
        ((:RecordDecl) :Record)
        ((:EnumDecl) :Enum)
        ((:EnumConstantDecl) :EnumConstant)
        ((:FieldDecl) :Field)
        ((:LabelDecl) :Label)
        ((:TranslationUnitDecl) :TopLevel)
        ((:ParmVarDecl) :ParmVar)
        ((:TypedefDecl) :Typedef)
        ((:StaticAssertDecl) :StaticAssert)
        (t sym)))))

(defgeneric clang-previous-decl (obj))
(defmethod clang-previous-decl ((obj new-clang-ast))
  (aget :PreviousDecl (new-clang-ast-attrs obj)))

(declaim (special *canonical-string-table* *current-line*))

(declaim (special *soft*))

;;; We cache the last lookup of certain slots, so that repeat values
;;; can be omitted in the json.  A special accessor maintains this cache

(declaim (special *aget-cache*))
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

;;; Question on this: are IDs unique between files?
;;; Or do we need keys that include file names?

;;; Normalize a file for "include" processing vs. a list of
;;; include dirs.

(defun normalize-include-dirs (include-dirs)
  "Put include directory strings into normal form"
  (setf include-dirs (remove "" include-dirs :test #'string=))
  (mapcar (lambda (s)
            (if (eql (elt s (1- (length s))) #\/)
                s
                (concatenate 'string s "/")))
          include-dirs))

(defun normalize-file-for-include (file-string original-dir include-dirs)
  "Returns the normalized version of file-string relative to the original
directory and the include-dirs, nd a value that is T if the string should
be in #include \"...\", NIL if in #include <...>"
  (cond
    ;; Empty string is erroneous
    ((string= file-string "")
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
     (let ((file-len (length file-string)))
       (flet ((%match (ind)
                "Attempt to match FILE-STRING against IND.  Returns
the match length if sucessful, NIL if not."
                (let ((ind-len (length ind)))
                  (when (< ind-len file-len)
                    (let ((mm (mismatch ind file-string)))
                      (when (= mm ind-len) ind-len))))))
         (let ((om (when original-dir (%match original-dir))))
           ;; (format t "om = ~a~%" om)
           (if om
               (values (concatenate 'string "\"" (subseq file-string om) "\"") t)
               (let ((max-match 0) (dir nil))
                 (iter (for ind in include-dirs)
                       (let ((mm (%match ind)))
                         ;; (format t "ind = ~a, mm = ~a~%" ind mm)
                         (when (and mm (> mm max-match))
                           (setf max-match mm
                                 dir ind))))
                 (if dir
                     (values (concatenate 'string "<" (subseq file-string max-match) ">")
                             nil)
                     (values (concatenate 'string "\"" file-string "\"") t))))))))))

(defgeneric original-directory (obj))
(defmethod original-directory ((obj source))
  (let ((file (original-file obj)))
    (when file
      (namestring (make-pathname :defaults (pathname file)
                                 :name nil :type nil)))))

;; (defvar *symbol-table* (make-hash-table :test #'equal)
;;   "Table mapping id values to the corresponding nodes.  This should
;; be bound for each clang program.")

(defun canonicalize-string (str)
  (if (boundp '*canonical-string-table*)
      (let ((table *canonical-string-table*))
        (or (gethash str table)
            (setf (gethash str table) str)))
      str))

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

;;; Json conversion

(defun clang-convert-json-for-file (json file genome-len)
  ;; The aget cache is used to record values of elided
  ;; json attributes, that are assumed to be equal to the previous
  ;; occurrence of such an attribute.  This res the json
  ;; be converted left to right.  cl-json produces alists
  ;; in the same order they appear in the json, fortunately.
  (let* ((*aget-cache* nil)
         (ast (clang-convert-json json :file file)))
    (unless (ast-attr ast :range)
      (let ((range (make-new-clang-range :begin 0 :end genome-len)))
        (setf (ast-attr ast :range) range)))
    ast))

(defun clang-convert-json (json &key inner file)
  "Convert json data in list form to data structures using NEW-CLANG-AST"
  (declare (ignorable file))
  (typecase json
    (null nil)
    (cons
     (let* ((json-kind (aget :kind json))
            (json-kind-symbol (if json-kind
                                  (json-kind-to-keyword json-kind)
                                  :unknown)))
       (unless (keywordp json-kind-symbol)
         (error "Cannot convert ~a to a json-kind keyword" json-kind))
       (let ((obj (j2ck json json-kind-symbol)))
         (when inner
           (let ((id (new-clang-ast-id obj)))
             (pushnew obj (gethash id (symbol-table *soft*)))))
         obj)))
    (string (canonicalize-string json))
    (t json)))

(defgeneric j2ck (json json-kind-symbol)
  (:documentation "Generic function for converting a json node
to a clang-node.  The purpose of this is to enable dispatch
on json-kind-symbol when special subclasses are wanted."))

(defmethod j2ck (json json-kind-symbol)
  (let ((obj (make-new-clang-ast)))
    (store-slots obj json)))

(defun alist-subsumed (al1 al2)
  (iter (for (k . v) in al1)
        (always (or (equal v "")
                    (equal v (aget k al2))))))

(defmethod j2ck :around (json json-kind-symbol)
  (let ((obj (call-next-method)))
    (when obj
      (let ((id (new-clang-ast-id obj)))
        (when (and id (ast-attr obj :loc))
          (push obj (gethash id (symbol-table *soft*))))))
    obj))

(defmethod j2ck :around (json (json-kind-symbol (eql :forstmt)))
  ;; Clang's json has {} for missing for clauses
  ;; cl-json converts these to NIL.  Just remove then,
  ;; as the old clang front end does.
  (let ((obj (call-next-method)))
    (setf (ast-children obj) (remove nil (ast-children obj)))
    obj))

(defgeneric store-slots (obj json)
  (:documentation "Store values in the json into obj.
Return the object, or another object to be used in
its place."))

(defmethod store-slots ((obj new-clang-ast) (json list))
  (dolist (x json)
    (let ((slot (car x))
          (value (cdr x)))
      (setf obj (store-slot obj slot value))))
  obj)

(defgeneric store-slot (obj slot value)
  (:documentation "Converts json VALUE into appropriate internal
form for SLOT, and stores into OBJ.  Returns OBJ or its replacement."))

(defmethod store-slot ((obj new-clang-ast) (slot symbol) value)
  ;; Generic case
  (let ((attrs (new-clang-ast-attrs obj)))
    (assert (null (aget slot attrs)) () "Duplicate slot ~a" slot)
    (let ((converted-value (convert-slot-value obj slot value)))
      (when converted-value
        (setf (new-clang-ast-attrs obj)
              (append attrs `((,slot . ,converted-value)))))))
  obj)

(defmethod store-slot ((obj new-clang-ast) (slot (eql :kind)) value)
  (setf (new-clang-ast-class obj) (json-kind-to-keyword value))
  obj)

;; (defmethod store-slot ((obj new-clang-ast) (slot (eql :id)) value)
;;   (call-next-method))

(defmethod store-slot ((obj new-clang-ast) (slot (eql :definitiondata)) value)
  ;; Do not translate this attribute for now
  obj)

(defmethod store-slot ((obj new-clang-ast) (slot (eql :id)) value)
  (setf (new-clang-ast-id obj) (convert-slot-value obj slot value))
  obj)

(defmethod store-slot ((obj new-clang-ast) (slot (eql :inner)) value)
  (let ((children (mapcar (lambda (o) (clang-convert-json o :inner t)) value)))
    ;; (format t "STORE-SLOT on ~a, :INNER with ~A children~%" value (length value))
    (setf (new-clang-ast-children obj) children))
  obj)

(defmethod store-slot ((obj new-clang-ast) (slot (eql :array_filler)) value)
  (let ((children (mapcar (lambda (o) (clang-convert-json o :inner t)) value)))
    ;; (setf (ast-attr obj :array-filler) t)
    (setf (new-clang-ast-children obj) children))
  obj)

(defgeneric convert-slot-value (obj slot value)
  (:documentation "Convert a value in the context of a specific slot.  Return of
NIL indicates no value."))

(defmethod convert-slot-value ((obj new-clang-ast) (slot symbol) value)
  ;; Default to a context-independent conversion
  (clang-convert-json value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :referenceddecl)) value)
  (clang-convert-json value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :decl)) value)
  (clang-convert-json value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :templateparams)) value)
  (mapcar #'clang-convert-json value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :id)) value)
  (read-c-integer value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :previousdecl)) value)
  (read-c-integer value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :name)) value)
  (and (not (equal value "")) (call-next-method)))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :tagused)) value)
  (cond
    ((equal value "struct") :struct)
    ((equal value "union") :union)
    (t (call-next-method))))

;; More conversions

(defun convert-loc-json (loc-json)
  "Special handler for values of loc attributes"
  (if (aget :spellingloc loc-json)
      (convert-macro-loc-json loc-json)
      (let ((col (aget :col loc-json))
            (file (cached-aget :file loc-json))
            (line (cached-aget :line loc-json))
            (tok-len (aget :toklen loc-json)))
        (when (stringp file)
          (setf file (canonicalize-string file)))
        (when (or col file line)
          (make-new-clang-loc :col col :file file :line line :tok-len tok-len)))))

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
    (if (or begin end)
        (make-new-clang-range :begin begin :end end))))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :loc)) value)
  (declare (ignorable obj slot))
  (convert-loc-json value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :range)) value)
  (declare (ignorable obj slot))
  (convert-range-json value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :valuecategory)) (value string))
  (intern (string-upcase value) :keyword))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :type)) (value list))
  (let ((qual-type (aget :qualtype value))
        (desugared-qual-type (aget :desugaredqualtype value)))
    ;; These should be strings, but convert anyway to canonicalize them
    (let* ((qual (clang-convert-json qual-type))
           (desugared (clang-convert-json desugared-qual-type))
           (key (cons qual desugared))
           (table (type-table *soft*)))
      (or (gethash key table)
          (setf (gethash key table)
                (make-new-clang-type :qual qual :desugared desugared))))))

(defgeneric clang-to-list (x)
  (:documentation "Convert clang tree structure to list form"))

(defun clang-alist-to-list (alist)
  (mapcar (lambda (p)
            (cons (car p) (clang-to-list (cdr p))))
          alist))

(defmethod clang-to-list ((x new-clang-ast))
  `((:id . ,(int-to-c-hex (new-clang-ast-id x)))
    (:kind . ,(new-clang-ast-class x))
    ,@(clang-alist-to-list (new-clang-ast-attrs x))
    ,@(let ((inner (mapcar #'clang-to-list (new-clang-ast-children x))))
        (when inner
          `((:inner ,@inner))))))

(defmethod clang-to-list ((x t)) x)
(defmethod clang-to-list ((x list))
  (mapcar #'clang-to-list x))

(defmethod clang-to-list ((r new-clang-range))
  (let ((begin (new-clang-range-begin r))
        (end (new-clang-range-end r)))
    `(,@(when begin `((:begin . ,(clang-to-list begin))))
        ,@(when end `((:end . ,(clang-to-list end)))))))

(defmethod clang-to-list ((l new-clang-loc))
  `((:col . ,(new-clang-loc-col l))
    (:file . ,(new-clang-loc-file l))
    (:line . ,(new-clang-loc-line l))))

(defmethod clang-to-list ((ct new-clang-type))
  (let ((qual (new-clang-type-qual ct))
        (ds (new-clang-type-desugared ct)))
    `(,@(when qual `((:qual . ,qual)))
        ,@(when ds `((:desugared-type . ,ds))))))

;;; Testing code

(defun dump-symbol-table ()
  "Print the contents of the symbol table"
  (let ((entries nil))
    (maphash (lambda (k v) (push (cons k v) entries))
             (symbol-table *soft*))
    (setf entries (sort entries #'< :key #'car))
    (loop for (k . v) in entries
       do (format t "~a ==> ~s~%"
                  (int-to-c-hex k)
                  (clang-to-list v))))
  (values))

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


;;;;

(declaim (special *make-statement-fn*))

(defun make-statement-new-clang (class syn-ctx children
                                 &key full-stmt guard-stmt opcode
                                   aux-data
                                   &allow-other-keys)
  "Create a statement AST of the NEW-CLANG type.

* CLASS class name of the AST node
* SYN-CTX surrounding syntactic context of the AST node
* CHILDREN children of the AST node
* FULL-STMT boolean indicating if the AST represents a complete statement
* GUARD-STMT  boolean indicating if the AST is a control-flow predicate
* OPCODE name of the operation for Unary/BinaryOp AST nodes

Other keys are allowed but are silently ignored.
"
  (let ((attrs nil))
    (macrolet ((%push (a &aux (k (intern (string a) :keyword)))
                 `(when ,a (push (cons ,k ,a) attrs))))
      (%push full-stmt)
      (%push guard-stmt)
      (%push opcode))
    (make-new-clang-ast
     :path nil
     :children children
     :syn-ctx syn-ctx
     :class class
     :attrs attrs
     :children children
     :aux-data aux-data)))

;;; For other make- functions, see clang.lisp


;;;; Massaging the AST to put it into the right form

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

(defmethod remove-asts-if (ast fn) ast)

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
  ;; Implicit ASTs are introduced, for example, in declarations
  ;; of builtin functions that omit argument types.
  (remove-asts-if ast #'ast-is-implicit)
  ast)

(defun remove-asts-in-classes (ast classes)
  (remove-asts-if ast (lambda (o) (member (ast-class o) classes))))


;;;; Invocation of clang to get json

(defmethod clang-json ((obj new-clang) &key &allow-other-keys)
  (with-temp-file-of (src-file (ext obj)) (genome obj)
                     (let ((cmd-fmt "~a -cc1 -ast-dump=json ~{~a~^ ~} ~a")
                           (genome-len (length (genome obj)))
                           (cbin (compiler obj)))
                       (unwind-protect
                            (multiple-value-bind (stdout stderr exit)
                                (let ((*trace-output* *standard-output*))
                                  (shell cmd-fmt
                                         cbin
                                         (flags obj)
                                         src-file))
                              (when (find exit '(131 132 134 136 139))
                                (error
                                 (make-condition 'mutate
                                                 :text (format nil "~a core dump with ~d, ~s"
                                                               cbin exit stderr)
                                                 :obj obj)))
                              (unless (zerop exit)
                                (error
                                 (make-condition 'mutate
                                                 :text (format nil "~a exit ~d~%cmd:~s~%stderr:~s"
                                                               cbin exit
                                                               (format nil cmd-fmt
                                                                       cbin
                                                                       (flags obj)
                                                                       src-file)
                                                               stderr)
                                                 :obj obj)))
                              (values (let ((*read-default-float-format* 'long-float)
                                            (json:*json-identifier-name-to-lisp* #'string-upcase))
                                        (decode-json-from-string stdout))
                                      src-file
                                      genome-len))))))

;;; Offsets into the genome

;; Vector giving offsets for the start of each line
;; This is obtained from a sw object using genome-line-offsets
;; (declaim (special *offsets*))

(defun ncmlimae (x)
  (new-clang-macro-loc-is-macro-arg-expansion x))


(defgeneric offset (obj))
(defmethod offset (obj) nil)
(defmethod offset ((obj new-clang-loc))
  (let ((line (new-clang-loc-line obj))
        (col (new-clang-loc-col obj)))
    (when (and line col)
      (+ (elt (genome-line-offsets *soft*) (1- line)) col -1))))
(defmethod offset ((obj new-clang-macro-loc))
  (let ((spelling-loc (new-clang-macro-loc-spelling-loc obj))
        (expansion-loc (new-clang-macro-loc-expansion-loc obj)))
    (when (typep expansion-loc 'new-clang-loc)
      (if ;; (new-clang-macro-loc-is-macro-arg-expansion obj)
       (ncmlimae obj)
       (offset spelling-loc)
       (offset expansion-loc)))))
(defmethod offset ((obj integer)) obj)

(defgeneric all-offsets (obj))
(defmethod all-offsets ((obj new-clang-ast))
  (append
   (let ((range (aget :range (new-clang-ast-attrs obj))))
     (all-offsets range))
   (reduce #'append
           (ast-children obj)
           :key #'all-offsets
           :initial-value nil)))
(defmethod all-offsets ((obj new-clang-range))
  (append (all-offsets (new-clang-range-begin obj))
          (all-offsets (new-clang-range-end obj))))
(defmethod all-offsets ((obj new-clang-loc))
  (awhen (offset obj)
         (list it)))
(defmethod all-offsets ((offset integer)) (list offset))
(defmethod all-offsets (x) nil)

(defgeneric begin-offsets (obj)
  (:documentation "List of unique BEGIN offsets in the AST"))
(defmethod begin-offsets ((obj new-clang-ast))
  (append
   (let ((range (ast-attr obj :range)))
     (begin-offsets range)
     (reduce #'append
             (ast-children obj)
             :key #'begin-offsets
             :initial-value nil))))
(defmethod begin-offsets ((obj new-clang-range))
  (begin-offsets (new-clang-range-begin obj)))
(defmethod begin-offsets ((obj new-clang-loc))
  (awhen (offset obj)
         (list it)))
(defmethod begin-offsets ((offset integer)) (list offset))

(defgeneric end-offsets (obj)
  (:documentation "List of unique END offsets in the AST"))
(defmethod end-offsets ((obj new-clang-ast))
  (append
   (let ((range (ast-attr obj :range)))
     (end-offsets range)
     (reduce #'append
             (ast-children obj)
             :key #'end-offsets
             :initial-value nil))))
(defmethod end-offsets ((obj new-clang-range))
  (end-offsets (new-clang-range-end obj)))
(defmethod end-offsets ((obj new-clang-loc))
  (awhen (offset obj)
         (list it)))
(defmethod end-offsets ((offset integer)) (list offset))

(defgeneric begin-offset (obj))
(defmethod begin-offset (obj) nil)
(defmethod begin-offset ((obj new-clang-ast))
  (begin-offset (aget :range (new-clang-ast-attrs obj))))
(defmethod begin-offset ((obj new-clang-range))
  (offset (new-clang-range-begin obj)))

(defgeneric end-offset (obj))
(defmethod end-offset (obj) nil)
(defmethod end-offset ((obj new-clang-ast))
  (end-offset (aget :range (new-clang-ast-attrs obj))))
(defmethod end-offset ((obj new-clang-range))
  (offset (new-clang-range-end obj)))

(defun unique-offsets (offsets)
  (sort (remove-duplicates offsets) #'<))

(defun all-unique-offsets (sw obj)
  (unique-offsets
   (let ((*soft* sw))
     (all-offsets obj))))

(defgeneric tok-len (x)
  (:method ((x new-clang-loc)) (new-clang-loc-tok-len x))
  (:method ((x new-clang-macro-loc))
    (if (not (new-clang-macro-loc-is-macro-arg-expansion x))
        (tok-len (new-clang-macro-loc-spelling-loc x))
        (tok-len (new-clang-macro-loc-expansion-loc x))))
  (:method ((x integer)) 0)
  (:method (x) nil))

;;; Compute beginning, ending offsets for an ast, other things
;;; The end offset is one past the last character in the ast-text
;;; for the ast
(defgeneric begin-and-end-offsets (x))
(defmethod begin-and-end-offsets ((obj new-clang-ast))
  (let ((range (ast-attr obj :range)))
    (begin-and-end-offsets range)))
(defmethod begin-and-end-offsets ((obj new-clang-range))
  (let ((end (new-clang-range-end obj)))
    (values (begin-offset obj)
            (when end
              (if-let ((end-offset (offset end))
                       (tok-len (tok-len end)))
                (+ end-offset tok-len))))))
(defmethod begin-and-end-offsets ((obj null)) (values nil nil))

(defun extended-end-offset (x)
  (nth-value 1 (begin-and-end-offsets x)))

;;; Given a list of unique offsets for an AST, and
;;; the AST, build an alist of (offset . node) pairs, the
;;; nodes corresponding to those offsets.  Each should be
;;; the 'last' (in a preodered traversal) node with that
;;; beginning offset

(defun sorted-list-p (list &optional (cmp-fn #'<=))
  (every cmp-fn list (cdr list)))

(defun compute-last-nodes-with-offsets (ast)
  (let ((node-stack nil))
    (flet ((%add (a)
             (let ((offset (begin-offset a)))
               (when offset
                 (if (eql offset (caar node-stack))
                     (setf (cdar node-stack) a)
                     (push (cons offset a) node-stack))))))
      (mapc-ast ast #'%add))
    (nreverse node-stack)))


(defun decorate-ast-with-strings (sw ast) ;; get ast from sw?
  (let* ((*soft* sw)
         (genome (genome sw)))
    (flet ((%decorate (a)
             ;; At ast node A, split the parts of the source
             ;; that are not in the children into substrings
             ;; that are placed between the children.  Do not
             ;; place strings for children for whom offsets
             ;; cannot be computed
             ;; (format t "Decorating ~a~%" a)
             (let ((children (ast-children a)))
               (multiple-value-bind (begin end)
                   (begin-and-end-offsets a)
                 ;; (format t "B/E offsets of ~a: ~a ~a~%" a begin end)
                 (when (and begin end)
                   (let ((i begin))
                     (setf (ast-children a)
                           (nconc
                            (iter (for c in children)
                                  (when (ast-p c)
                                    (multiple-value-bind (cbegin cend)
                                        (begin-and-end-offsets c)
                                      (when cbegin
                                        (assert (>= cbegin i) ()
                                                "Offsets out of order: i = ~a, cbegin = ~a, c = ~a, range = ~a"
                                                i cbegin c
                                                (ast-attr c :range))
                                        ;; (format t "Collecting ~a to ~a for ~a~%" i cbegin c)
                                        (collect (subseq genome i cbegin))
                                        (setf i cend))))
                                  (collect c))
                            (progn
                              ;; (format t "Collecting ~a to ~a at end of ~a~%" i end a)
                              (list (subseq genome i end)))))))))))
      (map-ast ast #'%decorate)))
  ast)

(defun fix-ancestor-ranges (sw ast)
  "Normalize the ast so the range of each node is a superset
of the ranges of its children"
  (let ((*soft* sw)
        changed?)
    (flet ((%normalize (a)
             (multiple-value-bind (begin end)
                 (begin-and-end-offsets a)
               (let ((min-begin begin)
                     (max-end end))
                 (iter (for c in (ast-children a))
                       (when (ast-p c)
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
                   ;; (format t "Expanding range of ~a from (~a,~a) to (~a,~a)~%" a begin end min-begin max-end)
                   (setf changed? t)
                   (setf (ast-attr a :range)
                         (make-new-clang-range :begin min-begin :end max-end)))))))
      ;; Fixpoint for normalization of ranges
      (loop
         (setf changed? nil)
         (map-ast ast #'%normalize)
         (map-ast-postorder ast #'%normalize)
         (unless changed? (return ast))))))

(defun combine-overlapping-siblings (sw ast)
  "Group sibling nodes with overlapping or out of order
ranges into 'combined' nodes.  Warn when this happens."
  (let ((*soft* sw)
        (genome (genome sw)))
    (declare (ignorable genome))
    (flet ((%check (a)
             #+cos-debug (format t "Enter %check on ~a~%" a)
             (let ((end 0)
                   changed? accumulator)
               (flet ((%combine ()
                        (case (length accumulator)
                          (0)
                          (1 (list (pop accumulator)))
                          (t
                           (let ((new-begin (reduce #'min accumulator :key #'begin-offset))
                                 (new-end (reduce #'max accumulator :key #'extended-end-offset)))
                             (prog1
                                 (if (eql new-begin new-end)
                                     (progn
                                       #+cos-debug (format t "No combination needed~&")
                                       accumulator)
                                     (progn
                                       (setf changed? t)
                                       #+cos-debug
                                       (progn
                                         (format t "Combining ~a overlapping asts over [~a,~a):~%"
                                                 (length accumulator) new-begin new-end)
                                         (format t "Combined asts are:~%~{~a~%~}" accumulator)
                                         (format t "~a~%" (subseq genome new-begin new-end))
                                         (format t "------------------------------------------------------------~%"))
                                       (list (make-new-clang-ast
                                              :class :combined
                                              :attrs `((:range . ,(make-new-clang-range
                                                                   :begin new-begin
                                                                   :end new-end))
                                                       (:subsumed . ,accumulator))))))
                               (setf accumulator nil)))))))
                 (let ((new-children
                        (append
                         (iter (for c in (ast-children a))
                               #+cos-debug (format t "end = ~a~%" end)
                               (multiple-value-bind (cbegin cend)
                                   (begin-and-end-offsets c)
                                 (if (and cbegin end cend (< cbegin end))
                                     (progn
                                       #+cos-debug
                                       (when accumulator
                                         (format t "Adding (~a,~a):~%~a~&" cbegin cend
                                                 (subseq genome cbegin cend)))
                                       (setf accumulator (append accumulator (list c))
                                             end (max end cend)))
                                     (progn
                                       #+cos-debug
                                       (format t "No overlap: cbegin = ~a, cend = ~a~%"
                                               cbegin cend)
                                       (appending (%combine))
                                       (setf accumulator (list c)
                                             end cend)))))
                         (%combine))))
                   (when changed?
                     #+cos-debug (format t "Old-children: ~a~%New-children: ~a~%"
                                         (ast-children a)
                                         new-children)
                     (setf (ast-children a) new-children)))))
             #+cos-debug (format t "Leave %check on ~a~%" a)))
      (map-ast ast #'%check))))

(defmethod update-asts ((obj new-clang))
  ;; Port of this method from clang.lsp, for new class
  (let ((*soft* obj))
    (with-slots (ast-root genome includes) obj
      (unless genome     ; get genome from existing ASTs if necessary
        ;; (clrhash (symbol-table *soft*))
        (setf genome (genome obj)
              ast-root nil))
      (flet ((%debug (s a &optional (f #'ast-class))
               (declare (ignorable s a f))
               #+nil
               (progn
                 (format t "After ~a:~%" s)
                 (dump-ast-val a f))
               nil)
             (%p (o)
               (let ((r (ast-range o))) (list r (begin-offset r) (end-offset r)))))
        (let ((*canonical-string-table* (make-hash-table :test 'equal)))
          (multiple-value-bind (json tmp-file genome-len)
              (clang-json obj)
            (setf (tmp-file obj) tmp-file)
            (let* ((raw-ast (clang-convert-json-for-file json tmp-file genome-len)))
              (%debug 'clang-convert-json-for-file raw-ast)
              ;; Store name -> def mappings
              (maphash (lambda (k v)
                         (declare (ignore k))
                         (dolist (a v)
                           (when (and a (ast-name a))
                             (push a (gethash (ast-name a) (name-symbol-table obj))))))
                       (symbol-table obj))
              (let ((ast (remove-non-program-asts raw-ast tmp-file)))
                (%debug 'remove-non-program-asts ast)
                (remove-asts-in-classes
                 ast '(:fullcomment :textcomment :paragraphcomment))
                (mark-macro-expansion-nodes ast tmp-file)
                (%debug 'mark-macro-expansion-nodes ast
                        #'(lambda (o) (let ((r (ast-range o)))
                                        (list r (begin-offset r) (end-offset r)
                                              (is-macro-expansion-node o)))))
                (encapsulate-macro-expansions ast)
                (%debug 'encapsulate-macro-expansions ast #'%p)
                (fix-ancestor-ranges obj ast)
                (%debug 'fix-ancestor-ranges ast #'%p)
                (combine-overlapping-siblings obj ast)
                (%debug 'combine-overlapping-siblings ast #'%p)
                (decorate-ast-with-strings obj ast)
                (compute-full-stmt-attrs ast)
                (compute-guard-stmt-attrs ast)
                (compute-syn-ctxs ast)
                (setf ast-root (sel/sw/parseable::update-paths (fix-semicolons ast))
                      genome nil)
                obj))))))))

;;; Macro expansion code

(defgeneric mark-macro-expansion-nodes (a file)
  (:documentation
   "Mark the nodes of an AST that were produced by macroexpansion.
These will be recognized because their spelling-loc lines precede the
line currently being processed.  The nodes are marked with attribute
:from-macro"))

(defmethod mark-macro-expansion-nodes (a file)
  (let ((*current-line* 0))
    (map-ast a #'(lambda (x) (mark-macro-expansion-at-node x file)))))

(defun mark-macro-expansion-at-node (a file)
  "Determine if this node is a macro expansion.  Possibly advance
*current-line* if it is not."
  (let* ((range (ast-attr a :range))
         (begin (and range (new-clang-range-begin range)))
         (end (and range (new-clang-range-end range))))
    ;; (format t "range = ~a, begin = ~a, end = ~a~%" range begin end)
    (cond
      ((and (typep begin 'new-clang-macro-loc)
            (typep end 'new-clang-macro-loc)
            (or #+nil (and (not (new-clang-macro-loc-is-macro-arg-expansion end))
                           (progn (format t "isMacroArgExpansion is false~%")
                                  t))
                (and (not (equal file (ast-file a t)))
                     (progn ;; (format t "file = ~a, (ast-file a t) = ~a~%" file (ast-file a t))
                       t))
                (and (< (new-clang-loc-line (new-clang-macro-loc-spelling-loc begin))
                        *current-line*)
                     (progn #+nil
                            (format t "(new-clang-loc-line (new-clang-macro-loc-spelling-loc begin)) = ~a, *current-line* = ~a~%"
                                    (new-clang-loc-line (new-clang-macro-loc-spelling-loc begin))
                                    *current-line*)
                            t))))
       ;; It's from a macro, mark it
       ;; (format t "Marking: ~a~%" a)
       (setf (ast-attr a :from-macro) t))
      ((typep begin 'new-clang-loc)
       (setf *current-line* (max *current-line* (or (new-clang-loc-line begin) 0))))
      ((typep begin 'new-clang-macro-loc)
       (if-let ((spelling-loc (new-clang-macro-loc-spelling-loc begin)))
         (if-let ((begin-line (new-clang-loc-line spelling-loc)))
           (setf *current-line* (max *current-line* begin-line))))))))

(defun encapsulate-macro-expansions (a)
  "Given an AST marked with :from-macro annotations, collapse macros into macro
nodes."
  (map-ast a #'encapsulate-macro-expansions-below-node))

(defun is-macro-expansion-node (a)
  (ast-attr a :from-macro))

(defun macro-loc-expansion-to-loc (mloc)
  (typecase mloc
    (new-clang-macro-loc (new-clang-macro-loc-expansion-loc mloc))
    (t mloc)))

(defun macro-loc-spelling-to-loc (mloc)
  (typecase mloc
    (new-clang-macro-loc (new-clang-macro-loc-spelling-loc mloc))
    (t mloc)))

(defgeneric macro-range-to-non-macro-range/expansion (r)
  (:method ((r new-clang-range))
    (let* ((begin (macro-loc-expansion-to-loc (new-clang-range-begin r)))
           (end (macro-loc-expansion-to-loc (new-clang-range-end r))))
      (make-new-clang-range :begin begin :end end)))
  (:method (r) r))

(defgeneric macro-range-to-non-macro-range/spelling (r)
  (:method ((r new-clang-range))
    (let* ((begin (macro-loc-spelling-to-loc (new-clang-range-begin r)))
           (end (macro-loc-spelling-to-loc (new-clang-range-end r))))
      (make-new-clang-range :begin begin :end end)))
  (:method (r) r))

(defun encapsulate-macro-expansions-below-node (a)
  (assert (not (is-macro-expansion-node a)))
  (let ((changed? nil))
    (flet ()
      #+nil
      ((%convert (c)
                 (format t "Enter %CONVERT on ~a~%" c)
                 (if (and (ast-p c) (is-macro-expansion-node c))
                     (let* ((macro-args (macro-expansion-arg-asts c)))
                       (let ((obj (make-new-clang-ast
                                   :class :macroexpansion
                                   :children macro-args)))
                         ;; (format t "Create :MACROEXPANSION node with children ~a~%" macro-args)
                         ;; Create a non-macro loc for this node
                         (setf (ast-range obj)
                               (macro-range-to-non-macro-range/expansion (ast-range c))
                               changed? t)
                         obj))
                     c)))
      (setf (ast-range a) (macro-range-to-non-macro-range/expansion (ast-range a)))
      ;; Must combine subsequences of children that are macro expansion nodes
      ;; and have the same spelling loc begin into a single macroexpansion node
      (let* ((children (ast-children a))
             (new-children
              (iter (while children)
                    (if (or (not (ast-p (car children)))
                            (not (is-macro-expansion-node (car children))))
                        (collect (pop children))
                        (collect
                         (let ((macro-child-segment
                                (iter (collect (pop children))
                                      (while (and children
                                                  (ast-p (car children))
                                                  (is-macro-expansion-node (car children)))))))
                           (let ((macro-args (macro-expansion-arg-asts macro-child-segment)))
                             ;; (format t "Create :MACROEXPANSION node with children ~a~%" macro-args)
                             (let ((obj (make-new-clang-ast
                                         :class :macroexpansion
                                         :children macro-args)))
                               (setf (ast-range obj)
                                     (macro-range-to-non-macro-range/expansion (ast-range (car macro-child-segment)))
                                     changed? t)
                               obj))))))))
        (when changed?
          (shiftf (ast-attr a :old-children) (ast-children a) new-children)
          new-children)))))

(defun macro-expansion-arg-asts (asts)
  "Search down below a for the nodes that are not marked with :from-macro.
They, from left to right, become the children of the macro node."
  (dolist (a asts) (assert (ast-attr a :from-macro)))
  (let ((children nil))
    (labels ((%traverse (x)
               (when (new-clang-ast-p x)
                 (if (ast-attr x :from-macro)
                     (mapc #'%traverse (ast-children x))
                     (push x children)))))
      (dolist (a asts)
        (mapc #'%traverse (ast-children a))))
    (nreverse children)))


;;; Reference-related code

(defgeneric ast-referenced-obj (obj)
  (:documentation "The object referenced by the reference attribute
of OBJ")
  (:method ((obj new-clang-ast))
    (let ((rd (ast-attr obj :ReferencedDecl)))
      (when rd
        (let ((id (new-clang-ast-id rd)))
          (when id
            (let ((defs (gethash id (symbol-table *soft*))))
              (assert (= (length defs) 1) ()
                      "ID = 0x~x, NAME = ~a, defs=~a, symbol-table=~a~%"
                      id (ast-name rd) defs (symbol-table *soft*))
              (car defs) ;; (ast-file (car defs))
              )))))))


;;; Reimplementations of ast-* functions for nodes

(defmethod map-ast ((ast new-clang-ast) fn)
  (case (ast-class ast)
    (:combine (mapcar {map-ast _ fn} (ast-attr ast :subsumed)))
    (t (call-next-method))))

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

;; returns ast nodes, not strings
(defmethod ast-args ((obj new-clang-ast))
  (mapcar
   (lambda (o) (list o (ast-type o)))
   (remove-if-not (ast-is-class-fun :ParmVar)
                  (ast-children obj))))

(defmethod ast-declares ((obj new-clang-ast))
  (case (ast-class obj)
    (:DeclStmt
     (reduce #'append (ast-children obj)
             :key #'ast-declares :initial-value nil))
    ((:ParmVar :Function :Var)
     (when (ast-name obj)
       (list obj)))
    ((:Combined)
     (reduce #'append (ast-attr obj :subsumed)
             :key #'ast-declares :initial-value nil))
    ;; More cases here
    (t nil)))

(defmethod ast-expr-type ((obj new-clang-ast))
  (ast-attr obj :type))

;; This field should be filled in by a pass
;; that marks AST nodes that are full statements
;; (and that might not otherwise be)
(defmethod ast-full-stmt ((obj new-clang-ast))
  (ast-attr obj :fullstmt))

(defmethod full-stmt-p ((obj new-clang) (ast new-clang-ast))
  (declare (ignorable obj))
  (ast-full-stmt ast))
#|
(case (ast-class obj)
((:Function :ReturnStmt :IfStmt :WhileStmt :ForStmt :SwitchStmt :NullStmt :DeclStmt) t)
(t (ast-attr obj :fullstmt))))
|#

;; This field should be filled in by a pass
;; that marks AST nodes that are guard statements
;; (and that might not otherwise be)
(defmethod ast-guard-stmt ((obj new-clang-ast))
  (ast-attr obj :guardstmt))

;;; 'includes' should be computed from the locations
;;; in the subtree.  Need logic to translate back to
;;; relative paths.

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defparameter *clang-decl-kinds*
    '(:AccessSpecDecl
      :BindingDecl
      :BlockDecl
      :BuiltinTemplateDecl
      :CapturedDecl
      :ClassScopeFunctionSpecializationDecl
      :ClassTemplateDecl
      :ClassTemplatePartialSpecializationDecl
      :ClassTemplateSpecializationDecl
      :ConstructorUsingShadowDecl
      :CXXConstructorDecl
      :CXXConversionDecl
      :CXXDestructorDecl
      :CXXMethodDecl
      :CXXRecordDecl
      ;; :DeclRefExpr
      ;; :DeclStmt
      :DecltypeType
      :DecompositionDecl
      ;; :DependentScopeDeclRefExpr
      :EmptyDecl
      :EnumConstant ;; :EnumConstantDecl
      :Enum ;; :EnumDecl
      :Field ;; :FieldDecl
      :FileScopeAsmDecl
      :FriendDecl
      :Function ;; :FunctionDecl
      :FunctionTemplateDecl
      :ImplicitParamDecl
      :IndirectFieldDecl
      :Label ;; :LabelDecl
      :LinkageSpecDecl
      :NamespaceAliasDecl
      :NamespaceDecl
      :NonTypeTemplateParmDecl
      :ParmVar  ;; :ParmVarDecl
      :PragmaCommentDecl
      :Record  ;; :RecordDecl
      :StaticAssertDecl
      :TemplateTemplateParmDecl
      :TemplateTypeParmDecl
      :TranslationUnitDecl
      :TypeAliasDecl
      :TypeAliasTemplateDecl
      :Typedef ;; :TypedefDecl
      :UnresolvedUsingTypenameDecl
      :UnresolvedUsingValueDecl
      :UsingDecl
      :UsingDirectiveDecl
      :UsingPackDecl
      :UsingShadowDecl
      :Var ;; :VarDecl
      :VarTemplateDecl
      :VarTemplatePartialSpecializationDecl
      :VarTemplateSpecializationDecl)))

(defmethod ast-is-decl ((obj new-clang-ast))
  (case (ast-class obj)
    (#.*clang-decl-kinds* t)
    (t nil)))

(defmethod ast-opcode ((obj new-clang-ast))
  (or (ast-attr obj :opcode) ""))

(defmethod ast-ret ((obj new-clang-ast))
  (case (ast-class obj)
    (:Function
     (let ((type (new-clang-type-qual (ast-type obj))))
       (ret-type-of-function-type type)))
    ;; Others?
    (t nil)))

(defmethod ast-void-ret ((obj new-clang-ast))
  (equal "void" (ast-ret obj)))

(defun reference-decls-at-ast (a)
  (let ((rd (ast-attr a :referencedDecl)))
    (when rd (list rd))))

(defun ast-reference-decls (ast)
  (map-ast-sets ast #'reference-decls-at-ast :key #'new-clang-ast-id))

(defmethod ast-varargs ((obj new-clang-ast))
  ;; Should just be :FunctionDecl objects
  (ast-attr obj :variadic))

(defun ret-type-of-function-type (s)
  "Returns a string that is the return type of the function type
given in the string S. Return nil if the return type cannot be
determined."
  ;; This is grossly incomplete now, and will fail on some
  ;; hairy types
  (let ((pos (position #\( s)))
    (when (and pos (> pos 0))
      (string-right-trim " " (subseq s 0 (1- pos))))))

;;; Computes some attributes on new clang ast nodes

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

(defgeneric compute-full-stmt-attr (obj ancestors)
  (:documentation "Fills in the :FULLSTMT attribute on clang ast
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
          (setf (ast-attr obj :fullstmt) t))))
    obj))

(defun compute-full-stmt-attrs (ast)
  (map-ast-with-ancestors ast #'compute-full-stmt-attr))

(defgeneric compute-guard-stmt-attr (obj ancestors)
  (:documentation "Fills in the :GUARDSTMT attribute on clang
ast nodes, as needed")
  (:method ((obj new-clang-ast) ancestors)
    (when ancestors
      (let ((parent (car ancestors)))
        (when (not (is-single-line-stmt obj parent))
          (case (ast-class parent)
            ((:CapturedStmt :CompoundStmt :CXXCatchStmt :DoStmt
                            :ForStmt :IfStmt :SwitchStmt :WhileStmt)
             (setf (ast-attr obj :GuardStmt) t))))))
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
      (setf (ast-syn-ctx obj) syn-ctx))
    obj))

(defgeneric fix-var-syn-ctx (ast)
  (:documentation "Fix the syn-ctx of Var and ParmVar nodes")
  (:method ((obj new-clang-ast))
    (let ((prev nil)
          (prev-var? nil))
      (unless (eql (ast-class obj) :toplevel)
        (iter (for c in (ast-children obj))
              (when (ast-p c)
                (case (ast-class c)
                  ((:Var :ParmVar)
                   ;; This logic makes single element ParmVar lists
                   ;; be :Generic.  Weird, but that's what clang-mutate
                   ;; did
                   (when prev-var? (setf (ast-syn-ctx prev) :ListElt)
                         (setf (ast-syn-ctx  c) :FinalListElt))
                   (setf ;; (ast-syn-ctx c) :FinalListElt
                    prev c
                    prev-var? t))
                  (t (setf prev-var? nil prev nil)))))))
    obj))

(defun compute-syn-ctxs (ast)
  (map-ast-with-ancestors ast #'compute-syn-ctx)
  (map-ast ast #'fix-var-syn-ctx))

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

;;; Logic for extracting #include directives

(let ((scanner (cl-ppcre:create-scanner "^#include\\s+(\\S+)\\s*$")))
  (defun includes-in-string (str)
    "Returns a fresh list of the #include payloads in a string"
    (if (find #\Newline str)
        (mapcan #'includes-in-string
                (split-sequence #\Newline str
                                :remove-empty-subseqs t))
      ;;; Check if this line begins with #include
      ;;; If so, pull off the payload
        (multiple-value-bind (start end reg-begins reg-ends)
            (cl-ppcre:scan scanner str)
          (declare (ignore end))
          (when start
            (list (subseq str
                          (elt reg-begins 0)
                          (elt reg-ends 0))))))))


;;; Debugging code

(defgeneric dump-ast (ast print-fn))

(defmethod dump-ast ((ast ast) print-fn)
  (labels ((%r (a d)
             ;; (dotimes (i d) (format t " "))
             (funcall print-fn a d)
             (let ((d (1+ d)))
               (dolist (c (ast-children a))
                 (when (ast-p c) (%r c d))))))
    (%r ast 0)))

(defmethod dump-ast ((sw parseable) print-fn)
  (dump-ast (ast-root sw) print-fn))

(defgeneric dump-ast-with-parent (ast print-fn))

(defmethod dump-ast-with-parent ((ast ast) print-fn)
  (labels ((%r (a d p)
             (dotimes (i d) (format t " "))
             (funcall print-fn a p d)
             (let ((d (1+ d)))
               (dolist (c (ast-children a))
                 (when (ast-p c) (%r c d a))))))
    (%r ast 0 nil)))

(defmethod dump-ast-with-parent ((sw parseable) print-fn)
  (dump-ast-with-parent (ast-root sw) print-fn))

(defun dump-ast-classes (ast &optional (s *standard-output*))
  (flet ((%print-class (a d)
           (let ((class (ast-class a)))
             (dotimes (i d) (format s " "))
             (format s "~a~%" class))))
    (dump-ast ast #'%print-class)))

(defgeneric dump-ast-val (ast val-fn &optional s)
  (:method (ast val-fn &optional (s *standard-output*))
    (flet ((%print (a d)
             (let ((class (ast-class a)))
               (dotimes (i d) (format s " "))
               (format s "~a: ~a~%"
                       class
                       (funcall val-fn a)))))
      (dump-ast ast #'%print)))
  (:method :around ((sw new-clang) val-fn &optional s)
           (declare (ignorable s))
           (let ((*soft* sw))
             (ast-root sw)
             (call-next-method))))

(defgeneric dump-ast-val-p (ast val-fn &optional s)
  (:method (ast val-fn &optional (s *standard-output*))
    (flet ((%print (a p d)
             (let ((class (ast-class a)))
               (dotimes (i d) (format s " "))
               (format s "~a: ~a~%"
                       class
                       (funcall val-fn a p)))))
      (dump-ast-with-parent ast #'%print)))
  (:method :around ((sw new-clang) val-fn &optional s)
           (declare (ignorable s))
           (let ((*soft* sw))
             (ast-root sw)
             (call-next-method))))

(defgeneric dump-ast-to-list (ast fn)
  (:method ((ast ast) fn)
    (funcall fn ast (mapcar (lambda (a) (dump-ast-to-list a fn))
                            (remove-if-not #'ast-p (ast-children ast)))))
  (:method ((sw parseable) fn)
    (dump-ast-to-list (ast-root sw) fn)))

(defgeneric dump-ast-val-to-list (ast val-fn)
  (:method (ast val-fn)
    (dump-ast-to-list
     ast
     (lambda (a child-results)
       (let ((val (funcall val-fn a)))
         (list (ast-class a) val child-results))))))
