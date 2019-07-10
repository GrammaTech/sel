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
           :remove-non-program-asts))
(in-package :software-evolution-library/software/new-clang)
(in-readtable :curry-compose-reader-macros)

(declaim (optimize (debug 3)))

(define-software new-clang (clang-base genome-lines-mixin)
  ((stmt-asts :initarg :stmt-asts :reader stmt-asts
              :initform nil :copier :direct
              :type #+sbcl (list (cons keyword *) *) #-sbcl list
              :documentation
              "List of statement ASTs which exist within a function body.")
   ;; TODO: We should split non-statement ASTs into typedefs,
   ;;       structs/classes, and global variables, all of which should
   ;;       have different mutation types defined.  This needs more design.
   (non-stmt-asts :initarg :non-stmt-asts :reader non-stmt-asts
                  :initform nil :copier :direct
                  :type #+sbcl (list (cons keyword *) *) #-sbcl list
                  :documentation
                  "List of global AST which live outside of any function.")
   (functions :initarg :functions :reader functions
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
   (symbol-table
    :initarg :symbol-tanle :accessor symbol-table
    :initform (make-hash-table :test #'equal)
    :copier copy-hash-table
    :type hash-table
    :documentation "Map from IDs to objects")
   (include-dirs
    :initarg :include-dirs
    :initform nil
    :type list
    :copier :direct
    :documentation "List of directories that are where include files are searched for
(the -I arguments to the C compiler")
   ))))
(:documentation
 "C language (C, C++, C#, etc...) ASTs using Clang, C language frontend for LLVM.
   See http://clang.llvm.org/.  This is for ASTs from Clang 9+."))


;;; clang object creation

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
    (ast-attr ast :is-implicit)))

(defgeneric ast-is-class (ast key)
  (:method (ast class) nil)
  (:method ((ast new-clang-ast) (key symbol))
    (and (ast-p ast)
         (eql (ast-class ast) key))))

(defmethod ast-type ((ast new-clang-ast))
  (ast-attr ast :type))

(defun ast-is-class-fun (key)
  (lambda (c) (ast-is-class c key)))

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
    (flet ((%areplace (key val alist)
             (cons (cons key val) (remove key alist :key #'car))))
      (when full-stmt-p
        (setf attrs (%areplace :full-stmt full-stmt attrs)))
      (when guard-stmt-p
        (setf attrs (%areplace :guard-stmt guard-stmt attrs)))
      (when opcode-p
        (setf attrs (%areplace :opcode opcode attrs)))
      (make-new-clang-ast :path path :children children
                          :class class :attrs attrs :id id
                          :syn-ctx syn-ctx))))

(defmethod ast-class ((obj new-clang-ast))
  (new-clang-ast-class obj))
(defmethod ast-syn-ctx ((obj new-clang-ast))
  (new-clang-ast-syn-ctx obj))
(defmethod source-text ((ast new-clang-ast))
  (with-output-to-string (out)
    (mapc [{write-string _ out} #'source-text] (ast-children ast))))

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
(defstruct new-clang-type qual desugared)

;; Wrapper for values in referencedDecl attribute, when
;; storing them in (symbol-table *soft*)
(defstruct reference-entry obj)

(defgeneric ast-file (ast)
  (:documentation "The file name associated with an AST node")
  (:method ((ast new-clang-ast))
    (ast-file (ast-attr ast :range)))
  (:method ((range new-clang-range))
    (ast-file (new-clang-range-begin range)))
  (:method ((loc new-clang-loc))
    (new-clang-loc-file loc))
  (:method ((loc new-clang-macro-loc))
    (ast-file
     (if (new-clang-macro-loc-is-macro-arg-expansion loc)
         (new-clang-macro-loc-spelling-loc loc)
         (new-clang-macro-loc-expansion-loc loc))))
  (:method (obj) nil))

(defun json-kind-to-keyword (json-kind)
  (when (stringp json-kind)
    (let ((sym (intern (string-upcase json-kind) :keyword)))
      (case sym
        ((:FunctionDecl) :Function)
        ((:TranslationUnitDecl) :TopLevel)
        ((:ParmVarDecl) :ParmVar)
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

(defun normalize-file-for-include (file-string include-dirs)
  "Returns the normalized version of file-string relative to include-dirs,
and a value that is T if the string should be in #include \"...\",
NIL if in #include <...>"
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
     (let ((max-match 0) (dir nil)
           (file-len (length file-string)))
       (iter (for ind in include-dirs)
             (let ((ind-len (length ind)))
               (when (< ind-len file-len)
                 (let ((mm (mismatch ind file-string)))
                   (when (and (= mm ind-len)
                              (> ind-len max-match))
                     (setf max-match ind-len
                           dir ind))))))
       (if dir
           (values (subseq file-string max-match) nil)
           (values file-string t))))))

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
        (format stream "~a~@[ ~a~]" (ast-class obj) (ast-name obj)))))

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
    (make-new-clang-type :qual (clang-convert-json qual-type)
                         :desugared (clang-convert-json desugared-qual-type))))

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
    (if (and (= (length children)
                (length new-children))
             (every #'eql children new-children))
        ast
        (copy ast :children new-children))))

(defmethod remove-asts-if (ast fn) ast)

(defgeneric remove-non-program-asts (ast file)
  (:documentation "Remove ASTs from ast that are not from the
actual source file"))

(defun is-non-program-ast (a file)
  (and (typep a 'new-clang-ast)
       (or (ast-attr a :isimplicit)
           (let ((f (ast-file a)))
             (and f (not (equal file f)))))))

(defmethod remove-non-program-asts ((ast ast) (file string))
  (flet ((%non-program (a) (is-non-program-ast a file)))
    (remove-asts-if ast #'%non-program)))


;;;; Invocation of clang to get json

(defparameter *clang-binary*
  "/pdietz/clang9-installed/bin/clang"
  "This is clearly not the final word on this")

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
      (if (new-clang-macro-loc-is-macro-arg-expansion obj)
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
             (format t "Decorating ~a~%" a)
             (let ((children (ast-children a)))
               (multiple-value-bind (begin end)
                   (begin-and-end-offsets a)
                 (format t "B/E offsets of ~a: ~a ~a~%"
                         a begin end)
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
                                        (format t "Collecting ~a to ~a for ~a~%" i cbegin c)
                                        (collect (subseq genome i cbegin))
                                        (setf i cend))))
                                  (collect c))
                            (progn
                              (format t "Collecting ~a to ~a at end of ~a~%" i end a)
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
                   (format t "Expanding range of ~a from (~a,~a) to (~a,~a)~%"
                           a begin end min-begin max-end)
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
                                       ;; #+cos-debug
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
    (with-slots (ast-root genome) obj
      (unless genome     ; get genome from existing ASTs if necessary
        ;; (clrhash (symbol-table *soft*))
        (setf genome (genome obj)
              ast-root nil))
      (multiple-value-bind (json tmp-file genome-len)
          (clang-json obj)
        (let* ((raw-ast (clang-convert-json-for-file json tmp-file genome-len))
               (ast (remove-non-program-asts raw-ast tmp-file)))
          (mark-macro-expansion-nodes ast tmp-file)
          (encapsulate-macro-expansions ast)
          (format t "After ENCAPSULATE-MACRO-EXPANSIONS:~%")
          (dump-ast-val ast #'(lambda (o) (let ((r (ast-range o))) (list r (begin-offset r) (end-offset r)))))
          (fix-ancestor-ranges obj ast)
          (format t "After FIX-ANCESTOR-RANGES:~%")
          (dump-ast-val ast #'(lambda (o) (let ((r (ast-range o))) (list r (begin-offset r) (end-offset r)))))
          (combine-overlapping-siblings obj ast)
          (format t "After COMBINE-OVERLAPPING-SIBLINGS:~%")
          (dump-ast-val ast #'(lambda (o) (let ((r (ast-range o))) (list r (begin-offset r) (end-offset r)))))
          (decorate-ast-with-strings obj ast)
          (compute-full-stmt-attrs ast)
          (compute-guard-stmt-attrs ast)
          (setf ast-root ast)
          obj)))))

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
                (and (not (equal file (ast-file a)))
                     (progn (format t "file = ~a, (ast-file a) = ~a~%"
                                    file (ast-file a))
                            t))
                (and (< (new-clang-loc-line (new-clang-macro-loc-spelling-loc begin))
                        *current-line*)
                     (progn (format t "(new-clang-loc-line (new-clang-macro-loc-spelling-loc begin)) = ~a, *current-line* = ~a~%"
                                    (new-clang-loc-line (new-clang-macro-loc-spelling-loc begin))
                                    *current-line*)
                            t))))
       ;; It's from a macro, mark it
       (format t "Marking: ~a~%" a)
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

(defun macro-range-to-non-macro-range/expansion (r)
  (let* ((begin (macro-loc-to-loc (new-clang-range-begin r)))
         (end (macro-loc-to-loc (new-clang-range-end r))))
    (make-new-clang-range :begin begin :end end)))

(defun macro-range-to-non-macro-range/spelling (r)
  (let* ((begin (macro-loc-spelling-to-loc (new-clang-range-begin r)))
         (end (macro-loc-spelling-to-loc (new-clang-range-end r))))
    (make-new-clang-range :begin begin :end end)))

(defun encapsulate-macro-expansions-below-node (a)
  (assert (not (is-macro-expansion-node a)))
  (let ((changed? nil))
    (flet ((%convert (c)
             (if (and (ast-p c) (is-macro-expansion-node c))
                 (let* ((macro-args (macro-expansion-arg-asts c))
                        (obj (make-new-clang-ast
                              :class :macroexpansion
                              :children macro-args)))
                   ;; Create a non-macro loc for this node
                   (setf (ast-range obj)
                         (macro-range-to-non-macro-range/expansion (ast-range c))
                         changed? t)
                   obj)
                 c)))
      (setf (ast-range a) (macro-range-to-non-macro-range/spelling (ast-range a)))
      (let* ((new-children (mapcar #'%convert (ast-children a))))
        (when changed?
          (shiftf (ast-attr a :old-children) (ast-children a) new-children)
          new-children)))))

(defun macro-expansion-arg-asts (a)
  "Search down below a for the nodes that are not marked with :from-macro.
They, from left to right, become the children of the macro node."
  (assert (ast-attr a :from-macro))
  (let ((children nil))
    (labels ((%traverse (x)
               (when (new-clang-ast-p x)
                 (if (ast-attr x :from-macro)
                     (mapc #'%traverse (ast-children x))
                     (push x children)))))
      (mapc #'%traverse (ast-children a)))
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
    ((:ParmVar :Function :VarDecl)
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
  (case (ast-class obj)
    ((:Function :CompoundStmt :ReturnStmt :IfStmt :WhileStmt :ForStmt :SwitchStmt
                :NullStmt :DeclStmt)
     t)
    (t (ast-attr obj :fullstmt))))

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
      :EnumConstantDecl
      :EnumDecl
      :FieldDecl
      :FileScopeAsmDecl
      :FriendDecl
      :Function ;; :FunctionDecl
      :FunctionTemplateDecl
      :ImplicitParamDecl
      :IndirectFieldDecl
      :LabelDecl
      :LinkageSpecDecl
      :NamespaceAliasDecl
      :NamespaceDecl
      :NonTypeTemplateParmDecl
      :ParmVar  ;; :ParmVarDecl
      :PragmaCommentDecl
      :RecordDecl
      :StaticAssertDecl
      :TemplateTemplateParmDecl
      :TemplateTypeParmDecl
      :TranslationUnitDecl
      :TypeAliasDecl
      :TypeAliasTemplateDecl
      :TypedefDecl
      :UnresolvedUsingTypenameDecl
      :UnresolvedUsingValueDecl
      :UsingDecl
      :UsingDirectiveDecl
      :UsingPackDecl
      :UsingShadowDecl
      :VarDecl
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

(defgeneric compute-full-stmt-attr (obj)
  (:documentation "Fills in the :FULLSTMT attribute on clang ast
nodes, as needed.")
  (:method ((obj new-clang-ast))
    (case (ast-class obj)
      (:CompoundStmt
       (iter (for c in (ast-children obj))
             (when (ast-p c)
               (setf (ast-attr c :fullstmt) t))))
      ;; Others?
      )
    obj))

(defun compute-full-stmt-attrs (ast)
  (map-ast ast #'compute-full-stmt-attr))

(defgeneric set-attr-ast-child (obj attr val &key count)
  (:method (obj attr val &key (count 1))
    (let* ((pos -1)
           (children (ast-children obj))
           (n (length children)))
      (iter (while (and pos (> count 0) (< pos n)))
            (decf count)
            (setf pos (position-if #'ast-p (ast-children obj) :start (1+ pos))))
      (when (and pos (= count 0))
        (setf (ast-attr (elt (ast-children obj) pos) attr) val)))
    obj))

(defgeneric compute-guard-stmt-attr (obj)
  (:documentation "Fills in the :GUARDSTMT attribute on clang
ast nodes, as needed")
  (:method ((obj new-clang-ast))
    (case (ast-class obj)
      ((:WhileStmt :IfStmt :SwitchStmt)
       (set-attr-ast-child obj :GuardStmt t))
      ((:DoStmt)
       (set-attr-ast-child obj :GuardStmt t :count 2))
      ;; other cases
      )
    obj))

(defun compute-guard-stmt-attrs (ast)
  (map-ast ast #'compute-guard-stmt-attr))
