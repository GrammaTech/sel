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
           :documentation "Association list of Names and values of macros."))
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

(defgeneric (setf ast-attr) (v ast attr))
(defmethod (setf ast-attr) (v (ast new-clang-ast) (attr symbol))
  (let* ((attrs (new-clang-ast-attrs ast))
         (p (assoc attr attrs)))
    (if p (setf (cdr p) v)
        (setf (new-clang-ast-attrs ast)
              (cons (cons attr v) attrs)))
    v))

(defgeneric ast-is-implicit (ast)
  (:method ((ast new-clang-ast))
    (ast-attr ast :is-implicit)))

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
;; storing them in *symbol-table*
(defstruct reference-entry obj)

(defgeneric ast-file (ast)
  (:documentation "The file name associated with an AST node")
  (:method ((ast new-clang-ast))
    (ast-file (ast-attr ast :range)))
  (:method ((range new-clang-range))
    (ast-file (new-clang-range-begin range)))
  (:method ((loc new-clang-loc))
    (new-clang-loc-file loc))
  (:method (obj) nil))

(defun json-kind-to-keyword (json-kind)
  (when (stringp json-kind)
    (values (intern (simplified-camel-case-to-lisp json-kind) :keyword))))

(defgeneric clang-previous-decl (obj))
(defmethod clang-previous-decl ((obj new-clang-ast))
  (aget :previous-decl (new-clang-ast-attrs obj)))

(declaim (special *canonical-string-table* *symbol-table*))

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

(defvar *symbol-table* (make-hash-table :test #'equal)
  "Table mapping id values to the corresponding nodes.  This should
be bound for each clang program.")

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
        (format stream "~a" (ast-class obj)))))

;;; Json conversion

(defun clang-convert-json-for-file (json file genome-len)
  ;; The aget cache is used to record values of elided
  ;; json attributes, that are assumed to be equal to the previous
  ;; occurrence of such an attribute.  This requires the json
  ;; be converted left to right.  cl-json produces alists
  ;; in the same order they appear in the json, fortunately.
  (let* ((*aget-cache* nil)
         (ast (clang-convert-json json :file file)))
    (unless (ast-attr ast :range)
      (let ((range (make-new-clang-range :begin 0 :end genome-len)))
        (setf (ast-attr ast :range) range)))
    ast))

(defun clang-convert-json (json &key referenced file)
  "Convert json data in list form to data structures using NEW-CLANG-AST"
  (declare (ignorable file referenced))
  (typecase json
    (null nil)
    (cons
     (let* ((json-kind (aget :kind json))
            (json-kind-symbol (if json-kind
                                  (json-kind-to-keyword json-kind)
                                  :unknown)))
       (unless (keywordp json-kind-symbol)
         (error "Cannot convert ~a to a json-kind keyword" json-kind))
       (j2ck json json-kind-symbol)
       #+nil
       (if referenced
           (j2ck-referenced json json-kind-symbol)
           (j2ck-unreferenced json json-kind-symbol))))
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
        (when id
          (push obj (gethash id *symbol-table*)))))
    obj))

#|
(defun j2ck-unreferenced (json json-kind-symbol)
"Version of J2CK on non-reference objects."
(let* ((obj (j2ck json json-kind-symbol)))
(when obj
(let ((table *symbol-table*)
(id (new-clang-ast-id obj)))
;; (format t "ID = ~s~%" id)
(if (integerp id)
(let ((entry (gethash id table)))
(typecase entry
(reference-entry
(setf (reference-entry-obj entry) obj)
(setf (gethash id table) obj))
((not null)
(unless (eql (ast-class entry) :builtin-type)
(let ((l1 (clang-to-list entry))
(l2 (clang-to-list obj))
(hex (int-to-c-hex id)))
(cond
((equal l1 l2)
;; (warn "Object id ~a occurs more than once:~%Same json:~a" hex l1)
)
((alist-subsumed l2 l1)
;; (warn "Object id ~a occurs more than once:~%Subsumed.~%" hex)
)
(t
(warn "Object id ~a occurs more than once.~%Old entry: ~a.~%New Json: ~a"
hex l1 l2)))))
obj)
(null
(setf (gethash id table) obj)
(let ((pr-id (clang-previous-decl obj)))
(when pr-id
(assert (integerp pr-id))
(let ((rd-obj (gethash pr-id table)))
(assert rd-obj () "Cannot find previous decl for ~a~%" pr-id)
(setf (new-clang-ast-id rd-obj) id)
(remhash pr-id table))))
obj)))
obj)))))

(defun j2ck-referenced (json json-kind-symbol)
"Version of J2CK on objects that are the value of a reference
attribute.  This properly handles normalization of references."
(let* ((obj (j2ck json json-kind-symbol)))
(when obj
(let ((table *symbol-table*)
(id (new-clang-ast-id obj)))
(when (integerp id)
(let ((entry (gethash id table)))
(if entry
(setf obj (if (reference-entry-p entry)
(reference-entry-obj entry)
entry))
(setf (gethash id table)
(make-reference-entry :obj obj)))))))
obj))
|#

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

(defmethod store-slot ((obj new-clang-ast) (slot (eql :definition-data)) value)
  ;; Do not translate this attribute for now
  obj)

(defmethod store-slot ((obj new-clang-ast) (slot (eql :id)) value)
  (setf (new-clang-ast-id obj) (convert-slot-value obj slot value))
  obj)

(defmethod store-slot ((obj new-clang-ast) (slot (eql :inner)) value)
  (let ((children (mapcar #'clang-convert-json value)))
    ;; (format t "STORE-SLOT on ~a, :INNER with ~A children~%" value (length value))
    (setf (new-clang-ast-children obj) children))
  obj)

(defgeneric convert-slot-value (obj slot value)
  (:documentation "Convert a value in the context of a specific slot.  Return of
NIL indicates no value."))

(defmethod convert-slot-value ((obj new-clang-ast) (slot symbol) value)
  ;; Default to a context-independent conversion
  (clang-convert-json value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :referenced-decl)) value)
  (clang-convert-json value :referenced t))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :decl)) value)
  (clang-convert-json value :referenced t))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :template-params)) value)
  (mapcar #'clang-convert-json value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :id)) value)
  (read-c-integer value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :previous-decl)) value)
  (read-c-integer value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :name)) value)
  (and (equal value "") (call-next-method)))

;; More conversions

(defun convert-loc-json (loc-json)
  "Special handler for values of loc attributes"
  (if (aget :spelling-loc loc-json)
      (convert-macro-loc-json loc-json)
      (let ((col (aget :col loc-json))
            (file (cached-aget :file loc-json))
            (line (cached-aget :line loc-json))
            (tok-len (aget :tok-len loc-json)))
        (when (stringp file)
          (setf file (canonicalize-string file)))
        (when (or col file line)
          (make-new-clang-loc :col col :file file :line line :tok-len tok-len)))))

(defun convert-macro-loc-json (loc-json)
  "This is the special case of a LOC that has spelling and expansion locs"
  (let ((spelling-loc (convert-loc-json (aget :spelling-loc loc-json)))
        (expansion-loc (convert-loc-json (aget :expansion-loc loc-json)))
        (is-macro-arg-expansion (aget :is-macro-arg-expansion loc-json)))
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

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :value-category)) (value string))
  (intern (string-upcase value) :keyword))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :type)) (value list))
  (let ((qual-type (aget :qual-type value))
        (desugared-qual-type (aget :desugared-qual-type value)))
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
             *symbol-table*)
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
       (or (ast-attr a :is-implicit)
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
                                (shell cmd-fmt
                                       cbin
                                       (flags obj)
                                       src-file)
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
                              (values (decode-json-from-string stdout)
                                      src-file
                                      genome-len))))))

;;; Offsets into the genome

;; Vector giving offsets for the start of each line
;; This is obtained from a sw object using genome-line-offsets
(declaim (special *offsets*))


(defgeneric offset (obj))
(defmethod offset (obj) nil)
(defmethod offset ((obj new-clang-loc))
  (let ((line (new-clang-loc-line obj))
        (col (new-clang-loc-col obj)))
    (when (and line col)
      (+ (elt *offsets* (1- line)) col -1))))
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
   (let ((*offsets* (genome-line-offsets sw)))
     (all-offsets obj))))

(defgeneric tok-len (x)
  (:method ((x new-clang-loc)) (new-clang-loc-tok-len x))
  (:method ((x new-clang-macro-loc))
    (if (new-clang-macro-loc-is-macro-arg-expansion x)
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
  (let* ((*offsets* (genome-line-offsets sw))
         (genome (genome sw)))
    (flet ((%decorate (a)
             ;; At ast node A, split the parts of the source
             ;; that are not in the children into substrings
             ;; that are placed between the children.  Do not
             ;; place strings for children for whom offsets
             ;; cannot be computed
             (let ((children (ast-children a)))
               (multiple-value-bind (begin end)
                   (begin-and-end-offsets a)
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
                                        (collect (subseq genome i cbegin))
                                        (setf i cend))))
                                  (collect c))
                            (list (subseq genome i end))))))))))
      (map-ast ast #'%decorate)))
  ast)

(defun fix-ancestor-ranges (sw ast)
  "Normalize the ast so the range of each node is a superset
of the ranges of its children"
  (let ((*offsets* (genome-line-offsets sw)))
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
                   (format t "Expanding range (~a,~a) to (~a,~a)~%"
                           begin end min-begin max-end)
                   (setf (ast-attr a :range)
                         (make-new-clang-range :begin min-begin :end max-end)))))))
      (map-ast ast #'%normalize))))

(defun combine-overlapping-siblings (sw ast)
  "Group sibling nodes with overlapping or out of order
ranges into 'combined' nodes.  Warn when this happens."
  (let ((*offsets* (genome-line-offsets sw))
        (genome (genome sw)))
    (flet ((%check (a)
             (let ((end 0)
                   changed? accumulator)
               (flet ((%combine ()
                        (case (length accumulator)
                          (0)
                          (1 (list (pop accumulator)))
                          (t
                           (let ((new-begin (reduce #'min accumulator :key #'begin-offset))
                                 (new-end (reduce #'max accumulator :key #'end-offset)))
                             (setf changed? t)
                             (format t "Combining ~a overlapping asts over [~a,~a):~%"
                                     (length accumulator) new-begin new-end)
                             (format t "~a~%" (subseq genome new-begin new-end))
                             (prog1
                                 (list (make-new-clang-ast
                                        :class :combined
                                        :attrs `((:range . ,(make-new-clang-range
                                                             :begin new-begin
                                                             :end new-end))
                                                 (:subsumed . ,accumulator))))
                               (setf accumulator nil)))))))
                 (let ((new-children
                        (append
                         (iter (for c in (ast-children a))
                               (multiple-value-bind (cbegin cend)
                                   (begin-and-end-offsets c)
                                 (if (and cbegin end cend (< cbegin end))
                                     (setf accumulator (append accumulator (list c))
                                           end (max end cend))
                                     (progn
                                       (appending (%combine))
                                       (setf accumulator (list c)
                                             end cend)))))
                         (%combine))))
                   (when changed?
                     (setf (ast-children ast) new-children)))))))
      (map-ast ast #'%check))))
