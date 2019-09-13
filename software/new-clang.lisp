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
           :*soft*
           :make-new-clang-macroexpand-hook
           :cpp-scan))
(in-package :software-evolution-library/software/new-clang)
(in-readtable :curry-compose-reader-macros)

;;; NOTE: *SOFT* is used in various low level functios that need
;;;  global information about the object that contains an AST.
;;;  The API does not always provide this object.  The downside
;;;  of binding *SOFT* is that higher level methods have to make
;;;  sure they bind it or the accessors don't work.  Tests in
;;;  test.lisp also must do this.
;;;
;;;  Changing APIs to pass the software object down "manually"
;;;  is another option, and would enable methods to be specialized
;;;  on that object.
;;;
;;;  The best approach may be to add :around methods for generic
;;;  functions that (1) have a new-clang object as an argument, and (2)
;;;  are going to access things under that object using accessors
;;;  that need *soft*.  The :around method will bind *soft* to that
;;;  new-clang object before invoking the next method.

(declaim (special *soft*))
(declaim (special *canonical-string-table* *current-line*))

(defparameter *clang-binary*
  ;; "/pdietz/clang9-installed/bin/clang"
  "/clang9/bin/clang"
  "This is the location clang is installed in the clang9 Docker image")
;; Install locally by:
;;  sudo docker cp clang9:/clang9 /clang9
;; This takes several minutes and uses about 34GB of disk space

(define-software new-clang (clang-base genome-lines-mixin)
  (
   ;; TODO: FUNCTIONS slot is not currently used.  Use or remove.
   #|
   (functions :initarg :functions :reader functions
              :initform nil :copier :direct
              :type #+sbcl (list (cons keyword *) *) #-sbcl list
              :documentation "Complete functions with bodies.")
   |#
   ;; TODO: PROTOTYPES slot is not current used.  Use or remove.
   #|
   (prototypes :initarg :prototypes :reader prototypes
               :initform nil :copier :direct
               :type #+sbcl (list (cons keyword *) *) #-sbcl list
               :documentation "Function prototypes.")
   |#
   (includes :initarg :includes :accessor includes
             :initform nil :copier :direct
             :type #+sbcl (list string *) #-sbcl list
             :documentation "Names of included includes.")
   (types :initarg :types :accessor types
          :initform (make-hash-table :test 'equal)
          :copier copy-hash-table
          :type #+sbcl hash-table #-sbcl hash-table
          :documentation "Association list of types keyed by HASH id.")
   ;;; TODO: Not currently used.  Remove or use.  This is carried over
   ;;;       from the old clang front end.
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
    :documentation "Mapping from qualtype/desugaredType pairs to
new-clang-type objects.  Used for canonicalization of these objects.")
   (include-dirs
    :initarg :include-dirs
    :accessor include-dirs
    :initform nil
    :type list
    :copier :direct
    :documentation "List of directories that are where include files
 are searched for the -I arguments to the C compiler")
   (compiler :initform *clang-binary*)
   ;;; NOTE: This will give the same hashs to types from different
   ;;;       source files.  Instead, consider initializing the counter
   ;;;       in a file-specific way, or compute the hashes from the
   ;;;       type object themselves (but that could still lead to
   ;;;       collisions.)
   (hash-counter :initform 1 :initarg :hash-counter
                 :accessor hash-counter
                 :type integer
                 :documentation "A counter used to assign hash values to
 type objects and possibly other things"))
  (:documentation
   "C language (C, C++, C#, etc...) ASTs using Clang, C language frontend
   for LLVM.  See http://clang.llvm.org/.  This is for ASTs from Clang 9+."))

;;; The INCLUDE-DIRS slot should be in normal form
;;; Use various methods to ensure this happens

(defmethod initialize-instance :after ((obj new-clang) &key &allow-other-keys)
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
             (collecting (trim-left-whitespace (cadr e))))
            ((and (equal (subseq x 0 2) "-I")
                  (> (length x) 2))
             (collecting (trim-left-whitespace (subseq x 2))))))))

(defgeneric map-ast-while (a fn)
  (:documentation "Apply FN to the nodes of AST A, stopping
the descent when FN returns NIL"))

(defmethod map-ast-while ((a ast) fn)
  (when (funcall fn a)
    (dolist (c (ast-children a))
      (when (ast-p c) (map-ast-while c fn)))))

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
    stmt-asts))

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
    non-stmt-asts))

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

(defmethod add-type :around ((sw new-clang) type)
  (declare (ignorable type))
  (let ((*soft* sw))
    (call-next-method)))

(defmethod get-ast-types :around ((sw new-clang) type)
  (declare (ignorable type))
  (let ((*soft* sw))
    (call-next-method)))

(defmethod bind-free-vars :around ((sw new-clang) a1 a2)
  (declare (ignorable a1 a2))
  (let ((*soft* sw))
    (call-next-method)))

(defmethod update-headers-from-ast :around ((sw new-clang) ast database)
  (let ((*soft* sw))
    (call-next-method)))

(defun find-macros-in-ast (ast)
  (find-macros-in-children (ast-children ast)))

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
          (make-new-clang-macro :hash hash :body body :name name))))))




;;; Code for adapting tests to use old or new clang front
;;; ends, controllably

;; This is DEFVAR so I can set the var and reload this file
;; without losing that value, which is useful for debugging.
(defvar *new-clang?* nil "When true, use NEW-CLANG instead of CLANG")

;;; TODO: determine which structure fields should be read-only

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

;;; TODO: identify which of these generic accessors should be read only,
;;;  beyond those that refer to read-only fields in new-clang-ast.
;;;  For those, remove the SETF method, or at least make it error
;;;  when called.

;;; There are no separate 'node' objects for new-clang
(defmethod ast-node ((obj new-clang-ast)) obj)

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

(defmethod ast-name ((s string)) s)

;; Special subclass for :CXXOperatorCallExpr nodes
(defstruct (cxx-operator-call-expr (:include new-clang-ast))
  ;; POS is the "actual" position of the operator in the
  ;; list of child ASTs (ignoring non-AST children).
  ;; When computing ranges, and when computing
  ;; source text, put it there.
  ;; The type FIXNUM is too big; figure out how much
  ;; smaller this can be made.
  (pos nil :type (or null fixnum)))

;;;
;;; NOTE: NEW-CLANG-TYPE is not a drop-in replacement for
;;;  SEL/SW/CLANG:CLANG-TYPE.  The latter contains additional
;;;  information that is not properly part of a type at all.
;;;
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
   (typedef :accessor new-clang-typedef
            :initform nil
            :initarg :typedef
            :type (or null new-clang-type)
            :documentation "Type for which this is a typedef of,
or NIL if this is not a typedef type.")
   #|
   (decl :reader new-clang-type-decl
   :initform nil
   :initarg :decl
   :documentation "The node at which the type is declared, if any")
   |#
   (nct+-list :accessor new-clang-type-nct+-list
              :initform nil
              :initarg :nct+-list
              :type list
              :documentation "List of NCT+ objects for this type, with various
storage classes.")
   (hash :accessor new-clang-type-hash
         :initarg :hash
         :initform (incf (hash-counter *soft*))
         :documentation "A hash code assigned to type
objects, for compatibility with old clang"))
  (:documentation "Objects representing C/C++ types.  Canonicalized
on QUAL and DESUGARED slots."))

(defclass nct+ ()
  ((type :initarg :type
         :reader nct+-type
         :type new-clang-type)
   (storage-class :initarg :storage-class
                  :reader type-storage-class
                  :type (member nil :static :extern :register))
   (modifiers :initarg :modifers
              :type integer
              :reader nct+-modifiers)
   (array :initarg :array
          :type string
          :reader type-array)
   (name :initarg :name
         :type string
         :reader type-name)
   (i-file :initarg :i-file
           :type (or null string)
           :reader type-i-file))
  (:documentation "Wrapper object that is intended to behave like
SEL/SW/CLANG:CLANG-TYPE.  This means it must have some information
that is not strictly speaking about types at all (storage class)."))

(defun make-nct+ (type storage-class ast)
  (assert (typep type 'new-clang-type))
  (or (find storage-class (new-clang-type-nct+-list type)
            :key #'type-storage-class)
      (make-instance 'nct+
        :type type
        :storage-class storage-class
        :i-file (new-clang-i-file *soft* type ast))))

(defstruct (new-clang-macro (:include clang-macro))
  ;; Spelling loc is the location of the body of the loc
  ;; Its presence in a clang-macro-loc indicates something
  ;; is a macroexpansion of this macro.  This may fail
  ;; if the macro expands into other macros.
  (spelling-loc nil :type (or integer null) :read-only t))

(defmethod scopes :around ((sw new-clang) (ast new-clang-ast))
  (let ((*soft* sw))
    (call-next-method)))

(defmethod replace-all ((string string) (part new-clang-ast)
                        (replacement new-clang-ast) &key (test #'char=)
                                                      confirm)
  (replace-all string (ast-name part) (ast-name replacement)
               :test test :confirm confirm))

(defmethod replace-all ((string string) (part string)
                        (replacement new-clang-ast) &key (test #'char=)
                                                      confirm)
  (replace-all string part (ast-name replacement)
               :test test :confirm confirm))

(defmethod replace-all ((string string) (part new-clang-ast)
                        replacement &key (test #'char=) confirm)
  (replace-all string (ast-name part) replacement
               :test test :confirm confirm))

(defmethod typedef-type ((obj new-clang) (nct nct+))
  ;; Must construct an NCT+ for this type
  #+(or)
  (let ((tp (nct+-type nct)))
    (format t "(nct+-type nct) = ~a~%" tp)
    (when tp
      (describe tp))
    (format t "(new-clang-typedef ...) = ~a~%"
            (new-clang-typedef tp)))
  (or (when-let ((tp (new-clang-typedef (nct+-type nct))))
        (assert (typep tp 'new-clang-type))
        (or (find nil (new-clang-type-nct+-list tp)
                  :key #'type-storage-class)
            (make-nct+ tp nil nil)))
      nct))

(defmethod type-trace-string ((type nct+) &key qualified)
  (type-trace-string* type qualified))

(defgeneric new-clang-i-file (obj type ast)
  (:method ((obj new-clang) (type new-clang-type) ast)
    ;; Get list of system files needed to handle the types
    ;; here
    (let* ((qual-includes
            (when-let ((qual (new-clang-type-qual type)))
              (includes-of-names-in-string obj qual)))
           (desugared-includes
            (when-let ((desugared (new-clang-type-desugared type)))
              (includes-of-names-in-string obj desugared)))
           (includes
            (remove-duplicates
             (remove-if (lambda (s) (eql (elt s 0) #\"))
                        (append qual-includes desugared-includes nil))
             :test #'equal)))
      (let ((files (sort includes #'string<))
            (od (absolute-original-directory obj))
            (include-dirs (include-dirs obj)))
        (iter (for f in files)
              (multiple-value-bind (str local?)
                  (normalize-file-for-include f od include-dirs)
                (unless local?
                  (collecting str))))))))

(defmethod initialize-instance :after ((obj new-clang-type)
                                       &key hash &allow-other-keys)
  (when (boundp '*soft*)
    (setf (gethash hash (slot-value *soft* 'types)) obj))
  obj)

(defmethod initialize-instance :after ((obj nct+) &key (compute-slots t) &allow-other-keys)
  (pushnew obj (new-clang-type-nct+-list (nct+-type obj)))
  ;; The COMPUTE-SLOTS argument is here so we can suppress the automatic initialization
  ;; of the slots of an nct+ object.  This is needed in the tests in test.lisp.
  ;; It may be needed in client code that will be manually creating type objects.
  (when compute-slots (compute-nct+-slots obj))
  obj)

(defmethod type-hash ((tp new-clang-type))
  (new-clang-type-hash tp))
(defmethod type-hash ((tp+ nct+))
  (type-hash (nct+-type tp+)))

;;; Pointer, const, volatile, and restrict are indicated by integers
;;;  in the modifiers slot.

(defconstant +pointer+ 1)
(defconstant +const+ 2)
(defconstant +volatile+ 4)
(defconstant +restrict+ 8)

(defmethod type-pointer ((tp+ nct+))
  (if (logtest +pointer+ (nct+-modifiers tp+)) t nil))
(defmethod type-const ((tp+ nct+))
  (if (logtest +const+ (nct+-modifiers tp+)) t nil))
(defmethod type-volatile ((tp+ nct+))
  (if (logtest +volatile+ (nct+-modifiers tp+)) t nil))
(defmethod type-restrict ((tp+ nct+))
  (if (logtest +restrict+ (nct+-modifiers tp+)) t nil))

(defgeneric compute-nct+-slots (tp+)
  (:method ((tp+ nct+))
    "Fill in the MODIFIERS, ARRAY, and POINTER slots of a NCT+ object"
    ;; This is only partially working
    ;; As in the old clang front end, it's not going to get
    ;; nested array and pointer declarators right in some cases
    (when-let* ((type (nct+-type tp+))
                (qual (new-clang-type-qual type)))
      (multiple-value-bind (pointer const volatile restrict n a)
          (compute-nct+-properties qual)
        (with-slots (array name modifiers) tp+
          (setf array a
                name n
                modifiers
                (pack-nct+-modifiers
                 pointer const volatile restrict)))))))

(defun pack-nct+-modifiers (pointer const volatile restrict)
  (logior
   (if pointer +pointer+ 0)
   (if const +const+ 0)
   (if volatile +volatile+ 0)
   (if restrict +restrict+ 0)))

(defun compute-nct+-properties (name)
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
  "Trim const, volatile, and restrict modifiers from a type name"
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

(defmethod ast-class ((obj new-clang-ast))
  (new-clang-ast-class obj))

(defmethod ast-syn-ctx ((obj new-clang-ast))
  (new-clang-ast-syn-ctx obj))

(defmethod ast-in-macro-expansion ((obj new-clang-ast))
  (eql (ast-class obj) :macroexpansion))

(defmethod source-text ((ast new-clang-ast))
  (with-output-to-string (out)
    (mapc [{write-string _ out} #'source-text]
          (ast-children ast))))

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

(defmethod type-name ((ast new-clang-type))
  (new-clang-type-qual ast))

(defmethod type-decl ((obj nct+))
  (type-decl (nct+-type obj)))

;;; WIP
(defmethod type-decl ((type new-clang-type))
  ;; This is "" for most types.
  ;; For struct or union types (or classes? no) it is
  ;; the string for the declaration of that type, pulled from
  ;; the source text.
  ""
  ;; For struct/union types, the qual will be
  ;;  "struct <name>" or "union <name>"
  ;; These will be stored in the record-name-table, with key
  ;; equal to this qual
  (or (when-let* ((qual (new-clang-type-qual type))
                  (desugared (new-clang-type-desugared type))
                  (len (length qual)))
        (flet ((is-prefix (s)
                 (let ((slen (length s)))
                   (and (>= len slen)
                        (string= s qual :end2 slen)))))
          (when (or (is-prefix "struct ")
                    (is-prefix "union "))
            (when-let* ((table (type-table *soft*))
                        (record-decl (gethash (cons qual desugared) table)))
              (concatenate 'string (new-clang-type-qual record-decl)  ";")))))
      ""))

(defmethod type-decl-string ((obj new-clang-type) &key &allow-other-keys)
  (new-clang-type-qual obj))

(defmethod type-decl-string ((obj nct+) &key &allow-other-keys)
  (type-decl-string (nct+-type obj)))

(defmethod find-or-add-type ((obj new-clang) name &key &allow-other-keys)
  ;; stub
  (let ((vals (hash-table-values (types obj))))
    (or (find name vals :key #'type-name :test #'string=)
        nil ; stub
        )))

(defmethod add-type ((obj new-clang) (type nct+))
  (sel/sw/clang::add-type* obj type))

#+(or)
(defmethod find-type ((obj new-clang) (type new-clang-type))
  ;; This looks like a stub, but isn't.
  ;; What's happening here is that while in old clang
  ;; find-type was used to look up types from hashes,
  ;; in the new front end the type objects are there directly.
  ;; The lookup function just returns the object in that case.
  type)

(defmethod find-type ((obj new-clang) (type nct+))
  ;; This looks like a stub, but isn't.
  ;; What's happening here is that while in old clang
  ;; find-type was used to look up types from hashes,
  ;; in the new front end the type objects are there directly.
  ;; The lookup function just returns the object in that case.
  type)

(defmethod find-type ((obj new-clang) (name string))
  (let ((vals (hash-table-values (types obj))))
    (find name vals :key #'type-name :test #'string=)))

(defun ast-is-class-fun (key)
  (lambda (c) (ast-is-class c key)))

(defun %areplace (key val alist)
  (cons (cons key val) (remove key alist :key #'car)))

(defmethod is-full-stmt-ast ((ast new-clang-ast))
  (ast-attr ast :fullstmt))

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
                ((path children class id syn-ctx aux-data) nil)
                ((attrs)
                 (unless attrs-p
                   (setf attrs-p t
                         attrs arg)))
                (t
                 ;; Otherwise, it's an attribute
                 (push (cons key arg) new-attrs))))))
    (iter (for (key . arg) in new-attrs)
          (setf attrs (%areplace key arg attrs)))

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

(defmethod remove-file-attr-in-same-file ((obj new-clang))
  "The FILE attribute is unnecessary when it's the same as the file for this
object (after include and macro processing have been performed).  Remove the
attribute when the value is the same as the file of OBJ.  After this is done,
asts can be transplanted between files without difficulty."
  (when-let ((tmp-file (tmp-file obj)))
    (map-ast (ast-root obj)
             (lambda (a) (remove-ast-file a tmp-file)))))

;;;  This code is now assuming AST-FILE is not present (or NIL) if
;;;  the file was the same as the one this AST is in.
;;;
;;;  This is also needed to make the i-file slot of nct+ work.

(defmethod ast-includes-in-obj ((obj new-clang) (ast new-clang-ast))
  ;; (format t "Enter ast-includes-in-obj~%")
  (prog1
      (let ((*soft* obj)
            (tmp-file (tmp-file obj))
            (file (ast-file ast))
            (include-dirs (include-dirs obj))
            (od (absolute-original-directory obj)))
        (mapcar
         (lambda (s) (normalize-file-for-include s od include-dirs))
         (delete-duplicates
          (nconc
           ;; The tests w. tmp-file here and below may not be
           ;; necessary, but are kept for safety
           (unless (or (not file) (equal tmp-file file))
             (list file))
           (when-let* ((ref (ast-referenced-obj ast))
                       (file (ast-file ref)))
             (unless (or (null file) (equal file tmp-file))
               (list file)))
           (when-let* ((type (ast-type ast))
                       (str (new-clang-type-qual type)))
             (includes-of-names-in-string obj str)))
          :test #'equal)))))

(defun includes-of-names-in-string (obj str)
  (let ((tmp-file (tmp-file obj))
        (table (name-symbol-table obj))
        (names (names-in-str str)))
    (iter (for n in names)
          (nconcing
           (iter (for v in (gethash n table))
                 (when v
                   (nconcing
                    (ast-include-from-type v tmp-file))))))))

;;; TODO:  Confirm that restricting this to just :typedef
;;;  is correct (both in that it replicates what old clang
;;;  front end did, and it's what's intended.)  Confirm
;;;  that structs and unions were not producing this in
;;;  old front end.
;;;
;;;  This should be updated when the json from Clang includes
;;;  the entire include chain, not just the final file
;;;  in the chain.
(defun ast-include-from-type (v tmp-file)
  (when (member (ast-class v) '(:typedef))
    (when-let ((file (ast-file v)))
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

(defgeneric compute-macros (obj)
  (:documentation "Fill in the MACROS slot of OBJ"))

(defmethod compute-macros ((obj new-clang))
  (setf (macros obj) (find-macros-in-children
                      (ast-children (ast-root obj)))))

(defmethod update-caches ((obj new-clang))
  (call-next-method)
  (compute-macros obj)
  (compute-includes obj)
  obj)

(defmethod clear-caches ((obj new-clang))
  (with-slots (includes) obj
    (setf includes nil))
  (call-next-method))

(defun names-in-str (str)
  "Find all substrings of STR that are C/C++ names"
  (split-sequence-if-not (lambda (c)
                           (and (typep c 'standard-char)
                                (or (alphanumericp c)
                                    (member c '(#\_ #\: #\< #\>)))))
                         str
                         :remove-empty-subseqs t))

(defmethod get-unbound-vals :around ((sw new-clang) ast)
  (let ((*soft* sw)) (call-next-method)))

(defmethod get-unbound-funs :around ((sw new-clang) ast)
  (let ((*soft* sw)) (call-next-method)))

(defmethod delete-decl-stmts :around ((sw new-clang) block replacements)
  (let ((*soft* sw)) (call-next-method)))

(defmethod apply-mutation :around ((sw new-clang) mutation)
  (let ((*soft* sw)) (call-next-method)))

(defmethod ast-unbound-vals ((ast new-clang-ast))
  (ast-unbound-vals* ast (ast-class ast)))

(defmethod ast-unbound-vals ((str string))
  (declare (ignore str))
  nil)

(defgeneric ast-unbound-vals* (ast class)
  (:documentation "Implementation function for ast-unbound-vals,
where class = (ast-class ast)."))

(defmethod ast-unbound-vals* ((ast new-clang-ast) (class (eql :declrefexpr)))
  (when-let ((obj (ast-referenced-obj ast)))
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
    (when-let* ((obj (ast-referenced-obj ast)))
      (when (eql (ast-class obj) :function)
        (list (list obj (ast-void-ret obj) (ast-varargs obj)
                    (count-if (lambda (a) (and (ast-p a) (eql (ast-class a) :ParmVar)))
                              (ast-children obj))))))))

(defmethod ast-includes ((ast new-clang-ast))
  (ast-includes-in-obj *soft* ast))

(defmethod ast-macros ((ast new-clang-ast))
  (ast-macros* ast (ast-class ast)))

(defmethod ast-macros ((ast string)) (declare (ignorable ast)) nil)

(defgeneric macros-of-macro-names (macro-names)
  (:documentation "Computes the macros associated with macro-names")
  (:method ((macro-names list) &aux (macros (macros *soft*)))
    (iter (for n in macro-names)
          (let ((m (find n macros :key #'macro-name :test #'equal)))
            (when m (collecting (macro-hash m)))))))

(defmethod ast-macros* ((ast new-clang-ast) class)
  (declare (ignorable class))
  (union (macros-of-macro-names (ast-attr ast :macro-names))
         (reduce #'union (ast-children ast)
                 :key #'ast-macros :initial-value nil)))

(defmethod ast-macros* ((ast new-clang-ast) (class (eql :toplevel)))
  (declare (ignorable ast class))
  nil)

(defmethod ast-macros* ((ast new-clang-ast) (class (eql :macroexpansion)))
  (declare (ignorable class))
  (reduce
   #'union
   (cons (macros-of-macro-names (ast-attr ast :macro-names))
         (mapcar #'ast-macros (ast-attr ast :macro-child-segment)))
   :initial-value nil))

(defmethod ast-types ((ast new-clang-ast))
  (ast-types* ast (ast-class ast)))

(defmethod ast-types ((ast string)) nil)

(defgeneric ast-types* (ast class)
  (:documentation "Dispatch function for computing AST-TYPES
on various ast classes"))

(defun ast-types*-on-decl (ast)
  (when-let ((type (ast-attr ast :type)))
    (list (make-nct+ type (ast-attr ast :storageclass) ast))))

(defmethod ast-types* ((ast new-clang-ast) (ast-class (eql :ParmVar)))
  (ast-types*-on-decl ast))

(defmethod ast-types* ((ast new-clang-ast) (ast-class (eql :Var)))
  ;; For :Var nodes, we must also include the types in the
  ;; initializer, if present
  (let ((nodes (remove ast (ast-nodes-in-subtree ast))))
    (sort
     (reduce #'union (mapcar #'ast-types nodes)
             :initial-value (ast-types*-on-decl ast))
     #'string<
     :key #'type-name)))

(defmethod ast-types* ((ast new-clang-ast) (ast-class (eql :Macroexpansion)))
  (let ((nodes (remove ast (ast-nodes-in-subtree ast)))
        (macro-child-segment (mapcan #'ast-nodes-in-subtree
                                     (ast-attr ast :macro-child-segment))))
    (sort
     (reduce #'union (mapcar #'ast-types (append nodes macro-child-segment))
             :initial-value nil)
     #'string<
     :key #'type-name)))

(defmethod ast-types* ((ast new-clang-ast) (ast-class (eql :UnaryExprOrTypeTraitExpr)))
  (let ((argtype (ast-attr ast :argtype))
        (types (ast-types*-on-decl ast)))
    (if argtype
        (adjoin (make-nct+ argtype nil ast) types)
        types)))

(defmethod ast-types* ((ast new-clang-ast) (ast-class (eql :Typedef)))
  (ast-types*-on-decl ast))

(defmethod ast-types* ((ast new-clang-ast) (ast-class symbol))
  (case ast-class
    ((;; :UnaryExprOrTypeTraitExpr
      :CstyleCastExpr
      :CXXFunctionalCastExpr
      :CXXReinterpretCastExpr)
     (ast-types*-on-decl ast))
    (t nil)))

(defmethod rebind-vars ((ast new-clang-ast)
                        var-replacements fun-replacements)
  (case (ast-class ast)
    (:macroexpansion
     ;; Revert back to string-based rebinding
     (let ((new-children
            (mapcar #'(lambda (s) (rebind-vars s var-replacements fun-replacements))
                    (ast-children ast))))
       (if (equal (ast-children ast) new-children)
           ast
           (copy ast :children new-children))))
    (:DeclRefExpr
     #+rebind-vars-debug
     (progn
       (format t "REBIND-VARS on DeclRefExpr:~%~a~%~a~%"
               (source-text ast) (ast-to-list ast))
       (format t "referenceddecl = ~a~%"
               (ast-to-list (ast-referenced-obj ast))))
     (iter (for (old new) in var-replacements)
           (assert old) (assert new)
           (when (eql (ast-referenced-obj ast) old)
             #+rebind-vars-debug
             (progn
               (describe ast)
               (describe old)
               (describe new)
               (format t "Replace var ~a~%~a~%with ~a~%"
                       (source-text ast)
                       (new-clang-ast-attrs ast)
                       (ast-name new))
               (format t "(ast-children ast) = ~a~%" (ast-children ast)))
             (setf ast (copy ast :referenceddecl new
                             :children (list (ast-name new))))
             #+rebind-vars-debug
             (format t "New ast: ~a~%~a~%" (source-text ast)
                     (new-clang-ast-attrs ast))))
     (iter (for (oldf newf) in fun-replacements)
           (let ((old (first oldf))
                 (new (first newf)))
             (assert old) (assert new)
             (when (eql (ast-referenced-obj ast) old)
               #+rebind-vars-debug (format t "Replace fun~%")
               (setf ast (copy ast :referenceddecl new)))))
     ast)
    (t (let ((c (mapcar {rebind-vars _ var-replacements fun-replacements}
                        (ast-children ast))))
         (if (every #'eql c (ast-children ast))
             ast
             (copy ast :children c))))))

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

(defun make-new-clang-type (&rest keys &key qual desugared typedef
                                         (hash (incf (hash-counter *soft*)))
                                         &allow-other-keys)
  (let* ((key (cons qual desugared))
         (sw *soft*)
         (table (when sw (type-table sw))))
    (flet ((%make ()
             (apply #'make-instance 'new-clang-type
                    :hash hash
                    ;; Do not store desugared if it's the same
                    ;; as the sugared type
                    :desugared (and (not (equal qual desugared)) desugared)
                    :typedef typedef
                    keys)))
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

(defgeneric remove-ast-file (ast file)
  (:documentation "Removes mentions of FILE from locations in the
range attribute of AST"))
(defmethod remove-ast-file ((ast new-clang-ast) file)
  (remove-ast-file (ast-attr ast :range) file))
(defmethod remove-ast-file ((r new-clang-range) file)
  (remove-ast-file (new-clang-range-begin r) file)
  (remove-ast-file (new-clang-range-end r) file))
(defmethod remove-ast-file ((loc new-clang-loc) file)
  (when (equal (new-clang-loc-file loc) file)
    (setf (new-clang-loc-file loc) nil)))
(defmethod remove-ast-file ((macro-loc new-clang-macro-loc) file)
  (remove-ast-file (new-clang-macro-loc-spelling-loc macro-loc) file)
  (remove-ast-file (new-clang-macro-loc-expansion-loc macro-loc) file))

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

(defgeneric clang-previous-decl (obj))
(defmethod clang-previous-decl ((obj new-clang-ast))
  (aget :PreviousDecl (new-clang-ast-attrs obj)))


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

;;; NOTE: will need to make sure this works on Windows also
;;;  Perhaps it should work on pathnames, not namestrings?

;;; NOTE: this assumes "..." does NOT search the include path
;;;  In Clang, this behavior is controlled by command line options,
;;;  which we'll need to recognize.

(defun normalize-file-for-include (file-string original-dir include-dirs)
  "Returns the normalized version of file-string relative to the original
directory and the include-dirs, nd a value that is T if the string should
be in #include \"...\", NIL if in #include <...>"
  (assert (or (null original-dir)
              (eql (car (pathname-directory original-dir))
                   :absolute))
          ()
          "ORIGINAL-DIR must be an absolute filename: ~a"
          original-dir)
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
               (values (concatenate 'string "\"" (subseq file-string om) "\"")
                       t)
               (let ((max-match 0) (dir nil))
                 (iter (for ind in include-dirs)
                       (let ((mm (%match ind)))
                         ;; (format t "ind = ~a, mm = ~a~%" ind mm)
                         (when (and mm (> mm max-match))
                           (setf max-match mm
                                 dir ind))))
                 (if dir
                     (values (concatenate 'string "<"
                                          (subseq file-string max-match) ">")
                             nil)
                     (values (concatenate 'string "\"" file-string "\"")
                             t))))))))))

(defgeneric original-directory (obj))
(defmethod original-directory ((obj source))
  (let ((file (original-file obj)))
    (when file
      (namestring (make-pathname :defaults (pathname file)
                                 :name nil :type nil)))))

(defgeneric absolute-original-directory (obj))
(defmethod absolute-original-directory ((obj source))
  (let ((file (absolute-original-file obj)))
    (when file
      (namestring (make-pathname :defaults (pathname file)
                                 :name nil :type nil)))))

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
         (when (and obj inner)
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
  (declare (ignorable json-kind-symbol))
  (let ((obj (make-new-clang-ast)))
    (store-slots obj json)))

(defun alist-subsumed (al1 al2)
  (iter (for (k . v) in al1)
        (always (or (equal v "")
                    (equal v (aget k al2))))))

(defmethod j2ck :around (json json-kind-symbol)
  (declare (ignorable json json-kind-symbol))
  (when-let ((obj (call-next-method)))
    (let ((id (new-clang-ast-id obj)))
      (when (ast-attr obj :loc)
        (push obj (gethash id (symbol-table *soft*)))))
    obj))

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

;; (defmethod store-slot ((obj new-clang-ast) (slot (eql :id)) value)
;;   (call-next-method))

(defmethod store-slot ((obj new-clang-ast) (slot (eql :definitiondata)) value)
  (declare (ignorable slot))
  ;; Do not translate this attribute for now
  obj)

(defmethod store-slot ((obj new-clang-ast) (slot (eql :id)) value)
  (setf (new-clang-ast-id obj) (convert-slot-value obj slot value))
  obj)

(defmethod store-slot ((obj new-clang-ast) (slot (eql :inner)) value)
  (declare (ignorable slot))
  (let ((children (remove nil (mapcar (lambda (o) (clang-convert-json o :inner t)) value))))
    ;; (format t "STORE-SLOT on ~a, :INNER with ~A children~%" value (length value))
    (setf (new-clang-ast-children obj) children))
  obj)

(defmethod store-slot ((obj new-clang-ast) (slot (eql :array_filler)) value)
  (declare (ignorable slot))
  (let ((children (remove nil (mapcar (lambda (o) (clang-convert-json o :inner t)) value))))
    ;; (setf (ast-attr obj :array-filler) t)
    (setf (new-clang-ast-children obj) children))
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
    ((equal value "static") :static)
    ((equal value "extern") :extern)
    ((equal value "register") :register)
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
      (let ((col (aget :col loc-json))
            (file (cached-aget :file loc-json))
            (line (cached-aget :line loc-json))
            (tok-len (aget :toklen loc-json)))
        (when (stringp file)
          (setf file (canonicalize-string file)))
        (when (or col file line)
          (make-new-clang-loc :col col :file file
                              :line line :tok-len tok-len)))))

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
  (convert-type-slot-value obj slot value))

(defmethod convert-slot-value ((obj new-clang-ast) (slot (eql :argtype)) (value list))
  (convert-type-slot-value obj slot value))

(defun convert-type-slot-value (obj slot value)
  (declare (ignore obj slot))
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

(defmethod remove-asts-if (ast fn) (declare (ignorable fn)) ast)

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
                                         (remove "-c" (flags obj) :test #'equal)
                                         src-file))
                              (when (find exit '(131 132 134 136 139))
                                (error
                                 (make-condition 'mutate
                                                 :text (format nil "~a core dump with ~d, ~s"
                                                               cbin exit stderr)
                                                 :obj obj)))
                              (restart-case
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
                                (keep-partial-asts ()
                                  :report "Ignore error retaining partial ASTs for software object."
                                  nil))
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
(defmethod offset (obj) (declare (ignorable obj)) nil)
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

(defgeneric (setf offset) (offset obj)
  (:documentation "Settor for OFFSET"))

(defmethod (setf offset) ((offset integer) (obj new-clang-ast))
  (if-let ((range (ast-attr obj :range)))
    (let ((begin (new-clang-range-begin range)))
      (if (typep begin '(or null integer))
          (setf (new-clang-range-begin range) offset)
          (setf (offset begin) offset)))
    (progn
      (setf (ast-attr obj :range)
            (make-new-clang-range :begin offset))
      offset)))

(defmethod (setf offset) ((offset integer) (obj new-clang-loc))
  (multiple-value-bind (line col)
      (offset-to-line-and-col *soft* offset)
    (setf (new-clang-loc-line obj) line
          (new-clang-loc-col obj) col)
    offset))

(defmethod (setf offset) ((offset integer) (obj new-clang-macro-loc))
  ;; Do nothing on macro locs
  ;; It may be possible to do better, but for now just give up
  offset)

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
(defmethod all-offsets (x) (declare (ignorable x))  nil)

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
(defmethod begin-offset (obj) (declare (ignorable obj)) nil)
(defmethod begin-offset ((obj new-clang-ast))
  (begin-offset (aget :range (new-clang-ast-attrs obj))))
(defmethod begin-offset ((obj new-clang-range))
  (offset (new-clang-range-begin obj)))

(defgeneric end-offset (obj))
(defmethod end-offset (obj) (declare (ignorable obj)) nil)
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
  (:method ((x integer)) (declare (ignorable x)) 0)
  (:method (x) (declare (ignorable x)) nil))

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
              (when-let ((end-offset (offset end))
                         (tok-len (tok-len end)))
                (+ end-offset tok-len))))))
(defmethod begin-and-end-offsets ((obj null))
  (declare (ignorable obj))
  (values nil nil))

(defun extended-end-offset (x)
  (nth-value 1 (begin-and-end-offsets x)))

(defun token (loc)
  (when-let ((offset (offset loc))
             (len (tok-len loc)))
    (subseq (genome *soft*) offset (+ offset len))))

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

(defgeneric compute-operator-positions (sw ast)
  (:documentation "Compute positions of operators in
CXXOperatorCallExpr nodes")
  (:method (sw (ast new-clang-ast))
    (let ((*soft* sw))
      (map-ast ast #'compute-operator-position))))

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
    (let ((*soft* sw))
      (map-ast ast #'put-operator-into-inner-position))))

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

(defgeneric put-operators-into-starting-positions (sw ast)
  (:documentation "Put operators into their starting positions
in CXXOperatorCallExpr nodes.")
  (:method ((sw new-clang) (ast new-clang-ast))
    (let ((*soft* sw))
      (map-ast ast #'put-operator-into-starting-position))))

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

(defun decorate-ast-with-strings (sw ast &aux (*soft* sw)
                                           (genome (genome sw)))
  ;; get ast from sw?
  (labels
      ((%assert1 (i cbegin c)
         (assert (>= cbegin i) ()
                 "Offsets out of order: i = ~a,~
                  cbegin = ~a, c = ~a, range = ~a"
                 i cbegin c
                 (ast-attr c :range)))
       (%decorate (a)
         ;; At ast node A, split the parts of the source
         ;; that are not in the children into substrings
         ;; that are placed between the children.  Do not
         ;; place strings for children for whom offsets
         ;; cannot be computed
;;; (format t "Decorating ~a~%" a)
         (let ((children (ast-children a)))
           (multiple-value-bind (begin end)
               (begin-and-end-offsets a)
;;; (format t "B/E offsets of ~a: ~a ~a~%" a begin end)
             (when (and begin end)
               (let ((i begin))
                 (setf
                  (ast-children a)
                  (nconc
                   (iter
                    (for c in children)
                    (when (ast-p c)
                      (multiple-value-bind (cbegin cend)
                          (begin-and-end-offsets c)
                        (when cbegin
                          (%assert1 i cbegin c)
;;; (format t "Collecting ~a to ~a for ~a~%" i cbegin c)
                          (collect (subseq genome i cbegin))
                          (setf i cend))))
                    (collect c))
                   (progn
;;; (format t "Collecting ~a to ~a at end of ~a~%" i end a)
                     (list (subseq genome i end)))))))))))
    (map-ast ast #'%decorate))
  ast)

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
        (setf pos (begin-offset prev))
        #+debug-fix-vardecl (format t "Starting var: name = ~a~%"
                                    (ast-name prev)))
      (do* ((e (cdr child-asts) (cdr e))
            (c (car e) (car e)))
           ((null e))
        (if (ast-p c)
            (let ((next-pos (begin-offset c))
                  (end (end-offset c)))
              (if (member (ast-class c) '(:var :field))
                  (progn
                    #+debug-fix-vardecl
                    (progn
                      (format t "Next var: name = ~a~%" (ast-name c))
                      (format t "prev = ~a, pos = ~a, (end-offset prev)~
                                 = ~a, c = ~a, next-pos = ~a, end = ~a~%"
                              prev pos (end-offset prev) c next-pos end))
                    (if prev
                        (if (and next-pos end)
                            (if (< (end-offset prev) next-pos)
                                ;; things are fine -- no overlap
                                (progn
                                  #+debug-fix-vardecl
                                  (format t "Fine -- no overlap~%")
                                  (setf prev c
                                        pos next-pos))
                                ;; There is overlap -- find the next
                                ;; position
                                (let ((comma-pos
                                       (cpp-scan (genome sw)
                                                 (lambda (c) (eql c #\,))
                                                 :start pos
                                                 :end (1+ end))))
                                  #+debug-fix-vardecl (format t "Overlap~%")
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
                   ;; (format t "Expanding range of ~a from (~a,~a)
                   ;; to (~a,~a)~%" a begin end min-begin max-end)
                   (setf changed? t)
                   (setf (ast-attr a :range)
                         (make-new-clang-range :begin min-begin
                                               :end max-end)))))))
      ;; Fixpoint for normalization of ranges
      (loop
         (setf changed? nil)
         (map-ast ast #'%normalize)
         (map-ast-postorder ast #'%normalize)
         (unless changed? (return ast))))))

;;; FIXME: refactor this into small functions, for
;;;  understandability and line length limits
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
                           (let ((new-begin (reduce #'min accumulator
                                                    :key #'begin-offset))
                                 (new-end (reduce #'max accumulator
                                                  :key #'extended-end-offset)))
                             (prog1
                                 (if (eql new-begin new-end)
                                     (progn
                                       #+cos-debug
                                       (format t "No combination needed~&")
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
                                       ;; Special case: there are two nodes, and the second is a typedef
                                       ;; In that case, make the first a child of the second
                                       (if (and (= (length accumulator) 2)
                                                (eql (ast-class (cadr accumulator)) :typedef))
                                           (progn
                                             (push (first accumulator) (ast-children (cadr accumulator)))
                                             (list (cadr accumulator)))
                                           (list (make-new-clang-ast
                                                  :class :combined
                                                  :attrs `((:range . ,(make-new-clang-range
                                                                       :begin new-begin
                                                                       :end new-end))
                                                           (:subsumed . ,accumulator)))))))
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
                                         (format t "Adding (~a,~a):~%~a~&"
                                                 cbegin cend
                                                 (subseq genome cbegin cend)))
                                       (setf accumulator
                                             (append accumulator (list c))
                                             end (max end cend)))
                                     (progn
                                       #+cos-debug
                                       (format
                                        t "No overlap: cbegin = ~a, cend = ~a~%"
                                        cbegin cend)
                                       (appending (%combine))
                                       (setf accumulator (list c)
                                             end cend)))))
                         (%combine))))
                   (when changed?
                     #+cos-debug (format
                                  t "Old-children: ~a~%New-children: ~a~%"
                                  (ast-children a)
                                  new-children)
                     (setf (ast-children a) new-children)))))
             #+cos-debug (format t "Leave %check on ~a~%" a)))
      (map-ast ast #'%check))))

(defgeneric record-typedef-decls (obj ast)
  (:method ((obj new-clang) (ast new-clang-ast))
    (declare (ignore obj))
    (map-ast ast #'record-typedef-decl)))

(defgeneric record-typedef-decl (ast)
  (:method ((ast new-clang-ast))
    (when (eql (ast-class ast) :Typedef)
      (when-let ((type (ast-type ast)))
        (when-let* ((qual (ast-name ast))
                    (desugared (or (new-clang-type-desugared type)
                                   (new-clang-type-qual type))))
          #+(or)
          (format t "Record typedef for ~a ==> ~a, ~a~%"
                  (ast-name ast) (new-clang-type-qual type) desugared)
          (let ((nct+ (make-new-clang-type :qual qual :desugared desugared)))
            (setf (new-clang-typedef nct+) type)))))))

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
               nil)
             (%p (o)
               (let ((r (ast-range o)))
                 (list r (begin-offset r) (end-offset r)))))
        (let ((*canonical-string-table* (make-hash-table :test 'equal)))
          (multiple-value-bind (json tmp-file genome-len)
              (clang-json obj)
            (setf (tmp-file obj) tmp-file)
            (let* ((raw-ast (clang-convert-json-for-file
                             json tmp-file genome-len)))
              (%debug 'clang-convert-json-for-file raw-ast)
              ;; Store name -> def mappings
              (maphash
               (lambda (k v)
                 (declare (ignore k))
                 (dolist (a v)
                   (when (and a (ast-name a))
                     (push a (gethash (ast-name a)
                                      (name-symbol-table obj))))))
               (symbol-table obj))
              (record-typedef-decls obj raw-ast)
              (let ((ast (remove-non-program-asts raw-ast tmp-file)))
                (%debug 'remove-non-program-asts ast)
                (remove-asts-in-classes
                 ast '(:fullcomment :textcomment :paragraphcomment))
                (compute-operator-positions obj ast)
                (put-operators-into-inner-positions obj ast)
                (fix-overlapping-vardecls obj ast)
                (multiple-value-bind (uses table) (find-macro-uses obj ast tmp-file)
                  (declare (ignorable table uses))
                  ;; (format t "~{~s~%~}" uses)
                  (encapsulate-macro-expansions-cheap ast table))
                ;; Fancier macro mechanism
                #+(or)
                (progn
                  (mark-macro-expansion-nodes ast tmp-file)
                  (%debug 'mark-macro-expansion-nodes ast
                          #'(lambda (o)
                              (let ((r (ast-range o)))
                                (list r (begin-offset r) (end-offset r)
                                      (is-macro-expansion-node o)))))
                  (encapsulate-macro-expansions ast)
                  (%debug 'encapsulate-macro-expansions ast #'%p))
                (fix-ancestor-ranges obj ast)
                (%debug 'fix-ancestor-ranges ast #'%p)
                (combine-overlapping-siblings obj ast)
                (%debug 'combine-overlapping-siblings ast #'%p)
                (decorate-ast-with-strings obj ast)
                (put-operators-into-starting-positions obj ast)
                (compute-full-stmt-attrs ast)
                (compute-guard-stmt-attrs ast)
                (compute-syn-ctxs ast)
                (setf ast-root (sel/sw/parseable::update-paths
                                (fix-semicolons ast))
                      genome nil)
                (update-symbol-table obj)
                obj))))))))

;;; TODO: determine if this needs to be invoked when the
;;;  tree changes
(defmethod update-symbol-table ((obj new-clang))
  ;; When objects are copied in an AST, the mapping from IDs to
  ;; objects is invalidated.  This function restores it.
  (let ((table (symbol-table obj)))
    (map-ast (ast-root obj)
             (lambda (a)
               (when-let ((id (new-clang-ast-id a)))
                 (setf (gethash id table) (list a))))))
  obj)


;;; Macro expansion code

(defgeneric find-macro-uses (obj a file)
  (:documentation "Identify the locations at which macros occur in the
file, and the length of the macro token.  Returns a list of lists.
The first element of the list is the position of the start of the
macro name, the second is the length, the third is true
when the macro has one or more macro parameters that became expressed
in the AST, and the fourth is the length of the macro expression in
genome.   Also return a hash table in which the first element
of these lists maps to the cdr."))

(defmethod find-macro-uses (obj a file)
  (let ((macro-occurrence-table (make-hash-table))
        (uses nil))
    (map-ast a (lambda (x) (find-macro-use-at-node
                            x file macro-occurrence-table)))
    (maphash (lambda (k v)
               (setf (third v)
                     (compute-macro-extent obj k (first v) (second v)))
               (push (cons k v) uses))
             macro-occurrence-table)
    (values (sort uses #'< :key #'car)
            macro-occurrence-table)))

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

(defun find-macro-use-at-node (a file table)
  (when-let ((range (ast-range a)))
    (flet ((%record (loc)
             (typecase loc
               (new-clang-macro-loc
                (when-let ((eloc (new-clang-macro-loc-expansion-loc loc)))
                  (when (let ((loc-file (new-clang-loc-file eloc)))
                          (or (null loc-file) (equal loc-file file)))
                    (let ((off (offset eloc))
                          (len (tok-len eloc))
                          (is-arg (new-clang-macro-loc-is-macro-arg-expansion
                                   loc)))
                      (let ((existing (gethash off table)))
                        (if (null existing)
                            (setf (gethash off table)
                                  (list len is-arg nil))
                            (setf (cadr existing) (or (cadr existing)
                                                      is-arg)))))))))))
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
                                (let ((obj (make-new-clang-ast
                                            :class :macroexpansion)))
                                  (assert m)
                                  (let ((new-begin (car m))
                                        (new-end (+ (car m) (fourth m))))
                                    (setf (ast-range obj)
                                          (make-new-clang-range
                                           :begin new-begin
                                           :end new-end)
                                          (ast-attr obj :macro-child-segment)
                                          macro-child-segment
                                          changed? t))
                                  obj)))
                             (setf m nil
                                   macro-child-segment nil
                                   last-offset nil)))
                      (if c
                          (let ((x (pop c)))
                            (let ((macro-info (%is-macro-expansion-node x table)))
                              (if macro-info
                                  (if (eql (car macro-info) last-offset)
                                      (setf macro-child-segment
                                            (append macro-child-segment (list x)))
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
    #+mme-debug (format t "range = ~a,~%begin = ~a,~%end = ~a~%"
                        range begin end)
    (cond
      ((and (typep begin 'new-clang-macro-loc)
            (typep end 'new-clang-macro-loc)

            (or #+mme-debug
                (and
                 (not (new-clang-macro-loc-is-macro-arg-expansion end))
                 (progn (format t "isMacroArgExpansion is false~%")
                        t))
                (and (not (equal file (ast-file a t)))
                     (progn #+mme-debug
                            (format t "file = ~a, (ast-file a t) = ~a~%"
                                    file (ast-file a t))
                            t))
                ;; This detect if there is stuff here from the macro which must
                ;; be at an earlier line.  However, it didn't detect the case
                ;; of an identity macro, which introduces nothing new.  Added
                ;; a case where lines are the same but the columns detect
                ;; macro expansion
                (let* ((slb (new-clang-macro-loc-spelling-loc begin))
                       (spelling-line (new-clang-loc-line slb)))
                  (and (or (< spelling-line  *current-line*)
                           (and (= spelling-line *current-line*)
                                (> (new-clang-loc-col slb)
                                   (new-clang-loc-col
                                    (new-clang-macro-loc-expansion-loc
                                     begin)))
                                ;; Mark the node so we know it was a
                                ;; direct macro arg
                                (setf (ast-attr a :direct-macro-arg) t)))
                       (progn
                         #+mme-debug
                         (format t "~a = ~a, *current-line* = ~a~%"
                                 "(new-clang-loc-line (ncmlsl begin))"
                                 (new-clang-loc-line
                                  (new-clang-macro-loc-spelling-loc begin))
                                 *current-line*)
                         t)))))
       ;; It's from a macro, mark it
       #+mme-debug (format t "Marking: ~a~%" a)
       ;; Compute the macro name at the expansion site
       (when-let* ((eloc (new-clang-macro-loc-expansion-loc begin))
                   (tok (token eloc)))
         (setf (ast-attr a :macro-names) (list tok)))
       (setf (ast-attr a :from-macro) t))
      ((typep begin 'new-clang-loc)
       (setf *current-line* (max *current-line*
                                 (or (new-clang-loc-line begin) 0))))
      ((typep begin 'new-clang-macro-loc)
       (when-let* ((spelling-loc (new-clang-macro-loc-spelling-loc begin))
                   (begin-line (new-clang-loc-line spelling-loc)))
         (setf *current-line* (max *current-line* begin-line)))))))

(defun encapsulate-macro-expansions (a)
  "Given an AST marked with :from-macro annotations, collapse macros into macro
nodes."
  (map-ast-while a #'encapsulate-macro-expansions-below-node))

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
    (setf (ast-range a)
          (macro-range-to-non-macro-range/expansion (ast-range a)))
    ;; Must combine subsequences of children that are macro expansion nodes
    ;; and have the same spelling loc begin into a single macroexpansion node
    (let* ((children (ast-children a))
           (new-children
            (iter
             (while children)
             (if (or (not (ast-p (car children)))
                     (not (is-macro-expansion-node (car children))))
                 (collect (pop children))
                 (collect
                  (let ((macro-child-segment
                         (iter
                          (collect (pop children))
                          (while children)
                          (while (ast-p (car children)))
                          (while (is-macro-expansion-node (car children))))))
                    (let ((macro-args (macro-expansion-arg-asts
                                       macro-child-segment)))
                      #+mme-debug
                      (format t
                              "Create :MACROEXPANSION node with children ~a~%"
                              macro-args)
                      (let ((obj (make-new-clang-ast
                                  :class :macroexpansion
                                  :children macro-args)))
                        (setf (ast-range obj)
                              (macro-range-to-non-macro-range/expansion
                               (ast-range (car macro-child-segment)))
                              (ast-attr obj :macro-child-segment)
                              macro-child-segment
                              changed? t)
                        obj))))))))
      (when changed?
        (shiftf (ast-attr a :old-children)
                (ast-children a)
                new-children)
        new-children))
    (not changed?)))

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
        (if (ast-attr a :direct-macro-arg)
            (progn
              (setf (ast-attr a :from-macro) nil)
              (push a children))
            (mapc #'%traverse (ast-children a)))))
    (nreverse children)))


;;; Reference-related code

(defgeneric ast-referenced-obj (obj)
  (:documentation "The object referenced by the reference attribute
of OBJ")
  (:method ((obj new-clang-ast))
    (let ((rd (ast-attr obj :ReferencedDecl)))
      (when rd
        (unless (ast-p rd)
          ;; This should never happen
          (format t "In ast-referenced-obj on ~a~%" obj)
          (describe obj)
          (format t "ast-attrs = ~a~%" (new-clang-ast-attrs obj))
          (describe rd)
          (error "Not an AST: ~a~%" rd))
        (let ((id (new-clang-ast-id rd)))
          (when id
            (let ((defs (gethash id (symbol-table *soft*))))
              (when (= (length defs) 1)
                (car defs)))))))))


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
    ((:ParmVar :Function :Var :Field :Record :TypeDef)
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
  (ast-attr obj :full-stmt))

(defmethod full-stmt-p ((obj new-clang) (ast new-clang-ast))
  (declare (ignorable obj))
  (ast-full-stmt ast))

;; This field should be filled in by a pass
;; that marks AST nodes that are guard statements
;; (and that might not otherwise be)
(defmethod ast-guard-stmt ((obj new-clang-ast))
  (ast-attr obj :guard-stmt))

;;; 'includes' should be computed from the locations
;;; in the subtree.  Need logic to translate back to
;;; relative paths.

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
      :VarTemplateSpecialization ;; :VarTemplateSpecializationDecl
      )))

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
      (trim-right-whitespace (subseq s 0 (1- pos))))))

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
    ;; NOTE: Windows
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
