(defpackage :software-evolution-library/python/tree-sitter-interface
  (:nicknames :sel/py/ts-int)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/command-line
        :software-evolution-library/software/parseable
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/python
        :software-evolution-library/software/json
        :software-evolution-library/utility/range)
  (:export :run-tree-sitter-interface))
(in-package :software-evolution-library/python/tree-sitter-interface)
(in-readtable :curry-compose-reader-macros)

;;;; Command line interface:
(defvar *external-asts* (make-hash-table)
  "Mapping of hashes to (AST . refcount) pairs for externally referenced ASTs.")

(declaim (inline safe-intern))
(defun safe-intern (string) (intern (string-upcase string) *package*))

(-> ast-key (functional-tree-ast) fixnum)
(defun ast-key (ast)
  "Return a hash key (non-negative fixnum) for the functional-tree-ast.
 This differs from ast-hash, in that two distinct asts which are
 equal?, but not equal, will get distinct hash keys.
 Example: (let* ((ast1 (make-instance 'functional-tree-ast))
                 (ast2 (copy ast1)))
            (values (= (ast-key ast1) (ast-key ast2))
                    (= (ast-hash ast1) (ast-hash ast2)))) => NIL, T"
  (sxhash ast))

(-> allocate-ast (ast) fixnum)
(defun allocate-ast (ast &aux (key (ast-key ast)))
  "Allocate AST to the *external-asts* hashtable and increment the reference
counter to allow for its use externally without garbage collection."
  (when (not (gethash key *external-asts*))
    (setf (gethash key *external-asts*) (cons ast 0)))
  (incf (cdr (gethash key *external-asts*)))
  key)

(-> deallocate-ast (ast) boolean)
(defun deallocate-ast (ast &aux (key (ast-key ast)))
  "Deallocate the AST from the *external-asts* hashtable if its reference
counter reaches zero."
  (when (gethash key *external-asts*)
    (let ((ref-count (decf (cdr (gethash key *external-asts*)))))
      (when (zerop ref-count)
        (remhash key *external-asts*)))))

(defun refcount (ast)
  "Return the reference count of the ast, or 0 if not found."
  (or (cdr (gethash (ast-key ast) *external-asts*)) 0))

;; (-> serialize (t) t)
(defgeneric serialize (it)
  (:documentation "Serialize IT to a form for use with the JSON text interface.")
  (:method :before ((it ast))
    (allocate-ast it))
  (:method ((it ast))
    `((:type . :ast) (:addr . ,(ast-key it))))
  (:method ((it list)) (mapcar #'serialize it))
  (:method ((it t)) it))

;; (-> deserialize (t) t)
(defgeneric deserialize (it)
  (:documentation "Deserialize IT from a form used with the JSON text interface.")
  (:method ((it list))
    (if (aget :addr it)
        (car (gethash (aget :addr it) *external-asts*))
        (mapcar #'deserialize it)))
  (:method ((it t)) it))

(-> handle-interface (list) t)
(defun handle-interface (json)
  "Handle a JSON input from the INTERFACE.  The JSON list should start with a
function name from the API followed by the arguments."
  (destructuring-bind (function-str . arguments) json
    (serialize
     (apply (safe-intern (concatenate 'string "INT/" (string-upcase function-str)))
            (mapcar #'deserialize arguments)))))

(define-command tree-sitter-interface (&spec (append +common-command-line-options+
                                                     +interactive-command-line-options+))
  "Tree-sitter command-line interface."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose load eval language manual))
  (when help (show-help-for-tree-sitter-interface) (exit-command tree-sitter-interface 0))
  (loop :for line := (read-line) :until (equalp line "QUIT")
     :do (format *standard-output* "~a~%"
                 (nest (encode-json-to-string)
                       (handle-interface)
                       (decode-json-from-string line)))))

;;;; API:
(-> int/ast (string string) (or ast nil))
(defun int/ast (language source-text)
  (handler-case
      (convert (safe-intern (concatenate 'string (string-upcase language) "-AST"))
               source-text)
    (error (e) (declare (ignorable e)) nil)))

(-> int/init (list) (or fixnum nil))
(defun int/init (&rest args)
  (when-let ((ast (apply #'int/ast args)))
    (allocate-ast ast)))

(-> int/del (ast) boolean)
(defun int/del (ast)
  (deallocate-ast ast))

(-> int/hash (ast) number)
(defun int/hash (ast)
  (ast-hash ast))

(-> int/eq (ast ast) boolean)
(defun int/eq (ast1 ast2)
  (equal? ast1 ast2))

(-> int/parent (ast ast) ast)
(defun int/parent (root ast) (get-parent-ast root ast))

(-> int/children (ast) list)
(defun int/children (ast) (children ast))

(-> int/source-text (ast) string)
(defun int/source-text (ast) (source-text ast))

(defun int/child-slots (ast)
  (mapcar «list [#'symbol-name #'car] #'cdr» (child-slots ast)))

(-> int/child-slot (ast string) (or list ast))
(defun int/child-slot (ast slot-name) (slot-value ast (safe-intern slot-name)))

(-> int/ast-at-point (ast integer integer) ast)
(defun int/ast-at-point (ast line column)
  (lastcar (asts-containing-source-location ast
                                            (make-instance 'source-location
                                              :line line :column column))))

(-> int/child-slots (ast) list)
(-> int/ast-type (ast) string)
(defun int/ast-type (ast) (symbol-name (type-of ast)))

(-> int/ast-types (ast) list)
(defun int/ast-types (ast)
  (labels ((int/ast-types-helper (clazz)
             (unless (eq (class-name clazz) t)
               (cons (symbol-name (class-name clazz))
                     (mappend #'int/ast-types-helper
                              (class-direct-superclasses clazz))))))
    (remove-duplicates (int/ast-types-helper (find-class (type-of ast)))
                       :test #'equal)))

(-> int/ast-language (ast) string)
(defun int/ast-language (ast)
  (etypecase ast
    (sel/sw/ts::python-ast "PYTHON")
    (sel/sw/ts::javascript-ast "JAVASCRIPT")
    (sel/sw/ts::c-ast "C")
    (sel/sw/ts::cpp-ast "CPP")))

(-> int/function-asts (ast) list)
(defun int/function-asts (ast)
  (remove-if-not {typep _ 'function-ast} (convert 'list ast)))

(-> int/call-asts (ast) list)
(defun int/call-asts (ast)
  (remove-if-not {typep _ 'call-ast} (convert 'list ast)))

(-> int/function-name (ast) string)
(defun int/function-name (ast) (function-name ast))

(-> int/function-parameters (ast) list)
(defun int/function-parameters (ast) (function-parameters ast))

(-> int/function-body (ast) ast)
(defun int/function-body (ast) (function-body ast))

(-> int/call-function (ast) ast)
(defun int/call-function (ast) (call-function ast))

(-> int/call-module (ast ast) string)
(defun int/call-module (root ast) (provided-by root ast))

(-> int/call-arguments (ast) list)
(defun int/call-arguments (ast) (call-arguments ast))

(-> int/ast-refcount (ast) string)
(defun int/ast-refcount (ast)
  (format nil "~D" (refcount ast)))
