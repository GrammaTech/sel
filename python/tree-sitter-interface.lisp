(defpackage :software-evolution-library/python/tree-sitter-interface
  (:nicknames :sel/py/ts-int)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/command-line
        :software-evolution-library/software/parseable
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/utility/range)
  (:export :run-tree-sitter-interface))
(in-package :software-evolution-library/python/tree-sitter-interface)
(in-readtable :curry-compose-reader-macros)


;;;; Command line interface:
(defvar *external-asts* (make-hash-table)
  "Hold onto ASTs which might be referenced externally.")

(declaim (inline safe-intern))
(defun safe-intern (string) (intern (string-upcase string) *package*))

;; (-> serialize (t) t)
(defgeneric serialize (it)
  (:documentation "Serialize IT to a form for use with the JSON text interface.")
  (:method ((it ast) &aux (hash (ast-hash it)))
    (setf (gethash hash *external-asts*) it)
    `((:type . :ast) (:hash . ,hash)))
  (:method ((it list)) (mapcar #'serialize it))
  (:method ((it t)) it))

;; (-> deserialize (t) t)
(defgeneric deserialize (it)
  (:documentation "Deserialize IT from a form used with the JSON text interface.")
  (:method ((it list))
    (if (aget :hash it)
        (gethash (aget :hash it) *external-asts*)
        (mapcar #'deserialize it)))
  (:method ((it t)) it))

(-> handle-interface (list) t)
(defun handle-interface (json)
  "Handle a JSON input from the INTERFACE.  The JSON list should start with a
function name from the API followed by the arguments."
  (destructuring-bind (function-str . arguments) json
    (serialize
     (apply (safe-intern (concatenate 'string "INT/" function-str))
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
(-> int/ast (string string) ast)
(defun int/ast (language source-text)
  (handler-case
      (convert (safe-intern (concatenate 'string (string-upcase language) "-AST"))
               source-text)
    (condition (c) (declare (ignorable c)) nil)))

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
