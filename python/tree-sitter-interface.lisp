(defpackage :software-evolution-library/python/tree-sitter-interface
  (:use :gt/full
        :cl-base64
        :software-evolution-library
        :software-evolution-library/command-line
        :software-evolution-library/software/parseable
        :software-evolution-library/software/tree-sitter)
  (:export :run-tree-sitter-interface))
(in-package :software-evolution-library/python/tree-sitter-interface)
(in-readtable :curry-compose-reader-macros)

(defvar external-asts (make-hash-table)
  "Hold onto ASTs which might be referenced externally.")

(declaim (inline safe-intern))
(defun safe-intern (string) (intern (string-upcase string) *package*))

(defgeneric encode-interface (it)
  (:documentation "Encode IT for passing through the interface.")
  (:method ((it ast) &aux (hash (ast-hash it)))
    (setf (gethash hash external-asts) it)
    (format nil "~d" hash))
  (:method ((it string)) (string-to-base64-string it))
  (:method ((it integer)) (format nil "~d" it))
  (:method ((it list)) (string-join (mapcar #'encode-interface it) #\Comma)))

(defmethod encode-interface :around (it)
  (concatenate 'string
               (etypecase it
                 (ast "AST")
                 (string "STR")
                 (list "LST")
                 (integer "INT"))
               ":"
               (call-next-method)))

(-> decode-interface (string) (or ast number string list))
(defun decode-interface (string)
  (let ((key (subseq string 0 3))
        (payload (subseq string 4)))
    (string-case key
      ("STR" (base64-string-to-string payload))
      ("AST" (gethash (parse-integer payload) external-asts))
      ("LST" (mapcar #'decode-interface (split-sequence #\Comma payload)))
      ("INT" (parse-integer payload)))))

(-> handle-interface (string) string)
(defun handle-interface (line)
  "Handle a line of input from the INTERFACE."
  (destructuring-bind (function-str . argument-strs) (split-sequence #\Space line)
    (encode-interface
     (apply (safe-intern (concatenate 'string "INT/" function-str))
            (mapcar #'decode-interface argument-strs)))))

(define-command tree-sitter-interface (&spec (append +common-command-line-options+
                                                     +interactive-command-line-options+))
  "Tree-sitter command-line interface."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose load eval language manual))
  (when help (show-help-for-tree-sitter-interface) (exit-command tree-sitter-interface 0))
  (loop :for line := (read-line) :until (string= line "QUIT")
     :do (princ (handle-interface line))))


;;;; API:

(-> int/ast (string string) ast)
(defun int/ast (language source-text)
  (convert (safe-intern (concatenate 'string (string-upcase language) "-AST"))
           source-text))

(-> int/children (ast) list)
(defun int/children (ast) (children ast))

(-> int/source-text (ast) string)
(defun int/source-text (ast) (source-text ast))

(-> int/ast-at-point (ast integer integer) ast)
(defun int/ast-at-point (ast column row)
  (car (asts-containing-source-location ast
                                        (make-instance 'source-location
                                          :column column :line row ))))

(-> int/child-slots (ast) list)
(defun int/child-slots (ast) (mapcar «list #'car #'cdr» (child-slots ast)))

(-> int/child-slot (ast string) (or list ast))
(defun int/child-slot (ast slot-name) (slot-value ast (safe-intern slot-name)))

(-> int/ast-type (ast) string)
(defun int/ast-type (ast) (symbol-name (type-of ast)))

(-> int/ast-language (ast) string)
(defun int/ast-language (ast)
  (etypecase ast
    (sel/sw/ts::python-ast "PYTHON")
    (sel/sw/ts::javascript-ast "JAVASCRIPT")
    (sel/sw/ts::c-ast "C")
    (sel/sw/ts::cpp-ast "CPP")))

(-> int/function-name (ast) string)
(defun int/function-name (ast) (function-name ast))

(-> int/function-parameters (ast) list)
(defun int/function-parameters (ast) (function-parameters ast))

(-> int/function-body (ast) ast)
(defun int/function-body (ast) (function-body ast))

(-> int/call-arguments (ast) list)
(defun int/call-arguments (ast) (call-arguments ast))

(-> int/call-function (ast) ast)
(defun int/call-function (ast)
  ;; (call-function ast) ; TODO: implement.
  ast)

(-> int/ast-parent (ast ast) ast)
(defun int/ast-parent (root ast) (get-parent-ast root ast))

(-> int/enclosing-function (ast ast) ast)
(defun int/enclosing-function (root ast) (enclosing-definition root ast))

(-> int/in-scope-names (ast ast) list)
(defun int/in-scope-names (root ast) (mapcar {aget :name} (get-vars-in-scope root ast)))

(-> int/defined-functions (ast) list)
(defun int/defined-functions (ast)
  (remove-if-not {typep _ 'function-ast} (outer-declarations ast)))

(-> int/callsites (ast ast) list)
(defun int/callsites (root ast)
  (remove-if-not [{equalp (function-name ast)} #'call-name]
                 (remove-if-not {typep _ 'call-ast} root)))

(-> int/callsite-signatures (ast) list)
(defun int/callsite-signatures (root)
  (mapcar «cons #'call-name [{mapcar #'variable-name} #'call-arguments]»
          (remove-if-not {typep _ 'call-ast} root)))

(-> int/callsite-module (ast ast) string)
(defun int/callsite-module (root ast) (provided-by root ast))
