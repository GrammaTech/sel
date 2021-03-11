(defpackage :software-evolution-library/python/tree-sitter-interface
  (:use :gt/full
        :cl-base64
        :software-evolution-library/command-line
        :software-evolution-library/software/parseable
        :software-evolution-library/software/tree-sitter)
  (:export :run-tree-sitter-interface))
(in-package :software-evolution-library/python/tree-sitter-interface)
(in-readtable :curry-compose-reader-macros)

(defvar external-asts (make-hash-table)
  "Hold onto ASTs which might be referenced externally.")

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
     (apply (intern (concatenate 'string "INT:" function-str) *package*)
            (mapcar #'decode-interface argument-strs)))))

(define-command tree-sitter-interface ()
  "" ""
  (loop :for line := (read-line) :until (string= line "QUIT")
     :do (princ (handle-interface line))))


;;;; API:
;;; ast(language, string) -> ast-handle
;;; children(ast) -> [ast-handle]
;;; source_text(ast) -> string
;;; ast_at_point(ast, column, row) -> ast
;;; child_slots(ast) -> [(string, number)]
;;; child_slot(ast, string) -> [ast] or ast
;;; ast_type(ast) -> string
;;; ast_language(ast) -> string
;;; function_name(ast) -> string
;;; function_parameters(ast) -> [ast]
;;; function_body(ast) -> ast
;;; call_arguments(ast) -> [ast]
;;; call_function(ast) -> ast
;;; ast_parent(ast) -> ast
;;; enclosing_function(ast) -> ast
;;; in_scope_names(ast) -> [string]
;;; defined_functions(ast) -> [ast]
;;; callsites(ast) -> [ast]
;;; callsite_signatures(ast) -> [string]
;;; callsite_module(ast) -> string
