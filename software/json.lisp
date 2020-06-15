;;; json.lisp --- Json software representation.
;;;
;;; Implements AST parsing for JSON software objects.  JSON software
;;; objects are a very thin customization on top of JavaScript
;;; software objects.
;;;
;;; @texi{json}
(defpackage :software-evolution-library/software/json
  (:nicknames :sel/software/json :sel/sw/json)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/components/file
        :software-evolution-library/software/parseable
        :software-evolution-library/software/javascript)
  (:shadowing-import-from :cl-json :decode-json-from-string)
  (:export :json))
(in-package :software-evolution-library/software/json)
(in-readtable :curry-compose-reader-macros)

(define-software json (javascript)
  ()
  (:documentation "JSON software representation."))

(defun valid-json-ast-type? (type)
  (string-case type
    (("ObjectExpression"
      "ArrayExpression"
      "Literal")
     t)
    (t nil)))

(defun valid-json-ast? (ast)
  (valid-json-ast-type? (aget :type ast)))

(defmethod parse-asts ((obj json) &optional (source (genome-string obj)))
  (convert 'json-ast source))

(defmethod convert ((to-type (eql 'json-ast)) (string string)
                    &key &allow-other-keys)
  (nest (mapcar (lambda (node)
                  (decf (slot-value node 'start) 2)
                  (decf (slot-value node 'end) 2)
                  node))
        (js-right) (js-expression) (first) (js-body)
        (convert 'javascript-ast (concatenate 'string "x=" string))))
