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
        :software-evolution-library/software/file
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

(defmethod parse-asts ((obj json))
  "Parse a JSON file (with acorn as JavaScript with a simple hack).
We do this by temporarily turning the JSON into a valid JavaScript
file by pre-pending the left hand side of an assignment.  We then
parse the resulting JavaScript into ASTs, extract the right hand side
of the assignment, and fix-up the :start and :end source range
pointers to adjust for the extra offset introduced by the added left
hand side."
  (with-temporary-file-of (:pathname src-file :type (ext obj))
      (concatenate 'string "x=" (genome obj))
    (multiple-value-bind (stdout stderr exit)
        (shell "acorn ~a" src-file)
      (unless (zerop exit)
        (error
         (make-instance 'mutate
           :text (format nil "acorn exit ~d~%stderr:~s"
                         exit
                         stderr)
           :obj obj :operation :parse)))
      (let* ((raw (decode-json-from-string stdout))
             (real-end (aget :end raw))
             (expr (aget :right (aget :expression (car (aget :body raw))))))
        (setf (aget :end expr) real-end)
        (assert (and expr (valid-json-ast? expr))
		(obj) "Object ~s is not valid JSON" obj)
        ;; Reduce every ::start and :end value by two to makeup for
        ;; the appended "x=" above.
        (labels ((push-back (value tree)
                   (cond
                     ((proper-list-p tree) (mapcar {push-back value} tree))
                     ((and (consp tree) (or (eql :start (car tree))
                                            (eql :end (car tree))))
                      (cons (car tree) (- (cdr tree) value)))
                     (t tree))))
          (push-back 2 expr))))))
