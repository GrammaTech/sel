(defpackage :software-evolution-library/software/tree-sitter-dev
  (:use :software-evolution-library/software/tree-sitter)
  (:documentation "Load this package for tree-sitter dev and debug tools."))
(in-package :software-evolution-library/software/tree-sitter)

(defun dump-tree-sitter-grammar-json (name
                                      &aux (*json-identifier-name-to-lisp*
                                            #'convert-name))
  "Dump the grammar for language NAME."
  (destructuring-bind (name grammar-file nodes-file)
      (or (find name
                *tree-sitter-language-files*
                :key 'car :test #'string-equal)
          (error "Unknown language: ~a" name))
    (declare (ignore name nodes-file))
    (decode-json-from-source (pathname grammar-file))))

#+nil
(let ((*json-identifier-name-to-lisp* #'convert-name))
  (setf rules
        (aget :rules
              (setf grammar (decode-json-from-string (file-to-string "/usr/share/tree-sitter/c/grammar.json"))))))

#+nil
(setf transformed-json
      (mapcar (lambda (rule)
                (cons (car rule) (transform-json-rule (cdr rule) grammar)))
              rules))

#+nil
(setf types (decode-json-from-string (file-to-string "/usr/share/tree-sitter/c/node-types.json")))

#+nil
(setf pruned (mapcar (op (list (car _) (prune-rule-tree (cdr _1))))  transformed-json))

#+nil
(setf collapsed (mapcar (op (list (car _) (collapse-rule-tree (cadr _1))))  pruned))

;;; TODO: this needs to be added into the code generation process and printed out
;;;       to *error-output* so that these issues are apparent.
#+nil
(mapcar (lambda (rule)
          ;; This will map rules to whether they're problematic or not.
          (list (car rule) (structured-rule-p (cadr rule))))
        collapsed)

#+nil
(setf expansions (mapcar (lambda (json)
                           (expand-choice-branches (aget (car json) pruned) (cdr json)))
                         transformed-json))

(defmacro defthings ()
  (generate-structured-text-methods grammar types :c (make-hash-table)))
