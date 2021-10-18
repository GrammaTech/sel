(defpackage :software-evolution-library/software/tree-sitter-dev
  (:use :software-evolution-library/software/tree-sitter)
  (:documentation "Load this package for tree-sitter dev and debug tools."))
(in-package :software-evolution-library/software/tree-sitter)

(defvar *grammar* nil
  "JSON list representation of the grammar.")
(defvar *rules* nil
  "The rules of the grammar in JSON.")
(defvar *transformed* nil
  "The transformed representation of the rules")
(defvar *types* nil
  "The types associated with the grammar.")
(defvar *pruned-rules* nil
  "The pruned verson of the rules")
(defvar *collapsed-rules* nil
  "The collapsed representation of the rules.")
(defvar *expanded-rules* nil
  "The epanded versions of each rule.")

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

(defun set-grammar (name)
  (setf *grammar* (dump-tree-sitter-grammar-json name)))

(defun set-rules (name)
    (setf *rules* (aget :rules (set-grammar name))))

(defun set-transformed-json (name)
  (setf *transformed*
        (mapcar (lambda (rule &aux (rule-name (car rule)))
                  (cons rule-name
                        (transform-json-rule
                         (cdr rule) *grammar* (list rule-name))))
                (set-rules name))))

(defun set-types (name)
  (setf *types*
        (destructuring-bind (name grammar-file nodes-file)
            (or (find name
                      *tree-sitter-language-files*
                      :key 'car :test #'string-equal)
                (error "Unknown language: ~a" name))
          (declare (ignore name grammar-file))
          (decode-json-from-source (pathname nodes-file)))))

(defun set-pruned-rules (name)
  (setf *pruned-rules*
        (mapcar (op (list (car _) (prune-rule-tree (cdr _1))))
                (set-transformed-json name))))

(defun set-collapsed-rules (name)
  (setf *collapsed-rules*
        (mapcar (op (list (car _) (collapse-rule-tree (cadr _1))))
                (set-pruned-rules name))))

#+broken
(defun set-expanded-rules (name)
  (set-collapsed-rules name)
  (setf *expanded-rules*
        (mapcar (lambda (json)
                  (expand-choice-branches (aget (car json) *pruned-rules*) (cdr json)))
                *transformed*)))

(defun set-everything (name)
  "Set all of the special variables to their relevant value."
  #+broken
  (set-expanded-rules name)
  (set-types name)
  (set-collapsed-rules name))

(defun get-problematic-rules (name)
  "Get the problematic rules in the grammar specified by NAME."
  (mapcar (lambda (collapsed pruned)
            ;; This will map rules to whether they're problematic or not.
            (list (car collapsed)
                  (structured-rule-p (cadr collapsed) (cadr pruned))))
          (set-collapsed-rules name)
          *pruned-rules*))
