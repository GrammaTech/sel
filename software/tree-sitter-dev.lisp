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

(defun update-class-allocated-slots ()
  "For all classes exported by sel/sw/ts, update any class-allocated slots according to their initforms.

\(Note that in CL class-allocated slots are *not* reinitialized if the
class definition changes.)

Return the number of slots re-initialized."
  (let* ((symbols (package-exports :sel/sw/ts))
         (classes (filter-map (op (find-class _ nil)) symbols))
         (update-count 0))
    (dolist (class classes update-count)
      (finalize-inheritance class)
      (when-let* ((slot-defs (class-slots class))
                  (class-slot-defs
                   (keep :class slot-defs :key #'slot-definition-allocation))
                  (instance (allocate-instance class)))
        (dolist (slot-def class-slot-defs)
          (and-let* ((slot (slot-definition-name slot-def))
                     ((slot-boundp instance slot))
                     ;; This is wrong in the general case (the
                     ;; initform should be evaluated in its original
                     ;; lexical environment) but all the initforms
                     ;; used by tree-sitter are constant so it doesn't
                     ;; matter here.
                     (initform (eval (slot-definition-initform slot-def)))
                     ((not (equal initform (slot-value instance slot)))))
            (setf (slot-value instance slot) initform)
            (incf update-count)))))))

(defun get-ts-files (dir)
  "List TypeScript files in DIR, excluding test files, node_modules,
and .d.ts files."
  (mapcar #'pathname
          (lines (cmd:$cmd "find" (ensure-directory-pathname dir)
                           "-name '*.ts' | grep -Ev"
                           (list "node_modules|/test|\.d\.ts$")))))

(defun test-project-parsing (files)
  "Attempt to parse and print every file in FILES, proving a restart
to advance to the next file."
  (dolist (file files)
    (with-simple-restart (continue "Next")
      (stefil:finishes
       (source-text (genome (from-file 'typescript file)))))))

(defun problematic-classes (files)
  "Parse and print FILES, collecting a list of problematic classes."
  (hash-table-alist
   (frequencies
    (with-collectors (collect)
      (handler-bind ((sel/sw/ts::parse-tree-matching-error
                      (lambda (e)
                        (collect
                         (sel/sw/ts::parse-tree-matching-error-superclass
                          e))
                        (invoke-restart 'continue)))
                     (sel/sw/ts::rule-matching-error
                      (lambda (e)
                        (collect
                         (type-of
                          (sel/sw/ts::rule-matching-error-ast
                           e)))
                        (invoke-restart 'continue))))
        (test-project-parsing files))))))
