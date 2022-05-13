;;;;
;;;; File:     error-char-count.lisp
;;;; Contents: Utility functions for analyzing characters coming from error-asts
;;;;           vs. full count of characters.
;;;;
;;;; Examples:
;;;;   (error-char-count <path-to-C-software>)
;;;;   (error-char-count "~/benchmark/busybox/")
;;;;   (error-char-count "~/benchmark/grep-single-file/grep.c")
;;;;
(defpackage :software-evolution-library/tools/error-char-count
  (:nicknames :sel/tools/error-char-count)
  (:use
   :gt/full
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/components/file
   :software-evolution-library/software/project
   :software-evolution-library/command-line)
  (:export :ast-error-char-count :error-char-count))
(in-package :software-evolution-library/tools/error-char-count)
(in-readtable :curry-compose-reader-macros)

(defun ast-error-char-count (ast &key print-error-asts)
  "Traverse tree-sitter-ast, looking only at leaf nodes
 (where (children <ast>) is nil.
 Returns two values:
 (1) total character count (of leaves)
 (2) total character count of error asts"
  (let ((char-count 0)
        (error-char-count 0))
    (mapc
     (lambda (x)
       (let ((char-length (length (source-text x))))
         (when (typep x '(or parse-error-ast source-text-fragment))
           (if print-error-asts
               (format t "Error ast: ~A Length: ~D~%" x char-length))
           (incf error-char-count char-length))
         (incf char-count
               (if (children x)
                   (reduce #'+
                           (remove-if-not #'stringp (output-transformation x))
                           :key #'length)
                   char-length))))
     ast)
    (values char-count error-char-count)))

(defun error-char-count (path &key print-error-asts)
  "Given a path to a software object, compute total number of text characters
 (for all evolve files, if it is a project), and total number of error asts
 characters.
 Outputs a report.
 If print-error-asts is true, error asts are written to the output."
  (let* ((soft (create-software path))
         (files (if (typep soft 'project)
                    (map 'list 'cdr (evolve-files soft))
                    (list soft)))
         (total-chars 0)
         (total-error-chars 0)
         (text (source-text soft))) ; force error propagation
    (dolist (x files)
      (multiple-value-bind (char-count error-char-count)
          (ast-error-char-count (genome x)
                                :print-error-asts print-error-asts)
        (incf total-chars char-count)
        (incf total-error-chars error-char-count)
        (format t "file: ~A total-chars: ~D error-chars: ~D~%"
                (file-namestring (original-path x))
                char-count
                error-char-count)))
    (format t "~%No. files: ~D~%Total chars: ~D~%Total error chars: ~D~%~
    Error%: ~F~%Source-text length: ~D~%"
            (length files)
            total-chars
            total-error-chars
            (* 100 (/ (float total-error-chars) (float total-chars)))
            (length text))))
