;;; test-parse.lisp -- command line utility to test parsing

(defpackage :software-evolution-library/components/test-parse
  (:nicknames :sel/components/test-parse
   :sel/cp/test-parse)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/command-line
        :cl-ppcre
        :software-evolution-library/software/parseable)
  (:import-from :software-evolution-library/software/tree-sitter
   :rule-matching-error :parse-error-ast)
  (:export :test-parse :run-test-parse))
(in-package :software-evolution-library/components/test-parse)
(in-readtable :curry-compose-reader-macros)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +command-line-options+
    '((("help" #\h #\?) :type boolean :optional t
                        :documentation "display help output")
      (("load" #\l) :type string
                    :action #'handle-load
                    :documentation "load FILE as lisp code")
      (("eval" #\e) :type string
                    :action #'handle-eval
                    :documentation "eval STRING as lisp code")
      (("error-type" #\E) :type string :action #'read-from-string
                          :documentation "type of errors to be recognized as interesting")
      (("error-pattern" #\P) :type string :action #'identity
                             :documentation "CL-PPCRE pattern for errors to be considered interesting")
      (("round-trip" #\r) :type boolean :optional t
                          :documentation "Attempt round trip comparison (ignoring whitespace)")
      (("idempotent" #\I) :type boolean :optional t
                          :documentation "Attempt to print, reread, then print, and confirm these are the same")
      (("language" #\L) :type string :initial-value "c"
                        :documentation
       "language of input files (e.g. c, c++, lisp, or javascript)"))))

(define-command test-parse (source &spec +command-line-options+)
  "Read a source file and check if there's an error.  If there
is an error, exit with code 0, otherwise, exit with code 1.
These codes are chosen to work with creduce."
  #.(format nil
            "Usage examples:
  test-parse --error-type=\"serapeum::econd-failure\" foo.c
  test-parse --error-pattern=\"unknown type specifier\" --language=cpp bar.cpp

Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable load eval))
  (flet ((interesting? (e)
           (or (typep e error-type)
               (and error-pattern (cl-ppcre:scan error-pattern (format nil "~s" e)))
               (and (not error-type) (not error-pattern)))))
    (handler-case
        (let ((g (genome (create-software source :language language))))
          (when round-trip
            (unless (eql (search (remove-if #'whitespacep (source-text g))
                                 (remove-if #'whitespacep (read-file-into-string source)))
                         0)
              (exit-command test-parse 0 nil)))
          (when idempotent
            (when (member-if {typep _ 'parse-error-ast} (convert 'list g))
              (exit-command test-parse 1 nil))
            (let* ((s (source-text g))
                   (g2 (genome (from-string
                                (make-instance (resolve-language-from-language-and-source language))
                                s)))
                   (s2 (source-text g2)))
              (unless (equal s s2)
                (exit-command test-parse 0 nil))))
          (exit-command test-parse 1 nil))
      (error (e)
        (format t "~s~%" e)
        (if (interesting? e)
            ;; A return value of 0 indicates that a partially reduced file is
            ;; interesting to creduce.
            (exit-command test-parse 0 e)
            ;; A return value that isn't 0 indicates that a partially reduced
            ;; file is not interesting to creduce.
            (exit-command test-parse 1 e))))))
