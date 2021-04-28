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
                :parsing-error :rule-matching-error)
  (:export test-parse run-test-parse))
(in-package :software-evolution-library/components/test-parse)

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
      (("language" #\L) :type string :initial-value "c"
                        :documentation
       "language of input files (e.g. c, c++, lisp, or javascript)"))))

(define-command test-parse (source &spec +command-line-options+)
  "Read a source file and check if there's an error.  If there
is an error, exit with code 0, otherwise, exit with code 1.
These codes are chosen to work with creduce."
    #.(format nil
              "~%Built from SEL ~a, and ~a ~a.~%"
              +software-evolution-library-version+
              (lisp-implementation-type) (lisp-implementation-version))
  "Usage examples:
      test-parse --error-type=\"serapeum::econd-failure\" foo.c
      test-parse --error-pattern=\"unknown type specifier\" --language=cpp bar.cpp"
  (declare (ignorable load eval))
  (flet ((interesting? (e)
           (or (and error-type (typep e error-type))
               (and error-pattern (cl-ppcre:scan error-pattern (format nil "~s" e)))
               (and (not error-type) (not error-pattern)))))
    (handler-case
        (progn (genome (create-software source :language language))
               (exit-command test-parse 1 nil))
      (parsing-error (e) (exit-command test-parse 1 e))
      (error (e) (if (interesting? e)
                     (exit-command test-parse 0 e)
                     (exit-command test-parse 1 e))))))
