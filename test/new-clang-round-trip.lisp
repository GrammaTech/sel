;;; new-clang-round-trip.lisp -- test harness for clang round trip testing
;;;
;;; The purpose of this simple command line tool is to use the new
;;; clang front end to read a file, then dump it to the standard output,
;;; to confirm that the file is byte-for-byte identical.
;;;
(defpackage :software-evolution-library/test/new-clang-round-trip
  (:nicknames :sel/test/new-clang-round-trip)
  (:use :common-lisp
        :alexandria
        :iterate
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/command-line
        :software-evolution-library/software/ast
        :software-evolution-library/software/parseable
        :software-evolution-library/software/source
        :software-evolution-library/software/new-clang)
  (:import-from :uiop :nest truenamize)
  (:export :new-clang-round-trip))
(in-package :software-evolution-library/test/new-clang-round-trip)

;;;; Command line
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +command-line-options+
    (append `((("help" #\h #\?) :type boolean :optional t
               :documentation "display help output")
              (("load" #\l) :type string
               :action #'handle-load
               :documentation "load FILE as lisp code")
              (("eval" #\e) :type string
               :action #'handle-eval
               :documentation "eval STRING as lisp code")
              (("compiler" #\c) :type string :initial-value "clang"
               :documentation "use CC as the C compiler")
              (("flags" #\F) :type string
               :action #'handle-comma-delimited-argument
               :documentation "comma-separated list of compiler flags")))))

(defun from-file-new-clang (pd &key flags compiler)
  (from-file
   (make-instance 'new-clang :flags flags :compiler compiler)
   (pathname pd)))

(define-command new-clang-round-trip
    (source out &spec +command-line-options+
            &aux )
  "Read and print a clang file using the new clang parser"
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable eval load))
  (when help (show-help-for-new-clang-round-trip))
  ;; Rewrite this to use new-clang class
  (let ((sw (make-instance 'new-clang :flags flags :compiler compiler)))
    (from-file sw source)
    (with-open-file (s out :direction :output :if-exists :supersede)
      (format s "~a" (source-text (ast-root sw))))))
