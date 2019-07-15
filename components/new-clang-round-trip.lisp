;;; new-clang-round-trip.lisp -- test harness for clang round trip testing
(defpackage :software-evolution-library/components/new-clang-round-trip
  (:nicknames :sel/components/new-clang-round-trip
              :sel/cp/new-clang-round-trip)
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
(in-package :software-evolution-library/components/new-clang-round-trip)

;;;; Command line
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +command-line-options+
    (append `((("help" #\h #\?) :type boolean :optional t
               :documentation "display help output")
              (("quiet" #\q) :type boolean :optional t
               :action #'handle-set-quiet-argument
               :documentation "set verbosity level to 0")
              (("verbose" #\V) :type integer :initial-value 2
               :action #'handle-set-verbose-argument
               :documentation "verbosity level 0-4")
              (("load" #\l) :type string
               :action #'handle-load
               :documentation "load FILE as lisp code")
              (("eval" #\e) :type string
               :action #'handle-eval
               :documentation "eval STRING as lisp code")
              (("compiler" #\c) :type string :initial-value ,sel/sw/new-clang::*clang-binary*
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
  (declare (ignorable quiet verbose))
  (when help (show-help-for-new-clang-round-trip))
  ;; Rewrite this to use new-clang class
  (let ((sw (make-instance 'new-clang :flags flags :compiler sel/sw/new-clang::*clang-binary*)))
    (from-file sw source)
    (with-open-file (s out :direction :output :if-exists :supersede)
      (format s "~a" (source-text (ast-root sw))))))




