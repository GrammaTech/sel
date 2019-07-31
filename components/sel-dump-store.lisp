;;; sel-dump-store.lisp -- command line utility to dump .store file

(defpackage :software-evolution-library/components/sel-dump-store
  (:nicknames :sel/components/sel-dump-store
              :sel/cp/sel-dump-store)
  (:use :common-lisp
        :alexandria
        :iterate
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/command-line
        :software-evolution-library/software/ast
        :software-evolution-library/software/parseable
        :software-evolution-library/software/source
        :software-evolution-library/software/forth
        :software-evolution-library/software/java
        :software-evolution-library/software/java-project
        :software-evolution-library/software/javascript
        :software-evolution-library/software/javascript-project
        :software-evolution-library/software/json
        :software-evolution-library/software/sexp
        :software-evolution-library/software/lisp
        :software-evolution-library/software/llvm
        :software-evolution-library/software/project
        :software-evolution-library/software/simple
        :software-evolution-library/software/clang
        :software-evolution-library/software/new-clang)
  (:import-from :uiop :nest truenamize)
  (:export :sel-dump-store :run-sel-dump-store))
(in-package :software-evolution-library/components/sel-dump-store)

;;; Command line
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
              (("out-dir" #\o) :type string
               :action #'handle-out-dir-argument
               :documentation "write final population into DIR"))
            ;; pulled from BI
            `((("no-store-software" #\S) :type boolean :optional t
               :documentation "inhibit caching the software")
              (("language" #\L) :type string :initial-value "c"
               :documentation
               "language of input files (e.g. c, c++, java, or javascript)"))
            +clang-command-line-options+
            +project-command-line-options+
            +clang-project-command-line-options+
            `((("new-clang" #\N) :type boolean :optional t
               :documentation "Use new clang front end")))))

(define-command sel-dump-store (source &spec +command-line-options+
                                       &aux project-name software-store)
  "Dump a software object to a store file"
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose new-clang))
  (when help (show-help-for-sel-dump-store))
  (setf out-dir (or out-dir (resolve-out-dir-from-source source))
        project-name (resolve-name-from-source source)
        software-store
        (unless no-store-software
          (resolve-store-path-from-out-dir-and-name out-dir project-name)))
  (let ((software
         (create-software source
                          :store-path software-store
                          :language language
                          :compiler compiler
                          :flags flags
                          :build-command build-command
                          :artifacts artifacts
                          :compilation-database compilation-database)))
    (cl-store:store software software-store)))


