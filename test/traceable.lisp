;;;; traceable.lisp --- Traceable tests.
(defpackage :software-evolution-library/test/traceable
  (:nicknames :sel/test/traceable)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/constants
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/utility)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :traceable))
(in-package :software-evolution-library/test/traceable)
(in-readtable :curry-compose-reader-macros)
(defsuite traceable)

(defvar *gcd* nil "Holds the gcd software object.")
(defvar *soft* nil "Software used in tests.")

(defixture traceable-gcd
  (:setup (setf *gcd* (from-file
                       (if *new-clang?*
                           (make-instance 'new-clang-traceable)
                           (make-instance 'clang-traceable))
                       (make-pathname :name "gcd"
                                      :type "c"
                                      :directory +gcd-dir+)))))

(defvar *gcd-inputs* '((:bin "1071" "1029")
                       (:bin "555" "666")
                       (:bin "678" "987")
                       (:bin "8767" "653")
                       (:bin "16777216" "512")
                       (:bin "16" "4")
                       (:bin "315" "831")
                       (:bin "513332" "91583315")
                       (:bin "112" "135")
                       (:bin "310" "55"))
  "Example test inputs for GCD.")

(defvar *gcd-test-suite*
  (make-instance
      'test-suite
    :test-cases
    (iter (for input in *gcd-inputs*)
          (collecting (make-instance 'test-case
                        :program-name (car input)
                        :program-args (cdr input))))))

(deftest (run-traceable-gcd :long-running) ()
  (with-fixture traceable-gcd
    (instrument *gcd*)
    (collect-traces *gcd* *gcd-test-suite*)
    (setf (traces *gcd*)
          (mapcar {get-trace (traces *gcd*)} (iota (n-traces (traces *gcd*)))))
    (is (= (length (traces *gcd*)) (length *gcd-inputs*)))
    (is (every {every {aget :c}} (mapcar {aget :trace} (traces *gcd*))))))

(deftest (run-traceable-gcd-w/collect-traces :long-running) ()
  (with-fixture traceable-gcd
    (instrument *gcd*)
    (collect-traces *gcd* *gcd-test-suite*)
    (setf (traces *gcd*)
          (mapcar {get-trace (traces *gcd*)} (iota (n-traces (traces *gcd*)))))
    (is (every {every {aget :c}} (mapcar {aget :trace} (traces *gcd*))))))

(defmethod phenome ((obj collect-traces-handles-directory-phenomes-mock)
                    &key (bin (temp-file-name)))
  (let ((dir (ensure-directory-pathname bin)))
    (setf (phenome-dir obj) dir)
    (ensure-directories-exist dir)))

(deftest collect-traces-handles-directory-phenomes ()
  (let ((obj (make-instance 'collect-traces-handles-directory-phenomes-mock)))
    (handler-bind ((trace-error (lambda (c)
                                  (declare (ignorable c))
                                  (invoke-restart 'ignore-empty-trace))))
      (collect-traces obj (make-instance 'test-suite)))
    (is (not (probe-file (phenome-dir obj)))
        "collect-traces did not remove a phenome directory")))

(deftest (long-running-program-killed-test :long-running) ()
  (with-fixture long-running-program-clang
    (with-temp-file (bin)
      (phenome *soft* :bin bin)
      (let ((proc (start-test bin
                              (make-instance 'test-case :program-name bin)
                              :wait nil))
            (*process-kill-timeout* 4))
        (finish-test proc)
        (is (not (process-running-p proc))
            "finish-test did not kill a long running process")))))

(deftest (env-variables-passed-through-to-test-suites :long-running) ()
  (with-fixture print-env-clang
    (with-temp-file (bin)
      (phenome *soft* :bin bin)
      (is (string=
           (concatenate 'string "__sel_bar" '(#\Newline))
           (stream-to-string
            (process-output-stream
             (start-test bin
                         (make-instance 'test-case
                           :program-name bin
                           :program-args '("__sel_foo"))
                         :wait t
                         :output :stream
                         :env '(("__sel_foo" . "__sel_bar"))))))))))
