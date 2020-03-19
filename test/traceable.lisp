;;;; traceable.lisp --- Traceable tests.
(defpackage :software-evolution-library/test/traceable
  (:nicknames :sel/test/traceable)
  (:use
   :gt/full
   :trace-db
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :software-evolution-library/stefil-plus
   :software-evolution-library
   :software-evolution-library/software/source
   :software-evolution-library/software/clang
   :software-evolution-library/components/instrument
   :software-evolution-library/components/traceable
   :software-evolution-library/components/test-suite)
  (:export :test-traceable))
(in-package :software-evolution-library/test/traceable)
(in-readtable :curry-compose-reader-macros)
(defsuite test-traceable "Traceable tests." (clang-available-p))

(define-software clang-traceable (clang binary-traceable) ())
(define-software java-traceable  (java sexp-traceable) ())
(define-software javascript-traceable  (javascript sexp-traceable) ())
(define-software javascript-traceable-project  (javascript-project sexp-traceable) ())
(define-software collect-traces-handles-directory-phenomes-mock
    (source binary-traceable)
  ((phenome-dir :initarg phenome-dir :accessor phenome-dir :initform nil
                :copier :direct)))

(defixture print-env-clang
  (:setup (setf *soft*
                (from-file (make-instance 'clang :compiler "clang")
                           (make-pathname :directory +etc-dir+
                                          :name "print-env"
                                          :type "c"))))
  (:teardown (setf *soft* nil)))

(define-constant +long-running-program-dir+
    (append +etc-dir+  (list "long-running-program"))
  :test #'equalp
  :documentation "Path to long running program example.")

(defun long-running-program-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +long-running-program-dir+))

(defixture long-running-program-clang
  (:setup
   (setf *soft*
         (from-file (make-instance 'clang)
                    (long-running-program-dir "long-running-program.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture traceable-gcd
  (:setup (setf *gcd* (from-file (make-instance 'clang-traceable)
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
    (with-temporary-file (:pathname bin)
      (phenome *soft* :bin bin)
      (let ((proc (start-test bin
                              (make-instance 'test-case :program-name bin)
                              :wait nil))
            (*process-kill-timeout* 4))
        (finish-test proc)
        (is (not (process-alive-p proc))
            "finish-test did not kill a long running process")))))

(deftest (env-variables-passed-through-to-test-suites :long-running) ()
  (with-fixture print-env-clang
    (with-temporary-file (:pathname bin)
      (phenome *soft* :bin bin)
      (is (string=
           (concatenate 'string "__sel_bar" '(#\Newline))
           (read-stream-content-into-string
            (process-info-output
             (start-test bin
                         (make-instance 'test-case
                           :program-name bin
                           :program-args '("__sel_foo"))
                         :wait t
                         :output :stream
                         :env '(("__sel_foo" . "__sel_bar"))))))))))
