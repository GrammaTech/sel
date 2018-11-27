;;;; stefil-plus.lisp --- Enhancements to the stefil library.
;;;;
;;;; Adds support for timing of tests, and warnings when tests exceed a
;;;; designated elapsed time threshold.
;;;;
;;;; Adds support for auto-generation of a second suite (with each
;;;; test suite created by DEFSUITE). This second suite will contain
;;;; tests designated as :long-running. Tests with this designation
;;;; will be kept in the long-running suite (with suffix "-long") and
;;;; will only be executed if special variable *long-tests* is true.
;;;;
;;;; To convert a synthesis module to use these features, modify the
;;;; project package to (:use sel/stefil+) instead of stefil and
;;;; include software-evolution-library/stefil-plus as a component it
;;;; depends on.  This is part of the software-evolution-library
;;;; package/release.
;;;;
;;;; The special variable STEFIL+:*LONG-TESTS* controls whether long
;;;; tests are executed. The "make check" command executes the
;;;; unit-check target (in cl.mk).  The unit-check target sets the
;;;; variable true e.g.
;;;;
;;;;     --eval '(setq sel/stefil+:*long-tests* t)' \
;;;;
;;;; If your project uses its own cl.mk it needs to add this line to
;;;; the unit-check target (see sel/cl.mk).
;;;;
(defpackage :software-evolution-library/stefil-plus
  (:nicknames :sel/stefil-plus :sel/stefil+)
  (:use
   :common-lisp
   :iterate
   :alexandria
   :cl-store
   :metabang-bind
   :software-evolution-library/utility
   :uiop)
  (:shadowing-import-from
   :stefil
   :defixture :with-fixture :defsuite* :in-suite
   :run-child-tests :find-test
   :is :signals :finishes :with-expected-failures
   :runs-without-failure?
   :without-debugging :without-test-progress-printing
   :run-failed-tests
   :*debug-on-unexpected-error*
   :*debug-on-assertion-failure*)
  (:shadowing-import-from :iterate :iter :for :until :collecting :in)
  (:shadowing-import-from :uiop :getenv :quit)
  (:shadowing-import-from
   :alexandria
   :appendf :ensure-list :featurep :emptyp
   :if-let :ensure-function :ensure-gethash :copy-file
   :parse-body :simple-style-warning)
  (:export :defroot
           :defsuite
	   :*long-tests*
           :*time-threshold*
           :deftest
           :defixture
           :with-fixture
           :is :signals :finishes :with-expected-failures
           :runs-without-failure?
           :without-debugging :without-test-progress-printing
           :run-failed-tests
           :*debug-on-unexpected-error*
           :*debug-on-assertion-failure*
	   :long-running-p
	   :long-running))

(in-package :sel/stefil+)

(define-constant *long-suite-suffix* "-LONG"
  :test #'string=
  :documentation
  "Suffix for version of suite which contains long running tests.")

(declaim (declaration long-running))    ;; allow (declare (long-running t))

(defvar *root-suite* nil
  "Default suite in which `sel-suite*' should define tests.")

;;;
;;; If *long-tests* is true, then long-test suites (test suites with
;;; *long-suite-suffix* suffix) will execute their child tests.
;;; Otherwise child tests will not be executed.
;;;
(defvar *long-tests* nil "Control execution of slow tests.")

;; If a test takes longer than *threshold* seconds, it should be
;; moved to the long test-suite (<test-suite-name>-LONG).
;;
(defvar *time-threshold* 1.0 "Time threshold for tests (in seconds).")

(defmacro defroot (name)
  (let ((local-name (intern (symbol-name name) *package*)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (stefil::defsuite ,local-name)
       (setf sel/stefil+::*root-suite* ',local-name)
       (setf (stefil::parent-of (stefil::find-test ',local-name)) nil))))

(defmacro defsuite (name documentation &optional (test-pre-check t))
  "Define NAME with DOCUMENTATION.
Optional TEST-PRE-CHECK if present should be one of the following:
:SILENT --- silently skip this test suite
NIL ------- skip this test suite with the default warning
Otherwise - test-pre-check will be invoked at runtime
return non-nil when the test suite should be run and nil otherwise."
  (let ((long-name (intern (format nil "~A~A" name *long-suite-suffix*))))
    (with-gensyms (test)
      `(progn
         (assert *root-suite* (*root-suite*)
              "Default suite *root-suite* must be set to use `DEFSUITE'.")

	 (when (or (boundp ',name) (fboundp ',name))
	   (warn 'stefil::test-style-warning
                 :format-control
                 "Defining ~a with `DEFSUITE' overwrites existing definition."
		 :format-arguments (list ',name)))
	 (when (or (boundp ',long-name) (fboundp ',long-name))
	   (warn 'stefil::test-style-warning
                 :format-control
                 "Defining ~a with `DEFSUITE' overwrites existing definition."
		 :format-arguments (list ',long-name)))
	 (defsuite* (,name :in ,(intern (symbol-name *root-suite*) *package*)
			   :documentation ,documentation) ()
	   (let ((,test (find-test ',name)))
	     (cond
	       ((stefil::test-was-run-p ,test)
		(warn "Skipped executing already run tests suite ~S"
		      (stefil::name-of ,test)))
	       ,@(if (eq test-pre-check :silent)
		     '()
		     `(,@(if test-pre-check
			     `((,test-pre-check
				(run-child-tests)))
			     nil)
			 (t (warn "Skipped executing disabled tests suite ~S."
				  (stefil::name-of ,test))))))))
	 (defsuite* (,long-name :in
				,(intern (symbol-name *root-suite*) *package*)
				:documentation ,documentation)
             ()
	   (let ((,test (find-test ',long-name)))
	     (cond
	       ((stefil::test-was-run-p ,test)
		(warn "Skipped executing already run tests suite ~S"
		      (stefil::name-of ,test)))
	       ,@(if (eq test-pre-check :silent)
		     '()
		     `(,@(if test-pre-check
			     `((,test-pre-check
				(when *long-tests* (run-child-tests))))
			     nil)
			 (t (warn "Skipped executing disabled tests suite ~S."
				  (stefil::name-of ,test))))))))
	 (in-suite ,name)))))

(defun mov-test (test-name new-parent-name &rest test-args)
  "Move a child test to a different parent."
  (bind ((test (apply #'find-test test-name test-args))
         (parent (when test
                   (stefil::parent-of test)))
	 (new-parent (find-test new-parent-name)))
    (when test
      (assert parent
              () "You can not move a test which has no parent")
      (assert test
              () (format nil "Test ~A not found" test))
      (assert new-parent
              () (format nil "Parent ~A not found" parent))
      (remhash test-name (stefil::children-of parent))
      (setf (stefil::parent-of test) new-parent)
      (setf (gethash test-name (stefil::children-of new-parent)) test))
    test))

(defun find-long-running-suite (suite-name)
  "Determine the name of the long-running suite associated with the passed
suite name. If the passed suite name ends with *long-suite-suffix* it returns
the argument."
  (let ((str-suite-name (symbol-name suite-name)))
    (if (ends-with-p str-suite-name *long-suite-suffix*)
        suite-name
        (values
         (intern (format nil "~A~A" str-suite-name *long-suite-suffix*))))))

(define-condition test-exceeds-time-threshold (simple-warning)
  ((name :initarg :name :reader test-exceeds-threshold-name)
   (time :initarg :time :reader test-exceeds-threshold-time))
  (:report
   (lambda (condition stream)
     (format stream
	     "Test ~A took ~F seconds to run, it is considered long-running."
	     (test-exceeds-threshold-name condition)
	     (test-exceeds-threshold-time condition)))))

(defmacro check-time (test-name &body body)
  "If elapsed real time of test exceeds *time-threshold*, a warning
is output. In any case, this macro executes BODY and returns any resulting
values."
  (with-gensyms (start-time result end-time elapsed)
    `(let* ((,start-time (get-internal-real-time))
	    (,result (progn ,@body))
	    (,end-time (get-internal-real-time))
            (,elapsed (float
		       (/ (- ,end-time ,start-time)
		          internal-time-units-per-second))))
       (declare (special *time-threshold*))
       (if (and
	    (> ,elapsed *time-threshold*)
	    (not (long-running-p ,test-name)))
	   (warn 'test-exceeds-time-threshold
	         :name ,test-name
	         :time ,elapsed))
       ,result)))

;;;
;;;
;;;
(defmacro deftest (name args &body body)
  "Expands on STEFIL::DEFTEST in these ways:
- Accepts (<test-name> :LONG-RUNNING) in place of <test-name> for the name
- When the test is executed, if the execution time (real time) is longer
  than *TIME-THRESHOLD* seconds, a warning is output."

  (let ((long-running nil)
	(decls-or-doc '())
	(body-remaining '()))
    (when (listp name)
      (if (member ':long-running name)
	  (setf long-running t))
      (if (symbolp (first name))
	  (setf name (first name))))

    ;; separate the declarations and doc strings from remaining forms
    (do ((b body (cdr b)))
	((null b))
      (if (or (stringp (car b))
	      (and (listp (car b)) (eq (caar b) 'declare)))
	  (push (car b) decls-or-doc)
	  (progn (setf body-remaining b) (return))))
    
    (if long-running
        `(let ((stefil::*suite*
	        (find-test
	         (find-long-running-suite
	          (stefil::name-of stefil::*suite*)))))
           (stefil::deftest ,name ,args
	     ,@(append (nreverse decls-or-doc) '((declare (long-running t))))
			    (check-time ',name (progn ,@body-remaining))))
        `(stefil::deftest ,name ,args ,@(nreverse decls-or-doc)
			  (check-time ',name (progn ,@body-remaining))))))

(defun long-running-p (test)
  "Given the name of a test, returns true iff test has been flagged 
long-running."
  (let ((test-obj (find-test test)))
    (and test-obj
	 (find-if
	  (lambda (x)
	    (and (listp x)
		 (equalp x '(declare (sel/stefil+::long-running t)))))
	  (stefil::declarations-of test-obj))
	 t)))
