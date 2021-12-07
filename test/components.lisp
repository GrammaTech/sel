;;; components.lisp -- Tests for components.
(defpackage :software-evolution-library/test/components
  (:nicknames :sel/test/components)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library/components/test-suite
   :software-evolution-library/command-line)
  (:import-from :software-evolution-library/components/test-suite
                :full-tests)
  (:export :test-components))
(in-package :software-evolution-library/test/components)
(in-readtable :curry-compose-reader-macros)
(defsuite test-components "Component tests")

(defun make-sleeper (&optional (n 3))
  (make 'test-case
        :program-name "sleep"
        :program-args (list (fmt "~a" n))))

(deftest no-timeout-test ()
  (let ((*process-kill-timeout* nil))
    (is (= 0 (nth-value 2 (run-test "/tmp" (make-sleeper 0.2))))))
  (let ((*process-kill-timeout* 0))
    (is (= 0 (nth-value 2 (run-test "/tmp" (make-sleeper 0.2)))))))

(deftest timeout-test ()
  (let ((*process-kill-timeout* 0.5))
    (is (= 143 (nth-value 2 (run-test "/tmp" (make-sleeper 3)))))))

(defun test-suite-test-dir (name)
  (make-pathname :directory (append +etc-dir+ (list name))))

(deftest all-tests-suite-test ()
  (if-let* ((suite (is (create-all-tests-suite
                        (namestring
                         (merge-pathnames "tests.sh"
                                          (test-suite-test-dir "gcd")))
                        :time-limit 60)))
            (num-cases (length (test-cases suite))))
    (is (= num-cases 1))))

(defparameter *test-suite* nil)

(deftest test-suite-test ()
  (if-let* ((suite (is (create-test-suite
                        (namestring
                         (merge-pathnames "tests.sh"
                                          (test-suite-test-dir "gcd")))
                        10
                        :time-limit 60)))
            (num-cases (length (test-cases suite))))
    (is (= num-cases 10))))

(deftest random-test-suite-test ()
  (if-let* ((suite (is (create-random-test-suite
                        (create-test-suite
                         (namestring
                          (merge-pathnames "tests.sh"
                                           (test-suite-test-dir "gcd")))
                         20
                         :time-limit 60)
                        5)
                       "Could not create random-test-suite"))
            (full-test-count (length (test-cases (full-tests suite))))
            (num-cases (length (test-cases suite))))
    (progn
      (is (= num-cases 5) "Wrong number of test cases in random-test-suite")
      (is (= full-test-count 20) "Wrong number of cases in full set"))))
