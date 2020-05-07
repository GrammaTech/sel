;;; components.lisp -- Tests for components.
(defpackage :software-evolution-library/test/components
  (:nicknames :sel/test/components)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   :software-evolution-library/components/test-suite)
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
    (is (= 143 (nth-value 2 (run-test "/tmp" (make-sleeper 1)))))))
