;;;; task-runner.lisp --- TASK-RUNNER tests.
(defpackage :software-evolution-library/test/task-runner
  (:nicknames :sel/test/task-runner)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/util
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
  (:export :task-runner))
(in-package :software-evolution-library/test/task-runner)
(in-readtable :curry-compose-reader-macros)
(defsuite task-runner)

;; simple test to see if the whole file parsed correctly
(deftest (task-runner-1 :long-running) ( )
  (let (length)
    (is (with-retries (100)
          (with-fixture task-runner
            (setf length (length (task-runner-results (first *soft*)))))
          (when (= length 20)
            (return t))))))

(deftest (task-runner-2 :long-running) ()
  (with-fixture task-runner
    (is (= (length (task-runner-results (second *soft*))) 20))))

(deftest (task-runner-3 :long-running) ()
  (with-fixture task-runner
    (is (= (task-runner-completed-tasks (first *soft*)) 20))
    (is (= (task-runner-completed-tasks (second *soft*)) 20))
    (is (= (task-runner-completed-jobs (first *soft*)) 1))
    (is (= (task-runner-completed-jobs (second *soft*)) 1))))

(deftest (task-runner-4 :long-running) ()
  (with-fixture task-runner
    (is (= (count "test1" (task-runner-results (first *soft*))
                  :test 'equal :key (lambda (s) (subseq s 0 5)))
           20))
    (is (= (count "test2" (task-runner-results (second *soft*))
                  :test 'equal :key (lambda (s) (subseq s 0 5)))
           20))))

(deftest some-task-similar-to-some ()
  (let ((runner1 (run-task (make-instance 'some-task
                             :object (iota 10)
                             :pred {= 5})
                           2))
        (runner2 (run-task (make-instance 'some-task
                             :object (iota 10)
                             :pred {= 15})
                           2)))
    (mapcar #'bt:join-thread (task-runner-workers runner1))
    (mapcar #'bt:join-thread (task-runner-workers runner2))
    (is (equal '(T) (task-runner-results runner1)))
    (is (eql (first (task-runner-results runner1))
             (some {= 5} (iota 10))))
    (is (null (task-runner-results runner2)))
    (is (eql (first (task-runner-results runner2))
             (some {= 15} (iota 10))))))

(deftest simple-task-map ()
  (let ((results (task-map 2 {+ 1} '(1 2 3))))
    ;; Ensure correct results are returned by multi-threaded task-map
    ;; (in any order).
    (mapc (lambda (result) (is (member result results))) '(2 3 4))))
