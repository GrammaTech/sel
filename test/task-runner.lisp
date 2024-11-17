;;;; task-runner.lisp --- TASK-RUNNER tests.
(defpackage :software-evolution-library/test/task-runner
  (:nicknames :sel/test/task-runner)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library/utility/task
   :software-evolution-library)
  (:export :test-task-runner))
(in-package :software-evolution-library/test/task-runner)
(in-readtable :curry-compose-reader-macros)
(defsuite test-task-runner "TASK-RUNNER tests.")

;;; Task support
(defclass child-task (task) ())
(defclass parent-task (task) ())
(defmethod task-job ((task parent-task) runner)
  (declare (ignore runner))
  (let ((index 0))
    (lambda ()
      (if (<= (incf index) 20)
          (make-instance 'child-task
            :object (format nil "~A-~D"
                            (task-object task) index))))))
(defmethod process-task ((task child-task) runner)
  (task-save-result runner (task-object task)) ;; save the object
  (sleep 1)) ;; sleep 1 second

(defixture task-runner
  (:setup (setf
           *soft*
           (list (run-task (make-instance 'parent-task :object "test1") 10)
                 (run-task (make-instance 'parent-task :object "test2") 20)))
          ;; wait for all the threads to terminate
          (mapcar 'bt:join-thread (task-runner-workers (first *soft*)))
          (mapcar 'bt:join-thread (task-runner-workers (second *soft*))))
  (:teardown
   (setf *soft* nil)))

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

(defvar *in-worker*)

(deftest test-worker-env ()
  "Using `*worker-funcall*' should change the environment of tasks
launched directly as well as tasks launched from the task thread."
  (is (not (boundp '*in-worker*)))
  (let* ((thread-count 4)
         (worker-fn
           (lambda (fn)
             (let ((*in-worker* t))
               (funcall fn))))
         (*worker-funcall* worker-fn)
         boundp)
    (task-map thread-count
              (lambda (x)
                (declare (ignore x))
                (synchronized ('boundp)
                  (push (boundp '*in-worker*) boundp))
                (task-map thread-count
                          (lambda (x)
                            (declare (ignore x))
                            (synchronized ('boundp)
                              (push (boundp '*in-worker*) boundp)))
                          (make-list thread-count)))
              (make-list thread-count))
    (is (every (eqls t) boundp))))
