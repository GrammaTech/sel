;;;; task.lisp --- Functions to run multiple tasks on multiple threads
;;;
;;; Functions to run multiple tasks (such as mutations and fitness
;;; tests) on multiple threads.
;;;
;;; This module makes use of Bordeaux Threads. See the documentation
;;; here:
;;; https://trac.common-lisp.net/bordeaux-threads/wiki/ApiDocumentation
;;;
;;; * Job: a function which returns a series of Tasks, one Task each
;;;        time it is called. When the series is exhausted, it will
;;;        return NIL.
;;;
;;; * Task: a thin wrapper over a Lisp object (typically a SOFTWARE
;;;         instance, but could be anything).  The task can be used to
;;;         customize how to process the object after fitness testing
;;;         (basically a completion routine) and to customize how it
;;;         spins off child Jobs.
;;;
;;; * Worker: one or more Worker threads can be created to process
;;;           Jobs.  When all jobs are finished, all the Worker
;;;           threads will exit.  Therefore you create Jobs first,
;;;           then the Workers.
;;;
;;; Example use:
;;;
;;;     (setf *runner* (run-task (make-instance 'single-cut-all :object *orig*)
;;;                              10))
;;;      ;; When (task-runner-worker-count *runner*) = 0,
;;;      ;; it means all threads are finished.
;;;
;;;
;;; A TASK is an operation to be performed by the multi-threaded
;;; TASK-RUNNER. A TASK can be customized by the client to generate
;;; a job (child series of tasks) by implementing the TASK-JOB method,
;;; and the code to be performed when processing a TASK is defined by the
;;; PROCESS-TASK method.
;;;
;;; A job is a Lisp function, which takes no arguments, and which will
;;; produce a TASK each time it is called, or NIL when all of its tasks are
;;; complete. Think of a job as a lazy sequence of TASK.
;;;
;;; TASK-RUNNER-JOBS is a stack of jobs. Worker threads will call the
;;; first job on the stack, and process the task returned.
;;;
;;; A task may add 1 or more jobs to the top of the stack, causing worker
;;; threads to immediately start processing those jobs since they are now
;;; higher on the stack and therefore have priority over other tasks.
;;;
;;; When the JOBS stack is empty/NIL, then all worker threads will exit.
;;; @texi{task}
(in-package :software-evolution-library/utility)
(in-readtable :curry-compose-reader-macros)

(defstruct task-runner
"The state needed to run multi-threaded tasks and associated jobs.
* jobs:         stack of current jobs to execute
* workers:      list running worker threads
* results:      result objects collected by worker threads
* jobs-lock:    (internal) used to synchronize jobs stack
* results-lock: (internal) used to synchronnize results list
* workers-lock: (internal) used to synchronize worker list
* completed-jobs: number of jobs that have been executed and finished
* completed-tasks: number of tasks that have finished"
  (jobs nil)
  (workers nil)
  (results nil)
  (jobs-lock (bt:make-recursive-lock "task-runner-jobs"))
  (results-lock (bt:make-lock "task-runner-results"))
  (workers-lock (bt:make-lock "task-runner-workers"))
  (completed-jobs 0)
  (completed-tasks 0))

(defparameter *task-runner* nil
  "Bind *TASK-RUNNER* for worker threads")

(defclass task ()
  ((object :initarg :object :accessor task-object))
  (:documentation "Base class for all task classes."))

;;;
;;; A class derived from the TASK class will override the method
;;; TASK-JOB to customize how the job's tasks are created and ordered.
;;; This method should return a Job, which is a function, which, when executed,
;;; will return the next TASK in the series. A Job returns NIL when no more
;;; tasks remain.
;;;
(defgeneric task-job (task runner)
 (:documentation  "Return a job for the *jobs* stack. This is a function which,
 when called, returns the next task in the series."))

;;; default method gives error if you try to create a job and there is no
;;; overridden method to do it
(defmethod task-job (obj runner)
  (declare (ignore obj runner))
  (error "Cannot create a task from the object"))

;;;
;;; Process task and task object, including evaluate fitness, push new Jobs,
;;; and store interesting results..
;;; PROCESS-TASK may add one ore more jobs based on the variant qualities
;;; and the fitness. The added jobs get pushed onto the TASK-RUNNER-JOBS stack
;;; and will be the next jobs to run.
;;;
;;; Also, PROCESS-TASK may save any interesting software variants on
;;; the TASK-RUNNER-RESULTS list.
;;;

(defgeneric process-task (task runner)
  (:documentation
   "Process the object, including evaluate fitness, push new Jobs, and
 store interesting results"))

(defmethod process-task (obj runner)
  (declare (ignore obj runner))
  ;; default does nothing
  nil)

;;;
;;; increment completed task counter after each task is processed
;;;
(defmethod process-task :after (obj runner)
  (declare (ignore obj))
  (bt:with-recursive-lock-held ((task-runner-jobs-lock runner))
    (incf (task-runner-completed-tasks runner))
    nil))

;;;
;;; Returns the next mutated software task, or NIL if no more tasks
;;;
(defun get-next-task (runner)
  (bt:with-recursive-lock-held ((task-runner-jobs-lock runner))
    (if (consp (task-runner-jobs runner))
	(let ((task (funcall (car (task-runner-jobs runner)))))
	  (if (null task)                       ;; if no more tasks in that job
	      (progn
		(pop (task-runner-jobs runner)) ;; pop the job
		(incf (task-runner-completed-jobs runner))
		(get-next-task runner)) ;; and recurse (until no more jobs)
	      task)))))

;;;
;;; Add a Job to the JOBS stack.
;;;
(defun task-runner-add-job (runner job)
  (bt:with-recursive-lock-held ((task-runner-jobs-lock runner))
    (push job (task-runner-jobs runner))))

;;;
;;; exit-worker
;;; Do any processing necessary when a worker thread exits
;;; For now, just decrement the *WORKER-COUNT* special variable.
;;;
(defun exit-worker (runner)
  (bt:with-lock-held ((task-runner-workers-lock runner))
    (setf (task-runner-workers runner)
	  (remove (current-thread) (task-runner-workers runner) :test 'equal))))

;;;
;;; The task executed by each worker thread.
;;; It simply executes a loop:
;;;     (a) get next task (GET-NEXT-TASK)
;;;     (b) process the task object (PROCESS-TASK)
;;; until no more tasks are found. Then the thread exits.
;;;
(defun worker-thread-task (runner)
  (do* ((task (get-next-task runner)(get-next-task runner)))
       ((null task)(exit-worker runner))
    (process-task task runner)))

;;; start-worker
;;; Do any processing when a worker thread is started.
;;;
(defun start-worker ()
  (let ((runner *task-runner*))  ;; get the special variable binding
    (worker-thread-task runner)))  ;; begin worker loop

(defun task-save-result (runner obj)
  "Save a result object."
  (bt:with-lock-held ((task-runner-results-lock runner))
    (push obj (task-runner-results runner))))

(let ((worker-id -1))
  (defun task-runner-create-worker (runner)
    "Create a new worker thread."
    (let ((*default-special-bindings* (acons '*task-runner* runner nil)))
      (with-lock-held ((task-runner-workers-lock runner))
        (push (make-thread 'start-worker
                :name (format nil "~A-~D" "software-mutator"
                              (incf worker-id)))
              (task-runner-workers runner))))))

;;
;; Re-initialize, overwrite any previous results.
;;
(defun task-runner-init-jobs (runner)
  (setf (task-runner-jobs runner) nil
	(task-runner-workers runner) nil
	(task-runner-results runner) nil))

;;
;; Remove all jobs from the jobs stack. This will cause
;; all the worker threads to finish and exit.
;;
(defun task-runner-stop-jobs (runner)
  (setf (task-runner-jobs runner) nil))

;;
;; Returns the TASK-RUNNER.
;; Client should save this result for further status updates.
;;
(defun run-task (task &optional (num-workers 1))
  "Create a TASK-RUNNER, using the specified task as the first job."
  (let ((runner (make-task-runner)))
    (task-runner-add-job runner (task-job task runner))
    (dotimes (i num-workers)
      (task-runner-create-worker runner))
    runner))

(defun run-task-and-block (task &optional (num-workers 1))
  "Create a TASK-RUNNER, using the specified task as the first job,
blocking until completion"
  (let ((runner (run-task task num-workers)))
    (mapcar #'join-thread (task-runner-workers runner))
    runner))

(defun task-runner-remaining-jobs (runner)
  "Returns the number of jobs remaining."
  (length (task-runner-jobs runner)))

(defun task-runner-workers-count (runner)
  "Returns the number of running threads."
  (length (task-runner-workers runner)))


;; Implementation of `some-task': mimics the behavior of `some'
;; except that results are stored as a list (due to implementation of
;; `task-save-result').
(defclass some-task (task)
  ((pred :initarg :pred :accessor some-task-pred
         :documentation "Predicate used by `some'."))
  (:documentation "Task for applying `some' in parallel.
The OBJECT field is a list on whose elements SOME-TASK-PRED is applied."))

(defclass some-test-task (task)
  ((pred :initarg :pred :accessor some-task-pred
         :documentation "Predicate used by `some'."))
  (:documentation "Task to apply predicate SOME-TASK-PRED to OBJECT."))

(defmethod task-job ((task some-task) runner)
  "Return the generating function for `some-task'.
Create new subtasks for each item in the list until either applying the
predicate SOME-TASK-PRED in TASK succeeds or there are no more items in the
list."
  (let ((ls (task-object task)))
    (lambda ()
      ;; Stop creating new tasks after a result is found or no elements remain.
      (when (and ls (not (task-runner-results runner)))
        (prog1
            (make-instance 'some-test-task
                           :object (car ls)
                           :pred (some-task-pred task))
          (setf ls (cdr ls)))))))

(defmethod process-task ((task some-test-task) runner)
  "Process a single TASK by applying SOME-TASK-PRED to the OBJECT in TASK.
NOTE: Since `task-save-result' pushes results to a list, it's possible for up to
N results to be saved (where N is the number of running threads), so `first'
should be used to retrieve one result. Additionally, due to differences in
timing, it's possible that the result won't match that of `some', since `some'
promises to find the first while `some-task' may return any element satisfying
`some-task-pred'."
  (when (not (task-runner-results runner))
    (when-let ((result (funcall (some-task-pred task) (task-object task))))
      (task-save-result runner result))))
