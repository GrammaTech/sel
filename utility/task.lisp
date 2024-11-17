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
;;; @subsection Description
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
;;;
;;; @subsection Example use
;;;
;;;     (setf *runner* (run-task (make-instance 'single-cut-all :object *orig*)
;;;                              10))
;;;      ;; When (task-runner-worker-count *runner*) = 0,
;;;      ;; it means all threads are finished.
;;;
;;; @subsection Complex Example use
;;;
;;;     (defmacro task-map (num-threads function sequence)
;;;       "Run FUNCTION over SEQUENCE using a `simple-job' `task-job'."
;;;       (with-gensyms (task-map task-item)
;;;         `(if (<= ,num-threads 1)
;;;              (mapcar ,function ,sequence) ; No threading.
;;;              (progn                     ; Multi-threaded implementation.
;;;                (defclass ,task-map (task) ())  ; Task to map over SEQUENCE.
;;;                (defclass ,task-item (task) ()) ; Task to process elements.
;;;                (defmethod task-job ((task ,task-map) runner)
;;;                  (declare (ignore runner))
;;;                  (let ((objs (task-object task))) ; Enclose SEQUENCE for fn.
;;;                    (lambda () (when objs ; Return nil when SEQUENCE is empty.
;;;                                ;; Return a task-item whose task-object run
;;;                                ;; FUNCTION on the next element of SEQUENCE.
;;;                            (make-instance ',task-item :object
;;;                                           (curry ,function (pop objs)))))))
;;;                (defmethod process-task ((task ,task-item) runner)
;;;                  ;; Evaluate the task-object for this item as created in
;;;                  ;; the task-job method above.  Save the results.
;;;                  (task-save-result runner (funcall (task-object task))))
;;;                (task-runner-results ; Return results from the results obj.
;;;                 ;; Create the task-map object, and run until exhausted.
;;;                 (run-task-and-block
;;;                  (make-instance ',task-map :object ,sequence)
;;;                  ,num-threads))))))
;;;
;;; The above example uses the tasks API to implement a simple
;;; parallel map spread across a configurable number of workers.  When
;;; more than one worker thread is requested the following objects and
;;; methods are created to implement the parallel map.
;;;
;;; * A TASK-MAP TASK is created to hold the sequence.
;;;
;;; * The TASK-JOB method is defined for this TASK-MAP.  This method
;;;   returns a function which has access to the SEQUENCE in a
;;;   closure.  The function will continually pop the first element
;;;   off the top of the sequence and wrap it in a TASK-ITEM object to
;;;   be returned until the SEQUENCE is empty at which point the
;;;   function returns nil causing all worker threads to exit.
;;;
;;; * The TASK-ITEM TASK is created to hold tasks for every item in
;;;   the sequence.
;;;
;;; * The PROCESS-TASK method is defined for this TASK-ITEM.  This
;;;   method evaluates the function stored in the TASK-OBJECT of this
;;;   TASK-ITEM and saves the result into the task runner's results.
;;;
;;; Finally, with the above objects and methods defined, the TASK-MAP
;;; macro wraps the sequence into a TASK-MAP TASK object and passes
;;; this to the RUN-TASK-AND-BLOCK function yielding a runner and the
;;; contents of that runner are extracted and returned using the
;;; TASK-RUNNER-RESULTS accessor.
;;;
;;; See the actual implementation of TASK-MAP in the SEL/UTILITY
;;; package for a more efficient implementation which doesn't use a
;;; macro or require new objects and methods to be defined on the fly.
;;;
;;; @texi{task}
(defpackage software-evolution-library/utility/task
  (:nicknames :sel/utility/task)
  (:use :gt)
  (:export
   :task-runner
   :*task-runner*
   :*worker-funcall*
   :task-runner-jobs
   :task-runner-workers
   :task-runner-workers-count
   :task-runner-results
   :task-runner-completed-jobs
   :task-runner-completed-tasks
   :task-runner-remaining-jobs
   :task-runner-init-jobs
   :task-runner-stop-jobs
   :task-runner-add-job
   :task-runner-create-worker
   :task
   :task-job
   :process-task
   :task-object
   :task-save-result
   :run-task
   :run-as-task
   :task-map
   :task-map-async
   :simple-task-async-runner
   :run-task-and-block
   :some-task
   :some-task-pred
   :some-test-task
   :task-map-in-order))
(in-package :software-evolution-library/utility/task)

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

(defparameter *worker-funcall* #'funcall
  "Function that invokes worker loop to run.
When this value is bound when starting a task, the value is propagated
through to any further tasks launched from the first task, and so on.")

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
    (let ((*default-special-bindings*
            (list* (cons '*task-runner* runner)
                   (cons '*worker-funcall* *worker-funcall*)
                   *default-special-bindings*)))
      (with-lock-held ((task-runner-workers-lock runner))
        (push (make-thread
               (lambda ()
                 (funcall *worker-funcall* #'start-worker))
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
  (let (done runner)
    (unwind-protect
         (progn
           (setf runner (run-task task num-workers))
           (mapcar #'join-thread (task-runner-workers runner))
           (setf done t)
           runner)
      ;; Terminate the runner on abnormal exit.
      (unless done
        (task-runner-stop-jobs runner)))))

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


;;;
;;; A simple way to just run a single task as a one-off
;;; Example:
;;;   (run-as-task (task1 runner1)
;;;     (with-output-to-string (s)
;;;       (dotimes (i 10)(format s "~A~%" i))
;;;       (task-save-result runner1 (get-output-stream-string s))))
;;;

(defclass simple-job (task) ())
(defmethod task-job ((task simple-job) runner)
  (declare (ignore runner))
  (let ((index 0))
    (lambda ()
      (if (<= (incf index) 1)
          (task-object task)))))

(defmacro run-as-task ((task runner) &body body)
  "Run the body code as a one-off task, which can access task and runner by
name. The supplied names may be any available symbols. Returns the TASK-RUNNER
object."
  (with-gensyms (task-type)
    `(progn
       (defclass ,task-type (task) ())
       (defmethod process-task ((,task ,task-type) ,runner)
         ,@body)
      (run-task
        (make-instance 'simple-job
          :object (make-instance ',task-type :object nil))))))

(defclass task-map (task)
  ((task-function :initarg :task-function :accessor task-function))
  (:documentation
   "Task object used to map a function over a sequence using workers."))

(defclass task-item (task) ()
  (:documentation
   "Task object used to execute a function on an element of a sequence.
See the `task-job' method on `task-map' objects."))

(defmethod task-job ((task task-map) runner)
  "Return a function which will spawn jobs for all of TASK's objects."
  (declare (ignore runner))
  (let ((objs (task-object task)))
    ;; Release the task object so it can be GC'd.
    (setf (task-object task) nil)
    (lambda () (when objs
                 ;; Return a task-item whose task-object run
                 ;; FUNCTION on the next element of OBJECTS.
                 (make-instance 'task-item :object
                                (curry (task-function task) (pop objs)))))))

(defmethod process-task ((task task-item) runner)
  "Evaluate the TASK saving the result in the runner."
  (task-save-result runner (funcall (task-object task))))

(defun task-map (num-threads function objects)
  "Run FUNCTION over OBJECTS using a `simple-job' `task-job'."
  (if (<= num-threads 1)
      (mapcar function objects)   ; No threading when num-threads <= 1.
      (task-runner-results  ; Return the results from the results obj.
       ;; Create the task-map object, and run until exhausted.
       (run-task-and-block (make-instance 'task-map
                                          :object objects
                                          :task-function function)
                           num-threads))))

(defun task-map-in-order (num-threads function objects)
  "Like `task-map', but preserves the order of OBJECTS in the output."
  (if (<= num-threads 1)
      (mapcar function objects)  ; No threading when num-threads <= 1.
      ;; Use index . value pairs so we preserve the order of results.
      (let* ((function (ensure-function function))
             (function (op (cons (car _1) (funcall function (cdr _1)))))
             (objects
              (iter (for i from 0)
                    (for object in objects)
                    (collect (cons i object))))
             (results
              (task-map num-threads function objects))
             (vector (make-array (length results))))
        (iter (for (i . value) in results)
              (setf (aref vector i) value))
        (coerce vector 'list))))

(defun task-map-async (num-threads func objects)
  "Run FUNC over OBJECTS using a `simple-job' `task-job'."
  ;; Create the task-map object, and run until exhausted.
  (run-task (make-instance 'task-map
              :object objects
              :task-function func)
            num-threads))

(defun simple-task-async-runner (num-threads func arguments)
  "Run FUNCTION with ARGUMENTS as a `simple-job' `task-job'."
  ;; Create the task-map object, and run until exhausted.
  (task-map-async num-threads (lambda (args) (apply func args)) arguments))
