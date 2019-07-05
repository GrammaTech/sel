;;; rest.lisp --- RESTful interface over SEL.
;;;
;;; Rest API for Software Evolution Library
;;;
;;; The Rest API for Software Evolution Library is implemented as a web
;;; service which may be accessed via HTTP operations.
;;;
;;; It attempts to conform to principals described here:
;;; @uref{https://en.wikipedia.org/wiki/Representational_state_transfer,
;;; Representational State Transfer}
;;;
;;; @subsection Dependencies
;;;
;;; The Rest API leverages a number of Common Lisp components, which
;;; are open-source and generally available via Quicklisp.  These
;;; packages support JSON <-> Common Lisp translations, JSON
;;; streaming, HTTP web server functions, client HTTP support and
;;; RESTful interface utilities.
;;;
;;;  CL-JSON
;;;      Parse and generate JSON format
;;;  ST-JSON
;;;      Stream support for JSON format
;;;  CLACK
;;;      utility to easily launch web services
;;;  DRAKMA
;;;      http client utilities for Common Lisp (for calling Rest
;;;      APIs/testing
;;;  HUNCHENTOOT
;;;      Web server, written in Common Lisp, hosts Rest APIs
;;;  SNOOZE
;;;      Rest API framework
;;;
;;; See rest.lisp for how to start the server.
;;;
;;;
;;; @subsection Resources and Operations
;;;
;;; This file provides the following:
;;;
;;; @subsubsection Resources
;;;
;;; The following types of resources are supported.
;;;
;;;
;;;  Jobs
;;;     Any long running operation which starts a task and returns to
;;;     the client immediately.  Examples of jobs include running
;;;     evolutions, fitness tests across large populations, and
;;;     searches.
;;;
;;;
;;; @subsubsection Operations on Resources
;;;
;;; Note: all operations (other than session create) require a client-ID
;;; parameter. Although only specified in the first ones below, all others
;;; require it as well.
;;;
;;; Async-Job:
;;;
;;;  POST
;;;     @code{<service-base>/async?type=<job-type>} Body
;;;     contains (JSON) parameters for EVOLVE task, FITNESS-TEST or
;;;     other defined type of task.  Returns immediately with a
;;;     Job-ID (jid).  Creating a Job starts a task (on a new
;;;     thread) which will execute until stopped or completed.
;;;  GET
;;;     @code{<service-base>/async?jid=<Job-ID>} Returns JSON
;;;     containing job status and results.
;;;  PUT
;;;     @code{<service-base>/async?jid=<Job-ID>&<update-vars>}
;;;     Allows some control of the task, such as stopping the task.
;;;
;;; @texi{rest}
(defpackage :software-evolution-library/rest-async-jobs
  (:nicknames :sel/rest-async-jobs)
  (:use
   :alexandria
   :arrow-macros
   :named-readtables
   :curry-compose-reader-macros
   :common-lisp
   :snooze
   :split-sequence
   :cl-json
   :iterate
   :trace-db
   :software-evolution-library/software-evolution-library
   :software-evolution-library/command-line
   :software-evolution-library/utility
   :software-evolution-library/components/test-suite
   :software-evolution-library/components/formatting
   :software-evolution-library/components/instrument
   :software-evolution-library/components/traceable
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang
   :software-evolution-library/rest-utility
   :software-evolution-library/rest-std-api
   :software-evolution-library/rest-sessions)
  (:shadowing-import-from :clack :clackup :stop)
  (:export :apply-async-job-func
           :async-job
           :async-job-name
           :define-async-job
           :lookup-job-func
           :get-job
           :session-jobs))
(in-package :software-evolution-library/rest-async-jobs)
(in-readtable :curry-compose-reader-macros)

;;; Asynchronous Jobs

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct async-job-type
    "Entry in async-job-type table."
    job-name        ; Symbol names the job type.
    arg-names       ; Lambda-list for user-defined function.
    arg-types func) ; Type of each argument
                                        ; :LAMBDA-LIST-KEYWORD pseudo-type is used
                                        ; for lambda list keywords.

  (defvar *async-job-types* (make-hash-table :test 'equalp)
    "Registry of async-job-types.")

  (defun register-async-job (job-name arg-names arg-types func)
    "Register an async-job-type."
    (setf (gethash job-name *async-job-types*)
          (make-async-job-type :job-name job-name :arg-names arg-names
                               :arg-types arg-types :func func))))

;;; The lambda-list should use (<name> <type>) for each variable name.
;;; The type can be any of: integer, float, string, or boolean.
;;;
;;; If body consists of special case (:function <function>) then
;;; <function> is assumed to be a function (or symbol bound to
;;; a function) with a lambda-list congruent with the specification
;;; of DEFINE-ASYNC-JOB. In this case that function will be called
;;; rather than a new function created.

(defmacro define-async-job
    (name lambda-list &rest body)
  "Define an async-job type."
  (let* ((arg-names (mapcar
                     (lambda (x)
                       (if (member x lambda-list-keywords)
                           x
                           (first x)))
                     lambda-list))
         (arg-types (mapcar
                     (lambda (x)
                       (if (member x lambda-list-keywords)
                           :lambda-list-keyword
                           (second x)))
                     lambda-list))
         (func (if (and (listp body)
                        (= (length body) 2)
                        (eq (first body) ':function))
                   `',(second body)
                   `(lambda (,@arg-names) ,@body))))

    `(progn
       (register-async-job
        ',name
        ',arg-names
        ',arg-types
        (lambda (arguments)
          (apply ,func arguments)))
       ',name)))

(defun lookup-async-job-type (name)
  "Given a name, lookup the async-job-type registry entry."
  (values (gethash name *async-job-types*)))

(defun apply-async-job-func (name &rest args)
  "Given a name and arguments, call the async-job-type function."
  (funcall (async-job-type-func (gethash name *async-job-types*)) args))

(defclass async-job ()
  ((name
    :initarg :name
    :accessor async-job-name
    :initform (symbol-name (gensym "JOB-"))
    :documentation "Unique name/id for the job")
   (arguments
    :initarg :arguments
    :accessor async-job-args
    :initform nil)
   (func   ;; function to run on each software object in population
    ;; e.g. EVALUATE, TEST-FITNESS, etc.
    :initarg :func
    :accessor async-job-func
    :initform nil)
   (task-runner
    :initarg :task-runner
    :accessor async-job-task-runner
    :initform nil))
  (:documentation "Task to perform asynchronously, against every item
in a population"))

(defun session-jobs (session)
  (session-store-value session "jobs"))

(defun (setf session-jobs) (value session)
  (setf (session-store-value session "jobs") value))

(defun find-job (client job-name)
  "Return the named job from the client record (if found)."
  (car (member job-name (session-jobs client)
               :key 'async-job-name :test 'equal)))

(defun format-job-as-json (async-job)
  (let ((task-runner (async-job-task-runner async-job)))
    (json:encode-json-plist-to-string
     (list
      :name (async-job-name async-job)
      :threads-running
      (task-runner-workers-count (async-job-task-runner async-job))
      :remaining-jobs
      (task-runner-remaining-jobs (async-job-task-runner async-job))
      :arguments (async-job-args async-job)
      :completed-tasks (task-runner-completed-tasks task-runner)
      :results (task-runner-results task-runner)))))

(defun get-job (cid name)
  (let* ((client (lookup-session cid)))
    (if client
        (let ((job (and name (find-job client name))))
          (if (null job)
              (let ((job-names (iter (for x in (session-jobs client))
                                     (collect (async-job-name x)))))
                (json:encode-json-to-string job-names))
              (format-job-as-json job))))))

(defun lookup-job-type-entry (name)
  "Allow some special-case names, otherwise fall through to symbol
 in SEL package by the specified name."
  (lookup-async-job-type (convert-symbol name)))

(defun lookup-job-func (func)
  "Allow some special-case names, otherwise fall through to symbol
 in SEL package by the specified name."
  (if-let ((entry (lookup-job-type-entry func)))
    (async-job-type-func entry)
    func))

(defun type-check (arguments job-type-entry)
  (declare (ignore job-type-entry)) ; use this later
  (mapcar (lambda (x)
            (mapcar (lambda (y) (convert-symbol y)) x))
          arguments))

(trace lookup-job-type-entry)
(trace type-check)
(trace convert-symbol)

(defun make-job
    (client arguments func-name func threads name)
  (apply 'make-instance
         'async-job
         :arguments arguments
         :func func
         :task-runner
         (sel/utility::task-map-async
          threads
          func
          (if (typep arguments 'population)
              (population-individuals arguments)
              (type-check arguments
                          (lookup-job-type-entry func-name))))
         (if name (list :name (string (gensym (string name)))))))

(trace make-job)
(trace session-jobs)
(trace async-job-name)


;; Async can now optionally take a population ID as `pid` or
;; arguments `arguments`...
(defroute
    async (:post "application/json" &key cid name)
  (let* ((json (handler-case
                   (json:decode-json-from-string (payload-as-string))
                 (error (e)
                   (http-condition 400 "Malformed JSON (~a)!" e))))
         (client (lookup-session cid))
         (pid (aget :pid json))  ; name/id of population
         (arguments
          (if pid ;; prefer population to arguments
              (find-population client pid) ; pid specifies a population
              (aget :arguments json))) ; else assume a list
         (func-name (aget :func json))
         (func (lookup-job-func func-name)) ; name of function to run
         (threads (or (aget :threads json) 1)) ; max number of threads to use
                                        ; must be at least 1 thread!
         (job (make-job client population func-name func threads name)))
    ;; store the software obj with the session
    ;; (push-session-store-value client "jobs" job)
    (push job (session-jobs client))
    (async-job-name job)))

(defroute
    async (:get :text/* &key cid name)
  (get-job cid name))

(defroute
    async (:get "application/json" &key cid name)
  (get-job cid name))
