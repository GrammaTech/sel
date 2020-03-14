;;; define-command-endpoint.lisp --- RESTful interface for command endpoints.
;;;
;;; Rest Endpoint definitions for Software Evolution Library
;;;
;;; This allows for the easy definition of new command endpoints to run
;;; the specified input as an asynchronous task, including retrieving the status
;;; and results of the task. See the core REST file for a full description and
;;; how to start a rest server.
;;;
;;; @subsubsection Dependencies
;;;
;;; The Rest API leverages a number of Common Lisp components, which
;;; are open-source and generally available via Quicklisp. In addition,
;;; this file is built from the session and asynchronous job definitions for
;;; REST provided as part of SEL.
;;;
;;; @subsubsection Using the endpoint service.
;;;
;;; The endpoint service provides one main entry point,
;;; @code{define-endpoint-route}. This takes the following arguments:
;;;
;;;  - route-name - The name to use for the REST route.
;;;  - func - The function to invoke on given arguments
;;;  - required-args - The named arguments to the function with associated
;;;    types, which will be used to decode the POST JSON.
;;;  - command-line-args (optional) - Additional, optional arguments defined as
;;;    per command-line.lisp.
;;;  - environment (optional) - Variables to let-bind before starting the
;;;    function (such as globals we should dynamically bind). These will be
;;;    initialized to NIL before each `func` task is created.
;;;  - status-fn (optional) - A function to run to retrieve the status of the
;;;    endpoint. It takes the current session and job name as input. If none is
;;;    specified, it defaults to
;;;    @code{sel/rest/async-jobs::lookup-session-job-status}.
;;;
;;; When invoked, this will produce a new REST resource with the following
;;; operations:
;;;
;;;  POST
;;;     @code{<service-base>/<route-name>?cid=<client-ID>&name=<task-name>}
;;;     This also takes JSON post data whose keys should correspond (by name)
;;;     to the required arguments specified in @code{required-args}, plus any
;;;     additional, optional arguments as specified by
;;;     @code{command-line-args}. It starts a new, asynchronous job with the
;;;     appropriate name and returns the job name. If @code{name} is not
;;;     specified, this will generate a new name based on the route name.
;;;  GET
;;;     @code{<service-base>/<route-name>?cid=<client-ID>}
;;;     Returns the list of active jobs for the session.
;;;  GET
;;;     @code{<service-base>/<route-name>soft?cid=<client-ID>&name=<task-name>}
;;;     Invokes `status-fn` on the client session and job name and returns the
;;;     result.
;;;  PUT
;;;     @code{<service-base>/<route-name>soft?cid=<client-ID>&name=<task-name>}
;;;     Performs the specified JSON operation on the named task, if
;;;     appropriate. (Unimplemented.)
;;;  DELETE
;;;     @code{<service-base>/<route-name>soft?cid=<client-ID>&name=<task-name>}
;;;     Terminates the named task, if appropriate. (Unimplemented.)
;;;
;;; @texi{rest-define-command-endpoint}
(defpackage :software-evolution-library/rest/define-command-endpoint
  (:nicknames :sel/rest/define-command-endpoint)
  (:use
   :gt/full
   :snooze
   :software-evolution-library/software-evolution-library
   :software-evolution-library/command-line
   :software-evolution-library/components/test-suite
   :software-evolution-library/components/formatting
   :software-evolution-library/components/instrument
   :software-evolution-library/components/traceable
   :software-evolution-library/rest/sessions
   :software-evolution-library/rest/async-jobs
   :software-evolution-library/rest/utility
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang
   :software-evolution-library/utility/task)
  (:shadowing-import-from :clack :clackup :stop)
  (:import-from :software-evolution-library/rest/async-jobs
                :lookup-session-job-status)
  (:export :define-endpoint-route))
(in-package :software-evolution-library/rest/define-command-endpoint)
(in-readtable :curry-compose-reader-macros)

(defun lookup-main-args
    (args json)
  "Resolve the `(<name> . <type>)` list of arguments from the JSON payload."
  (mapcar  (lambda (argument)
             (if-let ((result (aget (car argument) json :test #'string=)))
               (if (typep result (cdr argument))
                   result
                   (error "Invalid type"))
               (error "Did not find required argument ~a" (car argument))))
           args))

(defun lookup-command-line-args
    (args json)
  "Resolve the command line-based list of arguments from the JSON payload."
  (defun lookup-cl-arg
      (argument)
    (let* ((name (string-upcase (caar argument)))
           (properties (cdr argument))
           (arg-type (getf properties :type))
           (optional (getf properties :optional))
           (init-value (getf properties :initial-value))
           (json-value (aget name json :test #'string=)))
      (cond
        (json-value (if (typep json-value arg-type)
                        (cons (intern name :keyword) json-value)
                        (error "Expected type ~a for key ~a (with value ~a)"
                               arg-type name json-value)))
        ((and (not optional) init-value) (cons (intern name :keyword)
                                               init-value))
        ;; Ignore anything that does not have a default value or definition.
        (t '()))))
  (flatten (mapcar #'lookup-cl-arg args)))

(defun make-endpoint-job
    (session-id name job-fn json main-args command-line-args)
  "Perform the actual setup and invocation for the endpoint POST request.
Starts an asynchronous job through the task runner and pushes it onto the
client session jobs."
  (let* ((session (lookup-session session-id))
         (first-args (lookup-main-args main-args json))
         (cl-args (lookup-command-line-args command-line-args json))
         (args (list (append first-args cl-args)))
         (threads 1)
         (job (apply 'make-instance 'async-job
                     :func job-fn
                     :arguments args
                     :task-runner
                     (simple-task-async-runner
                      threads
                      job-fn
                      args)
                     (list :name name))))
    ;; store the job with the session
    (push job (session-jobs session))
    ;; return the job name
    (async-job-name job)))

(defmacro define-endpoint-route
    (route-name func required-args
     &optional (command-line-args '())
       (environment NIL)
       (status-fn 'lookup-session-job-status))
  "Macro to define routes to run the function and retrieve results.

The `define-endpoint-route` macro sets up endpoint routes via `DEFROUTE`
to start asynchronous jobs remotely, mirroring the command definitions from
`define-command`. The arguments are:

- route-name - The name to use for the REST route.
- func - The function to invoke on given arguments
- required-args - The named arguments to the function with associated
  types, which will be used to decode the POST JSON
  (e.g., `((x integer) (y string))`).
- command-line-args (optional) - Additional, optional arguments defined as
  per command-line.lisp.
- environment (optional) - Variables to let-bind before starting the
  function (such as globals we should dynamically bind). These will be
  initialized to NIL before each `func` task is created.
- status-fn (optional) - A function to run to retrieve the status of the
  endpoint. It takes the current session and job name as input. If none is
  specified, it defaults to 'sel/rest/async-jobs::lookup-session-job-status.

For example, the following invocation of `define-endpoint-route` will set up an
endpoint that adds five to numbers:

    (define-endpoint-route addfive (lambda (value) (+ value 5)) ((value integer)))

An appropriate REST invocation for this endpoint might look like:

    curl -X POST -H \"Accept: application/json\" \
                 -H \"Content-Type: application/json\" \
                 http://127.0.0.1:9003/addfive?cid='client-1001' \
                 -d '{\"value\" : 5}'

In addition, you can directly name endpoint functions, e.g.:

    (define-endpoint-route fact #'alexandria::factorial ((value integer)))

In this case, the function must be in scope wherever this macro is envoed."
  (let* ((package (package-name *package*))
         (cid (intern (symbol-name 'cid) package))
         (name (intern (symbol-name 'name) package))
         (json (intern (symbol-name 'json) package))
         (lookup-fn (intern (symbol-name 'lookup-fn) package))
         ;; If the environment is passed in with a leading quote, we have to
         ;; strip it so that define-endpoint-route can map over it properly.
         (environment (cond
                        ((and environment (equal environment '(QUOTE NIL))) NIL)
                        ((equal (car environment) 'QUOTE) (cadr environment))
                        (t environment)))
         (bindings (if environment
                       (mapcar (lambda (var) (list var nil)) environment)
                       NIL)))
    `(progn
       (let ((main-args (mapcar (lambda (arg)
                                  (cons (string (car arg)) (cadr arg)))
                                ',required-args)))
         (defroute
             ,route-name (:post "application/json"
                                &key ,cid
                                (,name (string-upcase
                                        (make-gensym-string ',route-name))))
           (let* ((,json
                   (handler-case
                       (if-let ((payload (decode-json-payload)))
                         (mapcar «cons [#'string #'car] #'cdr»
                                 payload))
                     (error (e)
                       (http-condition
                        400
                        "Malformed JSON (~a)!" e))))
                  (,lookup-fn (lookup-job-func ,func))
                  (,lookup-fn (if ,lookup-fn ,lookup-fn ,func)))
             (let ,bindings
               (make-endpoint-job ,cid ,name ,lookup-fn
                                  ,json main-args ,command-line-args)))))
       (defroute
           ,route-name (:get :text/* &key ,cid (,name nil))
         (funcall (function ,status-fn)
                  (lookup-session ,cid) (string-upcase (string ,name))))
       (defroute
           ,route-name (:get "application/json" &key ,cid (,name nil))
         (funcall (function ,status-fn)
                  (lookup-session ,cid) (string-upcase (string ,name)))))))
