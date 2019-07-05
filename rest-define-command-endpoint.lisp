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
;;; @subsection Running the Rest API Web Service
;;;
;;; See the main service for how to run the server.
;;;
;;; @subsection Resources and Operations
;;;
;;; A Rest API supports logical resources. This section lists the logical
;;; resources supported by the SEL Rest APIs. In some cases (e.g. Software)
;;; there is a class hierarchy in the SEL package which directly models the
;;; resource. Or rather, the resource can be thought to be a distillation
;;; of the methods of that class. In other cases (e.g. Populations) there
;;; isn't a specific class in SEL. The Rest API resource in this course
;;; provides a way to name and manage lists of Software objects.
;;;
;;; @subsubsection Resources
;;;
;;; The following types of resources are supported.
;;;
;;;  Software
;;;      Any non-abstract class that inherits from SOFTWARE
;;;
;;;
;;; @subsubsection Operations on Resources
;;;
;;; Note: all operations (other than session create) require a client-ID
;;; parameter. Although only specified in the first ones below, all others
;;; require it as well.
;;;
;;; Client:
;;;
;;;  POST
;;;     @code{<service-base>/client} Create a new client.  Body
;;;     (JSON) contains initial values for special variable settings.
;;;     Returns the Client-ID.
;;;  DELETE
;;;     @code{<service-base>/client&cid=<client-ID>} Delete
;;;     the client session, and cause all the software objects
;;;     owned by the client to become garbage.
;;;
;;; Software:
;;;
;;;  POST
;;;     @code{<service-base>/soft?cid=<client-ID>&type=<software-type>}
;;;     Body, JSON format, contains path (to C file, AST, Asm file,
;;;     etc.), or a URL to a file, or the actual source code which
;;;     would comprise the file.  If the body simply contains a
;;;     software ID (sid), makes a copy of the software object.
;;;     All JSON fields which are not [path, url, code,
;;;     software-id] are passed as keyword parameters to
;;;     (MAKE-INSTANCE '<software-type> &key).  Returns a software
;;;     object ID of newly created software.
;;;  GET
;;;     @code{<service-base>/soft?cid=<client-ID>&sid=<software
;;;     ID>} Returns JSON describing software object (differs
;;;     depending on type of software).
;;;  GET
;;;     @code{<service-base>/soft?type=<software type>} Returns
;;;     IDs of live software objects of the passed type, owned by
;;;     the client.
;;;  GET
;;;     @code{<service-base>/soft} Return IDs of all live
;;;     software objects owned by the client.
;;;  PUT
;;;     @code{<service-base>/soft?sid=<software ID>} Update a
;;;     software object.  Body (JSON) contains slots to update, new
;;;     values.
;;;  DELETE
;;;     @code{<service-base>/soft?sid=<software ID>} Delete a
;;;     software object.  (work in progress)
;;;
;;;
;;; @texi{rest}
(defpackage :software-evolution-library/rest-define-command-endpoint
  (:nicknames :sel/rest-define-command-endpoint)
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
   :software-evolution-library/rest-sessions
   :software-evolution-library/rest-async-jobs
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang)
  (:shadowing-import-from :clack :clackup :stop)
  (:export :define-endpoint-route))
(in-package :software-evolution-library/rest-define-command-endpoint)
(in-readtable :curry-compose-reader-macros)

;;; WIP
;;; WIP
;;; WIP
;;;
;;; Endpoint definitions macros for `define-command-rest`.
;;;
;;; The required arguments are of the form (<name> type>), as:
;;;
;;; ((source string) (num-tests integer))
;;;
;;; The type can be any of integer, float, string, or boolean.
;;;
;;; Optional arguments are expected to be of the form of arguments in
;;; command-line.lisp, e.g.:
;;;
;;; (("help" #\h #\?)
;;;  :TYPE BOOLEAN
;;;  :OPTIONAL T
;;;  :DOCUMENTATION "display help output")
;;;
;;; This should work:
#|
> (define-endpoint-route addfive (lambda (value) (+ value 5)) ((value integer)) ())
. . .
~> curl -X POST -H "Accept: application/json" \
-H "Content-Type: application/json" \
http://127.0.0.1:9003/addfive?cid='client-1001' \
-d '{"value" : 5}'

TODO
- Ideally the thing should work without an empty json field, but
payload-as-string gets angry in that case. Maybe we can find a workaroud...
- Write a parser / handler for optional arguments
- Incorporate into define-command-rest
- Write test cases
|#
(defun lookup-main-args
    (args json)
  (mapcar  (lambda (argument)
             (if-let ((result (aget (car argument) json :test #'string=)))
               (if (typep result (cdr argument))
                   result
                   (error "Invalid type"))
               (error "Did not find required argument ~a" (car argument))))
           args))

(defun lookup-command-line-args
    (args json)
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
                        (error "Expected type ~a for key ~a (with value ~a)" arg-type name json-value)))
        ((and (not optional) init-value) (cons (intern name :keyword) init-value))
        (t '())
        ;; TODO Not all optional parameters in command-line.lisp are marked as
        ;; such. Ask eschulte (or similar) if they should be fixed and the code
        ;; below should be instated, or this current code is permissive enough.
        ;; (optional '())
        ;; (t (error "Expected key ~a" name))
        )))
  (flatten (mapcar #'lookup-cl-arg args)))

(trace aget)

(defun fact-keyword
    (value &key help verbose)
  (if help
      "Computes factorial"
      (factorial value)))

(trace fact-keyword)

(defun make-endpoint-job
    (session-id name job-fn json main-args command-line-args)
  (let* ((session (lookup-session session-id))
         (_ (note 0 "~a" json))
         (first-args (lookup-main-args main-args json))
         (_ (note 0 "~a" first-args))
         (cl-args (lookup-command-line-args command-line-args json))
         (args (list (append first-args cl-args)))
         (threads 1)
         (job (apply 'make-instance 'async-job
                     :func job-fn
                     :arguments args
                     :task-runner
                     (sel/utility::simple-task-async-runner
                      threads
                      job-fn
                      args)
                     (list :name name))))
    ;; store the job with the session
    (push job (session-jobs session))
    ;; return the job name
    (async-job-name job)))

(trace lookup-command-line-args)
(trace make-endpoint-job)
(trace sel/utility::task-map-async)
(trace sel/utility::simple-task-async-runner)

#|
(define-endpoint-route addfive (lambda (value) (+ value 5)) ((value integer)) ())
(define-endpoint-route fact (lambda (n) (alexandria::factorial n)) ((value integer)) ())
(define-endpoint-route fact #'alexandria::factorial ((value integer)) ())
(define-endpoint-route iota-endpoint #'alexandria::iota ((value integer)) ())
(define-endpoint-route fact #'alexandria::factorial ((value integer)) +common-command-line-options+)
(define-endpoint-route
fact
#'software-evolution-library/rest-define-command-endpoint::fact-keyword
((value integer)) +common-command-line-options+)
|#
(defmacro define-endpoint-route
    (route-name func required-args command-line-args)
  (let ((cid (intern (symbol-name 'cid)))
        (name (intern (symbol-name 'name)))
        (json (intern (symbol-name 'json)))
        (lookup-fn (intern (symbol-name 'lookup-fn))))
    `(progn
       (let* ((main-args (mapcar (lambda (arg) (cons (string (car arg)) (cadr arg)))
                                 ',required-args)))
         (defroute
             ,route-name (:post "application/json" &key ,cid (,name (symbol-name (gensym (string ',route-name)))))
           (let* ((,json (handler-case
                             (if-let* ((payload (payload-as-string))
                                       (string-nonempty (not (emptyp payload))))
                                      (mapcar (lambda (entry) (cons (string (car entry)) (cdr entry)))
                                              (json:decode-json-from-string payload)))
                           (error (e)
                             (http-condition 400 "Malformed JSON (~a)!" e))))
                  (,lookup-fn (lookup-job-func ,func))
                  (,lookup-fn (if ,lookup-fn ,lookup-fn ,func)))
             (make-endpoint-job ,cid ,name ,lookup-fn ,json main-args ,command-line-args))))
       (defroute
           ,route-name (:get :text/* &key ,cid (,name nil))
         (get-job ,cid (string ,name)))
       (defroute
           ,route-name (:get "application/json" &key ,cid (,name nil))
         (get-job ,cid (string ,name))))))
