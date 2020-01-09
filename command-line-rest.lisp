(defpackage :software-evolution-library/command-line-rest
  (:nicknames :sel/command-line-rest)
  (:use
   :named-readtables
   :curry-compose-reader-macros
   :common-lisp
   :software-evolution-library
   :software-evolution-library/utility
   :software-evolution-library/command-line
   :software-evolution-library/rest/define-command-endpoint
   :software-evolution-library/rest/async-jobs
   :software-evolution-library/rest)
  (:import-from :uiop :writeln :truenamize :nest)
  (:import-from :software-evolution-library/rest/async-jobs
                :lookup-session-job-status)
  (:import-from :clack :clackup :stop)
  (:import-from :snooze :make-clack-app :defroute
                :payload-as-string :http-condition)
  (:import-from :cl-json :decode-json-from-string :encode-json-to-string)
  (:export :define-command-async-rest
           :define-command-rest
           :clackup :stop :make-clack-app :defroute
           :*port*
           :*address*
           :payload-as-string :http-condition
           :decode-json-from-string :encode-json-to-string))
(in-package :software-evolution-library/command-line-rest)
(in-readtable :curry-compose-reader-macros)

(defvar *port* 5000 "Port on which to run the clack server.")
(defvar *address* "127.0.0.1" "Address on which to bind the clack server.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +clackup-command-line-options+
    `((("port") :type integer :optional t :initial-value *port*
       :documentation "Port to use when starting the clack server.")
      (("address") :type string :optional t :initial-value "127.0.0.1"
       :documentation "Address to which the clack server with bind, ~
                       \"0.0.0.0\" allows remote connections")
      (("debug") :type boolean :optional t :initial-value nil
       :documentation "Run the clack server in debug mode.")
      (("silent") :type boolean :optional t :initial-value nil
       :documentation "Run the clack server in silent mode.")))
  (defparameter +server-command-line-options+
    (append +common-command-line-options+ +clackup-command-line-options+)))

(defmacro define-command-rest (name args pre-help post-help &rest body)
  `(progn (define-command ,name ,args ,pre-help ,post-help ,@body)
          (define-command-rest-server ,name)))

(defmacro define-command-rest-server (name)
  "Call `define-command' with all args, then define a serve-NAME REST server."
  (nest
   (let ((package (package-name *package*))))
   (flet ((in-pkg (symbol) (intern (string symbol) package))))
   (let ((spec-symbol (in-pkg '&spec))
         (aux-symbol (in-pkg '&aux))
         (server-name (intern (concatenate 'string "SERVE-" (string name))
                              package))
         (help-name (intern (concatenate 'string
                                         "SHOW-HELP-FOR-SERVE-" (string name))
                            package))))
   `(define-command ,server-name
        (,spec-symbol +server-command-line-options+ ,aux-symbol server)
      ,(format nil "Serve ~A as a rest end point." name) ""
      (declare (ignorable ,@(mapcar #'in-pkg '(eval load language))))
      (when ,(in-pkg 'quiet) (setf ,(in-pkg 'silent) t))
      (when ,(in-pkg 'help)
        (,help-name)
        (exit-command ,server-name 0))
      (setf *address* ,(in-pkg 'address))
      (setf *port* ,(in-pkg 'port))
      ;; Install exit handler for User C-c.
      (flet ((shutdown
                 (&optional (message "Stopping server . . .") (errno 0))
               (format t "~a" message)
               (stop server)
               (exit-command ,server-name errno)))
        ;; From https://github.com/LispCookbook/cl-cookbook/blob/master/scripting.md
        (handler-case
            ;; Run server, and wait for keyboard input to terminate.
            ;; Borrowed from `sel/rest.lisp`.
            (progn
              (setf server
                    (clackup (make-clack-app)
                             :port *port* :address *address*
                             :debug ,(in-pkg 'debug) :silent ,(in-pkg 'silent)))
              (unless *lisp-interaction*
                (loop :for char := (read-char *standard-input* nil #\p) :do
                     (if (member char '(#\q #\Q))
                         (shutdown)
                         (sleep 1)))))
          ;; Catch a user's C-c.
          (#.interrupt-signal ()
            (shutdown "Shutting down server." 130))
          (error (e)
            (shutdown (format nil "unexpected error ~S" e) 1)))
        (exit-command ,server-name 0 server)))))

(declaim (special *fitness-predicate*
                  *max-evals* *max-time* *orig* *population*
                  *target-fitness-p* *test-suite* *threads*))

(defmacro define-command-async-rest
    ((name &key (environment '(*fitness-predicate*
                               *max-evals* *max-time* *orig* *population*
                               *target-fitness-p* *test-suite* *threads*))
           (status 'lookup-session-job-status))
             args pre-help post-help &body body)
  "Define a function, executable, and a REST server function and executable.

Invokes `define-command' on NAME ARGS PRE-HELP POST-HELP and BODY to
define the NAME function and the RUN-NAME command-line executable
entry point.  See the definition of `define-command' for more
information on these arguments.

Use the above arguments and the additional ENVIRONMENT and STATUS
keywords to define an asynchronous REST entry point which runs the
function NAME asynchronously returning a job ID and another entry
point which may be used to retrieve the status of the async job.  A
new RUN-SERVE-NAME command-line executable entry point is defined.

ENVIRONMENT
:   List of variables which should be let-bound around the execution
    of the REST end-point.

STATUS
:   A function which will be invoked in the dynamic environment of the
    running job to return the status of the job or, when finished, to
    return the result."
  ;; Split the args to pull out types and pass names to `define-command'.
  ;; There isn't much we can do here about rest args, due to types. We leave
  ;; the binding unused for now.
  ;; Based on the parsing code for `define-command` in `fare/parse.lisp`.
  (let* ((package (package-name *package*))
         ;; I would prefer not to need route-name, but would need to change to
         ;; a rest library that does not use CLOS generics to represent routes
         ;; (to prevent the name collision between defining the entry function
         ;; and the endpoint).
         (route-name (intern (concatenate 'string "REST-"
                                          (string-upcase (string name)))
                             package))
         (spec-symbol (intern "&SPEC" package))
         (rest-symbol (intern "&REST" package))
         (command-line-specification (plist-get spec-symbol args))
         (typed-positional-args (take-until
                                 «and [#'not #'listp]
                                      [{equal #\&} {aref _ 0} #'symbol-name]»
                                 (plist-drop rest-symbol args)))
         (positional-args (mapcar #'car typed-positional-args)))
    ;; NOTE: Results just status or "finished/path."
    `(progn
       ;; 1. Define the command.
       (define-command ,name (,@positional-args
                              ,spec-symbol ,command-line-specification)
         ,pre-help ,post-help ,@body)
       ;; 2. Define the rest endpoint.
       (define-endpoint-route ,route-name #',name
         ,typed-positional-args
         ,command-line-specification
         ,environment ,status)
       ;; 3. Define the rest server command.
       (define-command-rest-server ,name))))

#+comment
(define-command-rest (four-types-2)
    ((a integer) (b string) (c float) (d boolean)
     &spec +common-command-line-options+)
  "Test that the four supported types can be passed via REST."
  nil
  (format nil "~A: ~D, ~A: ~S, ~A: ~F, ~A: ~A"
          (type-of a) a
          (type-of b) b
          (type-of c) c
          (type-of d) d))

#+comment
(macroexpand-1 '(define-command-rest (fact-entry)
                 ((n integer) &spec +common-command-line-options+)
                 "Test that canonical REST endpoints work. Computes factorial."
                 #.(format nil
                    "~%Built from SEL ~a, and ~a ~a.~%"
                    +software-evolution-library-version+
                    (lisp-implementation-type) (lisp-implementation-version))
                 (declare (ignorable quiet verbose))
                 (when help (show-help-for-fact-entry))
                 (factorial n)))
