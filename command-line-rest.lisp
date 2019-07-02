(defpackage :software-evolution-library/command-line-rest
  (:nicknames :sel/command-line-rest)
  (:use
   :named-readtables
   :curry-compose-reader-macros
   :common-lisp
   :software-evolution-library/utility
   :software-evolution-library/command-line
   :software-evolution-library/rest)
  (:import-from :uiop/utility :nest)
  (:export :define-command-rest))
(in-package :software-evolution-library/command-line-rest)
(in-readtable :curry-compose-reader-macros)

(defmacro define-command-rest
    (name args pre-help post-help &rest body)
  "Define a function, executable, and a REST server function and executable.

Invokes `define-command' on NAME ARGS PRE-HELP POST-HELP and BODY to
define the NAME function and the RUN-NAME command-line executable
entry point.  See the definition of `define-command' for more
information on these arguments.

Use the above arguments and the additional ENVIRONMENT and STATUS
commands to define an asynchronous REST entry point which runs the
function NAME asynchronously returning a job ID and another entry
point which may be used to retrieve the status of the async job.  A
new RUN-NAME-REST-SERVER command-line executable entry point is
defined.

ENVIRONMENT
:   List of variables which should be let-bound around the execution
    of the REST end-point.

STATUS
:   A function which will be invoked in the dynamic environment of the
    running job to return the status of the job or, when finished, to
    return the result."
  ;; Split the args to pull out types and pass names to `define-command'.
  (let* ((arg-names (mapcar
                     (lambda (x)
                       (if (member x lambda-list-keywords)
                           x
                           (first x)))
                     args)))
    ;; NOTE: Results just status or "finished/path."
    `(progn
       ;; 1. Define the command.
       (define-command ,name ,arg-names ,pre-help ,post-help ,@body)
       ;; 2. Setup rest.
       (sel/rest:define-async-job ,name ,args :function ,name)
       )))
