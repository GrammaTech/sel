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
(use-package :software-evolution-library/command-line-rest)
(in-readtable :curry-compose-reader-macros)

(defmacro define-command-rest
    (name args pre-help post-help environment status &rest body)
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
  (let ((arg-names (mapcar #'first args))
        (arg-types (mapcar #'second args)))
    (flet ((symcat (&rest syms)
             (intern (mapconcat (lambda (el) (string-upcase (string el)))
                                syms "-"))))
      ;; NOTE: Results just status or "finished/path."
      `(progn
         ;; 1. Define the command.
         (define-command ,name ,arg-names ,pre-help ,post-help ,@body)
         ;; 2. Setup rest.
         ;; 2.1 Define an ansyc job, using ENVIRONMENT and invoking NAME
         ;; 2.2 Define the entry point
         ;; 2.3 Define the command-line executable to run the server
         (define-command ,(symcat name '-rest-server)
             (port &spec +common-command-line-options+ &aux handler)
           ,(format nil "Run the ~a rest server." name)
           #.(format nil
                     "~%Built from SEL ~a, and ~a ~a.~%"
                     +software-evolution-library-version+
                     (lisp-implementation-type) (lisp-implementation-version))
           (declare
            (ignorable quiet verbose load eval out-dir read-seed save-seed))
           (flet ((shutdown (&optional (message "quit") (errno 0))
                    (format t "Stopping server on ~a~%" message)
                    (stop handler)
                    (exit-command rest-server errno)))
             (when help
               (show-help-for-rest-server)
               (exit-command rest-server 0))
             (setf handler
                   (clackup (make-clack-app) :port (parse-integer port)))
             (handler-case
                 (iter (for char = (read-char))
                       (when (or (eql char #\q) (eql char #\Q))
                         (shutdown)))
               ;; Catch a user's C-c
               (#+sbcl sb-sys:interactive-interrupt
                 #+ccl  ccl:interrupt-signal-condition
                 #+clisp system::simple-interrupt-condition
                 #+ecl ext:interactive-interrupt
                 #+allegro excl:interrupt-signal
                 () (shutdown "abort" 0))
               (error (e)
                 (shutdown (format nil "unexpected error ~S" e) 1)))))))))
