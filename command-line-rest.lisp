(defpackage :software-evolution-library/command-line-rest
  (:nicknames :sel/command-line-rest)
  (:use
   :named-readtables
   :curry-compose-reader-macros
   :common-lisp
   :trace-db
   :software-evolution-library/utility
   :software-evolution-library/command-line
   :software-evolution-library/rest-define-command-endpoint
   :software-evolution-library/rest)
  (:import-from :uiop/utility :nest)
  (:export :define-command-rest))
(in-package :software-evolution-library/command-line-rest)
(in-readtable :curry-compose-reader-macros)

(defun find-ampersand-el (el) (and (not (listp el)) (equal #\& (aref (symbol-name el) 0))))

(trace find-ampersand-el)

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
  ;; There isn't much we can do here about rest args, due to types. We leave
  ;; the binding unused for now.
  ;; Based on the parsing code for `define-command` in `fare/parse.lisp`.
  (let* ((package (package-name *package*))
         (command-line-specification (plist-get (intern "&SPEC" package) args))
         (rest-arg (let ((rest-arg (plist-get (intern "&REST" package) args)))
                     (when rest-arg (list (intern "&REST" package) rest-arg))))
         (typed-positional-args (take-until
                                 #'find-ampersand-el
                                 (plist-drop (intern "&REST" package) args)))
         (positional-args (mapcar #'car typed-positional-args))
         (aux-args (plist-drop
                    (intern "&REST" package)
                    (plist-drop
                     (intern "&SPEC" package)
                     (drop-until
                      #'find-ampersand-el
                      args)))))
    ;; NOTE: Results just status or "finished/path."
    `(progn
       (format t "~a" ,positional-args)
       (format t "~a" ,command-line-specification)
       (format t "~a" ,typed-positional-args)
       ;; 1. Define the command.
       (define-command ,name (,@positional-args &spec ,command-line-specification) ,pre-help ,post-help ,@body)
       ;; 2. Setup rest.
       (define-endpoint-route ,name #',name ,typed-positional-args ,command-line-specification))))

#|
(define-command-rest four-types-2
((a integer) (b string) (c float) (d boolean) &spec +common-command-line-options+)
nil nil
"Test that the four supported types can be passed via REST."
(format nil "~A: ~D, ~A: ~S, ~A: ~F, ~A: ~A"
(type-of a) a
(type-of b) b
(type-of c) c
(type-of d) d))
|#
