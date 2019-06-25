(defpackage :software-evolution-library/command-line-rest
  (:nicknames :sel/rest)
  (:use
   :named-readtables
   :curry-compose-reader-macros
   :common-lisp
   :software-evolution-library/utility
   :software-evolution-library/command-line
   :software-evolution-library/rest)
  (:export :define-command-rest))
(use-package :software-evolution-library/command-line-rest)
(in-readtable :curry-compose-reader-macros)

(defmacro define-command-rest
    (name args pre-help post-help environment status &rest body)
  "invokes `define-command'.

GLOBALS -- List of variables which should be let-bound around the
           execution of the REST end-point.



"
  ;; NOTE: Results just status or "finished/path."
  `(progn
     ;; 1. Define the command.
     (define-command ,name ,args ,pre-help ,post-help ,@body)
     ;; 2. Setup rest.
     ;; Define the async.
     (defroute ...)
     ;; define an ansyc job, using ENVIRONMENT and invoking NAME
     (define-command ,(sym-cat name '-rest-server)
         (port &spec +common-command-line-options+ &aux handler)
       ,(format nil "Run the ~a rest server." name)
       #.(format nil
                 "~%Built from SEL ~a, and ~a ~a.~%"
                 +software-evolution-library-version+
                 (lisp-implementation-type) (lisp-implementation-version))
       )
     ))
