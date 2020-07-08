;;; rest.lisp --- RESTful server definition for SEL.
;;;
;;; Rest server definition for Software Evolution Library
;;;
;;; The Rest API for Software Evolution Library is implemented as a web
;;; service which may be accessed via HTTP resources. This file serves as an
;;; entry point to these definitions, providing a simple way to start up the
;;; main REST service.
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
;;; Starting the server:
;;;
;;;     (start-server)
;;;
;;; Stopping the server:
;;;
;;;     (stop-server)
;;;
;;; Restart the server:
;;;
;;;     (start-server)            ;; will stop, if running, then start
;;;
;;; @subsection REST Services
;;;
;;; By default, this provides the endpoints defined in sessions, the standard
;;; REST API, and asynchronous jobs, plus providing the define-command-endpoint
;;; macro. If you would like to create a rest serevr without all of these
;;; resources, you should recreate a similar file but omit the imports you wish
;;; to avoid using.
;;;
;;; @texi{rest}
(defpackage :software-evolution-library/rest
  (:nicknames :sel/rest)
  (:use
   :gt/full
   :snooze
   :cl-json
   :software-evolution-library/software-evolution-library
   :software-evolution-library/components/test-suite
   :software-evolution-library/components/formatting
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang
   :software-evolution-library/rest/utility
   :software-evolution-library/rest/sessions
   :software-evolution-library/rest/std-api
   :software-evolution-library/rest/async-jobs
   :software-evolution-library/rest/define-command-endpoint
   :software-evolution-library/command-line)
  (:shadowing-import-from :clack :clackup :stop)
  (:shadowing-import-from :snooze :start)
  (:export :lookup-session
           :session-property
           :start-server
           :stop-server
           :define-async-job
           :apply-async-job-func))
(in-package :software-evolution-library/rest)
(in-readtable :curry-compose-reader-macros)

(setf snooze::*catch-http-conditions* nil)
(setf snooze::*catch-errors* :verbose)

(defvar *server* nil)
(defvar *default-rest-port* 9003)

(defun start-server (&optional (port *default-rest-port*))
  (if *server*
      (stop-server))
  (setf *server* (clack:clackup (snooze:make-clack-app) :port port)))

(defun stop-server ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)))

;;; Main REST server Command

(define-command rest-server
    (port &spec +common-command-line-options+ &aux handler)
  "Run the SEL rest server."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose load eval out-dir
                      read-seed save-seed language))
  (flet ((shutdown (&optional (message "quit") (errno 0))
           (format t "Stopping server on ~a~%" message)
           (stop handler)
           (exit-command rest-server errno)))
    (when help (show-help-for-rest-server) (exit-command rest-server 0))
    (setf handler (clackup (make-clack-app) :port (parse-integer port)))
    ;; From https://github.com/LispCookbook/cl-cookbook/blob/master/scripting.md
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
        (shutdown (format nil "unexpected error ~S" e) 1)))))
