;;; instrument.lisp --- Generic interface for instrumentation.
(defpackage :software-evolution-library/components/instrument
  (:nicknames :sel/components/instrument :sel/cp/instrument)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/project)
  (:export :instrument
           :uninstrument
           :instrumented-p
           :instrumenter
           :var-instrument
           :*instrument-log-env-name*
           :*instrument-handshake-env-name*
           :+instrument-log-variable-name+
           :+instrument-log-lock-variable-name+))
(in-package :software-evolution-library/components/instrument)
(in-readtable :curry-compose-reader-macros)


;;;; Instrumentation

(defvar *instrument-log-env-name* "__SEL_TRACE_FILE"
  "Default environment variable in which to store log file.")

(defvar *instrument-handshake-env-name* "__SEL_HANDSHAKE_FILE"
  "Default environment variable in which to store log file.")

(define-constant +instrument-log-variable-name+ "__sel_trace_file"
  :test #'string=
  :documentation "Variable used for instrumentation.")

(defgeneric instrumented-p (obj)
  (:documentation "Return true if OBJ is instrumented"))

(defgeneric instrument (obj &key points functions functions-after
                                 trace-file trace-env instrument-exit filter
                                 num-threads &allow-other-keys)
  (:documentation
   "Instrument OBJ to print AST index before each full statement.

The indices printed here are the position of the ast in (asts obj).

Keyword arguments are as follows:
  POINTS --------------- alist of additional values to print at specific points
  FUNCTIONS ------------ functions to calculate instrumentation at each point
  FUNCTIONS-AFTER ------ functions to calculate instrumentation after each point
  TRACE-FILE ----------- file for trace output
  TRACE-ENV ------------ trace output to file specified by ENV variable
  INSTRUMENT-EXIT ------ print counter of function body before exit
  FILTER --------------- function to select a subset of ASTs for instrumentation
  NUM-THREADS ---------- number of threads to use for instrumentation"))

(defgeneric var-instrument (key instrumenter ast &key print-strings)
  (:documentation
   "Generate ASTs for variable instrumentation.
* KEY a function used to pull the variable list out of AST
* INSTRUMENTER current instrumentation state
* AST the AST to instrument"))

(defgeneric uninstrument (obj &key num-threads)
  (:documentation "Remove instrumentation from OBJ

Keyword arguments are as follows:
  NUM-THREADS ---------- number of threads to use for uninstrumenting"))

(defgeneric instrumentation-files (project)
  (:documentation
   "Return files in PROJECT in the order which they would be instrumented"))

(defclass instrumenter ()
  ((software :accessor software :initarg :software :initform nil))
  (:documentation "Base class for objects which handle instrumentation.
Stores instrumentation state and provides methods for instrumentation
operations."))

(defmethod instrumented-p ((project project))
  "Return true if PROJECT is instrumented."
  (some #'instrumented-p (mapcar #'cdr (evolve-files project))))

(defmethod instrumentation-files ((project project))
  "Return files in PROJECT in the order which they would be instrumented."
  (evolve-files project))

(defmethod uninstrument ((project project)
                         &key (num-threads 1))
  "Remove instrumentation from PROJECT."
  (task-map num-threads
            #'uninstrument
            (mapcar #'cdr (instrumentation-files project)))
  project)
