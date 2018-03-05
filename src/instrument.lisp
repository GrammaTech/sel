;;; instrument --- generic interface for instrumentation
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;;; Instrumentation

(defvar *instrument-log-env-name* "__SEL_TRACE_FILE"
  "Default environment variable in which to store log file.")

(defvar *instrument-handshake-env-name* "__SEL_HANDSHAKE_FILE"
  "Default environment variable in which to store log file.")

(define-constant +instrument-log-variable-name+ "__sel_trace_file"
  :test #'string=
  :documentation "Variable used for instrumentation.")

(define-constant +instrument-log-lock-variable-name+ "__sel_trace_file_lock"
  :test #'string=
  :documentation "File lock variable used for instrumentation")

(defgeneric instrumented-p (obj)
  (:documentation "Return true if OBJ is instrumented"))

(defgeneric instrument (obj &key points functions functions-after
                                 trace-file trace-env instrument-exit filter)
  (:documentation
   "Instrument OBJ to print AST index before each full statement.

The indices printed here are not clang-mutate counters, but rather the
position of the ast in (asts obj).

Keyword arguments are as follows:
  POINTS --------------- alist of additional values to print at specific points
  FUNCTIONS ------------ functions to calculate instrumentation at each point
  FUNCTIONS-AFTER ------ functions to calculate instrumentation after each point
  TRACE-FILE ----------- file for trace output
  TRACE-ENV ------------ trace output to file specified by ENV variable
  INSTRUMENT-EXIT ------ print counter of function body before exit
  FILTER --------------- function to select a subset of ASTs for instrumentation
"))

(defgeneric uninstrument (obj)
  (:documentation "Remove instrumentation from OBJ"))

(defgeneric instrumentation-files (project)
  (:documentation
   "Return files in PROJECT in the order which they would be instrumented"))

(defclass instrumenter ()
  ((software :accessor software :initarg :software :initform nil))
  (:documentation "Base class for objects which handle instrumentation.
Stores instrumentation state and provides methods for instrumentation
operations."))
