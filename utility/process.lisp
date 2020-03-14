;;;; Process wrapping
;;;
;;; TODO: What is the benefit of this wrapper layer?  Just interop
;;;       between lisps implementations?  Does nothing else already
;;;       provide this?
;;;
;;; @texi{process}
(defpackage :software-evolution-library/utility/process
  (:nicknames :sel/utility/process)
  (:use :gt/full)
  (:export
   :process
   :os-process
   :process-id
   :process-input-stream
   :process-output-stream
   :process-error-stream
   :process-exit-code
   :process-running-p
   :kill-process))
(in-package :software-evolution-library/utility/process)

(defclass process ()
  ((os-process
    :initarg :os-process :initform nil :reader os-process
    :documentation "The underlying process object (compiler-specific).
This field will not usually need to be accessed directly: use methods
`process-input-stream', `process-output-stream',
`process-error-stream', `process-error-code', `process-status',
`signal-process' to interact with processes."))
  (:documentation "Object representing an external process.
Wraps around SBCL- or CCL-specific representations of external processes."))

(defgeneric process-id (process)
  (:documentation "Return the process id for PROCESS"))

(defmethod process-id ((process process))
  "Return the process id for PROCESS."
  (process-info-pid (os-process process)))

(defgeneric process-input-stream (process)
  (:documentation "Return the input stream for PROCESS."))

(defmethod process-input-stream ((process process))
  "Return the input stream for PROCESS."
  (process-info-input (os-process process)))

(defgeneric process-output-stream (process)
  (:documentation "Return the output stream for PROCESS."))

(defmethod process-output-stream ((process process))
  "Return the output stream for PROCESS."
  (process-info-output (os-process process)))

(defgeneric process-error-stream (process)
  (:documentation "Return the error stream for PROCESS."))

(defmethod process-error-stream ((process process))
  "Return the error stream for PROCESS."
  (process-info-error-output (os-process process)))

(defgeneric process-exit-code (process)
  (:documentation
   "Return the exit code for PROCESS, or nil if PROCESS has not exited."))

(defmethod process-exit-code ((process process))
  "Return the exit code for PROCESS, or nil if PROCESS has not exited."
  (and (not (process-running-p process))
       (wait-process (os-process process))))

(defgeneric process-running-p (process)
  (:documentation "Return T if PROCESS is running, NIL otherwise."))

(defmethod process-running-p ((process process))
  "Return T if PROCESS is running, NIL otherwise."
  (process-alive-p (os-process process)))

(defgeneric kill-process (process &key urgent children)
  (:documentation
   "Send a kill signal to PROCESS. If URGENT is T, send SIGKILL.
If CHILDREN is T, also kill all processes below PROCESS."))

(defmethod kill-process (process &key urgent children)
  (if (not children)
      (terminate-process (os-process process) :urgent urgent)
      (if (os-unix-p)
          (zerop (nth-value 2 (shell "kill -~d -$(ps -o pgid= ~d | tr -d ' ')"
                                     (if urgent 9 15) (process-id process))))
          (error "Killing all children not implemented on this platform"))))
