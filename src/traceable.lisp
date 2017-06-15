;;; traceable --- instrumentable software objects with dynamic traces

(defclass traceable (software)
  ((traces :initarg :traces :accessor traces :initform nil
           :documentation "Execution traces from execution of the software."))
  (:documentation
   "Instrumentable software with support for collecting dynamic traces."))

(defgeneric read-trace (software in)
  (:documentation "Read trace from IN and apply to SOFTWARE."))

(defmethod read-trace ((obj traceable) (in stream))
  (push (read-trace-stream in) (traces obj)))

(defmethod read-trace ((obj traceable) (in string))
  (read-trace obj (pathname in)))

(defmethod read-trace ((obj traceable) (in pathname))
  (push (read-trace-file in) (traces obj)))


;;; Execution with trace collection.
(defgeneric collect-traces (software inputs &rest instrument-args)
  (:documentation "Execute SOFTWARE on INPUTS collecting dynamic traces."))

(defmethod collect-traces
    ((obj software) (inputs sequence) &rest instrument-args)
  (iter (for input in inputs)
        (apply #'collect-trace obj input instrument-args)
        (finally obj)))

(defgeneric collect-trace (software input &rest instrument-args)
  (:documentation "Execute SOFTWARE on INPUT collecting a dynamic trace."))

(defmethod collect-trace ((obj software) input &rest instrument-args)
  (let ((inst (apply #'instrument obj instrument-args)))
    (with-temp-file (bin)
      (run-on-input inst input :bin bin))))

(defgeneric run-on-input (software input &key bin)
  (:documentation "Run SOFTWARE on INPUT."))

(defmethod run-on-input ((obj software) (input string) &key bin)
  (shell input (if bin
                   (phenome obj :bin bin)
                   (phenome obj))))
