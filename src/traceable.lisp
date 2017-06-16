;;; traceable --- instrumentable software objects with dynamic traces
(in-package :software-evolution)

(defclass traceable (software)
  ((traces :initarg :traces :accessor traces :initform nil
           :documentation "Execution traces from execution of the software."))
  (:documentation
   "Instrumentable software with support for collecting dynamic traces."))


;;; Execution with trace collection.
(defgeneric collect-trace (software input)
  (:documentation
   "Execute instrumented SOFTWARE on INPUT collecting a dynamic trace."))

(defmethod collect-trace ((obj software) input)
  (with-temp-file (bin)
    (with-temp-fifo (pipe)
      ;; Start run on the input.
      (let ((proc
             (let ((components
                    (split-sequence #\Space
                      (format nil input (if bin
                                            (phenome obj :bin bin)
                                            (phenome obj))))))
               #+sbcl
               (sb-ext:run-program (car components) (cdr components)
                                   :environment
                                   (cons (concatenate 'string
                                           *instrument-log-env-name* "=" pipe)
                                         (sb-ext:posix-environ))
                                   :wait nil)
               #+ccl
               (ccl:run-program (car components) (cdr components)
                                :env (list (cons *instrument-log-env-name* pipe))
                                :wait nil)
               #-(or sbcl ccl)
               (error "Implement for non-SBCL (requires non-blocking shell)."))))
        (list (cons :input input)
              (cons :trace (unwind-protect
                                ;; Read trace output from fifo.
                                (with-open-file (in pipe)
                                  (iter (for obj = (read in nil :eof))
                                        (until (eq obj :eof))
                                        (collect obj)))
                             #+sbcl (sb-ext:process-close proc)
                             #+ccl (ccl:process-kill proc)
                             #-(or sbcl ccl)
                             (error "`collect-trace' needs SBCL or CCL."))))))))
