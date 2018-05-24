;;; java-instrument --- Instrument java-language source files
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;;; Instrumentation

(defclass java-instrumenter (instrumenter)
  ((file-id :accessor file-id :initform nil))
  (:documentation "Handles instrumentation for JAVA software objects."))

(defmethod instrumented-p ((obj java))
  (search *instrument-log-env-name* (genome obj)))

(defmethod instrumented-p ((obj java-project))
  (some #'instrumented-p (mapcar #'cdr (evolve-files obj))))

(defmethod instrument ((obj java) &rest args)
  (apply #'instrument (make-instance 'java-instrumenter :software obj)
         args))

(defmethod instrument
  ((instrumenter java-instrumenter)
   &key points functions functions-after trace-file trace-env instrument-exit
     (filter #'identity)
   &aux (obj (software instrumenter)))
  (declare (ignorable points functions functions-after
                      trace-file trace-env
                      instrument-exit filter))
  (with-temp-file-of (src-file (ext obj)) (genome obj)
    (if (null (file-id instrumenter))
        (java-jar-exec (format nil "-instrument ~a -out=~a"
                               src-file
                               (directory-namestring src-file)))
        (java-jar-exec (format nil "-instrument ~a -out=~a -file=~a"
                               src-file
                               (directory-namestring src-file)
                               (file-id instrumenter))))
    (setf (genome obj) (file-to-string src-file)))

  obj)

(defmethod uninstrument ((obj java))
  (with-temp-file-of (src-file (ext obj)) (genome obj)
    (java-jar-exec (format nil "-uninstrument ~a -out=~a"
                           src-file
                           (directory-namestring src-file)))
    (setf (genome obj) (file-to-string src-file)))
  obj)

(defmethod instrument ((java-project java-project) &rest args
                       &aux (instrumenter (make-instance 'java-instrumenter)))
  (declare (ignorable args))
  (iterate (for (f . obj) in (instrumentation-files java-project))
           (for i upfrom 1)
           (note 3 "Instrumenting ~a" f)
           (note 4 "Instrument progress: ~a/~a"
                 i (length (instrumentation-files java-project)))
           (setf (software instrumenter) obj)
           (setf (file-id instrumenter) (1- i))
           (instrument instrumenter))
  java-project)

(defmethod uninstrument ((java-project java-project))
  (iter (for (f . obj) in (instrumentation-files java-project))
        (for i upfrom 1)
        (note 3 "Uninstrumenting ~a" f)
        (note 4 "Uninstrument progress: ~a/~a"
              i (length (instrumentation-files java-project)))
        (uninstrument obj))
  java-project)

(defmethod instrumentation-files ((java-project java-project))
  (evolve-files java-project))
