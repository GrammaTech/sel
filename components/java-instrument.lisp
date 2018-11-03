;;; java-instrument.lisp --- Instrument java-language source files.
(defpackage :software-evolution-library/components/java-instrument
  (:nicknames :sel/components/java-instrument :sel/cp/java-instrument)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :software-evolution-library
        :software-evolution-library/utility)
  (:export :java-instrumenter))
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;;; Instrumentation

(defclass java-instrumenter (instrumenter)
  ((file-id :accessor file-id :initarg :file-id :initform nil))
  (:documentation "Handles instrumentation for JAVA software objects."))

(defclass instrument-java-object-task    (task) ())
(defclass instrument-java-project-task   (task) ())
(defclass uninstrument-java-object-task  (task) ())
(defclass uninstrument-java-project-task (task) ())

(defmethod instrumented-p ((obj java))
  (search *instrument-log-env-name* (genome obj)))

(defmethod instrument ((obj java) &rest args)
  (apply #'instrument (make-instance 'java-instrumenter :software obj)
         args))

(defmethod instrument
  ((instrumenter java-instrumenter)
   &key points functions functions-after trace-file trace-env instrument-exit
     (filter #'identity) (num-threads 1)
   &aux (obj (software instrumenter)))
  (declare (ignorable points functions functions-after
                      trace-file trace-env
                      instrument-exit filter
                      num-threads))
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

(defmethod process-task ((task instrument-java-object-task) runner)
  (declare (ignorable runner))
  (instrument (task-object task)))

(defmethod uninstrument ((obj java) &key (num-threads 1))
  (declare (ignorable num-threads))
  (with-temp-file-of (src-file (ext obj)) (genome obj)
    (java-jar-exec (format nil "-uninstrument ~a -out=~a"
                           src-file
                           (directory-namestring src-file)))
    (setf (genome obj) (file-to-string src-file)))
  obj)

(defmethod process-task ((task uninstrument-java-object-task) runner)
  (declare (ignorable runner))
  (uninstrument (task-object task)))

(defmethod task-job ((task instrument-java-project-task) runner)
  (declare (ignorable runner))
  (let ((file-id -1)
        (files (instrumentation-files (task-object task))))
    (lambda ()
      (when-let ((file (pop files)))
        (incf file-id)
        (make-instance 'instrument-java-object-task
          :object (make-instance 'java-instrumenter
                    :software (cdr file)
                    :file-id file-id))))))

(defmethod instrument ((java-project java-project) &rest args)
  (declare (ignorable args))
  (run-task-and-block (make-instance 'instrument-java-project-task
                        :object java-project)
                      (or (plist-get :num-threads args) 1))

  java-project)

(defmethod task-job ((task uninstrument-java-project-task) runner)
  (declare (ignorable runner))
  (let ((file-id -1)
        (files (instrumentation-files (task-object task))))
    (lambda ()
      (when-let ((file (pop files)))
        (incf file-id)
        (make-instance 'uninstrument-java-object-task
          :object (cdr file))))))

(defmethod uninstrument ((java-project java-project) &key (num-threads 1))
  "Remove instrumentation from JAVA-PROJECT."
  (run-task-and-block (make-instance 'uninstrument-java-project-task
                        :object java-project)
                      num-threads)
  java-project)

(defmethod instrumentation-files ((java-project java-project))
  (evolve-files java-project))
