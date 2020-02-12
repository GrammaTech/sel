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
        :software-evolution-library/utility
        :software-evolution-library/components/instrument
        :software-evolution-library/software/parseable
        :software-evolution-library/software/source
        :software-evolution-library/software/java
        :software-evolution-library/software/project
        :software-evolution-library/software/java-project)
  (:export :java-instrumenter))
(in-package :software-evolution-library/components/java-instrument)
(in-readtable :curry-compose-reader-macros)


;;;; Instrumentation

(defclass java-instrumenter (instrumenter)
  ((file-id :accessor file-id :initarg :file-id :initform nil))
  (:documentation "Handles instrumentation for JAVA software objects."))

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

(defmethod uninstrument ((obj java) &key (num-threads 1))
  (declare (ignorable num-threads))
  (with-temp-file-of (src-file (ext obj)) (genome obj)
    (java-jar-exec (format nil "-uninstrument ~a -out=~a"
                           src-file
                           (directory-namestring src-file)))
    (setf (genome obj) (file-to-string src-file)))
  obj)

(defmethod instrument ((java-project java-project) &rest args)
  (declare (ignorable args))
  (task-map (or (plist-get :num-threads args) 1)
            (lambda (instrumenter)
              (apply #'instrument instrumenter args))
            (iter (for (file . obj) in (instrumentation-files java-project))
                  (for file-id upfrom 0)
                  (declare (ignorable file))
                  (collect (make-instance 'java-instrumenter
                             :software obj
                             :file-id file-id))))
  java-project)
