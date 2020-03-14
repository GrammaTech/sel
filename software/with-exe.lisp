;;; with-exe.lisp --- Software with an executable
(defpackage :software-evolution-library/software/with-exe
  (:nicknames :sel/software/with-exe :sel/sw/with-exe)
  (:use :gt/full
        :software-evolution-library)
  (:export :evaluate-with-script))
(in-package :software-evolution-library/software/with-exe)
(in-readtable :curry-compose-reader-macros)


;;; Software Object with an executable
(defclass software-exe (software)
  ((exe :initarg :exe :accessor raw-exe :initform nil))
  (:documentation "DOCFIXME"))

(defgeneric exe (software &optional place)
  (:documentation
   "Return the path to an executable of the software. (caching)"))

(defmethod (setf exe) (new (software software-exe))
  "DOCFIXME"
  (setf (raw-exe software) new))

(defmethod exe :around ((software software-exe) &optional place)
  "DOCFIXME

* SOFTWARE DOCFIXME
* PLACE DOCFIXME
"
  (declare (ignorable place))
  (or (raw-exe software) (setf (exe software) (or (call-next-method) :failed))))

(defgeneric delete-exe (software)
  (:documentation
   "Delete any external executables associated with the software."))

(defmethod delete-exe ((software software-exe))
  "DOCFIXME"
  (when (raw-exe software)
    (when (and (not (eq :failed (raw-exe software)))
               (probe-file (exe software)))
      (delete-file (exe software)))
    (setf (exe software) nil)))

(defun evaluate-with-script (software script pos-num neg-num)
  "Evaluate SOFTWARE with SCRIPT.
POS-NUM is the number of positive tests defined in SCRIPT NEG-NUM is
the number of negative tests.  SCRIPT will be called with the
following arguments.

  $ SCRIPT SOFTWARE-EXECUTABLE pN for all N upto POS-NUM
  $ SCRIPT SOFTWARE-EXECUTABLE nN for all N upto NEG-NUM

SCRIPT should return 0 on success and 1 on failure."
  (let ((pos 0) (neg 0))
    (if (eq (exe software) :failed)
        0
        (progn
          (loop :for i :from 1 :to pos-num
             :do (multiple-value-bind (output err-output exit)
                     (shell "~a ~a p~d" script (exe software) i)
                   (declare (ignorable output err-output))
                   (when (= exit 0) (incf pos))))
          (loop :for i :from 1 :to neg-num
             :do (multiple-value-bind (output err-output exit)
                     (shell "~a ~a n~d" script (exe software) i)
                   (declare (ignorable output err-output))
                   (when (= exit 0) (incf neg))))
          (incf *fitness-evals*)
          (delete-exe software)
          (+ pos neg)))))
