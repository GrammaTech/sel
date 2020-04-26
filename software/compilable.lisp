;;; compilable --- Mixin class for compiled software
;;;
;;; A compilable software object is one which may be compiled
;;; to an executable binary or object file.  It contains slots
;;; for both the compiler and compiler flags used in compilation.
;;; Additionally, it overrides the @ref{phenome} method to
;;; create a compiled executable binary or object file.
;;;
;;; @text{compilable}
(defpackage :software-evolution-library/software/compilable
  (:nicknames :sel/software/compilable :sel/sw/compilable)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/file)
  (:export :compilable :compiler :flags))
(in-package :software-evolution-library/software/compilable)
(in-readtable :curry-compose-reader-macros)


;;; Data structure definition
(defclass compilable ()
  ((compiler :initarg :compiler :initform nil :accessor compiler)
   (flags :initarg :flags :initform nil :accessor flags))
  (:documentation "Mixin class for compiled software."))

(defmethod copy :around ((obj compilable)
                         &key (compiler nil compiler-supplied-p)
                           (flags nil flags-supplied-p)
                         &allow-other-keys)
  (let ((copy (call-next-method)))
    (setf (slot-value copy 'compiler)
          (if compiler-supplied-p compiler (slot-value obj 'compiler))
          (slot-value copy 'flags)
          (if flags-supplied-p flags (slot-value obj 'flags)))
    copy))


;;; Method overrides for compilable-specific behavior
(defmethod phenome ((obj compilable) &key (bin (temp-file-name)))
  "Compile OBJ to create an executable version of the software
on the filesystem at BIN."
  #-ccl (declare (values t fixnum string string string))
  (setf bin (namestring bin))
  (with-temporary-file-of (:pathname src :type (ext obj)) (genome-string obj)
    (multiple-value-bind (stdout stderr errno)
        (shell "~a ~a -o ~a ~{~a~^ ~}" (compiler obj) src bin (flags obj))
      (restart-case
          (unless (zerop errno)
            (error (make-condition 'phenome :text stderr :obj obj :loc src)))
        (retry-project-build ()
          :report "Retry `phenome' on OBJ."
          (phenome obj :bin bin))
        (return-nil-for-bin ()
          :report "Allow failure returning NIL for bin."
          (setf bin nil)))
      (values bin errno stderr stdout src))))
