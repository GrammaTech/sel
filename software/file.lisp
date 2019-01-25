;;; file.lisp --- mixin for software in files
;;;
;;; This mixin should be utilized when preserving
;;; file attributes (e.g. permissions/modification
;;; time) is important.
;;;
;;; @texi{file}

(defpackage :software-evolution-library/software/file
  (:nicknames :sel/software/file :sel/sw/file)
  (:use :common-lisp
        :software-evolution-library
        :software-evolution-library/utility)
  (:import-from :osicat :file-permissions)
  (:export :file))
(in-package :software-evolution-library/software/file)

(defclass file ()
  ((permissions       :initarg :permissions
                      :accessor permissions
                      :initform nil)
   (modification-time :initarg :modification-time
                      :accessor modification-time
                      :initform nil))
  (:documentation
   "Mixin class for software where preserving file attributes is important."))

(defun file-modification-time (file)
  "Return FILE's modification time in the form YYYYMMDDHHMM.SS"
  (multiple-value-bind (stdout stderr exit)
      (shell "date +%Y%m%d%H%M.%S -r ~a" file)
    (declare (ignorable stderr))
    (when (zerop exit) (trim-whitespace stdout))))

(defmethod copy :around ((obj file) &key)
  "Wrap the copy method to ensure the OBJ's fields are copied."
  (let ((copy (call-next-method)))
    (with-slots (permissions modification-time) copy
      (setf permissions (permissions obj)
            modification-time (modification-time obj)))
    copy))

(defmethod from-file :before ((obj file) path)
  "Wrapper around the `from-file` method to store the file at PATH's
permissions and modification time when creating OBJ."
  (setf (modification-time obj) (file-modification-time path)
        (permissions obj) (file-permissions path)))

(defmethod to-file :after ((obj file) path)
  "Wrapper around the `to-file` method to preserve permissions and
modification time when writing OBJ to PATH."
  (when (modification-time obj)
    (shell "touch -t ~a ~a" (modification-time obj) path))
  (setf (file-permissions path) (permissions obj)))
