(defpackage :software-evolution-library/ast-diff/merge
  (:nicknames :sel/ast-diff/merge :software-evolution-library/ast-diff/merge)
  (:use :common-lisp
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/ast
        :software-evolution-library/software/simple
        :software-evolution-library/software/project)
  (:export :save-to))
(in-package :software-evolution-library/ast-diff/merge)

(defgeneric save-to (soft out-dir sub))

(defmethod save-to ((soft project) out-dir sub)
  (let ((dest (make-pathname :directory (append out-dir (list sub)))))
    (unless (probe-file dest)
      (to-file (copy soft) dest))))
