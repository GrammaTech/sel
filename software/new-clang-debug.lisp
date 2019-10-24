;;; new-clang-debug.lisp -- debugging code for new-clang.lisp
;;;
;;; This is debugging code and is not intended to be part of
;;; the main build.
;;;
(defpackage :software-evolution-library/software/new-clang-debug
  (:nicknames :sel/sw/new-clang-debug)
  (:use :common-lisp
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/ast
        :software-evolution-library/software/source
        :software-evolution-library/software/parseable
        :software-evolution-library/software/clang
        :software-evolution-library/software/new-clang)
  ;; (:import-from :software-evolution-library/software/new-clang :*soft*)
  (:shadowing-import-from :software-evolution-library/software/project
                          :project :evolve-files)
  (:export :dump-ast :dump-ast-with-parent
           :dump-ast-classes :dump-ast-val
           :dump-ast-val-p :dump-ast-to-list
           :dump-ast-val-to-list))

(in-package :software-evolution-library/software/new-clang-debug)

(defgeneric dump-ast (ast print-fn))

(defmethod dump-ast ((ast ast) print-fn)
  (labels ((%r (a d)
             (funcall print-fn a d)
             (let ((d (1+ d)))
               (dolist (c (ast-children a))
                 (when (ast-p c) (%r c d))))))
    (%r ast 0)))

(defmethod dump-ast ((sw parseable) print-fn)
  (dump-ast (ast-root sw) print-fn))

(defgeneric dump-ast-with-parent (ast print-fn))

(defmethod dump-ast-with-parent ((ast ast) print-fn)
  (labels ((%r (a d p)
             (dotimes (i d) (format t " "))
             (funcall print-fn a p d)
             (let ((d (1+ d)))
               (dolist (c (ast-children a))
                 (when (ast-p c) (%r c d a))))))
    (%r ast 0 nil)))

(defmethod dump-ast-with-parent ((sw parseable) print-fn)
  (dump-ast-with-parent (ast-root sw) print-fn))

(defun dump-ast-classes (ast &optional (s *standard-output*))
  (flet ((%print-class (a d)
           (let ((class (ast-class a)))
             (dotimes (i d) (format s " "))
             (format s "~a~%" class))))
    (dump-ast ast #'%print-class)))

(defgeneric dump-ast-val (ast val-fn &optional s)
  (:method ((sw project) val-fn &optional (s *standard-output*))
    (loop for (path . obj) in (evolve-files sw)
       do (dump-ast-val obj val-fn s)))
  (:method (ast val-fn &optional (s *standard-output*))
    (flet ((%print (a d)
             (let ((class (ast-class a)))
               (dotimes (i d) (format s " "))
               (format s "~a: ~s~%"
                       class
                       (funcall val-fn a)))))
      (dump-ast ast #'%print)))
  (:method :around ((sw clang-base) val-fn &optional s)
           (declare (ignorable s))
           (ast-root sw)
           (call-next-method)))

(defgeneric dump-ast-val-p (ast val-fn &optional s)
  (:method ((sw project) val-fn &optional (s *standard-output*))
    (loop for (path . obj) in (evolve-files sw)
       do (dump-ast-val-p obj val-fn s)))
  (:method (ast val-fn &optional (s *standard-output*))
    (flet ((%print (a p d)
             (let ((class (ast-class a)))
               (dotimes (i d) (format s " "))
               (format s "~a: ~s~%"
                       class
                       (funcall val-fn a p)))))
      (dump-ast-with-parent ast #'%print)))
  (:method :around ((sw new-clang) val-fn &optional s)
           (declare (ignorable s))
           (ast-root sw)
           (call-next-method)))

(defgeneric dump-ast-to-list (ast fn)
  (:method ((ast ast) fn)
    (funcall fn ast (mapcar (lambda (a) (dump-ast-to-list a fn))
                            (remove-if-not #'ast-p (ast-children ast)))))
  (:method ((sw parseable) fn)
    (dump-ast-to-list (ast-root sw) fn)))

(defgeneric dump-ast-val-to-list (ast val-fn)
  (:method (ast val-fn)
    (dump-ast-to-list
     ast
     (lambda (a child-results)
       (let ((val (funcall val-fn a)))
         (list (ast-class a) val child-results))))))
