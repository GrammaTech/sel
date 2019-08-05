;;; clang-diff-test.lisp -- differential testing for clang and new clang

(defpackage :software-evolution-library/components/clang-diff-test
  (:nicknames :sel/components/clang-diff-test
              :sel/cp/clang-diff-test)
  (:use :common-lisp
        :alexandria
        :iterate
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/command-line
        :software-evolution-library/software/ast
        :software-evolution-library/software/parseable
        :software-evolution-library/software/source
        :software-evolution-library/software/clang
        :software-evolution-library/software/new-clang)
  (:import-from :uiop :nest truenamize)
  (:export :clang-diff-test))
(in-package :software-evolution-library/components/clang-diff-test)

;;; Command line
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +command-line-options+
    (append `((("help" #\h #\?) :type boolean :optional t
               :documentation "display help output")
              (("load" #\l) :type string
               :action #'handle-load
               :documentation "load FILE as lisp code")
              (("eval" #\e) :type string
               :action #'handle-eval
               :documentation "eval STRING as lisp code")))))

(defun make-clang (file &key)
  (let ((obj1 (make-instance 'clang)))
    (from-file obj1 file)
    obj1))

(defun make-new-clang (file)
  (let ((obj1 (make-instance 'new-clang
                :compiler sel/sw/new-clang::*clang-binary*)))
    (from-file obj1 file)
    obj1))

;;; Utility functions

(declaim (special *diff-result-equal-diff*))

(defgeneric diff-result-equal (s1 s2))

(defmethod diff-result-equal :around (s1 s2)
  (let ((v (call-next-method)))
    ;; Push up to five contexts onto this special variable
    ;; so differences can be located
    (when (and (null v) (< (length *diff-result-equal-diff*) 5))
      (push (list s1 s2) *diff-result-equal-diff*))
    v))

;;; ugh
(defmethod diff-result-equal ((s1 (eql :nil)) (s2 (eql nil))) t)

(defmethod diff-result-equal ((s1 cons) (s2 cons))
  (or (eql (car s1) :combined)
      (eql (car s2) :combined)
      (and (consp (car s2))
           (eql (caar s2) :combined)
           (= (length s2) 1))
      (and (eql (car s2) :constantexpr)
           (not (eql (car s1) :constantexpr))
           (= (length (caddr s2)) 1)
           (diff-result-equal s1 (caaddr s2)))
      (and (= (length s1) (length s2))
           (every #'diff-result-equal s1 s2))))

(defmethod diff-result-equal (s1 s2)
  (equal s1 s2))

(defgeneric dump-ast (ast print-fn))

(defmethod dump-ast ((ast ast) print-fn)
  (labels ((%r (a d)
             ;; (dotimes (i d) (format t " "))
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
  (flet ((%print-class
             (a d)
           (let ((class (ast-class a)))
             (dotimes (i d) (format s " "))
             (format s "~a~%" class))))
    (dump-ast ast #'%print-class)))

(defgeneric dump-ast-val (ast val-fn &optional s)
  (:method (ast val-fn &optional (s *standard-output*))
    (flet ((%print (a d)
             (let ((class (ast-class a)))
               (dotimes (i d) (format s " "))
               (format s "~a: ~a~%"
                       class
                       (funcall val-fn a)))))
      (dump-ast ast #'%print)))
  (:method :around ((sw new-clang) val-fn &optional s)
           (declare (ignorable s))
           (let ((*soft* sw))
             (ast-root sw)
             (call-next-method))))

(defgeneric dump-ast-val-p (ast val-fn &optional s)
  (:method (ast val-fn &optional (s *standard-output*))
    (flet ((%print (a p d)
             (let ((class (ast-class a)))
               (dotimes (i d) (format s " "))
               (format s "~a: ~a~%"
                       class
                       (funcall val-fn a p)))))
      (dump-ast-with-parent ast #'%print)))
  (:method :around ((sw new-clang) val-fn &optional s)
           (declare (ignorable s))
           (let ((*soft* sw))
             (ast-root sw)
             (call-next-method))))

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

(defgeneric remove-ast-nodes-if (ast pred &optional ancestors)
  (:documentation
   "Remove nodes satisfying PRED, folding their children into the
list of children of their parent.  Cannot remove the root."))

(defmethod remove-ast-nodes-if ((sw parseable) pred &optional ancestors)
  (remove-ast-nodes-if (ast-root sw) pred ancestors))

(defmethod remove-ast-nodes-if ((ast ast) pred &optional ancestors)
  (let* ((ancestors (cons ast ancestors))
         (new-children
          (iter (for c in (ast-children ast))
                (if (ast-p c)
                    (progn
                      (remove-ast-nodes-if c pred ancestors)
                      (appending
                       (if (funcall pred c ancestors)
                           (ast-children c)
                           (list c))))
                    (appending (list c))))))
    (setf (ast-children ast) new-children)))

(defun is-constant-case-expr (n ancestors)
  (and ancestors
       (eql (ast-class (car ancestors)) :CaseStmt)
       (eql (ast-class n) :ConstantExpr)))

(defun parent-is-var (n ancestors)
  (declare (ignore n))
  (and ancestors
       (member (ast-class (car ancestors)) '(:var))))

(defun remove-constant-case-exprs (ast)
  "Hack to make new clang asts more compatible with old clang asts"
  (remove-ast-nodes-if ast #'is-constant-case-expr))

(defun remove-empty-decls (ast)
  (remove-ast-nodes-if ast (lambda (s p)
                             (declare (ignore p))
                             (eql (ast-class s) :EmptyDecl))))

(defun remove-builtintype (ast)
  (remove-ast-nodes-if ast (lambda (s p)
                             (declare (ignore p))
                             (eql (ast-class s) :BuiltinType))))

(defun remove-macro-args (ast)
  (remove-ast-nodes-if
   ast
   (lambda (n anc) (declare (ignore n))
           (and anc (eql (ast-class (car anc)) :macroexpansion)))))

(defun remove-var-integer-literals (ast)
  (remove-ast-nodes-if
   ast
   (lambda (n anc)
     (and anc (eql (ast-class (car anc)) :var)
          (eql (ast-class n) :IntegerLiteral)))))

(defun remove-typedef-body (ast)
  (remove-ast-nodes-if
   ast
   (lambda (n anc)
     (when (and anc (eql (ast-class (car anc)) :typedef))
       (setf (ast-children n) nil)
       t))))

;;; Hack to remove stuff new clang doesn't have
(defun cleanup-clang (c)
  (remove-var-integer-literals c)
  (remove-typedef-body c))

;;; Hack to remove stuff clang doesn't have
(defun cleanup-new-clang (nc)
  (remove-empty-decls nc)
  (remove-macro-args nc)
  (remove-constant-case-exprs nc)
  (remove-builtintype nc)
  (remove-var-integer-literals nc)
  (remove-typedef-body nc))

;;; Testing function

(defun check-attr (c nc fn)
  (cleanup-clang c)
  (cleanup-new-clang nc)
  (let ((*diff-result-equal-diff* nil))
    (let ((cl (dump-ast-val-to-list c fn))
          (ncl (dump-ast-val-to-list nc fn)))
      (if (diff-result-equal cl ncl)
          t
          (values nil *diff-result-equal-diff*)))))

;;; Main function

(define-command clang-diff-test
    (source &spec +command-line-options+
            &aux (fn 'ast-syn-ctx))
  "Differential testing of clang vs. new-clang"
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (when help (show-help-for-clang-diff-test))
  ;; Rewrite this to use new-clang class
  (let ((c (make-clang source))
        (nc (make-new-clang source)))
    (handler-case
        (multiple-value-bind (success? diagnostics)
            (check-attr c nc fn)
          (declare (ignore diagnostics))
          ;; Return 0 on difference because that's what creduce
          ;; wants to see for "interesting"
          (exit-command clang-diff-test (if success? 1 0) nil))
      (error (e) (exit-command clang-diff-test 99 (error e))))))
