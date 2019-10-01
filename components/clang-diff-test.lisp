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
  (:import-from :software-evolution-library/software/new-clang-debug
                :dump-ast-val-to-list)
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
               :documentation "eval STRING as lisp code")
              (("syn-ctx") :type boolean :optional t
               :documentation "Compare the AST-SYN-CTX values")
              (("types") :type boolean :optional t
               :documentation "Compare the AST-TYPES values")
              (("unbound-funs") :type boolean :optional t
               :documentation "Compare the AST-UNBOUND-FUNS values")
              (("unbound-vals") :type boolean :optional t
               :documentation "Compare the AST-UNBOUND-VALS values")
              (("errors") :type boolean :optional t
               :documentation
               "When true, new-clang errors are 'interesting'")))))

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

(defgeneric diff-result-equal (s1 s2 &key &allow-other-keys))

(defmethod diff-result-equal :around (s1 s2 &rest args)
  (declare (ignorable args))
  (let ((v (call-next-method)))
    ;; Push up to five contexts onto this special variable
    ;; so differences can be located
    (when (and (null v) (< (length *diff-result-equal-diff*) 5))
      (push (list s1 s2) *diff-result-equal-diff*))
    v))

;;; ugh
(defmethod diff-result-equal ((s1 (eql nil)) (s2 (eql nil))
                              &key &allow-other-keys) t)

(defmethod diff-result-equal ((s1 cons) (s2 cons) &rest args
                              &key loose &allow-other-keys)
  (let ((l1 (length s1))
        (l2 (length s2)))
    (or (eql (car s1) :combined)
        (eql (car s2) :combined)
        (and (consp (car s2))
             (eql (caar s2) :combined)
             (= l2 1))
        (and (eql (car s2) :constantexpr)
             (not (eql (car s1) :constantexpr))
             (= (length (caddr s2)) 1)
             (diff-result-equal s1 (caaddr s2)))
        (and (or loose (= l1 l2))
             (every #'(lambda (a b) (apply #'diff-result-equal a b args))
                    s1 s2)))))

(defmethod diff-result-equal (s1 s2 &key &allow-other-keys)
  (equal s1 s2))

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

(defun remove-var-children (ast)
  (remove-ast-nodes-if
   ast
   (lambda (n anc)
     (declare (ignore n))
     (and anc (eql (ast-class (car anc)) :var)))))

(defun remove-parmvar-children (ast)
  (remove-ast-nodes-if
   ast
   (lambda (n anc)
     (declare (ignore n))
     (and anc (eql (ast-class (car anc)) :parmvar)))))

(defun remove-typedef-body (ast)
  (remove-ast-nodes-if
   ast
   (lambda (n anc)
     (when (and anc (eql (ast-class (car anc)) :typedef))
       (setf (ast-children n) nil)
       t))))

(defun remove-typealias-body (ast)
  (remove-ast-nodes-if
   ast
   (lambda (n anc)
     (when (and anc (eql (ast-class (car anc)) :typealias))
       (setf (ast-children n) nil)
       t))))

(defun flatten-nested-fields (sw)
  (map-ast (ast-root sw) #'flatten-nested-fields-in-children))

(defun flatten-nested-fields-in-children (ast)
  (let ((c (ast-children ast)))
    (iter (while c)
          (let ((child (car c)))
            (when (and (ast-p child)
                       (eql (ast-class child) :field))
              (let ((cc (ast-children child))
                    (pred nil))
                (iter (while cc)
                      (when (and (ast-p (car cc))
                                 (eql (ast-class (car cc)) :field))
                        (return))
                      (setf pred cc)
                      (pop cc))
                (when (and cc pred)
                  (setf (cdr pred) nil)
                  (setf (cdr c) (append cc (cdr c)))))))
          (pop c))))

;;; Hack to remove stuff new clang doesn't have
(defun cleanup-clang (c)
  (flatten-nested-fields c)
  ;; (remove-var-integer-literals c)
  (remove-var-children c)
  (remove-parmvar-children c)
  (remove-typedef-body c))

;;; Hack to remove stuff clang doesn't have
(defun cleanup-new-clang (nc)
  (remove-empty-decls nc)
  (remove-macro-args nc)
  (remove-constant-case-exprs nc)
  (remove-builtintype nc)
  ;; (remove-var-integer-literals nc)
  (remove-var-children nc)
  (remove-parmvar-children nc)
  (remove-typedef-body nc)
  (remove-typealias-body nc))

;;; Testing function

(defun check-attr (c nc fn &key loose)
  (cleanup-clang c)
  (cleanup-new-clang nc)
  (let ((*diff-result-equal-diff* nil))
    (let ((cl (dump-ast-val-to-list c fn))
          (ncl (dump-ast-val-to-list nc fn)))
      (if (diff-result-equal cl ncl :loose loose)
          t
          (values nil *diff-result-equal-diff*)))))

;;; Main function

(define-command clang-diff-test
    (source &spec +command-line-options+
            &aux (fn #'ast-class))
  "Differential testing of clang vs. new-clang"
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable eval load))
  (when help (show-help-for-clang-diff-test))
  (when syn-ctx
    (setf fn #'ast-syn-ctx))
  (when types
    (setf fn #'(lambda (a) (length (ast-types a)))))
  (when unbound-funs
    (setf fn #'(lambda (a) (length (ast-unbound-funs a)))))
  (when unbound-vals
    (setf fn #'(lambda (a) (length (ast-unbound-vals a)))))
  (let ((c (make-clang source))
        (nc (make-new-clang source)))
    (handler-case (ast-root c)
      ;; We ignore errors from old clang
      (error (e) (exit-command clang-diff-test 98 (error e))))
    (handler-case (ast-root nc)
      ;; The ERRORS flag indicates we are looking for cases
      ;; where new-clang fails with an error.
      (error (e)
        (if errors
            (exit-command clang-diff-test 0 nil)
            (exit-command clang-diff-test 99 (error e)))))
    (handler-case
        (multiple-value-bind (success? diagnostics)
            (check-attr c nc fn)
          (declare (ignore diagnostics))
          ;; Return 0 on difference because that's what creduce
          ;; wants to see for "interesting"
          (exit-command clang-diff-test (if success? 1 0) nil))
      (error (e) (exit-command clang-diff-test 99 (error e))))))
