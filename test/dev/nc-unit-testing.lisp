(in-package :sel/test)

(import 'sel/sw/new-clang-debug::dump-ast-val)

;;; Temporary utility code for unit testing of new-clang

(defun name-of-test-description (d)
  (stefil::name-of
   (stefil::test-of
    (car (stefil::test-context-backtrace-of d)))))

(defun failing-tests ()
  "Produce a list of names of failing tests"
  (map 'list #'name-of-test-description
       (stefil::failure-descriptions-of
        stefil:*last-test-result*)))

;;; Typical use:
;;;
;;;  (in-package :sel/test)
;;;  (without-debugging (let ((*new-clang?* t)) (test)))
;;;  (failing-tests) ==> list of the names of test that failed

(defun dump-types (obj)
  (let ((types (types obj)))
    (maphash
     (lambda (k v)
       (format t "~a => ~a~%" k v))
     types)))

;;; *A* and *OA* are the new and the 'original' clang objects for a file
(declaim (special *a* *oa*))

(defun l (file)
  (let ((*new-clang?* t))
    ;; MAKE-CLANG is defined in test.lisp
    (setf *a* (from-file (make-clang) file)))
  (let ((*new-clang?* nil))
    (setf *oa* (from-file (make-clang) file)))
  (values *a* *oa*))

(when (find-package :sel/sw/new-clang-debug)
  (import (intern "DUMP-AST-VAL" :sel/sw/new-clang-debug)))

(defun l2 (file)
  (l file)
  (dump-ast-val *oa* #'ast-types)
  (dump-ast-val *a* #'ast-types)
  (values))

(defun l3 (file fn)
  (l file)
  (dump-ast-val *oa* fn)
  (dump-ast-val *a* fn)
  (values))

(defun on-types (sw fn)
  (lambda (a)
    (mapcar (lambda (id) (if-let ((tp (find-type sw id))) (funcall fn tp)))
            (get-ast-types sw a))))

(defun dump-type-fn (sw fn)
  (let ((sel/sw/new-clang::*soft* sw))
    (dump-ast-val (ast-root sw) (on-types sw fn))))

(defgeneric td (x)
  (:documentation "Dump contents of a type in readable form"))

(defmethod td ((x sel/sw/clang:clang-type))
  (list (type-array x)
        (type-decl x)
        (type-hash x)
        (type-i-file x)
        (type-pointer x)
        (type-const x)
        (type-volatile x)
        (type-restrict x)
        (type-storage-class x)
        (type-reqs x)
        (type-name x)))

(defgeneric dump-when-fn (ast fn))
(defmethod dump-when-fn ((sw parseable) fn)
  (dump-when-fn (ast-root sw) fn))
(defmethod dump-when-fn ((ast ast) fn)
  "Traverse an AST, printing the ast-class names.  When FN
   evaluated on an AST node returns a non-nil value, print that value."
  (map-ast ast
           (lambda (a)
             (let ((x (funcall fn a)))
               (when x (format t "~s~%" x)))))
  (values))

(defun dump-when-type-fn (sw fn)
  "Traverse an AST, printing (along with the ast class names) the
values produced by applying FN to the list of values returned
by GET-AST-TYPES"
  (dump-when-fn sw
                (lambda (a)
                  (remove nil
                          (mapcar
                           (lambda (i)
                             (let ((tp (find-type sw i)))
                               (when tp
                                 (let ((val (funcall fn tp)))
                                   (and (not (equal val "")) val)))))
                           (get-ast-types sw a)))))
  (values))

(defmacro nc (&body body)
  (let ((v1 (gensym)) (v2 (gensym)))
    `(let ((,v1 *new-clang?*)
           (,v2 *make-statement-fn*))
       (unwind-protect
            (progn
              (setf *new-clang?* t
                    *make-statement-fn* #'make-statement-new-clang)
              (let ()
                ,@body))
         (setf *new-clang?* ,v1 *make-statement-fn* ,v2)))))

(defmacro wdnc (&body body)
  `(without-debugging (nc ,@body)))

;;; Differential testing

(defun dt (files fn &key loose)
  (loop for d in files
     do (handler-case
            (progn (l d)
                   (multiple-value-bind (good? diff)
                       (sel/cp/clang-diff-test::check-attr *oa* *a* fn :loose loose)
                     (declare (ignore diff))
                     (format t "~d: ~a~%" d good?)))
          (error (e) (format t "~d: ~a" d e)))))
