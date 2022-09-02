(defpackage :software-evolution-library/test/scalability
  (:nicknames :sel/test/scale :sel/test/scale)
  (:use :gt/full
   :software-evolution-library
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/c
   :software-evolution-library/software/c-project
   :software-evolution-library/software/c-cpp-project)
  (:export ::s1 :s1e :s1e2  :s2 :s3))
(in-package :software-evolution-library/test/scalability)

(defmacro def-scl (name (&rest args) &body body)
  "Defines a scalability test"
  (with-gensyms (o s sw)
    `(defun ,name (,@args)
       (macrolet ((emit (fmt &rest fmt-args) (list* 'format ',o fmt fmt-args)))
         (let* ((,s (with-output-to-string (,o)
                      ,@body))
                (,sw (from-string (make-instance 'c) (coerce ,s 'base-string))))
           (genome ,sw)
           ,sw)))))

;;; Run each test a few times to get JIT compiled methods out of the way

(def-scl s1 (n)
  (iter (for i from 1 to n)
        (emit "int x~x;~%" i)))

;;; Error cases.  These have showed a scalability problem in parsing
;;; large programs with error nodes.

;;; Try with n = 100000 to see the quadratic scaling

(def-scl s1e (n)
  (emit "#ifdef X")
  (iter (for i from 1 to n)
        (emit "int x~x;~%" i)))

(def-scl s1e2 (n)
  (emit "extern \"c\" {~%")
  (iter (for i from 1 to n)
        (emit "int x~x;~%" i)))

;;; Many function definitions

(def-scl s2 (n)
  (iter (for i from 1 to n)
        (emit "int f~x(int y~x) { if (y~x > 0) return 1; else return 2; }~%" i i i)))

;;; Many local variable declarations

(def-scl s3 (n)
  (emit "void f() {~%")
  (iter (for i from 1 to n)
        (emit "    int x~x;~%" i))
  (emit "}~%"))
