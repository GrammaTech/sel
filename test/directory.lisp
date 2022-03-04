;;;; directory.lisp --- Javascript project.
(defpackage :software-evolution-library/test/directory
  (:nicknames :sel/test/directory)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/directory)
  (:export :test-directory))
(in-package :software-evolution-library/test/directory)
(in-readtable :curry-compose-reader-macros)
(defsuite test-directory "Directory functional trees."
  (and (javascript-tree-sitter-available-p)
       (typescript-tree-sitter-available-p)
       (find-class 'directory-project nil)))

(defixture fib-project-javascript
  (:setup
   (setf *soft*
         (from-file (make-instance 'directory-project)
                    (asdf:system-relative-pathname
                     "software-evolution-library"
                     "test/etc/javascript/fib-project.js"))))
  (:teardown
   (setf *soft* nil)))

(defixture fib-project-typescript
    (:setup
     (setf *soft*
           (from-file (make-instance 'directory-project)
                      (asdf:system-relative-pathname
                       "software-evolution-library"
                       "test/etc/javascript/fib-project.ts"))))
  (:teardown
   (setf *soft* nil)))

(deftest can-load-a-javascript-directory ()
  (with-fixture fib-project-javascript
    (is (subtypep (type-of *soft*) 'directory-project))
    (is (> (size *soft*) 100))))

(deftest can-load-a-typescript-directory ()
  (with-fixture fib-project-typescript
    (is (subtypep (type-of *soft*) 'directory-project))
    (is (> (size *soft*) 100))))

(deftest can-find-a-javascript-function-in-a-directory-w-mapcar ()
  (with-fixture fib-project-javascript
    (let (function)
      (mapcar (op (when (subtypep (type-of _1) 'function-ast) (setf function _1))) *soft*)
      (is function))))

(deftest can-find-a-typescript-function-in-a-directory-w-mapcar ()
  (with-fixture fib-project-typescript
    (let (function)
      (mapcar (op (when (subtypep (type-of _1) 'function-ast) (setf function _1))) *soft*)
      (is function))))
