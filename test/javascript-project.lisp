;;;; javascript-project.lisp --- Javascript project.
(defpackage :software-evolution-library/test/javascript-project
  (:nicknames :sel/test/javascript-project)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/javascript
   :software-evolution-library/software/json
   :software-evolution-library/software/project
   :software-evolution-library/software/javascript-project
   :software-evolution-library/components/test-suite)
  (:export :test-javascript-project))
(in-package :software-evolution-library/test/javascript-project)
(in-readtable :curry-compose-reader-macros)
(defsuite test-javascript-project "Javascript representation."
  (javascript-tree-sitter-available-p))

(defixture fib-project-javascript
  (:setup
   (setf *soft*
         (from-file (make-instance 'javascript-project
                      :component-class 'sel/sw/ts:javascript)
                    (javascript-dir #P"fib-project/"))))
  (:teardown
   (setf *soft* nil)))

(defixture fib-project-typescript
    (:setup
     (setf *soft*
           (from-file (make-instance 'typescript-project
                                     :component-class 'sel/sw/ts:typescript)
                      (javascript-dir #P"fib-project.ts/"))))
  (:teardown
   (setf *soft* nil)))

(deftest (can-parse-a-javascript-project :long-running) ()
  (with-fixture fib-project-javascript
    (is (equal 2 (length (evolve-files *soft*))))
    (is (not (zerop (size *soft*))))))

(deftest (can-parse-a-typescript-project :long-running) ()
  (with-fixture fib-project-typescript
    (is (typep *soft* 'typescript-project))
    (is (equal 2 (length (evolve-files *soft*))))
    (is (not (zerop (size *soft*))))))

(deftest (can-compile-a-typescript-project :long-running) ()
  (with-fixture fib-project-typescript
    (let ((phenome (ensure-directory-pathname (phenome *soft*))))
      (is (file-exists-p (path-join phenome "app.js")))
      (is (file-exists-p (path-join phenome "fib.js"))))))
