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

  (:import-from :software-evolution-library/command-line
                :guess-language)
  (:import-from :software-evolution-library/software/javascript-project
                :package-json-scripts
                :javascript-project-package-json
                :guess-build-script)
  (:import-from :software-evolution-library/software/tree-sitter
                :json)
  (:export :test-javascript-project))
(in-package :software-evolution-library/test/javascript-project)
(in-readtable :curry-compose-reader-macros)
(defsuite test-javascript-project "Javascript representation."
  (and (javascript-tree-sitter-available-p)
       (typescript-tree-sitter-available-p)))

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

(deftest can-identity-a-javascript-project ()
  (is (eql 'javascript-project
           (guess-language (javascript-dir #p"fib-project/")))))

(deftest can-identity-a-javascript-project-with-typescript-deps ()
  "Can identify a JS project as such (even if it has TS deps)."
  (is (eql 'javascript-project
           (guess-language (javascript-dir #p"fib-project.js/")))))

(deftest can-identity-a-typescript-project ()
  (is (eql 'typescript-project
           (guess-language (javascript-dir #p"fib-project.ts/")))))

(deftest can-process-package-json ()
  (with-fixture fib-project-typescript
    (let ((package.json (javascript-project-package-json *soft*)))
      (is (typep package.json 'json))
      (let ((scripts (package-json-scripts (genome package.json))))
        (is (equal '(("build" . "tsc -b")
                     ("lint" . "eslint src --ext ts"))
                   scripts)
            (is (equal "tsc -b"
                       (guess-build-script scripts))))))))

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
