;;;; directory.lisp --- Directory functional trees.
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
  (:import-from :software-evolution-library/software/parseable
                :source-text
                :source-text=)
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

(deftest can-ensure-a-path-under-a-directory ()
  (let ((dir (make-instance 'directory-ast :name "foo")))
    (is (sel/sw/directory::ensure-path dir "bar"))
    (is (sel/sw/directory::get-path dir "bar"))
    (is (subtypep (type-of (sel/sw/directory::get-path dir "bar")) 'file-ast))))

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

(deftest can-index-into-the-genome-of-a-directory-to-get-to-file-asts ()
  (with-fixture fib-project-javascript
    (is (> (size (genome *soft*)) 100))))

(deftest can-recurse-into-file-contents ()
  (with-fixture fib-project-javascript
    (is (= 1 (count-if (op (subtypep (type-of _) 'function-ast)) (genome *soft*))))
    (is (find-if (op (subtypep (type-of _) 'function-ast)) (genome *soft*)))))

(deftest can-replace-file-in-directory ()
  "Test that the AST and evolve files remain in sync when updating."
  (with-fixture fib-project-javascript
    (let* ((new-source (fmt "~
module.exports = {
    fibonacci: function(num) {
      if ((num == 0) || (num == 1))
          return num;
       else
          return fib(num - 1) + fib(num - 2);
    }
}"))
           (new (from-string 'javascript new-source))
           (v1 (with *soft* (aget "fib.js" (evolve-files *soft*) :test #'equal)
                     new))
           (v2 (with *soft* "fib.js" new)))
      (is (equal (source-text (aget "fib.js" (evolve-files v1) :test #'equal))
                 new-source))
      (is (string= new-source
                   (string-right-trim ""
                                      (source-text (lookup v1 "fib.js")))))
      (is (equal (source-text (aget "fib.js" (evolve-files v2) :test #'equal))
                 new-source))
      (is (string= new-source
                   (string-right-trim ""
                                      (source-text (lookup v2 "fib.js")))))
      (is (not (equal? *soft* v1)))
      (is (not (equal? *soft* v2)))
      (is (equal? (genome v1) (genome v2))))))

(deftest invoke-sequence-methods-on-project-genomes ()
  ;; This test checks that we're invoking sequence methods on project
  ;; genomes and NOT going through the defmethod of mapcar/mapc on
  ;; projects that invokes the function over the EVOLVE-FILEs of the
  ;; object.  Hence the use of mapc/mapcar with result in the below
  ;; instead of just using reduce.
  (with-fixture fib-project-typescript
    (is (equal? (@ *soft* '(0 0 1)) (@ (genome *soft*) '(0 0 1))))
    (is (equal? (let (result)
                  (mapc (op (push (serial-number _) result)) *soft*)
                  result)
                (let (result)
                  (mapc (op (push (serial-number _) result)) (genome *soft*))
                  result)))
    (is (equal? (let (result)
                  (mapcar (op (prog1 nil (push (serial-number _) result))) *soft*)
                  result)
                (let (result)
                  (mapcar (op (prog1 nil (push (serial-number _) result))) (genome *soft*))
                  result)))
    (is (equal? (convert 'list *soft*) (convert 'list (genome *soft*))))))

(deftest collect-all-string-asts-from-a-project ()
  (with-fixture fib-project-javascript
    (let ((all-strings (reduce (lambda (acc node)
                                 (if (typep node 'string-ast)
                                     (cons node acc)
                                     acc))
                               *soft*)))
      (is all-strings)
      (is (every (op (typep _ 'string-ast)) all-strings)))))
