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
   :software-evolution-library/software/directory
   :software-evolution-library/software/javascript-project
   :software-evolution-library/software/cpp-project
   :software-evolution-library/software/json)
  (:import-from :software-evolution-library/software/directory
                :*verify-project-in-sync*)
  (:import-from :software-evolution-library/software/parseable
                :ast-path
                :ast-source-ranges
                :collect-if
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
         (from-file (make-instance 'javascript-project)
                    (asdf:system-relative-pathname
                     "software-evolution-library"
                     "test/etc/javascript/fib-project.js"))))
  (:teardown
   (setf *soft* nil)))

(defixture fib-project-typescript
    (:setup
     (setf *soft*
           (from-file (make-instance 'typescript-project)
                      (asdf:system-relative-pathname
                       "software-evolution-library"
                       "test/etc/javascript/fib-project.ts"))))
  (:teardown
   (setf *soft* nil)))

(def +alert.js+
  "module.exports = {
  alert: function(arg) {
    window.alert(arg);
  }
}")

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

(deftest mapc-returns-project ()
  (with-fixture fib-project-javascript
    (is (typep (mapc #'identity *soft*) 'directory-project))))

(deftest mapcar-returns-project ()
  (with-fixture fib-project-javascript
    (is (typep (mapcar #'identity *soft*) 'directory-project))))

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

(defun can-insert-file-helper (old-project path new-file)
  (is (stringp path))
  ;; The file doesn't already exist.
  (is (null (evolve-files-ref old-project path)))
  (is (null (lookup old-project path)))
  (let ((new-project (with old-project path new-file)))
    ;; Make sure the old project hasn't been mutated.
    (is (null (evolve-files-ref old-project path)))
    (is (null (lookup old-project path)))
    ;; The evolve files have been updated.
    (is (eql new-file (evolve-files-ref new-project path)))
    ;; A file AST has been created.
    (let ((file-ast (lookup new-project path)))
      (is (eql file-ast
               (lookup new-project (ast-path new-project file-ast))))
      (is (typep file-ast 'file-ast))
      (is (eql (genome new-file)
               (only-elt (children file-ast)))))
    (with-temporary-directory (:pathname d)
      (phenome new-project :bin d)
      (is (file-exists-p (path-join d path))))
    new-project))

(deftest can-insert-file/same-level ()
  (with-fixture fib-project-javascript
    (let* ((new-js (from-string 'javascript +alert.js+)))
      (can-insert-file-helper *soft* "alert.js" new-js))))

(deftest can-insert-file/subdirectory ()
  (with-fixture fib-project-javascript
    (let* ((new-js (from-string 'javascript +alert.js+))
           (random (princ-to-string (random-in-range 100000 999999))))
      (can-insert-file-helper *soft* (string+ random "/alert.js") new-js))))

(deftest can-replace-file-in-directory ()
  "Test that the AST and evolve files remain in sync when updating."
  (with-fixture fib-project-javascript
    (let* ((*verify-project-in-sync* t)
           (new-source (fmt "~
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

(deftest test-can-remove-file-by-name ()
  "Removing a file by name should remove both the file-ast and evolve files entry."
  (let* ((*verify-project-in-sync* t)
         (sel-dir (asdf:system-relative-pathname :software-evolution-library nil))
         (project-dir (path-join sel-dir #p"test/etc/cpp-symbol-table-project2"))
         (project (is (from-file (make 'cpp-project) project-dir))))
    (is (assoc "my_class.h" (evolve-files project) :test #'equal))
    (is (lookup project "my_class.h"))
    (is (lookup (genome project) "my_class.h"))
    (is (length= 3 (evolve-files project)))
    (is (length= 3 (collect-if (of-type 'file-ast) project)))
    (let ((new-project (less project "my_class.h")))
      (is (null (assoc "my_class.h" (evolve-files new-project) :test #'equal)))
      (is (null (lookup new-project "my_class.h")))
      (is (null (lookup (genome new-project) "my_class.h")))
      (is (length= 2 (evolve-files new-project)))
      (is (length= 2 (collect-if (of-type 'file-ast) new-project))))))

(deftest test-can-remove-file-ast ()
  "Removing a file-ast should remove the corresponding evolve files entry."
  (let* ((*verify-project-in-sync* t)
         (sel-dir (asdf:system-relative-pathname :software-evolution-library nil))
         (project-dir (path-join sel-dir #p"test/etc/cpp-symbol-table-project2"))
         (project (is (from-file (make 'cpp-project) project-dir)))
         (file-ast
          (is (lookup project "my_class.h"))))
    (is (assoc "my_class.h" (evolve-files project) :test #'equal))
    (is (typep file-ast 'file-ast))
    (is (lookup (genome project) "my_class.h"))
    (is (length= 3 (evolve-files project)))
    (is (length= 3 (collect-if (of-type 'file-ast) project)))
    (let ((new-project (less project file-ast)))
      (is (null (assoc "my_class.h" (evolve-files new-project) :test #'equal)))
      (is (null (lookup new-project "my_class.h")))
      (is (null (lookup (genome new-project) "my_class.h")))
      (is (length= 2 (evolve-files new-project)))
      (is (length= 2 (collect-if (of-type 'file-ast) new-project))))))

(defmacro define-sync-test (name (&rest args) &body body)
  `(deftest ,name ,args
     (labels ((file-root (file-ast)
                (only-elt (contents file-ast))))
       (let* ((*verify-project-in-sync* t)
              (sel-dir (asdf:system-relative-pathname :software-evolution-library nil))
              (project-dir (path-join sel-dir #p"test/etc/cpp-symbol-table-project2"))
              (project (is (from-file (make 'cpp-project) project-dir)))
              (hpp-file (lookup project "my_class.h"))
              (hpp-ast (find-if (of-type 'cpp-field-declaration) hpp-file)))
         (declare (ignorable hpp-ast))
         (flet ((test-sync (new-project new-file)
                  (is (not (eql new-project project)))
                  (is (not (eql hpp-file new-file)))
                  (is (rassoc (file-root new-file)
                              (evolve-files new-project)
                              :key #'genome))))
           ,@body)))))

(define-sync-test test-update-evolve-files-with-genome/with ()
  (let* ((new-ast (tree-copy hpp-ast))
         (id (find-if (of-type 'cpp-field-identifier) new-ast))
         (new-ast
          (with new-ast
                id
                (copy id :text "do_something_else")))
         (new-project
          (with project hpp-ast new-ast))
         (new-file (lookup new-project "my_class.h")))
    (test-sync new-project new-file)))

(define-sync-test test-update-evolve-files-with-genome/less ()
  (let* ((new-project (less project hpp-ast))
         (new-file (lookup new-project "my_class.h")))
    (test-sync new-project new-file)))

(define-sync-test test-update-evolve-files-with-genome/insert ()
  ;; Insert
  (let* ((new-project
          (insert project
                  (ast-path project
                            (first (children
                                    (is (find-enclosing
                                         'cpp-translation-unit
                                         project hpp-ast)))))
                  (make 'cpp-identifier :text "new-id")))
         (new-file (lookup new-project "my_class.h")))
    (test-sync new-project new-file)))

(define-sync-test test-update-evolve-files-with-genome/splice ()
  ;; Splice
  (let* ((new-project
          (splice project
                  (ast-path project
                            (first (children
                                    (is (find-enclosing
                                         'cpp-translation-unit
                                         project hpp-ast)))))
                  (list
                   (make 'cpp-identifier :text "new-id"))))
         (new-file (lookup new-project "my_class.h")))
    (test-sync new-project new-file)))

(define-sync-test test-update-evolve-files-with-genome/mapcar ()
  (let* ((new-project
          (mapcar
           (lambda (ast)
             (and (typep ast 'identifier-ast)
                  (source-text= ast "do_something")
                  (copy ast :text "do_something_else")))
           project))
         (new-file (lookup new-project "my_class.h")))
    (test-sync new-project new-file)))

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

(deftest test-project-ast-source-ranges ()
  "The results of `ast-source-ranges' on a `file-ast' should match its contents.

This is a regression for the bug that, since `file-ast' does not
itself inherit from `indentation', calling `ast-source-ranges' on a
`file-ast' would give very different results than calling it on the
enclosed `root-ast'."
  (with-fixture fib-project-javascript
    (is *soft*)
    (let ((file-asts (collect-if (of-type 'file-ast) *soft*)))
      (is file-asts)
      (dolist (file-ast file-asts)
        (is (equal? (ast-source-ranges file-ast)
                    (ast-source-ranges (only-elt (children file-ast)))))))))

(deftest test-project-ast-source-text ()
  "The results of `source-text' on a `file-ast' should match its contents.
Even though `file-ast' does not itself inherit from `indentation'."
  (with-fixture fib-project-javascript
    (is *soft*)
    (let ((file-asts (collect-if (of-type 'file-ast) *soft*)))
      (is file-asts)
      (dolist (file-ast file-asts)
        (is (equal (source-text file-ast)
                   (source-text (only-elt (children file-ast)))))))))
