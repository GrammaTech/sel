;;;; indentation.lisp --- Indentation utility.
;;;;
;;;; This file tests the functions that operate on
;;;; python and python-ast objects.
(defpackage :software-evolution-library/test/indentation
  (:nicknames :sel/test/indentation)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/python
   :software-evolution-library/components/file
   :software-evolution-library/components/formatting)
  (:export :test-indentation))
(in-package :software-evolution-library/test/indentation)
(in-readtable :curry-compose-reader-macros)
(defsuite test-indentation "Indentation." (python3.8-available-p))

;;; TODO: make this indentation-python instead?


;;; Utility
(defmacro with-software-file ((filename software-var genome-var)
                              &body body)
  `(let* ((,software-var (from-file
                          (make-instance 'python)
                          (make-pathname :name ,filename
                                         :type "py"
                                         :directory +python-utility-dir+)))
          (,genome-var (genome ,software-var)))
     ,@body))

(defun is-round-trip (filename)
  "Test that FILENAME can be read into a python-ast object and
then printed out without modifying the file."
  (let ((file-string
          (file-to-string (make-pathname :name filename
                                         :type "py"
                                         :directory +python-utility-dir+))))
    (with-software-file (filename soft genome)
      (is (equalp file-string (source-text genome))))))

(defun build-move-ops (insert-at move-asts)
  (mapcar (lambda (ast)
            `(:insert
              (:stmt1 . ,insert-at)
              (:value1 . ,ast)))
          move-asts))

(defun move-asts (obj insert-at move-asts)
  "Move MOVE-ASTS to INSERT-AT in a copy of OBJ."
  (apply-mutation-ops (copy obj) (build-move-ops insert-at move-asts)))

(defun is-moveable (obj position-ast insert-asts result-file)
  "Insert INSERT-ASTS at POSITION-AST in OBJ and then compare the
result with RESULT-FILE."
  (let ((mut-obj (move-asts obj position-ast insert-asts))
        (result-string (file-to-string
                        (make-pathname :name result-file
                                       :type "py"
                                       :directory +python-utility-dir+))))
    (is (equalp result-string (source-text (genome mut-obj))))))


;;; Round-Trip Tests
(deftest indentation-round-trip-python-1 ()
  "Test that nested functions can round-trip through a python object."
  (is-round-trip "nested-global"))

(deftest indentation-round-trip-python-2 ()
  "Test that multiline strings can round-trip through a python object."
  (is-round-trip "multiline-string"))

(deftest indentation-round-trip-python-3 ()
  "Test that multiple newlines in a row are handled correctly."
  (is-round-trip "multiple-newlines"))


;;; Mutation Tests
(deftest moveable-indentation-python-1 ()
  (with-software-file ("nested-functions" soft genome)
    (is-moveable soft
                 (find-if {typep _ 'py-pass} genome)
                 (list (find-if {typep _ 'py-function-def} genome))
                 "nested-functions-mut")))
