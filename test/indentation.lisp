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
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/python
   :software-evolution-library/software/go
   :software-evolution-library/components/file
   :software-evolution-library/components/formatting)
  (:export :test-indentation))
(in-package :software-evolution-library/test/indentation)
(in-readtable :curry-compose-reader-macros)
(defsuite test-indentation "Indentation."
    (and (python-tree-sitter-available-p)
         (go-tree-sitter-available-p)
         (c-tree-sitter-available-p)))


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

(deftest indentation-parameters-regression-round-trip ()
  (is-round-trip "indented-parameters"))

;;; TODO: at some point, refactor golang round trips into a function.
(deftest indentation-round-trip-inner-whitespace-1 ()
  "inner-whitespace ASTs aren't considered children of their parent when
calculating the whitespace before it."
  (let ((source "
const (
             a
)
"))
    (is (equal source (source-text (convert 'golang-ast source))))))

(deftest indentation-round-trip-computed-text-1 ()
  "Computed text ASTs' text field does not have indentation added to it."
  (let ((source "

                {
            }
"))
    (is (equal source (source-text (convert 'golang-ast source))))))

(deftest indentation-round-trip-empty-string-1 ()
  "Empty text isn't indented after a newline occurs."
  (let ((source "

              {
                ;
              }
"))
    (is (equal source (source-text (convert 'golang-ast source))))))

(deftest indentation-round-trip-trailing-newline-before-text ()
  "Before text with trailing indentation after a newline is processed correctly."
  (let ((source "import
        \"\""))
    (is (equal source (source-text (convert 'golang-ast source))))))

(deftest indentation-backpatches-sibling-indentation ()
  "Process indentation backpatches the indent-adjustment slot of siblings
after the parent indent-children slot is set."
  (let ((source "if (1) {
  return 0;
} else
  return 1;"))
    (is (equal source (source-text (convert 'c-ast source))))))


;;; Mutation Tests
(deftest moveable-indentation-python-1 ()
  (with-software-file ("nested-functions" soft genome)
    (is-moveable soft
                 (find-if {typep _ 'python-pass-statement} genome)
                 (list (find-if {typep _ 'python-function-definition} genome))
                 "nested-functions-mut")))
