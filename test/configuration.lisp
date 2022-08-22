;;;; configuration.lisp --- Configuration Files.
(defpackage :software-evolution-library/test/configuration
  (:nicknames :sel/test/configuration :sel/test/cfg)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/project
   :software-evolution-library/software/c-project
   :software-evolution-library/software/directory
   :software-evolution-library/components/configuration)
  (:export :test-configuration))
(in-package :software-evolution-library/test/configuration)
(in-readtable :curry-compose-reader-macros)
(defsuite test-configuration "Configuration File tests")


;;; Utility

(define-constant +ignore-paths-project+
    (append +etc-dir+ (list "ignore-path-config"))
  :test #'equalp
  :documentation "Path to the ignore-paths configuration example.")

(defun ignore-paths-project-path ()
  (make-pathname :directory +ignore-paths-project+))


;;; Tests

(deftest configuration-ignore-paths ()
  "Configuration files correctly set ignore-paths when read in."
  (let ((project (from-file 'c-project (ignore-paths-project-path))))
    (is (find-if {string= "x.h"} (ignore-paths project)))
    (is (not (find-if (of-type 'file-ast) (genome project))))))
