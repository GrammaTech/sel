;;;; cl-tree-sitter.lisp --- Common Lisp tree-sitter representation.
(defpackage :software-evolution-library/test/cl-tree-sitter
  (:nicknames :sel/test/cl-tree-sitter :sel/test/cl-ts)
  (:use
    :gt/full
    :software-evolution-library/test/util
    :stefil+
    :software-evolution-library
    :software-evolution-library/software/parseable
    :software-evolution-library/software/tree-sitter
    :software-evolution-library/software/string-clauses
    :software-evolution-library/software/cl
    :software-evolution-library/components/file
    :software-evolution-library/components/formatting
    :software-evolution-library/utility/range)
  (:import-from :cmd :$cmd)
  (:import-from :asdf/system
                :system-relative-pathname)
  (:export :test-cl-tree-sitter))
(in-package :software-evolution-library/test/cl-tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-cl-tree-sitter "CL tree-sitter representation."
  (cl-tree-sitter-available-p))



(deftest cl-parse-sel-itself ()
  (let* ((root (asdf:system-relative-pathname "software-evolution-library" ""))
         (files (lines ($cmd "find " root " -name '*.lisp'"))))
    (is files)
    (dolist (file files)
      (let ((genome (finishes (genome (from-file 'cl file)))))
        (is (equal (read-file-into-string file)
                   (source-text genome)))))))
