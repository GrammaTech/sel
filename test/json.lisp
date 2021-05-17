;;;; json.lisp --- JSON tree-sitter representation.
(defpackage :software-evolution-library/test/json
  (:nicknames :sel/test/json)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/json
   :software-evolution-library/components/file
   :software-evolution-library/components/formatting)
  (:export :test-json))
(in-package :software-evolution-library/test/json)
(in-readtable :curry-compose-reader-macros)
(defsuite test-json "JSON tree-sitter representation."
  (json-tree-sitter-available-p))


;;; Utility
(defixture trivial-json
  (:setup
   (setf *soft*
         (from-file (make-instance 'json) (javascript-dir #P"trivial.json"))))
  (:teardown
   (setf *soft* nil)))


;;; Tests
(deftest json-can-parse-a-software-object ()
  (with-fixture trivial-json
    (is (not (zerop (size *soft*))))
    (is (typep (genome *soft*) 'json-ast))))

(deftest json-preserves-trailing-whitespace ()
  (let* ((ws (fmt "     ~%"))
         (genome (string+ "{\"x\": 1}" ws))
         (json (make-instance 'json :genome genome)))
    (is (string$= ws (genome-string json)))))

(deftest json-lone-surrogate-round-trip ()
  (let* ((json "\"\\ud800-\\udbff\"")
         (json-ast (convert 'json-ast json)))
    (is (equalp json (source-text json-ast)))))
