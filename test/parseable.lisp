;;; parseable.lisp -- tests for software/parseable.lisp
(defpackage :software-evolution-library/test/parseable
  (:nicknames :sel/test/parseable)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/utility/range
   :software-evolution-library/software/parseable
   :software-evolution-library/software/python
   :software-evolution-library/software/javascript
   :software-evolution-library/software/lisp
   :software-evolution-library/software/clang)
  (:import-from :software-evolution-library/software/parseable
                :hash-type
                :parseable)
  (:export :test-parseable))

(in-package :software-evolution-library/test/parseable)

(defsuite test-parseable "Tests of software/parseable")

(deftest mapcar-test ()
  "Test that mapcar on parseable returns parseable."
  (is (typep (mapcar #'identity
                     (make 'parseable :genome (make 'ast)))
             'parseable)))

(deftest ast-hash-tests ()
  ;; Test that ast-hash works
  (let ((values (list 0 1 nil :a 'a '(a . b)
                      "foo" #(1 2 3) #b110101
                      (make-array '(3) :element-type '(unsigned-byte 8))
                      #'car #p"/")))
    (dolist (v values)
      (let ((h (ast-hash v)))
        (is (typep h 'hash-type)
            "Value not hashed to a hash-type: ~a ==> ~a"
            v h)))))

(deftest ast-hash-overflow-test ()
  "Test that ast hash doesn't overflow on a long list."
  (finishes (ast-hash (make-list (expt 2 18) :initial-element "foo"))))

(defun expand-wildcard (wildcard)
  "Get test files matching WILDCARD relative to test/etc/."
  (is (wild-pathname-p wildcard))
  (let* ((path
          (path-join (asdf:system-relative-pathname
                      :software-evolution-library
                      #p"test/etc/")
                     wildcard))
         (files (directory path)))
    (is (not (emptyp files)))
    files))

(deftest test-javascript-source-ranges ()
  (let ((js-files (expand-wildcard #p"javascript/*/*.js")))
    (test-ast-source-ranges-for-files 'javascript js-files)))

(deftest test-python-source-ranges ()
  (let ((py-files (expand-wildcard #p"python/*/*.py")))
    ;; We ignore whitespace here because
    (test-ast-source-ranges-for-files 'python py-files
                                      :ignore-indentation t)))

(deftest test-lisp-source-ranges ()
  (let ((lisp-files (expand-wildcard #p"lisp*/*.lisp")))
    (test-ast-source-ranges-for-files 'lisp lisp-files)))

(deftest test-clang-source-ranges ()
  (let ((c-files (expand-wildcard #p"*/*.c")))
    ;; There are a lot of Clang source files and parsing them is slow
    ;; so set a limit. Note the files actually tested are chosen at
    ;; random from the set of all files.
    (test-ast-source-ranges-for-files 'clang c-files :limit 10)))

(defun test-ast-source-ranges-for-files (class files
                                         &key (limit 1000)
                                           ignore-indentation)
  (iter (for file in-vector (take limit (reshuffle files)))
        (ignore-errors                      ;Ignore unparseable files.
         (test-single-ast-source-ranges
          class file
          :ignore-indentation ignore-indentation))))

(defun test-single-ast-source-ranges (class file &key ignore-indentation)
  "Test that AST source ranges round-trip.
That is, test that the result of calling `source-text' on an AST is the same as calling `ast-source-ranges' on its containing software and extracting the specified range from the software's serialization."
  (ignore-some-conditions (mutate)
    (let* ((sw (from-file (make class) file))
           (ranges (ast-source-ranges sw))
           (text (source-text (genome sw))))
      (is (not (emptyp ranges)))
      (iter (for (ast . range) in ranges)
            (let ((reference-text (source-range-subseq text range))
                  (output-text (source-text ast)))
              (if ignore-indentation
                  (let ((output-lines (lines output-text))
                        (reference-lines (lines reference-text)))
                    (is (length= output-lines reference-lines))
                    (is (every #'string$= output-lines reference-lines)))
                  (is (equal reference-text output-text))))))))
