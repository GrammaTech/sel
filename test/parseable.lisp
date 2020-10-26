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
                :hash-type)
  (:export :test-parseable))

(in-package :software-evolution-library/test/parseable)

(defsuite test-parseable "Tests of software/parseable")

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

(defun expand-wildcard (wildcard)
  (is (wild-pathname-p wildcard))
  (let* ((path
          (path-join (asdf:system-relative-pathname
                      :software-evolution-library
                      #p"test/etc/")
                     wildcard))
         (files (directory path)))
    (is (not (emptyp files)))
    files))

(deftest test-js-source-ranges ()
  (let ((js-files (expand-wildcard #p"javascript/*/*.js")))
    (test-ast-source-ranges-for-files 'javascript js-files)))

;; (deftest test-python-source-ranges ()
;;   (let ((py-files (expand-wildcard #p"python/*/*.py")))
;;     (test-ast-source-ranges-for-files 'python py-files)))

(deftest test-lisp-source-ranges ()
  (let ((lisp-files (expand-wildcard #p"lisp*/*.lisp")))
    (test-ast-source-ranges-for-files 'lisp lisp-files)))

(deftest test-clang-source-ranges ()
  (let ((c-files (expand-wildcard #p"*/*.c")))
    (test-ast-source-ranges-for-files 'clang c-files :limit 10)))

(defun test-ast-source-ranges-for-files (class files
                                         &key (limit 1000))
  (iter (for file in-vector (take limit (reshuffle files)))
        (ignore-errors                  ;Ignore unparseable files.
         (test-single-ast-source-ranges class file))))

(defun test-single-ast-source-ranges (class file)
  (declare (optimize debug))
  (ignore-some-conditions (mutate)
    (let* ((sw (from-file (make class) file))
           (ranges (ast-source-ranges sw))
           (text (source-text (genome sw))))
      (is (not (emptyp ranges)))
      (iter (for (ast . range) in ranges)
            (is (equal (source-range-subseq text range)
                       (source-text ast)))))))
