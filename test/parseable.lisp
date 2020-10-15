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
   :software-evolution-library/software/lisp)
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

(-> source-range-subseq (string source-range)
    (values string &optional))
(defun source-range-subseq (text range)
  (ematch range
    ((source-range :begin (source-location :line line1 :column col1)
                   :end (source-location :line line2 :column col2))
     (assert (<= line1 line2))
     (let* ((lines (serapeum:lines text
                                   :count line2
                                   :keep-eols t)))
       (cond ((emptyp lines) "")
             ((= line1 line2)
              (let ((line (nth (1- line1) lines)))
                (subseq line (1- col1) (1- col2))))
             ((and (= line2 (1+ line1))
                   (= col2 1))
              (let ((line (nth (1- line1) lines)))
                (subseq line (1- col1))))
             (t
              (let* ((lines (drop (1- line1) lines))
                     ;; Subtlety: source-range addresses a node that ends
                     ;; in a newline as (n+1,1), but that extra line is
                     ;; not returned by serapeum:lines. So the "last"
                     ;; line may not exist.
                     (lines (take (if (= col2 1)
                                      (- line2 line1)
                                      (1+ (- line2 line1)))
                                  lines))
                     (first-line (subseq (first lines) (1- col1)))
                     (last-line (nth (1- line2) lines))
                     (last-line-sliced
                      (and last-line
                           (slice last-line 0 (1- col2)))))
                (multiple-value-call #'concatenate 'string
                  first-line
                  (values-list (rest (remove last-line lines)))
                  (if last-line last-line-sliced (values))))))))))

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
  (declare (optimize debug))
  (let ((js-files (expand-wildcard #p"javascript/*/*.js")))
    (test-ast-source-ranges-for-files 'javascript js-files)))

(deftest test-python-source-ranges ()
  (declare (optimize debug))
  (let ((py-files (expand-wildcard #p"python/*/*.py")))
    (test-ast-source-ranges-for-files 'python py-files)))

(deftest test-lisp-source-ranges ()
  (declare (optimize debug))
  (let ((lisp-files (expand-wildcard #p"lisp*/*.lisp")))
    (test-ast-source-ranges-for-files 'lisp lisp-files)))

(defun test-ast-source-ranges-for-files (class files)
  (iter (for file in-vector (reshuffle files))
        (test-single-ast-source-ranges class file)))

(defun test-single-ast-source-ranges (class file)
  (declare (optimize debug))
  (let* ((sw (from-file (make class) file))
         (ranges (ast-source-ranges sw))
         (text (source-text (genome sw))))
    (iter (for (ast . range) in ranges)
          (is (equal (source-range-subseq text range)
                     (source-text ast))))))
