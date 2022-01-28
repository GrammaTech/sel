(defpackage :software-evolution-library/utility/include
  (:documentation "Code to regenerate standard library header synopses.")
  (:use :gt/full)
  (:import-from :serapeum :~>>)
  (:export :extract-llvm-synopses
           :*std-header-dir*
           :extract-header-synopsis
           :std-headers-available-p))
(in-package :software-evolution-library/utility/include)

(defparameter *std-header-dir*
  (asdf:system-relative-pathname :software-evolution-library
                                 "utility/libcxx-src/include/")
  "Directory to search for standard library headers.")

(defun std-headers-available-p ()
  "Are the standard headers available for reference?"
  (file-exists-p (path-join *std-header-dir* "list")))

(defun extract-synopsis-from-string (file-string)
  (let ((synopsis (find "synopsis"
                        (split "/\\*|\\*/" file-string)
                        :test #'search)))
    (apply #'string+
           (drop-while (op (or (blankp _1)
                               (search "synopsis" _1)))
                       (lines synopsis :keep-eols t)))))

(defun extract-header-synopsis (header)
  (let* ((file (path-join *std-header-dir* (pathname-name header))))
    (cond
      ;; As of 2022-01-26, execution doesn't have a synopsis.
      ((equal (pathname-name file) "execution") nil)
      ;; The synopsis for format doesn't include the token `synopsis'.
      ((equal (pathname-name file) "format")
       (extract-synopsis-from-string
        (string-replace "/*"
                        (read-file-into-string file)
                        "/* synopsis ")))
      (t (extract-synopsis-from-string (read-file-into-string file))))))
