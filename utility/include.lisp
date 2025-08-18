(defpackage :software-evolution-library/utility/include
  (:documentation "Code to regenerate standard library header synopses.")
  (:use :gt/full)
  (:shadowing-import-from :serapeum :~>)
  (:export :*std-header-dir*
           :std-headers-available-p
           :extract-header-synopsis))
(in-package :software-evolution-library/utility/include)

(defparameter *std-header-dir*
  (asdf:system-relative-pathname :software-evolution-library
                                 "utility/libcxx-src/include/")
  "Directory to search for standard library headers.")

(defun std-headers-available-p ()
  "Are the standard headers available for reference?"
  (file-exists-p (path-join *std-header-dir* "list")))

(defun extract-synopsis-from-string (file-string)
  (when-let ((synopsis
              (find "synopsis"
                    (split "/\\*|\\*/" file-string)
                    :test #'search)))
    (apply #'string+
           (drop-while (op (or (blankp _1)
                               (search "synopsis" _1)))
                       (lines synopsis :keep-eols t)))))

(defun extract-header-synopsis (header)
  (let ((file (path-join *std-header-dir* header)))
    (cond
      ;; As of 2022-01-26, execution doesn't have a synopsis.
      ((equal (pathname-name file) "execution") nil)
      ;; The synopsis for format doesn't include the token `synopsis'.
      ((equal (pathname-name file) "format")
       (extract-synopsis-from-string
        (string-replace "/*"
                        (read-file-into-string file)
                        "/* synopsis ")))
      ((equal (pathname-name file) "iterator")
       (~> file
           read-file-into-string
           ;; Fix up an unparseable argument by turning hyphens into
           ;; underscores.
           (string-replace "a-private-type" _ "a_private_type")
           ;; Remove extra } that prematurely closes the std namespace.
           (string-replace "}}" _ "}")
           extract-synopsis-from-string))
      (t (extract-synopsis-from-string (read-file-into-string file))))))
