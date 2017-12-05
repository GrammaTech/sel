;;; .gendoc.lisp --- Customization of gendoc for SEL
;;;
;;; Makes the following changes.
;;; - `foo' becomes a link to [foo](#foo).
;;; - `*FOO*' becomes a literal to `*FOO*`.
;;; - The return values are taken from the end instead of the beginning.
;;;

(defpackage :software-evolution-library-gendoc
  (:nicknames :sel-gendoc)
  (:use :common-lisp :gendoc :cl-ppcre)
  (:export :gendoc))

(in-package :software-evolution-library-gendoc)

;;; Customize `gendoc::apiref-doc' to handle lisp conventions.
;;; - `foo' becomes a link to foo.
;;; - `*FOO*' becomes a link to *FOO*.
(defun lisp-documentation-to-markdown (documentation-string)
  (regex-replace-all "`([\\S][\\S]+)'"
    (regex-replace-all "\\*([A-Z-_0-9]+)\\*" documentation-string "`*\\1*`")
    "[`\\1`](#apiref-\\1)"))

(defun original-apiref-doc (type sym)
  (or 
   (ecase type
     (:special (documentation sym 'variable))
     ((or :macro :function)
      (let* ((ds (documentation sym 'function))
             (last-line-start (position #\Newline ds :from-end t)))
        (if last-line-start
            (let ((last-line (subseq ds (1+ last-line-start))))
              (if (and (> (length last-line) 1)
                       (string= (subseq last-line 0 2) "=>"))
                  (subseq ds 0 (position #\Newline ds :from-end t))
                  ds))
            ds))))
   "*Undocumented!*"))

(defun gendoc::apiref-doc (type sym)
  (lisp-documentation-to-markdown (original-apiref-doc type sym)))

;;; Change apiref-result to work with "=>" at the end of the line.
(defun gendoc::apiref-result (type sym)
  (ecase type
    (:special "")
    ((or :macro :function)
     (let* ((ds (documentation sym 'function))
            (last-line-start (position #\Newline ds :from-end t)))
       (if last-line-start
           (let ((last-line (subseq ds (1+ last-line-start))))
             (if (and (> (length last-line) 1)
                      (string= (subseq last-line 0 2) "=>"))
                 (subseq last-line 2 (position #\Newline last-line))
                 ""))
           "")))))

(gendoc (:output-filename "doc/api.html" :css "api.css")
  (:mdf #P"./README.md")
  (:apiref :software-evolution-library :software-evolution-library-utility))
