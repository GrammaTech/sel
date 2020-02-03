(defpackage :software-evolution-library/test/asm
  (:nicknames :sel/test/asm)
  (:use :common-lisp :alexandria)
  (:export :+etc-dir+
           :+gcd-dir+
           :+grep-prj-dir+
           :+multi-file-dir+
           :+headers-dir+))
(in-package :software-evolution-library/test/asm)

(define-constant +etc-dir+
    (append (pathname-directory
             #.(or *compile-file-truename*
                   *load-truename*
                   *default-pathname-defaults*))
            (list "test" "etc"))
  :test #'equalp
  :documentation "Path to directory holding testing artifacts.")

(define-constant +gcd-dir+ (append +etc-dir+ (list "gcd"))
  :test #'equalp
  :documentation "Path to directory holding gcd.")

(define-constant +grep-prj-dir+ (append +etc-dir+ (list "grep-prj"))
  :test #'equalp
  :documentation "Path to directory holding the grep project.")

(define-constant +multi-file-dir+ (append +etc-dir+ (list "multi-file"))
  :test #'equalp
  :documentation "Path to directory holding the multi-file example.")

(define-constant +headers-dir+ (append +etc-dir+ (list "headers"))
  :test #'equalp
  :documentation "Path to directory holding headers.")

(defun gcd-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +gcd-dir+))

(defun headers-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +headers-dir+))
