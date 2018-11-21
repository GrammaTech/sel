;;; formatting.lisp --- source code formatting
;;;
;;; This file provides a generic interface for all software
;;; objects implementing source code formatting along with
;;; free functions with concrete implements for code
;;; formatting (beautifying) software objects.
;;;
;;; @texi{formatting}
(defpackage :software-evolution-library/components/formatting
  (:nicknames :sel/components/formatting :sel/cp/formatting)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/ast
        :software-evolution-library/software/source
        :software-evolution-library/software/parseable)
  (:export :format-genome
           :astyle
           :clang-tidy
           :clang-format
           :prettier))
(in-package :software-evolution-library/components/formatting)
(in-readtable :curry-compose-reader-macros)


;;; Generic method for applying a code formatting tool to a software
;;; object's genome.
(defgeneric format-genome (software &key)
  (:documentation "Generic method to code format SOFTWARE which
may be beautified."))


;;; Specific formatting functions for individual software objects.
(defun astyle (obj &optional (style "kr") (options '("--add-brackets"))
               &aux errno)
  "Apply Artistic Style to OBJ.
* OBJ object to format and return
* STYLE style to utilize
* OPTIONS list of additional options to astyle
* ERRNO exit code of astyle binary
"
  (with-temp-file-of (src (ext obj)) (genome obj)
    (setf (genome obj)
          (multiple-value-bind (stdout stderr exit)
              (shell "astyle --suffix=none ~{~a~^ ~} --style=~a ~a"
                     options style src)
            (declare (ignorable stdout stderr))
            (setf errno exit)
            (if (zerop exit)
                (file-to-string src)
                (genome obj)))))
  (values obj errno))

(defun clang-tidy (obj &optional
    (checks '("cppcore-guidelines*"
              "misc*"
              "-misc-macro-parentheses"
              "-misc-static-assert"
              "-misc-unused-parameters"
              "-modernize*"
              "performance*"
              "-performance-unnecessary-value-param"
              "readability*"
              "-readability-else-after-return"
              "-readability-function-size"
              "-readability-identifier-naming"
              "-readability-implicit-bool-conversion"
              "-readability-inconsistent-declaration-parameter-name"
              "-readability-non-const-parameter"
              "-readability-redundant-control-flow"
              "-readability-redundant-declaration"))
     &aux errno)
  "Apply `clang-tidy` to OBJ.
* OBJ object to tidy and return
* CHECKS list of clang-tidy checks to apply
* ERRNO exit code of clang-tidy
"
  (setf (genome obj)
        (with-temp-file-of (src (ext obj)) (genome obj)
          (multiple-value-bind (stdout stderr exit)
              (shell
               "clang-tidy -fix -fix-errors -checks=~{~a~^,~} ~a -- ~a 1>&2"
               checks
               src
               (mapconcat #'identity (flags obj) " "))
            (declare (ignorable stdout stderr))
            (setf errno exit)
            (if (zerop exit) (file-to-string src) (genome obj)))))
  (values obj errno))

(defun clang-format (obj &optional style &aux errno)
  "Apply `clang-format` to OBJ.
* OBJ object to format and return
* STYLE clang-format style to utilize
* ERRNO exit code of clang-format
"
  (with-temp-file-of (src (ext obj)) (genome obj)
    (setf (genome obj)
          (multiple-value-bind (stdout stderr exit)
              (shell "clang-format ~a ~a"
                     (if style
                         (format nil "-style=~a" style)
                         (format nil
                                 "-style='{BasedOnStyle: Google,~
                                AllowShortBlocksOnASingleLine: false,~
                                AllowShortCaseLabelsOnASingleLine: false,~
                                AllowShortFunctionsOnASingleLine: false,~
                                AllowShortIfStatementsOnASingleLine: false,~
                                AllowShortLoopsOnASingleLine: false,~
                                ReflowComments: false,~
                                SortIncludes: false}'"))
                     src)
            (declare (ignorable stderr))
            (setf errno exit)
            (if (zerop exit) stdout (genome obj)))))
  (values obj errno))

(defun prettier (obj &aux errno)
  "Apply `prettier` to OBJ.
* OBJ object to format and return
* ERRNO exit code of prettier
"
  (with-temp-file-of (src (ext obj)) (genome obj)
    (setf (genome obj)
          (multiple-value-bind (stdout stderr exit)
              (shell "prettier ~a" src)
            (declare (ignorable stderr))
            (setf errno exit)
            (if (zerop exit) stdout (genome obj)))))
  (values obj errno))
