;;; clang.lisp --- clang software representation
;;;
;;; Mixins classes for adding the ability to code format (beautify)
;;; software objects.
;;;
;;; @texi{formattable}
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;; Formatting methods
(defgeneric astyle (software &optional style options)
  (:documentation "Apply Artistic Style to SOFTWARE, optionally using
the given STYLE and list of OPTIONS"))

(defgeneric clang-tidy (software &optional checks)
  (:documentation "Apply the fixing clang tool to SOFTWARE, optionally using
the given list of CHECKS."))

(defgeneric clang-format (software &optional style)
  (:documentation "Apply the formatting clang tool to SOFTWARE, optionally using
the given STYLE."))


;;; Mixin classes
(defclass astyleable () ())
(defclass clang-formattable () ())
(defclass clang-tidyable () ())
(defclass astyleable-project () ())
(defclass clang-formattable-project () ())
(defclass clang-tidyable-project () ())


;;; Implementation
(defmethod astyle
    ((obj astyleable) &optional (style "kr") (options '("--add-brackets"))
     &aux errno)
  "Apply Artistic Style to OBJ.
* OBJ object to format and return
* STYLE style to utilize
* OPTIONS list of additional options to astyle
* ERRNO Exit code of astyle binary
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

(defmethod clang-tidy
    ((obj clang-tidyable)
     &optional (checks '("cppcore-guidelines*"
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
  "Apply clang-tidy to OBJ.
* OBJ object to tidy and return
* CHECKS list of clang-tidy checks to apply
* ERRNO Exit code of clang-tidy
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

(defmethod clang-format ((obj clang-formattable) &optional style &aux errno)
  "Apply clang-format to OBJ.
* OBJ object to format and return
* STYLE clang-format style to utilize
* ERRNO Exit code of clang-format
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

(defmethod astyle ((project astyleable-project)
                   &optional (style nil style-p) (options nil options-p))
  "Apply Artistic Style to PROJECT.
* PROJECT project to format and return
* STYLE style to utilize
* OPTIONS list of additional options to astyle
"
  (apply-to-project project
                    (cond
                      (options-p {astyle _ style options})
                      (style-p {astyle _ style})
                      (t #'astyle))))

(defmethod clang-format ((project clang-formattable-project)
                         &optional (style nil style-p))
  "Apply `clang-format' to PROJECT.
* PROJECT project to format and return
* STYLE `clang-format' style to utilize
"
  (apply-to-project project
                    (if style-p {clang-format _ style} #'clang-format)))

(defmethod clang-tidy ((project clang-tidyable-project)
                       &optional (checks nil checks-p))
  "Apply `clang-tidy' to PROJECT.
* PROJECT project to tidy and return
"
  (apply-to-project project
                    (if checks-p {clang-tidy _ checks} #'clang-tidy)))

