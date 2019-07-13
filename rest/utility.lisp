;;; utility.lisp --- Utilities for RESTful interfaces over SEL.
;;;
;;; This includes generic, shared helper functions for use with the REST
;;; library in SEL.
;;;
(defpackage :software-evolution-library/rest/utility
  (:nicknames :sel/rest/util :sel/rest/utility)
  (:use
   :common-lisp
   :alexandria
   :named-readtables
   :curry-compose-reader-macros
   :common-lisp)
  (:shadowing-import-from :clack :clackup :stop)
  (:export :convert-symbol
           :make-gensym-string))
(in-package :software-evolution-library/rest/utility)
(in-readtable :curry-compose-reader-macros)

(defun convert-symbol (string)
  "If a string contains '::' then convert it to a symbol if possible."
  (if-let ((sym (and (stringp string) (search "::" string)
                     (read-from-string string))))
    (if (symbolp sym)
        sym
        string)
    string))

(defun make-gensym-string (input)
  "Converts the input to a string, appending a random (gensym'd) number."
  (symbol-name (gensym (string input))))
