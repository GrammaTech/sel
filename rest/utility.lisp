;;; utility.lisp --- Utilities for RESTful interfaces over SEL.
;;;
;;; This includes generic, shared helper functions for use with the REST
;;; library in SEL.
;;;
(defpackage :software-evolution-library/rest/utility
  (:nicknames :sel/rest/util :sel/rest/utility)
  (:use
   :gt/full
   :cl-json
   :snooze)
  (:shadowing-import-from :clack :clackup :stop)
  (:export :convert-symbol
           :make-gensym-string
           :decode-json-payload))
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

(defun decode-json-payload ()
  "Returns valid JSON or an empty list, when the payload is empty.
Will raise an error if the JSON is malformed and decoding fails."
  (if-let* ((payload (payload-as-string))
            (string-nonempty (not (emptyp payload))))
    ;;(json:decode-json-from-string payload)
    (json:decode-json-from-string payload)
    '()))
