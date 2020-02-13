;;; Utilities associated with json processing
(defpackage software-evolution-library/utility/json
  (:nicknames :sel/util/json)
  (:use :gt/full))
(in-package :software-evolution-library/utility/json)

(defun convert-jsown-tree (jt &optional (key-fn (lambda (s)
                                                  (intern (string-upcase s)
                                                          :keyword))))
  "Converts the tree representation from JSOWN into something similar to
output from CL-JSON.  KEY-FN, if present, maps keyword strings to keywords."
  (labels ((%convert (jt)
             (typecase jt
               ((cons (eql :obj) t)
                (%convert-obj (cdr jt)))
               (cons
                (mapcar-improper-list #'%convert jt))
               (t jt)))
           (%convert-obj (key-alist)
             (iter (for (key . val) in key-alist)
                   (collect (cons (funcall key-fn key)
                                  (%convert val))))))
    (%convert jt)))

(defun strings-to-string-cases (strings)
  (iter (for n in strings)
        (collect (list n (intern (string-upcase n)
                                 :keyword)))))

(defun string-case-to-keyword-body (strings s)
  `(string-case (,s) ,@(strings-to-string-cases strings)
                (t (intern (string-upcase ,s) :keyword))))

(defmacro string-case-to-keywords (strings str)
  "Macro to convert a string to a keyword, using string-case to
accelerate the common cases given by STRINGS."
  (unless (and (listp strings)
               (every #'stringp strings))
    (error "Usage: (string-case-to-keywords <list of string constants> form)"))
  (let ((v (gensym "STR")))
    `(let ((,v ,str))
       (etypecase ,v
         (simple-base-string
          ,(string-case-to-keyword-body strings `(the simple-base-string ,v)))
         #-ccl
         ((and simple-string (vector character))
          ,(string-case-to-keyword-body
            strings `(the (and simple-string (vector character)) ,v)))
         (string (intern (string-upcase ,v) :keyword))))))
