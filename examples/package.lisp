(defpackage :software-evolution-example
  (:use :common-lisp
        :software-evolution
        :software-evolution-utility))
(in-package :software-evolution-example)
(enable-curry-compose-reader-macros :include-utf8)
#+allegro
(set-dispatch-macro-character #\# #\_
                              #'(lambda (s c n) (declare (ignore s c n)) nil))
