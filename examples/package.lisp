(defpackage :software-evolution-example
  (:use :common-lisp
        :software-evolution
        :software-evolution-utility))
#+allegro
(set-dispatch-macro-character #\# #\_
                              #'(lambda (s c n) (declare (ignore s c n)) nil))
