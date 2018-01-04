(defpackage :software-evolution-library/example
  (:use :common-lisp
        :software-evolution-library
        :software-evolution-library/utility))
#+allegro
(set-dispatch-macro-character #\# #\_
                              #'(lambda (s c n) (declare (ignore s c n)) nil))
