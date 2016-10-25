(defpackage :software-evolution-view
  (:nicknames :se-view)
  (:use
   :common-lisp
   :alexandria
   :cl-interpol
   :metabang-bind
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :trivial-shell
   :cl-ppcre
   :cl-store
   :cl-dot
   :diff
   :bordeaux-threads)
  (:export :view))

#+allegro
(set-dispatch-macro-character #\# #\_
                              #'(lambda (s c n) (declare (ignore s c n)) nil))
