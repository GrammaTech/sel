(defpackage :software-evolution-view
  (:nicknames :se-view)
  (:use
   :common-lisp
   :alexandria
   :metabang-bind
   :curry-compose-reader-macros
   :cl-arrows
   :iterate
   :split-sequence
   :cl-ppcre
   :cl-store
   :cl-dot
   :diff
   :bordeaux-threads
   :software-evolution
   :software-evolution-utility
   :flexi-streams
   :cl-interpol)
  (:shadow :diff)
  (:export :*view-stream*
           :*view-length*
           :*view-running*
           :*view-run-name*
           :*view-mutation-show-header*n
           :*view-max-mutations*
           :view-status
           :view-start))

#+allegro
(set-dispatch-macro-character #\# #\_
                              #'(lambda (s c n) (declare (ignore s c n)) nil))
