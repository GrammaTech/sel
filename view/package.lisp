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
   :trivial-shell
   :cl-ppcre
   :cl-store
   :cl-dot
   :diff
   :bordeaux-threads
   :software-evolution
   :flexi-streams)
  (:export :*view-running*
           :*view-length*
           :*view-stream*
           :view-status
           :view-start))

#+allegro
(set-dispatch-macro-character #\# #\_
                              #'(lambda (s c n) (declare (ignore s c n)) nil))
