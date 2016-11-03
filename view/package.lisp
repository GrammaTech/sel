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
   :cl-interpol)
  (:shadow :diff)
  (:export :*view-stream*
           :*view-length*
           :*view-delay*
           :*view-running*
           :*view-application-name*
           :*view-application-version*
           :*view-run-name*
           :*view-mutation-header-p*
           :*view-max-mutations*
           :*view-max-note-lines*
           :*view-max-best-lines*
           :view-truncate
           :view-status
           :view-start))

#+allegro
(set-dispatch-macro-character #\# #\_
                              #'(lambda (s c n) (declare (ignore s c n)) nil))
