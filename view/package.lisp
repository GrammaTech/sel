(defpackage :software-evolution-library/view
  (:nicknames :sel/view)
  (:use
   :common-lisp
   :alexandria
   :metabang-bind
   :named-readtables
   :curry-compose-reader-macros
   :arrow-macros
   :iterate
   :split-sequence
   :cl-ppcre
   :cl-store
   :cl-dot
   :diff
   :bordeaux-threads
   :software-evolution-library
   :software-evolution-library/utility
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
           :*view-max-best-offset*
           :*view-functions*
           ;; Colors.
           :+set-G1+
           :+reset-G1+
           :+b-start+
           :+b-stop+
           :+b-h+
           :+b-v+
           :+b-lt+
           :+b-rt+
           :+b-lb+
           :+b-rb+
           :+b-x+
           :+b-vr+
           :+b-vl+
           :+b-ht+
           :+b-hb+
           :+term-home+
           :+term-clear+
           :+ceol+
           :+cursor-hide+
           :+cursor-show+
           :+color-BLK+
           :+color-RED+
           :+color-GRN+
           :+color-BRN+
           :+color-BLU+
           :+color-MGN+
           :+color-CYA+
           :+color-NOR+
           :+color-GRA+
           :+color-LRD+
           :+color-LGN+
           :+color-YEL+
           :+color-LBL+
           :+color-PIN+
           :+color-LCY+
           :+color-BRI+
           :+color-RST+
           :+golden-ratio+
           :label-line-print
           ;; Utility functions.
           :best-print
           ;; View functions.
           :timing-view-function
           :population-label-view-function
           :fitness-view-function
           :genome-view-function
           :mutation-label-view-function
           :mutation-view-function
           :best-label-view-function
           :best-view-function
           :notes-label-view-function
           :notes-view-function
           ;; Interface functions.
           :with-delayed-invocation
           :view-truncate
           :view-start
           :view-controller-start))
#+allegro
(set-dispatch-macro-character #\# #\_
                              #'(lambda (s c n) (declare (ignore s c n)) nil))
