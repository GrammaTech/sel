(defpackage :software-evolution-library/terminal
  (:use :gt :cffi :cl-interpol)
  (:export :make-terminal-raw
           :make-terminal-unraw
           :ioctl
           :term-size
           :return-no-extent-term
           :*view-stream*
           ;; Colors and control sequences.
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
           :+color-RST+))
