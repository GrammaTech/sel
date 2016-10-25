;;; view.lisp --- view functions

;;; Commentary:

;;; Code:
(in-package :software-evolution-view)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; AFL, forgive me this.
  (mapc (lambda-bind ((name value documentation))
          (eval `(define-constant ,name ,value :test 'equalp
                                  :documentation ,documentation)))
        '((+set-G1+      ")0"     "Set G1 for box drawing")
          (+reset-G1+    ")B"     "Reset G1 to ASCII")
          (+b-start+     ""       "Enter G1 drawing mode")
          (+b-stop+      ""       "Leave G1 drawing mode")
          (+b-h+         "q"        "Horizontal line")
          (+b-v+         "x"        "Vertical line")
          (+b-lt+        "l"        "Left top corner")
          (+b-rt+        "k"        "Right top corner")
          (+b-lb+        "m"        "Left bottom corner")
          (+b-rb+        "j"        "Right bottom corner")
          (+b-x+         "n"        "Cross")
          (+b-vr+        "t"        "Vertical, branch right")
          (+b-vl+        "u"        "Vertical, branch left")
          (+b-ht+        "v"        "Horizontal, branch top")
          (+b-hb+        "w"        "Horizontal, branch bottom")
          (+term-home+   "[H"     "Set terminal back to home (top left).")
          (+term-clear+  "[H[2J" "Clear terminal.")
          (+ceol+        "[0K"    "Clear to end of line.")
          (+cursor-hide+ "[?25l"  "Hide the cursor.")
          (+cursor-show+ "[?25h"  "Show the cursor.")
          ;; Colors
          (+color-BLK+   "[0;30m" "Color BLK.")
          (+color-RED+   "[0;31m" "Color RED.")
          (+color-GRN+   "[0;32m" "Color GRN.")
          (+color-BRN+   "[0;33m" "Color BRN.")
          (+color-BLU+   "[0;34m" "Color BLU.")
          (+color-MGN+   "[0;35m" "Color MGN.")
          (+color-CYA+   "[0;36m" "Color CYA.")
          (+color-NOR+   "[0;37m" "Color NOR.")
          (+color-GRA+   "[1;30m" "Color GRA.")
          (+color-LRD+   "[1;31m" "Color LRD.")
          (+color-LGN+   "[1;32m" "Color LGN.")
          (+color-YEL+   "[1;33m" "Color YEL.")
          (+color-LBL+   "[1;34m" "Color LBL.")
          (+color-PIN+   "[1;35m" "Color PIN.")
          (+color-LCY+   "[1;36m" "Color LCY.")
          (+color-BRI+   "[1;37m" "Color BRI.")
          (+color-RST+   "[0m"    "Color RST."))))

(defun clear-terminal (stream)
  (format stream "~a" +term-clear+))

(defun hide-cursor (stream)
  (format stream "~a" +cursor-hide+))

(defun show-cursor (stream)
  (format stream "~a" +cursor-show+))

(defmacro with-line-printing (stream &rest body)
  `(unwind-protect
        (progn (format ,stream "~a" +set-G1+)
               (format ,stream "~a" +b-start+)
               ,@body)
     (format ,stream "~a" +b-stop+)
     (format ,stream "~a" +reset-G1+)
     (force-output ,stream)))

(defmacro with-color-printing (stream color &rest body)
  `(unwind-protect
        (progn (format ,stream "~a" ,color) ,@body)
     (format ,stream "~a" +color-RST+)
     (force-output ,stream)))

(make-thread
 (lambda ()
   (clear-terminal *standard-output*)
   (hide-cursor *standard-output*)
   (with-color-printing *standard-output* +color-GRA+
                        (with-line-printing *standard-output*
                          (format *standard-output* "~a"
                                  (concatenate 'string +b-lt+ +b-h+ +b-h+))))
   (format *standard-output* " example ")
   (with-color-printing *standard-output* +color-GRA+
                        (with-line-printing *standard-output*
                          (format *standard-output* "~a"
                                  (concatenate 'string +b-h+ +b-h+ +b-rt+))))
   (format *standard-output* "~%")))
