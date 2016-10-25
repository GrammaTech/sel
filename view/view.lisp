;;; view.lisp --- view functions

;;; Commentary:

;;; Code:
(in-package :software-evolution-view)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar *view-stream* t
  "Dynamically bind to use modify.")

(define-constant +golden-ratio+ 21/34)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; AFL, forgive me this.
  (mapc (lambda-bind ((name value documentation))
          (eval `(define-constant ,name ,value :test 'equalp
                                  :documentation ,documentation)))
        '((+set-G1+      ")0"     "Set G1 for box drawing")
          (+reset-G1+    ")B"     "Reset G1 to ASCII")
          (+b-start+     ""       "Enter G1 drawing mode")
          (+b-stop+      ""       "Leave G1 drawing mode")
          (+b-h+         #\q        "Horizontal line")
          (+b-v+         #\x        "Vertical line")
          (+b-lt+        #\l        "Left top corner")
          (+b-rt+        #\k        "Right top corner")
          (+b-lb+        #\m        "Left bottom corner")
          (+b-rb+        #\j        "Right bottom corner")
          (+b-x+         #\n        "Cross")
          (+b-vr+        #\t        "Vertical, branch right")
          (+b-vl+        #\u        "Vertical, branch left")
          (+b-ht+        #\v        "Horizontal, branch top")
          (+b-hb+        #\w        "Horizontal, branch bottom")
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

(defun clear-terminal ()
  (format *view-stream* "~a" +term-clear+))

(defun hide-cursor ()
  (format *view-stream* "~a" +cursor-hide+))

(defun show-cursor ()
  (format *view-stream* "~a" +cursor-show+))

(defmacro with-line-printing (&rest body)
  `(unwind-protect
        (progn (format ,*view-stream* "~a" +set-G1+)
               (format ,*view-stream* "~a" +b-start+)
               ,@body)
     (format ,*view-stream* "~a" +b-stop+)
     (format ,*view-stream* "~a" +reset-G1+)
     (force-output ,*view-stream*)))

(defmacro with-color-printing (color &rest body)
  `(unwind-protect
        (progn (format ,*view-stream* "~a" ,color) ,@body)
     (format ,*view-stream* "~a" +color-RST+)
     (force-output ,*view-stream*)))

(defun label-line-print (length &key (label "") (left +b-lt+) (right +b-rt+)
                                  (balance (- 1 +golden-ratio+))
                                  (color +color-GRA+)
                                  (label-color +color-RST+)
                                  (filler +b-h+))
  (let ((left-l (floor (* (- length (+ 2 (length label))) balance)))
        (right-l (ceiling (* (- length (+ 2 (length label))) (- 1 balance)))))
    (assert (and (>= left-l 0) (>= right-l 0))
            (left-l right-l)
            "Padding on one side is negative (~a,~a)" left-l right-l)
    (with-color-printing color
      (with-line-printing
          (format *view-stream* "~a" (concatenate 'string
                                       (string left)
                                       (make-string left-l
                                                    :initial-element filler)))))
    (with-color-printing label-color (format *view-stream* label))
    (with-color-printing color
      (with-line-printing
          (format *view-stream* "~a" (concatenate 'string
                                       (make-string right-l
                                                    :initial-element filler)
                                       (string right)))))
    (format *view-stream* "~%")))

(eval-when (:execute)
  (make-thread
   (lambda ()
     (let ((*view-stream* *standard-output*))
       (clear-terminal)
       (hide-cursor)
       (label-line-print 65 :label " example " :label-color +color-CYA+)
       (label-line-print 65 :label " lorem ipsum "
                         :balance 0 :filler #\Space :left +b-v+ :right +b-v+)
       (label-line-print 65 :label " example2 " :label-color +color-CYA+
                         :left +b-vr+ :right +b-vl+)
       (label-line-print 65 :label " lorem ipsum "
                         :balance 0 :filler #\Space :left +b-v+ :right +b-v+)
       (label-line-print 65 :left +b-lb+ :right +b-rb+)))))
