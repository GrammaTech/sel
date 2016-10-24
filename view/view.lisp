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
        '((+set-G1+      "\\x1b)0"    "Set G1 for box drawing")
          (+reset-G1+    "\\x1b)B"    "Reset G1 to ASCII")
          (+b-start+     "\\x0e"      "Enter G1 drawing mode")
          (+b-stop+      "\\x0f"      "Leave G1 drawing mode")
          (+b-h+         "q"          "Horizontal line")
          (+b-v+         "x"          "Vertical line")
          (+b-lt+        "l"          "Left top corner")
          (+b-rt+        "k"          "Right top corner")
          (+b-lb+        "m"          "Left bottom corner")
          (+b-rb+        "j"          "Right bottom corner")
          (+b-x+         "n"          "Cross")
          (+b-vr+        "t"          "Vertical, branch right")
          (+b-vl+        "u"          "Vertical, branch left")
          (+b-ht+        "v"          "Horizontal, branch top")
          (+b-hb+        "w"          "Horizontal, branch bottom")
          (+term-home+   "\\x1b[H"    "Set terminal back to home (top left).")
          (+term-clear+  "\\x1b[H\\x1b[2J" "Clear terminal.")
          (+ceol+        "\\x1b[0K"   "Clear to end of line.")
          (+cursor-hide+ "\\x1b[?25l" "Hide the cursor.")
          (+cursor-show+ "\\x1b[?25h" "Show the cursor.")
          ;; Colors
          (+color-BLK+   "\\x1b[0;30m" "Color BLK.")
          (+color-RED+   "\\x1b[0;31m" "Color RED.")
          (+color-GRN+   "\\x1b[0;32m" "Color GRN.")
          (+color-BRN+   "\\x1b[0;33m" "Color BRN.")
          (+color-BLU+   "\\x1b[0;34m" "Color BLU.")
          (+color-MGN+   "\\x1b[0;35m" "Color MGN.")
          (+color-CYA+   "\\x1b[0;36m" "Color CYA.")
          (+color-NOR+   "\\x1b[0;37m" "Color NOR.")
          (+color-GRA+   "\\x1b[1;30m" "Color GRA.")
          (+color-LRD+   "\\x1b[1;31m" "Color LRD.")
          (+color-LGN+   "\\x1b[1;32m" "Color LGN.")
          (+color-YEL+   "\\x1b[1;33m" "Color YEL.")
          (+color-LBL+   "\\x1b[1;34m" "Color LBL.")
          (+color-PIN+   "\\x1b[1;35m" "Color PIN.")
          (+color-LCY+   "\\x1b[1;36m" "Color LCY.")
          (+color-BRI+   "\\x1b[1;37m" "Color BRI.")
          (+color-RST+   "\\x1b[0m"    "Color RST."))))

#+(or )
(string-to-file
 (concatenate 'string
   +set-G1+ +b-start+ +color-GRA+ +b-lt+ +b-h+ +b-h+ +b-stop+ +reset-G1+
   +color-CYA+ " eric " +color-RST+
   +set-G1+ +b-start+ +color-GRA+ +b-h+ +b-h+ +b-rt+ +b-stop+ +color-RST+ +reset-G1+)
 "/tmp/it")
;;; printf "$(cat /tmp/it)\n"
