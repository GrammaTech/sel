(in-package :software-evolution-library/terminal)
(in-readtable :interpol-syntax)

(defun make-terminal-raw ()
  "Place the terminal into 'raw' mode, no echo or delete.
This allows characters to be read directly without waiting for a newline.
See 'man 3 termios' for more information."
  #+win32 (error "`make-terminal-raw' not implemented for windows.")
  #-sbcl (error "`make-terminal-raw' not implemented for non-SBCL.")
  #+sbcl
  (let ((options (sb-posix:tcgetattr 0)))
    (setf (sb-posix:termios-lflag options)
          (logand (sb-posix:termios-lflag options)
                  (lognot (logior sb-posix:icanon
                                  sb-posix:echo
                                  sb-posix:echoe
                                  sb-posix:echok
                                  sb-posix:echonl))))
    (sb-posix:tcsetattr 0 sb-posix:TCSANOW options)))

(defun make-terminal-unraw ()
  "Place the terminal out of 'raw' mode, with echo and delete.
This allows characters to be read directly without waiting for a newline.
See 'man 3 termios' for more information."
  #+win32 (error "`make-terminal-raw' not implemented for windows.")
  #-sbcl (error "`make-terminal-raw' not implemented for non-SBCL.")
  #+sbcl
  (let ((options (sb-posix:tcgetattr 0)))
    (setf (sb-posix:termios-lflag options)
          (logior (sb-posix:termios-lflag options)
                  sb-posix:icanon
                  sb-posix:echo
                  sb-posix:echoe
                  sb-posix:echok
                  sb-posix:echonl))
    (sb-posix:tcsetattr 0 sb-posix:TCSANOW options)))


;;; Terminal size with CFFI and ioctl.
;;; Adapted from:
;;; https://github.com/cffi/cffi/blob/master/examples/gettimeofday.lisp
(define-foreign-type ioctl-result-type ()
  ()
  (:actual-type :int)
  (:simple-parser ioctl-result))

(define-condition ioctl (error)
  ((ret :initarg :ret :initform nil :reader ret))
  (:report (lambda (condition stream)
             (format stream "IOCTL call failed with return value ~d~%~
(NOTE: IOCTL fails when called from slime.)"
                     (ret condition)))))

(defmethod translate-from-foreign (value (type ioctl-result-type))
  (if (minusp value)
      (error (make-condition 'ioctl :ret value))
      value))

(defcfun ("ioctl" %ioctl) ioctl-result
  (fd :int)
  (request :unsigned-long)
  (winsz (:pointer (:struct winsize))))

(defun term-size ()
  "Return terminal size information.
The following are returned in a property list row, col, xpixels,
ypixels.  See `man 2 ioctl` for more inforamtion.  Note ioctl and thus
`term-size' will throw an error of type IOCTL when called from SLIME."
  (restart-case
      (with-foreign-object (wnsz '(:struct winsize))
        (%ioctl STDOUT-FILENO TIOCGWINSZ wnsz)
        (with-foreign-slots ((row col xpixel ypixel) wnsz (:struct winsize))
          `(:row ,row
                 :col ,col
                 :xpixel ,xpixel
                 :ypixel ,ypixel)))
    (return-no-extent-term ()
      :report "Return info for a terminal with no extent."
      '(:row 0
        :col 0
        :xpixel 0
        :ypixel 0))))


;;; Color and control sequences
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *view-stream* t
    "Dynamically bind to use modify.")

  (defmacro define-multiple-constants (&body specs)
    `(progn ,@(mapcar (lambda (spec)
                        (destructuring-bind (name value documentation) spec
                          `(define-constant ,name ,value :test 'equalp
                                            :documentation ,documentation)))
                      specs))))

(define-multiple-constants
  (+set-G1+      #?"\x1b)0"   "Set G1 for box drawing")
  (+reset-G1+    #?"\x1b)B"   "Reset G1 to ASCII")
  (+b-start+     #?"\x0e"     "Enter G1 drawing mode")
  (+b-stop+      #?"\x0f"     "Leave G1 drawing mode")
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
  (+term-home+   #?"\x1b[H"     "Set terminal back to home (top left).")
  (+term-clear+  #?"\x1b[H[2J" "Clear terminal.")
  (+ceol+        #?"\x1b[0K"    "Clear to end of line.")
  (+cursor-hide+ #?"\x1b[?25l"  "Hide the cursor.")
  (+cursor-show+ #?"\x1b[?25h"  "Show the cursor.")
  ;; Colors
  (+color-BLK+   #?"\x1b[0;30m" "Color BLK.")
  (+color-RED+   #?"\x1b[0;31m" "Color RED.")
  (+color-GRN+   #?"\x1b[0;32m" "Color GRN.")
  (+color-BRN+   #?"\x1b[0;33m" "Color BRN.")
  (+color-BLU+   #?"\x1b[0;34m" "Color BLU.")
  (+color-MGN+   #?"\x1b[0;35m" "Color MGN.")
  (+color-CYA+   #?"\x1b[0;36m" "Color CYA.")
  (+color-NOR+   #?"\x1b[0;37m" "Color NOR.")
  (+color-GRA+   #?"\x1b[1;30m" "Color GRA.")
  (+color-LRD+   #?"\x1b[1;31m" "Color LRD.")
  (+color-LGN+   #?"\x1b[1;32m" "Color LGN.")
  (+color-YEL+   #?"\x1b[1;33m" "Color YEL.")
  (+color-LBL+   #?"\x1b[1;34m" "Color LBL.")
  (+color-PIN+   #?"\x1b[1;35m" "Color PIN.")
  (+color-LCY+   #?"\x1b[1;36m" "Color LCY.")
  (+color-BRI+   #?"\x1b[1;37m" "Color BRI.")
  (+color-RST+   #?"\x1b[0m"    "Color RST."))


;;; Utility functions

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
     (format ,*view-stream* "~a" +reset-G1+)))

(defmacro with-color-printing (color &rest body)
  `(unwind-protect
        (progn (format ,*view-stream* "~a" ,color) ,@body)
     (format ,*view-stream* "~a" +color-RST+)))

(defun string-output-stream-p (stream)
  (typep stream
         #+sbcl 'sb-impl::string-output-stream
         #+ccl  'ccl:string-output-stream
         #- (or sbcl ccl)
         (error "`string-output-stream-p' only supported for SBCL and CCL")))
