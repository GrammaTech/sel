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
#-windows
(defcstruct winsize (row :short) (col :short) (xpixel :short) (ypixel :short))

#-windows
(define-foreign-type null-pointer-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser null-pointer))

#-windows
(defgeneric translate-to-foreign (value type)
  (:method (value (type null-pointer-type))
    (cond
      ((null value) (null-pointer))
      ((null-pointer-p value) value)
      (t (error "~A is not a null pointer." value)))))

#-windows
(define-foreign-type ioctl-result-type ()
  ()
  (:actual-type :int)
  (:simple-parser ioctl-result))

#-windows
(define-condition ioctl (error)
  ((ret :initarg :ret :initform nil :reader ret))
  (:report (lambda (condition stream)
             (format stream "IOCTL call failed with return value ~d"
                     (ret condition)))))
#-windows
(defgeneric translate-from-foreign (value type)
  (:method (value (type ioctl-result-type))
    (if (minusp value)
        (make-condition 'ioctl :ret value)
        value)))

#-windows
(defcfun ("ioctl" %ioctl) ioctl-result
  (fd :int)
  (request :int)
  (winsz :pointer))

#-windows
(defun term-size ()
  "Return terminal size information.
The following are returned as separate values; rows, columns,
x-pixels, y-pixels.  Note, this may throw an error when called from
SLIME."
  (with-foreign-object (wnsz '(:struct winsize))
    ;; 0 == STDIN_FILENO
    ;; 21523 == TIOCGWINSZ
    (%ioctl 0 21523 wnsz)
    (with-foreign-slots ((row col xpixel ypixel) wnsz (:struct winsize))
      (values row col xpixel ypixel))))
