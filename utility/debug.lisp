;;;; Debugging helpers
;;;
;;; Functions useful for debugging lisp code.  Of particular note are
;;; the `note' functions and associated `*note-level*' and
;;; `*note-out*' variables which provide a basic logging framework.
;;;
;;; @texi{debugging}
(defpackage software-evolution-library/utility/debug
  (:nicknames :sel/utility/debug)
  (:use :gt/full)
  (:export
   :*compile-w/tracing*
   :traced
   :*note-level*
   :*note-out*
   :note
   :with-warnings-as-notes
   :trace-memory))
(in-package :software-evolution-library/utility/debug)
(in-readtable :curry-compose-reader-macros)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *compile-w/tracing* nil
    "Controls compilation of tracing information with the `traced' macro."))

(defmacro traced ((fn &rest args))
  "Trace wrapped function call when `*compile-w/tracing*' is non-nil.
This is useful for `flet' and `labels' functions which can't be traced
with `cl-user:trace'."
  (if *compile-w/tracing*
      (let ((result-sym (gensym)))
        `(progn (format t "  X: ~S ~S~%" ',fn (list ,@args))
                (let ((,result-sym (,fn ,@args)))
                  (format t ,(format nil "  X: ~a returned~~%      ~~S~~%" fn)
                          ,result-sym)
                  ,result-sym)))
      `(,fn ,@args)))

(defvar *note-level* 0 "Enables execution notes.")
(defvar *note-out*
  (list (make-synonym-stream '*standard-output*))
  "Targets of notation.")

(defun print-time (&optional (out t))
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignorable day-of-week dst-p tz))
    (format out "~d.~2,'0d.~2,'0d.~2,'0d.~2,'0d.~2,'0d"
            year month date hour minute second)))

(defun note (level format-control &rest format-args)
  (when (>= *note-level* level)
    (let ((*print-pretty* nil))
      (mapcar
       #'finish-output
       (mapc
        (lambda (stream)
          (format stream "~&;;~a: ~?~%"
                  (print-time nil)
                  format-control
                  format-args))
        *note-out*))))
  ;; Always return nil.
  nil)

(define-compiler-macro note (&whole call level format-control &rest format-args)
  (if (stringp format-control)
      `(note ,level (formatter ,format-control) ,@format-args)
      call))

(defmacro with-warnings-as-notes (note-level &body forms)
  `(handler-bind ((warning (lambda (c)
                             (note ,note-level "~&~A~%" c)
                             (invoke-restart 'muffle-warning))))
     ,@forms))

#+sbcl
(defun trace-memory ()
  (when (>= *note-level* 2)
    (let ((percentage-used (/ (sb-vm::dynamic-usage)
                              (sb-ext::dynamic-space-size))))
      (if (>= *note-level* 4)
        (note 4 "~a ~,2f~%" (second (sb-debug:list-backtrace))
                            percentage-used)
        (when (>= percentage-used 0.5)
          (note 2 "~a ~,2f~%" (second (sb-debug:list-backtrace))
                              percentage-used))))))
