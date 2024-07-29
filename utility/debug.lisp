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
   :note-level
   :*note-out*
   :note
   :with-warnings-as-notes
   :trace-memory
   :define-symbolic-note-level))
(in-package :software-evolution-library/utility/debug)
(in-readtable :curry-compose-reader-macros)

(eval-always
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

(deftype note-level ()
  '(and fixnum unsigned-byte))

(defvar *note-level* 0 "Enables execution notes.")
(declaim (note-level *note-level*))

(defvar *note-out*
  (list (make-synonym-stream '*standard-output*))
  "Targets of notation.")

(eval-always
  (defvar *note-level-names* (make-hash-table)))

(defmacro define-symbolic-note-level (name level)
  "Register LEVEL as a symbolic note level."
  `(setf (gethash (assure symbol ',name) *note-level-names*)
         (assure note-level
           ,level)))

(define-condition unknown-symbolic-note-level (error)
  ((level :initarg :level :type symbol))
  (:report (lambda (c s)
             (with-slots (level) c
               (format s "Unknown symbolic note level: ~s" level)))))

(-> numeric-note-level ((or note-level symbol)) note-level)
(defun numeric-note-level (level)
  (assure note-level
    (etypecase level
      (note-level level)
      (symbol
       (or (gethash level *note-level-names*)
           (error 'unknown-symbolic-note-level
                  :level level))))))

(-> note-level () note-level)
(defun note-level ()
  "Get the current note level (as an integer)."
  *note-level*)

(defun (setf note-level) (level)
  (setf *note-level* (numeric-note-level level)))

(define-note-level-name :critical 0)
(define-note-level-name :fatal 0)
(define-note-level-name :error 1)
(define-note-level-name :warning 2)
(define-note-level-name :info 3)
(define-note-level-name :information 3)
(define-note-level-name :debug 4)
(define-note-level-name :trace 5)
(define-note-level-name :everything most-positive-fixnum)
(define-note-level-name :all most-positive-fixnum)

(defun print-time (&optional (out t))
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignorable day-of-week dst-p tz))
    (format out "~d.~2,'0d.~2,'0d.~2,'0d.~2,'0d.~2,'0d"
            year month date hour minute second)))

(-> note ((or note-level symbol) (or function string) &rest t)
    null)
(defun note (level format-control &rest format-args)
  "When LEVEL is <= `*note-level*', log to `*note-out*'.
LEVEL may be a symbolic level defined with `define-note-level-name'."
  (let ((level (numeric-note-level level)))
    (when (<= level *note-level*)
      (let ((*print-pretty* nil))
        (mapcar
         #'finish-output
         (mapc
          (lambda (stream)
            (format stream "~&;;~a: ~?~%"
                    (print-time nil)
                    format-control
                    format-args))
          *note-out*)))))
  ;; Always return nil.
  nil)

(define-compiler-macro note (&whole call level format-control &rest format-args)
  ;; Check symbolic note levels at compile time.
  (when (symbolp level)
    (handler-case (numeric-note-level level)
      ;; Convert the error into a style warning.
      (unknown-symbolic-note-level (e)
        (simple-style-warning "~a" e))))
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
