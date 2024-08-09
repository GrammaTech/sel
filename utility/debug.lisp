;;;; Debugging helpers
;;;
;;; Functions useful for debugging lisp code.
;;;
;;; The @code{note} and @code{note-level} functions and @code{*note-out*}
;;; variable provide a basic logging framework:
;;;
;;;     (note 3 "Something happened: ~a" thing)
;;;
;;; Notes will only be printed if the level supplied is <= to
;;; @code{*note-level*}:
;;;
;;;     (setf (note-level) 2)
;;;     (note 3 "I won't be seen")
;;;
;;; By default, notes go to @code{*note-out*}. You can redirect them
;;; by rebinding (or setting) the variable:
;;;
;;;     (with-output-to-file (*note-out* #p"saved-notes.txt")
;;;       ...)
;;;
;;; Note levels can also be symbolic:
;;;
;;;     (setf (note-level) :info)
;;;     (note :info "Informational note")
;;;
;;; Symbolic note levels will be familiar from other logging
;;; frameworks: @code{FATAL}, @code{ERROR}, @code{WARN}, @code{DEBUG},
;;; @code{TRACE}, @{ALL}, and variants thereof.
;;;
;;; One pitfall of logging is that getting the information to log can
;;; be expensive:
;;;
;;;     (note :debug "What went wrong: ~a" (generate-expensive-report))
;;;
;;; In this case @code{generate-expensive-report} gets called whether
;;; or not any logging takes place. You can use `lazy-note' to avoid
;;; this:
;;;
;;;     (lazy-note :debug "Problem: ~a" (generate-expensive-report))
;;;
;;; Using @code{lazy-note}, @code{generate-expensive-report} (or any
;;; other argument) will only be evaluated at the appropriate logging
;;; level.

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
   :lazy-note
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

(define-symbolic-note-level :critical 0)
(define-symbolic-note-level :fatal 0)
(define-symbolic-note-level :error 1)
(define-symbolic-note-level :warn 2)
(define-symbolic-note-level :warning 2)
(define-symbolic-note-level :info 3)
(define-symbolic-note-level :information 3)
(define-symbolic-note-level :debug 4)
(define-symbolic-note-level :trace 5)
(define-symbolic-note-level :everything most-positive-fixnum)
(define-symbolic-note-level :all most-positive-fixnum)

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

(eval-always
  (defun check-level (level env)
    "Warn if a symbolic note level is not correct."
    (multiple-value-bind (level constant?)
        (eval-if-constant level env)
      (when (and constant? (symbolp level))
        (handler-case (numeric-note-level level)
          ;; Convert the error into a style warning.
          (unknown-symbolic-note-level (e)
            (simple-style-warning "~a" e)))))))

(define-compiler-macro note (&whole call
                                    level format-control
                                    &rest format-args
                                    &environment env)
  (check-level level env)
  (if (stringp format-control)
      `(note ,level (formatter ,format-control) ,@format-args)
      call))

(defmacro lazy-note (level format-control &rest format-args &environment env)
  "Like `note', but only evaluates its arguments if the message would be
printed.

This is a useful alternative to `note' when computing the arguments
would be expensive."
  (check-level level env)
  (once-only (level)
    `(when (<= (numeric-note-level ,level) *note-level*)
       (note ,level ,format-control ,@format-args))))

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
