;;;; Debugging helpers
;;;
;;; Functions useful for debugging lisp code.  Of particular note are
;;; the `note' functions and associated `*note-level*' and
;;; `*note-out*' variables which provide a basic logging framework.
;;;
;;; @texi{debugging}
(defpackage software-evolution-library/utility/debug
  (:nicknames :sel/util/debug)
  (:use :gt/full)
  (:import-from :diff :original-length :modified-length))
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
(defvar *note-out* '(t) "Targets of notation.")

(defun replace-stdout-in-note-targets (&optional (targets *note-out*))
  "Replace `t' which is a place holder for `*standard-output*'.
Ideally we would like to set the value of `*note-out*' to a list
holding `*standard-output*', however in compiled binaries the value of
`*standard-output*' changes each time the binary is launched.  So
instead we use `t' as a place-holder, and provide this function for
performing the replacement on the fly when `note' is called.  To
specify a particular value for `*standard-output*' the user may
replace `t' in the `*note-out*' list."
  (mapcar (lambda (s) (if (eq s t) *standard-output* s)) targets))

(defun print-time (&optional (out t))
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignorable day-of-week dst-p tz))
    (format out "~d.~2,'0d.~2,'0d.~2,'0d.~2,'0d.~2,'0d"
            year month date hour minute second)))

(defun note (level &rest format-args)
  (when (>= *note-level* level)
    (let ((*print-pretty* nil))
      (mapcar
       #'finish-output
       (mapc
        {write-sequence
         (concatenate 'string ";;" (print-time nil) ": "
                      (apply #'format nil format-args)
                      (list #\Newline))}
        (replace-stdout-in-note-targets)))))
  ;; Always return nil.
  nil)

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

;; adopted from a public domain lisp implementation copied from the

;; scheme implementation given at
;; http://en.wikipedia.org/wiki/Levenshtein_distance
(defun levenshtein-distance (s1 s2 &key (test #'char=) (key #'identity))
  (let* ((width (1+ (length s1)))
         (height (1+ (length s2)))
         (d (make-array (list height width))))
    (dotimes (x width)
      (setf (aref d 0 x) x))
    (dotimes (y height)
      (setf (aref d y 0) y))
    (dotimes (x (length s1))
      (dotimes (y (length s2))
        (setf (aref d (1+ y) (1+ x))
              (min (1+ (aref d y (1+ x)))
                   (1+ (aref d (1+ y) x))
                   (+ (aref d y x)
                      (if (funcall test
                                   (funcall key (aref s1 x))
                                   (funcall key (aref s2 y)))
                          0
                          1))))))
    (aref d (1- height) (1- width))))

;;; Diff computing
(defun diff-scalar (original-seq modified-seq)
  "Return an integer representing the diff size of two sequences
Sum O + |O - M| over each diff region.  O is the length of the
original diff region and M is the length of the modified diff
region."
  (reduce (lambda (acc region)
            (+ acc
               (ecase (type-of region)
                 (common-diff-region 0)
                 (modified-diff-region
                   (+ (original-length region)
                      (abs (- (original-length region)
                              (modified-length region))))))))
          (diff:compute-raw-seq-diff original-seq modified-seq)
          :initial-value 0))

;;; memory mapping, address -> LOC
(defun gdb-disassemble (phenome function)
  "Return the raw gdb disassembled code of FUNCTION in PHENOME."
  (shell "gdb --batch --eval-command=\"disassemble ~s\" ~s 2>/dev/null"
         function phenome))

(defun addrs (phenome function)
  "Return the numerical addresses of the lines (in order) of FUNCTION."
  (remove nil
    (mapcar
     (lambda (line)
       (multiple-value-bind (matchp strings)
           (scan-to-strings "[\\s]*0x([\\S]+)[\\s]*<([\\S]+)>:.*" line)
         (when matchp (parse-integer (aref strings 0) :radix 16))))
     (split-sequence #\Newline (gdb-disassemble phenome function)))))

(defun function-lines (lines)
  "Return the line numbers of the lines (in order) of FUNCTION.
LINES should be the output of the `lines' function on an ASM object."
  (loop :for line :in lines :as counter :from 0
     :for function = (register-groups-bind
                         (line-function) ("^\\$*([^\\.][\\S]+):" line)
                       line-function)
     :collect (or function counter)))

(defun calculate-addr-map (lines phenome genome)
  "Calculate a map of memory address to offsets in LINES.
LINES should be the output of the `lines' function on an ASM object,
PHENOME should be the phenome of an ASM object and GENOME should be
the genome of an ASM object."
  (let ((flines (function-lines lines))
        (genome (coerce genome 'vector))
        (map (make-hash-table)))
    (loop
       :for addrs :in (mapcar (lambda (func) (addrs phenome func))
                              (remove-if-not #'stringp flines))
       :for lines :in (cdr (mapcar
                            {remove-if
                             [{scan "^[\\s]*\\."} {aget :code} {aref genome}]}
                            (split-sequence-if #'stringp flines)))
       :do (mapc (lambda (addr line) (setf (gethash addr map) line))
                 addrs lines))
    map))
