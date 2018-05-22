;;; lisp-diff.lisp --- Calculate and render Lisp AST diffs at the command line
;;;
;;; The following git configuration will register lisp-diff as a tool
;;; to be used with @code{git difftool}.
;;;
;;;     # Set lisp-diff as the default difftool.
;;;     [diff]
;;;     	tool = lisp-diff
;;;
;;;     # Command-line to use with lisp-diff.  Piping through
;;;     # colordiff is optional but nice to highlight diffs.
;;;     [difftool "lisp-diff"]
;;;     	cmd = "lisp-diff $LOCAL $REMOTE|colordiff"
;;;
;;;     # Optionally don't prompt.
;;;     [difftool]
;;;     	prompt = false
;;;
(defpackage :software-evolution-library/lisp-diff
  (:nicknames :sel/lisp-diff)
  (:use :common-lisp
        :alexandria
        :cl-arrowz
        :named-readtables
        :curry-compose-reader-macros
        :metabang-bind
        :iterate
        :uiop
        :software-evolution-library/utility
        :software-evolution-library/ast-diff
        :eclector.concrete-syntax-tree)
  (:shadowing-import-from
   :uiop :getenv :directory-exists-p :copy-file :appendf :parse-body
   :ensure-list :simple-style-warning :ensure-gethash :ensure-function
   :if-let :emptyp :featurep))
(in-package :software-evolution-library/lisp-diff)
(in-readtable :curry-compose-reader-macros)


;;; Build up a concrete syntax tree which includes comment regions.
(defvar *extras*)

(defclass extra ()
  ((%source :initform nil :initarg :source :accessor source)
   (%reason :initform nil :initarg :reason :accessor reason)))

(defmethod print-object ((object extra) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "reason: ~s" (reason object))))

(defmethod source ((object concrete-syntax-tree:cst))
  (concrete-syntax-tree:source object))

(defmethod record-skipped-input (client stream reason source)
  (declare (ignorable client stream))
  (push (make-instance 'extra :reason reason :source source) *extras*))

(defun slurp-stream-forms-and-extras (input &key count)
  "Read the contents of the INPUT stream as a list of forms and unread input,
and return those forms.

If COUNT is null, read to the end of the stream;
if COUNT is an integer, stop after COUNT forms were read.

BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
  (check-type count (or null integer))
  (labels ((thread (csts extras)
             (cond
               ((and (null csts) (null extras)) nil)
               ((null csts) (cons (car extras) (thread csts (cdr extras))))
               ((null extras) (cons (car csts) (thread (cdr csts) extras)))
               ((< (car (source (car csts))) (car (source (car extras))))
                (cons (car csts) (thread (cdr csts) extras)))
               (:otherwise
                (cons (car extras) (thread csts (cdr extras)))))))
    (let (*extras*)
      (thread (loop :with eof = '#:eof
                 :for n :from 0
                 :for form = (if (and count (>= n count))
                                 eof
                                 (cst-read input nil eof))
                 :until (eq form eof) :collect form)
              (nreverse *extras*)))))

(defun read-file-forms-and-extras (file &rest keys &key count &allow-other-keys)
    "Open input FILE with option KEYS (except COUNT),
and read its contents as per SLURP-STREAM-FORMS with given COUNT.
BEWARE: be sure to use WITH-SAFE-IO-SYNTAX, or some variant thereof"
    (apply 'call-with-input-file file
           #'(lambda (input) (slurp-stream-forms-and-extras input :count count))
           (remove-plist-key :count keys)))
