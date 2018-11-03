;;; lisp-diff.lisp --- Calculate and render Lisp AST diffs at the command line
;;;
;;; The following git configuration will register lisp-diff as a tool
;;; to be used with @code{git difftool} (see
;;; @url{https://git-scm.com/docs/git-difftool}).
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
;;; See @code{Eclector/code/parse-result/second-climacs-test.lisp}.
;;; Note that this requires the @code{wip-parse-result-protocol-2}
;;; branch of Eclector (see
;;; @url{https://github.com:robert-strandh/Eclector}).
;;;
;;; @texi{lisp-diff}
(defpackage :software-evolution-library/ast-diff/lisp
  (:nicknames :sel/ast-diff/lisp)
  (:use :common-lisp
        :alexandria
        :arrow-macros
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
   :if-let :emptyp :featurep)
  (:shadowing-import-from :concrete-syntax-tree
                          :first :rest :source :cst :cons-cst :atom-cst :raw)
  (:export :run-lisp-diff))
(in-package :software-evolution-library/ast-diff/lisp)
(in-readtable :curry-compose-reader-macros)


;;; Build up a concrete syntax tree which includes comment regions.
(defvar *comments*)
(defvar *string*)

(defclass comment ()
  ((kind :initform nil :initarg :kind :accessor kind)
   (source :initform nil :initarg :source :accessor source)
   (raw :initform nil :initarg :raw :accessor raw)))

(defmethod print-object ((object comment) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "raw: ~s" (raw object))))

(defclass whitespace ()
  ((source :initform nil :initarg :source :accessor source)
   (raw :initform nil :initarg :raw :accessor raw)))

(defmethod print-object ((object whitespace) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "source: ~s" (source object))))

(defmethod record-skipped-input (client stream reason source)
  (declare (ignorable client stream))
  (push (make-instance 'comment
          :kind reason
          :source source
          :raw (subseq *string* (car source) (cdr source)))
        *comments*))

(defun read-file-forms-and-extras (file &key count)
  "Read forms and comments from FILE returned interleaved in a tree."
  (check-type count (or null integer))
  (let ((previous 0) *comments* holding)
    (labels ((starts (object)
               (etypecase object
                 ((or atom-cst comment) (car (source object)))
                 (cons (or (starts (car object))
                           (starts (cdr object))))))
             (ends (object)
               (etypecase object
                 ((or atom-cst comment) (cdr (source object)))
                 (cons (or (ends (car object))
                           (ends (cdr object))))))
             (cst-to-cons (object)
               (if (concrete-syntax-tree:consp object)
                   (progn
                     (unless (first object)
                       (setf (source (first object))
                             (source (rest object))))
                     (cons (cst-to-cons (first object))
                           (cst-to-cons (rest object))))
                   object))
             (thread (csts)
               ;; Thread *COMMENTS* through CSTS.
               (if (null *comments*)
                   csts
                   (when csts
                     (if (consp csts)
                         (if (< (starts *comments*) (starts csts))
                             (cons (pop *comments*) (thread csts))
                             (if (starts (car csts))
                                 (cons (thread (car csts))
                                       (thread (cdr csts)))
                                 (progn (push (car csts) holding)
                                        (thread (cdr csts)))))
                         (if (and (starts csts)
                                  (< (starts *comments*) (starts csts)))
                             (cons (pop *comments*) csts)
                             (let ((ret (if (null (raw csts)) nil csts)))
                               (if holding
                                   (cons (pop holding) ret)
                                   ret)))))))
             (add-whitespace (tree)
               (when tree
                 (let ((end (starts tree)))
                   (if (< previous end)
                       (cons (make-instance 'whitespace
                               :raw (subseq *string* previous end)
                               :source (cons previous end))
                             (progn (setf previous (ends tree))
                                    (add-whitespace tree)))
                       (if (consp tree)
                           (cons (car tree)
                                 (progn (setf previous (ends (car tree)))
                                        (add-whitespace (cdr tree))))
                           (progn (setf previous (ends tree)) nil)))))))
      (let ((*string* (file-to-string file)))
        (append
         (add-whitespace
          (append
           (thread (prog1
                       (with-open-file (input file)
                         (mapcar #'cst-to-cons
                                 (loop :with eof = '#:eof
                                    :for n :from 0
                                    :for form = (if (and count (>= n count))
                                                    eof
                                                    (cst-read input nil eof))
                                    :until (eq form eof) :collect form)))
                     (setf *comments* (nreverse *comments*))))
           *comments*))
         (when (< previous (length *string*))
           (list (make-instance 'whitespace
                   :raw (subseq *string* previous)
                   :source (cons previous (length *string*))))))))))


;;; Interface to ast-diff for lisp source trees.
(defmethod ast-equal-p ((ast-a atom-cst) (ast-b atom-cst))
  (ast-equal-p (raw ast-a) (raw ast-b)))

(defmethod ast-equal-p ((ast-a comment) (ast-b comment))
  (ast-equal-p (raw ast-a) (raw ast-b)))

(defmethod ast-equal-p ((ast-a whitespace) (ast-b whitespace))
  (ast-equal-p (raw ast-a) (raw ast-b)))

(defmethod ast-text ((ast atom-cst))
  (format nil "~a" (raw ast)))

(defmethod ast-text ((ast comment))
  (raw ast))

(defmethod ast-text ((ast whitespace))
  (raw ast))


;;; Command-line interface to lisp differencing.
(setf *note-out* *error-output*)
(defun handle-set-verbose-argument (level)
  (when (>= level 4) (setf *shell-debug* t))
  (setf *note-level* level))

(defun run-lisp-diff (&aux (self (argv0)) (args *command-line-arguments*)
                        raw flags)
  "Run a lisp difference on *COMMAND-LINE-ARGUMENTS*."
  (flet ((report (fmt &rest args)
           (apply #'format *error-output* (concatenate 'string "~a: " fmt)
                  self args)))
    (when (or (not args)
              (< (length args) 1)
              (string= (subseq (car args) 0 (min 2 (length (car args))))
                       "-h")
              (string= (subseq (car args) 0 (min 3 (length (car args))))
                       "--h"))
      (format t "Usage: ~a [OPTION]... FILES
Compare Lisp source FILES AST by AST.

Options:
 -r, --raw                 output diff in raw Sexp (default is as text)
 -v, --verbose [NUM]       verbosity level 0-4

Built with ~a version ~a.~%"
              self (lisp-implementation-type) (lisp-implementation-version))
      (quit))
    ;; Argument handling and checking.
    (getopts (args :unknown :return)
      ("-r" "--raw" (setf raw t))
      ("-v" "--verbose" (handle-set-verbose-argument
                         (parse-integer (pop args)))))
    (when (= (length args) 1)
      (report "missing operand after '~a'~%" (car args))
      (report "Try '~a --help' for more information." self)
      (quit 2))
    (when (> (length args) 2)
      (report "extra operand '~a'~%" (third args))
      (report "Try '~a --help' for more information." self)
      (quit 2))
    (when (some #'identity
                (mapcar (lambda (file)
                          (unless (probe-file file)
                            (format *error-output*
                                    "~a: ~a: No such file or directory~%"
                                    self (third args))
                            t))
                        args))
      (quit 2))
    ;; Setup clang-mutate options.
    (setf flags (list "-I" (pwd)))
    ;; Create the diff.
    (let ((diff
           (ast-diff (read-file-forms-and-extras (car args))
                     (read-file-forms-and-extras (second args)))))
      ;; Print according to the RAW option.
      (if raw
          (writeln (ast-diff-elide-same diff) :readably t)
          (print-diff diff))
      ;; Only exit with 0 if the two inputs match.
      (quit (if (every [{eql :same} #'car] diff) 0 1)))))
