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
;;; See Eclector/code/parse-result/second-climacs-test.lisp in
;;; eclector-parse-result-protocol-2 branch of Eclector.
;;;
(defpackage :software-evolution-library/lisp-diff
  (:nicknames :sel/lisp-diff)
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
        :eclector.parse-result)
  (:shadowing-import-from
   :uiop :getenv :directory-exists-p :copy-file :appendf :parse-body
   :ensure-list :simple-style-warning :ensure-gethash :ensure-function
   :if-let :emptyp :featurep)
  (:shadowing-import-from :eclector.parse-result :read)
  (:shadowing-import-from :software-evolution-library/view
                          +color-RED+ +color-GRN+ +color-RST+))
(in-package :software-evolution-library/lisp-diff)
(in-readtable :curry-compose-reader-macros)


;;; Differencing adapted from second-climacs-test.lisp in
;;; wip-parse-result-protocol-2 branch of eclector.
(defvar *string*)

(defclass result ()
  ((start :initarg :start
          :reader  start)
   (end   :initarg :end
          :reader  end)
   (string-pointer :initarg :string-pointer
                   :initform *string*
                   :reader  string-pointer)))

(defclass expression-result (result)
  ((expression :initarg :expression
               :reader  expression)
   (children   :initarg :children
               :accessor children)))

(defclass skipped-input-result (result)
  ((reason :initarg :reason
           :reader  reason)))

(defclass second-climacs (parse-result-mixin)
  ((cache :reader   cache
          :initform (make-hash-table :test #'eql))))

(defmethod getcache ((position t) (client second-climacs))
  (gethash position (cache client)))

(defmethod (setf getcache) ((new-value t) (position t) (client second-climacs))
  (setf (gethash position (cache client)) new-value))

(defmethod make-expression-result
    ((client second-climacs) (result t) (children t) (source cons))
  (make-instance 'expression-result
    :expression result
    :children children
    :start (car source)
    :end (cdr source)))

(defmethod make-skipped-input-result
    ((client second-climacs) stream reason source)
  (make-instance 'skipped-input-result
    :reason reason :start (car source) :end (cdr source)))

(defmethod eclector.reader:read-common :around
    ((client second-climacs) input-stream eof-error-p eof-value)
  (let ((position (source-position client input-stream)))
    (if-let ((cached (getcache position client)))
      (progn
        (assert (eql (start cached) position))
        (loop :repeat (- (end cached) position) :do (read-char input-stream))
        (values (expression cached) cached))
      (progn
        (multiple-value-bind (expression parse-result) (call-next-method)
          (setf (getcache position client) parse-result)
          (values expression parse-result))))))

(defun read-file-forms+ (file &key count)
  "Read forms and comments from FILE returned interleaved in a tree."
  (check-type count (or null integer))
  (let ((*string* (file-to-string file))
        (eclector.reader:*client* (make-instance 'second-climacs)))
    (labels
        ((make-space (start end)
           (when (< start end)
             (list (make-instance 'skipped-input-result
                     :start start :end end :reason 'whitespace))))
         (w/space (tree from to)
           (etypecase tree
             (list
              (append
               (iter (for subtree in tree)
                     (appending (make-space from (start subtree)))
                     (appending (w/space subtree (start subtree) (end subtree)))
                     (setf from (end subtree)))
               (make-space from to)))
             (result
              (when (subtypep (type-of tree) 'expression-result)
                (setf (children tree)
                      (w/space
                       (children tree) (start tree) (end tree))))
              (append
               (make-space from (start tree))
               (list tree)
               (make-space (end tree) to))))))
      (w/space (with-open-file (input file)
                 (loop :with eof = '#:eof
                    :for n :from 0
                    :for form = (if (and count (>= n count))
                                    eof
                                    (read input nil eof))
                    :until (eq form eof) :collect form))
               0 (length *string*)))))


;;; Interface to ast-diff for lisp source trees.
(defmethod source ((result result))
  (subseq (string-pointer result) (start result) (end result)))

(defmethod ast-equal-p ((ast-a result) (ast-b result))
  (ast-equal-p (source ast-a) (source ast-b)))

(defmethod ast-cost ((result expression-result))
  ;; NOTE: Interesting question here about if we want comments to add
  ;;       to the weight of a source expression.
  (ast-cost (expression result)))

(defmethod ast-cost ((result skipped-input-result)) 1)

(defmethod ast-can-recurse ((ast-a expression-result) (ast-b expression-result))
  (and (children ast-a) (children ast-b)))

(defmethod ast-can-recurse
    ((ast-a skipped-input-result) (ast-b expression-result))
  nil)

(defmethod ast-can-recurse
    ((ast-a expression-result) (ast-b skipped-input-result))
  nil)

(defmethod ast-can-recurse
    ((ast-a skipped-input-result) (ast-b skipped-input-result))
  nil)

(defmethod ast-on-recurse ((ast expression-result))
  (children ast))

(defmethod ast-un-recurse ((ast expression-result) children)
  (make-instance 'expression-result
    :start (start (car children))
    :end (end (lastcar children))
    :expression (mapcar #'expression children)
    :children children
    :string-pointer (string-pointer (car children))))

(defmethod ast-text ((result result))
  (source result))


;;; Command-line interface to lisp differencing.
(setf *note-out* *error-output*)
(defun handle-set-verbose-argument (level)
  (when (>= level 4) (setf *shell-debug* t))
  (setf *note-level* level))

(defun run-lisp-diff (&aux (self (argv0)) (args *command-line-arguments*)
                        raw flags (colorp t))
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
 -C, --no-color            inhibit color printing
 -v, --verbose [NUM]       verbosity level 0-4

Built with ~a version ~a.~%"
              self (lisp-implementation-type) (lisp-implementation-version))
      (quit))
    ;; Argument handling and checking.
    (getopts (args :unknown :return)
      ("-r" "--raw" (setf raw t))
      ("-C" "--no-color" (setf colorp nil))
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
           (ast-diff (read-file-forms+ (car args))
                     (read-file-forms+ (second args)))))
      ;; Print according to the RAW option.
      (if raw
          (writeln (ast-diff-elide-same diff) :readably t)
          (if colorp
              (print-diff diff *standard-output*
                          (format nil "~a[-" +color-RED+)
                          (format nil "-]~a" +color-RST+)
                          (format nil "~a{+" +color-GRN+)
                          (format nil "+}~a" +color-RST+))
              (print-diff diff)))
      ;; Only exit with 0 if the two inputs match.
      (quit (if (every [{eql :same} #'car] diff) 0 1)))))
