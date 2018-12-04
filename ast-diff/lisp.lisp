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
        :software-evolution-library
        :software-evolution-library/command-line
        :software-evolution-library/utility
        :software-evolution-library/ast-diff/ast-diff
        :software-evolution-library/software/lisp
        :eclector.parse-result)
  (:shadowing-import-from
   :uiop :getenv :directory-exists-p :copy-file :appendf :parse-body
   :ensure-list :simple-style-warning :ensure-gethash :ensure-function
   :if-let :emptyp :featurep)
  (:shadowing-import-from :eclector.parse-result :read)
  (:shadowing-import-from :software-evolution-library/view
                          +color-RED+ +color-GRN+ +color-RST+))
(in-package :software-evolution-library/ast-diff/lisp)
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

(defun walk-forms+ (function forms)
  (mapcar (lambda (form)
            (etypecase form
              (skipped-input-result (funcall function form))
              (expression-result (walk-forms+ function (children form)))))
          forms))

(defun write-file-forms+ (forms)
  "Re-prints the exact output as the original file."
  (with-open-file (out "/tmp/out.lisp" :direction :output :if-exists :supersede)
    (walk-forms+ [{format out "~a"} #'ast-text] forms)))


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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +command-line-options+
    (append +common-command-line-options+
            `((("raw" #\r) :type boolean :optional t
               :documentation "output diff in raw Sexp (default is as text)")
              (("no-color" #\C) :type boolean :optional t
               :documentation "inhibit color printing")))))

(define-command lisp-diff (file1 file2 &spec +command-line-options+)
  "Compare Lisp source in FILE1 and FILE2 by AST."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose))
  (when help (show-help-for-lisp-diff))
  (when (some #'identity
              (mapcar (lambda (file)
                        (unless (probe-file file)
                          (format *error-output*
                                  "~a: No such file or directory~%"
                                  file)
                          t))
                      (list file1 file2)))
    (quit 2))
  ;; Create the diff.
  (let ((diff
         (ast-diff (read-file-forms+ file1)
                   (read-file-forms+ file2))))
    ;; Print according to the RAW option.
    (if raw
        (writeln (ast-diff-elide-same diff) :readably t)
        (if no-color
            (print-diff diff)
            (print-diff diff *standard-output*
                        (format nil "~a[-" +color-RED+)
                        (format nil "-]~a" +color-RST+)
                        (format nil "~a{+" +color-GRN+)
                        (format nil "+}~a" +color-RST+))))
    ;; Only exit with 0 if the two inputs match.
    (if uiop/image:*lisp-interaction*
        (quit (if (every [{eql :same} #'car] diff) 0 1))
        diff)))
