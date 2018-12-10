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
  (:import-from :software-evolution-library/software/ast :ast-hash :ast-text)
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
  (let ((position (eclector.parse-result:source-position client input-stream)))
    (if-let ((cached (getcache position client)))
      (progn
        (assert (eql (start cached) position))
        (loop :repeat (- (end cached) position) :do (read-char input-stream))
        (values (expression cached) cached))
      (progn
        (multiple-value-bind (expression parse-result) (call-next-method)
          (setf (getcache position client) parse-result)
          (values expression parse-result))))))

(defun read-file-forms+ (file &rest args)
  "Read forms and comments from FILE returned interleaved in a tree."
  (apply #'read-forms+ (file-to-string file) args))

(defun read-forms+ (string &key count)
  (check-type count (or null integer))
  (let ((*string* string)
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
      (w/space (with-input-from-string (input string)
                 (loop :with eof = '#:eof
                    :for n :from 0
                    :for form = (if (and count (>= n count))
                                    eof
                                    (eclector.parse-result:read input nil eof))
                    :until (eq form eof) :collect form))
               0 (length string)))))

(defun walk-forms+ (function forms)
  (mapcar (lambda (form)
            (etypecase form
              (skipped-input-result (funcall function form))
              (expression-result (walk-forms+ function (children form)))))
          forms))

(defun write-file-forms+ (forms)
  "Re-prints the exact output as the original file."
  (with-open-file (out "/tmp/out.lisp" :direction :output :if-exists :supersede)
    (write-stream-forms+ forms out)))

(defun write-stream-forms+ (forms stream)
  "Re-prints the exact output as the original file, to STREAM"
  (walk-forms+  [{format stream "~a"} #'ast-text] forms))

(defun write-string-forms+ (forms)
  "Re-prints the exact output as the original file, to a string, which is returned"
  (with-output-to-string (s) (write-stream-forms+ forms s)))



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

(defmethod ast-hash ((result skipped-input-result))
  (ast-hash (source result)))

(defmethod ast-hash ((result expression-result))
  (ast-hash (cons :lisp-expression (mapcar #'ast-hash (children result)))))

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
  t)

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

(defmethod ast-diff ((a skipped-input-result) (b skipped-input-result))
  (ast-diff (source a) (source b)))

(defmethod ast-diff ((a expression-result) (b expression-result))
  (ast-diff (children a) (children b)))

(defmethod ast-patch ((a skipped-input-result) script &key &allow-other-keys)
  (let* ((str (source a))
	 (new-str (ast-patch str script)))
    (if (equal str new-str)
	a
	(make-instance 'skipped-input-result
		       :start 0
		       :end (length new-str)
		       :string-pointer new-str
		       :reason (reason a)))))

(defmethod ast-patch ((a expression-result) script &key &allow-other-keys)
  (let* ((c (children a))
	 (new-c (ast-patch c script)))
    (if (equal c new-c)
	a
	(let ((src (mapconcat #'source new-c ""))
	      (form (iter (for child in new-c)
			  (when (typep child 'expression-result)
			    (collect (expression child))))))
	  ;; Special handling for dotted lists
	  ;; These are indicated by having the next
	  ;; to last element of the list be a "CONSING DOT"
	  (let ((len (length form))
		(consing-dot eclector.reader::*consing-dot*))
	    (when (>= len 3)
	      (let ((last3 (last form 3)))
		(when (eql (cadr last3) consing-dot)
		  ;; FORM was newly allocated, so we can
		  ;; destructively modify it
		  (setf (cdr last3) (caddr last3))))))
	  (make-instance 'expression-result
			 :start 0
			 :end (length src)
			 :string-pointer src
			 :children new-c
			 :expression form)))))


;;; Command-line interface to lisp differencing.
(setf *note-out* *error-output*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +command-line-options+
    (append +common-command-line-options+
            `((("raw" #\r) :type boolean :optional t
               :documentation "output diff in raw Sexp (default is as text)")
              (("no-color" #\C) :type boolean :optional t
               :documentation "inhibit color printing")))))

(declaim (special *lisp-forms1* *lisp-forms2* *diff*))

(define-command lisp-diff (file1 file2 &spec +command-line-options+)
  "Compare Lisp source in FILE1 and FILE2 by AST."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose))
  (when help (show-help-for-lisp-diff))
  (unless (every #'resolve-file (list file1 file2))
    (exit-command lisp-diff 2 (error "Missing file.")))
  ;; Create the diff.
  (let* ((forms1 (read-file-forms+ file1))
	 (forms2 (read-file-forms+ file2))
	 (diff (ast-diff forms1 forms2)))
    (setf *lisp-forms1* forms1
	  *lisp-forms2* forms2
	  *diff* diff)
    ;; Print according to the RAW option.
    (if raw
        (writeln (ast-diff-elide-same diff) :readably t)
        (if no-color
            (print-diff diff :no-color t)
            (print-diff diff)))
    ;; Only exit with 0 if the two inputs match.
    (unless uiop/image:*lisp-interaction*
      (quit (if (every [{eql :same} #'car] diff) 0 1)))
    diff))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +merge-command-line-options+
    (append +common-command-line-options+
	    `())))

(define-command lisp-merge (file1 file2 file3 output-file
				  &spec +merge-command-line-options+)
  "Merge changes from file1->file2 and file1->file3 to produce output-file"
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose))
  (when help (show-help-for-lisp-merge))
  (unless (every #'resolve-file (list file1 file2 file3))
    (exit-command lisp-merge 2 (error "Missing file.")))
  (let ((forms1 (read-file-forms+ file1))
	(forms2 (read-file-forms+ file2))
	(forms3 (read-file-forms+ file3)))
    (let ((result (converge forms1 forms2 forms3)))
      (prog1 (output-lisp-forms result output-file)
	(unless uiop/image:*lisp-interaction* (quit 0))))))

(defun output-lisp-forms (forms file)
  "Dump a list of Lisp eclector objects to FILE.  Returns
   list of strings from each object"
  (with-open-file (s file :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (mapcar (lambda (form)
	      (let ((str (source form)))
		(princ str s)
		str))
	    forms)))
