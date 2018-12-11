;;; lisp.lisp --- software representation of lisp code
;;;
;;; See @code{Eclector/code/parse-result/second-climacs-test.lisp}.
;;; Note that this requires the @code{wip-parse-result-protocol-2}
;;; branch of Eclector (see
;;; @url{https://github.com:robert-strandh/Eclector}).
;;;
;;; @texi{lisp}
(defpackage :software-evolution-library/software/lisp
  (:nicknames :sel/software/lisp :sel/sw/lisp)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :metabang-bind
        :iterate
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/ast
        :software-evolution-library/software/simple
        :software-evolution-library/ast-diff/ast-diff
	:eclector.parse-result)
  (:shadowing-import-from :eclector.parse-result :read)
  (:import-from :uiop :nest)
  (:export :lisp))
(in-package :software-evolution-library/software/lisp)
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

(defmethod print-object ((obj expression-result) stream)
  (nest (with-slots (start end string-pointer expression children) obj)
        (if *print-readably*
            (format stream "~S" `(make-instance 'expression-result
                                   :start ,start
                                   :end ,end
                                   :string-pointer *string*
                                   :expression ,expression
                                   :children (list ,@children))))
        (print-unreadable-object (obj stream :type t))
        (format stream ":EXPRESSION ~a :CHILDREN ~S" expression children)))

(defclass skipped-input-result (result)
  ((reason :initarg :reason
           :reader  reason)))

(defmethod print-object ((obj skipped-input-result) stream &aux (max-length 8))
  (nest (with-slots (start end string-pointer reason) obj)
        (if *print-readably*
            (format stream "~S" `(make-instance 'skipped-input-result
                                   :start ,start
                                   :end ,end
                                   :string-pointer *string*
                                   :reason ,reason)))
        (print-unreadable-object (obj stream :type t))
        (format stream ":REASON ~a :TEXT ~S" reason)
        (if (> (- end start) (- max-length 3))
            (concatenate
             'string
             (subseq string-pointer start (+ start (- max-length 3)))
             "...")
            (subseq string-pointer start end))))

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

(defun write-stream-forms+ (forms stream)
  "Write the original source text of FORMS to STREAM."
  (walk-forms+  [{format stream "~a"} #'ast-text] forms))

(defun write-string-forms+ (forms)
  "Write the original source text of FORMS to a string."
  (with-output-to-string (s) (write-stream-forms+ forms s)))


;;; Lisp software object
(define-software lisp (simple)
  ((genome :initarg :genome :accessor genome :initform nil :copier copy-tree))
  (:documentation "Common Lisp source represented naturally as lists of code."))

(defmethod from-file ((lisp lisp) file)
  (with-open-file (in file)
    (setf (genome lisp) (read-forms+ (file-to-string file))))
  lisp)

(declaim (inline genome-string))
(defmethod genome-string ((lisp lisp) &optional stream)
  (if stream
      (write-stream-forms+ (genome lisp) stream)
      (write-string-forms+ (genome lisp))))

(defmethod to-file ((lisp lisp) path)
  (with-open-file (out path :direction :output :if-exists :supersede)
    (write-stream-forms+ (genome lisp) out)))

(defmethod size ((lisp lisp) &aux (size 0))
  (walk-forms+ (lambda (_) (declare (ignorable _)) (incf size)) (genome lisp))
  size)


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

(defmethod ast-diff ((a lisp) (b lisp))
  (ast-diff (genome a) (genome b)))

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
