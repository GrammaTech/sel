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
        :software-evolution-library/ast-diff/ast-diff
        :software-evolution-library/software/ast
        :software-evolution-library/software/source
        :software-evolution-library/software/parseable
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
                (when (children tree)
                  (setf (children tree)
                        (w/space
                         (children tree) (start tree) (end tree)))))
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

(defun walk-skipped-forms (function forms)
  (mapcar
   (lambda (form)
     (etypecase form
       (skipped-input-result (funcall function form))
       (expression-result (walk-skipped-forms function (children form)))))
   forms))

(defun walk-forms (function forms)
  (mapcar (lambda (form)
            (etypecase form
              (skipped-input-result (funcall function form))
              (expression-result (funcall function form)
                                 (walk-forms function (children form)))))
          forms))

(defun write-stream-forms+ (forms stream)
  "Write the original source text of FORMS to STREAM."
  (walk-skipped-forms  [{format stream "~a"} #'ast-text] forms))

(defun write-string-forms+ (forms)
  "Write the original source text of FORMS to a string."
  (with-output-to-string (s) (write-stream-forms+ forms s)))


;;; Lisp software object
(define-software lisp (parseable)
  ()
  (:documentation "Common Lisp source represented naturally as lists of code."))

(defmethod from-string ((lisp lisp) string)
  (setf (genome lisp) string)
  lisp)

(defmethod from-file ((lisp lisp) file)
  (from-string lisp (file-to-string file)))

(define-ast (lisp-ast (:conc-name ast)))

(defmethod print-object ((obj lisp-ast-node) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t)
        (format stream "~a" (ast-class obj)))))

(defmethod parse-asts ((obj lisp))
  (read-forms+ (genome obj)))

(defmethod update-asts ((obj lisp))
  (labels
      ((text (form)
         (subseq (string-pointer form) (start form) (end form)))
       (make-tree (form)
         (make-lisp-ast
          :node
          (etypecase form
            (skipped-input-result
             (make-lisp-ast-node
              :class :skipped
              :aux-data (list (cons :reason (reason form)))))
            (expression-result
             (make-lisp-ast-node
              :class :expression
              :aux-data (cons
                         (cons :expression (expression form))
                         (unless (children form)
                           (list (cons :text (text form))))))))
          :children
          (etypecase form
            (skipped-input-result
             (list (text form)))
            (expression-result
             (mapcar #'make-tree (children form)))))))
    (setf (ast-root obj)
          (make-lisp-ast :node (make-lisp-ast-node :class :top)
                         :children (mapcar #'make-tree (parse-asts obj)))
          (slot-value obj 'genome) nil)
    obj))

;;; NOTE: From here down we're swimming upstream.
;;;
;;; The AST class assumes that *all* source text is stored as a raw
;;; string somewhere in the AST.  It also assumes that if two ASTs
;;; have the same class and the same source text then they are equal.
;;; These assumptions are not true for the current lisp AST class.
;;; Violating these assumptions has meant customizing the following
;;; methods from ast.lisp.

(defmethod source-text ((ast lisp-ast))
  (if (ast-children ast)
      (nest (apply #'concatenate 'string)
            (mapcar #'source-text (ast-children ast)))
      (source-text (ast-node ast))))

(defmethod source-text ((node lisp-ast-node))
  (ecase (ast-class node)
    (:top (call-next-method))
    (:expression (aget :text (ast-aux-data node)))
    (:skipped ; We should never call `source-text' on skipped
              ; LISP-AST-NODES because skipped LISP-ASTs should always
              ; have children (specifically string children).
     (error "source-text should never be called on skipped"))))

(defmethod ast-equal-p ((ast-a lisp-ast) (ast-b lisp-ast))
  (and (eq (ast-class ast-a) (ast-class ast-b))
       (ecase (ast-class ast-a)
         (:top (and (eq (length (ast-children ast-a))
                        (length (ast-children ast-b)))
                    (every #'ast-equal-p (ast-children ast-a)
                           (ast-children ast-b))))
         (:skipped (and (eql (aget :reason (ast-aux-data ast-a))
                             (aget :reason (ast-aux-data ast-b)))
                        (ast-equal-p (ast-text ast-a) (ast-text ast-b))))
         (:expression
          (if (or (ast-children ast-a) (ast-children ast-b))
              (and (eq (length (ast-children ast-a))
                       (length (ast-children ast-b)))
                   (every #'ast-equal-p (ast-children ast-a) (ast-children ast-b)))
              (progn
                (assert (and (atom (aget :expression (ast-aux-data ast-a)))
                             (atom (aget :expression (ast-aux-data ast-b)))))
                (eql (aget :expression (ast-aux-data ast-a))
                     (aget :expression (ast-aux-data ast-b)))))))))

(defmethod update-caches ((obj lisp))
  (labels ((collect-asts (tree)
             ;; Collect all subtrees
             (cons tree
                   (iter (for c in (ast-children tree))
                         (unless (stringp c)
                           (appending (collect-asts c)))))))
    (setf (slot-value obj 'asts)
          (cdr (collect-asts (ast-root obj))))))
