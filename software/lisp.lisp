;;; lisp.lisp --- software representation of lisp code
;;;
;;; Eclector, see @url{https://github.com/robert-strandh/Eclector},
;;; is used to parse lisp source into concrete ASTs.
;;;
;;; @texi{lisp}
(defpackage :software-evolution-library/software/lisp
  (:nicknames :sel/software/lisp :sel/sw/lisp)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/parseable
        :eclector.parse-result)
  (:import-from :eclector.reader
                :evaluate-expression
                :interpret-symbol)
  (:shadowing-import-from :eclector.readtable
                          :copy-readtable
                          :set-dispatch-macro-character)
  (:shadowing-import-from :eclector.parse-result
                          :read
                          :read-from-string
                          :read-preserving-whitespace)
  (:export :lisp :lisp-ast :lisp-ast-p
           :expression :expression-result))
(in-package :software-evolution-library/software/lisp)
(in-readtable :curry-compose-reader-macros)


(defvar *string*)

(defclass lisp-ast (ast)
  ((expression :initarg :expression :initform nil :reader expression
               :type list)
   (children :reader children
             :type list
             :initarg :children
             :initform nil
             :documentation "The list of children of the node,
which may be more nodes, or other values.")
   (child-slots :initform '(children) :allocation :class)
   (data-slot :initform 'expression :allocation :class))
  (:documentation "Class of Common Lisp ASTs."))

(defmethod fset-default-node-accessor ((node-type (eql 'lisp-ast)))
  'expression)

(defclass result (lisp-ast)
  ((start :initarg :start :initform (when *string* 0)
          :reader start :type (or null (integer 0 *)))
   (end :initarg :end :initform (when *string* (length *string*))
        :reader end :type (or null (integer 0 *)))
   (string-pointer :initarg :string-pointer :initform *string*
                   :reader string-pointer :type (or null string))))

(defclass expression-result (result) ())

(defmethod print-object ((obj expression-result) stream)
  (with-slots (start end string-pointer expression children) obj
    (if *print-readably*
        (format stream "~S" `(make-instance 'expression-result
                               :start ,start
                               :end ,end
                               :string-pointer *string*
                               :expression ,expression
                               :children (list ,@children)))
        (print-unreadable-object (obj stream :type t)
          (format stream ":EXPRESSION ~a" expression)))))

(defclass skipped-input-result (result)
  ((reason :initarg :reason :reader  reason)))

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

(defmethod convert ((to-type (eql 'lisp-ast)) (sequence list)
                    &key (spaces nil) (expression sequence) &allow-other-keys)
  (labels ((m/space (&optional string)
             (or (and (not string) spaces (pop spaces))
                 (let ((*string* (or string " ")))
                   (make-instance 'skipped-input-result :reason :whitespace))))
           (m/keyword (symbol)
             (let ((*string* (concatenate 'string ":" (string-downcase
                                                       (symbol-name symbol)))))
               (make-instance 'expression-result :expression symbol)))
           (m/symbol (symbol)
             (let ((*string* (string-downcase (symbol-name symbol))))
               (make-instance 'expression-result :expression symbol)))
           (m/other (other)
             (let ((*string* (format nil "~S" other)))
               (make-instance 'expression-result :expression other)))
           (intersperse-spaces (list)
             (let ((ult (length list))
                   (last nil))
               (iter (for el in list)
                     (for i upfrom 0)
                     (if (and (< i ult)
                              (> i 0)
                              (not (string= "(" (source-text last)))
                              (not (string= ")" (source-text el))))
                         (appending (list (m/space) el))
                         (collecting el))
                     (setf last el))))
           (convert (node)
             (when node
               (typecase node
                 (lisp-ast node)
                 (keyword (m/keyword node))
                 (symbol (m/symbol node))
                 (list
                  (let ((*string* ""))
                    (make-instance 'expression-result :expression expression
                                   :children
                                   (intersperse-spaces
                                    (append (list (m/space "("))
                                            (mapcar #'convert node)
                                            (list (m/space ")")))))))
                 (t (m/other node))))))
    (populate-fingers (convert sequence))))

;;; Trivial Eclector client used to customize parsing for SEL.
(defclass client (parse-result-client) ())

(defun sharpsign-sign-reader (stream char n)
  (declare (ignore n))
  (let* ((client (make-instance 'client))
         (prefix
          (ecase char
            (#\+ '|#+|)
            (#\- '|#-|)))
         (feature-expression (read client stream))
         (expression (read client stream)))
    (list prefix feature-expression expression)))

(defparameter *lisp-ast-readtable*
  (let ((readtable (copy-readtable eclector.readtable:*readtable*)))
    (set-dispatch-macro-character readtable #\# #\+ 'sharpsign-sign-reader)
    (set-dispatch-macro-character readtable #\# #\- 'sharpsign-sign-reader)
    readtable))

(defmethod make-expression-result
    ((client client) (result t) (children t) (source cons))
  (make-instance 'expression-result :expression result
                 :children children
                 :start (car source)
                 :end (cdr source)))

(defmethod make-skipped-input-result
    ((client client) stream reason source)
  (declare (ignorable client stream))
  (make-instance 'skipped-input-result
    :reason reason :start (car source) :end (cdr source)))

(defmethod interpret-symbol
    ((client client) input-stream package-indicator symbol-name internp)
  (let ((package (case package-indicator
                   (:current *package*)
                   (:keyword (find-package "KEYWORD"))
                   (t        (or (find-package package-indicator)
                                 ;; Return a fake package for missing packages.
                                 (find-package :missing)
                                 (make-package :missing))))))
    (if internp
        (intern symbol-name package)
        (multiple-value-bind (symbol status)
            (find-symbol symbol-name package)
          (cond ((null status) ; Ignore `symbol-does-not-exist' errors.
                 ;; (eclector.base::%reader-error
                 ;;  input-stream 'eclector.reader::symbol-does-not-exist
                 ;;  :package package
                 ;;  :symbol-name symbol-name)
                 symbol)
                ((eq status :internal) ; Ignore `symbol-is-not-external' errors.
                 ;; (eclector.base::%reader-error
                 ;;  input-stream 'eclector.reader::symbol-is-not-external
                 ;;  :package package
                 ;;  :symbol-name symbol-name)
                 symbol)
                (t
                 symbol))))))

;;; The next two forms are used to avoid throwing errors when a
;;; #. reader macro attempts to execute code during parsing.  We want
;;; to avoid this as we will typically not have the requisite
;;; variables defined.
(defgeneric wrap-in-sharpsign-dot (client material)
  (:method (client material)
    (list '|#.| material)))

(defmethod evaluate-expression ((client client) expression)
  (wrap-in-sharpsign-dot client expression))

(defun read-forms+ (string &key count)
  (check-type count (or null integer))
  (let ((*string* string)
        (client (make-instance 'client))
        (eclector.readtable:*readtable* *lisp-ast-readtable*))
    (labels
        ((make-space (start end)
           (when (< start end)
             (list (make-instance 'skipped-input-result
                     :start start :end end :reason 'whitespace))))
         (w/space (tree from to)
           (let ((result
                  (etypecase tree
                    (list
                     (append
                      (iter (for subtree in tree)
                            (appending (make-space from (start subtree)))
                            (appending (w/space subtree
                                                (start subtree) (end subtree)))
                            (setf from (end subtree)))
                      (make-space from to)))
                    (result
                     (when (subtypep (type-of tree) 'expression-result)
                       (when (children tree)
                         ;; Use (sef slot-value) because this is now a
                         ;; functional tree node so the default setf would
                         ;; have no effect (it would create a copy).
                         (setf (slot-value tree 'children)
                               (w/space
                                (children tree) (start tree) (end tree)))))
                     (append
                      (make-space from (start tree))
                      (list tree)
                      (make-space (end tree) to))))))
             result)))
      (w/space
       (with-input-from-string (input string)
         (loop :with eof = '#:eof
            :for n :from 0
            :for form = (if (and count (>= n count))
                            eof
                            (read client input nil eof))
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
  (walk-skipped-forms  [{format stream "~a"} #'source-text] forms))

(defun write-string-forms+ (forms)
  "Write the original source text of FORMS to a string."
  (with-output-to-string (s) (write-stream-forms+ forms s)))


;;; Lisp software object
(define-software lisp (parseable)
  ()
  (:documentation "Common Lisp source represented naturally as lists of code."))

(defmethod convert ((to-type (eql 'lisp-ast)) (string string)
                    &key &allow-other-keys)
  (make-instance 'lisp-ast :children (read-forms+ string)))

(defmethod from-string ((lisp lisp) string)
  (setf (genome lisp) (convert 'lisp-ast string))
  lisp)

(defmethod from-file ((lisp lisp) file)
  (from-string lisp (file-to-string file)))

(defmethod update-paths ((node node) &optional path)
  (declare (ignorable path))
  node)

(defmethod source-text ((obj result) &optional stream)
  (if (children obj)
      (mapc [{write-string _ stream} #'source-text] (children obj))
      (write-string (subseq (string-pointer obj) (start obj) (end obj)) stream)))

#+example
(progn

;;; Rewrite ->> to use nest instead.
  (defun fix-double-arrow (node)
    (flet ((children (node)
             (let* ((*string* "nest")
                    (nest (make-instance 'expression-result
                            :expression 'nest :start 0 :end (length *string*)))
                    (space (remove-if-not
                            {typep _ 'skipped-input-result} (children node)))
                    (exprs (cons nest (reverse (cdr (remove-if-not
                                                     {typep _ 'expression-result}
                                                     (children node)))))))
               (mapcar ‹etypecase (skipped-input-result (pop space))
                        (expression-result (pop exprs))›
                        (children node)))))
      (let* ((*string* nil)
             (expression (cons 'nest (reverse (cdr (expression node)))))
             (children (children node)))
        (make-instance 'expression-result
          :expression expression
          :children children))))

  (defun rewrite-double-arrow (software)
    (setf (genome software)
          (map-tree (lambda (node)
                      (if (and (typep node 'expression-result)
                               (listp (expression node))
                               (equal '->> (first (expression node))))
                          (values (fix-double-arrow node) t)
                          (values node nil)))
                    (genome software))))

  (defun rewrite-double-arrow-in-place (file)
    (string-to-file (source-text (rewrite-double-arrow
                                  (from-file (make-instance 'lisp) file))) file)))
