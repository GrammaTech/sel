;;; ast.lisp --- ast software representation

;; TODO: get memoization working

(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;; ast software objects
(define-software ast (software)
  ((genome   :initarg :genome   :accessor genome   :initform ""
             :copier :direct)
   (flags    :initarg :flags    :accessor flags    :initform nil
             :copier copy-tree)
   (compiler :initarg :compiler :accessor compiler :initform "clang"
             :copier copy-seq)
   (ext      :initarg :ext      :accessor ext      :initform "c"
             :copier copy-tree)
   (raw-size :initarg :size     :accessor raw-size :initform nil
             :copier :none))
  (:documentation "DOCFIXME"))

(defmethod phenome ((obj ast) &key (bin (temp-file-name)))
  "DOCFIXME
* OBJ DOCFIXME
* BIN DOCFIXME
"
  #-ccl (declare (values t fixnum string string string))
  (setf bin (ensure-path-is-string bin))
  (with-temp-file-of (src (ext obj)) (genome-string obj)
    (multiple-value-bind (stdout stderr errno)
        (shell "~a ~a -o ~a ~{~a~^ ~}" (compiler obj) src bin (flags obj))
      (restart-case
          (unless (zerop errno)
            (error (make-condition 'phenome :text stderr :obj obj :loc src)))
        (retry-project-build ()
          :report "Retry `phenome' on OBJ."
          (phenome obj :bin bin))
        (return-nil-for-bin ()
          :report "Allow failure returning NIL for bin."
          (setf bin nil)))
      (values bin errno stderr stdout src))))

(defmethod genome-string ((ast ast) &optional stream)
  "DOCFIXME
* AST DOCFIXME
* STREAM DOCFIXME
"
  (let ((genome (or (genome ast) "")))
    (if stream (write-string genome stream) genome)))

(defmethod from-file ((ast ast) path)
  "DOCFIXME
* AST DOCFIXME
* PATH DOCFIXME
"
  (setf (genome ast) (file-to-string path))
  (setf (ext ast)  (pathname-type (pathname path)))
  ast)

(defun ast-from-file (path &key flags)
  "DOCFIXME
* PATH DOCFIXME
* FLAGS DOCFIXME
"
  (assert (listp flags) (flags) "flags must be a list")
  (from-file (make-instance 'ast :flags flags) path))

(defun ast-to-file (software path &key if-exists)
  "DOCFIXME
* SOFTWARE DOCFIXME
* PATH DOCFIXME
* IF-EXISTS DOCFIXME
"
  (string-to-file (genome software) path :if-exists if-exists))

(defmethod size ((ast ast))
  "DOCFIXME"
  (or (raw-size ast)
      (setf (raw-size ast)
            (or (progn
                  (error "TODO: alternate interface for :list and :ids.")
                  (ignore-errors
                    (parse-number (apply-mutation ast (list :ids)))))
                0))))

(defmethod mutate ((ast ast))
  "DOCFIXME"
  (unless (> (size ast) 0)
    (error (make-condition 'mutate :text "No valid IDs" :obj ast)))
  (let ((op (case (random-elt '(cut insert swap))
              (cut    `(:cut    (:stmt1 . ,(pick-bad ast))))
              (insert `(:insert (:stmt1 . ,(pick-bad ast)) 
                                (:stmt2 . ,(pick-good ast))))
              (swap   `(:swap   (:stmt1 . ,(pick-bad ast)) 
                                (:stmt2 . ,(pick-good ast)))))))
    (apply-mutation ast op)
    (values ast op)))

(defmethod crossover ((a ast) (b ast))
  "DOCFIXME
* A DOCFIXME
* B DOCFIXME
"
  (let ((a-point (random-elt (line-breaks (genome a))))
        (b-point (random-elt (line-breaks (genome b))))
        (new (copy a)))
    (setf (genome new)
          (copy-seq (concatenate 'string
                      (subseq (genome a) 0 a-point)
                      (subseq (genome b) b-point))))
    (values new (list a-point b-point))))

(defgeneric select-crossover-points (a b)
  (:documentation "Select suitable crossover points in A and B.
If no suitable points are found the returned points may be nil."))

(defmethod (setf genome-string) (text (obj ast))
  "DOCFIXME
* TEXT DOCFIXME
* OBJ DOCFIXME
"
  (setf (genome obj) text))

(defmethod lines ((ast ast))
  "DOCFIXME"
  (split-sequence #\Newline (genome ast)))

(defmethod line-breaks ((ast ast))
  "DOCFIXME"
  (cons 0 (loop :for char :in (coerce (genome ast) 'list) :as index 
                :from 0
                :when (equal char #\Newline) :collect index)))

(defmethod (setf lines) (new (ast ast))
  "DOCFIXME
* NEW DOCFIXME
* AST DOCFIXME
"  
  (setf (genome ast) (format nil "~{~a~^~%~}" new)))

(defgeneric asts (software)
  (:documentation "Return a list of all asts in SOFTWARE."))
