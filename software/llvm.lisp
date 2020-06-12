;;; llvm.lisp --- llvm software representation
;;;
;; This software object uses the llvm-mutate [1] compiler pass run by
;; llvm opt [2] to manipulate LLVM intermediate representation (IR)
;; code (see [3] for more information).
;;;
;; [1] https://github.com/eschulte/llvm-mutate
;; [2] http://llvm.org/releases/3.2/docs/CommandGuide/opt.html
;; [3] http://llvm.org/docs/LangRef.html#introduction
(defpackage :software-evolution-library/software/llvm
  (:nicknames :sel/software/llvm :sel/sw/llvm)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/components/file
        :software-evolution-library/software/compilable
        :software-evolution-library/software/source)
  (:export :llvm))
(in-package :software-evolution-library/software/llvm)
(in-readtable :curry-compose-reader-macros)


;;; llvm software objects
(define-software llvm (source compilable)
  ((linker   :initarg :linker   :accessor linker   :initform "gcc"))
  (:documentation
   "Low Level Virtual Machine (LLVM) intermediate representation (IR).
See http://llvm.org)."))

(defmethod initialize-instance :after ((obj llvm) &key &allow-other-keys)
  "Wrapper after the constructor to ensure the compiler slot has been
initialized to a default value if one is not provided."
  (setf (compiler obj) (or (compiler obj) "llc")))

(defmethod from-file ((llvm llvm) path)
  "DOCFIXME

* LLVM DOCFIXME
* PATH DOCFIXME
"
  (setf (genome llvm) (file-to-string path))
  llvm)

(defmethod mutate ((llvm llvm))
  "DOCFIXME"
  (unless (> (size llvm) 0)
    (error (make-condition 'mutate :text "No valid IDs" :obj llvm)))
  (let ((op (case (random-elt '(cut replace insert swap))
              (cut     `(:cut     ,(pick-bad llvm)))
              (replace `(:replace ,(pick-bad llvm) ,(pick-good llvm)))
              (insert  `(:insert  ,(pick-bad llvm) ,(pick-good llvm)))
              (swap    `(:swap    ,(pick-bad llvm) ,(pick-good llvm))))))
    (apply-mutation llvm op)
    (values llvm op)))

(defmethod apply-mutation ((llvm llvm) op)
  "DOCFIXME

* LLVM DOCFIXME
* OP DOCFIXME
"
  (with-temporary-file-of (:pathname src :type (ext llvm)) (genome llvm)
    (multiple-value-bind (stdout stderr exit)
        (shell "cat ~a|llvm-mutate --~a ~a"
               src
               (string-downcase (symbol-name (car op)))
               (mapconcat [{format nil "~a"} #'1+] (cdr op) ","))
      (declare (ignorable stdout stderr))
      (unless (zerop exit)
        (error (make-condition 'mutate
                 :text "llvm-mutate" :obj llvm :operation op)))
      llvm)))

(defmethod phenome ((llvm llvm) &key (bin (temp-file-name)))
  "DOCFIXME

* LLVM DOCFIXME
* BIN DOCFIXME
"
  #-ccl (declare (values t fixnum string string string))
  (setf bin (namestring bin))
  (with-temporary-file-of (:pathname src :type (ext llvm)) (genome llvm)
    (multiple-value-bind (stdout stderr errno)
        (shell "cat ~a|~a|~a ~{~a~^ ~} -x assembler - -o ~a"
               src (compiler llvm) (linker llvm) (flags llvm) bin)
      (declare (ignorable stdout stderr))
      (values bin errno stderr stdout src))))
