;;; clang-w-binary.lisp --- clang software with binary information

;;; Code:
(in-package :software-evolution)

(define-software clang-w-binary (clang)
  ;; Bytes in the associated binary.
  ((bytes :initarg :bytes :accessor bytes :copier copy
          :initform (error "CLANG-W-BINARY objects require bytes")
          :copier :direct)
   (diff-addresses :initarg :diff-addresses :accessor diff-addresses
                   :copier copy-seq :initform nil))
  (:documentation
   "Clang software object associated with a compiled binary which may
be used to associate bytes with AST elements."))

(defmethod update-asts ((obj clang-w-binary) &key)
  ;; Wrap update-asts to pull dwarf information from the binary
  ;; associated with OBJ.
  (with-temp-file-of-bytes (bin) (bytes obj)
    (call-next-method obj :clang-mutate-args (list (list :bin bin)))))

(defmethod fitness-extra-data ((obj clang-w-binary))
  (diff-addresses obj))

(defmethod (setf fitness-extra-data) (extra-data (obj clang-w-binary))
  (setf (diff-addresses obj) extra-data)
  (call-next-method))

(defvar *targeted-mutation-chance* 0.75
  "Probability of performing a targeted vs. random mutation.")

(defmethod pick-bad((obj clang-w-binary))
  (if (and (diff-addresses obj)
           (< (random 1.0) *targeted-mutation-chance*))
      (pick-bad-targetted obj)
      (call-next-method)))

(defmethod pick-bad-targetted((obj clang-w-binary))
  "Return the AST of a binary-difference inducing AST in clang-w-fodder"
  (random-stmt (mappend (lambda (address-range)
                          (asts-in-binary-range obj
                                                (aget :begin-addr address-range)
                                                (aget :end-addr address-range)))
                        (diff-addresses obj))))

(defmethod asts-containing-binary-address ((obj clang-w-binary) address)
  (remove-if-not (lambda (ast)
                   (and (aget :begin--addr ast)
                        (or (>= address (aget :begin--addr ast))
                            (<= address (aget :end--addr ast)))))
                 (asts obj)))

(defmethod asts-in-binary-range ((obj clang-w-binary) begin-addr end-addr)
  ;; Find all asts which overlap bin range.
  (remove-if-not (lambda (ast)
                   (and (aget :begin--addr ast)
                        (or (> (aget :begin--addr ast) end-addr)
                            (< (aget :end--addr ast) begin-addr))))
                 (asts obj)))
