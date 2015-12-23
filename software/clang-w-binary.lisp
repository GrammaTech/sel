;;; clang-w-binary.lisp --- clang software with binary information

;;; Code:
(in-package :software-evolution)

(define-software clang-w-binary (clang)
  ;; Bytes in the associated binary.
  ((bytes :initarg :bytes :accessor bytes :copier copy
          :initform (error "CLANG-W-BINARY objects require bytes")
          :copier :direct)
   (diff-addresses :initarg :diff-addresses :accessor diff-addresses
                   :copier copy-seq :initform nil :type '(list range)))
  (:documentation
   "Clang software object associated with a compiled binary which may
be used to associate bytes with AST elements."))

(defmethod update-asts ((obj clang-w-binary) &key)
  ;; Wrap update-asts to pull dwarf information from the binary
  ;; associated with OBJ.
  (with-temp-file-of-bytes (bin) (bytes obj)
    (call-next-method obj :clang-mutate-args (list (cons :bin bin)))))

(defmethod fitness-extra-data ((obj clang-w-binary))
  (diff-addresses obj))

(defmethod (setf fitness-extra-data) (extra-data (obj clang-w-binary))
  (setf (diff-addresses obj)
        (mapcar (lambda (data) (make-instance 'range
                            :begin (aget :begin-addr data)
                            :end (aget :end-addr data)))
                extra-data))
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
  (random-stmt
   ;; Collect all ASTs contained in the corresponding source ranges.
   ;; In effect these should be the children of the below ASTs.
   (mappend [{asts-contained-in-source-range obj} #'ast-to-source-range]
            ;; Collect all ASTs intersecting bad binary ranges.
            (mappend {asts-intersecting-binary-range obj}
                     (diff-addresses obj)))))

(defmethod asts-containing-binary-address ((obj clang-w-binary) address)
  (remove-if-not [{contains _ address} #'ast-to-binary-range] (asts obj)))

(defmethod asts-contained-in-binary-range ((obj clang) (range range))
  (remove-if-not [{contains range} #'ast-to-binary-range] (asts obj)))

(defmethod asts-intersecting-binary-range ((obj clang) (range range))
  (remove-if-not [{intersects range} #'ast-to-binary-range] (asts obj)))
