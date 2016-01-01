;;; clang-w-binary.lisp --- clang software with binary information

;;; Code:
(in-package :software-evolution)

(define-software clang-w-binary (clang)
  ((cached-compilation :accessor cached-compilation 
                       :copier :direct
                       :initform nil
                       :copier :direct)
   (diff-data :initarg :diff-data 
              :accessor diff-data
              :initform nil
              :copier :direct))
  (:documentation
   "Clang software object associated with a compiled binary which may
be used to associate bytes with AST elements."))

(defmethod update-asts ((obj clang-w-binary) &key)
  ;; Wrap update-asts to pull dwarf information from the binary
  ;; associated with OBJ.
  (with-temp-file (bin)
    (multiple-value-bind (output exit src-file-path)
      (phenome obj :bin bin)
      (call-next-method obj :clang-mutate-args 
                            (when (zerop exit)
                              `((:bin . ,output)
                                (:dwarf-src-file-path . ,src-file-path)))))))

(defmethod fitness-extra-data ((obj clang-w-binary))
  (diff-data obj))

(defmethod (setf fitness-extra-data) (extra-data (obj clang-w-binary))
  (setf (diff-data obj) (extra-data))
  (call-next-method))

(defmethod phenome ((obj clang-w-binary) &key (bin (temp-file-name)))
  (let ((bin-name bin))
    ;; Ensure cache is filled
    (unless (cached-compilation obj)
      (with-temp-file (bin-name)
        (multiple-value-bind (output exit src-file-path)
          (call-next-method obj :bin bin-name)
          (setf (cached-compilation obj) 
                `((:output . ,(if (zerop exit) (file-to-bytes output) output))
                  (:exit . ,exit)
                  (:src-file-path . ,src-file-path)))))

    ;; Since we keep up-to-date bytes associated with the binary, there
    ;; is no need to compile at phenome generation time.
    (let ((exit (aget :exit (cached-compilation obj)))
          (output (aget :output (cached-compilation obj)))
          (src-file-path (aget :src-file-path (cached-compilation obj))))
      (when (zerop exit)
        (bytes-to-file output bin-name)
        (shell "chmod +x ~a" bin-name))
      (values (if (zerop exit) bin-name output) exit src-file-path)))))

(defmethod apply-mutation :around ((obj clang-w-binary) op)
  (multiple-value-call (lambda (variant &rest rest)
                         (unless (member (car op) '(:ids :list :json))
                           (setf (cached-compilation obj) nil))
                         (apply #'values variant rest))
    (call-next-method)))

(defvar *targeted-mutation-chance* 0.75
  "Probability of performing a targeted vs. random mutation.")

(defmethod pick-bad((obj clang-w-binary))
  (if (and (diff-data obj)
           (< (random 1.0) *targeted-mutation-chance*))
    (let ((random-stmt (pick-bad-targetted obj)))
      (if random-stmt
          random-stmt
          (call-next-method)))
    (call-next-method)))

(defmethod pick-bad-targetted((obj clang-w-binary))
  "Return the AST of a binary-difference inducing AST in clang-w-fodder"

  ;; Collect all ASTs contained in the corresponding source ranges.
  ;; In effect these should be the children of the below ASTs.
  (let ((bad-asts (mappend [{asts-contained-in-source-range obj} #'ast-to-source-range]
                   ;; Collect all ASTs intersecting bad binary ranges.
                      (mappend {asts-intersecting-binary-range obj}
                          (mapcar {aget :modified-range} (diff-data obj))))))
    (when bad-asts (random-stmt bad-asts))))

(defmethod asts-containing-binary-address ((obj clang-w-binary) address)
  (remove-if-not [{contains _ address} #'ast-to-binary-range] (asts obj)))

(defmethod asts-contained-in-binary-range ((obj clang) (range range))
  (remove-if-not [{contains range} #'ast-to-binary-range] (asts obj)))

(defmethod asts-intersecting-binary-range ((obj clang) (range range))
  (remove-if-not [{intersects range} #'ast-to-binary-range] (asts obj)))

(defmethod get-diffs-intersecting-ast((obj clang-w-fodder-and-binary) ast)
  "Get the diffs intersecting the given AST"
  (when (diff-data obj)
    (let ((ast-bin-range (make-instance 'range
                                        :begin (aget :begin--addr ast)
                                        :end (aget :end--addr ast))))
      (remove-if-not [{intersects ast-bin-range} 
                      #'(lambda(diff) (aget :modified-range diff))]
        (diff-data obj)))))
                     
(defmethod get-nearest-ast-w-bytes((obj clang-w-fodder-and-binary) ast)
  "Get the nearest AST in the hierarchy with bytes associated with it"
  (when ast
    (if (aget :binary--contents ast)
      ast
      (get-ast-w-bytes obj (get-ast obj (aget :parent--counter ast))))))
