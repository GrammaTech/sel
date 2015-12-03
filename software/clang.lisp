;;; clang.lisp --- clang software representation

;; Copyright (C) 2012 Eric Schulte

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(in-package :software-evolution)

(defclass clang (ast)
  ((clang-asts :initarg :clang-asts 
               :initform nil)))

(defmethod apply-mutation ((clang clang) op)
  (multiple-value-bind (stdout exit)
    (clang-mutate clang op)
    (if (zerop exit) stdout nil)))

(defmethod apply-mutation :after ((clang clang) op)
  (with-slots (clang-asts) clang
    (when (not (member (car op) '(:ids :list :list-json)))
      (setf clang-asts nil))))

(defmethod clang-mutate ((clang clang) op)
  (with-temp-file-of (src-file (ext clang)) (genome clang)
    (multiple-value-bind (stdout stderr exit)
      (shell "clang-mutate ~a ~a ~a -- ~a | tail -n +2"
             (ecase (car op)
               (:cut              "-cut")
               (:cut-full-stmt    "-cut")
               (:insert           "-insert")
               (:swap             "-swap")
               (:swap-full-stmt   "-swap")
               (:set-value        "-set")
               (:insert-value     "-insert-value")
               (:insert-full-stmt "-insert-value")
               (:ids              "-ids")
               (:list             "-list")
               (:list-json        "-list -json"))
             (mapconcat 
               (lambda (arg-pair)
                 (ecase (car arg-pair)
                   (:stmt1 
                     (format nil "-stmt1=~d" (cdr arg-pair)))
                   (:stmt2 
                     (format nil "-stmt2=~d" (cdr arg-pair)))
                   (:value 
                     (format nil "-value='~a'" (cdr arg-pair)))
                   (:bin   
                     (when (cdr arg-pair) 
                       (multiple-value-bind (bin exit)
                         (compile-software-object clang src-file)
                         (when (zerop exit)
                           (format nil "-binary=~a" bin)))))))
               (cdr op) 
               " ")
             src-file 
             (mapconcat #'identity (flags clang) " "))
      (declare (ignorable stderr))
      (values stdout exit))))

(defmethod to-ast-list ((clang clang))
  (with-slots (clang-asts) clang
    (if clang-asts
        clang-asts
        (setf clang-asts
          (let ((list-string (clang-mutate clang `(:list-json (:bin . t)))))
            (unless (zerop (length list-string))
              (json:decode-json-from-source list-string)))))))

(defmethod to-ast-list-containing-bin-range((clang clang) begin-addr end-addr)
  (let ((ast-list (to-ast-list clang))
        (smallest-enclosing-ast nil)
        (smallest-enclosing-ast-sub-asts nil))
    (dolist (ast-entry ast-list)
      ;; Find the smallest AST which encloses the range [begin-addr, end-addr]
      (when (and (aget :begin--addr ast-entry)
                 (aget :end--addr ast-entry)
                 (<= (aget :begin--addr ast-entry) begin-addr)
                 (<= end-addr (aget :end--addr ast-entry)))
        (if smallest-enclosing-ast
          (when (< (- (aget :end--addr ast-entry)
                      (aget :begin--addr ast-entry))
                   (- (aget :end--addr smallest-enclosing-ast)
                      (aget :begin--addr smallest-enclosing-ast)))
            (setf smallest-enclosing-ast ast-entry)
            (setf smallest-enclosing-ast-sub-asts nil))
          (setf smallest-enclosing-ast ast-entry)))

      ;; Collect all sub-asts of the smallest AST which encloses the 
      ;; range [begin-addr, end-addr].  
      ;; @TODO: This could be optimized further
      ;; to include only those sub-ASTs which have bytes
      ;; in the range begin-addr/end-addr.
      (when (and smallest-enclosing-ast
                 (<= (aget :begin--src--line smallest-enclosing-ast)
                     (aget :begin--src--line ast-entry))
                 (<= (aget :begin--src--col smallest-enclosing-ast)
                     (aget :begin--src--col ast-entry))
                 (<= (aget :end--src--line ast-entry)
                     (aget :end--src--line smallest-enclosing-ast))
                 (<= (aget :end--src--col ast-entry)
                     (aget :end--src--col smallest-enclosing-ast)))
        (setf smallest-enclosing-ast-sub-asts
              (cons ast-entry smallest-enclosing-ast-sub-asts))))
    (reverse smallest-enclosing-ast-sub-asts)))

(defmethod to-ast-hash-table ((clang clang) &key bin)
  (let ((ast-hash-table (make-hash-table :test 'equal))
        (ast-list (if bin (to-ast-list clang :bin bin)
                          (to-ast-list clang))))
    (dolist (ast-entry ast-list)
      (let* ((ast-class (aget :ast--class ast-entry))
             (cur (gethash ast-class ast-hash-table)))
        (setf (gethash ast-class ast-hash-table) (cons ast-entry cur))))
    ast-hash-table))

(defmethod crossover ((a clang) (b clang))
  (let* ((a-asts (to-ast-hash-table a))
         (b-asts (to-ast-hash-table b))
         (random-ast-class (random-hash-table-key a-asts))
         (a-crossover-ast (when-let ((it (gethash random-ast-class a-asts)))
                            (random-elt it)))
         (b-crossover-ast (when-let ((it (gethash random-ast-class b-asts)))
                            (random-elt it)))
         (variant (copy a)))
    (if (and a-crossover-ast b-crossover-ast)
        (let* ((a-crossover-src-ln (aget :end--src--line a-crossover-ast))
               (a-crossover-src-col (aget :end--src--col a-crossover-ast))
               (a-line-breaks (line-breaks a))
               (a-crossover-pt (+ (nth (1- a-crossover-src-ln) a-line-breaks)
                                  a-crossover-src-col))
               (b-crossover-src-ln (aget :begin--src--line b-crossover-ast))
               (b-crossover-src-col (aget :begin--src--col b-crossover-ast))
               (b-line-breaks (line-breaks b))
               (b-crossover-pt (+ (nth (1- b-crossover-src-ln) b-line-breaks)
                                  b-crossover-src-col)))
          (setf (genome variant)
                (copy-seq (concatenate 'string
                            (subseq (genome a) 0 a-crossover-pt)
                            (subseq (genome b) b-crossover-pt))))
          (values variant a-crossover-pt b-crossover-pt))
        (values variant nil nil))))

(defmethod phenome ((clang clang) &key bin)
  (with-temp-file-of (src-file (ext clang)) (genome clang)
    (compile-software-object clang src-file :bin bin)))

(defmethod compile-software-object ((clang clang) src-file &key bin)
  (let ((bin (or bin (temp-file-name))))
    (multiple-value-bind (stdout stderr exit)
        (shell "~a ~a -o ~a ~a"
               (compiler clang)
               src-file bin (mapconcat #'identity (flags clang) " "))
      (declare (ignorable stdout stderr))
      (values (if (zerop exit) bin stderr) exit))))

(defmethod clang-tidy ((clang clang))
  (setf (genome clang)
        (with-temp-file-of (src (ext clang)) (genome clang)
          (multiple-value-bind (stdout stderr exit)
              (shell "clang-tidy -fix -checks=~{~a~^,~} ~a -- ~a 1>&2"
                     '("-cppcore-guidelines-pro-bounds-array-to-pointer-decay"
                       "-google-build-explicit-make-pair"
                       "-google-explicit-constructor"
                       "-google-readability-namespace-comments"
                       "-google-readability-redundant-smartptr-get"
                       "-google-readability-runtime-int"
                       "-google-readability-readability-function-size"
                       "-llvm-namespace-commant"
                       "-llvm-include-order"
                       "-misc-mode-constructor-init"
                       "-misc-noexcept-move-constructor"
                       "-misc-uniqueptr-reset-release"
                       "-modernize*"
                       "-readability-container-size-empty"
                       "-readability-function-size"
                       "-readability-redundant-smart-ptr-get"
                       "-readability-uniqueptr-delete-release")
                     src
                     (mapconcat #'identity (flags clang) " "))
            (declare (ignorable stdout stderr))
            (if (zerop exit) (file-to-string src) (genome clang))))))
