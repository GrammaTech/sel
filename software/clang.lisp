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

(define-software clang (ast)
  ((compiler :initarg :compiler :accessor compiler :initform "clang")
   (clang-asts :initarg :clang-asts :accessor clang-asts :initform nil)
   (mitochondria :initarg :mitochondria
                 :accessor mitochondria
                 :initform (make-instance 'clang-mito)
                 :copier copy)))

(defgeneric mitochondria (clang)
  (:documentation "Additional 'foreign' genome required to build phenome."))

(defmethod apply-mutation ((clang clang) op)
  (multiple-value-bind (stdout exit)
    (restart-case (clang-mutate clang op) 
      (skip-mutation ()
        :report "Skip mutation and return nil genome" 
        (setf stdout nil))
      (tidy () 
        :report "Call clang-tidy before re-attempting mutation"
        (clang-tidy clang))
      (mutate () 
        :report "Apply another mutation before re-attempting mutations"
        (mutate clang)))
    stdout))

(defmethod apply-mutation :after ((clang clang) op)
  (with-slots (clang-asts) clang
    (when (not (member (car op) '(:ids :list :list-json)))
      (setf clang-asts nil))))

(defun extract-clang-genome (full-genome)
  (keep-lines-after-matching "======^======" full-genome))

(defmethod clang-mutate ((clang clang) op)
  (with-temp-file-of (src-file (ext clang)) (genome-string clang)
    (with-temp-file (value-file) ""
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
                       (string-to-file (cdr arg-pair) value-file)
                       (format nil "-value=~a" value-file))
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
        (when (not (zerop exit))
          (if (or (= exit 131)
                  (= exit 132)
                  (= exit 134)
                  (= exit 136)
                  (= exit 139))
               (error 'mutate 
                 :text (format t 
                          "\"clang-mutate core dump with args ~{~a~^ ~}\""
                          op))
               (error 'mutate
                 :text (format t
                          "\"clang-mutate non-zero exit with args ~{~a~^ ~}\""
                          op))))
        (values
         (if (member (car op) '(:ids :list :list-json))
             stdout
           (extract-clang-genome stdout))
         exit)))))

(defun json-db-to-vector (json)
  (let ((vec (make-array (1+ (length json)))))
    (loop for snippet in json
       do (let ((counter (aget :counter snippet)))
            (when counter
              (setf (aref vec counter) snippet))))
    vec))

(defmethod to-ast-list ((clang clang))
  (with-slots (clang-asts) clang
    (if clang-asts
        clang-asts
        (setf clang-asts
          (let ((list-string (clang-mutate clang `(:list-json (:bin . t)))))
            (unless (zerop (length list-string))
              (json-db-to-vector
               (json:decode-json-from-source list-string))))))))

(defun containing-asts (ast-list line col)
  (remove-if-not (lambda (snippet)
                   (let ((beg-line (aget :begin--src--line snippet))
                         (end-line (aget :end--src--line snippet))
                         (beg-col (aget :begin--src--col snippet))
                         (end-col (aget :end--src--col snippet)))
                     (and (or (< beg-line line)
                              (and (= beg-line line) (<= beg-col col)))
                          (or (> end-line line)
                              (and (= end-line line) (>= end-col col))))))
                 ast-list))

(defmethod line-breaks ((clang clang))
  (cons 0 (loop :for char :in (coerce (genome-string clang) 'list) :as index 
                :from 0
                :when (equal char #\Newline) :collect index)))

(defmethod to-ast-list-containing-bin-range((clang clang) begin-addr end-addr)
  (let ((ast-list (to-ast-list clang))
        (smallest-enclosing-ast nil)
        (smallest-enclosing-ast-sub-asts nil))
    (loop for ast-entry across ast-list
      ;; Find the smallest AST which encloses the range [begin-addr, end-addr]
       do (progn
            (when (and (/= (aget :parent--counter ast-entry) 0)
                       (aget :begin--addr ast-entry)
                       (aget :end--addr ast-entry)
                       (<= (aget :begin--addr ast-entry) begin-addr)
                       (<= end-addr (aget :end--addr ast-entry)))
              (if smallest-enclosing-ast
                  (when (< (- (aget :end--addr ast-entry)
                              (aget :begin--addr ast-entry))
                           (- (aget :end--addr smallest-enclosing-ast)
                              (aget :begin--addr smallest-enclosing-ast)))
                    (setf smallest-enclosing-ast-sub-asts nil)
                    (setf smallest-enclosing-ast ast-entry))
                  (setf smallest-enclosing-ast ast-entry)))

            ;; Collect all sub-asts of the smallest AST which encloses the
            ;; range [begin-addr, end-addr].
            ;; @TODO: This could be optimized further
            ;; to include only those sub-ASTs which have bytes
            ;; in the range begin-addr/end-addr.

            (when (and smallest-enclosing-ast
                       (is-parent-ast? ast-list smallest-enclosing-ast ast-entry))
              (setf smallest-enclosing-ast-sub-asts
                    (cons ast-entry smallest-enclosing-ast-sub-asts)))))
    (reverse smallest-enclosing-ast-sub-asts)))

(defun is-parent-ast?(ast-list possible-parent-ast ast)
  (cond ((= (aget :counter possible-parent-ast)
            (aget :counter ast)) t)
        ((= (aget :parent--counter ast) 0) nil)
        (t (is-parent-ast? ast-list 
                           possible-parent-ast
                           (aref (aget :counter possible-parent-ast) ast-list)))))

(defmethod to-ast-hash-table ((clang clang))
  (let ((ast-hash-table (make-hash-table :test 'equal)))
    (loop for ast-entry across (to-ast-list clang)
         do (let* ((ast-class (aget :ast--class ast-entry))
                   (cur (gethash ast-class ast-hash-table)))
              (setf (gethash ast-class ast-hash-table) (cons ast-entry cur))))
    ast-hash-table))

(defmethod nesting-depth ((clang clang) index &optional depth-acc)
  (let* ((ast (aref (to-ast-list clang) index))
         (is-block (equal (aget :ast--class ast) "CompoundStmt"))
         (depth (or depth-acc (if is-block -1 0)))
         (new-index (aget :parent--counter ast))
         (new-depth (if is-block (1+ depth) depth)))
    (if (or (not new-index) (= new-index 0))
        new-depth
        (nesting-depth clang new-index new-depth))))

;; FIXME: this is biased towards selecting crossover points at
;; ASTs of the least common class.
(defmethod crossover ((a clang) (b clang))
  (let* ((a-asts (to-ast-hash-table a))
         (b-asts (to-ast-hash-table b))
         (random-ast-class (random-hash-table-key a-asts))
         (a-crossover-ast (when-let ((it (gethash random-ast-class a-asts)))
                            (random-elt it)))
         (b-crossover-ast (when-let ((it (gethash random-ast-class b-asts)))
                            (random-elt it)))
         (variant (copy a)))
    (union-mito (mitochondria variant) (mitochondria b))
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
          (setf (genome-string variant)
                (copy-seq (concatenate
                           'string
                           (subseq (genome-string a) 0 a-crossover-pt)
                           (subseq (genome-string b) b-crossover-pt))))
          (values variant a-crossover-pt b-crossover-pt))
        (values variant nil nil))))

(defmethod genome-string ((clang clang) &optional stream)
  (format stream "~a~%//===============^==================~%~a"
          (genome-string (mitochondria clang))
          (genome clang)))

(defmethod phenome ((clang clang) &key bin)
  (with-temp-file-of (src-file (ext clang)) (genome-string clang)
    (compile-software-object clang src-file :bin bin)))

(defmethod compile-software-object ((clang clang) src-file &key bin)
  (let ((bin (or bin (temp-file-name))))
    (multiple-value-bind (stdout stderr exit)
        (shell "~a ~a -o ~a ~a"
               (compiler clang)
               src-file bin (mapconcat #'identity (flags clang) " "))
      (declare (ignorable stdout))
      (values (if (zerop exit) bin stderr) exit))))

(defmethod clang-tidy ((clang clang))
  (setf (genome-string clang)
        (with-temp-file-of (src (ext clang)) (genome-string clang)
          (multiple-value-bind (stdout stderr exit)
              (shell "clang-tidy -fix -fix-errors -checks=~{~a~^,~} ~a -- ~a 1>&2"
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
            (if (zerop exit) (file-to-string src) (genome-string clang)))))
  clang)

(defmethod clang-format ((obj clang) &optional style)
  ;; STYLE may be one of LLVM, Google, Chromium, Mozilla, WebKit.
  (setf (genome-string obj)
        (with-temp-file-of (src (ext obj)) (genome-string obj)
          (multiple-value-bind (stdout stderr exit)
              (shell "clang-format ~a ~a "
                     (if style (format nil "-style=~a" style) "")
                     src)
            (declare (ignorable stderr))
            (if (zerop exit) stdout (genome-string obj)))))
  obj)

(defmethod (setf genome-string) (text (clang clang))
  (setf (genome clang) (extract-clang-genome text)))

(defmethod lines ((obj clang))
  (split-sequence '#\Newline (genome-string obj)))

(defmethod (setf lines) (new (obj clang))
  (setf (genome-string obj) (format nil "~{~a~^~%~}" new)))
