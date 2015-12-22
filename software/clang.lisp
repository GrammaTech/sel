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
   (asts :initarg :asts :initform nil :copier :direct)
   (mitochondria :initarg :mitochondria
                 :accessor mitochondria
                 :initform (make-instance 'clang-mito)
                 :copier copy)))

(defgeneric update-asts (software &key)
  (:documentation "Update the store of asts associated with SOFTWARE."))

(defgeneric asts (software)
  (:documentation "Return a list of all asts in SOFTWARE."))

(defgeneric get-ast (software id)
  (:documentation "Return the statement in SOFTWARE indicated by ID."))

(defgeneric mitochondria (clang)
  (:documentation "Additional 'foreign' genome required to build phenome."))

(defmethod update-asts ((obj clang) &key clang-mutate-args)
  (with-slots (asts) obj
    (setf asts
          (let* ((json (json:decode-json-from-source
                        (clang-mutate obj (cons :json clang-mutate-args))))
                 (vec (make-array (length json) :initial-element nil)))
            ;; NOTE: if we can guarantee the ordering of elements from
            ;; clang-mutate we could replace the following with a
            ;; simple `coerce' of a list to a vector.
            (loop for snippet in json
               do (let ((counter (aget :counter snippet)))
                    (when counter
                      (setf (aref vec (1- counter)) snippet))))
            vec))))

(defmethod from-file ((obj clang) path)
  (setf (genome obj) (file-to-string path))
  (setf (ext obj)  (pathname-type (pathname path)))
  (update-asts obj)
  obj)

(defmethod asts ((obj clang))
  (with-slots (asts) obj
    (remove-if {equal 0} (coerce asts 'list))))

(defmethod (setf asts) (new (obj clang))
  (format t "NEW:~S~%" new)
  (with-slots (asts) obj (setf asts new)))

(defmethod get-ast ((obj clang) id)
  (with-slots (asts) obj (aref asts (1- id))))

(defmethod recontextualize ((clang clang) snippet pt)
  (let ((text (bind-free-vars clang snippet pt)))
    (format nil "~a~%"
            (if (is-full-stmt clang pt)
                (add-semicolon-if-needed text)
                text))))

(defun do-not-filter () (lambda (asts) asts))

(defun with-class-filter (class asts)
  (remove-if-not [{equal class} {aget :ast--class } ] asts))

(defun full-stmt-filter (asts)
  (remove-if-not { aget :full--stmt } asts))

(defvar *good-asts-override* nil
  "Override for the return value of the good-asts method.")

(defmethod good-asts ((clang clang))
  (or *good-asts-override* (asts clang)))

(defvar *bad-asts-override* nil
  "Override for the return value of the bad-asts method.")

(defmethod bad-asts ((clang clang))
  (or *bad-asts-override* (asts clang)))

(defun random-stmt (asts)
  (aget :counter (random-elt asts)))

(defmethod pick-good ((clang clang))
  (random-stmt (good-asts clang)))

(defmethod pick-bad ((clang clang))
  (random-stmt (bad-asts clang)))

(defmethod get-ast-class ((clang clang) stmt)
  (aget :ast--class (get-ast clang stmt)))

(defun execute-picks (get-asts1 &optional connector get-asts2)
  (let* ((stmt1 (when get-asts1
                  (random-stmt (funcall get-asts1))))
         (stmt2 (when get-asts2
                  (random-stmt (funcall connector stmt1
                                        (funcall get-asts2))))))
    (acons :stmt1 stmt1
       (if stmt2 (acons :stmt2 stmt2 nil) nil))))

(defun op-name (op &key full same)
  (intern
   (concatenate 'string
     (symbol-name (car op))
     (if full "-FULL" "")
     (if (and same (aget :stmt2 (cdr op))) "-SAME" ""))
   "KEYWORD"))

(defvar *clang-full-stmt-bias* 0.75
  "The probability that a mutation will operate on a full statement.")

(defvar *clang-same-class-bias* 0.75
  "The probability that a mutation uses AST class matching.")

(defvar *clang-mutation-cdf*
  (cdf (uniform-probability '(:cut :insert :swap :replace)))
  "The basic clang mutations probability distribution, as a CDF.")

(defmethod mutate ((clang clang))
  (unless (> (size clang) 0)
    (error 'mutate :text "No valid IDs" :obj clang))
  (let* ((full-stmt  (random-bool :bias *clang-full-stmt-bias*))
         (same-class (random-bool :bias *clang-same-class-bias*))
         (mutation (random-elt '(:cut :insert :swap :replace))))

    (labels ((filter (asts) (if full-stmt (full-stmt-filter asts) asts)))
      (let* ((then (if same-class
                       (lambda (stmt asts)
                         (with-class-filter (get-ast-class clang stmt) asts))
                       (lambda (stmt asts) (declare (ignorable stmt)) asts)))
             (good (lambda () (filter (good-asts clang))))
             (bad  (lambda () (filter (bad-asts  clang))))
             (todo
              (ecase mutation
                (:cut     (list bad))
                (:insert  (list bad then good))
                (:swap    (list bad then bad))
                (:replace (list bad then good))))
             (op (cons mutation (apply #'execute-picks todo))))

        (apply-mutation clang op)
        (values clang (cons (op-name op
                                     :full full-stmt
                                     :same same-class)
                            (cdr op)))))))

;; Replace the basic mutation operations with versions that
;; rebind free variables in the appropriate context.
(defmethod recontextualize-mutation-op ((clang clang) op)
  (let* ((mut (car op))
         (properties (cdr op))
         (stmt1  (aget :stmt1  properties))
         (stmt2  (aget :stmt2  properties))
         (value1 (aget :value1 properties)))

    (case mut
      (:insert
       (cons :insert-value
          (list (cons :stmt1 stmt1)
                (cons :value1
                      (recontextualize clang
                                       (if stmt2
                                           (get-ast clang stmt2)
                                           value1)
                                       stmt1)))))
      (:replace
       (cons :set
          (list (cons :stmt1 stmt1)
                (cons :value1
                      (recontextualize clang
                                       (if stmt2
                                           (get-ast clang stmt2)
                                           value1)
                                       stmt1)))))
      (:swap
       (cons :set2
          (list (cons :stmt1 stmt1)
                (cons :value1
                      (recontextualize clang
                                       (get-ast clang stmt2)
                                       stmt1))
                (cons :stmt2 stmt2)
                (cons :value2
                      (recontextualize clang
                                       (get-ast clang stmt1)
                                       stmt2)))))
      (otherwise op))))

(defmethod apply-mutation ((clang clang) op)
  (multiple-value-bind (stdout exit)
      (restart-case
          (clang-mutate clang (recontextualize-mutation-op clang op))
        (skip-mutation ()
          :report "Skip mutation and return nil genome")
        (tidy ()
          :report "Call clang-tidy before re-attempting mutation"
          (clang-tidy clang))
        (mutate ()
          :report "Apply another mutation before re-attempting mutations"
          (mutate clang)))
    (values stdout exit)))

(defmethod apply-mutation :after ((obj clang) op)
  (unless (member (car op) '(:ids :list :json))
    (update-asts obj)))

(defmethod mutation-key ((obj clang) operation)
  ;; Return a list of the operation keyword, and the classes of any
  ;; stmt1 or stmt2 arguments.
  (cons (car operation)
        (mapcar [{aget :ast--class} {get-ast obj} #'cdr]
                (remove-if-not [{member _ (list :stmt1 :stmt2)} #'car]
                               (remove-if-not #'consp operation)))))

(defun extract-clang-genome (full-genome)
  (keep-lines-after-matching "======^======" full-genome))

(defmethod clang-mutate ((clang clang) op)
  (with-temp-file-of (src-file (ext clang)) (genome-string clang)
   (with-temp-file (value1-file) ""
    (with-temp-file (value2-file) ""
     (multiple-value-bind (stdout stderr exit)
       (shell "clang-mutate ~a ~a ~a -- ~a"
          (ecase (car op)
            (:cut                "-cut")
            (:insert             "-insert")
            (:swap               "-swap")
            (:set                "-set")
            (:set2               "-set2")
            (:set-range          "-set-range")
            (:insert-value       "-insert-value")
            (:ids                "-ids")
            (:list               "-list")
            (:json               "-json"))
          (mapconcat
           (lambda (arg-pair)
             (ecase (car arg-pair)
               (:stmt1
                (format nil "-stmt1=~d" (cdr arg-pair)))
               (:stmt2
                (format nil "-stmt2=~d" (cdr arg-pair)))
               (:fields
                (format nil "-fields=~a"
                 (mapconcat (lambda (field)
                  (ecase field
                   (:counter "counter")
                   (:parent--counter "parent_counter")
                   (:ast--class "ast_class")
                   (:src--file--name "src_file_name")
                   (:begin--src--line "begin_src_line")
                   (:begin--src--col "begin_src_col")
                   (:end--src--line "end_src_line")
                   (:end--src--col "end_src_col")
                   (:src--text "src_text")
                   (:guard--stmt "guard_stmt")
                   (:full--stmt "full_stmt")
                   (:unbound--vals "unbound_vals")
                   (:unbound--funs "unbound_funs")
                   (:macros "macros")
                   (:types "types")
                   (:stmt--list "stmt_list")
                   (:binary--file--path "binary_file_path")
                   (:begin--addr "begin_addr")
                   (:end--addr "end_addr")
                   (:binary--contents "binary_contents")))
                  (cdr arg-pair)
                  ",")))
               (:value1
                (string-to-file (cdr arg-pair) value1-file)
                (format nil "-file1=~a" value1-file))
               (:value2
                (string-to-file (cdr arg-pair) value2-file)
                (format nil "-file2=~a" value2-file))
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
         (error 'mutate
                :text (if (find exit '(131 132 134 136 139))
                          "clang-mutate core dump"
                          "clang-mutate non-zero exit")
                :obj clang
                :operation op))
       (values
        (if (member (car op) '(:ids :list :json))
            stdout
            (extract-clang-genome stdout))
        exit))))))

(defclass source-location ()
  ((line :initarg :line :accessor line :initform nil)
   (column :initarg :column :accessor column :initform nil)))

(defmethod source-< ((a source-location) (b source-location))
  (or (< (line a) (line b))
      (and (= (line a) (line b))
           (< (column a) (column b)))))

(defmethod source-<= ((a source-location) (b source-location))
  (or (< (line a) (line b))
      (and (= (line a) (line b))
           (<= (column a) (column b)))))

(defmethod source-> ((a source-location) (b source-location))
  (or (> (line a) (line b))
      (and (= (line a) (line b))
           (> (column a) (column b)))))

(defmethod source->= ((a source-location) (b source-location))
  (or (> (line a) (line b))
      (and (= (line a) (line b))
           (>= (column a) (column b)))))

(defmethod asts-containing-source-location ((obj clang) (loc source-location))
  (remove-if-not (lambda (snippet)
                   (and (<= (make-instance 'source-location
                             :line (aget :begin--src--line snippet)
                             :column (aget :begin--src--col snippet))
                            loc)
                        (>= (make-instance 'source-location
                             :line (aget :end--src--line snippet)
                             :column (aget :end--src--col snippet))
                            loc)))
                 (asts obj)))

(defmethod asts-in-source-range ((obj clang)
                                 (beg source-location)
                                 (end source-location))
  (remove-if-not (lambda (snippet)
                   (or (<= (make-instance 'source-location
                             :line (aget :begin--src--line snippet)
                             :column (aget :begin--src--col snippet))
                           end)
                       (>= (make-instance 'source-location
                             :line (aget :end--src--line snippet)
                             :column (aget :end--src--col snippet))
                           beg)))
                 (asts obj)))

(defmethod line-breaks ((clang clang))
  (cons 0 (loop :for char :in (coerce (genome-string clang) 'list) :as index
                :from 0
                :when (equal char #\Newline) :collect index)))

(defmethod is-parent-ast? ((clang clang) possible-parent-ast ast)
  (cond ((= (aget :counter possible-parent-ast)
            (aget :counter ast)) t)
        ((= (aget :parent--counter ast) 0) nil)
        (t (is-parent-ast? clang
                           possible-parent-ast
                           (get-ast clang (aget :parent--counter ast))))))

(defmethod nesting-depth ((clang clang) index &optional orig-depth)
  (let ((depth (or orig-depth 0)))
    (if (= 0 index)
        depth
        (nesting-depth clang (enclosing-block clang index) (1+ depth)))))

(defmethod enclosing-block ((clang clang) index &optional child-index)
  (if (= index 0) (values  0 child-index)
    (let* ((ast (get-ast clang index))
           (is-block (equal (aget :ast--class ast) "CompoundStmt")))
      (if (and is-block child-index)
          (values index child-index)
          (enclosing-block clang (aget :parent--counter ast) index)))))

(defmethod is-full-stmt ((clang clang) stmt)
  (and stmt (equal stmt (enclosing-full-stmt clang stmt))))

(defmethod enclosing-full-stmt ((clang clang) index &optional child-index)
  (if (= index 0) nil
    (let* ((ast (get-ast clang index))
           (is-block (equal (aget :ast--class ast) "CompoundStmt")))
      (if (and is-block child-index)
          child-index
          (enclosing-full-stmt clang (aget :parent--counter ast) index)))))

(defun get-entry-after (item list)
  (cond ((null list) nil)
        ((not (equal (car list) item)) (get-entry-after item (cdr list)))
        ((null (cdr list)) nil)
        (t (cadr list))))

(defun get-entry-before (item list &optional saw)
  (cond ((null list) nil)
        ((equal (car list) item) saw)
        (t (get-entry-before item (cdr list) (car list)))))

(defmethod block-successor ((clang clang) raw-index)
  (let* ((index (enclosing-full-stmt clang raw-index))
         (block-index (enclosing-block clang index))
         (the-block (get-ast clang block-index))
         (the-stmts (if (= 0 block-index) nil
                        (aget :stmt--list the-block))))
    (get-entry-after index the-stmts)))

(defmethod block-predeccessor ((clang clang) raw-index)
  (let* ((index (enclosing-full-stmt clang raw-index))
         (block-index (enclosing-block clang index))
         (the-block (get-ast clang block-index))
         (the-stmts (if (= 0 block-index) nil
                        (aget :stmt--list the-block))))
    (get-entry-before index the-stmts)))

(defmethod get-ast-text ((clang clang) stmt)
  (json-string-unescape (aget :src--text (get-ast clang stmt))))

(defun add-semicolon-if-needed (text)
  (if (equal text "") ";"
      ;; Add a semicolon unless the text ends in a } (CompoundStmts, etc)
      ;; or already includes a semicolon (only seen for DeclStmts).
      (if (find (char text (1- (length text)))
                (list #\} #\;))
          text
          (concatenate 'string text ";"))))

(defun process-full-stmt-text (snippet)
  (let ((text (json-string-unescape (aget :src--text snippet))))
    (add-semicolon-if-needed text)))

(defmethod full-stmt-text ((clang clang) raw-index)
  (process-full-stmt-text (get-ast clang
                                   (enclosing-full-stmt clang raw-index))))

(defmethod full-stmt-info ((clang clang) raw-index)
  (let* ((index (enclosing-full-stmt clang raw-index)))
    (if (or (null index) (= 0 index))
        nil
        (get-ast clang index))))

(defmethod full-stmt-successors
    ((clang clang) index &optional do-acc acc blocks)
  (if (or (null index) (= 0 index))
      ;; We've made it to the top-level scope; return the accumulator.
      (reverse (if (null acc)
                   blocks
                   (cons acc blocks)))
      ;; Not at the top-level scope yet; accumulate this statement/block.
      (let* ((next-stmt (block-successor clang index))
             (snippet (full-stmt-info clang index))
             (new-acc (if do-acc (cons snippet acc) acc)))
        (if next-stmt
            ;; We're not the last statement of the block. Accumulate
            ;; this snippet and move on to the next one.
            (full-stmt-successors clang next-stmt t
                                  new-acc
                                  blocks)
            ;; We are the last statement in this block; move up a
            ;; scope and push the accumulated statements onto the
            ;; block stack.
            (full-stmt-successors
             clang (enclosing-full-stmt clang (enclosing-block clang index)) nil
             '()
             (cons (reverse new-acc) blocks))))))

(defun create-sequence-snippet (scopes)
  (let ((funcs  (make-hash-table :test 'equal))
        (macros (make-hash-table :test 'equal))
        (types  (make-hash-table :test 'equal))
        (vars   (make-hash-table :test 'equal))
        (stmts  '())
        (source (intercalate (format nil "~%}~%")
                   (loop for scope in scopes for k from 0
                      collecting (unlines
                         (mapcar #'process-full-stmt-text scope))))))
    (loop for scope in scopes for scope-depth from 0 do (progn
      (loop for stmt in scope do (progn
        (setf stmts (cons (aget :counter stmt) stmts))
        (list->ht (aget :types         stmt) types)
        (list->ht (aget :macros        stmt) macros)
        (list->ht (aget :unbound--funs stmt) funcs)
        (loop for var-def in (aget :unbound--vals stmt)
           do (let* ((var (first var-def))
                     (already-seen (gethash var vars nil)))
                (when (not already-seen)
                  (setf (gethash var vars) scope-depth))))))))

    (alist :src--text (json-string-escape source)
           :unbound--vals (ht->list vars)
           :unbound--funs (ht->list funcs)
           :types  (ht->list types)
           :macros (ht->list macros)
           :stmts stmts)))

(defmethod update-mito-from-snippet ((clang clang) snippet)
  (let ((functions (aget :UNBOUND--FUNS snippet)))
    (loop for f in functions
       do (add-includes-for-function (mitochondria clang) f)))

  (loop for type in (aget :TYPES snippet)
     do (add-type (mitochondria clang) type))

  (let ((macros (aget :MACROS snippet)))
    (loop for macro in macros
       do (add-macro (mitochondria clang)
                     (first macro)
                     (second macro)))))

(defun nonempty-lines (text)
  (remove-if (lambda (x) (string= x ""))
             (split-sequence #\Newline text)))

(defmethod get-vars-in-scope ((clang clang) pt &optional keep-globals)
  (gethash 0 (get-indexed-vars-in-scope clang pt keep-globals)))

(defmethod get-indexed-vars-in-scope ((clang clang) pt &optional keep-globals)
  (let ((index-table (make-hash-table :test 'equal))
        (max-index 0))
    (with-temp-file-of (src (ext clang)) (genome-string clang)
      (loop :for line
         :in (nonempty-lines
              (shell "clang-mutate -get-scope=~a -stmt1=~a ~a -- ~{~a~^ ~}"
                     20
                     pt
                     src (flags clang)))
         for index from 0
         do (setf (gethash index index-table)
                  (cdr (split-sequence #\Space line))
                  max-index index)))
    ;; Merge variables downward, so that every index-1 variable appears in
    ;; the index-0 list etc.
    (when (and (< 1 max-index) (not keep-globals))
        (setf max-index (1- max-index)))
    (loop for index from max-index downto 1
       do (let ((vars-n (gethash index index-table))
                (vars-n-minus-1 (gethash (1- index) index-table)))
            (setf (gethash (1- index) index-table)
                  (concatenate 'list vars-n-minus-1 vars-n))))
    index-table))

(defmethod bind-free-vars ((clang clang) snippet pt &optional keep-globals)
  (let* ((raw-code    (aget :src--text snippet))
         (free-vars   (make-hash-table :test 'equal))
         (respect-depth (aget :respect--depth snippet))
         (scope-vars (get-indexed-vars-in-scope clang pt keep-globals)))
    (list->ht (aget :unbound--vals snippet) free-vars)
    (json-string-unescape
     (apply-replacements
      (loop for var being the hash-keys of free-vars
         using (hash-value index)
         collecting
           (cons var (or (random-elt-with-decay
                          (gethash (if respect-depth index 0) scope-vars) 0.3)
                         (format nil "/* no bound vars in scope at depth ~a */"
                                 index))))
      raw-code))))

(defmethod nth-enclosing-block ((clang clang) depth stmt)
  (let ((the-block (enclosing-block clang stmt)))
    (if (>= 0 depth) the-block
        (nth-enclosing-block clang (1- depth) the-block))))

(defmethod prepare-sequence-snippet ((clang clang) depth full-seq)
  (let* ((initial-seq (loop for scope in full-seq
                        for i from 0 to (1- depth)
                        collecting scope))
         (last-seq (nth depth full-seq))
         (tail-size (if (null initial-seq)
                        (1+ (random (length last-seq)))
                        (random (1+ (length last-seq)))))
         (init (if (null initial-seq)
                   (car last-seq)
                   (caar initial-seq)))
         (last (loop for stmt in last-seq
                  for i from 1 to tail-size
                  collecting stmt)))
    (acons   :stmt1 (aget :counter init)
      (acons :stmt2 (if (= 0 tail-size)
                        (nth-enclosing-block clang (1- depth)
                                             (aget :counter init))
                        (aget :counter (last-elt last)))
        (acons :respect--depth t
               (create-sequence-snippet (append initial-seq (list last))))))))

;; Perform 2-point crossover. The second point will be within the same
;; function as the first point, but may be in an enclosing scope.
;; The number of scopes exited as you go from the first crossover point
;; to the second crossover point will be matched between a and b.
;; Free variables are rebound in such a way as to ensure that they are
;; bound to variables that are declared at each point of use.
(defmethod crossover-2pt-outward ((a clang) (b clang))
  (let ((a-begin (enclosing-full-stmt a (pick-bad a)))
        (b-begin (enclosing-full-stmt b (pick-bad b)))
        (variant (copy a)))

    (if (and a-begin b-begin)
        ;; The selected initial crossover points are valid; choose a
        ;; nesting depth and find the second crossover points.
        (let* ((depth (random (min (nesting-depth a a-begin)
                                   (nesting-depth b b-begin))))
               (a-snippet (prepare-sequence-snippet a
                            depth (full-stmt-successors a a-begin t)))
               (b-snippet (prepare-sequence-snippet b
                            depth (full-stmt-successors b b-begin t))))

          ;; Now execute the crossover, replacing a-snippet in variant
          ;; with the recontextualized b-snippet.
          (update-mito-from-snippet variant b-snippet)
          (apply-mutation
           variant
           (cons :set-range
                 (list
                  (cons :stmt1 (aget :stmt1 a-snippet))
                  (cons :stmt2 (aget :stmt2 a-snippet))
                  (cons :value1 (bind-free-vars variant b-snippet a-begin)))))
          (values variant
                  (list (aget :stmt1 a-snippet) (aget :stmt2 a-snippet))
                  (list (aget :stmt1 b-snippet) (aget :stmt2 b-snippet))))

        ;; The selected initial crossover points were not valid; return a
        ;; copy of a.
        (values variant nil nil))))

;; Perform crossover by selecting a single AST from a and b to cross.
;; Free variables are recontextualized to the insertion point.
(defmethod crossover-single-stmt ((a clang) (b clang))
  (let ((a-begin (enclosing-full-stmt a (pick-bad a)))
        (b-begin (enclosing-full-stmt b (pick-bad b)))
        (variant (copy a)))
    (if (and a-begin b-begin)
        (let ((b-snippet (create-sequence-snippet
                          (list (list (get-ast b b-begin))))))
          (update-mito-from-snippet variant b-snippet)
          (apply-mutation
           variant
           (cons :set-range
                 (list
                  (cons :stmt1 a-begin)
                  (cons :stmt2 a-begin)
                  (cons :value1 (bind-free-vars variant b-snippet a-begin)))))
          (values variant a-begin b-begin))
        (values variant nil nil))))

(defmethod crossover ((a clang) (b clang))
  (let ((style (random-elt (list #'crossover-single-stmt
                                 #'crossover-2pt-outward))))
    (apply style (list a b))))

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
              (shell
               "clang-tidy -fix -fix-errors -checks=~{~a~^,~} ~a -- ~a 1>&2"
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
            (if (zerop exit) (file-to-string src) (genome-string clang))))))

(defmethod clang-format ((obj clang) &optional style)
  ;; STYLE may be one of LLVM, Google, Chromium, Mozilla, WebKit.
  (setf (genome-string obj)
        (with-temp-file-of (src (ext obj)) (genome-string obj)
          (multiple-value-bind (stdout stderr exit)
              (shell "clang-format ~a ~a "
                     (if style (format nil "-style=~a" style) "")
                     src)
            (declare (ignorable stderr))
            (if (zerop exit) stdout (genome-string obj))))))

(defmethod (setf genome-string) (text (clang clang))
  (setf (genome clang) (extract-clang-genome text)))

(defmethod lines ((obj clang))
  (split-sequence '#\Newline (genome-string obj)))

(defmethod (setf lines) (new (obj clang))
  (setf (genome-string obj) (format nil "~{~a~^~%~}" new)))
