;;; clang-w-fodder.lisp

;;; clang software representation with a
;;; JSON 'database' containing AST entries
;;; as fodder for the evolution process.

;;; Each AST entry in the JSON 'database'
;;; contains source text and, where applicable,
;;; the corresponding binary source code.
(in-package :software-evolution)

(defvar *json-database* (make-hash-table :test 'equal)
  "A database of source code snippets, grouped by AST class name.")

(defvar *json-database-bins* nil
  "The inverse cumulative distribution function for the AST class name of
a uniformly selected element of the JSON database.")

(defvar *json-database-full-stmt-bins* nil
  "The inverse cumulative distribution function for the AST class name of
a uniformly selected element of the JSON database.")

(defvar *type-database* nil
  "A database of user-defined types.")

(defun select-random-bin ()
  (let ((binprob (random 1.0)))
    (cdr (find-if (lambda (datum)
                    (<= binprob (car datum))) *json-database-bins*))))

(defun select-random-full-stmt-bin ()
  (let ((binprob (random 1.0)))
    (cdr (find-if (lambda (datum)
                    (<= binprob (car datum)))
                  *json-database-full-stmt-bins*))))

(defun random-full-stmt-snippet ()
  (random-elt (gethash (select-random-full-stmt-bin)
                       *json-database*
                       '("/* bad snippet */"))))

(defun random-snippet ()
  (assert (not (null *json-database-bins*)) (*json-database-bins*)
          "*json-database-bins* must be precomputed.")
  (random-elt (gethash (select-random-bin)
                       *json-database*
                       '("/* bad snippet */"))))

(defun populate-database-bins ()
  ;; All database bins
  (setf *json-database-bins*
        (compute-icdf-with-filter (lambda (x) t)))

  ;; "Full statement" database bins
  (let ((ht (make-hash-table :test 'equal)))
    (loop for bin in '("CompoundStmt" "DeclStmt" "BinaryOperator" "IfStmt"
                       "CallExpr" "UnaryOperator" "ForStmt" "BreakStmt"
                       "CompoundAssignOperator" "ConditionalOperator")
       do (setf (gethash bin ht) t))
    (setf *json-database-full-stmt-bins*
          (compute-icdf-with-filter (lambda (x) (gethash x ht nil))))))

(defun compute-icdf-with-filter (filter)
  (let ((total 0)
        (totalp 0))

    (setq bins '())
    (maphash (lambda (k v)
               (when (funcall filter k)
                 (setf total (+ total (length v)))))
             *json-database*)

    (maphash (lambda (k v)
               (when (funcall filter k)
                 (setq totalp (+ totalp (/ (length v) total)))
                 (setq bins
                       (cons (cons totalp k) bins))))
             *json-database*)

    (reverse bins)))

(define-software clang-w-fodder (clang) 
  ((diff-addresses :initarg :diff-addresses 
                   :accessor diff-addresses 
                   :initform nil
                   :copier copy-seq)))

(defmethod from-file :before ((obj clang-w-fodder) path)
  (assert (not (null *json-database-bins*))))

(defmethod (setf fitness-extra-data) (extra-data (clang-w-fodder clang-w-fodder))
  (setf (diff-addresses clang-w-fodder) extra-data)
  (call-next-method))

(defun clang-w-fodder-setup-db (json-db-path)
  ;; Clobber the existing database
  (setq *json-database* (make-hash-table :test 'equal))
  (setq *type-database* (make-hash-table :test 'equal))
  (setq *json-database-bins* '())

  ;; Load the snippet database and classify by AST class.
  (dolist (snippet (with-open-file (json-stream json-db-path)
                     (json:decode-json-from-source json-stream)))
    (let ((ast-class (aget :AST--CLASS snippet)))
      (if ast-class
          ;; This entry describes a code snippet
          (let ((cur (gethash ast-class *json-database*)))
            (setf (gethash ast-class *json-database*) (cons snippet cur)))
          ;; This entry describes a type
          (let ((type-id (aget :HASH snippet)))
            (setf (gethash type-id *type-database*) snippet)))))

  ;; Compute the bin sizes so that (random-snippet) becomes useful.
  (populate-database-bins))


;; Returns multiple values: (stmt-class-string has-semicolon)
(defmethod get-stmt-info ((clang-w-fodder clang-w-fodder) pt)
  (with-temp-file-of (src (ext clang-w-fodder)) (genome-string clang-w-fodder)
     (multiple-value-bind (stdout stderr exit)
         (shell "clang-mutate -get-info -stmt1=~a ~a -- ~{~a~^ ~}"
                pt
                src
                (flags clang-w-fodder))
       (apply #'values
        (let ((result (nonempty-lines stdout)))
          (if (not (equal (length result) 3))
              '("[unknown-class]" nil pt)
              (list (first result)
                    (equal (second result) "has-semi")
                    (parse-integer (third result)))))))))

(defmethod pick-any-json ((clang-w-fodder clang-w-fodder) pt)
  (let ((stmt-info (get-stmt-info clang-w-fodder pt)))
    (multiple-value-bind (ast-class has-semi full-stmt) stmt-info
      (prepare-code-snippet clang-w-fodder
                            pt
                            (if has-semi
                                (random-full-stmt-snippet)
                                (random-snippet))
                            has-semi
                            ))))


(defmethod pick-full-stmt-json ((clang-w-fodder clang-w-fodder) pt)
  (prepare-code-snippet clang-w-fodder
                        pt
                        (random-full-stmt-snippet)
                        t))

(defmethod pick-json-by-class ((clang-w-fodder clang-w-fodder) pt)
  (let ((stmt-info (get-stmt-info clang-w-fodder pt)))
    (multiple-value-bind (ast-class has-semi full-stmt) stmt-info
      (let ((asts (gethash ast-class *json-database* '())))
        (prepare-code-snippet clang-w-fodder
                              pt
                              (if (null asts)
                                  (random-snippet)
                                  (random-elt asts)) 
                              has-semi)))))

(defmethod update-mito-from-snippet ((clang-w-fodder clang-w-fodder)
                                     snippet)
  (let ((functions (aget :UNBOUND--FUNS snippet)))
    (loop for f in functions
       do (add-includes-for-function (mitochondria clang-w-fodder) f)))

  (loop for type in (aget :TYPES snippet)
     do (add-type (mitochondria clang-w-fodder) type))

  (let ((macros (aget :MACROS snippet)))
    (loop for macro in macros
       do (add-macro (mitochondria clang-w-fodder)
                     (first macro)
                     (second macro)))))

(defmethod prepare-code-snippet ((clang-w-fodder clang-w-fodder)
                                 pt
                                 snippet
                                 has-semi)
  (update-mito-from-snippet clang-w-fodder snippet)
  
  (let ((raw-code   (aget :SRC--TEXT snippet))
        (free-vars  (mapcar #'car (aget :UNBOUND--VALS snippet)))
        (scope-vars (get-vars-in-scope clang-w-fodder pt)))
    (concatenate 'string
      (json-string-unescape
       (apply-replacements
        (map 'list
             (lambda (x)
               (cons x (or (random-elt-with-decay scope-vars 0.5)
                           "/* no bound vars */")))
             free-vars) raw-code))
      (if has-semi (format nil ";~%") ""))))

(defmethod extend-to-enclosing ((clang-w-fodder clang-w-fodder) pt)
    (multiple-value-bind (ast-class has-semi full-stmt)
        (get-stmt-info clang-w-fodder pt)
      (declare (ignorable ast-class has-semi))
      full-stmt))

(defmethod mutate ((clang-w-fodder clang-w-fodder))
  (unless (> (size clang-w-fodder) 0)
    (error 'mutate :text "No valid IDS" :obj clang-w-fodder))
  (unless (> (hash-table-size *json-database*) 0)
    (error 'mutate :text "No valid JSON 'database' for fodder"
           :obj clang-w-fodder))

  (setf (fitness clang-w-fodder) nil)

  (let* ((good  (pick-good clang-w-fodder))
         (bad   (pick-bad  clang-w-fodder))
         (good-stmt (extend-to-enclosing clang-w-fodder good))
         (bad-stmt  (extend-to-enclosing clang-w-fodder bad))
         (op (case (random-elt '(cut insert swap
                                  set-value insert-value
                                  insert-full-stmt
                                  cut-full-stmt
                                  swap-full-stmt))
               (cut            
                 `(:cut            
                    (:stmt1 . ,bad)))
               (cut-full-stmt  
                 `(:cut-full-stmt  
                    (:stmt1 . ,bad-stmt)))
               (insert         
                 `(:insert         
                    (:stmt1 . ,bad) 
                    (:stmt2 . ,good)))
               (swap           
                 `(:swap           
                    (:stmt1 . ,bad) 
                    (:stmt2 . ,good)))
               (swap-full-stmt 
                 `(:swap-full-stmt 
                    (:stmt1 . ,bad-stmt) 
                    (:stmt2 . ,good-stmt)))
               (set-value     
                 `(:set-value    
                    (:stmt1 . ,good)
                    (:value . ,(pick-json-by-class clang-w-fodder 
                                                     good))))
               (insert-value  
                 `(:insert-value
                    (:stmt1 . ,good)
                    (:stmt2 . ,(pick-any-json clang-w-fodder 
                                              good))))
               (insert-full-stmt
                 `(:insert-full-stmt
                    (:stmt1 . ,good)
                    (:value . ,(pick-full-stmt-json clang-w-fodder 
                                                    good)))))))
    (apply-mutation clang-w-fodder op)
    (values clang-w-fodder op)))

(defvar *targeted-mutation-chance* 0.75
  "Probability of performing a targeted vs. random mutation.")

(defmethod pick-bad((clang-w-fodder clang-w-fodder))
  (if (and (diff-addresses clang-w-fodder) 
           (< (random 1.0) *targeted-mutation-chance*))
    (pick-bad-targetted clang-w-fodder)
    (call-next-method)))

(defmethod pick-bad-targetted((clang-w-fodder clang-w-fodder))
  "Return the AST of a binary-difference inducing AST in clang-w-fodder"
  ;; Loop until we find a diff range corresponding to one or more ASTs
  ;; or we run out of diffs to consider.
  (loop
    (let* ((target-diff (random-elt (diff-addresses clang-w-fodder)))
           (bad-asts (to-ast-list-containing-bin-range 
                       clang-w-fodder
                         (aget :begin-addr target-diff)
                         (aget :end-addr target-diff))))
      ;; If ASTs could be found for the diff, set the counter
      ;; Otherwise, remove the diff from consideration.
      (if bad-asts
        (return (aget :counter (random-elt bad-asts)))
        (setf (diff-addresses clang-w-fodder)
              (remove target-diff (diff-addresses clang-w-fodder)))))
    
    ;; When there are no diffs left to consider, return a random
    ;; element
    (when (not (diff-addresses clang-w-fodder))
      (return (random (size clang-w-fodder))))))

(defmethod apply-mutation ((clang-w-fodder clang-w-fodder) op)
  (clang-mutate clang-w-fodder op))
