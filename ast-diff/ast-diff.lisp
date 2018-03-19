;;; ast-diff.lisp --- diffs between ASTs and other tree structures
(defpackage :software-evolution-library/ast-diff
  (:nicknames :sel/ast-diff)
  (:use
   :common-lisp
   :software-evolution-library/utility
   :alexandria
   :cl-arrows
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :cl-who)
  (:export
   :ast-diff
   :apply-edit-script
   :diff-to-html))
(in-package :software-evolution-library/ast-diff)
(in-readtable :curry-compose-reader-macros)

(defun ast-equal-p (ast-a ast-b)
  "Is AST-A structurally equal to AST-B?"
  (or (eq ast-a ast-b)
      (and (stringp ast-a) (stringp ast-b) (string= ast-a ast-b))
      (and (consp ast-a) (consp ast-b)
           (eq (sel:ast-class (car ast-a)) (sel:ast-class (car ast-b)))
           (eq (length ast-a) (length ast-b))
           (every #'ast-equal-p (cdr ast-a) (cdr ast-b)))))

(defun ast-diff (ast-a ast-b)
  "Return a least-cost edit script which transforms AST-A into AST-B.

See APPLY-EDIT-SCRIPT for more details on edit scripts.
"
  ;; Edit scripts are represented by a (length a) x (length b).  Each
  ;; position A,B on the grid stores the diff between (subseq ast-a A)
  ;; and (subseq ast-b B), along with its cost. The problem then
  ;; reduces to finding the least-cost path from the upper-left to
  ;; lower-right corners of the grid.

  ;; Along the way we can make the following moves:
  ;; Delete the head of AST-A (move right in the grid)
  ;; Insert the head of AST-B (move down in the grid)\
  ;; If the head of AST-A and AST-B are equal, move diagonally.
  ;; Recursively edit the head of AST-A to match the head of AST-B,
  ;; and move diagonally.

  ;; Intermediate results are cached a in two-dimensional array to
  ;; avoid redundant computation. Each dimension is padded by 1 to
  ;; simplify boundary conditions.
  (let ((costs (make-array (mapcar [#'1+ #'length] (list ast-a ast-b))
                           :initial-element nil))
        (vec-a (coerce (append ast-a '(nil)) 'vector))
        (vec-b (coerce (append ast-b '(nil)) 'vector)))
    (labels
        ((tree-size (ast)
           (if (listp ast)
               (apply #'+ (mapcar #'tree-size (cdr ast)))
               1))
         (cost (result)
           (if result (second result) infinity))
         (script (result)
           (when result (first result)))
         (extend (result script cost)
           (list (cons script (script result))
                 (+ cost (cost result))))
         (compute-diff (index-a index-b)
           ;; Moving off the grid is illegal/infinite cost.
           (when (some #'>= (list index-a index-b) (array-dimensions costs))
             (return-from compute-diff (list nil infinity)))

           ;; Use memoized value if available
           (&>> (aref costs index-a index-b)
                (return-from compute-diff))

           (let* ((head-a (aref vec-a index-a))
                  (head-b (aref vec-b index-b))
                  (heads-equal (ast-equal-p head-a head-b))
                  (can-recurse (and (consp head-a)
                                    (consp head-b)
                                    (eq (sel:ast-class (car head-a))
                                        (sel:ast-class (car head-b)))))
                  ;; Compute neighbors
                  (down (compute-diff index-a (1+ index-b)))
                  (across (compute-diff (1+ index-a) index-b))
                  ;; Only compute diagonal and recursive costs if
                  ;; needed.
                  (diagonal (when (or heads-equal can-recurse)
                              (compute-diff (1+ index-a) (1+ index-b))))
                  (subtree (when (and can-recurse (not heads-equal))
                             (multiple-value-list
                              (ast-diff (cdr head-a) (cdr head-b)))))
                  ;; Actions
                  (same (when heads-equal
                          (extend diagonal
                                  (list :same head-a head-b)
                                  0)))
                  (recurse (extend diagonal
                                   (cons :recurse (script subtree))
                                   (cost subtree)))
                  (insert (extend down
                                  (cons :insert head-b)
                                  (tree-size (aref vec-b index-b))) )
                  (delete (extend across
                                  (cons :delete head-a)
                                  (tree-size (aref vec-a index-a)))))
             ;; Pick the best action.
             ;; Note: illegal actions will have infinite cost so we
             ;; don't have to consider them specially here.
             (setf (aref costs index-a index-b)
                   (cond
                     ((and (<= (cost same) (cost insert))
                           (<= (cost same) (cost delete)))
                      same)
                     ((and (<= (cost recurse) (cost insert))
                           (<= (cost recurse) (cost delete)))
                      recurse)
                     ((< (cost insert) (cost delete))
                      insert)
                     (t delete))))))
      ;; Pre-fill the grid for the target state.
      (setf (aref costs (1- (length vec-a)) (1- (length vec-b)))
            '(nil 0))
      (values-list (compute-diff 0 0)))))

(defun apply-edit-script (original script)
  "Create an edited AST by applying SCRIPT to ORIGINAL.

An edit script is a sequence of actions, of the following types:
:same A B  : keep the current AST
:insert B  : insert B at the current position
:remove A  : remove the current AST
:recurse S : recursively apply script S to the current AST
"

  (when script
    (destructuring-bind (action . args) (car script)
      (ecase action
        (:recurse (cons (cons (car (car original))
                              (apply-edit-script (cdr (car original))
                                                 args))
                        (apply-edit-script (cdr original) (cdr script))))
        (:same (assert (apply #'ast-equal-p args))
               (cons (car original)
                     (apply-edit-script (cdr original) (cdr script))))
        (:delete (assert (ast-equal-p (car original) args))
                 (apply-edit-script (cdr original) (cdr script)))
        (:insert (cons args
                       (apply-edit-script original (cdr script))))))))

(defun diff-to-html (orig-asts edit-script)
  "Generate HTML which shows side-by-side diff.

Shows source text of ORIG-ASTS alongside the result of applying
EDIT-SCRIPT, with highlighting of inserts and deletes.
"
  (labels
      ((render-ast (ast)
         (let ((text (peel-bananas (sel:source-text ast))))
           (values (escape-string text) (count #\newline text))))
       (render-diff (asts script)
         (destructuring-bind (action . args) (car script)
           (ecase action
             (:return '(nil nil))
             (:recurse (mapcar #'append
                               (render-diff (cdr (car asts)) args)
                               (render-diff (cdr asts) (cdr script))))
             (:same (assert (apply #'ast-equal-p args))
                    (mapcar #'cons
                            (make-list 2
                                       :initial-element
                                       (render-ast (car asts)))
                            (render-diff (cdr asts) (cdr script))))
             (:delete (assert (ast-equal-p (car asts) args))
                      (multiple-value-bind (text line-count)
                          (render-ast (car asts))
                        (mapcar #'cons
                                (list (format nil
                                              "<span class=\"delete\">~a</span>"
                                              text)
                                      (format nil "~{~a~}"
                                              (make-list line-count
                                                         :initial-element
                                                         #\newline)))
                                (render-diff (cdr asts) (cdr script)))))
             (:insert (multiple-value-bind (text line-count)
                          (render-ast args)
                        (mapcar #'cons
                                (list (format nil "~{~a~}"
                                              (make-list line-count
                                                         :initial-element
                                                         #\newline))
                                      (format nil
                                              "<span class=\"insert\">~a</span>"
                                              text))
                                (render-diff asts (cdr script)))))))))
    (apply #'format nil "<!DOCTYPE html>
<html>
  <head>
    <style>
.delete {
  border: 1px solid black;
  background-color: DarkSalmon;
}
.insert {
  border: 1px solid black;
  background-color: MediumSeaGreen;
}
.pre {
  margin: 0px;
  padding: 4px;
  font-size: 10pt;
  color: black;
  background-color: white;
  border: 1px solid black;
  border-radius: 4px;
}
.column {
  border-color: black;
  border-width: 1px;
  float: left;
  width: 48%;
  margin: 0.5%;
}
    </style>
  </head>
  <body>
<div class=\"column\"> <pre class=\"pre\">~{~a~}</pre></div>
<div class=\"column\"> <pre class=\"pre\">~{~a~}</pre></div>
</body>
</html>"
           (render-diff orig-asts edit-script))))
