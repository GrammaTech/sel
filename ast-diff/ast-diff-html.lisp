;;; ast-diff-html.lisp --- render ast diffs to html
(defpackage :software-evolution-library/ast-diff-html
  (:nicknames :sel/ast-diff-html)
  (:use :common-lisp :cl-who)
  (:export :diff-to-html))
(in-package :software-evolution-library/ast-diff-html)

(defun diff-to-html (interface orig-asts edit-script)
  "Generate HTML which shows side-by-side diff.

Shows source text of ORIG-ASTS alongside the result of applying
EDIT-SCRIPT, with highlighting of inserts and deletes.
"
  (labels
      ((render-ast (ast)
         (let ((text (sel/ast-diff::ast-text interface ast)))
           (values (sel/ast-diff::escape-string text) (count #\newline text))))
       (render-diff (asts script)
         (if (null script)
             '(nil nil)
             (destructuring-bind (action . args) (car script)
               (ecase action
                 (:recurse
                  (mapcar #'append
                          (render-diff (cdr (car asts)) args)
                          (render-diff (cdr asts) (cdr script))))
                 (:same
                  (assert (apply #'sel/ast-diff::ast-equal-p interface args))
                  (mapcar #'cons
                          (make-list 2
                                     :initial-element
                                     (render-ast (car asts)))
                          (render-diff (cdr asts) (cdr script))))
                 (:delete
                  (assert (sel/ast-diff::ast-equal-p interface (car asts) args))
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
                 (:insert
                  (multiple-value-bind (text line-count)
                      (render-ast args)
                    (mapcar #'cons
                            (list (format nil "~{~a~}"
                                          (make-list line-count
                                                     :initial-element
                                                     #\newline))
                                  (format nil
                                          "<span class=\"insert\">~a</span>"
                                          text))
                            (render-diff asts (cdr script))))))))))
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
           (render-diff (cdr orig-asts) edit-script))))
