;;; cpp-project.lisp --- C++ Projects
;;;
(defpackage :software-evolution-library/software/cpp-project
  (:nicknames :sel/software/cpp-project :sel/sw/cpp-project)
  (:use :gt/full
        :cl-json
        :functional-trees/attrs
        :software-evolution-library
        :software-evolution-library/software/simple
        :software-evolution-library/software/parseable
        :software-evolution-library/software/project
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/compilable
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/c-cpp-project
        :software-evolution-library/software/directory)
  (:export :cpp-project))
(in-package :software-evolution-library/software/cpp-project)
(in-readtable :curry-compose-reader-macros)

(defparameter *cpp-extensions*
  '("h" "cpp" "cp" "cc")
  "List of extensions we will consider for evolving.")

(define-software cpp-project (c/cpp-project)
  ()
  (:documentation "Project specialization for c++ software objects."))

(defmethod initialize-instance :after ((project cpp-project) &key)
  (unless (component-class project)
    (setf (component-class project) 'cpp))
  (unless (compiler project)
    (setf (compiler project) "c++")))

(defmethod collect-evolve-files ((project cpp-project))
  (collect-evolve-files* project :extensions *cpp-extensions*))

#+:TREE-SITTER-CPP
(progn

(defmethod multi-declaration-keys ((root cpp-project))
  +cpp-multi-declaration-keys+)

(defmethod attr-missing ((fn-name (eql 'namespace)) (node cpp-ast))
  (let* ((attrs-root (attrs-root *attrs*))
         (parents (get-parent-asts attrs-root node))
         (root (find-if (of-type 'root-ast) parents)))
    (cond
      ;; TODO: having to check for whether it's an extra AST may be a bug from
      ;;       somewhere else.
      ((and (null parents)
            (typep node '(or comment-ast parse-error-ast source-text-fragment)))
       (namespace node ""))
      (root (namespace root ""))
      (t (namespace attrs-root "")))))

) ; #+:TREE-SITTER-CPP
