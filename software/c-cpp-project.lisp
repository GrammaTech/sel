(defpackage :software-evolution-library/software/c-cpp-project
  (:nicknames :sel/software/c-cpp-project :sel/sw/c-cpp-project)
  (:use :gt/full
        :functional-trees/attrs
        :software-evolution-library
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/c
        :software-evolution-library/software/cpp
        :software-evolution-library/software/project
        :software-evolution-library/software/directory))

(in-package :software-evolution-library/software/c-cpp-project)
(in-readtable :curry-compose-reader-macros)

#+(or :TREE-SITTER-C :TREE-SITTER-CPP)
(progn

(defun find-symbol-table-from-include (project include-ast)
  (labels ((trim-path-string (path-ast)
             "Return the text of PATH-AST with the quotes around it removed."
             (remove #\" (remove #\" (text path-ast)
                                 :count 1)
                     :from-end t
                     :count 1))
           (process-system-header (path-ast)
             ;; TODO: use the clang header stuff?
             ;;       Just return an empty map for now.
             (declare (ignorable path-ast))
             (empty-map))
           (process-relative-header (path-ast)
             "Get the corresponding symbol table for the relative path
              represented by PATH-AST."
             (if-let ((software
                       (aget (trim-path-string path-ast)
                             (evolve-files project)
                             :test #'equal)))
               (symbol-table software (empty-map))
               (empty-map))))
    (ematch include-ast
      ((c/cpp-preproc-include
        (c/cpp-path (and path (c/cpp-string-literal))))
       (process-relative-header path))
      ((c/cpp-preproc-include
        (c/cpp-path (and path (c/cpp-system-lib-string))))
       (process-system-header path)))))

(defmethod symbol-table ((node c/cpp-preproc-include) &optional in)
  (declare (ignore in))
  (let ((root (attrs-root *attrs*)))
    (if (typep root 'directory-project)
        (find-symbol-table-from-include root node)
        (call-next-method))))

) ; #+(or :TREE-SITTER-C :TREE-SITTER-CPP)
