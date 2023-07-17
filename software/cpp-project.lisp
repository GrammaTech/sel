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

(defparameter *cpp-module-extensions*
  '(;; Module unit extensions. Visual Studio uses .ixx, Clang
    ;; uses the extensions ending with -m.
    "ixx" "cppm" "ccm" "cxxm" "c++m"))

(defparameter *cpp-extensions*
  `("h" "hpp" "cpp" "cp" "cc" "cxx" ,@*cpp-module-extensions*)
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

(defun relative-module-defaults (importing-path
                                 importing-name
                                 imported-name)
  "Compute the base pathname (no extension) for an imported module in
the same directory as IMPORTING-PATH.

Note IMPORTING-NAME is only needed when IMPORTED-NAME is a module
partition."
  (declare (pathname importing-path)
           ((or string null) importing-name)
           (string imported-name))
  (let* ((file-name
          (if (string^= ":" imported-name)
              (progn
                (unless importing-name
                  (error "Cannot import a module partition directly"))
                (string+ importing-name "-" (drop-prefix ":" imported-name)))
              imported-name)))
    (make-pathname :name file-name
                   :defaults importing-path
                   :type nil)))

(defun find-relative-module (defaults)
  "Find an imported module based on DEFAULTS."
  (some (lambda (type)
          (file-exists-p
           (make-pathname :defaults defaults
                          :type type)))
        *cpp-module-extensions*))

(defun find-module (defaults list &key (key #'identity))
  "Find a module based on DEFAULTS in LIST, a list of pathname designators."
  (let ((target-name (pathname-name defaults)))
    (block nil
      (with-item-key-function (key)
        (dolist (item list)
          (let ((path (pathname (key item))))
            (when (and (equal (pathname-name path)
                              target-name)
                       (member (pathname-type path)
                               *cpp-module-extensions*
                               :test #'equal))
              (return item))))))))

(defun find-project-module (project defaults)
  "Find a module that satisfies DEFAULTS in PROJECT."
  (cdr (find-module defaults (evolve-files project) :key #'car)))

) ; #+:TREE-SITTER-CPP
