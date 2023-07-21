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
  (:import-from :software-evolution-library/software/tree-sitter
                :symbol-table :exported?)
  (:export :cpp-project))
(in-package :software-evolution-library/software/cpp-project)
(in-readtable :curry-compose-reader-macros)

(defparameter *cpp-module-extensions*
  '(;; Module unit extensions. Visual Studio uses .ixx, Clang
    ;; uses the extensions ending with -m.
    "ixx" "cppm" "ccm" "cxxm" "c++m"))

(defparameter *cpp-implementation-extensions*
  '("cpp" "cp" "cc" "cxx"))

(defparameter *cpp-extensions*
  (append *header-extensions*
          *cpp-implementation-extensions*
          *cpp-module-extensions*)
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

(defun find-relative-module (defaults &key (interface t))
  "Find an imported module based on DEFAULTS."
  (some (lambda (type)
          (file-exists-p
           (make-pathname :defaults defaults
                          :type type)))
        (if interface
            *cpp-module-extensions*
            *cpp-implementation-extensions*)))

(defun find-module (defaults list &key (key #'identity) (interface t))
  "Find a module based on DEFAULTS in LIST, a list of pathname designators."
  (let ((target-name (pathname-name defaults))
        (extensions
         ;; Implementation units do not use module extensions.
         (if interface
             *cpp-module-extensions*
             *cpp-implementation-extensions*)))
    (block nil
      (with-item-key-function (key)
        (dolist (item list)
          (let ((path (pathname (key item))))
            (when (and (equal (pathname-name path)
                              target-name)
                       (member (pathname-type path)
                               extensions
                               :test #'equal))
              (return item))))))))

(defun find-project-module (project defaults &key (interface t))
  "Find a module that satisfies DEFAULTS in PROJECT."
  (cdr (find-module defaults (evolve-files project)
                    :key #'car
                    :interface interface)))

(defun get-surrounding-module (ast)
  (when-let* ((project (attrs-root*))
              (file-ast (find-enclosing 'file-ast project ast))
              (unit (find-if (of-type 'cpp-translation-unit) file-ast)))
    (module? unit)))

(defmethod symbol-table ((ast cpp-module-declaration) &optional in)
  "Anonymous implementation units implicitly import the primary module
interface unit."
  (let ((module (module? ast)))
    (if (typep module 'anonymous-implementation-unit)
        (let* ((defaults
                (relative-module-defaults
                 (enclosing-file-pathname ast)
                 (module-unit-full-name module)
                 (module-unit-full-name (module? ast))))
               (module
                (find-project-module (attrs-root*) defaults))
               (symbol-table
                (symbol-table (genome module) (empty-map))))
          (symbol-table-union ast in (@ symbol-table :exports)))
        (call-next-method))))

(defun import-symbol-table (ast in)
  (when-let* ((imported-name (source-text (cpp-name ast)))
              (project (attrs-root*))
              (base-path (enclosing-file-pathname ast)))
    (let* ((module (get-surrounding-module ast))
           (importing-name
            (if module (module-unit-full-name module) ""))
           (partition? (string^= ":" imported-name))
           (exported? (exported? ast))
           (defaults
            (relative-module-defaults base-path importing-name imported-name))
           (imported-module-software
            (find-project-module project defaults
                                 :interface
                                 (or (not partition?)
                                     (and partition? (not exported?)))))
           (symtab
            (symbol-table (genome imported-module-software)
                          (empty-map))))
      (when partition? (assert module))
      (cond
        ;; A re-exported partition. We can see all definitions.
        ((and partition? exported?)
         (symbol-table-union ast in symtab))
        ;; An implementation partition. We can see all definitions.
        (partition?
         (symbol-table-union ast in (less symtab :export)))
        ;; A non-partition reexport.
        (exported?
         (when-let ((exports (@ symtab :export)))
           (if (exported? ast)
               (symbol-table-union ast in (with exports :export exports))
               (symbol-table-union ast in exports))))
        (t (@ symtab :export))))))

(defmethod symbol-table ((ast cpp-import-declaration) &optional in)
  (or (import-symbol-table ast in)
      (call-next-method)))

) ; #+:TREE-SITTER-CPP
