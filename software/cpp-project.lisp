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
  ;; Make sure these symbols are interned, as they are only defined
  ;; and used inside a feature guard.
  (:intern :relative-module-defaults :find-module)
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
  (let ((file-name
         (cond ((string^= ":" imported-name)
                (unless importing-name
                  (error "Cannot import a module partition outside a module"))
                (string+
                 ;; Drop any trailing partition.
                 (take-until (eqls #\:) importing-name)
                 "-"
                 (drop-prefix ":" imported-name)))
               ;; Implicit import from a module partition implementation.
               ((find #\: imported-name)
                (substitute #\- #\: imported-name))
               (t imported-name))))
    (make-pathname :name file-name
                   :defaults importing-path
                   :type nil)))

(defun find-relative-module (defaults &key (interface t))
  "Find an imported module on the filesystem based on DEFAULTS."
  (some (lambda (type)
          (file-exists-p
           (make-pathname :defaults defaults
                          :type type)))
        (if interface
            *cpp-module-extensions*
            *cpp-implementation-extensions*)))

(defun find-module (defaults list &key (key #'identity))
  "Find a module based on DEFAULTS in LIST, a list of pathname designators."
  (let ((target-name (pathname-name defaults))
        (extensions *cpp-module-extensions*))
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

(defun find-project-module (project defaults)
  "Find a module that satisfies DEFAULTS in PROJECT."
  (cdr (find-module defaults (evolve-files project)
                    :key #'car)))

(defun get-enclosing-module (ast)
  (when-let* ((project (attrs-root*))
              (file-ast (find-enclosing 'file-ast project ast))
              (unit (find-if (of-type 'cpp-translation-unit) file-ast)))
    (module? unit)))

(defun module-declaration-symbol-table (ast)
  (when-let (module (module? ast))
    ;; NB MSVC and Clang seem to differ in how they understand module
    ;; partition implementation files. For MSVC, it appears a module
    ;; partition implementation file is to a module partition
    ;; interface file what a (non-partition) implementation file is to
    ;; a interface file, and it also implicitly imports the partition
    ;; interface file. For Clang, through, module partition units are
    ;; *unique* and *either* interface or implementation files. For
    ;; now we support the MSVC style by looking for interface files
    ;; for *all* implementation files, but since they may not exist we
    ;; need to guard against self-inclusion.
    (when (typep module 'implementation-unit)
      (let* ((*dependency-stack*
              (or *dependency-stack*
                  (list (find-enclosing-software (attrs-root*) ast))))
             (defaults
              (relative-module-defaults
               (enclosing-file-pathname ast)
               (module-unit-full-name module)
               (module-unit-full-name (module? ast))))
             (module
              (find-project-module (attrs-root*) defaults)))
        (update-dependency-graph (attrs-root*) module)
        (unless (eql (genome module)
                     (find-enclosing 'root-ast (attrs-root*) ast))
          (let ((*dependency-stack* (cons module *dependency-stack*)))
            (symbol-table (genome module) (empty-map))))))))

(defmethod symbol-table ((ast cpp-module-declaration) &optional in)
  "Anonymous implementation units (and possibly module partition
implementation units) implicitly import the corresponding interface
unit."
  (if-let (symtab (module-declaration-symbol-table ast))
    (if-let (exports (@ symtab :export))
      (prog1 (symbol-table-union ast in (@ symtab :export))
        ;; Propagate the symbol table to the children (module
        ;; identifiers).
        (call-next-method))
      (call-next-method))
    (call-next-method)))

(defun module-software-symbol-table (imported-module-software)
  (symbol-table (genome imported-module-software)
                (empty-map)))

(defun module-import-symbol-table (ast in)
  (when-let* ((imported-name (source-text (cpp-name ast)))
              (project (attrs-root*))
              (base-path (enclosing-file-pathname ast)))
    (let* ((module (get-enclosing-module ast))
           (importing-name
            (if module (module-unit-full-name module) ""))
           (partition? (string^= ":" imported-name))
           (exported? (exported? ast))
           (defaults
            (relative-module-defaults base-path importing-name imported-name))
           (imported-module-software
            (find-project-module project defaults)))
      (when partition? (assert module))
      (update-dependency-graph (attrs-root*) imported-module-software)
      (let* ((*dependency-stack*
              (cons imported-module-software *dependency-stack*))
             (symtab
              (module-software-symbol-table imported-module-software)))
        (cond
          ;; A re-exported partition. We can see all definitions.
          ((and partition? exported?)
           (symbol-table-union ast in symtab))
          ;; An implementation partition. We can see all definitions.
          (partition?
           (symbol-table-union ast in (less symtab :export)))
          ;; A non-partition reexport.
          (exported?
           (if-let ((exports (@ symtab :export)))
             (symbol-table-union ast in (with exports :export exports))
             in))
          (t (symbol-table-union ast in (@ symtab :export))))))))

(defun header-unit-import-symbol-table (ast in)
  (let* ((symtab
          (find-symbol-table-from-include
           (attrs-root*)
           ast
           :in in
           :global nil
           :header-dirs '(:stdinc)))
         ;; Macro definitions from the header are not exposed.
         (symtab (less symtab :macro))
         (exported? (exported? ast)))
    (if exported?
        (symbol-table-union ast in (with symtab :export symtab))
        (symbol-table-union ast in symtab))))

(defun import-symbol-table (ast in)
  (when-let* ((imported-name (source-text (cpp-name ast))))
    (if (scan "^<.*>$" imported-name)
        ;; Handle a header unit import.
        (header-unit-import-symbol-table ast in)
        ;; Handle a module import.
        (module-import-symbol-table ast in))))

(defmethod symbol-table ((ast cpp-import-declaration) &optional in)
  (let ((*dependency-stack*
         (or *dependency-stack*
             (list
              (find-enclosing-software (attrs-root*) ast)))))
    (if-let (symtab (import-symbol-table ast in))
      (prog1 symtab
        (call-next-method ast symtab))
      (call-next-method))))

) ; #+:TREE-SITTER-CPP
