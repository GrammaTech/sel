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
  (:local-nicknames
   (:dbg :software-evolution-library/utility/debug))
  ;; Make sure these symbols are interned, as they are only defined
  ;; and used inside a feature guard.
  (:intern :relative-module-defaults :find-module)
  (:local-nicknames
   (:dbg :software-evolution-library/utility/debug))
  (:export :cpp-project))
(in-package :software-evolution-library/software/cpp-project)
(in-readtable :curry-compose-reader-macros)

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

(defun restrict-map (base-map filter-map)
  "Remove keys from BASE-MAP not present in FILTER-MAP, recursively.
Like `fset:restrict', but recursive."
  (map-intersection base-map filter-map
                    (lambda (val1 val2)
                      (if (and (typep val1 'fset:map)
                               (typep val2 'fset:map))
                          (restrict-map val1 val2)
                          val1))))

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
  "Compute a list of potential base pathnames (no extension) for an
imported module in the same directory as IMPORTING-PATH.

For dotted modules (x.y) we look in both in ./x/y and ./x.y.

Note IMPORTING-NAME is only needed when IMPORTED-NAME is a module
partition."
  (declare (pathname importing-path)
           ((or string null) importing-name)
           (string imported-name))
  (labels ((move-extension-to-name (pathname)
             "If PATHNAME has an extension, make it part of the file name."
             (with-accessors ((type pathname-type)
                              (name pathname-name))
                 pathname
               (if type
                   (make-pathname
                    :defaults pathname
                    :type nil
                    :name (string+ name "." type))
                   pathname)))
           (relative-module-defaults (dot-replacement)
             (let* ((dot-replacement (character dot-replacement))
                    (importing-name
                      (substitute dot-replacement #\. importing-name))
                    (imported-name
                      (substitute dot-replacement #\. imported-name))
                    (module-file-name
                      (cond ((string^= ":" imported-name)
                             (unless importing-name
                               (error "Cannot import a module partition outside a module"))
                             (string+
                              ;; Drop any trailing partition.
                              (take (or (position #\: importing-name :from-end t)
                                        (length importing-name))
                                    importing-name)
                              "-"
                              (drop-prefix ":" imported-name)))
                            ;; Implicit import from a module partition implementation.
                            ((find #\: imported-name)
                             (substitute #\- #\: imported-name))
                            (t imported-name)))
                    (module-pathname
                      ;; Don't override the extension of
                      ;; importing-path if there are dots in
                      ;; module-file-name.
                      (move-extension-to-name
                       (pathname module-file-name))))
               (path-join importing-path module-pathname))))
    (nub (list
          (relative-module-defaults "/")
          (relative-module-defaults ".")))))

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
  (labels ((find-module (defaults list)
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
                         (return item)))))))))
    (some (op (find-module _ list))
          (ensure-list defaults))))

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
               (or (some (op (find-project-module (attrs-root*) _))
                         defaults)
                   (warn "Could not find module with defaults:~%~a"
                         defaults))))
        (unless (eql (genome module)
                     (find-enclosing 'root-ast (attrs-root*) ast))
          (update-dependency-graph (attrs-root*) module)
          (let ((*dependency-stack* (cons module *dependency-stack*)))
            (symbol-table (genome module) (empty-ch-map))))))))

(defmethod symbol-table ((ast cpp-module-declaration) &optional in)
  "Anonymous implementation units (and possibly module partition
implementation units) implicitly import the corresponding interface
unit."
  (if-let (symtab (module-declaration-symbol-table ast))
    (if-let (exports (@ symtab :export))
      (prog1 (symbol-table-union ast in exports)
        ;; Propagate the symbol table to the children (module
        ;; identifiers).
        (call-next-method))
      (call-next-method))
    (call-next-method)))

(defun module-software-symbol-table (imported-module-software)
  "The raw symbol table returned from the module software."
  (symbol-table (genome imported-module-software)
                (empty-ch-map)))

(defun augment-exported-declarations-with-definitions (ast symtab)
  "Merge definitions of external declarations from SYMTAB into exports.

The goal is add definitions of previously declared functions into the
symbol table, even if those definitions are not explicitly exported.

If SYMTAB does not have interface exports, return it unchanged."
  (if-let (interface-exports (@ symtab :interface-export))
    ;; TODO Should exporting from an implementation unit be an error?
    (let* ((symtab (less symtab :interface-export))
           ;; Remove any keys from SYMTAB not present in the interface.
           (restricted (restrict-map symtab interface-exports)))
      (with symtab :export
            (symbol-table-union ast interface-exports restricted)))
    symtab))

(defun module-import-symbol-table (ast)
  (when-let* ((imported-name (source-text (cpp-name ast)))
              (project (attrs-root*))
              (base-path (enclosing-file-pathname ast)))
    (dbg:note :debug "Importing module ~a from ~a"
              imported-name base-path)
    (let* ((module (get-enclosing-module ast))
           (importing-name
             (if module (module-unit-full-name module) ""))
           (partition? (string^= ":" imported-name))
           (exported? (exported? ast))
           (defaults
             (relative-module-defaults base-path importing-name imported-name))
           (imported-module-software
             (or (some (op (find-project-module project _))
                       defaults)
                 (restart-case
                     (error "Could not find module with defaults ~a" defaults)
                   (continue ()
                     :report (lambda (s) (format s "Skip importing ~a from ~a"
                                            imported-name
                                            importing-name))
                     (return-from module-import-symbol-table
                       (empty-ch-map))))))
           (imported-module (module? (genome imported-module-software))))
      (when partition? (assert module))
      (update-dependency-graph (attrs-root*) imported-module-software)
      (let* ((*dependency-stack*
               (cons imported-module-software *dependency-stack*))
             (symtab
               (module-software-symbol-table imported-module-software)))
        (cond
          ((no symtab) nil)
          ;; A re-exported partition. We can see all definitions.
          ((and partition? exported?)
           symtab)
          ;; Importing an interface partition into an implementation
          ;; partition. We need to save the exports to add definitions
          ;; to them later.
          ((and partition?
                (typep module 'module-partition-implementation-unit)
                (typep imported-module 'module-partition-interface-unit))
           ;; Save the export key in an interface-export key.
           (with (less symtab :export)
                 :interface-export
                 (@ symtab :export)))
          ;; Importing an implementation partition. We can see all
          ;; definitions, but don't re-export.
          (partition?
           ;; Insert new definitions into the inherited exports.
           (augment-exported-declarations-with-definitions ast symtab))
          ;; A non-partition reexport.
          (exported?
           (when-let ((exports (@ symtab :export)))
             (with exports :export exports)))
          (t (@ symtab :export)))))))

(defun header-unit-import-symbol-table (ast)
  (when-let* ((symtab
               (find-symbol-table-from-include
                (attrs-root*)
                ast
                :global nil
                :header-dirs '(:stdinc)))
              ;; Macro definitions from the header are not exposed.
              (symtab (less symtab :macro)))
    (if (exported? ast)
        (with symtab :export symtab)
        symtab)))

(defun import-symbol-table (ast)
  (when-let* ((imported-name (source-text (cpp-name ast))))
    (restart-case
        (if (scan "^<.*>$" imported-name)
            ;; Handle a header unit import.
            (header-unit-import-symbol-table ast)
            ;; Handle a module import.
            (module-import-symbol-table ast))
      (continue ()
        :report (lambda (s) (format s "Skip import ~a" imported-name))
        (empty-ch-map)))))

(defmethod symbol-table ((ast cpp-import-declaration) &optional in)
  (let ((*dependency-stack*
         (or *dependency-stack*
             (list
              (find-enclosing-software (attrs-root*) ast)))))
    (if-let (symtab (import-symbol-table ast))
      (prog1 (symbol-table-union ast in symtab)
        (call-next-method))
      (call-next-method))))

) ; #+:TREE-SITTER-CPP
