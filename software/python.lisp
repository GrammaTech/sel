(defpackage :software-evolution-library/software/python
  (:nicknames :sel/software/python :sel/sw/python)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/software/tree-sitter))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language "python")
;;;===================================================

(defun modify-async-parse-tree (parse-tree)
  "Transform PARSE-TREE such that an async field is present for 'async'
identifiers."
  (append
   (butlast parse-tree)
   (list
    (mapcar
     (lambda (child-tree)
       (cond
         ((equal (car child-tree) :async)
          (cons (list :async :async) (cdr child-tree)))
         (t child-tree)))
     (lastcar parse-tree)))))

(defmethod transform-parse-tree
    ((language (eql ':python)) (class (eql 'python-function-definition))
     parse-tree)
  (modify-async-parse-tree parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':python)) (class (eql 'python-for-statement))
     parse-tree)
  (modify-async-parse-tree parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':python)) (class (eql 'python-with-statement))
     parse-tree)
  (modify-async-parse-tree parse-tree))

(defmethod transform-parse-tree
    ((language (eql ':python)) (class (eql 'python-parameters))
     parse-tree)
  "Transform PARSE-TREE such that positional-only and keyword-only classes
are created if they're present in PARSE-TREE."
  (labels ((positional-only-p (subtree &aux (children (lastcar subtree)))
             "Return true if subtree represents a positional only separator."
             (and (eql :error (car subtree))
                  (eql :/ (caar children))
                  (eql :|,| (caadr children))))
           (keyword-only-p (subtree)
             "Return true if subtree represents a keyword only separator."
             (and (eql :list-splat-pattern (car subtree))
                  (not (caddr subtree)))))
    (append
     (butlast parse-tree)
     (list
      (iter
        (for child-tree in (lastcar parse-tree))
        (cond
          ((positional-only-p child-tree)
           (let ((children (lastcar child-tree)))
             (collect (cons :positional-only-separator (cdar children)))
             (collect (cadr children))))
          ((keyword-only-p child-tree)
           (collect (cons :keyword-only-separator (cdr child-tree))))
          (t (collect child-tree))))))))

(defmethod output-transformation :around ((ast python-yield)
                                          &rest rest &key &allow-other-keys)
  (declare (ignorable rest))
  (let ((transformation (call-next-method)))
    (if-let ((position (position "from" transformation :test #'equal)))
      ;; Add the space in between 'yield' and 'from'.
      (replace transformation (list " from") :start1 position)
      transformation)))

(defmethod comparisonp ((ast python-comparison-operator))
  t)


;;; Methods common to all software objects

(defmethod initialize-instance :after ((self python-empty-argument-list) &key)
  (setf (text self) "()"))

(defmethod initialize-instance :after ((self python-empty-parameters) &key)
  (setf (text self) "()"))

(defmethod imports ((software python))
  (imports (genome software)))

(defmethod imports ((ast python-ast)) nil)

(defmethod imports ((ast python-module))
  (mappend #'imports (children ast)))

(defmethod imports ((ast python-import-statement))
  (mapcar #'imports (python-name ast)))

(defmethod imports ((ast python-dotted-name))
  (list (source-text ast)))

(defmethod imports ((ast python-aliased-import))
  (list (source-text (python-name ast)) (source-text (python-alias ast))))

(defmethod imports ((ast python-import-from-statement))
  (if (python-name ast)
      (mapcar [{list (source-text (python-module-name ast)) nil} #'source-text]
              (python-name ast))
      (list (list (source-text (python-module-name ast))))))

(defmethod provided-by ((software python) ast)
  (provided-by (genome software) ast))

(defmethod provided-by ((root python-ast) (ast python-identifier))
  (car (find-if [{equalp (source-text ast)} #'third] (imports root))))

(defmethod provided-by ((root python-ast) (ast python-attribute))
  (labels ((top-attribute (root ast)
             (let ((parent (get-parent-ast root ast)))
               (if (and parent (typep parent 'python-attribute))
                   (top-attribute root parent)
                   ast))))
    (source-text (python-object (top-attribute root ast)))))

(defmethod provided-by (root (ast python-expression-statement))
  (provided-by root (first (children ast))))

(defmethod provided-by ((root python-ast) (ast python-call))
  (provided-by root (call-function ast)))

(defmethod phenome ((obj python) &key (bin (temp-file-name)))
  (interpreted-phenome obj bin))

(defmethod type-in ((obj python) (ast python-ast))
  nil)

(defmethod type-in ((obj python) (ast python-identifier))
  (let ((name (source-text ast))
        (scopes (scopes obj ast)))
    (when-let* ((binding
                 (find-if-in-scopes
                  (op (equal (aget :name _) name))
                  scopes))
                (decl (aget :decl binding)))
      (make-keyword
       (string-upcase
        (source-text
         (or
          ;; Extract just the name of the variable from a parameter.
          (some
           (lambda (parent)
             (and (typep parent 'python-parameter)
                  (find-if
                   (lambda (child)
                     (and (typep child 'python-identifier)
                          (equal name (source-text ast))))
                   parent)))
           decl)
          decl)))))))

(defmethod enclosing-scope ((obj python) (ast python-ast))
  "Return the enclosing scope of AST in OBJ.
OBJ python software object
AST ast to return the enclosing scope for"
  (or (find-if (lambda (parent)
                 ;; Normal case: AST is a member of a class
                 ;; of ASTs defining a new scope.
                 (typep parent '(or
                                 python-function-definition
                                 python-class-definition
                                 python-lambda)))
               (get-parent-asts* obj ast))
      (genome obj)))

(defmethod built-ins ((obj python))
  ;; $ python3
  ;; Python 3.9.5 (default, May  4 2021, 03:36:27)
  ;; [Clang 12.0.0 (clang-1200.0.32.29)] on darwin
  ;; Type "help", "copyright", "credits" or "license" for more information.
  ;; >>> import builtins
  ;; >>> dir(builtins)
  '("ArithmeticError"
    "AssertionError"
    "AttributeError"
    "BaseException"
    "BlockingIOError"
    "BrokenPipeError"
    "BufferError"
    "BytesWarning"
    "ChildProcessError"
    "ConnectionAbortedError"
    "ConnectionError"
    "ConnectionRefusedError"
    "ConnectionResetError"
    "DeprecationWarning"
    "EOFError"
    "Ellipsis"
    "EnvironmentError"
    "Exception"
    "False"
    "FileExistsError"
    "FileNotFoundError"
    "FloatingPointError"
    "FutureWarning"
    "GeneratorExit"
    "IOError"
    "ImportError"
    "ImportWarning"
    "IndentationError"
    "IndexError"
    "InterruptedError"
    "IsADirectoryError"
    "KeyError"
    "KeyboardInterrupt"
    "LookupError"
    "MemoryError"
    "ModuleNotFoundError"
    "NameError"
    "None"
    "NotADirectoryError"
    "NotImplemented"
    "NotImplementedError"
    "OSError"
    "OverflowError"
    "PendingDeprecationWarning"
    "PermissionError"
    "ProcessLookupError"
    "RecursionError"
    "ReferenceError"
    "ResourceWarning"
    "RuntimeError"
    "RuntimeWarning"
    "StopAsyncIteration"
    "StopIteration"
    "SyntaxError"
    "SyntaxWarning"
    "SystemError"
    "SystemExit"
    "TabError"
    "TimeoutError"
    "True"
    "TypeError"
    "UnboundLocalError"
    "UnicodeDecodeError"
    "UnicodeEncodeError"
    "UnicodeError"
    "UnicodeTranslateError"
    "UnicodeWarning"
    "UserWarning"
    "ValueError"
    "Warning"
    "ZeroDivisionError"
    "__build_class__"
    "__debug__"
    "__doc__"
    "__import__"
    "__loader__"
    "__name__"
    "__package__"
    "__spec__"
    "abs"
    "all"
    "any"
    "ascii"
    "bin"
    "bool"
    "breakpoint"
    "bytearray"
    "bytes"
    "callable"
    "chr"
    "classmethod"
    "compile"
    "complex"
    "copyright"
    "credits"
    "delattr"
    "dict"
    "dir"
    "divmod"
    "enumerate"
    "eval"
    "exec"
    "exit"
    "filter"
    "float"
    "format"
    "frozenset"
    "getattr"
    "globals"
    "hasattr"
    "hash"
    "help"
    "hex"
    "id"
    "input"
    "int"
    "isinstance"
    "issubclass"
    "iter"
    "len"
    "license"
    "list"
    "locals"
    "map"
    "max"
    "memoryview"
    "min"
    "next"
    "object"
    "oct"
    "open"
    "ord"
    "pow"
    "print"
    "property"
    "quit"
    "range"
    "repr"
    "reversed"
    "round"
    "set"
    "setattr"
    "slice"
    "sorted"
    "staticmethod"
    "str"
    "sum"
    "super"
    "tuple"
    "type"
    "vars"
    "zip"))

(defmethod scopes ((obj python) (target-ast python-ast)
                   &aux (enclosing-scope (enclosing-scope obj target-ast)))
  "Return lists of variables in each enclosing scope of AST.
Each variable is represented by an alist containing :NAME, :DECL, and :SCOPE.
OBJ python software object
AST ast to return the scopes for"
  ;; NOTE: in the unlikely event this function becomes a bottleneck, it may
  ;;       make sense to cache the get-vars calls.
  (labels ((build-alist (ast name scope)
             "Return an alist containing :name, :decl, and :scope for the
                variable in AST."
             `((:decl . ,ast)
               (:name . ,name)
               (:scope . ,scope)))
           (build-alist* (get-vars-alist
                          &aux (definition (aget :definition get-vars-alist)))
             "Return an alist containing :name, :decl, and :scope for
                GET-VARS-ALIST."
             (build-alist
              definition
              (aget :name get-vars-alist)
              (aget :scope get-vars-alist)))
           (name-in-get-vars-p (obj ast name)
             "Return the variable alist that corresponds to
                NAME if it exists."
             (find-if
              (lambda (alist)
                (equal name (aget :name alist)))
              (get-vars obj ast)))
           (find-get-vars-binding (obj ast enclosing-scope name)
             "Find a variable bound in AST that is named NAME and in
                ENCLOSING-SCOPE."
             (find-if
              (lambda (ast)
                (eq (aget :scope (name-in-get-vars-p obj ast name))
                    enclosing-scope))
              ast))
           (find-declaration (function ast)
             "Find the declaration that is returned by FUNCTION
                starting at AST."
             (find-if
              (lambda (ast)
                (when-let ((declaration (funcall function ast)))
                  (return-from find-declaration declaration)))
              ast))
           (find-nonlocal-binding* (name enclosing-scope)
             "Find the nonlocal binding for NAME in ENCLOSING-SCOPE."
             (find-declaration
              (lambda (ast)
                (if (typep ast 'python-nonlocal-statement)
                    (when (and
                           (find-if
                            (lambda (identifier)
                              (equal name
                                     (source-text identifier :trim t)))
                            (remove-if-not {typep _ 'python-identifier}
                                           (python-children ast)))
                           (not (eq enclosing-scope (genome obj))))
                      (find-nonlocal-binding
                       name (enclosing-scope obj enclosing-scope)))
                    (find-get-vars-binding obj ast enclosing-scope name)))
              (get-asts-in-namespace obj enclosing-scope)))
           (find-nonlocal-binding (name enclosing-scope)
             "Find and build the alist for the nonlocal binding for NAME
                in ENCLOSING-SCOPE."
             (build-alist
              (find-nonlocal-binding* name enclosing-scope)
              name enclosing-scope))
           (find-global-binding
               (identifier &aux (genome (genome obj))
                             (name (source-text identifier)))
             "Find the global binding for NAME in ENCLOSING-SCOPE."
             (build-alist
              (find-declaration
               (lambda (ast)
                 (find-get-vars-binding obj ast genome name))
               (remove nil (children genome)))
              name genome))
           (find-enclosing-bindings (scope)
             "Find the enclosing bindings that occur in scope."
             (mapcar
              #'build-alist*
              (remove-if-not
               (lambda (alist &aux (attributes (aget :attributes alist)))
                 (cond
                   ;; NOTE: imports behave differently than other bindings
                   ;;       that are available from enclosing scopes.
                   ((member :import attributes)
                    (not
                     (path-later-p obj (aget :definition alist) target-ast)))
                   ((intersection '(:class :function) attributes) t)))
               (mappend {get-vars obj} (remove nil (children scope))))))
           (find-local-bindings ()
             "Find local bindings in scope. Returns the py-name
                objects associated with the bindings."
             ;; NOTE: this doesn't correctly return bindings
             ;;       that occur based on control flow like with if-else
             ;;       statements. This typically isn't something that can
             ;;       be accounted for before runtime.
             (remove-duplicates
              (mappend
               (lambda (ast)
                 (remove-if-not
                  (lambda (alist)
                    ;; Check for child scopes allows for
                    ;; namespace bindings in list comps and
                    ;; such.
                    (shares-path-of-p
                     obj target-ast (aget :scope alist)))
                  (get-vars obj ast)))
               ;; Remove ASTs after.
               (remove-if
                (lambda (ast)
                  (path-later-p obj ast target-ast))
                (get-asts-in-namespace obj enclosing-scope)))
              :test (lambda (alist1 alist2)
                      (equal (aget :name alist1)
                             (aget :name alist2)))
              :from-end t))
           (get-global-bindings ()
             "Get the global bindings in scope."
             (mappend (lambda (ast)
                        (mapcar #'find-global-binding
                                (remove-if-not {typep _ 'python-identifier}
                                               (python-children ast))))
                      (remove-if-not
                       {typep _ 'python-global-statement}
                       (get-asts-in-namespace obj enclosing-scope))))
           (get-nonlocal-bindings ()
             "Get the nonlocal bindings in scope."
             (mappend (lambda (ast)
                        (mapcar
                         {find-nonlocal-binding
                         _ (enclosing-scope obj enclosing-scope)}
                         (mapcar
                          (op (source-text _ :trim t))
                          (remove-if-not {typep _ 'python-identifier}
                                         (python-children ast)))))
                      (remove-if-not
                       {typep _ 'python-nonlocal-statement}
                       (get-asts-in-namespace obj enclosing-scope))))
           (get-enclosing-bindings
               (scope &aux (enclosing-scope (enclosing-scope obj scope))
                        (enclosing-bindings (find-enclosing-bindings scope)))
             "Get the enclosing bindings available in scope."
             (if (eq scope enclosing-scope)
                 enclosing-bindings
                 (append enclosing-bindings
                         (get-enclosing-bindings enclosing-scope))))
           (get-local-bindings ()
             "Get the local bindings available in scope."
             ;; Remove bindings after
             (remove-if-not
              (lambda (binding-alist)
                (path-later-p obj target-ast (aget :decl binding-alist)))
              ;; build-alist
              (mapcar #'build-alist* (find-local-bindings))))
           (group-by-scope (bindings)
             "Group BINDINGS by scope."
             (assort bindings :key (lambda (alist) (aget :scope alist))))
           (sort-bottom->up (scopes)
             "Sort SCOPES from the top-most to the bottom-most."
             (sort scopes
                   (lambda (ast1 ast2)
                     (path-later-p obj ast1 ast2))
                   :key (lambda (list)
                          (aget :scope (car list))))))
    (sort-bottom->up
     (group-by-scope
      (remove-duplicates
       (remove-if
        #'null
        ;; NOTE: order of the append matters here for get-except-binding and
        ;;       get-local-bindings.
        (append (get-global-bindings)
                (get-nonlocal-bindings)
                (get-enclosing-bindings enclosing-scope)
                (get-local-bindings)))
       :test (lambda (alist1 alist2)
               (equal (aget :name alist1) (aget :name alist2)))
       :from-end t)))))

(defmethod get-unbound-vals ((obj python) (ast python-ast))
  "Return all variables used (but not defined) within AST.
* OBJ python software object containing AST
* AST ast to retrieve unbound variables within"
  (labels ((call-name-p (parent name)
             "Return T if NAME is a function or method call."
             (typecase parent
               (python-call
                (let ((func (call-function parent)))
                  (typecase func
                    ;; free function
                    (python-identifier (eq func name))
                    ;; method call
                    (python-attribute (eq (python-attribute func) name)))))
               (python-attribute
                (call-name-p (get-parent-ast obj parent) name))))
           (bound-name-p (parent)
             (typep parent
                    '(or
                      python-function-definition
                      python-class-definition)))
           (get-unbound-vals-helper (obj parent ast)
             (remove-duplicates
              (apply #'append
                     (when (and (typep ast 'python-identifier)
                                (not (or (bound-name-p parent)
                                         (call-name-p parent ast))))
                       (list (cons :name (source-text ast))))
                     (mapcar {get-unbound-vals-helper obj ast}
                             (remove nil (children ast))))
              :test #'equal)))
    (get-unbound-vals-helper obj (get-parent-ast obj ast) ast)))

(defmethod get-unbound-funs ((obj python) (ast python-ast)
                             &aux (children (remove nil (children ast))))
  "Return all functions used (but not defined) within AST.  The returned
value will be of the form (list FUNCTION-ATTRS) where FUNCTION-ATTRS is a
list of form (FUNCTION-NAME UNUSED UNUSED NUM-PARAMS).

* OBJ python software object containing AST
* AST ast to retrieve unbound functions within"
  (remove-duplicates
   (apply #'append
          (when-let ((callee (and (typep ast 'python-call)
                                  (call-function ast))))
            (cond ((typep callee 'python-identifier)
                   ;; Free function call
                   (list (list (source-text callee)
                               nil nil
                               (length (call-arguments ast)))))
                  ((typep callee 'python-attribute)
                   ;; Member Function call
                   ;;
                   (list (list (source-text (python-attribute callee))
                               nil nil
                               (length
                                (call-arguments ast)))))
                  (t nil)))
          (mapcar {get-unbound-funs obj} children))
   :test #'equal))

;; TODO: move into parseable?
(-> find-if-in-scopes (function list) list)
(defun find-if-in-scopes (predicate scopes)
  "Return the first binding in SCOPES that satisfies PREDICATE."
  (mapc
   (lambda (scope)
     (when-let ((return-value (find-if predicate scope)))
       (return-from find-if-in-scopes return-value)))
   scopes)
  nil)

(defmethod get-function-from-function-call ((obj python) (ast python-ast))
  (match ast
    ((python-call
      (python-function identifier))
     (when-let ((function-alist
                 (find-if-in-scopes
                  (lambda (scope)
                    (and (equal (source-text identifier)
                                (aget :name scope))
                         (typep (aget :decl scope)
                                'python-function-definition)))
                  (scopes obj ast))))
       (aget :decl function-alist)))))

(defmethod map-arguments-to-parameters
    ((obj python) (funcall python-ast)
     &aux (function (get-function-from-function-call obj funcall)))
  ;; This method assumes a well-formed function and function call.
  (unless function
    ;; Exit early if the function isn't found to prevent errors.
    (return-from map-arguments-to-parameters nil))
  (labels ((get-parameter-alist (parameters)
             "Construct an alist of all the parameters."
             (let ((positional-only
                     ;; This is a hack around a lack of PEP 570 support
                     ;; in tree-sitter-python.
                     (position-if [{equal "/,"} {source-text}] parameters))
                   (list-splat
                     (position-if {typep _ 'python-list-splat-pattern}
                                  parameters))
                   (dictionary-splat
                     (position-if {typep _ 'python-dictionary-splat-pattern}
                                  parameters)))
               `((:positional ,@(when positional-only
                                  (subseq parameters 0 positional-only)))
                 (:regular ,@(subseq parameters
                                     (or (and positional-only
                                              (1+ positional-only))
                                         0)
                                     (or list-splat dictionary-splat)))
                 (:list-splat ,(when-let ((list-splat-ast
                                           (and list-splat
                                                (nth list-splat parameters))))
                                 (unless (equal (source-text list-splat-ast)
                                                "*")
                                   list-splat-ast)))
                 (:keyword ,@(when list-splat
                               (subseq
                                parameters (1+ list-splat) dictionary-splat)
                               (subseq parameters (1+ list-splat))))
                 (:dictionary-splat ,(when dictionary-splat
                                       (nth dictionary-splat parameters))))))
           (same-name-p (arg parameter)
             "Return T if PARAMETER1 and PARAMETER2 represent the same id."
             (equal (source-text arg) (source-text parameter)))
           (get-identifier (ast)
             "Get the relevant identifier for AST if one exists."
             (typecase ast
               ((or python-default-parameter python-keyword-argument)
                (python-name ast))
               ((or python-list-splat-pattern python-dictionary-splat-pattern)
                (car (python-children ast)))
               (t ast)))
           (get-default-parameters (parameters)
             "Get a mapping of default parameters to their defaults."
             ;; NOTE: collect all defaults and remove the duplicates later.
             (mapcar
              (lambda (default)
                (cons (python-value default)
                      (python-name default)))
              (remove-if-not {typep _ 'python-default-parameter}
                             parameters)))
           (get-positional-args-to-parameters (parameters-alist args-list)
             "Get a mapping of positional arguments to their parameters."
             (let ((positionals  (append (aget :positional parameters-alist)
                                         (aget :regular parameters-alist)))
                   (list-splat (car (aget :list-splat parameters-alist)))
                   (positional-args
                     (remove-if {typep _ 'python-keyword-argument} args-list)))
               (append (mapcar
                        (lambda (arg parameter)
                          (cons arg (get-identifier parameter)))
                        positional-args positionals)
                       (when list-splat
                         (list
                          (cons (create-tuple
                                 (drop (length positionals) positional-args))
                                (get-identifier list-splat)))))))
           (create-keyword-dictionary (keyword-args)
             "Create the dictionary created for '**' style parameters."
             (create-dictionary
              (mapcar
               (lambda (arg)
                 (convert
                  'python-ast
                  `((:class . :string)
                    (:text . ,(format nil "\"~a\""
                                      (source-text
                                       (python-name arg)))))))
               keyword-args)
              (mapcar #'python-value keyword-args)))
           (get-keyword-args-to-parameters (parameters-alist args-list)
             "Get a mapping of keyword arguments to their parameters."
             (iter
               (iter:with
                dict-splat = (car (aget :dictionary-splat parameters-alist)))
               (iter:with dict-splat-args)
               (for arg in (remove-if-not {typep _ 'python-keyword-argument}
                                          args-list))
               (if-let ((parameter
                         (find-if
                          [{same-name-p (get-identifier arg)}
                          #'get-identifier]
                          ;; potential keywords
                          (append (aget :regular parameters-alist)
                                  (aget :keyword parameters-alist)))))
                 (collect (cons (python-value arg) (get-identifier parameter))
                   into mapping)
                 (push arg dict-splat-args))
               (finally
                (return
                  (if dict-splat
                      (cons (cons (create-keyword-dictionary dict-splat-args)
                                  (get-identifier dict-splat))
                            mapping)
                      mapping))))))
    (let* ((parameters (python-children (python-parameters function)))
           (parameters-alist (get-parameter-alist parameters))
           (args-list (call-arguments funcall)))
      ;; NOTE: all default parameters are returned by get-default-parameters.
      ;;       The defaults that are actually used need to be removed here.
      (remove-duplicates
       ;; NOTE: append order matters.
       (append (get-positional-args-to-parameters
                parameters-alist args-list)
               (get-keyword-args-to-parameters
                parameters-alist args-list)
               (get-default-parameters parameters))
       :test #'same-name-p :key #'cdr :from-end t))))

(defmethod assign-to-var-p ((ast python-ast) (identifier python-ast))
  ;; Return the python-identifier that matches in case the caller wants
  ;; to check if it is the same as identifier.
  (match ast
    ((python-augmented-assignment :python-left lhs)
     (and (identical-name-p identifier lhs) lhs))
    ((python-assignment
      :python-left lhs)
     (typecase lhs
       (python-identifier (and (identical-name-p identifier lhs) lhs))
       (python-pattern-list
        (find-if {identical-name-p identifier} (python-children lhs)))))))

(defmethod function-name ((node python-function-definition))
  (source-text (python-name node)))

(defmethod function-name ((node python-attribute))
  (source-text (python-attribute node)))

(defmethod function-parameters ((ast python-function-definition))
  (function-parameters (second (children ast))))

(defmethod function-parameters ((ast python-parameters))
  (children ast))

(defmethod end-of-parameter-list
    ((software python) (function-node function-ast))
  (ematch function-node
    ((python-function-definition
      :python-parameters (and parameters (type node)))
     (ematch (ast-end software parameters)
       ((source-location :line line :column column)
        (make 'source-location :line line :column column))))
    ((python-lambda :python-parameters (and parameters (type node)))
     (ast-end software parameters))
    ((python-lambda :python-parameters nil)
     (ematch (ast-start software function-node)
       ((source-location :line line :column column)
        (make 'source-location
              :line line
              :column (+ column #.(length "lambda"))))))))

(defmethod function-body ((ast python-function-definition)) (python-body ast))

;; NB There is no single "operator" for a chained comparison.
(defmethod lhs ((ast python-comparison-operator))
  (if (single (python-operators ast))
      (first (sorted-children ast))
      (call-next-method)))
(defmethod rhs ((ast python-comparison-operator))
  (if (single (python-operators ast))
      (third (sorted-children ast))
      (call-next-method)))
(defmethod operator ((ast python-comparison-operator))
  (if (single (python-operators ast))
      (first (python-operators ast))
      (call-next-method)))

(defmethod control-flow-condition ((ast python-if-statement)) (car (children ast)))
(defmethod control-flow-condition ((ast python-while-statement)) (car (children ast)))


;;; Indentation

(defmethod indentablep ((ast python-string)) nil)

(defmethod get-default-indentation ((ast python-ast) (parents list))
  ;; Search for the first AST which has a colon in its source-text and
  ;; use its indent-children value. If none are found, call-next-method.
  (labels ((indented-obj-p (ast &aux (indent-children (indent-children ast)))
             "Return T if AST is an obj that should have indented children."
             (and
              indent-children
              (not (eql t indent-children))
              (typep ast '(or
                           ;; TODO: at some point, add more classes here.
                           python-class-definition python-function-definition
                           python-if-statement python-while-statement
                           python-for-statement)))))
    (if-let ((indented-obj (find-if #'indented-obj-p (or (lastcar parents)
                                                         ast))))
      (indent-children indented-obj)
      (call-next-method))))


;;; Helper functions

(-> collect-var-uses (python python-ast) (values list &optional))
(defun collect-var-uses (obj ast)
  "Collect uses of AST in OBJ."
  ;;TODO: at some point, expand this to work inside classes.
  ;;      This may require significat modifications to acount
  ;;      for 'self' variables.
  (labels ((same-name-p (ast name)
             "Return T if AST represents an AST that contains the same
                name as NAME."
             (typecase ast
               (python-identifier (equal (source-text ast) name))
               ((or python-global-statement
                    python-nonlocal-statement)
                (find-if {same-name-p _ name} (python-children ast)))))
           (find-name-in-scopes (name scopes)
             "Search SCOPES for a variable named NAME."
             (mappend
              (lambda (scope)
                (find-if
                 (lambda (var-info)
                   (equal name (aget :name var-info)))
                 scope))
              scopes))
           (get-analysis-set (scope first-occurrence name)
             "Collect all relevant asts with NAME in SCOPE. BINDING-CLASS
                determines whether 'global' or 'nonlocal' should be used to
                determine if NAME is in-scope for assignments."
             ;; Currently, python-identifier and either
             ;; python-nonlocal-statement or python-global-statement are
             ;; relevant.
             (remove-if-not
              (lambda (ast)
                (or (same-name-p ast name) (eq ast first-occurrence)))
              (collect-if
               (lambda (ast)
                 (typep ast 'python-identifier))
               scope)))
           (find-var-uses (assorted-by-scope binding-class)
             "Search assorted-by-scope for usages of variables
                with the same name. BINDING-CLASS specifies whether
                the variable is global or local and provides the
                name of the class used for binding it to a scope."
             (iter
               (iter:with out-of-scope = nil)
               (iter:with
                local-var-p = (eq binding-class 'python-nonlocal-statement))
               (for vars-in-scope in assorted-by-scope)
               (for scope = (enclosing-scope obj (car vars-in-scope)))
               ;; Prune any scope that occurs after the local binding
               ;; has been squashed.
               (when out-of-scope
                 (if (shares-path-of-p obj scope out-of-scope)
                     (next-iteration)
                     (setf out-of-scope nil)))
               (cond
                 ((find-if
                   (lambda (ast)
                     (find-if-in-parents {typep _ 'python-parameters} obj ast))
                   vars-in-scope)
                  ;; All nested scopes are out-of-scope.
                  (and local-var-p (setf out-of-scope scope)))
                 ((find-if
                   (lambda (var)
                     (find-if-in-parents {typep _ binding-class} obj var))
                   vars-in-scope)
                  (collect vars-in-scope))
                 ((find-if
                   (lambda (var)
                     (find-if-in-parents
                      (lambda (parent)
                        (and (typep parent 'python-assignment)
                             (assign-to-var-p parent var)))
                      obj var))
                   vars-in-scope)
                  ;; All nested scopes are out-of-scope.
                  (and local-var-p (setf out-of-scope scope)))))))
    (let* ((name (and (typep ast 'python-identifier) (source-text ast)))
           (var-info (find-name-in-scopes name (scopes obj ast)))
           (scope (or (aget :scope var-info) (enclosing-scope obj ast)))
           ;; The path will be nil when given the global scope.
           (binding-class (if (ast-path obj scope)
                              'python-nonlocal-statement
                              'python-global-statement))
           (assorted-by-scope
             ;; Make sure the top-most scope comes first.
             (sort
              (assort
               (get-analysis-set scope (or (aget :decl var-info)
                                           (get-parent-full-stmt obj ast))
                                 name)
               :key {enclosing-scope obj})
              (lambda (ast1 ast2)
                (< (length (ast-path obj (enclosing-scope obj ast1)))
                   (length (ast-path obj (enclosing-scope obj ast2)))))
              :key #'car)))
      ;; Don't pass in the first scope of assorted-by-scope as the first
      ;; one may include a parameter which find-var-uses would misinterpret
      ;; as squashing the binding's scope.
      (flatten (cons (car assorted-by-scope)
                     (find-var-uses (cdr assorted-by-scope)
                                    binding-class))))))

(-> collect-fun-uses (python python-ast) list)
(defun collect-fun-uses (obj ast)
  (labels ((same-name-p (ast name)
             "Return T if AST represents an AST that contains the same
                name as NAME."
             (equal (source-text ast) name))
           (get-analysis-set (scope name)
             "Collect all relevant asts with NAME in SCOPE."
             ;; Currently, py-name, py-arg, and either py-nonlocal or py-global
             ;; are relevant.
             (mapcar
              ;; Map the ASTs into parents that are easier
              ;; to work with.
              (lambda (ast)
                (cond-let result
                  ((find-if-in-parents {typep _ 'python-parameters} obj ast)
                   result)
                  ((find-if-in-parents {typep _ 'python-function-definition}
                                       obj ast)
                   (if (eq (python-name result) ast)
                       result
                       ast))
                  (t ast)))
              (remove-if-not
               (lambda (ast)
                 (same-name-p ast name))
               (collect-if
                (lambda (ast)
                  (member
                   (type-of ast)
                   `(python-identifier python-function-definition)))
                scope))))
           (get-shadowed-asts (analysis-set shadowing-ast)
             "Get the ASTs in ANALYSIS-SET that are shadowed by SHADOWING-AST."
             (intersection
              analysis-set
              (remove-if
               (lambda (ast)
                 (path-later-p obj shadowing-ast ast))
               (get-asts-in-namespace
                obj (enclosing-scope obj shadowing-ast)))))
           (shadowing-ast-p (ast)
             "Return T if AST is an AST that shadows the function."
             (etypecase ast
               ((or python-parameters python-function-definition) t)
               (python-identifier
                ;; TODO: at some point,for loops and other binding forms
                ;;       can also shadow, but support for identifying this
                ;;       still needs to be done.
                (find-if-in-parents
                 (lambda (parent)
                   (assign-to-var-p parent ast))
                 obj ast))))
           (remove-shadowed-asts (analysis-set)
             "Remove all ASTs that are shadowing the target function
                from the analysis set."
             ;; The initial definition is seen as a shadowing ast,
             ;; so remove it from consideration and add it back
             ;; after analysis.
             (cons
              (car analysis-set)
              (iter
                (iter:with shadowed-asts)
                (for ast in (cdr analysis-set))
                (when (shadowing-ast-p ast)
                  (setf shadowed-asts
                        (append shadowed-asts
                                (get-shadowed-asts analysis-set ast))))
                (unless (member ast shadowed-asts)
                  (collect ast))))))
    (when (typep ast 'python-function-definition)
      (remove-shadowed-asts
       (get-analysis-set
        (enclosing-scope obj ast)
        (source-text (python-name ast)))))))

(defgeneric get-vars (obj ast)
  (:documentation "Get the variables that are bound by AST.")
  (:method ((obj python) ast) nil))

(defun create-var-alist (obj definition name &key attributes scope)
  "Create an alist with information about a variable."
  `((:name . ,name)
    (:definition . ,definition)
    (:scope . ,(or scope (enclosing-scope obj definition)))
    ,@(when attributes
        (list (cons :attributes attributes)))))

(defun get-vars-name-handler (obj ast &key scope)
  "Handle AST as a py-name object."
  (create-var-alist
   obj ast (source-text ast)
   :scope scope
   :attributes '(:variable)))

(defun get-vars-name-or-tuple-handler (obj ast &key scope)
  "Handle AST as a py-name or a py-tuple object."
  (typecase ast
    ((or python-tuple python-pattern-list)
     (mapcar
      (lambda (element)
        (create-var-alist
         obj element (source-text element)
         :scope scope
         :attributes '(:variable)))
      (python-children ast)))
    ((or python-identifier python-dotted-name)
     (list
      (get-vars-name-handler obj ast :scope scope)))))

(defmethod get-vars ((obj python) (ast python-for-statement))
  (get-vars-name-or-tuple-handler obj (python-left ast)))

;; TODO: python-except-clause doesn't appear to have any slots.
;;       This may be an issue to open in tree-sitter-python.
(defmethod get-vars ((obj python) (ast python-except-clause))
  ;; NOTE: try except appears to unbind the variable in the namespace.
  ;;       This may be because the exception has been freed by the time
  ;;       it is out of the except handler.
  ;;       This may require a special attribute for handling it.
  (let ((name-ast (cadr (python-children ast))))
    (when (typep name-ast 'python-identifier)
      (list
       (create-var-alist
        obj ast (source-text name-ast)
        :attributes '(:variable))))))

(defun get-vars-comprehension-handler (obj ast)
  ;; NOTE: this is tricky since there are essentially two ASTs
  ;;       that the variable binding is available in. The chances
  ;;       of this becoming an issue are probably slim.
  ;;
  ;;       x = [1, 2, 3, 4, 5]
  ;;       x = [x for x in x]
  (mappend
   (lambda (for-in-clause)
     (mapcar
      (lambda (name-or-tuple)
        (get-vars-name-or-tuple-handler
         obj name-or-tuple :scope ast))
      (python-left for-in-clause)))
   (remove-if-not {typep _ 'python-for-in-clause}
                  (python-children ast))))

(defmethod get-vars ((obj python) (ast python-list-comprehension))
  (get-vars-comprehension-handler obj ast))

(defmethod get-vars ((obj python) (ast python-set-comprehension))
  (get-vars-comprehension-handler obj ast))

(defmethod get-vars ((obj python) (ast python-dictionary-comprehension))
  (get-vars-comprehension-handler obj ast))

(defmethod get-vars ((obj python) (ast python-generator-expression))
  (get-vars-comprehension-handler obj ast))

(defun get-vars-import-handler (obj ast)
  "Handle AST as a python-import-statement or python-import-from-statement."
  (mapcar
   (lambda (name)
     (create-var-alist
      obj ast
      (typecase name
        ;; TODO: at some point,figure out how we want to handle dotted names.
        (python-dotted-name
         (source-text name))
        (python-aliased-import
         (source-text (python-alias name))))
      :attributes '(:import)))
   (python-name ast)))

(defmethod get-vars ((obj python) (ast python-import-statement))
  (get-vars-import-handler obj ast))

(defmethod get-vars ((obj python) (ast python-import-from-statement))
  (get-vars-import-handler obj ast))

(defmethod get-vars ((obj python) (ast python-with-statement))
  (remove
   nil
   (mappend
    (lambda (clause)
      (mapcar
       (lambda (item)
         (when-let ((var (python-alias item)))
           (get-vars-name-handler obj var)))
       (python-children clause)))
    (python-children ast))))

(defmethod get-vars ((obj python) (ast python-assignment)
                     &aux (lhs (python-left ast)))
  (typecase lhs
    (python-pattern-list
     (mapcar
      (lambda (item)
        (create-var-alist
         obj ast (source-text item)
         :attributes '(:variable)))
      (python-children lhs)))
    (python-identifier
     (list
      (create-var-alist obj ast (source-text lhs) :attributes '(:variable))))))

(defmethod get-vars ((obj python) (ast python-function-definition))
  (append
   (list (create-var-alist obj ast (source-text (python-name ast))
                           :attributes '(:function)))
   (when-let ((parameters (python-parameters ast)))
     (mapcar
      (lambda (parameter)
        (create-var-alist
         obj parameter
         (source-text
          ;; Avoid printing the type as part of the name.
          (or (find-if (of-type 'identifier-ast)
                       (children parameter))
              parameter))
         :scope ast
         :attributes '(:variable)))
      (python-children parameters)))))

(defmethod get-vars ((obj python) (ast python-lambda))
  (mapcar
   (lambda (parameter)
     (create-var-alist
      obj parameter (source-text parameter)
      :scope ast
      :attributes '(:variable)))
   (when-let ((parameters (python-parameters ast)))
     (python-children parameters))))

(defmethod get-vars ((obj python) (ast python-class-definition))
  (unless (in-class-def-p obj ast)
    (list
     (create-var-alist
      obj ast (source-text (python-name ast))
      :attributes '(:class)))))

(-> in-class-def-p (python python-ast)
  (values (or null python-class-definition) &optional))
(defun in-class-def-p (obj ast)
  "Return the class definition if AST is inside one."
  (find-if-in-parents {typep _ 'python-class-definition} obj ast))

(-> identical-name-p (python-ast python-ast) boolean)
(defun identical-name-p (name1 name2)
  "Return T if the IDs of NAME1 and NAME2 are the same."
  (and (typep name1 'python-identifier)
       (typep name2 'python-identifier)
       (equal (source-text name1) (source-text name2))))

(-> get-asts-in-namespace (python python-ast) list)
(defun get-asts-in-namespace (obj ast)
  "Get all of the ASTs in AST which are considered to be
in the same namespace."
  ;; Note that with the first call to this function, AST should be the start
  ;; of a namespace.
  (labels ((new-namespace-p (ast)
             "Return T if AST starts a new namespace."
             ;; TODO: probably need to add some more types here.
             (typep
              ast '(or python-function-definition python-class-definition)))
           (collect-asts (namespace)
             "Collect the asts in NAMESPACE."
             (let ((children (remove nil (children namespace))))
               (append children
                       (mappend (lambda (child)
                                  (unless (new-namespace-p child)
                                    (collect-asts child)))
                                children)))))
    (cons ast (sort (collect-asts ast)
                    (lambda (ast1 ast2)
                      (path-later-p obj ast2 ast1))))))

(-> create-tuple (list) (values python-ast &optional))
(defun create-tuple (values)
  "Create a new tuple AST that contains values."
  (convert 'python-ast
           `((:class . ,(if values :tuple- :empty-tuple))
             (:children . ,values))))

(-> create-dictionary (list list) (values (or python-ast null) &optional))
(defun create-dictionary (keys values &aux (length (length keys)))
  "Create a new dictionary AST that maps KEYS to VALUES.
Returns nil if the length of KEYS is not the same as VALUES'."
  (when (= length (length values))
    (convert 'python-ast
             `((:class . ,(if values :dictionary- :empty-dictionary))
               (:children
                ,@(mapcar
                   (lambda (key value)
                     (convert
                      'python-ast
                      `((:class . :pair)
                        (:key . ,key)
                        (:value . ,value))))
                   keys values))))))

;; Implement the generic format-genome method for python objects.
(defmethod format-genome ((obj python) &key)
  "Format the genome of OBJ using YAPF (Yet Another Python Formatter)."
  (yapf obj))


;;; Whitespace rules

(defclass pep8 () ()
  (:documentation "PEP 8 style."))

(def +pep8+ (make 'pep8))

;;; Default to PEP8.
(defmethod whitespace-between/parent ((parent t) (style null) (x python-ast) y)
  (whitespace-between/parent parent +pep8+ x y))
(defmethod whitespace-between/parent ((parent t) (style null) x (y python-ast))
  (whitespace-between/parent parent +pep8+ x y))

(define-empty-whitespace-methods (pep8)
  python-lambda-parameters (eql :|:|)
  python-** python-ast
  python-ast python-**
  (eql :|.|) python-ast
  python-ast (eql :|.|)
  python-ast (eql :|:|))

(defmethod whitespace-between ((style pep8)
                               (x python-expression-statement)
                               (y python-expression-statement))
  "A newline between expression statements."
  (fmt "~%"))

(defmethod whitespace-between ((style pep8)
                               (x python-ast)
                               (y python-return-statement))
  "A newline before a return statement."
  (fmt "~%"))

(defmethod whitespace-between ((style pep8)
                               (x python-class-definition)
                               (y python-ast))
  "Surround class definitions with two newlines."
  #.(fmt "~3%"))

(defmethod whitespace-between ((style pep8)
                               (x python-ast)
                               (y python-class-definition))
  (whitespace-between style y x))

(defmethod whitespace-between/parent ((parent null)
                                      (style pep8)
                                      (x python-function-definition)
                                      (y python-ast))
  "Per PEP 8: one blank line for methods, two for functions."
  (fmt "~3%"))

(defmethod whitespace-between/parent ((parent python-module)
                                      (style pep8)
                                      (x python-function-definition)
                                      (y python-ast))
  "Per PEP 8: one blank line for methods, two for functions."
  (fmt "~3%"))

(defmethod whitespace-between ((style pep8)
                               (x python-function-definition)
                               (y python-ast))
  "Per PEP 8: one blank line for methods, two for functions."
  (fmt "~2%"))

(defmethod whitespace-between ((style pep8)
                               (x python-ast)
                               (y python-function-definition))
  (whitespace-between style y x))

(defmethod whitespace-between ((style pep8)
                               (x (eql :|:|))
                               (y python-block))
  #.(fmt "~%"))
