(defpackage :software-evolution-library/test/lisp-bindings
  (:nicknames :sel/test/lisp-bindings)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/lisp)
  (:import-from :software-evolution-library/software/lisp
                :get-vars-from-ordinary-lambda-list)
  (:export :test-lisp-bindings))
(in-package :software-evolution-library/test/lisp-bindings)
(in-readtable :curry-compose-reader-macros)

(defsuite test-lisp-bindings "Lisp bindings")


;;; Utility
(defun scope-contains-var-p (scope symbol)
  "Return the variable alist associated with symbol if it exists."
  (find-if [{equal symbol} {aget :name}] scope))

(defun is-var-in-scope (scope symbol)
  "Test if var is in scope, succeeding if it is."
  (is (scope-contains-var-p scope symbol)
      "Vars should contain '~a'." symbol))

(defun is-not-var-in-scope (scope symbol)
  "Test if var is in scope, succeeding if it isn't."
  (is (not (scope-contains-var-p scope symbol))
      "Vars should not contain '~a'." symbol))

(defun scopes-contains-var-p (scopes symbol)
  "Return the variable alist associated with symbol if it exists."
  (mappend {scope-contains-var-p _ symbol} scopes))

(defun is-scopes-contains-var-p (scopes symbol)
  "Test if var is in an enclosing scope, succeeding if it is."
  (is (scopes-contains-var-p scopes symbol)
      "Scopes should contain '~a'." symbol))

(defun is-not-scopes-contains-var-p (scopes symbol)
  "Test if var is in an enclosing scope, succeeding if it is."
  (is (not (scopes-contains-var-p scopes symbol))
      "Scopes should not contain '~a'." symbol))

(defmacro with-software-file ((filename software-var genome-var)
                             &body body)
  " "
  `(let* ((,software-var (from-file
                          (make-instance 'lisp)
                          (make-pathname :name ,filename
                                         :type "lisp"
                                         :directory +lisp-scopes-dir+)))
          (,genome-var (genome ,software-var)))
     ,@body))

(defun find-compound-form (symbol ast)
  "Return the first compound form in AST that symbol
is the car of."
  (find-if {compound-form-p _ :name symbol} ast))


;;; Tests
(deftest lisp-get-vars-from-binding-form-let-1 ()
  "Includes all variables in 'let and doesn't include any variables if
the ast provided to scopes isn't in scope
of the 'let form."
  (with-software-file ("let-1" obj ast)
    (let* ((binding-form (find-compound-form 'let ast))
           (vars (get-vars-from-binding-form obj 'let binding-form)))
      (is-var-in-scope vars 'x)
      (is-var-in-scope vars 'y)
      (is-var-in-scope vars 'z)
      (is (not (get-vars-from-binding-form
                obj 'let binding-form :reference-ast binding-form))
          "Vars should not contain any variables outside of scope."))))

(deftest lisp-get-vars-from-binding-form-let*-1 ()
  "Includes all variables in 'let* that are in-scope of the reference ast."
  (with-software-file ("let-asterisk-1" obj ast)
    (let* ((binding-form (find-compound-form 'let* ast))
           (vars (get-vars-from-binding-form
                  obj 'let* binding-form
                  :reference-ast (find-compound-form 'c ast))))
      (is-var-in-scope vars 'a)
      (is-var-in-scope vars 'b))))

(deftest lisp-get-vars-from-ordinary-lambda-list-1 ()
  "Can get all vars from a lambda list and vars not in scope
of the reference ast should not be included."
  (with-software-file ("get-vars-from-ordinary-lambda-list-1" obj ast)
    (let* ((binding-form (find-compound-form 'defun ast))
           (lambda-list (nth 5 (children binding-form)))
           (vars (get-vars-from-ordinary-lambda-list
                  obj binding-form lambda-list))
           (reference-vars
             (get-vars-from-ordinary-lambda-list
              obj binding-form lambda-list
              :reference-ast
              (find-compound-form 'e ast))))
      (is-var-in-scope vars 'a)
      (is-var-in-scope vars 'b)
      (is-var-in-scope vars 'd)
      (is-var-in-scope vars 'e)
      (is-var-in-scope reference-vars 'a)
      (is-var-in-scope reference-vars 'b)
      (is-var-in-scope reference-vars 'd)
      (is-not-var-in-scope reference-vars 'e))))

(deftest lisp-get-vars-from-binding-form-if-let-1 ()
  "'if-let has no variables in scope when in the else clause."
  (with-software-file ("if-let-1" obj ast)
    (is (not (get-vars-from-binding-form
              obj 'if-let (find-compound-form 'if-let ast)
              :reference-ast (find-compound-form '- ast)))
        "There should be no vars in scope in the 'else' clause.")))

(deftest lisp-get-vars-from-binding-form-if-let*-1 ()
  "'if-let* has no variables in scope when in the else clause."
  (with-software-file ("if-let-asterisk-1" obj ast)
    (is (not (get-vars-from-binding-form
              obj 'if-let* (find-compound-form 'if-let* ast)
              :reference-ast (find-compound-form '- ast)))
        "There should be no vars in scope in the 'else' clause.")))

(deftest lisp-get-vars-from-binding-form-defun-1 ()
  "'defun gets variables from the parameter clause."
  (with-software-file ("defun-1" obj ast)
    (let* ((binding-form (find-compound-form 'defun ast))
           (vars (get-vars-from-binding-form
                  obj 'defun binding-form
                  :reference-ast (find-compound-form '+ ast))))
      (is-var-in-scope vars 'a)
      (is-var-in-scope vars 'b))))

(deftest lisp-get-vars-from-binding-form-labels-1 ()
  "'defun gets variables from the parameter clause
of the relevant local function."
  (with-software-file ("labels-1" obj ast)
    (let* ((binding-form (find-compound-form 'labels ast))
           (f-vars (get-vars-from-binding-form
                    obj 'labels binding-form
                    :reference-ast (find-compound-form '- ast)))
           (g-vars (get-vars-from-binding-form
                    obj 'labels binding-form
                    :reference-ast (find-compound-form '+ ast))))
      (is-var-in-scope g-vars 'a)
      (is-var-in-scope g-vars 'b)
      (is-var-in-scope f-vars 'z))))

(deftest lisp-get-vars-from-binding-form-defvar-1 ()
  "'defvar variables are found."
  (with-software-file ("defvar-1" obj ast)
    (let* ((binding-form (find-compound-form 'defvar ast))
           (vars (get-vars-from-binding-form
                  obj 'defvar binding-form)))
      (is-var-in-scope vars 'a))))

(deftest lisp-get-vars-from-binding-form-symbol-macrolet-1 ()
  "symbol macros are returned when the relevant special variable is set
and not if it isn't set."
  (with-software-file ("symbol-macrolet-1" obj ast)
    (let* ((binding-form (find-compound-form 'symbol-macrolet ast))
           (vars-without (get-vars-from-binding-form
                          obj 'symbol-macrolet binding-form))
           (*bindings-allows-symbol-macros-p* t)
           (vars-with (get-vars-from-binding-form
                       obj 'symbol-macrolet binding-form)))
      (is-not-var-in-scope vars-without 'a)
      (is-var-in-scope vars-with 'a))))

(deftest lisp-get-vars-from-binding-form-define-symbol-macro-1 ()
  "symbol macros are returned when the relevant special variable is set
and not if it isn't set."
  (with-software-file ("define-symbol-macro-1" obj ast)
    (let* ((binding-form (find-compound-form 'define-symbol-macro ast))
           (vars-without (get-vars-from-binding-form
                          obj 'define-symbol-macro binding-form))
           (*bindings-allows-symbol-macros-p* t)
           (vars-with (get-vars-from-binding-form
                       obj 'define-symbol-macro binding-form)))
      (is-not-var-in-scope vars-without 'a)
      (is-var-in-scope vars-with 'a))))

(deftest lisp-scopes-1 ()
  "'scopes collects variables from all enclosing scopes."
  (with-software-file ("scopes-1" obj ast)
    (let* ((scopes (scopes obj (find-compound-form 'list ast))))
      (is-scopes-contains-var-p scopes 'a)
      (is-scopes-contains-var-p scopes 'b)
      (is-scopes-contains-var-p scopes 'c) )))

(deftest lisp-scopes-2 ()
  "'scopes collects top-level variables when the relevant
special variable is set."
  (with-software-file ("scopes-2" obj ast)
    (let* ((scopes-with (scopes obj (find-compound-form '+ ast)))
           (*bindings-allows-top-level-p* nil)
           (scopes-without (scopes obj (find-compound-form '+ ast))))
      (is-scopes-contains-var-p scopes-with '*a*)
      (is-scopes-contains-var-p scopes-with '*b*)
      (is-not-scopes-contains-var-p scopes-without '*a*)
      (is-not-scopes-contains-var-p scopes-without '*b*))))
