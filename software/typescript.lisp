(defpackage :software-evolution-library/software/typescript
  (:nicknames :sel/software/typescript :sel/sw/typescript)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/template))

(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

;;;===================================================
;;; Generate the language definitions
;;;===================================================
(create-tree-sitter-language "typescript/tsx")
(create-tree-sitter-language "typescript/typescript")
;;;===================================================

(defmethod from-file ((class (eql 'typescript)) file)
  (from-file 'typescript-ts file))

(defmethod from-file ((software typescript) file)
  (from-file (change-class software 'typescript-ts) file))

(defmethod from-string ((class (eql 'typescript)) string)
  (from-string 'typescript-ts string))

(defmethod from-string ((software typescript) string)
  (from-string (change-class software 'typescript-ts) string))

(defmacro with-tsx-methods ((&key) &body body)
  "Given methods specialized on `typescript-ts', also generate methods
specialized on `typescript-tsx'."
  (assert (every (eqls 'defmethod) (mapcar #'car body)))
  (let ((package (find-package :sel/sw/ts)))
    (flet ((retarget (form from to)
             (leaf-map (lambda (leaf)
                         (if (and (symbolp leaf)
                                  (string^= from leaf))
                             (intern (string+ to
                                              (drop-prefix
                                               (string from)
                                               (string leaf)))
                                     (if (keywordp leaf)
                                         #.(find-package :keyword)
                                         package))
                             leaf))
                       form)))
      `(progn
         ,@(iter (for form in body)
                 (collect form)
                 (collect (retarget form
                                    'typescript-ts
                                    'typescript-tsx)))))))

(defun transform-ts-parameter-parse-tree (parse-tree)
  (with-modify-parse-tree (parse-tree)
    ((:accessibility-modifier :override-modifier
      :readonly)
     (label-as :modifiers))))

#+:TREE-SITTER-TYPESCRIPT/TYPESCRIPT
(progn

(with-tsx-methods ()

  (defmethod transform-parse-tree
      ((language (eql ':typescript-ts))
       (class (eql 'typescript-ts-arrow-function))
       parse-tree &key)
    (with-modify-parse-tree (parse-tree)
      (:async (label-as :async))))

  (defmethod transform-parse-tree
      ((language (eql ':typescript-ts))
       (class (eql 'typescript-ts-enum-declaration))
       parse-tree &key)
    (with-modify-parse-tree (parse-tree)
      (:const (label-as :kind))))

  (defmethod transform-parse-tree
      ((language (eql ':typescript-ts))
       (class (eql 'typescript-ts-export-statement))
       parse-tree &key)
    (with-modify-parse-tree (parse-tree)
      ((:default := :as :namespace :*) (label-as :default))))

  (defmethod transform-parse-tree
      ((language (eql ':typescript-ts))
       (class (eql 'typescript-ts-function))
       parse-tree &key)
    (with-modify-parse-tree (parse-tree)
      (:async (label-as :async))))

  (defmethod transform-parse-tree
      ((language (eql ':typescript-ts))
       (class (eql 'typescript-ts-function-declaration))
       parse-tree &key)
    (with-modify-parse-tree (parse-tree)
      (:async (label-as :async))))

  (defmethod transform-parse-tree
      ((language (eql ':typescript-ts))
       (class (eql 'typescript-ts-member-expression))
       parse-tree &key)
    (with-modify-parse-tree (parse-tree)
      ((:?. :.) (label-as :operator))))

  (defmethod transform-parse-tree
      ((language (eql ':typescript-ts))
       (class (eql 'typescript-ts-method-definition))
       parse-tree &key)
    (with-modify-parse-tree (parse-tree)
      ((:get :set :*) (label-as :getter-setter))))

  (defmethod transform-parse-tree
      ((language (eql :typescript-ts))
       (class (eql 'typescript-ts-public-field-definition))
       parse-tree &key)
    (with-modify-parse-tree (parse-tree)
      ((:static
        :accessibility-modifier :override-modifier
        :readonly :abstract)
       (label-as :modifiers))
      ((:? :!) (label-as :optional))))

  (defmethod transform-parse-tree
      ((language (eql :typescript-ts))
       (class (eql 'typescript-ts-required-parameter))
       parse-tree &key)
    (transform-ts-parameter-parse-tree parse-tree))

  (defmethod transform-parse-tree
      ((language (eql :typescript-ts))
       (class (eql 'typescript-ts-optional-parameter))
       parse-tree &key)
    (transform-ts-parameter-parse-tree parse-tree))

  (defmethod transform-parse-tree
      ((language (eql :typescript-ts))
       (class (eql 'typescript-ts-property-signature))
       parse-tree &key)
    (with-modify-parse-tree (parse-tree)
      ((:?) (label-as :optional))))

  ;; NB What should `function-parameters' return in the presence of
  ;; destructuring? Given a parameter list like `({a, b, c}, {x, y z})'
  ;; are there two parameters or six? Currently our answer is two, not
  ;; just because it's simpler, but because it's not just for analysis;
  ;; it's also for programs that wants to generate test inputs. That
  ;; said the six-parameter case might eventually deserve another
  ;; function.

  ;; Function declaration.

  (defmethod function-name ((ast typescript-ts-function-declaration))
    (source-text (typescript-ts-name ast)))

  (defmethod function-parameters ((ast typescript-ts-function-declaration))
    (direct-children (typescript-ts-parameters ast)))

  (defmethod end-of-parameter-list ((software typescript-ts)
                                    (fn typescript-ts-function-declaration))
    (ast-end software (typescript-ts-parameters fn)))

  (defmethod function-body ((ast typescript-ts-function-declaration))
    (typescript-ts-body ast))

  ;; Function signature (overload).

  (defmethod function-name ((ast typescript-ts-function-signature))
    (source-text (typescript-ts-name ast)))

  (defmethod function-parameters ((ast typescript-ts-function-signature))
    (direct-children (typescript-ts-parameters ast)))

  (defmethod end-of-parameter-list ((software typescript-ts)
                                    (fn typescript-ts-function-signature))
    (ast-end software (typescript-ts-parameters fn)))

  (defmethod function-body ((ast typescript-ts-function-signature)) nil)

  ;; Anonymous function (with function keyword)

  (defmethod function-parameters ((ast typescript-ts-function))
    (direct-children (typescript-ts-parameters ast)))

  (defmethod end-of-parameter-list ((software typescript-ts)
                                    (fn typescript-ts-function))
    (ast-end software (typescript-ts-parameters fn)))

  (defmethod function-body ((ast typescript-ts-function))
    (typescript-ts-body ast))

  ;; Arrow function.

  ;; NB an arrow function like `x => 1' has a non-null
  ;; `typescript-ts-parameter` slot, while `(x) => 1' has a non-null
  ;; `typescript-ts-parameters` slot.

  (defmethod function-parameters ((fn typescript-ts-arrow-function))
    (econd-let p
      ((typescript-ts-parameters fn)
       (direct-children p))
      ((typescript-ts-parameter fn)
       (list p))))

  (defmethod end-of-parameter-list ((software typescript-ts)
                                    (fn typescript-ts-arrow-function))
    (ast-end software
             (or (typescript-ts-parameters fn)
                 (typescript-ts-parameter fn)))))

  (defmethod function-body ((ast typescript-ts-arrow-function))
    (typescript-ts-body ast))

)
