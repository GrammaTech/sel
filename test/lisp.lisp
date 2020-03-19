(defpackage :software-evolution-library/test/lisp
  (:nicknames :sel/test/lisp)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/software/lisp)
  (:import-from :uiop :nest)
  (:import-from :asdf
                :system-relative-pathname)
  (:import-from :functional-trees
                :traverse-nodes :map-tree)
  (:import-from :fset
                :convert)
  (:import-from :trivia
                :match)
  (:import-from :software-evolution-library/software/parseable
                :source-text)
  (:import-from :software-evolution-library/software/lisp
                :*string*)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :test-lisp))
(in-package :software-evolution-library/test/lisp)
(in-readtable :curry-compose-reader-macros)

(defsuite test-lisp "Lisp representation")

(deftest self-parse ()
  (nest
   (finishes)
   (from-file (make-instance 'lisp))
   (namestring)
   (system-relative-pathname "software-evolution-library" "test/lisp.lisp")))

(deftest read-eval-preserved ()
  (let ((ast (convert 'lisp-ast "(defvar *day-seconds* (* 24 60 60))"))
        (quit nil)
        found?)
    (traverse-nodes ast
                    (lambda (node)
                      (if (typep node 'expression-result)
                          (if (equal (expression node) '(* 24 60 60))
                              (progn
                                (setf found? t)
                                quit)
                              :keep-going)
                          :keep-going)))
    (is found?)))

(deftest read-conditional-preserved ()
  (let* ((ast (convert 'lisp-ast "(list #+sbcl :sbcl #-sbcl :not-sbcl)"))
         (quit nil)
         found-sbcl? found-not-sbcl?)
    (traverse-nodes ast
                    (lambda (node)
                      (if (typep node 'expression-result)
                          (case (expression node)
                            (:sbcl (setf found-sbcl? t)
                                   quit)
                            (:not-sbcl (setf found-not-sbcl? t)
                                       quit)
                            (t :keep-going))
                          :keep-going)))
    (is (and found-sbcl? found-not-sbcl?))))

(defun gather-features (ast)
  "Report all the features referenced in AST."
  (let ((features '()))
    (traverse-nodes ast
                    (lambda (node)
                      (if (typep node '(or sharpsign-minus sharpsign-plus))
                          (let ((feature-expression (flatten (ensure-list (feature-expression node)))))
                            (dolist (feature feature-expression)
                              (pushnew feature features))
                            :keep-going)
                          :keep-going)))
    (nest (remove-if {member _ '(:or :and :not)})
          (flatten)
          (mapcar #'expression
                  features))))

(deftest test-gather-features ()
  (let ((example
         ;; A real bit of code from ASDF.
         "(let* ((i (first (input-files o c)))
           (f (compile-file-pathname
               i #+clasp :output-type #+ecl :type #+(or clasp ecl) :fasl
               #+mkcl :fasl-p #+mkcl t)))
      `(,f ;; the fasl is the primary output, in first position
        #+clasp
        ,@(unless nil ;; was (use-ecl-byte-compiler-p)
            `(,(compile-file-pathname i :output-type :object)))
        #+clisp
        ,@`(,(make-pathname :type \"lib\" :defaults f))
        #+ecl
        ,@(unless (use-ecl-byte-compiler-p)
            `(,(compile-file-pathname i :type :object)))
        #+mkcl
        ,(compile-file-pathname i :fasl-p nil) ;; object file
        ,@(when (and *warnings-file-type* (not (builtin-system-p (component-system c))))
            `(,(make-pathname :type *warnings-file-type* :defaults f)))))"))
    (is (set-equal
         '(:clasp :ecl :mkcl :clisp)
         (gather-features (convert 'lisp-ast example))))))

(deftest rewrite-empty-feature-expression ()
  (is (equal "#+(or) (coda-non-grata)"
             (let* ((ast (map-tree (lambda (node)
                                     (if (and (typep node 'sharpsign-plus)
                                              (null (expression (feature-expression node))))
                                         (values
                                          (make-instance 'sharpsign-plus
                                            :feature-expression
                                            (make-instance 'expression-result
                                              :expression '(:or)
                                              :start 0
                                              :end 4
                                              :string-pointer "(or)")
                                            :expression (expression node))
                                          t)
                                         node))
                                   (convert 'lisp-ast "#+() (coda-non-grata)"))))
               (source-text ast)))))
