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
                :traverse-nodes)
  (:import-from :fset
                :convert)
  (:import-from :trivia
                :match)
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
