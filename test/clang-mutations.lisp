;;;; clang-mutations.lisp --- Detailed clang mutation tests.
(defpackage :software-evolution-library/test/clang-mutations
  (:nicknames :sel/test/clang-mutations)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/utility
   :software-evolution-library/software/ast
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang
   :software-evolution-library/software/new-clang
   :software-evolution-library/components/condition-synthesis)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :test-clang-mutations))
(in-package :software-evolution-library/test/clang-mutations)
(in-readtable :curry-compose-reader-macros)
(defsuite test-clang-mutations "Detailed clang mutation tests."
  (clang-mutate-available-p))

(defvar *nested* nil "Holds the nested software object.")

(define-constant +nested-dir+ (append +etc-dir+ (list "nested"))
  :test #'equalp
  :documentation "Location of the nested example directory")

(defun nested-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +nested-dir+))

(defixture nested-clang
  (:setup
   (setf *nested*
         (from-file (make-clang :compiler "gcc" :flags '("-g -m32 -O0"))
                    (nested-dir "nested.c")))
   (inject-missing-swap-macro *nested*))
  (:teardown
   (setf *nested* nil)))

(defixture gcd-clang-control-picks
  (:setup
   (setf *gcd*
         (from-file (make-clang-control-picks :compiler "clang-3.7")
                    (gcd-dir "gcd.c"))))
  (:teardown
   (setf *gcd* nil)))

(defun asts-with-text (obj &rest texts)
  (mapcar {stmt-with-text obj} texts))

(deftest cut-full-removes-full-stmt ()
  (with-fixture hello-world-clang
    (let ((*clang-mutation-types* '((clang-cut-full . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (< (count-if #'ast-full-stmt (asts variant))
             (count-if #'ast-full-stmt (asts *hello-world*)))))))

(deftest cut-removes-any-stmt ()
  (with-fixture hello-world-clang
    (let ((*clang-mutation-types* '((clang-cut . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (< (length (asts variant)) (length (asts *hello-world*)))))))

(deftest insert-full-adds-full-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*clang-mutation-types* '((clang-insert-full . 1)))
          (*bad-asts* (asts-with-text *hello-world* "return 0;"))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (> (count-if #'ast-full-stmt (asts variant))
             (count-if #'ast-full-stmt (asts *hello-world*)))))))

(deftest insert-adds-any-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "printf"))
          (*good-asts* (asts-with-text *hello-world* "printf"))
          (*clang-mutation-types* '((clang-insert . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-starting-with-text variant "printfprintf")))))

(deftest insert-same-adds-same-class ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "0"))
          (*clang-mutation-types* '((clang-insert-same . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-with-text variant "return 00;" :no-error t)))))

(deftest insert-full-same-adds-same-class-full-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "printf" "return 0;"))
          (*clang-mutation-types* '((clang-insert-full-same . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (> (count-if [{eq :ReturnStmt} #'ast-class]
                       (asts variant))
             (count-if [{eq :ReturnStmt} #'ast-class]
                       (asts *hello-world*)))))))

(deftest replace-changes-any-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "0"))
          (*good-asts* (asts-with-text *hello-world* "printf"))
          (*clang-mutation-types* '((clang-replace . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-with-text variant "return printf;" :no-error t)))))

(deftest replace-full-changes-full-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "printf" "return 0;"))
          (*good-asts* (asts-with-text *hello-world*
                                       "0" "printf(\"Hello, World!\\n\");"))
          (*clang-mutation-types* '((clang-replace-full . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (> (count-if [{eq :CallExpr} #'ast-class]
                       (asts variant))
             (count-if [{eq :CallExpr} #'ast-class]
                       (asts *hello-world*)))))))

(deftest replace-same-changes-same-class ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "\"Hello, World!\\n\""))
          (*good-asts* (asts-with-text *hello-world*
                                       "0" "printf"))
          (*clang-mutation-types* '((clang-replace-same . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-with-text variant "printf(printf);" :no-error t)))))

(deftest replace-full-same-changes-same-class-full-stmt ()
  (with-fixture hello-world-clang
    (let ((*clang-mutation-types* '((clang-replace-full-same . 1)))
          (variant (copy *hello-world*)))
      (multiple-value-bind  (variant mutation) (mutate variant)
        (is (ast-full-stmt (aget :stmt1 (targets mutation))))
        (is (ast-full-stmt (aget :stmt2 (targets mutation))))

        ;; Not a very interesting test: this can only replace a
        ;; statement with itself, but sometimes there are whitespace
        ;; changes. Just compare AST classes to avoid spurious
        ;; failures.
        (is (equal (mapcar [#'ast-class] (asts variant))
                   (mapcar [#'ast-class] (asts *hello-world*))))))))

(deftest swap-changes-any-stmts ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world*
                                      "\"Hello, World!\\n\"" "0"))
          (*clang-mutation-types* '((clang-swap . 1)))
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-with-text variant "\"Hello, World!\\n\"" :no-error t))
      (is (stmt-with-text variant "0" :no-error t)))))

(deftest move-changes-any-stmts ()
  (with-fixture hello-world-clang-control-picks
    (let* ((bad-1 "printf(\"Hello, World!\\n\");")
           (bad-2 "return 0;")
           (*bad-asts* (asts-with-text *hello-world* bad-1 bad-2))
           (*clang-mutation-types* '((clang-move . 1))))
      (multiple-value-bind (variant mut) (mutate (copy *hello-world*))
        ;; If both targets are the same, genome will not change.
        (unless (eq (aget :stmt1 (targets mut)) (aget :stmt2 (targets mut)))
          (is (not (string= (genome *hello-world*) (genome variant)))
              "Move changes genome."))
        ;; Still exist (> 0).
        (is (stmt-with-text variant bad-1 :no-error t)
            "Move doesn't remove \"Hello, World!\\n\".")
        (is (stmt-with-text variant bad-2 :no-error t)
            "Move doesn't remove \"0\".")
        ;; No duplicates (< 2).
        (is
         (= 1 (length (all-matches-as-strings (quote-meta-chars bad-1)
                                              (genome variant))))
         "Move doesn't duplicate \"Hello, World!\\n\".")
        (is
         (= 1 (length (all-matches-as-strings (quote-meta-chars bad-2)
                                              (genome variant))))
         "Move doesn't duplicate \"0\".")))))

(deftest swap-full-changes-full-stmts ()
  (with-fixture hello-world-clang-control-picks
    (let ((*clang-mutation-types* '((clang-swap-full . 1)))
          ;; Avoid swapping the function body
          (*bad-asts* (remove-if [{member _  '(:CompoundStmt :Function)}
                                  #'ast-class]
                                 (asts *hello-world*)))
          (variant (copy *hello-world*)))

      (multiple-value-bind  (variant mutation) (mutate variant)
        ;; We can't predict exactly what will be swapped. Just
        ;; sanity check.
        (is (ast-full-stmt (aget :stmt1 (targets mutation))))
        (is (ast-full-stmt (aget :stmt2 (targets mutation))))
        (is (stmt-with-text variant "printf" :no-error t))
        (is (stmt-with-text variant "return 0;" :no-error t))))))

(deftest swap-full-same-changes-same-class-full-stmt ()
  (with-fixture hello-world-clang
    (let ((*clang-mutation-types* '((clang-swap-full-same . 1)))
          (variant (copy *hello-world*)))
      (multiple-value-bind  (variant mutation) (mutate variant)
        (is (ast-full-stmt (aget :stmt1 (targets mutation))))
        (is (ast-full-stmt (aget :stmt2 (targets mutation))))

        ;; Not a very interesting test: this can only swap a
        ;; statement with itself, but sometimes there are whitespace
        ;; changes. Just compare AST classes to avoid spurious
        ;; failures.
        (is (equal (mapcar #'ast-class (asts variant))
                   (mapcar #'ast-class (asts *hello-world*))))))))

(deftest (unguard-conditional-compound-statements :long-running) ()
  (flet ((subsequent-lines-p (genome-string first second)
           (nest
            (string= second)
            (second)
            (member first (mapcar {string-trim " "}
                                  (split-sequence #\Newline genome-string))
                    :test #'string=))))
    (with-fixture nested-clang
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome
              (apply-mutation copy
                              (make-instance 'clang-promote-guarded
                                :object copy
                                :targets (nest (third)
                                               (get-parent-asts *nested*)
                                               (stmt-with-text *nested* "puts('WHILE');"))))))
           "/* While loop. */"
           "puts('WHILE');")
          "Promotes single-line body from within while loop.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome
              (apply-mutation copy
                              (make-instance 'clang-promote-guarded
                                :object copy
                                :targets (nest (third)
                                               (get-parent-asts *nested*)
                                               (stmt-with-text *nested* "puts('DO');"))))))
           "/* Do loop. */"
           "puts('DO');")
          "Promotes single-line body from within do loop.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome
              (apply-mutation copy
                              (make-instance 'clang-promote-guarded
                                :object copy
                                :targets (nest (third)
                                               (get-parent-asts *nested*)
                                               (stmt-with-text *nested* "puts('FOR-1');"))))))
           "/* For loop. */"
           "puts('FOR-1');")
          "Promotes single-line body from within for loop.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome
              (apply-mutation copy
                              (make-instance 'clang-promote-guarded
                                :object copy
                                :targets (nest (third)
                                               (get-parent-asts *nested*)
                                               (stmt-with-text *nested* "puts('FOR-2');"))))))
           "/* For loop with empty header. */"
           "puts('FOR-2');")
          "Promotes single-line body from within for loop 2.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome
              (apply-mutation copy
                              (make-instance 'clang-promote-guarded
                                :object copy
                                :targets (nest (third)
                                               (get-parent-asts *nested*)
                                               (stmt-with-text *nested* "puts('IF-1');"))))))
           "/* Single child. */"
           "puts('IF-1');")
          "Promotes single-line sole branch of if.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome
              (apply-mutation copy
                              (make-instance 'clang-promote-guarded
                                :object copy
                                :targets (nest (third)
                                               (get-parent-asts *nested*)
                                               (stmt-with-text *nested* "puts('IF-2');"))))))
           "/* Empty then. */"
           "puts('IF-2');")
          "Promotes single-line else of if w/o then.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome
              (apply-mutation copy
                              (make-instance 'clang-promote-guarded
                                :object copy
                                :targets (nest (third)
                                               (get-parent-asts *nested*)
                                               (stmt-with-text *nested* "puts('IF-3');"))))))
           "/* Empty else. */"
           "puts('IF-3');")
          "Promotes single-line then of if w/o else.")
      (is (subsequent-lines-p
           (let ((copy (copy *nested*)))
             (genome
              (apply-mutation copy
                              (make-instance 'clang-promote-guarded
                                :object copy
                                :targets (nest (third)
                                               (get-parent-asts *nested*)
                                               (stmt-with-text *nested* "puts('IF-3');"))))))
           "/* Empty else. */"
           "puts('IF-3');")
          "Promotes single-line then of if w/o else.")
      (let ((genome-string
             (let ((copy (copy *nested*)))
               (genome
                (apply-mutation copy
                                (make-instance 'clang-promote-guarded
                                  :object copy
                                  :targets (nest (third)
                                                 (get-parent-asts *nested*)
                                                 (stmt-with-text *nested* "puts('MULTILINE');"))))))))
        (is (and (subsequent-lines-p genome-string
                                     "/* Multiline loop. */"
                                     "puts('MULTILINE');")
                 (subsequent-lines-p genome-string
                                     "puts('MULTILINE');"
                                     "puts('WHILE-1');")
                 (subsequent-lines-p genome-string ; Ensure peels bananas.
                                     "puts('WHILE-1');"
                                     "j++;"))
            "Promotes multi-line body from within while loop.")))

    (with-fixture gcd-wo-curlies-clang
      (let ((genome-string
             (let ((copy (copy *gcd*)))
               (genome
                (apply-mutation copy
                                (make-instance 'clang-promote-guarded
                                  :object copy
                                  :targets (stmt-starting-with-text *gcd* "if (a == 0)")))))))
        (is (and (subsequent-lines-p genome-string
                                     "printf(\"%g\\n\", b);"
                                     "while (b != 0)"))
            "Promotes unbraced then from within if wi/o else.")))))

(deftest if-to-while-test ()
  (with-fixture gcd-clang-control-picks
    (let ((*clang-mutation-types* '((if-to-while . 1)))
          (*bad-asts* (list (find-if [{eq :IfStmt} #'ast-class]
                                     (stmt-asts *gcd*)))))
      (multiple-value-bind  (variant mutation) (mutate (copy *gcd*))
        (is (eq :IfStmt
                (ast-class (targets mutation))))
        (let ((stmt (stmt-starting-with-text variant "while")))
          (is stmt)
          (is (eq :WhileStmt (ast-class stmt))))))))
