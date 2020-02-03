;;;; clang-syntactic-contexts.lisp --- Clang syntactic contexts.
(defpackage :software-evolution-library/test/clang-syntactic-contexts
  (:nicknames :sel/test/clang-syntactic-contexts)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/constants
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/utility)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :clang-syntactic-contexts))
(in-package :software-evolution-library/test/clang-syntactic-contexts)
(in-readtable :curry-compose-reader-macros)
(defsuite clang-syntactic-contexts)

(defvar *contexts* nil "Holds the syntactic-contexts software object.")

;; Tests of basic clang mutation operators
(defun count-matching-chars-in-stmt (char stmt)
  (let ((ast (if (listp stmt) (car stmt) stmt)))
    (count-if {eq char} (source-text ast))))

(defun find-function (obj name)
  (find-if [{string= name} #'ast-name]
           (functions obj)))

(deftest cut-full-stmt-removes-semicolon ()
  (with-fixture contexts
    (apply-mutation-ops *contexts*
                        `((:cut (:stmt1 . ,(stmt-with-text
                                            *contexts* "int x = 0;")))))
    (is (eq 0
            (count-matching-chars-in-stmt
             #\;
             (find-function *contexts* "full_stmt"))))))

(deftest insert-full-stmt-adds-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x = 0;")))
      (apply-mutation-ops *contexts*
                          `((:insert (:stmt1 . ,target)
                                     (:value1 . ,target)))))
    (is (eq 2 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "full_stmt"))))))

(deftest insert-braced-full-stmt-does-not-add-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x = 0;"))
          (inserted (function-body *contexts*
                                   (find-function *contexts*
                                                  "list"))))
      (apply-mutation-ops *contexts*
                          `((:insert (:stmt1 . ,target)
                                     (:value1 . ,inserted)))))
    (is (eq 1 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "full_stmt"))))))

(deftest replace-full-stmt-does-not-add-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x = 0;"))
          (replacement (stmt-with-text *contexts* "int x = 1")))
      (apply-mutation-ops *contexts*
                          `((:set (:stmt1 . ,target)
                                  (:value1 . ,replacement)))))
    (is (eq 1 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "full_stmt"))))))

(deftest replace-full-stmt-with-braced-removes-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x = 0;"))
          (replacement (function-body *contexts*
                                      (find-function *contexts*
                                                     "list"))))
      (apply-mutation-ops *contexts*
                          `((:set (:stmt1 . ,target)
                                  (:value1 . ,replacement)))))
    (is (eq 0 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "full_stmt"))))))

(deftest insert-non-full-stmt-into-fullstmt-context-makes-full ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x"))
          (location (stmt-starting-with-text *contexts* "if (1)")))
      (apply-mutation-ops *contexts*
                          `((:insert (:stmt1 . ,location)
                                     (:value1 . ,target))))
      (is (not (ast-full-stmt target))))
    (is (->> (find-function *contexts* "braced_body")
             (function-body *contexts*)
             (get-immediate-children *contexts*)
             (first)
             (ast-full-stmt)))))

(deftest cut-list-elt-removes-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int b")))
      (apply-mutation-ops *contexts*
                          `((:cut (:stmt1 . ,target)))))
    (is (starts-with-subseq
         "void list(int a,  int c)"
         (source-text (find-function *contexts* "list"))))))

(deftest insert-list-elt-adds-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int b")))
      (apply-mutation-ops *contexts*
                          `((:insert (:stmt1 . ,target)
                                     (:value1 . ,target)))))
    (is (starts-with-subseq
         "void list(int a, int b,int b, int c)"
         (source-text (find-function *contexts* "list"))))))

(deftest replace-list-elt-keeps-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int b"))
          (replacement (stmt-with-text *contexts* "int a")))
      (apply-mutation-ops *contexts*
                          `((:set (:stmt1 . ,target)
                                  (:value1 . ,replacement)))))
    (is (starts-with-subseq
         "void list(int a, int a, int c)"
         (source-text (find-function *contexts* "list"))))))

(deftest cut-final-list-elt-removes-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int c")))
      (apply-mutation-ops *contexts*
                          `((:cut (:stmt1 . ,target)))))
    (is (starts-with-subseq
         "void list(int a, int b)"
         (source-text (find-function *contexts* "list"))))))

(deftest insert-final-list-elt-adds-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int c")))
      (apply-mutation-ops *contexts*
                          `((:insert (:stmt1 . ,target)
                                     (:value1 . ,target)))))
    (is (starts-with-subseq
         "void list(int a, int b, int c,int c)"
         (source-text (find-function *contexts* "list"))))))

(deftest replace-final-list-elt-keeps-comma ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int c"))
          (replacement (stmt-with-text *contexts* "int a")))
      (apply-mutation-ops *contexts*
                          `((:set (:stmt1 . ,target)
                                  (:value1 . ,replacement)))))
    (is (starts-with-subseq
         "void list(int a, int b, int a)"
         (source-text (find-function *contexts* "list"))))))

(deftest replace-braced-adds-braces-and-semicolon ()
  (with-fixture contexts
    (let ((target
           (second (get-immediate-children
                    *contexts*
                    (car (get-immediate-children
                          *contexts*
                          (function-body *contexts*
                                         (find-function *contexts*
                                                        "braced_body")))))))
          (replacement (stmt-with-text *contexts* "int x = 0")))
      (apply-mutation-ops *contexts*
                          `((:set (:stmt1 . ,target)
                                  (:value1 . ,replacement)))))
    (let ((function (find-function *contexts* "braced_body")))
      (is (eq 2 (count-matching-chars-in-stmt #\{ function)))
      (is (eq 2 (count-matching-chars-in-stmt #\} function)))
      (is (eq 1 (count-matching-chars-in-stmt #\; function)))
      ;; Braces should be part of a new CompoundStmt AST rather than
      ;; free-floating text.
      (is (eq 2 (count-if «and [{eq :CompoundStmt} #'ast-class]
                               {ancestor-of *contexts* function}»
                          (stmt-asts *contexts*)))))))

(deftest (cut-unbraced-body-adds-nullstmt :long-running) ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "x = 2;")))
      (apply-mutation-ops *contexts*
                          `((:cut (:stmt1 . ,target)))))
    ;; Note -- this is no longer a good mutation, since there's a ; missing
    ;; Cutting statements from non-compound statement should introduce
    ;; a semicolon (or this should be fixed up)
    (is (eq 0 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "unbraced_body"))))
    (is (eq :NullStmt
            (some->> (stmt-starting-with-text *contexts* "if (2)")
                     (get-immediate-children *contexts*)
                     (second)
                     (ast-class))))))

(deftest cut-braced-body-adds-nullstmt ()
  (with-fixture contexts
    (let ((target (->> (stmt-with-text *contexts* "int x = 1;")
                       (get-parent-ast *contexts*))))
      (apply-mutation-ops *contexts*
                          `((:cut (:stmt1 . ,target)))))
    (is (eq 1 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "braced_body"))))
    (is (eq :NullStmt
            (some->> (stmt-starting-with-text *contexts* "if (1)")
                     (get-immediate-children *contexts*)
                     (second)
                     (ast-class))))))

(deftest replace-unbraced-body-keeps-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "x = 2;")))
      (apply-mutation-ops *contexts*
                          `((:set (:stmt1 . ,target)
                                  (:value1 . ,target)))))
    (is (eq 1 (count-matching-chars-in-stmt
               #\;
               (find-function *contexts* "unbraced_body"))))))

(deftest replace-unbraced-body-with-braced ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "x = 2;"))
          (replacement (function-body *contexts*
                                      (find-function *contexts*
                                                     "full_stmt"))))
      (apply-mutation-ops *contexts*
                          `((:set (:stmt1 . ,target)
                                  (:value1 . ,replacement)))))
    (let ((function (find-function *contexts* "unbraced_body")))
      (is (eq 2 (count-matching-chars-in-stmt #\{ function)))
      (is (eq 2 (count-matching-chars-in-stmt #\} function)))
      (is (eq 1 (count-matching-chars-in-stmt #\; function))))))

(deftest insert-after-full-stmt-adds-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int x = 0;")))
      (apply-mutation-ops *contexts*
                          `((:insert-after (:stmt1 . ,target)
                                           (:value1 . ,target)))))
    (let ((function (find-function *contexts* "full_stmt")))
      (is (eq 2 (count-matching-chars-in-stmt #\; function)))
      (is (eq 2 (count-if [{string= "int x = 0;"} #'source-text]
                          (asts *contexts*)))))))

(deftest insert-after-braced-body-adds-trailing-semicolon ()
  (with-fixture contexts
    (let ((target (->> (stmt-with-text *contexts* "int x = 1;")
                       (get-parent-ast *contexts*)))
          (inserted (stmt-with-text *contexts* "int x = 0;")))
      (apply-mutation-ops *contexts*
                          `((:insert-after (:stmt1 . ,target)
                                           (:value1 . ,inserted)))))
    (let ((function (find-function *contexts* "braced_body")))
      (is (eq 2 (count-matching-chars-in-stmt #\; function)))
      (is (eq 2 (count-matching-chars-in-stmt #\{ function)))
      (is (eq 2 (count-matching-chars-in-stmt #\} function)))
      (is (eq 2 (count-if [{string= "int x = 0;"} #'source-text]
                          (asts *contexts*)))))))

(deftest insert-braced-after-braced-body-does-not-add-semicolon ()
  (with-fixture contexts
    (let ((target (->> (stmt-with-text *contexts* "int x = 1;")
                       (get-parent-ast *contexts*))))
      (apply-mutation-ops *contexts*
                          `((:insert-after (:stmt1 . ,target)
                                           (:value1 . ,target)))))
    (let ((function (find-function *contexts* "braced_body")))
      (is (eq 2 (count-matching-chars-in-stmt #\; function)))
      (is (eq 3 (count-matching-chars-in-stmt #\{ function)))
      (is (eq 3 (count-matching-chars-in-stmt #\} function)))
      (is (eq 2 (count-if [{string= "int x = 1"} #'source-text]
                          (asts *contexts*)))))))

;; FIXME: this behavior is not implemented, so we can get incorrect
;; trees in some cases.
;; (deftest insert-before-unbraced-body-adds-braces ()
;;   ;; clang-mutate will simply insert at the given location, leading to
;;   ;; text like
;;   ;; if (2) int x = 1; x = 2

;;   ;; This is problematic when working directly on the ASTs because
;;   ;; both statements end up as direct children of the "if". We can fix
;;   ;; that by wrapping them in braces.
;;   (with-fixture contexts
;;     (let ((target (stmt-with-text *contexts* "x = 2"))
;;           (insert (stmt-with-text *contexts* "int x = 1")))
;;       (apply-mutation-ops *contexts*
;;                               `((:insert (:stmt1 . ,target)
;;                                          (:value1 . ,insert)))))
;;     (let ((function (find-function *contexts* "unbraced_body")))
;;       (is (eq 2 (count-matching-chars-in-stmt #\{ function)))
;;       (is (eq 2 (count-matching-chars-in-stmt #\} function)))
;;       (is (eq 2 (count-matching-chars-in-stmt #\; function)))
;;       (is (eq 2 (count-if «and [{string= "CompoundStmt"} #'ast-class]
;;                                {ancestor-of *contexts* function}»
;;                           (stmt-asts *contexts*)))))))

(deftest cut-field-removes-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int f1;")))
      (apply-mutation-ops *contexts*
                          `((:cut (:stmt1 . ,target)))))
    (let ((struct (stmt-starting-with-text *contexts* "struct")))
      (is (eq 2
              (count-matching-chars-in-stmt #\; struct))))))

(deftest insert-field-adds-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int f1;")))
      (apply-mutation-ops *contexts*
                          `((:insert (:stmt1 . ,target)
                                     (:value1 . ,target)))))
    (let ((struct (stmt-starting-with-text *contexts* "struct")))
      (is (eq 4
              (count-matching-chars-in-stmt #\; struct))))))

(deftest replace-field-keeps-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "int f1;"))
          (replacement (stmt-with-text *contexts* "int f2;")))
      (apply-mutation-ops *contexts*
                          `((:set (:stmt1 . ,target)
                                  (:value1 . ,replacement)))))
    (let ((struct (stmt-starting-with-text *contexts* "struct")))
      (is (eq 3
              (count-matching-chars-in-stmt #\; struct))))))

(deftest insert-toplevel-adds-semicolon ()
  (with-fixture contexts
    (let ((location (stmt-starting-with-text *contexts* "struct"))
          (inserted (stmt-with-text *contexts* "int x = 0;"))
          (semicolons (count-if {eq #\;} (genome *contexts*))))
      (apply-mutation-ops *contexts*
                          `((:insert (:stmt1 . ,location)
                                     (:value1 . ,inserted))))
      (is (eq (1+ semicolons)
              (count-if {eq #\;} (genome *contexts*)))))))

(deftest insert-toplevel-braced ()
  (with-fixture contexts
    (let ((location (stmt-starting-with-text *contexts* "struct"))
          (inserted (stmt-starting-with-text *contexts* "void list"))
          (semicolons (count-if {eq #\;} (genome *contexts*))))
      (apply-mutation-ops *contexts*
                          `((:insert (:stmt1 . ,location)
                                     (:value1 . ,inserted))))
      (is (eq semicolons
              (count-if {eq #\;} (genome *contexts*)))))))

(deftest splice-asts-and-text ()
  (with-fixture contexts
    (let ((location (stmt-with-text *contexts* "int x = 0;"))
          (inserted (list (format nil "/*comment 1*/~%")
                          (stmt-starting-with-text *contexts*
                                                   "int x = 1")
                          (format nil ";~%/*comment 2*/~%"))))
      (apply-mutation-ops *contexts*
                          `((:splice (:stmt1 . ,location)
                                     (:value1 . ,inserted))))

      (is (not (stmt-with-text *contexts* "int x = 0;" :no-error t)))
      (is (stmt-with-text *contexts* "int x = 1;" :no-error t))
      (is (eq 1
              (->> (stmt-starting-with-text *contexts* "void full_stmt")
                   (function-body *contexts*)
                   (get-immediate-children *contexts*)
                   (remove-if-not #'ast-full-stmt)
                   (length))))
      (is (search "comment 1" (genome *contexts*)))
      (is (search "comment 2" (genome *contexts*))))))

(deftest cut-initialization-list-preserves-semicolon ()
  (with-fixture contexts
    (let ((target (stmt-with-text *contexts* "{ 1, 2, 3 }")))
      (apply-mutation-ops *contexts*
                          `((:cut (:stmt1 . ,target)))))
    (is (eq 1 (->> (find-function *contexts* "initialization_list")
                   (count-matching-chars-in-stmt #\;))))))

(deftest replace-removes-trailing-semicolon-with-whitespace ()
  (with-fixture contexts
    (let ((location (stmt-starting-with-text *contexts* "MACRO"))
          (replacement (->> (find-function *contexts* "unbraced_body")
                            (get-immediate-children *contexts*)
                            (first))))
      (apply-mutation-ops *contexts*
                          `((:cut (:stmt1 . ,location)
                                  (:value1 . ,replacement)))))
    (is (eq 0 (->> (find-function *contexts* "trailing_semi_with_whitespace")
                   (count-matching-chars-in-stmt #\;))))))
