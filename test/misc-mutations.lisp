;;;; misc-mutations.lisp --- Misc. mutation tests.
(defpackage :software-evolution-library/test/misc-mutations
  (:nicknames :sel/test/misc-mutations)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :software-evolution-library/stefil-plus
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang)
  (:export :test-misc-mutations))
(in-package :software-evolution-library/test/misc-mutations)
(in-readtable :curry-compose-reader-macros)
(defsuite test-misc-mutations "Misc. mutation tests." (clang-available-p))

(defvar *empty-while* nil "Holds the empty-while software object.")

(define-constant +explode-for-loop-dir+
    (append +etc-dir+ (list "explode-for-loop"))
  :test #'equalp
  :documentation "Location of the explode for loop example dir")

(defun explode-for-loop-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +explode-for-loop-dir+))

(define-constant +expand-arithmatic-op-dir+
    (append +etc-dir+ (list "expand-arithmatic-op"))
  :test #'equalp
  :documentation "Location of the expand arithmatic op example dir")

(defun expand-arithmatic-op-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +expand-arithmatic-op-dir+))

(define-constant +coalesce-while-loop-dir+
    (append +etc-dir+ (list "coalesce-while-loop"))
  :test #'equalp
  :documentation "Location of the coalesce while loop example dir")

(defun coalesce-while-loop-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +coalesce-while-loop-dir+))

(defixture empty-while-clang
  (:setup
   (setf *empty-while*
         (from-file (make-instance 'clang)
                    (coalesce-while-loop-dir "empty-while.c"))))
  (:teardown
   (setf *empty-while* nil)))

(defixture while-with-no-precedent-clang
  (:setup
   (setf *soft*
         (from-file (make-instance 'clang)
                    (coalesce-while-loop-dir "no-precedent.c"))))
  (:teardown
   (setf *soft* nil)))

(deftest single-decl-works ()
  (with-fixture scopes-clang
    (is (= 1 (length (ast-declares (stmt-with-text *scopes* "int a;")))))))

(deftest multiple-decl-works ()
  (with-fixture scopes-clang
    ;; NOTE: Why isn't this statement reliably found?
    (when-let* ((ast (stmt-with-text *scopes* "int f, g;" :no-error t)))
      (is (= 2 (length (ast-declares ast)))))))

(deftest (pick-for-loop-works :long-running) ()
  (with-fixture scopes-clang
    (is (eq :ForStmt (nest (ast-class)
                           (aget :stmt1)
                           (pick-for-loop *scopes*)))
        "Simply able to pick a for loop.")
    (let ((var (copy *scopes*)))
      (apply-mutation var (make-instance 'explode-for-loop :object var))
      (is (not (string= (genome var) (genome *scopes*)))
          "Exploded for loop changes genome.")
      (is (not (scan (quote-meta-chars "for") (genome var)))
          "Exploded for loop contains no for loop.")
      (flet ((run-and-get-return (obj)
               (with-temp-file (bin)
                 (phenome obj :bin bin)
                 (multiple-value-bind (stdout stderr return)
                     (shell bin)
                   (declare (ignorable stdout stderr))
                   return))))
        (is (= (run-and-get-return var) (run-and-get-return *scopes*))
            "Exploded for loop doesn't change program behavior.")))))

(deftest (explode-for-loop-mutation-works :long-running) ()
  "Test conversion of for loop variants computing factorials to while loops"
  (let ((simple-loop       (from-file (make-instance 'clang)
                                      (explode-for-loop-dir
                                       "simple-loop.c")))
        (no-initialization (from-file (make-instance 'clang)
                                      (explode-for-loop-dir
                                       "loop-no-initialization.c")))
        (no-conditional    (from-file (make-instance 'clang)
                                      (explode-for-loop-dir
                                       "loop-no-conditional.c")))
        (no-increment      (from-file (make-instance 'clang)
                                      (explode-for-loop-dir
                                       "loop-no-increment.c")))
        (no-body           (from-file (make-instance 'clang)
                                      (explode-for-loop-dir
                                       "loop-no-body.c"))))
    (apply-mutation simple-loop
                    (make-instance 'explode-for-loop :object simple-loop))
    (apply-mutation no-initialization
                    (make-instance 'explode-for-loop :object no-initialization))
    (apply-mutation no-conditional
                    (make-instance 'explode-for-loop :object no-conditional))
    (apply-mutation no-increment
                    (make-instance 'explode-for-loop :object no-increment))
    (apply-mutation no-body
                    (make-instance 'explode-for-loop :object no-body))

    (flet ((run-factorial (obj n)
             (with-temp-file (bin)
               (phenome obj :bin bin)
               (multiple-value-bind (stdout stderr exit)
                   (shell (format nil "~a ~d" bin n))
                 (declare (ignorable stdout stderr))
                 exit))))

      (is (= 120 (run-factorial simple-loop 5)))
      (is (= 120 (run-factorial no-initialization 5)))
      (is (= 120 (run-factorial no-conditional 5)))
      (is (= 120 (run-factorial no-increment 5)))
      (is (= 120 (run-factorial no-body 5)))
      (is (not (scan (quote-meta-chars "for") (genome simple-loop))))
      (is (not (scan (quote-meta-chars "for") (genome no-initialization))))
      (is (not (scan (quote-meta-chars "for") (genome no-conditional))))
      (is (not (scan (quote-meta-chars "for") (genome no-increment))))
      (is (not (scan (quote-meta-chars "for") (genome no-body)))))))

(deftest (pick-while-loop-works :long-running) ()
  (with-fixture scopes-clang
    (is (eq :WhileStmt (nest (ast-class)
                             (aget :stmt1)
                             (pick-while-loop *scopes*)))
        "Simply able to pick a while loop.")
    (let ((var (copy *scopes*)))
      (apply-mutation var (make-instance 'coalesce-while-loop :object var))
      (is (not (string= (genome var) (genome *scopes*)))
          "Coalesced while loop changes genome.")
      (is (not (scan (quote-meta-chars "while") (genome var)))
          "Coalesced while loop contains no while loop.")
      (flet ((run-and-get-return (obj)
               (with-temp-file (bin)
                 (phenome obj :bin bin)
                 (multiple-value-bind (stdout stderr return)
                     (shell bin)
                   (declare (ignorable stdout stderr))
                   return))))
        (is (= (run-and-get-return var) (run-and-get-return *scopes*))
            "Coalesced while loop doesn't change program behavior.")))))

(deftest pick-while-loop-works-even-with-empty-body ()
  (with-fixture empty-while-clang
    (let ((var (copy *empty-while*)))
      (apply-mutation var (make-instance 'coalesce-while-loop :object var))
      (is (not (scan (quote-meta-chars "while") (genome var)))
          "Coalesced while loop contains no while loop."))))

(deftest pick-while-loop-works-even-with-no-precedent ()
  (with-fixture while-with-no-precedent-clang
    (let ((var (copy *soft*)))
      (apply-mutation var (make-instance 'coalesce-while-loop :object var))
      (is (not (scan (quote-meta-chars "while") (genome var)))
          "Coalesced while loop contains no while loop."))))

(deftest (delete-decl-stmts-works :long-running) ()
  (with-fixture scopes-clang
    (let ((variant (copy *scopes*)))
      (apply-mutation
       variant
       `(cut-decl (:stmt1 . ,(stmt-with-text *scopes* "int a;"))))
      (is (phenome-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant))))
      (let ((stmt (or (stmt-with-text variant "b = 13;" :no-error t)
                      (stmt-with-text variant "c = 13;" :no-error t))))
        (is stmt)
        ;; unbound-vals are updated correctly
        (let ((unbound (mapcar {aget :name} (get-unbound-vals variant stmt))))
          (is (or (fully-every #'name= unbound '("b"))
                  (fully-every #'name= unbound '("c")))))))
    (let ((variant (copy *scopes*)))
      (apply-mutation
       variant
       `(cut-decl (:stmt1 . ,(stmt-with-text *scopes* "int d;"))))
      (is (phenome-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))
    (when-let* ((variant (copy *scopes*))
                (id (stmt-with-text *scopes* "int f, g;" :no-error t)))
      (apply-mutation variant `(cut-decl (:stmt1 . ,id)))
      (is (phenome-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))))

(deftest (swap-decls-works :long-running) ()
  (with-fixture scopes-clang
    ;; Check if the basic swap-decls mutation works.
    (let ((variant (copy *scopes*))
          (*bad-asts*
           (list (stmt-with-text *scopes* "int a;"))))
      (apply-mutation variant
                      (make-instance 'swap-decls :object variant))
      (is (phenome-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))))

(deftest (rename-variable-works :long-running) ()
  (with-fixture scopes-clang
    (let ((variant (copy *scopes*))
          (*bad-asts*
           (list (stmt-with-text *scopes* "b = 1;"))))
      (apply-mutation variant
                      (make-instance 'rename-variable :object variant))
      (is (phenome-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))))

(deftest expand-arithmatic-op-throws-error-if-no-arithmatic-ops ()
  (let ((obj (from-file (make-instance 'clang
                         :compiler "clang"
                         :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "no-compound-assign.c"))))
    (signals no-mutation-targets
             (build-op (make-instance 'expand-arithmatic-op :object obj) obj))))

(deftest expand-arithmatic-op-works-simple-compound-assignment ()
  (let ((obj (from-file (make-instance 'clang
                         :compiler "clang"
                         :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir
                         "simple-compound-assign.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "argc = argc * 2" :no-error t))))

(deftest expand-arithmatic-op-works-complex-compound-assignment ()
  (let ((obj (from-file (make-instance 'clang
                         :compiler "clang"
                         :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir
                         "complex-compound-assign.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "argc = argc + ((argc*4) / rand())" :no-error t))))

(deftest expand-arithmatic-op-works-increment ()
  (let ((obj (from-file (make-instance 'clang
                         :compiler "clang"
                         :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "increment.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "i = i + 1" :no-error t))))

(deftest expand-arithmatic-op-works-decrement ()
  (let ((obj (from-file (make-instance 'clang
                         :compiler "clang"
                         :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "decrement.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "argc = argc - 1" :no-error t))))

(deftest expand-arithmatic-op-works-field-increment ()
  (let ((obj (from-file (make-instance 'clang
                         :compiler "clang"
                         :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "field-increment.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "t.x = t.x + 1" :no-error t))))

(deftest expand-arithmatic-op-works-field-decrement ()
  (let ((obj (from-file (make-instance 'clang
                         :compiler "clang"
                         :flags '("-g" "-m32" "-O0"))
                        (expand-arithmatic-op-dir "field-decrement.c"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "t.x = t.x - 1" :no-error t))))

(deftest expand-arithmatic-op-works-class-member-increment ()
  (let ((obj (nest (from-file (make-instance 'clang
                               :compiler "clang"
                               :flags '("-g" "-m32" "-O0")))
                   (expand-arithmatic-op-dir "class-member-increment.cpp"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "x = x + 1" :no-error t))))

(deftest expand-arithmatic-op-works-class-member-decrement ()
  (let ((obj (nest (from-file (make-instance 'clang
                               :compiler "clang"
                               :flags '("-g" "-m32" "-O0")))
                   (expand-arithmatic-op-dir "class-member-decrement.cpp"))))
    (apply-mutation obj (make-instance 'expand-arithmatic-op :object obj))
    (is (stmt-with-text obj "x = x - 1" :no-error t))))
