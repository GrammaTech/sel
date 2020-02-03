(defpackage :software-evolution-library/test/util
  (:nicknames :sel/test/util)
  (:use :common-lisp
        :alexandria
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/clang
        :software-evolution-library/test/util)
  (:export :+etc-dir+
           :+gcd-dir+
           :+grep-prj-dir+
           :+multi-file-dir+
           :+headers-dir+
           ;; Variables referenced in tests
           :*headers*
           :*hello-world*))
(in-package :software-evolution-library/test/util)

(defmethod good-stmts ((obj clang-control-picks))
  (or *good-asts* (stmt-asts obj)))
(defmethod bad-stmts ((obj clang-control-picks))
  (or *bad-asts* (stmt-asts obj)))

(defmethod good-stmts ((obj new-clang-control-picks))
  (or *good-asts* (stmt-asts obj)))
(defmethod bad-stmts ((obj new-clang-control-picks))
  (or *bad-asts* (stmt-asts obj)))

(define-constant +headers-dir+ (append +etc-dir+ (list "headers"))
  :test #'equalp
  :documentation "Path to directory holding headers.")

(define-constant +hello-world-dir+ (append +etc-dir+ (list "hello-world"))
  :test #'equalp
  :documentation "Location of the hello world example directory")

(defvar *new-clang?* t)
(defvar *hello-world* nil "Holds the hello world software object.")

(defixture empty-function-body-crossover-bug-clang
  (:setup
   (setf *soft*
         (from-file (make-clang :compiler "clang"
                                :flags '("-g -m32 -O0"))
                    (clang-crossover-dir
                     "empty-function-body-crossover-bug.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture select-intraprocedural-pair-non-null-clang
  (:setup
   (setf *soft*
         (from-file (make-clang :compiler "clang"
                                :flags '("-g -m32 -O0"))
                    (clang-crossover-dir
                     "select-intraprocedural-pair-non-null.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture intraprocedural-2pt-crossover-bug-clang
  (:setup
   (setf *soft*
         (from-file (make-clang :compiler "clang"
                                :flags '("-g -m32 -O0"))
                    (clang-crossover-dir
                     "intraprocedural-2pt-crossover-bug.c"))))
  (:teardown
   (setf *soft* nil)))

(defun make-clang-control-picks (&rest args)
  (apply #'make-instance
         (if *new-clang?* 'new-clang-control-picks 'clang-control-picks)
         :allow-other-keys t
         args))

(defun headers-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +headers-dir+))

(defun hello-world-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +hello-world-dir+))

(defixture binary-search-clang
  (:setup
   (setf *binary-search*
         (from-file
          (make-instance 'new-clang
            :flags (list
                    "-I"
                    (namestring (make-pathname :directory +etc-dir+))))
          (make-pathname
           :name "binary_search"
           :type "c"
           :directory +etc-dir+))))
  (:teardown
   (setf *binary-search* nil)))

#-windows
(defixture gcd-clang
  (:setup
   (setf *gcd*
         (from-file (make-instance 'clang :compiler "clang")
                    (gcd-dir "gcd.c"))))
  (:teardown
   (setf *gcd* nil)))

#+windows
(defixture gcd-clang
  (:setup
   (setf *gcd*
         (from-file (make-clang :compiler "clang")
                    (gcd-dir "gcd.windows.c")))
   (setf (sel/sw/new-clang::include-dirs *gcd*)
         (split ";" (uiop:getenv "INCLUDE"))))
  (:teardown
   (setf *gcd* nil)))

(defixture gcd-wo-curlies-clang
  (:setup
   (setf *gcd*
         (from-file (make-instance 'clang :compiler "clang")
                    (gcd-dir "gcd-wo-curlies.c"))))
  (:teardown
   (setf *gcd* nil)))

(defixture headers-clang
  (:setup
   (setf *headers*
         (from-file (make-clang
                     :compiler "clang"
                     :flags (list "-I" (namestring
                                        (make-pathname
                                         :directory +headers-dir+))))
                    (headers-dir "main.c"))))
  (:teardown
   (setf *hello-world* nil)))

(defixture hello-world-clang
  (:setup
   (setf *hello-world*
         (from-file (make-clang :compiler "clang"
                                :flags '("-g -m32 -O0"))
                    (hello-world-dir "hello_world.c"))))
  (:teardown
   (setf *hello-world* nil)))

(defixture sqrt-clang
  (:setup
   (setf *sqrt*
         (from-file (make-clang)
                    (make-pathname :name "sqrt"
                                   :type "c"
                                   :directory +etc-dir+))))
  (:teardown
   (setf *sqrt* nil)))

(defixture print-env-clang
  (:setup (setf *soft*
                (from-file (make-clang :compiler "clang")
                           (make-pathname :directory +etc-dir+
                                          :name "print-env"
                                          :type "c"))))
  (:teardown (setf *soft* nil)))

(defixture hello-world-clang-control-picks
  (:setup
   (setf *hello-world*
         (from-file (make-clang-control-picks :compiler "clang-3.7"
                                              :flags '("-g -m32 -O0"))
                    (hello-world-dir "hello_world.c"))))
  (:teardown
   (setf *hello-world* nil)))

(defixture hello-world-clang-w-fodder
  (:setup
   (setf *database*
         (with-open-file (in (make-pathname :name "euler-example.json"
                                            :directory +etc-dir+))
           (make-instance 'json-database :json-stream in)))
   (setf *hello-world*
         (from-file (make-instance 'clang-w-fodder :compiler "clang"
                                   :flags '("-g -m32 -O0"))
                    (hello-world-dir "hello_world.c"))))
  (:teardown
   (setf *database* nil)
   (setf *hello-world* nil)))

(defixture no-insert-fodder-decl-mutation-targets-clang
  (:setup
   (setf *database*
         (with-open-file (in (make-pathname :name "euler-example.json"
                                            :directory +etc-dir+))
           (make-instance 'json-database :json-stream in)))
   (setf *soft* (from-file (make-instance 'clang-w-fodder)
                           (lisp-bugs-dir
                            "no-insert-fodder-decl-mutation-targets.c"))))
  (:teardown
   (setf *database* nil)
   (setf *soft* nil)))

(defixture contexts
  (:setup
   (setf *contexts*
         (from-file (make-clang :compiler "clang-3.7")
                    (contexts-dir "contexts.c"))))
  (:teardown
   (setf *contexts* nil)))

(defixture typedef
  (:setup
   (setf *soft*
         (from-file (make-clang :compiler "clang-3.7")
                    (typedef-dir "typedef.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture cpp-strings
  (:setup
   (setf *soft*
         (from-file (make-clang :compiler "clang++")
                    (strings-dir "cpp-strings.cpp"))))
  (:teardown
   (setf *soft* nil)))

(defixture c-strings
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (strings-dir "c-strings.c"))))
  (:teardown
   (setf *soft* nil)))

(defun inject-missing-swap-macro (obj)
  ;; Inject a macro that clang-mutate currently misses, then force the ASTs to
  ;; be recalculated by setting the genome-string.
  (->> (make-clang-macro
        :name "swap_"
        :body "swap_(I,J) do { int t_; t_ = a[(I)]; a[(I)] = a[(J)]; a[(J)] = t_; } while (0)"
        :hash 1179176719466053316)
       (add-macro obj))
  (setf (genome-string obj) (genome-string obj)))

(defixture gcd-clang-w-fodder
  (:setup
   (setf *database*
         (with-open-file (in (make-pathname :name "euler-example"
                                            :type "json"
                                            :directory +etc-dir+))
           (make-instance 'json-database :json-stream in)))
   (setf *gcd*
         (from-file
          (make-instance 'clang-w-fodder
            :flags (list
                    "-I"
                    (namestring (make-pathname :directory +etc-dir+))))
          (gcd-dir "gcd.c"))))
  (:teardown
   (setf *database* nil)
   (setf *gcd* nil)))

(defixture empty-while-clang
  (:setup
   (setf *empty-while*
         (from-file (make-clang)
                    (coalesce-while-loop-dir "empty-while.c"))))
  (:teardown
   (setf *empty-while* nil)))

(defixture while-with-no-precedent-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (coalesce-while-loop-dir "no-precedent.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture huf-clang
  (:setup
   (setf *huf*
         (from-file (make-clang :compiler "gcc" :flags '("-g -m32 -O0"))
                    (huf-dir "huf.c")))
   (inject-missing-swap-macro *huf*))
  (:teardown
   (setf *huf* nil)))

(defixture nested-clang
  (:setup
   (setf *nested*
         (from-file (make-clang :compiler "gcc" :flags '("-g -m32 -O0"))
                    (nested-dir "nested.c")))
   (inject-missing-swap-macro *nested*))
  (:teardown
   (setf *nested* nil)))

(defixture scopes-clang
  (:setup
   (setf *scopes*
         (from-file (make-clang-control-picks
                     :compiler "clang" :flags '("-g -m32 -O0"))
                    (scopes-dir "scopes.c"))))
  (:teardown
   (setf *scopes* nil)))

(defixture scopes2-clang
  (:setup
   (setf *scopes*
         (from-file (make-clang-control-picks
                     :compiler "clang" :flags '("-g -m32 -O0"))
                    (scopes-dir "scopes2.c"))))
  (:teardown
   (setf *scopes* nil)))

(defixture scopes-type-field-clang
  (:setup
   (setf *scopes*
         (from-file (make-clang
                     :compiler "clang" :flags '("-g -m32 -O0"))
                    (scopes-dir "scopes-type-field.c"))))
  (:teardown
   (setf *scopes* nil)))

(defixture scopes-cxx-clang
  (:setup
   (setf *scopes*
         (from-file (make-clang-control-picks :compiler "clang")
                    (scopes-dir "scopes.cxx"))))
  (:teardown
   (setf *scopes* nil)))

(defixture collatz-clang
  (:setup
   (setf *collatz*
         (from-file (make-clang
                     :compiler "clang"
                     :flags '("-m32" "-O0" "-g" "-c"))
                    (collatz-dir "collatz.c"))))
  (:teardown
   (setf *collatz* nil)))

(defixture fib-clang
  (:setup
   (setf *fib*
         (from-file (make-clang
                     :compiler "clang"
                     :flags '("-m32" "-O0" "-g" "-c"))
                    (fib-dir "fib.c"))))
  (:teardown
   (setf *fib* nil)))

(defixture crossover-no-compound-stmt-clang
  (:setup
   (setf *soft*
         (from-file (make-clang
                     :compiler "clang"
                     :flags '("-m32" "-O0" "-g"))
                    (clang-crossover-dir
                     "crossover-no-compound-stmt.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture crossover-switch-stmt-clang
  (:setup
   (setf *soft*
         (from-file (make-clang
                     :compiler "clang"
                     :flags '("-m32" "-O0" "-g"))
                    (clang-crossover-dir
                     "crossover-switch-stmt.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture tidy-adds-braces-clang
  (:setup
   (setf *soft*
         (from-file (make-clang
                     :compiler "clang"
                     :flags '("-m32" "-O0" "-g"))
                    (clang-tidy-dir "tidy-adds-braces.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture type-of-var-clang
  (:setup
   (setf *soft*
         (from-file (make-clang
                     :compiler "clang"
                     :flags '("-m32" "-O0" "-g"))
                    (type-of-var-dir "type-of-var.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture variety-clang
  (:setup
   (setf *variety*
         (from-file (make-clang
                     :compiler "clang"
                     :flags '("-m32" "-O0" "-g"))
                    (variety-dir "variety.c"))))
  (:teardown
   (setf *variety* nil)))

(defixture shadow-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (shadow-dir "shadow.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture unicode-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (unicode-dir "unicode.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture fl-tiny-clang
  (:setup (setf *soft*
                (from-file (make-clang)
                           (fl-tiny-dir "tiny-test.c")))
          (setf *test-suite*
                (make-instance 'test-suite
                  :test-cases
                  (iter (for i below 6)
                        (collecting
                         (make-instance 'test-case
                           :program-name (namestring (fl-tiny-dir "fitness.sh"))
                           :program-args (list :bin (write-to-string i))))))))
  (:teardown
   (setf *soft* nil)
   (setf *test-suite* nil)))

(defixture cs-tiny-clang
  (:setup (setf *soft*
                (from-file (make-clang)
                           (cs-tiny-dir "tiny-test.c")))
          (setf *test-suite*
                (make-instance 'test-suite
                  :test-cases
                  (iter (for i below 6)
                        (collecting
                         (make-instance 'test-case
                           :program-name (namestring (cs-tiny-dir "fitness.sh"))
                           :program-args (list :bin (write-to-string i))))))))
  (:teardown
   (setf *soft* nil)
   (setf *test-suite* nil)))

(defixture cs-tighten-clang
  (:setup (setf
           *soft*
           (from-file (make-clang)
                      (cs-tighten-dir "test-tighten.c"))
           *test-suite*
           (make-instance 'test-suite
             :test-cases
             (iter (for i below 6)
                   (collecting
                    (make-instance 'test-case
                      :program-name (namestring (cs-tighten-dir "fitness.sh"))
                      :program-args (list :bin (write-to-string i))))))))
  (:teardown
   (setf *soft* nil)
   (setf *test-suite* nil)))

(defixture cs-add-guard-clang
  (:setup (setf
           *soft*
           (from-file (make-clang)
                      (cs-add-guard-dir "test-add-guard.c"))
           *test-suite*
           (nest
            (make-instance 'test-suite :test-cases)
            (iter (for i below 8))
            (collecting
             (make-instance 'test-case
               :program-name (namestring (cs-add-guard-dir "fitness.sh"))
               :program-args (list :bin (write-to-string i)))))))
  (:teardown
   (setf *soft* nil)
   (setf *test-suite* nil)))

(defixture cs-divide-clang
  (:setup (setf
           *soft*
           (from-file (make-clang)
                      (cs-divide-dir "divide.c"))
           *test-suite*
           (make-instance 'test-suite
             :test-cases
             (iter (for i below 5)
                   (collecting
                    (make-instance 'test-case
                      :program-name (namestring (cs-divide-dir "fitness.sh"))
                      :program-args (list :bin (write-to-string i))))))))
  (:teardown
   (setf *soft* nil)
   (setf *test-suite* nil)))


(defixture switch-macros-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (switch-macros-dir "switch-macros.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture simple-macros-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (simple-macros-dir "simple-macros.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture assert-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (assert-dir "assert.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture long-running-program-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (long-running-program-dir "long-running-program.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture typedef-type-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (typedef-type-dir "typedef-type.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture unbound-fun-clang
  (:setup
   (setf *soft*
         (from-file (make-clang)
                    (unbound-fun-dir "unbound-fun.c"))))
  (:teardown
   (setf *soft* nil)))

(defixture no-mutation-targets-clang
  (:setup
   (setf *soft* (from-file (make-clang)
                           (lisp-bugs-dir "no-mutation-targets.c"))))
  (:teardown
   (setf *soft* nil)))
