(defpackage :software-evolution-library/test/util-clang
  (:nicknames :sel/test/util-clang)
  (:use :common-lisp
        :alexandria
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/stefil-plus
        :software-evolution-library/software/clang
        :software-evolution-library/software/clang-w-fodder
        :software-evolution-library/test/util)
  (:export :+etc-dir+
           :+gcd-dir+
           :+grep-prj-dir+
           :+multi-file-dir+
           :+headers-dir+
           ;; Variables referenced in tests
           :*headers*
           :*sqrt*
           :*hello-world*
           :*good-asts*
           :*bad-asts*
           :*huf*
           :*scopes*
           :*variety*))
(in-package :software-evolution-library/test/util-clang)

(defvar *good-asts* nil "Control pick-good")
(defvar *bad-asts* nil "Control pick-bad")
(defvar *headers* nil "Holds the headers software object.")
(defvar *sqrt* nil "Holds the hello world software object.")
(defvar *huf* nil "Holds the huf software object.")
(defvar *scopes* nil "Holds the scopes software object.")
(defvar *variety* nil "Holds the variety software object.")

(define-software clang-control-picks (clang) ())
(define-software new-clang-control-picks (new-clang) ())

(define-software clang-styleable-test-class (clang styleable) ())
(define-software new-clang-styleable-test-class (new-clang styleable) ())
(define-software mutation-failure-tester (clang) ())

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

(define-constant +clang-crossover-dir+
    (append +etc-dir+ (list "clang-crossover"))
  :test #'equalp
  :documentation "Location of clang crossover example directory")

(defun clang-crossover-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +clang-crossover-dir+))

(define-constant +lisp-bugs-dir+
    (append +etc-dir+ (list "lisp-bugs"))
  :test #'equalp
  :documentation "Location of the lisp bugs directory")

(defun lisp-bugs-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +lisp-bugs-dir+))

(define-constant +typedef-dir+ (append +etc-dir+ (list "typedef"))
  :test #'equalp
  :documentation "Path to the typedef example.")

(defun typedef-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +typedef-dir+))

(define-constant +strings-dir+ (append +etc-dir+ (list "strings"))
  :test #'equalp
  :documentation "Path to the strings examples.")

(defun strings-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +strings-dir+))

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

(define-constant +huf-dir+ (append +etc-dir+ (list "huf"))
  :test #'equalp
  :documentation "Location of the huf example directory")

(defun huf-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +huf-dir+))

(define-constant +scopes-dir+ (append +etc-dir+ (list "scopes"))
  :test #'equalp
  :documentation "Location of the scopes example directory")

(defun scopes-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +scopes-dir+))

(define-constant +clang-tidy-dir+
    (append +etc-dir+ (list "clang-tidy"))
  :test #'equalp
  :documentation "Location of the clang-tidy example dir")

(defun clang-tidy-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +clang-tidy-dir+))

(define-constant +type-of-var-dir+
    (append +etc-dir+ (list "type-of-var"))
  :test #'equalp
  :documentation "Location of the type-of-var example dir")

(defun type-of-var-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +type-of-var-dir+))

(define-constant +variety-dir+
    (append +etc-dir+ (list "variety"))
  :test #'equalp
  :documentation "Location of the variety example dir")

(defun variety-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +variety-dir+))

(define-constant +shadow-dir+ (append +etc-dir+ (list "shadow"))
  :test #'equalp
  :documentation "Path to the shadow example.")

(defun shadow-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +shadow-dir+))

(define-constant +unicode-dir+ (append +etc-dir+ (list "unicode"))
  :test #'equalp
  :documentation "Path to the unicode example.")

(defun unicode-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +unicode-dir+))

(define-constant +unbound-fun-dir+ (append +etc-dir+ (list "unbound-fun"))
  :test #'equalp
  :documentation "Location of the unbound-fun example directory")

(defun unbound-fun-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +unbound-fun-dir+))

(defvar *new-clang?* t)
(defvar *hello-world* nil "Holds the hello world software object.")

(defun make-clang (&rest key-args)
  (apply #'make-instance (if *new-clang?* 'new-clang 'clang) key-args))


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
  (add-macro obj
             (make-clang-macro
              :name "swap_"
              :body "swap_(I,J) do { int t_; t_ = a[(I)]; a[(I)] = a[(J)]; a[(J)] = t_; } while (0)"
              :hash 1179176719466053316))
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

(defixture huf-clang
  (:setup
   (setf *huf*
         (from-file (make-clang :compiler "gcc" :flags '("-g -m32 -O0"))
                    (huf-dir "huf.c")))
   (inject-missing-swap-macro *huf*))
  (:teardown
   (setf *huf* nil)))

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
