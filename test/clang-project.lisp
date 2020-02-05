;;;; clang-project.lisp --- Project tests.
(defpackage :software-evolution-library/test/clang-project
  (:nicknames :sel/test/clang-project)
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
   :arrow-macros                        ; FIXME: Remove.
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/utility
   :software-evolution-library/software/simple
   :software-evolution-library/software/source
   :software-evolution-library/software/clang
   :software-evolution-library/software/project
   :software-evolution-library/software/clang-project)
  (:import-from :uiop :nest)
  #-windows (:shadowing-import-from :osicat :file-permissions :pathname-as-directory)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :test-clang-project))
(in-package :software-evolution-library/test/clang-project)
(in-readtable :curry-compose-reader-macros)
(defsuite test-clang-project "Clang representation." (clang-mutate-available-p))

(defvar *s1*)
(defvar *s2*)
(defixture project
  (:setup
   (setf *s1* (make-instance 'simple))
   (setf (lines *s1*) (list "s1-genome"))
   (setf *s2* (make-instance 'simple))
   (setf (lines *s2*) (list "s2-genome"))
   (setf *project* (make-instance 'project
                     :evolve-files `(("s1" . ,*s1*)
                                     ("s2" . ,*s2*)))))
  (:teardown (setf *project* nil)))

(defmethod test-method ((obj simple) value)
  value)

(deftest to-file-fails-with-nil-path ()
  (with-fixture project
    (signals error (to-file *project* nil))))

(deftest (simple-to-from-file-without-project-dir-works :long-running) ()
  (with-fixture project
    (setf (project-dir *project*) nil)
    (with-temp-dir (file)
      (progn
        (to-file *project* file)
        (is (member :user-read (file-permissions file)))
        (is (member :user-write (file-permissions file)))
        (let ((s1-2 (from-file (make-instance 'simple)
                               (make-pathname :name "s1"
                                              :directory file)))
              (s2-2 (from-file (make-instance 'simple)
                               (make-pathname :name "s2"
                                              :directory file))))
          (is (equalp (genome *s1*) (genome s1-2)))
          (is (equalp (genome *s2*) (genome s2-2))))))))

(deftest ignored-paths-are-ignored ()
  (is (sel/sw/project::ignored-path-p
       "README" :ignore-paths '("README")))
  (is (sel/sw/project::ignored-path-p
       "README" :ignore-paths '("*")))
  (is (not (sel/sw/project::ignored-path-p
            "Makefile" :ignore-paths '("README"))))
  (is (sel/sw/project::ignored-path-p
       "etc/foo" :ignore-paths '("etc/*")))
  (is (sel/sw/project::ignored-path-p
       "./etc/foo" :ignore-paths '("etc/*")))
  (is (sel/sw/project::ignored-path-p
       "./etc/foo/bar/baz" :ignore-paths '("etc/**/*")))
  (is (not (sel/sw/project::ignored-path-p
            "Makefile" :ignore-paths '("etc/*"))))
  (is (not (sel/sw/project::ignored-path-p
            "src/foo" :ignore-paths '("etc/*"))))
  (is (not (sel/sw/project::ignored-path-p
            "./src/foo" :ignore-paths '("etc/*")))))

(deftest only-paths-are-only ()
  (is (not (sel/sw/project::ignored-path-p
            "README" :only-paths '("README"))))
  (is (sel/sw/project::ignored-path-p
       "Makefile" :only-paths '("README")))
  (is (not (sel/sw/project::ignored-path-p
            "etc/foo" :only-paths '("etc/*"))))
  (is (not (sel/sw/project::ignored-path-p
            "./etc/foo" :only-paths '("etc/*"))))
  (is (not (sel/sw/project::ignored-path-p
            "./etc/foo/bar/baz" :only-paths '("etc/**/*"))))
  (is (sel/sw/project::ignored-path-p
       "Makefile" :only-paths '("etc/*")))
  (is (sel/sw/project::ignored-path-p
       "src/foo" :only-paths '("etc/*")))
  (is (sel/sw/project::ignored-path-p
       "./src/foo" :only-paths '("etc/*"))))

(deftest (project-copy-preserves-permissions :long-running) ()
  ;; Ensure `to-file' preserves permissions on executable files.
  (nest
   (with-fixture grep-project)
   (with-temp-file (dir-path))
   (let ((dir (pathname-directory (pathname-as-directory dir-path))))
     (is (project-dir *project*))
     (to-file *project* dir-path)
     (is (member :user-exec
                 (file-permissions
                  (make-pathname :name "test"
                                 :type "sh"
                                 :directory (append dir (list "support")))))))))

(deftest (clang-project-test :long-running) ()
  (with-fixture grep-project
    (is (equal "make grep" (build-command *project*)))
    (is (equalp '("grep") (artifacts *project*)))
    (is (equal 1 (length (evolve-files *project*))))
    (is (equal "grep.c" (car (first (evolve-files *project*)))))
    (is (equal "cc" (compiler (cdr (first (evolve-files *project*))))))
    ;; Don't include binaries in `other-files'.
    (is (not (member "support/inputs/grepBinary"
                     (mapcar #'car (other-files *project*)) :test #'equalp)))
    (is (equal (namestring (make-pathname :directory +grep-prj-dir+))
               (second (member "-I" (flags (cdr (car (evolve-files *project*))))
                               :test #'equal))))
    (is (equal (namestring (make-pathname :directory +grep-prj-dir+))
               (namestring (project-dir *project*))))))

(deftest (apply-mutations-to-project-unique-test :long-running) ()
  (with-fixture clang-project
    (let ((proj (copy *project*)))
      (multiple-value-bind (objs muts)
          (apply-mutations proj
                           (make-instance 'clang-cut
                             :object proj
                             :targeter (lambda (obj)
                                         (mapcar (lambda (stmt)
                                                   (list (cons :stmt1 stmt)))
                                                 (bad-stmts obj)))
                             :picker (lambda (obj)
                                       (list (cons :stmt1
                                                   (random-elt
                                                    (bad-stmts obj))))))
                           10)
        (declare (ignorable objs))
        (is (< 1 (length (remove-duplicates (mapcar {targets} muts)
                                            :test #'equalp))))))))

(deftest (apply-picked-mutations-to-project-unique-test :long-running) ()
  (with-fixture clang-project
    (let ((proj (copy *project*)))
      (multiple-value-bind (objs muts)
          (apply-mutations proj
                           (make-instance 'clang-cut
                             :object proj
                             :targeter (lambda (obj)
                                         (mapcar (lambda (stmt)
                                                   (list (cons :stmt1 stmt)))
                                                 (bad-stmts obj)))
                             :picker (lambda (obj)
                                       (list (cons :stmt1
                                                   (random-elt
                                                    (bad-stmts obj))))))
                           10)
        (declare (ignorable objs))
        (is (< 1 (-> (mapcar {targets} muts)
                     (remove-duplicates :test #'equalp)
                     (length))))))))

(deftest clang-project-compilation-database-flags-test ()
  (is (equal (list "-DDIR='\"/tmp\"'" "-DIN" "\"-D_U_=a\"")
             (sel/sw/clang-project::compilation-db-entry-flags
              `((:command .
                          "cc -DDIR=\\\"/tmp\\\" -DIN \"-D_U_=a\"")))))
  (is (equal (list "-DDIR1='\"/tmp1\"'" "-DDIR2='\"/tmp2\"'")
             (sel/sw/clang-project::compilation-db-entry-flags
              `((:command .
                          "cc -DDIR1=\\\"/tmp1\\\" -DDIR2=\\\"/tmp2\\\"")))))
  (is (equal (list "-DDIR='\"\"'")
             (sel/sw/clang-project::compilation-db-entry-flags
              `((:command .
                          "cc -DDIR=\\\"\\\""))))))
