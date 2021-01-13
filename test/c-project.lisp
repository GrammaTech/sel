;;;; c-project.lisp --- Project tests.
(defpackage :software-evolution-library/test/c-project
  (:nicknames :sel/test/c-project)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/simple
   :software-evolution-library/software/compilable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/parseable
   :software-evolution-library/software/project
   :software-evolution-library/software/c-project)
  #-windows (:shadowing-import-from :osicat :file-permissions :pathname-as-directory)
  (:export :test-c-project))
(in-package :software-evolution-library/test/c-project)
(in-readtable :curry-compose-reader-macros)

(defun c-available-p ()
  "Returns true is the c tree-sitter parser is loaded."
  (let* ((p (find-package :sel/sw/tree-sitter))
         (s (and p (find-symbol "C" p))))
    (and s (find-class s))))

(defsuite test-c-project
    "C tree-sitter project representation."
  (c-available-p))

(defvar *s1*)
(defvar *s2*)
(defixture c-project
  (:setup
   (setf *s1* (make-instance 'simple))
   (setf (lines *s1*) (list "s1-genome"))
   (setf *s2* (make-instance 'simple))
   (setf (lines *s2*) (list "s2-genome"))
   (setf *project* (make-instance 'project
                     :evolve-files `(("s1" . ,*s1*)
                                     ("s2" . ,*s2*)))))
  (:teardown (setf *project* nil)))

(defixture multiple-artifacts-project
  (:setup
   (setf *project*
         (from-file
          (make-instance 'c-project
            :build-command "make"
            :artifacts (list "run_shared"
                             "shared.so.link"
                             "nested-dir/helper"))
          (make-pathname :directory +multiple-artifact-dir+))))
  (:teardown (setf *project* nil)))

(defixture grep-project
  (:setup
   (setf *project*
         (from-file
          (make-instance 'c-project
            :build-command "make grep"
            :artifacts '("grep")
            :compilation-database
            (list
             (list
              (cons :file
                    (namestring
                     (make-pathname :directory +grep-prj-dir+
                                    :name "grep"
                                    :type "c")))
              (cons :directory
                    (directory-namestring
                     (make-pathname :directory +grep-prj-dir+)))
              (cons :command
                    (format nil "cc -c -o grep ~a"
                            (namestring
                             (make-pathname :directory +grep-prj-dir+
                                            :name "grep"
                                            :type "c")))))))
          (make-pathname :directory +grep-prj-dir+))
         *mutation-stats* (make-hash-table :test 'equal)))
  (:teardown (setf *project* nil
                   *mutation-stats* nil)))

(defmethod test-method ((obj simple) value)
  value)

(deftest to-file-fails-with-nil-path ()
  (with-fixture c-project
    (signals error (to-file *project* nil))))

(deftest (simple-to-from-file-without-project-dir-works :long-running) ()
  (with-fixture project
    (setf (project-dir *project*) nil)
    (with-temporary-directory (:pathname dir)
      (progn
        (to-file *project* dir)
        (is (member :user-read (file-permissions dir)))
        (is (member :user-write (file-permissions dir)))
        (let ((s1-2 (from-file (make-instance 'simple)
                               (make-pathname :name "s1"
                                              :directory dir)))
              (s2-2 (from-file (make-instance 'simple)
                               (make-pathname :name "s2"
                                              :directory dir))))
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
            "./src/foo" :ignore-paths '("etc/*"))))
  (is (sel/sw/project::ignored-path-p
       #p"dist/x.min.map" :ignore-paths '("*.min.map")))
  (is (sel/sw/project::ignored-path-p
       #p"dist/x.min.js" :ignore-paths '("*.min.js"))))

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
   (with-temporary-file (:pathname dir-path))
   (let ((dir (pathname-directory (pathname-as-directory dir-path))))
     (is (project-dir *project*))
     (to-file *project* dir-path)
     (is (member :user-exec
                 (file-permissions
                  (make-pathname :name "test"
                                 :type "sh"
                                 :directory (append dir (list "support")))))))))

(deftest (project-copy-maintains-relative-paths :long-running) ()
  (with-fixture multiple-artifacts-project
    (with-temporary-file (:pathname dir-path)
      ;; Check if project still runs once it is built and copied over.
      ;; If it runs, symbolic links are followed and nested directories
      ;; are maintained.
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *project* :bin dir-path))))
          "multiple-artifacts did not build correctly")
      (let ((bin (merge-pathnames-as-file
                  (ensure-directory-pathname dir-path)
                  "./run_shared")))
        (is (zerop (nth-value 2 (shell "cd ~a && ~a"
                                       (namestring dir-path)
                                       (namestring bin))))
            "copied multiple-artifacts failed to run")))))

(deftest (c-project-test :long-running) ()
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
               (namestring (project-dir *project*))))))

(deftest c-project-compilation-database-flags-test ()
  (is (equal (list "-DDIR='\"/tmp\"'" "-DIN" "\"-D_U_=a\"")
             (sel/sw/c-project::compilation-db-entry-flags
              `((:command .
                          "cc -DDIR=\\\"/tmp\\\" -DIN \"-D_U_=a\"")))))
  (is (equal (list "-DDIR1='\"/tmp1\"'" "-DDIR2='\"/tmp2\"'")
             (sel/sw/c-project::compilation-db-entry-flags
              `((:command .
                          "cc -DDIR1=\\\"/tmp1\\\" -DDIR2=\\\"/tmp2\\\"")))))
  (is (equal (list "-DDIR='\"\"'")
             (sel/sw/c-project::compilation-db-entry-flags
              `((:command .
                          "cc -DDIR=\\\"\\\""))))))
