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
   :software-evolution-library/software/c-cpp-project
   :software-evolution-library/software/c-project
   :functional-trees/attrs)
  #-windows (:shadowing-import-from :osicat
                                    :file-permissions :pathname-as-directory)
  (:export :test-c-project))
(in-package :software-evolution-library/test/c-project)
(in-readtable :curry-compose-reader-macros)

(defun c-available-p ()
  "Returns true if the c tree-sitter parser is loaded."
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
   (setf *project* (make-instance 'c-project
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
            :compiler "gcc"
            :flags "-v")
          (make-pathname :directory +grep-prj-dir+))
         *mutation-stats* (make-hash-table :test 'equal)))
  (:teardown (setf *project* nil
                   *mutation-stats* nil)))

(defixture include-processing
  (:setup
   (setf *project*
         (from-file
          (make-instance 'c-project)
          (make-pathname :directory +include-processing-dir+))
         (include-paths *project*)
         (directories-of-header-files *project*)))
  (:teardown (setf *project* nil)))

(defixture symbol-table-project
  (:setup
   (setf *project*
         (from-file 'c-project
                    (make-pathname
                     :directory (append1 +etc-dir+
                                         "c-symbol-table-project")))))
  (:teardown (setf *project* nil)))

(defmethod test-method ((obj simple) value)
  value)

(deftest to-file-fails-with-nil-path ()
  (with-fixture c-project
    (signals error (to-file *project* nil))))

(deftest (simple-to-from-file-without-project-dir-works :long-running) ()
  (with-fixture c-project
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

#-ccl ; TODO: fails with ccl
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

#-ccl ; TODO: fails with ccl
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

(deftest project-copy-preserves-permissions ()
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

(deftest project-copy-maintains-relative-paths ()
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

(deftest c-project-test ()
  (with-fixture grep-project
    (is (equal "make grep" (build-command *project*)))
    (is (equalp '("grep") (artifacts *project*)))
    (is (equal 14 (length (evolve-files *project*))))
    (is (member "grep.c" (evolve-files *project*) :test 'equal :key 'car))
    (is (equal "gcc"
               (compiler (cdar (member "grep.c" (evolve-files *project*)
                                       :test 'equal :key 'car)))))
    (is (equal "-v"
               (flags (cdar (member "grep.c" (evolve-files *project*)
                                       :test 'equal :key 'car)))))
    ;; Don't include binaries in `other-files'.
    (is (not (member "support/inputs/grepBinary"
                     (mapcar #'car (other-files *project*)) :test #'equalp)))
    (is (equal (namestring (make-pathname :directory +grep-prj-dir+))
               (namestring (project-dir *project*))))
    (is (equal (compiler *project*) "gcc"))
    (is (equal (flags *project*) "-v"))))

(deftest c-project-can-build ()
  (with-fixture grep-project
    (is (phenome *project*))))

(deftest c-project-can-lookup ()
  (with-fixture grep-project
    (is (not (null (@ *project* "grep.c"))))))

(deftest c-include-processing ()
  (with-fixture include-processing
    (let* ((all-files (all-files *project*))
           (f1h (cdr (assoc "f1.h" all-files :test #'equal)))
           (f1c (cdr (assoc "f1.c" all-files :test #'equal)))
           (lib/f1c (cdr (assoc "lib/f1.c" all-files :test #'equal)))
           (lib/f1h (cdr (assoc "lib/f1.h" all-files :test #'equal))))
      (is (equal (find-include-files *project* nil "f1.h")
                 (list f1h lib/f1h)))
      (is (equal (find-include-files *project* f1c "f1.h")
                 (list f1h)))
      (is (equal (find-include-files *project* lib/f1c "f1.h")
                 (list lib/f1h)))
      (is (equal
           (let ((include-ast (second (convert 'list f1c))))
             (find-include-files *project* f1c include-ast))
           (list f1h))))))


;;; System Headers
(deftest c-project-system-headers-1 ()
  "System headers are populated at project creation."
  (with-fixture symbol-table-project
    (with-slots (system-headers) (genome *project*)
      (get-std-header *project* "stdio.h")
      (is (find-if (op (equal (header-name _) "stdio.h"))
                   system-headers)))))

(deftest c-project-system-headers-2 ()
  "System headers that can't be found create an entry without children."
  (with-fixture symbol-table-project
    (let ((system-header (get-std-header *project* "doesn't-exist")))
      (is (null (children system-header)))
      (is (eq system-header
              (find-if (op (equal (header-name _) "doesn't-exist"))
                       (system-headers (genome *project*))))))))


;;; Symbol Table

(defun get-symbol-map (software symbol-alist)
  (labels ((symbol-list (symbol)
             (list symbol (stmt-with-text (genome software) symbol))))
    (convert 'fset:map
             (mapcar
              (lambda (pair)
                (list* (car pair)
                       (convert 'fset:map
                                (mapcar #'symbol-list (cdr pair)))))
              symbol-alist))))

(deftest c-project-symbol-table-1 ()
  "Included files have their symbols imported into the symbol table of the
file including it."
  (labels ((test-main.c ()
             "Test that the symbols from file.h are available in main.c."
             (let* ((software (aget "main.c" (evolve-files *project*)
                                    :test #'equal))
                    (target-ast (find-if (of-type 'c-preproc-include)
                                         (genome software)))
                    (included-software (aget "file.h" (evolve-files *project*)
                                             :test #'equal)))
               (is (equal? (symbol-table target-ast)
                           (get-symbol-map included-software
                                           '((:variable "x")
                                             (:function "function"))))))))
    (with-fixture symbol-table-project
      (with-attr-table *project*
        (symbol-table *project* (empty-map))
        (test-main.c)))))

(deftest c-project-symbol-table-2 ()
  "Included system files have their symbols imported into the symbol table of the
file including it."
  (labels ((find-function (symbol-table name)
             (lookup (lookup symbol-table :function)
                     name))
           (test-main.c ()
             "Test that a symbol from stdio.h is in the symbol table in main.c."
             (let* ((software (aget "main.c" (evolve-files *project*)
                                    :test #'equal))
                    (target-ast (second
                                 (collect-if (of-type 'c-preproc-include)
                                             (genome software))))
                    (system-header
                      (find-if (op (equal (header-name _) "stdio.h"))
                               (system-headers (genome *project*))))
                    (symbol-table (symbol-table target-ast)))
               (is (find-function symbol-table "printf"))
               (is (find-if (op (eq (car (find-function symbol-table "printf"))
                                    _))
                            system-header)))))
    (with-fixture symbol-table-project
      (with-attr-table *project*
        (symbol-table *project* (empty-map))
        (test-main.c)))))
