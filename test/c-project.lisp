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
  (:import-from :cmd)
  (:import-from
    :software-evolution-library/components/compilation-database
    :compilation-database
    :macro-name
    :macro-def)
  (:local-nicknames
   (:c/cpp-project :software-evolution-library/software/c-cpp-project))
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
         (from-file-as-temporary-project
          (make-instance 'c-project
            :build-command "make"
            :artifacts (list "run_shared"
                             "shared.so.link"
                             "nested-dir/helper"))
          (make-pathname :directory +multiple-artifact-dir+))))
  (:teardown
   (delete-temporary-project *project*)
   (setf *project* nil)))

(defixture grep-project
  (:setup
   ;; Ensure the directory is clean
   (cmd:cmd! "git clean -xdf"
             (make-pathname :directory +grep-prj-dir+)
             :in
             (make-pathname :directory +grep-prj-dir+))
   (setf *project*
         (from-file-as-temporary-project
          (make-instance 'c-project
            :build-command "make grep"
            :artifacts '("grep")
            :compiler "gcc"
            :flags '("-ansi" "-v"))
          (make-pathname :directory +grep-prj-dir+))
         *mutation-stats* (make-hash-table :test 'equal)))
  (:teardown
   (delete-temporary-project *project*)
   (setf *project* nil
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

(defixture duplicate-compile-commands-project
  (:setup
   (setf *project*
         (from-file
          (make-instance 'c-project)
          (make-pathname
           :directory
           (append1 +etc-dir+ "duplicate-compile-commands")))))
  (:teardown (setf *project* nil)))

(defixture symbol-table-project
  (:setup
   (setf *project*
         (from-file 'c-project
                    (make-pathname
                     :directory (append1 +etc-dir+
                                         "c-symbol-table-project")))))
  (:teardown (setf *project* nil)))

(defixture symbol-table-project2
    (:setup
     (setf *project*
           (from-file 'c-project
                      (make-pathname
                       :directory (append1 +etc-dir+
                                           "c-symbol-table-project2")))))
  (:teardown (setf *project* nil)))

(defixture extra-files
  (:setup
   (setf *project*
         (from-file 'c-project
                    (make-pathname
                     :directory (append1 +etc-dir+
                                         "c-extra-files")))))
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

(deftest (project-copy-preserves-permissions :long-running) ()
  ;; Ensure `to-file' preserves permissions on executable files.
  (nest
   (with-fixture grep-project)
   (with-temporary-file (:pathname dir-path))
   (let ((dir (pathname-directory (uiop:ensure-directory-pathname dir-path))))
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
    (is (null (compilation-database *project*)))
    (is (equal 14 (length (evolve-files *project*))))
    (is (member "grep.c" (evolve-files *project*) :test 'equal :key 'car))
    (is (equal "gcc"
               (compiler (cdar (member "grep.c" (evolve-files *project*)
                                       :test 'equal :key 'car)))))
    (is (equal '("-ansi" "-v")
               (flags (cdar (member "grep.c" (evolve-files *project*)
                                       :test 'equal :key 'car)))))
    ;; Don't include binaries in `other-files'.
    (is (not (member "support/inputs/grepBinary"
                     (mapcar #'car (other-files *project*)) :test #'equalp)))
    (is (equal (compiler *project*) "gcc"))
    (is (equal (flags *project*) '("-ansi" "-v")))))

(deftest (c-project-can-build :long-running) ()
  (with-fixture grep-project
    (is (phenome *project*))))

(deftest (c-project-can-lookup :long-running) ()
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
           (list f1h)))
      (with-attr-table *project*
        (is (equal (project-dependency-tree *project*)
                   '(("lib/f1.c"
                      ("lib/f1.h"))
                     ("f1.c"
                      ("lib/f2.h")
                      ("f1.h")))))))))

(deftest duplicate-compile-commands ()
  (with-fixture duplicate-compile-commands-project
    (with-attr-table *project*
      (finishes (symbol-table *project*)))))


;;; System Headers
(deftest c-project-system-headers-1 ()
  "System headers are populated at project creation."
  (with-fixture symbol-table-project
    (with-slots (system-headers) (genome *project*)
      (get-standard-path-header *project* "stdio.h")
      (is (find-if (op (equal (header-name _) "stdio.h"))
                   system-headers)))))

(deftest c-project-system-headers-2 ()
  "System headers that can't be found create an entry without children."
  (with-fixture symbol-table-project2
    (with-attr-table *project*
      (is (occurs-if (op (and (symbolp _1)
                              (not (keywordp _1))
                              (string^= "doesnt-exist" _1)))
                     (project-dependency-tree *project*))))))

(defun get-sysroot-project ()
  "Load and return the c-sysroot test project."
  (from-file 'c-project (base-path-join +etc-dir-path+ "c-sysroot")))

(defun sysroot-project-resolved-p (project)
  "Check that the symbol table in PROJECT, a variation of the c-sysroot
project, is correct."
  (with-attr-table project
    (and-let* ((hello.c (evolve-files-ref project "hello.c"))
               (symtab
                (symbol-table (genome hello.c)))
               (vars (@ symtab :variable)))
      (@ vars "SOME_HEADER_VAR"))))

(deftest c-project-no-sysroot ()
  "System headers that rely on a sysroot to resolve shouldn't be resolved without it."
  (let (c/cpp-project::*system-header-cache*)
    (with-temporary-directory-of-project
        ((get-sysroot-project) :pathname d)
      (delete-file (path-join d "compile_commands.json"))
      (let ((project (from-file 'c-project d)))
        (is (not (sysroot-project-resolved-p project)))))))

(deftest c-project-sysroot-from-command-object ()
  "System headers should be resolved relative to the command object sysroot."
  (let (c/cpp-project::*system-header-cache*)
    (with-temporary-directory-of-project ((get-sysroot-project) :pathname d)
      (let ((db (path-join d "compile_commands.json")))
        (is (file-exists-p db))
        ;; Rewrite the sysroot argument to point into the test
        ;; directory on this machine.
        (write-string-into-file
         (ppcre:regex-replace-all
          "/nowhere"
          (read-file-into-string db)
          (namestring +etc-dir-path+))
         db
         :if-exists :supersede))
      (is (sysroot-project-resolved-p (from-file 'c-project d))))))

(deftest c-project-sysroot-from-project/bad ()
  "Expected failure if the project sysroot is invalid."
  (mvlet*
      ((c/cpp-project::*system-header-cache*)
       (project (get-sysroot-project))
       (bad-project
        (with-temporary-directory-of-project (project :pathname d)
          ;; Delete the compilation database so only the project
          ;; sysroot is used.
          (delete-file (path-join d "compile_commands.json"))
          (from-file
           (make 'c-project
                 :isysroot
                 "/doesnt/exist/")
           d))))
    (is (not (compilation-database bad-project)))
    (is (not (sysroot-project-resolved-p bad-project)))))

(deftest c-project-sysroot-from-project/good ()
  "System headers should be resolved relative to the project sysroot."
  (mvlet*
      ((c/cpp-project::*system-header-cache*)
       (project (get-sysroot-project))
       (good-project
        (with-temporary-directory-of-project (project :pathname d)
          ;; Delete the compilation database so only the project
          ;; sysroot is used.
          (delete-file (path-join d "compile_commands.json"))
          (from-file
           (make 'c-project
                 :isysroot
                 (namestring
                  (base-path-join
                   +etc-dir-path+
                   "c-sysroot-sysroot/")))
           d))))
    (is (not (compilation-database good-project)))
    (is (sysroot-project-resolved-p good-project))))

(deftest c-project-sysroot-from-db-and-project/bad ()
  "Expected failure when project isysroot is invalid, even when the commands
also have a sysroot."
  (mvlet*
      ((c/cpp-project::*system-header-cache*)
       (project (get-sysroot-project))
       (bad-project
        (with-temporary-directory-of-project (project :pathname d)
          (let ((db (path-join d "compile_commands.json")))
            (is (file-exists-p db))
            ;; Remove the nowhere/ prefix.
            (write-string-into-file
             (string-replace-all
              "/nowhere"
              (read-file-into-string db)
              "")
             db
             :If-exists :supersede))
          (values
           (from-file
            (make 'c-project :isysroot "/doesnt/exist")
            d)))))
    (is (compilation-database bad-project))
    (is (not (sysroot-project-resolved-p bad-project)))))

(deftest c-project-sysroot-from-db-and-project/good ()
  "Test that isysroot from a project is prefixed to --isysroot in the DB
arguments."
  (mvlet*
      ((c/cpp-project::*system-header-cache*)
       (project (get-sysroot-project))
       (good-project
        (with-temporary-directory-of-project (project :pathname d)
          (let ((db (path-join d "compile_commands.json")))
            (is (file-exists-p db))
            ;; Remove the nowhere/ prefix.
            (write-string-into-file
             (string-replace-all
              "/nowhere"
              (read-file-into-string db)
              "")
             db
             :If-exists :supersede))
          (from-file
           (make 'c-project :isysroot (namestring +etc-dir-path+))
           d))))
    (is (compilation-database good-project))
    (is (sysroot-project-resolved-p good-project))))


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
               (is (equal? (less (symbol-table target-ast) :macro)
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

(deftest c-project-dependency-order-1 ()
  "Header files that aren't included by any of the project files are still
present in evolve-files/dependency-order."
  (with-fixture multiple-artifacts-project
    (with-attr-session (*project*)
      (let* ((evolve-files (evolve-files *project*))
             (evolve-files/dependency-order
               (evolve-files/dependency-order *project*
                                              :skip-unparsed nil)))
        (is (length= evolve-files evolve-files/dependency-order))
        ;; Same files
        (is (not (set-exclusive-or
                  (mapcar #'car evolve-files)
                  (mapcar #'car evolve-files/dependency-order)
                  :test #'equal)))))))

(deftest c-include-asts-have-symbol-tables ()
  "ASTs inside include statements should have a symbol table."
  (with-fixture include-processing
    (let ((include-ast (is (find-if (of-type 'c-preproc-include) *project*))))
      (with-attr-table *project*
        (symbol-table *project*)
        (mapc (lambda (ast)
                (is (has-attribute-p ast 'symbol-table)))
              include-ast)))))

(deftest c-project-initially-excludes-headers ()
  "Projects should initially only parse files in the compilation database."
  (with-fixture extra-files
    (is (equal (mapcar #'car (evolve-files *project*))
               '("hello_world.c")))))

(deftest test-header-symbol-table-contamination-regression ()
  "Headers should not cause cross-file symbol table contamination."
  (let* ((proj
           (from-file 'c-project
                      (path-join (make-pathname :directory +etc-dir+)
                                 "c-symbol-table-contamination/")))
         (file1
           (is (evolve-files-ref proj "file1.c")))
         (x1 (is (lastcar
                  (collect-if
                   (conjoin
                    (of-type 'identifier-ast)
                    (op (source-text= "x" _)))
                   file1))))
         (file2
           (is (evolve-files-ref proj "file2.c")))
         (x2
           (is (lastcar
                (collect-if
                 (conjoin
                  (of-type 'identifier-ast)
                  (op (source-text= "x" _)))
                 file2))))
         (header
           (is (evolve-files-ref proj "common_header.h")))
         (header-x
           (is (lastcar
                (collect-if
                 (conjoin
                  (of-type 'identifier-ast)
                  (op (source-text= "x" _)))
                 header)))))
    (with-attr-table proj
      (is (reachable? (genome header)))
      (is (reachable? header-x))
      (is (null (get-declaration-ast :type header-x)))
      (let ((x1-decl (is (get-declaration-ast :variable x1)))
            (x2-decl (is (get-declaration-ast :variable x2))))
        (is (typep x1-decl 'c-declaration))
        (is (typep x2-decl 'c-declaration))
        (is (not (eq x1-decl x2-decl)))
        (is (source-text= (c-value (car (c-declarator x1-decl))) "1"))
        (is (source-text= (c-value (car (c-declarator x2-decl))) "2"))))))

(deftest c-project-implicit-headers ()
  "Test implicit headers are incorporated into file symbol tables."
  (with-fixture extra-files
    (let* ((file (cdar (evolve-files *project*))))
      (is (get-implicit-header *project* file))
      (with-attr-table *project*
        (let ((symtab (symbol-table (genome file))))
          (is symtab)
          (is (lookup (lookup symtab :macro) "NULL")))))))

(deftest test-header-implicit-header ()
  "Headers should inherit the \"implicit\" headers of (some of) their
includers."
  (let* ((path (base-path-join
                (make-pathname :directory +etc-dir+)
                "c-symbol-table-project-with-db/"))
         (project (from-file 'c-project path)))
    (with-attr-table project
      (symbol-table project)
      (let ((file.h (is (evolve-files-ref project "file.h"))))
        (is (@ (@ (symbol-table (first (children (genome file.h)))) :macro)
               "MYMACRO"))
        (dolist (sh (is (system-headers (genome project))))
          (is (@ (@ (symbol-table (first (children sh))) :macro)
                 "MYMACRO")))))))

(deftest test-parse-predefined-macros ()
  "Test we parse all predefined macros."
  (iter (for compiler in '(:gcc :g++ :clang :clang++))
        (mvlet* ((file
                  (c/cpp-project::default-predefined-macros-file
                   compiler))
                 (string (read-file-into-string file))
                 (defs
                  (c/cpp-project::default-predefined-macros compiler)))
          (is defs)
          (is (length= defs (remove-if #'emptyp (lines string))))
          (is (every (of-type
                      '(cons macro-name macro-def))
                     defs)))))

(deftest test-dump-compiler-macros ()
  "Test we actually get compiler-specific macros."
  (when (resolve-executable "gcc")
    (is (not (assoc "__clang__"
                    (c/cpp-project::dump-predefined-macros "gcc")
                    :test #'equal))))
  (when (resolve-executable "clang")
    (is (assoc "__clang__"
               (c/cpp-project::dump-predefined-macros "clang")
               :test #'equal))))

(deftest test-dump-predefined-macros ()
  "Test we can dump predefined macros."
  (let ((compilers '(:gcc :g++ :clang :clang++))
        (dumps '()))
    (dolist (compiler compilers)
      (when (resolve-executable (string-downcase compiler))
        (let ((defs (c/cpp-project::dump-predefined-macros compiler)))
          (is defs)
          (is (every (of-type
                      '(cons macro-name macro-def))
                     defs))
          (push defs dumps))))
    (is (length=
         dumps
         (remove-duplicates
          dumps
          :test
          (op (set-equal _ _ :test #'equal)))))))
