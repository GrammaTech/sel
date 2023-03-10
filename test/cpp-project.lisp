;;;; cpp-project.lisp --- Project tests.
(defpackage :software-evolution-library/test/cpp-project
  (:nicknames :sel/test/cpp-project)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :stefil+
   :functional-trees/attrs
   :software-evolution-library
   :software-evolution-library/software/simple
   :software-evolution-library/software/compilable
   :software-evolution-library/software/tree-sitter
   :software-evolution-library/software/parseable
   :software-evolution-library/software/project
   :software-evolution-library/software/c-cpp-project
   :software-evolution-library/software/cpp-project)
  #-windows (:shadowing-import-from :osicat
                                    :file-permissions :pathname-as-directory)
  (:export :test-cpp-project))
(in-package :software-evolution-library/test/cpp-project)
(in-readtable :curry-compose-reader-macros)

(defun cpp-available-p ()
  "Returns true if the c tree-sitter parser is loaded."
  (let* ((p (find-package :sel/sw/tree-sitter))
         (s (and p (find-symbol "CPP" p))))
    (and s (find-class s))))

(defsuite test-cpp-project
    "CPP project representation."
  (cpp-available-p))

(defvar *s1*)
(defvar *s2*)
(defixture cpp-project
  (:setup
   (setf *s1* (make-instance 'simple))
   (setf (lines *s1*) (list "s1-genome"))
   (setf *s2* (make-instance 'simple))
   (setf (lines *s2*) (list "s2-genome"))
   (setf *project* (make-instance 'cpp-project
                     :evolve-files `(("s1" . ,*s1*)
                                     ("s2" . ,*s2*)))))
  (:teardown (setf *project* nil)))

(defixture multiple-artifacts-project
  (:setup
   (setf *project*
         (from-file
          (make-instance 'cpp-project
            :build-command "make"
            :artifacts (list "run_shared"
                             "shared.so.link"
                             "nested-dir/helper"))
          (make-pathname :directory +multiple-artifact-dir+))))
  (:teardown (setf *project* nil)))

(defixture cpp-sample-project
  (:setup
   (setf *project*
         (from-file
          (make-instance 'cpp-project
            :build-command "make"
            :artifacts '("sample")
            :compiler "g++"
            :flags "-v")
          (make-pathname :directory +cpp-sample-dir+))
         *mutation-stats* (make-hash-table :test 'equal)))
  (:teardown (setf *project* nil
                   *mutation-stats* nil)))

(defixture cpp-symbol-table-project
  (:setup
   (setf *project*
         (from-file 'cpp-project
                    (make-pathname
                     :directory (append1 +etc-dir+
                                         "cpp-symbol-table-project")))))
  (:teardown (setf *project* nil)))

(defixture cpp-relative-include-symbol-table-project
    (:setup
     (setf *project*
           (from-file 'cpp-project
                      (make-pathname
                       :directory (append1 +etc-dir+
                                           "cpp-symbol-table-project2")))))
  (:teardown (setf *project* nil)))

(defixture cpp-symbol-table-project3
    (:setup
     (setf *project*
           (from-file 'cpp-project
                      (make-pathname
                       :directory (append1 +etc-dir+
                                           "cpp-symbol-table-project3")))))
  (:teardown (setf *project* nil)))

(defmethod test-method ((obj simple) value)
  value)

(deftest to-file-fails-with-nil-path ()
  (with-fixture cpp-project
    (signals error (to-file *project* nil))))

(deftest simple-to-from-file-without-project-dir-works ()
  (with-fixture cpp-project
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

(deftest cpp-project-test ()
  (with-fixture cpp-sample-project
    (is (equal "make" (build-command *project*)))
    (is (equalp '("sample") (artifacts *project*)))
    (is (equal 3 (length (evolve-files *project*))))
    (is (member "src/sample.cc" (evolve-files *project*)
                :test 'equal :key 'car))
    (is (equal "g++"
               (compiler (cdar (member "src/sample.cc" (evolve-files *project*)
                                       :test 'equal :key 'car)))))
    (is (equal "-v"
               (flags (cdar (member "src/sample.cc" (evolve-files *project*)
                                       :test 'equal :key 'car)))))
    (is (equal (namestring (make-pathname :directory +cpp-sample-dir+))
               (namestring (project-dir *project*))))
    (is (equal (compiler *project*) "g++"))
    (is (equal (flags *project*) "-v"))))

(deftest cpp-project-can-build ()
  (with-fixture cpp-sample-project
    (is (phenome *project*))))

(deftest test-cpp-function-declaration-enclosing ()
  (let ((cpp (from-string 'cpp-project (fmt "~
boost::property_tree::ptree
make_config(const std::string& path_prefix,
            const std::unordered_map<std::string, std::string>& overrides = {},
            const std::unordered_set<std::string>& removes = {});~%"))))
    (with-attr-table cpp
      (find-enclosing-declaration 'function-declaration-ast
                                  cpp
                                  (stmt-with-text cpp "make_config")))))


;;; Symbol Table
(deftest cpp-project-symbol-table-1 ()
  "Every included system file has its namespace qualified symbols
imported into the symbol table of the file including it."
  (labels ((lookup-variable (symbol-table name)
             (lookup (lookup symbol-table :variable) name))
           (test-main.cc ()
             "Test that a symbol from stdio.h is in the symbol table in main.c."
             (let* ((software (aget "main.cc" (evolve-files *project*)
                                    :test #'equal))
                    (target-ast (find-if (of-type 'cpp-preproc-include)
                                         (genome software)))
                    (return-ast (find-if (of-type 'cpp-return-statement)
                                         (genome software)))
                    (system-header
                      (find-if (op (equal (header-name _) "iostream"))
                               (system-headers (genome *project*))))
                    (target-symbol-table (symbol-table target-ast))
                    (return-symbol-table (symbol-table return-ast)))
               (is (lookup-variable target-symbol-table "std::cout"))
               (is (lookup-variable return-symbol-table "std::cout"))
               (is (find-if (op (eq (car (lookup-variable target-symbol-table
                                                          "std::cout"))
                                    _))
                            system-header)))))
    (with-fixture cpp-symbol-table-project
      (with-attr-table *project*
        (test-main.cc)
        *project*))))

(deftest cpp-project-symbol-table-2 ()
  "Locally included header files have their symbols imported into the
symbol table of the file including it."
  (labels ((lookup-type (symbol-table name)
             (lookup (lookup symbol-table :type) name)))
    (with-fixture cpp-relative-include-symbol-table-project
      (with-attr-table *project*
        (let* ((software (aget "my_program.cc" (evolve-files *project*)
                               :test #'equal))
               (target-ast (find-if (of-type 'cpp-preproc-include)
                                    (genome software)))
               (return-ast (find-if (of-type 'cpp-return-statement)
                                    (genome software)))
               (target-symbol-table (symbol-table target-ast))
               (return-symbol-table (symbol-table return-ast)))
          (is (lookup-type target-symbol-table "N::my_class"))
          (is (lookup-type return-symbol-table "N::my_class")))))))

(deftest cpp-project-include-tree-2 ()
  "Locally included header files have their symbols imported into the
symbol table of the file including it."
  (with-fixture cpp-relative-include-symbol-table-project
    (with-attr-table *project*
      (is (equal (project-include-tree *project*)
                 '(("my_class.cc" (:|iostream|) ("my_class.h"))
                   ("my_program.cc" ("my_class.h")))))
      (is (set-equal '("my_class.cc" "my_program.cc")
                     (who-includes? *project* "my_class.h")
                     :test #'equal))
      (is (equal '(("my_class.h"))
                 (file-include-tree *project* "my_program.cc"))))))

(deftest cpp-project-symbol-table-3 ()
  "Check that we handle the case where a header is added that was not
present when the project was created."
  (with-fixture cpp-symbol-table-project3
    (let* ((main (aget "main.cc" (evolve-files *project*)
                       :test #'equal))
           (genome (genome main))
           (include (is (find-if (of-type 'cpp-system-lib-string) genome)))
           (new-genome
            (with genome include (copy include :text "<vector>")))
           (new-genome
            (mapcar (hash-table-function
                     (maphash-into
                      (make-hash-table)
                      (lambda (ast)
                        (if (source-text= ast "list")
                            (values ast (copy ast :text "vector"))
                            (values ast ast)))
                      (convert 'list genome)))
                    new-genome))
           (new-project
            (with *project* genome new-genome)))
      (with-attr-table new-project
        (get-declaration-ids
         :function
         (find-if (of-type 'cpp-field-expression)
                  new-genome))))))

(deftest test-cpp-project-include-resolution/compdb ()
  "Test simple include resolution for program and system includes.
For each permutation, we load the project with a specific compilation
directory pointing to one of two include directories defining the same
binding with different contents and check that the symbol table gets
the correct binding."
  (let* ((sel-dir (asdf:system-relative-pathname :software-evolution-library nil))
         (project-dir (path-join sel-dir #p"test/etc/cpp-include-project/"))
         (db-templates (sort (directory (path-join project-dir #p"*.json"))
                             #'string< :key #'pathname-name))
         (source-files '("program_include.cc" "system_include.cc")))
    (is (length= db-templates 2))
    (macrolet ((with-temp-compdb ((db temp &key) &body body)
                 (with-thunk (body temp)
                   `(call/temp-compdb ,db ,body))))
      (labels ((copy-file/subst (from to)
                 "Copy FROM to TO, replacing ${SEL} with the SEL path."
                 (let ((prefix (drop-suffix "/" (namestring sel-dir))))
                   (write-string-into-file
                    (string-replace-all
                     "${SEL}"
                     (read-file-into-string from)
                     prefix)
                    to
                    :if-exists :error)))
               (call/temp-compdb (db fn)
                 (with-temporary-file (:pathname p)
                   (copy-file/subst db p)
                   (is (file-exists-p p))
                   (funcall fn p)))
               (do-test (n db file)
                 "Test a single permutation.
                Temporarily copy DB as compile_commands.json before loading the project."
                 (with-temp-compdb (db temp)
                   (let* ((project
                           (is (from-file
                                (make 'cpp-project :compilation-database-path temp)
                                project-dir)))
                          (software
                           (is (assocdr file (evolve-files project) :test #'equal)))
                          (ast
                           (is (find-if (op (source-text= "MYCONST" _))
                                        (genome software)))))
                     (is (sel/cp/compdb:command-header-dirs
                          (only-elt
                           (is (sel/cp/compdb:command-object project file)))))
                     (with-attr-table project
                       (let* ((decl (is (get-declaration-ast :variable ast)))
                              (assignment
                               (is (find-if (of-type 'variable-initialization-ast)
                                            decl))))
                         (is (source-text= (rhs assignment)
                                           (princ-to-string n)))
                         project))))))
        (iter (for i from 1)
              (for db in db-templates)
              (for file in source-files)
              (do-test i db file))))))

(defun call/temp-compdb (db-template fn)
  "Call FN with a temporary database realized from DB-TEMPLATE."
  (let ((sel-dir (asdf:system-relative-pathname :software-evolution-library nil)))
    (labels ((copy-file/subst (from to)
               "Copy FROM to TO, replacing ${SEL} with the SEL path."
               (let ((prefix (drop-suffix "/" (namestring sel-dir))))
                 (write-string-into-file
                  (string-replace-all
                   "${SEL}"
                   (read-file-into-string from)
                   prefix)
                  to
                  :if-exists :error))))
      (with-temporary-file (:pathname p)
        (copy-file/subst db-template p)
        (is (file-exists-p p))
        (funcall fn p)))))

(defmacro with-temp-compdb ((db-template temp &key) &body body)
  "Run BODY with a temporary database realized from DB-TEMPLATE."
  (with-thunk (body temp)
    `(call/temp-compdb ,db-template ,body)))

(deftest test-cpp-project-preproc-resolution/compdb ()
  (let* ((sel-dir (asdf:system-relative-pathname :software-evolution-library nil))
         (project-dir (path-join sel-dir #p"test/etc/cpp-preproc-project/"))
         (db-template (path-join project-dir #p"compile_commands_template.json")))
    (with-temp-compdb (db-template temp)
      (let* ((project
              (is (from-file
                   (make 'cpp-project :compilation-database-path temp)
                   project-dir)))
             (software
              (is (assocdr "main.cc" (evolve-files project) :test #'equal)))
             (const-1
              (is (find-if (op (source-text= "MYCONST_1" _))
                           (genome software))))
             (const-2
              (is (find-if (op (source-text= "MYCONST_2" _))
                           (genome software))))
             (plus
              (is (find-if (op (source-text= "MYPLUS" _))
                           (genome software)))))
        (with-attr-table project
          (list (get-declaration-ast :macro const-1)
                (get-declaration-ast :macro const-2)
                (get-declaration-ast :macro plus)))))))

(deftest test-cpp-project-recursive-include-resolution/compdb ()
  "Test that nested includes inherit the header search path of the compilation unit."
  (let* ((sel-dir (asdf:system-relative-pathname :software-evolution-library nil))
         (project-dir (path-join sel-dir #p"test/etc/cpp-recursive-include-project/"))
         (db-template (path-join project-dir #p"compile_commands_template.json")))
    (with-temp-compdb (db-template temp)
      (let* ((project
              (is (from-file
                   (make 'cpp-project :compilation-database-path temp)
                   project-dir)))
             (main
              (is (assocdr "main.cc" (evolve-files project) :test #'equal)))
             (const-2
              (is (find-if (op (source-text= "MYCONST_2" _))
                           (genome main)))))
        (with-attr-table project
          (let* ((const-2-decl
                  (is (get-declaration-ast :variable const-2)))
                 (const-1
                  (find-if (op (source-text= "MYCONST_1" _))
                           const-2-decl)))
            (is (get-declaration-ast :variable const-1))))))))

(deftest test-cpp-project-circular-inclusion ()
  "Test that circular inclusion works."
  (let* ((sel-dir (asdf:system-relative-pathname :software-evolution-library nil))
         (project-dir (path-join sel-dir #p"test/etc/cpp-circular-include-project/"))
         (project (is (from-file (make 'cpp-project) project-dir)))
         (main (is (assocdr "main.cc" (evolve-files project) :test #'equal)))
         (const-1
          (is (find-if (op (source-text= "MYCONST_1" _))
                       (genome main)))))
    (with-attr-table project
      (is (get-declaration-ast :variable const-1)))))
