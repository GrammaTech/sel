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
  (:import-from :software-evolution-library/software/cpp-project
                :find-module
                :relative-module-defaults
                :restrict-map)
  (:local-nicknames
   (:dir :software-evolution-library/software/directory))
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

(defixture cpp-dependency-order
    (:setup
     (setf *project*
           (from-file 'cpp-project
                      (make-pathname
                       :directory (append1 +etc-dir+
                                           "cpp-dependency-order")))))
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



;;; Module resolution tests

#+tree-sitter-cpp
(deftest test-relative-module-defaults ()
  (is (equal "BasicPlane.Figures"
             (pathname-name
              (relative-module-defaults #p"files/BasicPlane.Figures.ixx"
                                        nil
                                        "BasicPlane.Figures")))))

#+tree-sitter-cpp
(deftest test-relative-partition-defaults ()
  (is (equal "BasicPlane.Figures-Point"
             (pathname-name
              (relative-module-defaults #p"files/BasicPlane.Figures-Rectangle.ixx"
                                        "BasicPlane.Figures:Rectangle"
                                        ":Point")))))

#+tree-sitter-cpp
(deftest test-relative-module-defaults/partition ()
  (is (equal "BasicPlane.Figures-Rectangle"
             (pathname-name
              (relative-module-defaults #p"files/BasicPlane.Figures.ixx"
                                        "BasicPlane.Figures"
                                        ":Rectangle")))))

#+tree-sitter-cpp
(deftest test-relative-module-defaults/implicit ()
  (is (equal "BasicPlane.Figures-Rectangle"
             (pathname-name
              (relative-module-defaults #p"files/BasicPlane.Figures-Rectangle.cpp"
                                        "BasicPlane.Figures:Rectangle"
                                        "BasicPlane.Figures:Rectangle")))))

#+tree-sitter-cpp
(deftest test-find-module ()
  (is (equal :figures
             (cdr
              (find-module (relative-module-defaults
                            #p"main.cpp" nil "BasicPlane.Figures")
                           '(("somewhere/BasicPlane.Figures.cppm" . :figures))
                           :key #'car)))))

#+tree-sitter-cpp
(deftest test-find-module-partition ()
  (is (equal :rectangle
             (cdr
              (find-module (relative-module-defaults
                            #p"BasicPlane\\.Figures.cpp"
                            "BasicPlane.Figures"
                            "BasicPlane.Figures-Rectangle")
                           '(("somewhere/BasicPlane.Figures.cppm" . :figures)
                             ("somewhere/BasicPlane.Figures-Rectangle.cppm" . :rectangle))
                           :key #'car)))))

(deftest test-restrict-map ()
  (is (equal?
       (restrict-map (fset:map (:x 1) (:y 2))
                     (fset:map (:x 2)))
       (fset:map (:x 1))))
  (is (equal?
       (restrict-map (fset:map (:x (fset:map (:y 1) (:z 2))))
                     (fset:map (:x (fset:map (:y 2)))))
       (fset:map (:x (fset:map (:y 1))))))
  (is (equal?
       (restrict-map (fset:map (:x (fset:map (:y 1) (:z 2))))
                     (fset:map (:x 1)))
       (fset:map (:x (fset:map (:y 1) (:z 2)))))))


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

(deftest cpp-project-dependency-tree-1 ()
  "Check computed include tree for a project."
  (with-fixture cpp-relative-include-symbol-table-project
    (with-attr-table *project*
      (is (equal (project-dependency-tree *project*)
                 '(("my_program.cc" ("my_class.h"))
                   ("my_class.cc"
                    (:|iostream|
                     (:|streambuf|)
                     (:|ostream|)
                     (:|istream|)
                     (:|ios| (:|iosfwd|)))
                    ("my_class.h")))))
      (is (set-equal '("my_class.cc" "my_program.cc")
                     (who-includes? *project* "my_class.h")
                     :test #'equal))
      (is (equal '(("my_class.h"))
                 (file-dependency-tree *project* "my_program.cc"))))))

(deftest cpp-project-dependency-tree-2 ()
  "Ensure we get the same results for local files from system and program headers.

I.e. `#include \"x\"` and `#include <x>` should get the same results
for non-system headers (assuming they're on the path)."
  (with-fixture cpp-relative-include-symbol-table-project
    (let ((project2
           (mapcar (lambda (ast)
                     (when (typep ast 'cpp-preproc-include)
                       (unless (typep (cpp-path ast) 'cpp-system-lib-string)
                         (tree-copy
                          (copy ast
                                :cpp-path
                                (make 'cpp-system-lib-string
                                      :text (source-text (cpp-path ast))))))))
                   *project*)))
      (is (equal (with-attr-table *project*
                   (project-dependency-tree *project*))
                 (with-attr-table project2
                   (project-dependency-tree project2)))))))

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
      (is (get-declaration-ast :variable const-1))
      (is (equal
           '(("main.cc"
              (:|stdio.h|)
              ("include.h"
               (:circle "include.h"))))
           (project-dependency-tree project))))))

(deftest cpp-test-std-id ()
  "Test that we can look up a symbol defined in a transitively included
standard header."
  (let* ((source (fmt "~
#include <iostream>

int main () {
    std::cout << \"Hello, world\" << std::endl;
}"))
         (cpp (from-string 'cpp-project source))
         (endl (find-if (op (source-text= "std::endl" _)) cpp)))
    (with-attr-table cpp
      (symbol-table endl)
      (get-declaration-ast :variable endl))))

(deftest test-incremental-symbol-table ()
  "Recomputation of the symbol table should be incremental."
  (is ft/attrs::*enable-cross-session-cache*)
  (labels ((file-root (file-ast)
             (only-elt (dir:contents file-ast)))
           (symbol-table-alist (project)
             (with-attr-table project
               (iter (for file in (collect-if (of-type 'dir:file-ast) project))
                     (collect (cons (namestring (dir:full-pathname file))
                                    (symbol-table (file-root file)))))))
           (compare-projects (old-project new-project &key unchanged changed)
             (let ((old-symbol-table-alist (symbol-table-alist old-project))
                   (new-symbol-table-alist (symbol-table-alist new-project)))
               (dolist (file unchanged)
                 (is (and (lookup old-project file)
                          (lookup new-project file))
                     "File must be in projects: ~a" file)
                 (is (eql (lookup old-project file)
                          (lookup new-project file))
                     "Unchanged files must actually be unchanged: ~a" file)
                 (let ((v1 (aget* file old-symbol-table-alist))
                       (v2 (aget* file new-symbol-table-alist)))
                   (is (eql v1 v2)
                       "Unchanged files must have the same symbol table: ~a" file)))
               (dolist (file changed)
                 (let ((v1 (aget* file old-symbol-table-alist))
                       (v2 (aget* file new-symbol-table-alist)))
                   (is (not (eql v1 v2))
                       "Changed files/dependents must have new symbol tables: ~a" file)))))
           (aget* (name alist)
             (let ((result (aget name alist :test #'equal)))
               (is result)
               (is (not (empty? result)))
               result)))
    (let* ((sel-dir (asdf:system-relative-pathname :software-evolution-library nil))
           (project-dir (path-join sel-dir #p"test/etc/cpp-symbol-table-project2"))
           (project (is (from-file (make 'cpp-project) project-dir)))
           (cc-file (lookup project "my_program.cc"))
           (cc-ast (find-if (of-type 'cpp-call-expression) cc-file))
           (hpp-file (lookup project "my_class.h"))
           (hpp-ast (find-if (of-type 'cpp-field-declaration) hpp-file)))
      ;; Compute before copying.
      (symbol-table-alist project)
      ;; Test changing the .cc file.
      (let* ((new-ast (cpp* "mc.do_something_else()"))
             (new-project
               (with project cc-ast new-ast))
             (new-file (lookup new-project "my_program.cc")))
        (is (not (eql new-project project))
            "The project must have changed")
        (is (not (eql (file-root cc-file)
                      (file-root new-file)))
            "The cc file must have changed")
        (is (eql (file-root hpp-file)
                 (file-root (lookup new-project (ast-path project hpp-file))))
            "The hpp file must not have changed")
        (compare-projects project new-project
                          :unchanged '("my_class.cc" "my_class.h")
                          :changed '("my_program.cc")))
      ;; Test changing the header file.
      (let* ((new-ast (tree-copy hpp-ast))
             (id (find-if (of-type 'cpp-field-identifier) new-ast))
             (new-ast
               (with new-ast
                     id
                     (copy id :text "do_something_else")))
             (new-project
               (with project hpp-ast new-ast))
             (new-file (lookup new-project "my_class.h")))
        (is (not (eql new-project project))
            "The project must have changed")
        (is (not (eql hpp-file
                      new-file))
            "The header file must have changed")
        ;; (is (ft/attrs::reachable? new-project new-file))
        ;; (is (not (ft/attrs::reachable? new-project hpp-file)))
        (compare-projects project new-project
                          :changed '("my_class.cc" "my_class.h" "my_program.cc"))))))

(deftest test-recursive-exports ()
  "Test that exports persist through layers of reexports."
  (let ((cpp (from-file 'cpp-project
                        (make-pathname
                         :directory (append +etc-dir+
                                            '("module-examples" "cpp-reexport"))))))
    (with-attr-table cpp
      (let* ((main (is (evolve-files-ref cpp "main.cc")))
             (symtab (symbol-table (genome main))))
        (is (@ (@ symtab :function) "main"))
        (is (@ (@ symtab :function) "b::say_hello"))
        (is (not (@ (@ symtab :function) "b::say_goodbye"))))
      (project-dependency-tree cpp))))

(deftest test-recursive-exports/dependencies ()
  (let ((cpp (from-file 'cpp-project
                        (make-pathname
                         :directory (append +etc-dir+
                                            '("module-examples" "cpp-reexport"))))))
    (with-attr-table cpp
      (is (equal (project-dependency-tree cpp :entry-points (list "main.cc"))
                 '(("main.cc"
                    ("a.cppm"
                     ("b.cppm"
                      (:|iostream| (:|streambuf|) (:|ostream|) (:|istream|)
                       (:|ios| (:|iosfwd|))))))))))))

(deftest test-ms-module-example-1 ()
  "Example of modularized code from Visual Studio docs."
  (let ((cpp (from-file 'cpp-project
                        (make-pathname
                         :directory (append +etc-dir+
                                            '("module-examples" "ms-basic-example"))))))
    (is (length= 2 (evolve-files cpp)))
    (with-attr-table cpp
      (let* ((file (is (evolve-files-ref cpp "MyProgram.cpp")))
             (symtab (symbol-table (stmt-with-text file "Example_NS::f()"))))
        (is (> (size (@ symtab :function)) 2))
        (is (typep (only-elt (@ (@ symtab :function) "Example_NS::f")) 'cpp-ast))
        symtab))))

(deftest test-ms-module-example-1/dependencies ()
  (let ((cpp (from-file 'cpp-project
                        (make-pathname
                         :directory (append +etc-dir+
                                            '("module-examples"
                                              "ms-basic-example"))))))
    (with-attr-table cpp
      (is (equal (project-dependency-tree cpp :entry-points '("MyProgram.cpp"))
                 '(("MyProgram.cpp" ("Example.ixx")
                    (:|iostream| (:|streambuf|) (:|ostream|) (:|istream|)
                     (:|ios| (:|iosfwd|))))))))))

(deftest test-ms-module-example-2 ()
  "Basic plane example from Visual Studio docs."
  (let* ((cpp (from-file 'cpp-project
                         (make-pathname
                          :directory (append +etc-dir+
                                             '("module-examples"
                                               "ms-basic-plane-example"))))))
    (with-attr-table cpp
      (let ((main (is (evolve-files-ref cpp "main.cpp"))))
        (is (typep (get-declaration-ast :type (stmt-with-text (genome main) "Rectangle"))
                   'cpp-struct-specifier)))
      (let* ((impl-file (is (evolve-files-ref cpp "BasicPlane.Figures-Rectangle.cpp")))
             (type-ast (find-if (op (and (source-text= _1 "Rectangle")
                                         (typep _1 'cpp-type-identifier)))
                                impl-file)))
        (is (typep (get-declaration-ast :type type-ast) 'cpp-struct-specifier))))))

(deftest test-ms-module-example-2/dependencies ()
  (let* ((cpp (from-file 'cpp-project
                         (make-pathname
                          :directory (append +etc-dir+
                                             '("module-examples"
                                               "ms-basic-plane-example"))))))
    (with-attr-table cpp
      (is (equal
           (project-dependency-tree
            cpp
            :entry-points '("main.cpp"
                            "BasicPlane.Figures-Rectangle.cpp"))
           '(("main.cpp"
              ("BasicPlane.Figures.ixx"
               ("BasicPlane.Figures-Rectangle.ixx"
                ("BasicPlane.Figures-Point.ixx"))
               ("BasicPlane.Figures-Point.ixx"))
              (:|iostream| (:|streambuf|) (:|ostream|) (:|istream|)
               (:|ios| (:|iosfwd|))))
             ("BasicPlane.Figures-Rectangle.cpp"
              ("BasicPlane.Figures-Rectangle.ixx"
               ("BasicPlane.Figures-Point.ixx")))))))))

(deftest test-ms-module-example-2/clang ()
  "Clang-compilable version of basic plane example."
  (let* ((cpp (from-file 'cpp-project
                         (make-pathname
                          :directory (append +etc-dir+
                                             '("module-examples" "clang-basic-plane-example"))))))
    (with-attr-table cpp
      (let ((main (is (evolve-files-ref cpp "main.cpp"))))
        (is (typep (get-declaration-ast :type (stmt-with-text (genome main) "Rectangle"))
                   'cpp-struct-specifier))))))

(deftest test-ms-module-example-2/clang/dependencies ()
  "Clang-compilable version of basic plane example."
  (let* ((cpp (from-file 'cpp-project
                         (make-pathname
                          :directory (append +etc-dir+
                                             '("module-examples"
                                               "clang-basic-plane-example"))))))
    (with-attr-table cpp
      (is (equal (project-dependency-tree cpp :entry-points '("main.cpp"))
                 '(("main.cpp"
                    ("BasicPlane.Figures.cppm"
                     ("BasicPlane.Figures-Rectangle.cppm"
                      ("BasicPlane.Figures-Point.cppm"))
                     ("BasicPlane.Figures-Point.cppm"))
                    (:|iostream| (:|streambuf|) (:|ostream|) (:|istream|)
                     (:|ios| (:|iosfwd|))))))))))

(deftest test-multi-file-simple ()
  (let* ((cpp (from-file 'cpp-project
                         (make-pathname
                          :directory (append +etc-dir+
                                             '("module-examples"
                                               "multi-file-simple")))))
         (main (evolve-files-ref cpp "main.cc")))
    (with-attr-table cpp
      (get-declaration-ast :type (stmt-with-text (genome main) "A")))))

(deftest cpp-evolve-files-dependency-order ()
  "Cycles are handled without error."
  (with-fixture cpp-dependency-order
    (with-attr-session (*project*)
      (let* ((evolve-files (evolve-files *project*))
             (evolve-files/dependency-order
               (evolve-files/dependency-order *project*)))
        (is (length= evolve-files evolve-files/dependency-order))
        ;; Same files

        (is (equal '("a.h" "b.h" "f.h" "g.h" "main.cc")
                   (mapcar #'car evolve-files/dependency-order)))))))

(deftest test-multi-file-simple/dependencies ()
  (let* ((cpp (from-file 'cpp-project
                         (make-pathname
                          :directory (append +etc-dir+
                                             '("module-examples"
                                               "multi-file-simple"))))))
    (with-attr-table cpp
      (is (equal (project-dependency-tree cpp :entry-points (list "main.cc"))
                 ;; "Circular" dependencies are due to MSVC behavior
                 ;; for module self-inclusion.
                 '(("main.cc"
                    ("a.cppm"
                     ("a-impl_part.cppm"
                      ("b.cppm"
                       ("b-impl_part.cppm"
                        ("b-interface_part.cppm")
                        (:|iostream| (:|streambuf|) (:|ostream|)
                          (:|istream|)
                          (:|ios| (:|iosfwd|))))
                       ("b-interface_part.cppm"))
                      ("a-interface_part.cppm"))
                     ("a-interface_part.cppm")))))))))

(deftest test-header-unit ()
  (let ((cpp
         (from-file 'cpp-project
                    (make-pathname
                     :directory
                     (append1 +etc-dir+
                              "cpp-header-unit")))))
    (with-attr-table cpp
      (is (get-declaration-ast :variable (stmt-with-text cpp "std::cout")))
      (is (get-declaration-ast :variable (stmt-with-text cpp "std::endl"))))))

(deftest test-interface-part-export-block ()
  (let ((cpp (from-file 'cpp-project
                        (make-pathname
                         :directory
                         (append +etc-dir+
                                 '("module-examples"
                                   "interface-part-example"))))))
    (with-attr-table cpp
      (let ((fn (find-if (of-type 'cpp-function-definition)
                         (dir:evolve-files-ref cpp "B-impl_part.cppm"))))
        (is (typep (get-declaration-ast :function fn)
                   'cpp-declaration))))))

(deftest test-impl-module-definitions-visible-in-symbol-table ()
  (let ((cpp (from-file 'cpp-project
                        (make-pathname
                         :directory
                         (append +etc-dir+
                                 '("module-examples"
                                   "impl-files-symtab"))))))
    (with-attr-table cpp
      (let* ((call
              (find-if (of-type 'call-ast)
                       (dir:evolve-files-ref cpp "main.cc")))
             (fn (call-function call)))
        (is (length= 2 (get-declaration-asts :function fn)))))))
