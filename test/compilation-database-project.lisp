;;;; clang-project.lisp --- Project tests.
(defpackage :software-evolution-library/test/compilation-database-project
  (:nicknames :sel/test/compilation-database-project)
  (:local-nicknames
   (:compdb :software-evolution-library/components/compilation-database))
  (:use
    :gt/full
    #+gt :testbot
    :software-evolution-library/test/util
    :software-evolution-library/test/util-clang
    :stefil+
    :software-evolution-library
    :software-evolution-library/components/compilation-database
    :software-evolution-library/software/simple
    :software-evolution-library/software/compilable
    :software-evolution-library/software/clang
    :software-evolution-library/software/project)
  (:export :test-compilation-database-project))
(in-package :software-evolution-library/test/compilation-database-project)
(in-readtable :curry-compose-reader-macros)
(defsuite test-compilation-database-project "Mixin for compilation databases.")

(deftest compilation-database-relative-names ()
  (let ((db (parse-compilation-database
             '(((:arguments "cc")
                (:directory . "/home/me/files/dir1/")
                (:file . "../dir2/file"))))))
    (is (lookup db "/home/me/files/dir2/file"))))

(deftest compilation-database-flags-test ()
  (is (equal (list "-D" "DIR=\"/tmp\"" "-D" "IN" "-D" "_U_=a")
             (command-flags
              (make 'command-object
                    :directory ""
                    :file ""
                    :command "cc -DDIR=\\\"/tmp\\\" -DIN \"-D_U_=a\""))))
  (is (equal (list "-D" "DIR1=\"/tmp1\"" "-D" "DIR2=\"/tmp2\"")
             (command-flags
              (make 'command-object
                    :directory ""
                    :file ""
                    :command "cc -DDIR1=\\\"/tmp1\\\" -DDIR2=\\\"/tmp2\\\""))))
  (is (equal (list "-D" "DIR=\"\"")
             (command-flags
              (make 'command-object
                    :directory ""
                    :file ""
                    :command `"cc -DDIR=\\\"\\\"")))))

(deftest normalize-flags-test ()
  (is (equal (normalize-flags "/foo/" (list "-Wall"))
             (list "-Wall")))
  (is (equal (normalize-flags "/foo/" (list "-I/bar/"))
             (list "-I" "/bar/")))
  (is (equal (normalize-flags "/foo/" (list "-I" "/bar/"))
             (list "-I" "/bar/")))
  (is (equal (normalize-flags "/foo/" (list "-L/bar/"))
             (list "-L" "/bar/")))
  (is (equal (normalize-flags "/foo/" (list "-L" "/bar/"))
             (list "-L" "/bar/")))
  (is (equal (normalize-flags "/foo/" (list "-D\"blah\\ blah\""))
             (list "-D" "\"blah\\ blah\"")))
  (is (equal (normalize-flags "/foo/" (list "-D\"blah blah\""))
             (list "-D" "\"blah blah\"")))
  (is (find "/foo/" (normalize-flags "/foo/" (list "-I."))
            :test (lambda (s1 s2) (search s1 s2 :test #'equal))))
  (is (find "/foo/" (normalize-flags "/foo/" (list "-I" "."))
            :test (lambda (s1 s2) (search s1 s2 :test #'equal))))
  (is (find "/foo/" (normalize-flags "/foo/" (list "-L."))
            :test (lambda (s1 s2) (search s1 s2 :test #'equal))))
  (is (find "/foo/" (normalize-flags "/foo/" (list "-L" "."))
            :test (lambda (s1 s2) (search s1 s2 :test #'equal)))))

(deftest test-normalize-flags-string ()
  (is (equal '("-D" "name(args)=def")
             (normalize-flags-string "" "-D'name(args)=def'")))
  (is (equal '("-D" "name")
             (normalize-flags-string "" "-Dname")))
  (is (equal '("-D" "name")
             (normalize-flags-string "" "-D'name'")))
  (is (equal '("-D" "name")
             (normalize-flags-string "" "-D name")))
  (is (equal '("-D" "name")
             (normalize-flags-string "" "-D 'name'"))))

(deftest test-parse-function-like-macro-def ()
  (is (equal (parse-macro-def-arg "name(arg)=x")
             '(("name" "arg") . "x")))
  (is (equal (parse-macro-def-arg "name()=x")
             '(("name") . "x")))
  (is (equal (parse-macro-def-arg "name(...)=x")
             '(("name" "...") . "x")))
  (is (equal (parse-macro-def-arg "name(x,y)=x")
             '(("name" "x" "y") . "x")))
  (is (equal (parse-macro-def-arg "name(x,y,...)=x")
             '(("name" "x" "y" "...") . "x"))))

(deftest test-parse-bad-function-like-macro-def ()
  ;; If you put a space before the arguments to a macro, it treats the
  ;; arguments as part of the definition. Try it for yourself:

  ;; `echo 'x' | cpp -D'x (y)=z=1'`
  (is (equal (parse-macro-def-arg "x (y)=z")
             '("x" . "(y) z")))
  (is (equal (parse-macro-def-arg "x (y)=z=1")
             '("x" . "(y) z=1"))))

(deftest test-command-object-definitions ()
  (is (subsetp `(("_U_" . "a")
                 ("IN" . "1")
                 ("DIR" . "\"/tmp\""))
               (command-preproc-defs
                (make 'command-object
                      :directory ""
                      :file ""
                      :command "cc -DDIR=\\\"/tmp\\\" -DIN \"-D_U_=a\""))
               :test #'equal))
  (is (subsetp `(("DIR2" . "\"/tmp2\"")
                 ("DIR1" . "\"/tmp1\""))
               (command-preproc-defs
                (make 'command-object
                      :directory ""
                      :file ""
                      :command "cc -DDIR1=\\\"/tmp1\\\" -DDIR2=\\\"/tmp2\\\""))
               :test #'equal))
  (is (subsetp `(("DIR" . "\"\""))
               (command-preproc-defs
                (make 'command-object
                      :directory ""
                      :file ""
                      :command `"cc -DDIR=\\\"\\\""))
               :test #'equal)))

(deftest test-compute-header-dirs ()
  (is (equal '(:current :always :system :stdinc)
             (compute-header-dirs nil)))
  (is (equal '(:current :always :system)
             (compute-header-dirs '("-nostdinc"))))
  (is (equal '(:current :always :system)
             (compute-header-dirs '("-nostdinc++"))))
  (is (equal '(:current :always "mydir" :system :stdinc)
             (compute-header-dirs '("-I" "mydir"))))
  (is (equal '(:current :always "mydir" :system :stdinc)
             (compute-header-dirs '("-I" "mydir"))))
  (is (equal '(:current :always "mydir" :system :stdinc)
             (compute-header-dirs '("--include-directory" "mydir"))))
  (is (equal '(:current :always :system "../mydir" :stdinc)
             (compute-header-dirs '("-isystem" "../mydir"))))
  (is (equal '(:current :always :system "../mydir" :stdinc)
             (compute-header-dirs '("-cxx-isystem" "../mydir"))))
  (is (equal '(:current :always :system :stdinc "../mydir")
             (compute-header-dirs '("-idirafter" "../mydir"))))
  (is (equal '(:current :always :system :stdinc "../mydir")
             (compute-header-dirs '("--include-directory-after" "../mydir"))))
  (is (equal '(:current "../mydir" :always :system :stdinc)
             (compute-header-dirs '("-iquote" "../mydir"))))
  (is (equal '("../mydir" :always "../my-other-dir" :system :stdinc)
             (compute-header-dirs '("-I" "../mydir" "-I-" "-I" "../my-other-dir"))))
  (is (equal '("../my-dir" :always "../my-other-dir" :system :stdinc)
             (compute-header-dirs '("-I" "../my-dir" "--include-barrier" "-I" "../my-other-dir"))))
  (is (equal '("../my-dir" :always "../my-other-dir" :system :stdinc)
             (compute-header-dirs '("-I" "../my-dir" "--include-barrier" "-I" "../my-other-dir"))))
  (is (equal '(:current :always "/prefix/suffix2" :system :stdinc "/prefix/suffix1")
             (compute-header-dirs '("-iprefix"
                                    "/prefix/"
                                    "-iwithprefix"
                                    "suffix1"
                                    "-iwithprefixbefore"
                                    "suffix2")))))

(deftest test-compute-header-dirs-regression ()
  (nest
   (is)
   (length= 2)
   (filter #'stringp)
   (compute-header-dirs
    '("-c" "-D" "DEF1" "-D" "DEF2" "-D" "DEF3"
      "-D" "DEF4" "-D" "DEF5" "-I" "dir1" "-I" "dir2"
      "-std=c++11" "-std=gnu++11" "-Wno-tautological-constant-compare" "-O3" "-D"
      "NDEBUG" "-o" "out"))))

(deftest test-compute-header-dirs/sysroot ()
  (is (equal '(:current :always "/sysroot/mydir" :system :stdinc)
             (compute-header-dirs
              (normalize-flags
               ""
               '("-isysroot"
                 "/sysroot"
                 "-I"
                 "=/mydir")))))
  (is (equal '(:current :always "/sysroot/mydir" :system :stdinc)
             (compute-header-dirs
              (normalize-flags
               ""
               '("-isysroot"
                 "/sysroot"
                 "-I"
                 "$SYSROOT/mydir")))))
  (is (equal '(:current :always "/sysroot/mydir" :system :stdinc)
             (compute-header-dirs
              (normalize-flags
               ""
               '("-isysroot"
                 "/sysroot"
                 "--sysroot"
                 "/other-sysroot"
                 "-I"
                 "=/mydir")))))
  (is (equal '(:current :always "/sysroot/mydir" :system :stdinc)
             (compute-header-dirs
              (normalize-flags
               ""
               '("--sysroot=/sysroot"
                 "-I"
                 "=/mydir"))))))

(deftest test-dump-compiler-macros ()
  (when (resolve-executable "gcc")
    (is (assoc "__GNUC__" (compdb::compiler-dump-predefined-macros "gcc")
               :test #'equal))))
