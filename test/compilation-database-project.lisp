;;;; clang-project.lisp --- Project tests.
(defpackage :software-evolution-library/test/compilation-database-project
  (:nicknames :sel/test/compilation-database-project)
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

(deftest test-parse-macro-definition ()
  (is (equal (multiple-value-list (parse-macro-definition "name"))
             (list "name" "1")))
  (is (equal (multiple-value-list (parse-macro-definition "name="))
             (list "name" "")))
  (is (equal (multiple-value-list (parse-macro-definition "name=myname"))
             (list "name" "myname")))
  (is (equal (multiple-value-list (parse-macro-definition
                                   (fmt "name=before-newline~%after-newline")))
             (list "name" "before-newline")))
  (is (equal (multiple-value-list (parse-macro-definition "x=x=1"))
             (list "x" "x=1"))))

(deftest test-command-object-definitions ()
  (is (equal `(("_U_" . "a")
               ("IN" . "1")
               ("DIR" . "\"/tmp\""))
             (command-preproc-defs
              (make 'command-object
                    :directory ""
                    :file ""
                    :command "cc -DDIR=\\\"/tmp\\\" -DIN \"-D_U_=a\""))))
  (is (equal `(("DIR2" . "\"/tmp2\"")
               ("DIR1" . "\"/tmp1\""))
             (command-preproc-defs
              (make 'command-object
                    :directory ""
                    :file ""
                    :command "cc -DDIR1=\\\"/tmp1\\\" -DDIR2=\\\"/tmp2\\\""))))
  (is (equal `(("DIR" . "\"\""))
             (command-preproc-defs
              (make 'command-object
                    :directory ""
                    :file ""
                    :command `"cc -DDIR=\\\"\\\"")))))
