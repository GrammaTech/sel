;;;; cl.lisp --- Command Line tool tests.
(defpackage :software-evolution-library/test/command-line
  (:nicknames :sel/test/command-line)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :stefil+
   :software-evolution-library
   :software-evolution-library/command-line
   :software-evolution-library/software/simple
   :software-evolution-library/software/project
   :software-evolution-library/software/clang-project
   :software-evolution-library/software/c-project
   :software-evolution-library/software/cpp-project
   :software-evolution-library/software/all-tree-sitter)
  (:import-from :software-evolution-library/software/clang :clang)
  (:export :test-cl))
(in-package :software-evolution-library/test/command-line)
(in-readtable :curry-compose-reader-macros)
(defsuite test-command-line "Command Line tool tests.")

(define-command fact-cl-entry
    (n &spec +common-command-line-options+)
  "Test that canonical REST endpoints work. Computes factorial."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose language))
  (if help
      (show-help-for-fact-cl-entry)
      (factorial n)))

(deftest run-factorial-cl-func ()
  (let ((*standard-output* (make-broadcast-stream)))
    (is (eql (fact-cl-entry 5 :verbose 3) 120))
    (is (eql (fact-cl-entry 52235215 :help T) nil))))

;;; FIXME: this does not work if (sel/test:test) is run while
;;; in some directory other than the sel root directory.
(deftest guess-language-test ()
  (is (eql (sel/command-line::find-cpp) (guess-language #P"this/foo.cpp")))
  (is (string-equal "json" (guess-language #P"this/foo.json")))
  (is (string-equal "json" (guess-language #P"this/foo.json"
                                           #P"this/bar.json")))
  (is (string-equal "c-sharp" (guess-language #P"this/foo.cs")))
  (is (string-equal "java" (guess-language #P"this/foo.java")))
  (is (string-equal "javascript" (guess-language #P"this/foo.js")))
  (is (string-equal "javascript" (guess-language #P"this/foo.mjs")))
  (is (string-equal "julia" (guess-language #P"this/foo.jl")))
  (is (string-equal "php" (guess-language #P"this/foo.php")))
  (is (string-equal "rust" (guess-language #P"this/foo.rs")))
  (is (string-equal "ruby" (guess-language #P"this/foo.rb")))
  (is (string-equal "scala" (guess-language #P"this/foo.scala")))
  (is (string-equal "typescript" (guess-language #P"this/foo.ts")))
  ;; TypeScript interface files are used in JS projects.
  (is (string-equal "javascript" (guess-language #P"this/foo.d.ts")))
  (is (eql (sel/command-line::language-to-project (sel/command-line::find-c))
           (guess-language (make-pathname :directory +grep-prj-dir+))))
  (is (eql 'simple (guess-language #P"this/Makefile")))
  (is (null (guess-language #P"foo.js" #P"bar.lisp"))))

(deftest resolving-languages-works ()
  (mapc (lambda (pair)
          (destructuring-bind (args result) pair
            (is result (apply #'resolve-language-from-language-and-source
                              args))))
        `((("c") 'clang)
          (("C++") 'clang)
          (("CL") 'lisp)
          (("JSON") 'json)
          (("C" ,(make-pathname :directory +grep-prj-dir+)) 'clang-project)
          (("lisp" "git@github.com:eschulte/lisp-format") 'lisp-git-project)
          (("C" "git://example.com:foo/bar.git") 'clang-git-project))))

#-windows
(deftest (create-software-guesses-clang-project :long-running) ()
  (let ((sw (create-software
             (make-pathname :directory +grep-prj-dir+)
             :build-command "make")))
    (is sw)
    (is (eql (sel/command-line::language-to-project (sel/command-line::find-c))
             (type-of sw)))
    (is (equal "make" (build-command sw)))))
