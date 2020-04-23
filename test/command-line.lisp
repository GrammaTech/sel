;;;; cl.lisp --- Command Line tool tests.
(defpackage :software-evolution-library/test/command-line
  (:nicknames :sel/test/command-line)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :software-evolution-library/stefil-plus
   :software-evolution-library
   :software-evolution-library/command-line
   :software-evolution-library/software/simple
   :software-evolution-library/software/clang
   :software-evolution-library/software/project
   :software-evolution-library/software/clang-project
   :software-evolution-library/software/json)
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
  (is (eql 'clang (guess-language #P"this/foo.cpp")))
  (is (eql 'json (guess-language #P"this/foo.json")))
  (is (eql 'json (guess-language #P"this/foo.json" #P"this/bar.json")))
  (is (eql 'clang-project
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
             :build-command
             (concatenate 'string
                          (namestring (make-pathname :directory +grep-prj-dir+
                                                     :name "build"
                                                     :type "sh"))
                          " --full --nonsense-arg"))))
    (is sw)
    (is (eql 'clang-project (type-of sw)))
    (is (not (string= "./" (subseq (build-command sw) 0 2))))
    (is (search "--full" (build-command sw)))))
