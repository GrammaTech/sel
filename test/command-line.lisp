;;;; command-line.lisp --- Command line tests
(defpackage :software-evolution-library/test/command-line
  (:nicknames :sel/test/command-line)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/utility)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :command-line))
(in-package :software-evolution-library/test/command-line)
(in-readtable :curry-compose-reader-macros)
(defsuite command-line)

;;; FIXME: this does not work if (sel/test:test) is run while
;;; in some directory other than the sel root directory.
(deftest guess-language-test ()
  (is (eql (if *new-clang?* 'new-clang 'clang)
           (guess-language #P"this/foo.cpp")))
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
          (("java" ,(make-pathname :directory +grep-prj-dir+)) 'java-project)
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
