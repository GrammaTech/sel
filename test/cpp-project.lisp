;;;; cpp-project.lisp --- Project tests.
(defpackage :software-evolution-library/test/cpp-project
  (:nicknames :sel/test/cpp-project)
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
   :software-evolution-library/software/cpp-project)
  #-windows (:shadowing-import-from :osicat
                                    :file-permissions :pathname-as-directory)
  (:export :test-cpp-project))
(in-package :software-evolution-library/test/cpp-project)
(in-readtable :curry-compose-reader-macros)

(defun cpp-available-p ()
  "Returns true is the c tree-sitter parser is loaded."
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
