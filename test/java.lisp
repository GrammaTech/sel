;;;; java.lisp --- JAVA representation.
(defpackage :software-evolution-library/test/java
  (:nicknames :sel/test/java)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   #+gt :testbot
   :trace-db
   :software-evolution-library
   :software-evolution-library/utility
   :software-evolution-library/software/source
   :software-evolution-library/software/java
   :software-evolution-library/software/project
   :software-evolution-library/software/java-project
   :software-evolution-library/components/instrument
   :software-evolution-library/components/traceable)
  (:import-from :uiop :nest)
  (:shadowing-import-from :software-evolution-library/test/util
                          :java-project)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :test-java))
(in-package :software-evolution-library/test/java)
(in-readtable :curry-compose-reader-macros)
(defsuite test-java "JAVA representation." :silent)

(defun java-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +java-dir+))

(define-constant +java-jars-dir+ (append +java-dir+ (list "Jars"))
  :test #'equalp
  :documentation "Path to directory holding the Build Folder jars.")

(defvar *java-file-name* nil "File name to be tested in a test case.")

(defixture general-fixture-java
  (:setup
   (setf *soft*
         (from-file (make-instance 'java)
                    (java-dir (concatenate 'string
                                           *java-file-name* ".java")))))
  (:teardown
   (setf *soft* nil)))

(defixture general-fixture-java-traceable
  (:setup
   (setf *soft*
         (from-file (make-instance 'java-traceable)
                    (java-dir (concatenate 'string
                                           *java-file-name* ".java")))))
  (:teardown
   (setf *soft* nil)))

;; Copy software object.
(deftest (java-test-copy :long-running) ()
  "Checks that when a deep copy of a software object is created. If the
  genome is updated in the original whenever the copy is modified, then
  there is a problem with the directives for the genome slot value
  defined in the java software object."
  (let ((*java-file-name* "TestSimple"))
    (with-fixture general-fixture-java
      (let ((temporary-obj (copy *soft*)))
        (setf (slot-value temporary-obj 'genome) "new genome")
        (is (not (equal (genome temporary-obj) (genome *soft*))))))))

(deftest (insert_testsimple :long-running) ()
  "Check if print stmt was inserted in the new genome, but not in original."
  (let ((*java-file-name* "TestSimple_WhileForIfPrint_2"))
    (with-fixture general-fixture-java
      (let ((before-genome (genome *soft*))
            (after-genome
             (nest
              (genome)
              (apply-mutation *soft*)
              (make-instance 'java-insert :targets)
              (list (cons :stmt1 (java-make-literal :integer 2)))
              (cons :value1)
              (java-make-literal :string)
              "System.out.println(\"THIS STATEMENT INSERTED\");"))
            (target "THIS STATEMENT INSERTED"))
        (is (not (scan-to-strings target before-genome)))
        (is (scan-to-strings target after-genome))))))

(deftest (add-import-test-java-1 :long-running) ()
  "Check if import was inserted into file with no package name."
  (let ((*java-file-name* "TestSimple_WhileForIfPrint_2"))
    (with-fixture general-fixture-java
      (let ((before-genome (genome *soft*))
            (after-genome (genome (add-import *soft* "java.util.LinkedList")))
            (target "java[.]util[.]LinkedList"))
        (is (not (scan-to-strings target before-genome)))
        (is (scan-to-strings target after-genome))))))

(deftest (add-import-test-java-2 :long-running) ()
  "Check if import was inserted into file with package name."
  (let ((*java-file-name* "TestSimple_package_name"))
    (with-fixture general-fixture-java
      (let ((before-genome (genome *soft*))
            (after-genome (genome (add-import *soft* "java.util.LinkedList")))
            (target "java[.]util[.]LinkedList"))
        (is (not (scan-to-strings target before-genome)))
        (is (scan-to-strings target after-genome))))))

(defun is-genome-modified-by-instrumentation-of (file)
  (let ((*java-file-name* file))
    (with-fixture general-fixture-java
      (let ((inst (copy *soft*))
            (target "java[.]io[.]PrintWriter"))
        (instrument inst)
        (is (not (scan-to-strings target (genome *soft*)))
            "Original version of ~a does not include ~a." file target)
        (is (scan-to-strings target (genome inst))
            "Instrumented version of ~a does include ~a." file target)))))

;;; Instrumentation tests.
(deftest (instrument-testsimple :long-running) ()
  "Currently checks whether file was modified with instrumentation commands."
  (is-genome-modified-by-instrumentation-of "TestSimple"))

(deftest (instrument-testsimple-with-one-pck :long-running) ()
  "Similar to `instrument_testsimple', but with one package name."
  (is-genome-modified-by-instrumentation-of "TestSimple_package_name"))

(deftest (instrument-testsimple-with-mult-pcks :long-running) ()
  "Similar to `instrument_testsimple', but with long package name."
  (is-genome-modified-by-instrumentation-of "TestSimple_longer_package_name"))

;; Phenome tests.
(defun is-phenome-execution-script-created-for (file)
  (let ((*java-file-name* file))
    (with-fixture general-fixture-java
      (instrument *soft*)
      (with-temp-file (bin)
        (phenome *soft* :bin bin)
        (is (probe-file bin))))))

(deftest (phenome-testsimple :long-running) ()
  "Runs the phenome function and checks if execution script was created."
  (is-phenome-execution-script-created-for "TestSimple"))

(deftest (phenome-testsimple-with-one-pck :long-running) ()
  "Runs the phenome function and checks if execution script was created."
  (is-phenome-execution-script-created-for "TestSimple_package_name"))

(deftest (phenome-testsimple-with-mult-pcks :long-running) ()
  "Runs the phenome function and checks if execution script was created."
  (is-phenome-execution-script-created-for "TestSimple_longer_package_name"))

;; Collect traces tests.
(defun is-exact-output-of-collect-traces-expected-for
    (file &optional (target '((:TRACE ((:C . 1) (:SCOPES))
                               ((:C . 2) (:SCOPES)))
                              (:INPUT :BIN))))
  (let ((*java-file-name* file))
    (with-fixture general-fixture-java-traceable
      (instrument *soft*)
      (is (equalp (get-trace
                   (collect-traces
                    *soft*
                    (make-instance 'test-suite
                      :test-cases (list (make-instance 'test-case
                                          :program-name :bin)))) 0)
                  target)))))

(deftest (collect-traces-testsimple :long-running) ()
  "Check exact output of collect-traces with the expected result."
  (is-exact-output-of-collect-traces-expected-for "TestSimple"))

(deftest (collect-traces-testsimple-with-one-pck :long-running) ()
  "Similar to `collect-traces-testsimple', but with one package name."
  (is-exact-output-of-collect-traces-expected-for "TestSimple_package_name"))

(deftest (collect-traces-testsimple-with-mult-pcks :long-running) ()
  "Similar to `collect-traces-testsimple', but with longer package name."
  (is-exact-output-of-collect-traces-expected-for
   "TestSimple_longer_package_name"))

(deftest (collect-traces-testsimple-whileforifprint :long-running) ()
  "Similar to `collect-traces-testsimple', but with larger trace file."
  (is-exact-output-of-collect-traces-expected-for
   "TestSimple_WhileForIfPrint"
   '((:TRACE
      ((:C . 1)  (:SCOPES))
      ((:C . 2)  (:SCOPES . (("test1" "int" 0))))
      ((:C . 3)  (:SCOPES . (("test1" "int" 0))))
      ((:C . 4)  (:SCOPES . (("test1" "int" 0))))
      ((:C . 3)  (:SCOPES . (("test1" "int" 1))))
      ((:C . 4)  (:SCOPES . (("test1" "int" 1))))
      ((:C . 3)  (:SCOPES . (("test1" "int" 2))))
      ((:C . 4)  (:SCOPES . (("test1" "int" 2))))
      ((:C . 5)  (:SCOPES . (("test1" "int" 3))))
      ((:C . 6)  (:SCOPES . (("k" "int" 0)("test1" "int" 3))))
      ((:C . 7)  (:SCOPES . (("k" "int" 0)("test1" "int" 3))))
      ((:C . 8)  (:SCOPES . (("k" "int" 0)("test1" "int" 3))))
      ((:C . 7)  (:SCOPES . (("k" "int" 1)("test1" "int" 3))))
      ((:C . 8)  (:SCOPES . (("k" "int" 1)("test1" "int" 3))))
      ((:C . 9)  (:SCOPES . (("k" "int" 1)("test1" "int" 3))))
      ((:C . 10) (:SCOPES . (("k" "int" 1)("test1" "int" 3)
                             ("test2" "int" 30))))
      ((:C . 11) (:SCOPES . (("k" "int" 2)("test1" "int" 3))))
      ((:C . 12) (:SCOPES . (("k" "int" 2)("test1" "int" 3))))
      ((:C . 13) (:SCOPES . (("k" "int" 2)("test1" "int" 3)))))
     (:INPUT :BIN))))

(deftest (collect-traces-testsimple-whileforifprint-2 :long-running) ()
  "Similar to `collect-traces-testsimple', but with larger trace file."
  (is-exact-output-of-collect-traces-expected-for
   "TestSimple_WhileForIfPrint_2"
   '((:TRACE
      ((:C . 1) (:SCOPES))
      ((:C . 2) (:SCOPES . (("test1" "int" 0))))
      ((:C . 3) (:SCOPES . (("test1" "int" 0))))
      ((:C . 4) (:SCOPES . (("test1" "int" 0))))
      ((:C . 3) (:SCOPES . (("test1" "int" 1))))
      ((:C . 4) (:SCOPES . (("test1" "int" 1))))
      ((:C . 3) (:SCOPES . (("test1" "int" 2))))
      ((:C . 4) (:SCOPES . (("test1" "int" 2))))
      ((:C . 5) (:SCOPES . (("test1" "int" 3))))
      ((:C . 6) (:SCOPES . (("k" "int" 0)("test1" "int" 3))))
      ((:C . 7) (:SCOPES . (("k" "int" 0)("test1" "int" 3))))
      ((:C . 6) (:SCOPES . (("k" "int" 1)("test1" "int" 3))))
      ((:C . 7) (:SCOPES . (("k" "int" 1)("test1" "int" 3))))
      ((:C . 8) (:SCOPES . (("k" "int" 1)("test1" "int" 3))))
      ((:C . 9) (:SCOPES . (("k" "int" 1)("test1" "int" 3)
                            ("test2" "int" 30)))))
     (:INPUT :BIN))))

(deftest (java-project-test :long-running) ()
  (with-fixture java-project
    (is (equal "./gt-harness.sh build" (build-command *soft*)))
    (is (equal 2 (length (evolve-files *soft*))))
    (is (find "src/main/java/com/simple/multi/maven/app/App.java"
              (evolve-files *soft*) :key #'car :test #'string=))
    (is (find "src/main/java/com/simple/multi/maven/extras/SharedClassImpl.java"
              (evolve-files *soft*) :key #'car :test #'string=))
    (is (equal "java" (compiler (cdr (first (evolve-files *soft*))))))))

(deftest (java-build-folder-jar-test :long-running) ()
  "Tests if applicable file names in a build-folder are found."
  (with-temp-dir-of (temp-dir) (make-pathname :directory +java-jars-dir+)
                    (is (equal 9 (length (get-files-jar temp-dir))))))
