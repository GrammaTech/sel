;;;; clang-ancestry.lisp --- Ancestry tests.
(defpackage :software-evolution-library/test/clang-ancestry
  (:nicknames :sel/test/clang-ancestry)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :stefil+
   :software-evolution-library
   :software-evolution-library/software/ancestral
   :software-evolution-library/software/clang)
  (:export :test-clang-ancestry))
(in-package :software-evolution-library/test/clang-ancestry)
(in-readtable :curry-compose-reader-macros)
(defsuite test-clang-ancestry "Ancestry tests." (clang-available-p))

(defclass clang-w-ancestry (clang ancestral) ())

(defixture hello-world-clang-w-ancestry
  (:setup
   (setf sel/sw/ancestral::*next-ancestry-id* 0
         *hello-world*
         (from-file (make-instance 'clang-w-ancestry :compiler "clang"
                                   :flags '("-g" "-O0"))
                    (hello-world-dir "hello_world.c"))
         *test* [#'length #'genome-string])
   (evaluate *test* *hello-world*))
  (:teardown
   (setf *hello-world* nil
         *test* nil
         *next-ancestry-id* 0)))

(deftest apply-mutation-logs-ancestry ()
  (with-fixture hello-world-clang-w-ancestry
    (let ((op (make-instance 'clang-cut
                :object *hello-world*
                :targets `((:stmt1 . ,(stmt-with-text *hello-world*
                                                      "return 0;"))))))
      (apply-mutation *hello-world* op)
      (evaluate *test* *hello-world*)

      (is (< 1 (length (ancestors *hello-world*))))

      (is (= 1 (plist-get :id (first (ancestors *hello-world*)))))
      (is (not (null (plist-get :fitness (first (ancestors *hello-world*))))))
      (is (equal (type-of op)
                 (plist-get :mutant (first (ancestors *hello-world*)))))

      (is (= 0 (plist-get :id (second (ancestors *hello-world*)))))
      (is (equal 'from-file
                 (plist-get :how (second (ancestors *hello-world*))))))))

(deftest crossover-logs-ancestry ()
  (with-fixture hello-world-clang-w-ancestry
    (let ((crossed (crossover *hello-world* *hello-world*)))
      (is (< 1 (length (ancestors crossed))))

      (is (not (null (plist-get :crossover (first (ancestors crossed))))))
      (is (= 0
             (plist-get :id
                        (first (plist-get :cross-with
                                          (first (ancestors crossed)))))))
      (is (equal
           'from-file
           (plist-get :how
                      (first (plist-get :cross-with
                                        (first (ancestors crossed))))))))))

(deftest (graphing-ancestry :long-running) ()
  (unless (resolve-executable "dot")
    (return-from graphing-ancestry))
  (with-fixture hello-world-clang-w-ancestry
    (apply-mutation *hello-world*
                    (make-instance 'clang-cut
                      :object *hello-world*
                      :targets `((:stmt1 . ,(stmt-with-text *hello-world*
                                                            "return 0;")))))
    (with-temporary-file (:pathname save-base)
      (multiple-value-bind (stdout stderr errno)
          (save-ancestry *hello-world*
                         (pathname-directory save-base)
                         (pathname-name save-base))
        (declare (ignorable stdout stderr))
        (let ((svg (make-pathname :directory (pathname-directory save-base)
                                  :name (pathname-name save-base)
                                  :type "svg"))
              (dot (make-pathname :directory (pathname-directory save-base)
                                  :name (pathname-name save-base)
                                  :type "dot")))
          (when (probe-file svg) (delete-file svg))
          (when (probe-file dot) (delete-file dot)))
        (is (zerop errno))))))
