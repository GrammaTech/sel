;;;; fix-compilation.lisp --- Fix compilation tests.
(defpackage :software-evolution-library/test/fix-compilation
  (:nicknames :sel/test/fix-compilation)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/utility
   :software-evolution-library/software/ast
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang
   :software-evolution-library/software/new-clang
   :software-evolution-library/software/clang-w-fodder
   :software-evolution-library/components/fix-compilation)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :test-fix-compilation))
(in-package :software-evolution-library/test/fix-compilation)
(in-readtable :curry-compose-reader-macros)
(defsuite test-fix-compilation "Fix compilation tests."
  (clang-mutate-available-p))

(defvar *broken-clang* nil "")
(defvar *broken-gcc* nil "")

(defixture broken-compilation
  (:setup (setf *broken-clang*
                (make-instance 'clang-w-fodder
                  :genome "int main(int argc, char **argv) {
        printf(\"Hello, World!\\n\");
        return missing_variable;}"))
          (setf *database*
                (with-open-file (in (make-pathname :name "euler-example.json"
                                                   :directory +etc-dir+))
                  (make-instance 'json-database :json-stream in))))
  (:teardown (setf *database* nil)))

(defixture broken-compilation-gcc
  (:setup (setf *broken-gcc*
                (make-instance 'clang-w-fodder
                  :compiler "gcc"
                  :flags '("-m32" "-O0" "-g")
                  :genome "int main(int argc, char **argv) {
        printf(\"Hello, World!\\n\");
        return missing_variable;}"))
          (setf *database*
                (with-open-file (in (make-pathname :name "euler-example"
                                                   :type "json"
                                                   :directory +etc-dir+))
                  (make-instance 'json-database :json-stream in))))
  (:teardown (setf *database* nil)))

(deftest (fix-compilation-inserts-missing-include :long-running) ()
  (with-fixture broken-compilation
    (is (scan (format nil "\#include <~a>" "stdio.h")
              (genome-string (fix-compilation *broken-clang* 1)))))
  (with-fixture broken-compilation-gcc
    (is (scan (format nil "\#include <~a>" "stdio.h")
              (genome-string (fix-compilation *broken-gcc* 1))))))

(deftest (fix-compilation-inserts-declaration-and-initializes :long-running) ()
  (let ((*compilation-fixers*
         (remove-if-not
          «or {starts-with-subseq ":(\\d+):\\d+: error: use of undeclared"}
              {starts-with-subseq ":(\\d+):\\d+: error: (‘|')(\\S+)(’|')"}»
          *compilation-fixers*
          :key #'car)))
    (with-fixture broken-compilation
      (is (scan (quote-meta-chars "missing_variable =")
                (genome (fix-compilation *broken-clang* 4)))))
    (with-fixture broken-compilation-gcc
      (is (scan (quote-meta-chars "missing_variable =")
                ;; Without the retries this test can fail stochastically.
                (iter (for fixed =
                           (fix-compilation
                            (handler-bind
                                ((mutate
                                  (lambda (e)
                                    (declare (ignorable e))
                                    (invoke-restart 'keep-partial-asts))))
                              (copy *broken-gcc*))
                            4))
                      (unless (zerop (length (genome fixed)))
                        (return (genome fixed)))))))))

(deftest (fix-compilation-declare-var-as-pointer :long-running) ()
  (let ((*compilation-fixers*
         (remove-if-not
          «or {starts-with-subseq ":(\\d+):(\\d+): error: invalid type arg"}
              {starts-with-subseq ":(\\d+):(\\d+): error: indirection requir"}»
          *compilation-fixers*
          :key #'car)))
    (with-temp-file (genome ".c")
      (string-to-file "int main(int argc, char **argv) {
                        int y = 0;
                        return *y;
                      }"
                      genome)
      (let ((broken-clang (from-file (make-clang
                                      :compiler "clang"
                                      :flags '("-m32" "-O0" "-g"))
                                     genome))
            (broken-gcc   (from-file (make-clang
                                      :compiler "gcc"
                                      :flags '("-m32" "-O0" "-g"))
                                     genome)))
        (is (phenome-p (fix-compilation broken-clang 1)))
        (is (phenome-p (fix-compilation broken-gcc 1)))))))
