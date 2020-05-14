;;;; fix-compilation.lisp --- Fix compilation tests.
(defpackage :software-evolution-library/test/fix-compilation
  (:nicknames :sel/test/fix-compilation)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :software-evolution-library/stefil-plus
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang
   :software-evolution-library/software/clang-w-fodder
   :software-evolution-library/components/fix-compilation
   :software-evolution-library/components/json-fodder-database)
  (:export :test-fix-compilation))
(in-package :software-evolution-library/test/fix-compilation)
(in-readtable :curry-compose-reader-macros)
(defsuite test-fix-compilation "Fix compilation tests." (clang-available-p))

(defvar *broken-clang* nil "")
(defvar *broken-gcc* nil "")

(defixture broken-compilation
  (:setup (setf *broken-clang*
                (make-instance 'clang-w-fodder
                  :genome "int main(int argc, char **argv) {
        printf(\"Hello, World!\\n\");
        return missing_variable;}")))
  (:teardown (setf *broken-clang* nil)))

(defixture broken-compilation-gcc
  (:setup (setf *broken-gcc*
                (make-instance 'clang
                  :compiler "gcc"
                  :flags '("-m32" "-O0" "-g")
                  :genome "int main(int argc, char **argv) {
        printf(\"Hello, World!\\n\");
        return missing_variable;}")))
  (:teardown (setf *broken-gcc* nil)))

(deftest (fix-compilation-inserts-declaration-and-initializes :long-running) ()
  (let ((*compilation-fixers*
         (remove-if-not
          «or {starts-with-subseq ":(\\d+):\\d+: error: use of undeclared"}
              {starts-with-subseq ":(\\d+):\\d+: error: (‘|')(\\S+)(’|')"}»
          *compilation-fixers*
          :key #'car)))
    (with-fixture broken-compilation
      (is (scan (quote-meta-chars "missing_variable =")
                (genome-string (fix-compilation *broken-clang* 4)))))
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
                      (unless (zerop (length (genome-string fixed)))
                        (return (genome-string fixed)))))))))

(deftest (fix-compilation-declare-var-as-pointer :long-running) ()
  (let ((*compilation-fixers*
         (remove-if-not
          «or {starts-with-subseq ":(\\d+):(\\d+): error: invalid type arg"}
              {starts-with-subseq ":(\\d+):(\\d+): error: indirection requir"}»
          *compilation-fixers*
          :key #'car)))
    (with-temporary-file (:pathname genome :type ".c")
      (string-to-file "int main(int argc, char **argv) {
                        int y = 0;
                        return *y;
                      }"
                      genome)
      (let ((broken-clang (from-file (make-instance 'clang
                                      :compiler "clang"
                                      :flags '("-m32" "-O0" "-g"))
                                     genome))
            (broken-gcc   (from-file (make-instance 'clang
                                      :compiler "gcc"
                                      :flags '("-m32" "-O0" "-g"))
                                     genome)))
        (is (phenome-p (fix-compilation broken-clang 1)))
        (is (phenome-p (fix-compilation broken-gcc 1)))))))
