;;;; python-utility.lisp --- Python utility.
;;;;
;;;; This file tests the functions that operate on
;;;; python and python-ast objects.
(defpackage :software-evolution-library/test/python-utility
  (:nicknames :sel/test/python-utility
              :sel/test/py-util)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/python
   :software-evolution-library/software/non-homologous-parseable
   :software-evolution-library/components/file
   :software-evolution-library/components/formatting)
  (:export :test-python-utility))
(in-package :software-evolution-library/test/python-utility)
(in-readtable :curry-compose-reader-macros)
(defsuite test-python-utility "Python utility." (python3.8-available-p))



;;; Utility
(defmacro with-software-file ((filename software-var genome-var)
                              &body body)
  `(let* ((,software-var (from-file
                          (make-instance 'python)
                          (make-pathname :name ,filename
                                         :type "py"
                                         :directory +python-utility-dir+)))
          (,genome-var (genome ,software-var)))
     ,@body))


;;; Tests
(deftest python-collect-var-uses-1 ()
  "collect-var-uses collects global variable usages and ignores local bindings."
  (with-software-file ("global" soft genome)
    (let ((var-uses (collect-var-uses
                     soft
                     (find-if (lambda (ast)
                                (and (typep ast 'py-name)
                                     (string= "a"
                                              (ast-annotation ast :id))))
                              genome))))
      (is (= 4 (length var-uses))
          "~A did not contain the expected number of uses"
          var-uses))))

(deftest python-collect-var-uses-2 ()
  "collect-var-uses collects global variable usages when a local binding
appears in a scope above the global usage."
  (with-software-file ("nested-global" soft genome)
    (let ((var-uses (collect-var-uses
                     soft
                     (find-if (lambda (ast)
                                (and (typep ast 'py-name)
                                     (string= "a"
                                              (ast-annotation ast :id))))
                              genome))))
      (is (= 3 (length var-uses))
          "~A did not contain the expected number of uses"
          var-uses))))

(deftest python-collect-var-uses-3 ()
  "collect-var-uses collects nested local variable usages."
  (with-software-file ("local" soft genome)
    (let ((var-uses (collect-var-uses
                     soft
                     (find-if (lambda (ast)
                                (and (typep ast 'py-name)
                                     (string= "a"
                                              (ast-annotation ast :id))))
                              genome))))
      (is (= 4 (length var-uses))
          "~A did not contain the expected number of uses"
          var-uses))))

(deftest python-collect-var-uses-4 ()
  "collect-var-uses collects local variable usages and ignores global
bindings when shadowed."
  (with-software-file ("local-shadow" soft genome)
    (let ((var-uses (collect-var-uses
                     soft
                     (cadr
                      (collect-if (lambda (ast)
                                    (and (typep ast 'py-name)
                                         (string= "a"
                                                  (ast-annotation ast :id))))
                                  genome)))))
      (is (= 3 (length var-uses))
          "~A did not contain the expected number of uses"
          var-uses))))

(deftest python-collect-var-uses-5 ()
  "collect-var-uses doesn't include parameters as uses when targeting
a variable in a scope above it."
  (with-software-file ("parameter" soft genome)
    (let ((var-uses (collect-var-uses
                     soft
                     (find-if (lambda (ast)
                                (and (typep ast 'py-name)
                                     (string= "a"
                                              (ast-annotation ast :id))))
                              genome))))
      (is (= 2 (length var-uses))
          "~A did not contain the expected number of uses"
          var-uses))))
