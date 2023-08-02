(defpackage :software-evolution-library/test/simple
  (:nicknames :sel/test/simple)
  (:use
    :gt/full
    #+gt :testbot
    :software-evolution-library/test/util
    :stefil+
    :software-evolution-library
    :software-evolution-library/software/simple)
  (:export :test-simple))

(in-package :software-evolution-library/test/simple)

(defsuite test-simple "Simple software object tests")

(deftest simple-from-string ()
  (is (typep (from-string 'simple "simple-text")
             'simple)))
