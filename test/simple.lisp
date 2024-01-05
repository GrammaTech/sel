(defpackage :software-evolution-library/test/simple
  (:nicknames :sel/test/simple)
  (:use
    :gt/full
    #+gt :testbot
    :software-evolution-library/test/util
    :stefil+
    :software-evolution-library
    :software-evolution-library/software/simple
    :software-evolution-library/software/parseable)
  (:export :test-simple))

(in-package :software-evolution-library/test/simple)

(defsuite test-simple "Simple software object tests")

(deftest simple-from-string ()
  (is (typep (from-string 'simple "simple-text")
             'simple)))

(deftest simple-source-text ()
  (is (equal (source-text (from-string 'simple "simple-text"))
             (fmt "simple-text"))))

(def +contents+ "hello world")

(deftest test-lazy-load-simple-genome-string ()
  (with-temporary-file-of (:pathname p) +contents+
    (let ((simple (from-file 'simple p)))
      (is (equal (slot-value simple 'genome) (pathname p)))
      (is (equal (genome-string simple) +contents+))
      (is (stringp (slot-value simple 'genome)))
      (is (equal (slot-value simple 'genome) +contents+)))))

(deftest test-lazy-load-simple-genome ()
  (let ((contents (string+ +contents+ #\Newline)))
    (with-temporary-file-of (:pathname p) contents
      (let ((simple (from-file 'simple p)))
        (is (equal (slot-value simple 'genome) (pathname p)))
        (is (listp (genome simple)))
        (is (equal (genome-string simple) contents))))))

(deftest test-lazy-copy-simple-genome-from-path ()
  (with-temporary-file-of (:pathname p) +contents+
    (let ((simple1 (from-file 'simple p)))
      (is (equal (slot-value simple1 'genome) (pathname p)))
      (let ((simple2 (copy simple1)))
        (is (pathnamep (slot-value simple2 'genome)))
        (is (eql (slot-value simple1 'genome)
                 (slot-value simple2 'genome)))))))

(deftest test-lazy-copy-simple-genome-from-string ()
  (let ((simple1 (from-string 'simple +contents+)))
    (is (equal (slot-value simple1 'genome) +contents+))
    (let ((simple2 (copy simple1)))
      (is (stringp (slot-value simple2 'genome)))
      (is (eql (slot-value simple1 'genome)
               (slot-value simple2 'genome))))))

(deftest test-actually-copy-simple-genome ()
  (with-temporary-file-of (:pathname p) +contents+
    (let ((simple1 (from-file 'simple p)))
      (is (listp (genome simple1)))
      (let ((simple2 (copy simple1)))
        (is (not (eql simple1 simple2)))
        (is (listp (slot-value simple2 'genome)))
        (is (equal (genome simple1)
                   (genome simple2)))
        (is (not (eql (slot-value simple1 'genome)
                      (slot-value simple2 'genome))))
        (is (not (eql (genome simple1)
                      (genome simple2))))))))
