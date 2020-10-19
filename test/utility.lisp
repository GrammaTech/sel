;;;; utility.lisp --- Utility tests.
(defpackage :software-evolution-library/test/utility
  (:nicknames :sel/test/utility)
  (:use
   :gt/full
   :cl-store
   #+gt :testbot
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library/utility/json
   :software-evolution-library/utility/range
   :software-evolution-library)
  (:import-from :jsown :parse)
  (:export :test-utility))
(in-package :software-evolution-library/test/utility)
(in-readtable :curry-compose-reader-macros)
(defsuite test-utility "Utility tests.")

(deftest intersects-does-not-include-endpoints ()
  (is (not (intersects (make-instance 'range :begin 0 :end 1)
                       (make-instance 'range :begin 1 :end 2))))
  (is (not (intersects (make-instance 'source-range
                         :begin (make-instance 'source-location :line 1
                                               :column 0)
                         :end   (make-instance 'source-location :line 2
                                               :column 0))
                       (make-instance 'source-range
                         :begin  (make-instance 'source-location :line 2
                                                :column 0)
                         :end    (make-instance 'source-location :line 3
                                                :column 0))))))

(deftest json-base-strings ()
  (let ((ct (convert-jsown-tree (jsown:parse "{ \"x\" : \"y\"}"))))
    (is (equal ct '((:x . "y"))))
    (is (typep (cdar ct) 'simple-base-string)))
  #+(or sbcl ccl)
  (let* ((c (string #\CENT_SIGN))
         (ct (convert-jsown-tree (jsown:parse
                                  (concatenate 'string
                                               "{ \"x\" : \""
                                               c
                                               "\"}")))))
    (is (equal ct `((:x . ,c))))))



