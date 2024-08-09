;;;; utility.lisp --- Utility tests.
(defpackage :software-evolution-library/test/utility
  (:nicknames :sel/test/utility)
  (:use
   :gt/full
   :cl-store
   #+gt :testbot
   :software-evolution-library/test/util
   :stefil+
   :software-evolution-library/utility/debug
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

(deftest test-source-location-position-conversion ()
  (flet ((loc (line col) (make 'source-location :line line :column col))
         (pos= (s loc pos)
           (is (equal? loc (position->source-location s pos)))
           (is (equal? pos (source-location->position s loc)))
           t))
    (is (equal? (loc 1 1)
                (position->source-location "" 0)))
    (let ((s (fmt "~%")))
      (is (pos= s (loc 1 1) 0))
      (is (pos= s (loc 2 1) 1)))
    (let ((s (fmt "~%~%")))
      (is (pos= s (loc 1 1) 0))
      (is (pos= s (loc 2 1) 1))
      (is (pos= s (loc 3 1) 2)))
    (let ((s (fmt "~%a~%b")))
      (is (pos= s (loc 1 1) 0))
      (is (pos= s (loc 2 1) 1))
      (is (pos= s (loc 2 2) 2))
      (is (pos= s (loc 3 1) 3))
      (is (pos= s (loc 3 2) 4)))))

(deftest test-symbolic-note ()
  "`note' should accept symbolic levels."
  (let ((*note-level* 0))
    (is (emptyp (with-output-to-string (*note-out*)
                  (note :trace "Tracing")))))
  (let ((*note-level* most-positive-fixnum))
    (is (search "Tracing"
               (with-output-to-string (*note-out*)
                  (note :trace "Tracing"))))))

(deftest test-lazy-note ()
  "`lazy-note' shouldn't evaluate arguments until the log level is met."
  (let ((side-effect? nil)
        (*note-out* (make-broadcast-stream)))
    (let ((*note-level* 0))
      (lazy-note :trace "Tracing ~a"
                 (setf side-effect? t)))
    (is (null side-effect?))
    (let ((*note-level* most-positive-fixnum))
      (lazy-note :trace "Tracing ~a"
                 (setf side-effect? t)))
    (is side-effect?)))
