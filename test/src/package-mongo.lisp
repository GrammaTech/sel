;; Copyright (C) 2013  Eric Schulte
(defpackage :software-evolution-mongo-test
  (:nicknames :se-mongo-test)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :metabang-bind
   :curry-compose-reader-macros
   :cl-arrows
   :split-sequence
   :stefil
   :software-evolution
   :software-evolution-utility
   :software-evolution-test
   :software-evolution-mongo
   :testbot)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:shadowing-import-from
   :alexandria
   :appendf :ensure-list :featurep :emptyp
   :if-let :ensure-function :ensure-gethash :copy-file
   :parse-body :simple-style-warning)
  (:export :mongo-database-tests))
#+allegro
(set-dispatch-macro-character #\# #\_
                              #'(lambda (s c n) (declare (ignore s c n)) nil))
