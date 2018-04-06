(defpackage :software-evolution-library/mongo-test
  (:nicknames :sel/mongo-test)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :metabang-bind
   :named-readtables
   :curry-compose-reader-macros
   :cl-arrowz
   :split-sequence
   :stefil
   :software-evolution-library
   :software-evolution-library/utility
   :software-evolution-library/test
   :software-evolution-library/mongo
   #+gt :testbot)
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
