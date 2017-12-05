;; Copyright (C) 2013  Eric Schulte
(defpackage :software-evolution-library-test
  (:nicknames :sel-test)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :metabang-bind
   :curry-compose-reader-macros
   :cl-arrows
   :iterate
   :split-sequence
   :stefil
   :cl-ppcre
   :software-evolution-library
   :software-evolution-library-utility
   :testbot
   :trace-db
   :trivial-shell
   :uiop)
  (:shadowing-import-from :iterate :iter :for :until :collecting :in)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:shadowing-import-from :uiop :getenv :quit)
  (:shadowing-import-from
   :alexandria
   :appendf :ensure-list :featurep :emptyp
   :if-let :ensure-function :ensure-gethash :copy-file
   :parse-body :simple-style-warning)
  (:export :test :batch-test :testbot-test :sel-suite*))
#+allegro
(set-dispatch-macro-character #\# #\_
                              #'(lambda (s c n) (declare (ignore s c n)) nil))
