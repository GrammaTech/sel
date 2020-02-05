;;;; database.lisp --- Database tests.
(defpackage :software-evolution-library/test/database
  (:nicknames :sel/test/database)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/utility
   :software-evolution-library/software/clang-w-fodder
   :software-evolution-library/components/fodder-database)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :test-database))
(in-package :software-evolution-library/test/database)
(in-readtable :curry-compose-reader-macros)
(defsuite test-database "Database tests.")

(defixture json-database
  (:setup
   (setf *database*
         (with-open-file (in (make-pathname :name "euler-example"
                                            :type "json"
                                            :directory +etc-dir+))
           (make-instance 'json-database :json-stream in))))
  (:teardown
   (setf *database* nil)))

(deftest json-database-find-snippet-respects-class ()
  (with-fixture json-database
    (is (null (-<>> (find-snippets *database* :ast-class "CompoundStmt")
                    (remove "CompoundStmt" <> :test #'string=
                            :key {aget :class}))))))

(deftest json-database-find-snippet-respects-decl ()
  (with-fixture json-database
    (is (null (nest (remove-if-not {aget :is-decl})
                    (find-snippets *database* :decls nil))))))

(deftest json-database-find-snippet-respects-full-stmt ()
  (with-fixture json-database
    (is (null (nest (remove-if {aget :full-stmt})
                    (find-snippets *database* :full-stmt t))))))

(deftest json-database-find-snippet-is-random ()
  (with-fixture json-database
    (let ((picks (loop :for i :from 0 :to 5
                    :collect (aget :hash (find-snippets *database*
                                                        :limit 1)))))
      (equal picks (remove-duplicates picks)))))
