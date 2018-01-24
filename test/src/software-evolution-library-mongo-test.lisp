(in-package :software-evolution-library/mongo-test)
(in-readtable :curry-compose-reader-macros)

#-gt (load (make-pathname :name "testbot"
                          :type "lisp"
                          :directory (pathname-directory
                                      #.(or *compile-file-truename*
                                            *load-truename*
                                            *default-pathname-defaults*))))


;;;; Mongo Database tests.
(sel-suite* mongo-database-tests "Mongo database tests."
            (and (fboundp 'mongo-database)
                 (let ((host "dog")
                       (port 27017))
                   (handler-case (make-instance 'mongo-database
                                                :db "euler_test_clang_O0_no_pic"
                                                :host host
                                                :port port)
                     (usocket:ns-host-not-found-error (e)
                       (declare (ignorable e))
                       nil)))))

(defixture mongo-database
  (:setup
   (setf *database*
         (let ((host "dog")
               (port 27017))
           (make-instance 'mongo-database
                          :db "euler_test_clang_O0_no_pic"
                          :host host
                          :port port))))
  (:teardown
   (setf *database* nil)))

(deftest mongo-database-find-snippet-respects-class ()
  (with-fixture mongo-database
    (when *database*
      (is (null (-<>> (find-snippets *database* :ast-class "CompoundStmt")
                      (remove "CompoundStmt" <> :test #'string=
                              :key {aget :ast-class})))))))

(deftest mongo-database-find-snippet-respects-decl ()
  (with-fixture mongo-database
    (when *database*
      (is (null (->> (find-snippets *database* :decls nil)
                     (remove-if-not {aget :is-decl})))))))

(deftest mongo-database-find-snippet-respects-full-stmt ()
  (with-fixture mongo-database
    (when *database*
      (is (null (->> (find-snippets *database* :full-stmt t)
                     (remove-if {aget :full-stmt})))))))

(deftest mongo-database-find-snippet-is-random ()
  (with-fixture mongo-database
    (when *database*
      (let ((picks (loop :for i :from 0 :to 5
                         :collect (aget :hash (find-snippets *database*
                                                             :limit 1)))))
        (equal picks (remove-duplicates picks))))))
