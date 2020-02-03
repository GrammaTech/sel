;;;; declaration-type-databases.lisp --- Tests of declaration and type databases on clang objects.
(defpackage :software-evolution-library/test/declaration-type-databases
  (:nicknames :sel/test/declaration-type-databases)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/constants
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/utility)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :declaration-type-databases))
(in-package :software-evolution-library/test/declaration-type-databases)
(in-readtable :curry-compose-reader-macros)
(defsuite declaration-type-databases)

(defvar *huf* nil "Holds the huf software object.")

(deftest (huf-knows-types :long-running) ()
  (with-fixture huf-clang
    (is (and (hash-table-p (types *huf*))
             (not (zerop (hash-table-count (types *huf*)))))
        "Huf software object has a type database.")
    (let ((pointer-types
           (remove-if-not
            «and #'type-pointer
                 [#'not #'type-const]
                 [#'not {find #\(} #'type-name]»
            (hash-table-values (types *huf*)))))
      (is (subsetp '("char" "heap_t" "int" "huffcode_t"
                     "huffcode_t*" "long" "void")
                   (mapcar [{remove #\Space} #'type-name]
                           pointer-types)
                   :test #'equal)
          "Huf has seven expected pointer types"))
    (let ((const-pointer-types
           (remove-if-not
            «and #'type-pointer
                 #'type-const
                 [#'not {find #\(} #'type-name]»
            (hash-table-values (types *huf*)))))
      (is (subsetp '("char" "void")
                   (mapcar [{remove #\Space} #'type-name]
                           const-pointer-types)
                   :test #'equal)
          "Huf has two expected pointer types"))
    (let ((array-types
           (remove-if
            [#'emptyp #'type-array]
            (hash-table-values (types *huf*)))))
      (is (>= (count "int" array-types :key #'type-name :test #'equal) 1)
          "Huf has at least 1 int array type")
      (is (>= (count "char" array-types :key #'type-name :test #'equal) 3)
          "Huf has at least 3 char array types")
      (is (>= (count "long" array-types :key #'type-name :test #'equal) 2)
          "Huf has at least 2 long array types"))
    (is (<= 3 (count-if [{string= "int"} #'type-name]
                        (hash-table-values (types *huf*))))
        "Huf has at least three different \"int\" types ~
         (some are array and pointer).")))

(deftest (huf-finds-type-info-for-variables :long-running) ()
  (with-fixture huf-clang
    (let ((type (->> (stmt-with-text *huf* "p = test;")
                     (get-vars-in-scope *huf*)
                     (find-if [{name= "strbit"} {aget :name}])
                     (find-var-type *huf*))))
      (is type "Found type for \"strbit\" in huf.")
      (is (string= "[100]" (type-array type))
          "Variable \"strbit\" in huf is a dynamically sized array.")
      (is (not (type-pointer type))
          "Variable \"strbit\" in huf is not a pointer."))))
