;;;; type-traces.lisp --- Types and traces tests.
(defpackage :software-evolution-library/test/type-traces
  (:nicknames :sel/test/type-traces)
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
   :software-evolution-library/utility)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :type-traces))
(in-package :software-evolution-library/test/type-traces)
(in-readtable :curry-compose-reader-macros)
(defsuite type-traces)

(deftest type-trace-string-test ()
  (is (equalp "int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array ""
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "*int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer t
                                                  :array ""
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "[]int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array "[]"
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "[5]int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array "[5]"
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "*[]int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer t
                                                  :array "[]"
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "*[5]int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer t
                                                  :array "[5]"
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "const int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array ""
                                                  :const t
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "volatile int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array ""
                                                  :volatile t
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "*restrict int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer t
                                                  :array ""
                                                  :restrict t
                                                  :storage-class :None
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "auto int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array ""
                                                  :storage-class :Auto
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "static int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array ""
                                                  :storage-class :static
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "extern int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array ""
                                                  :storage-class :extern
                                                  :hash 0
                                                  :reqs nil))))
  (is (equalp "register int"
              (type-trace-string (make-clang-type :name "int"
                                                  :pointer nil
                                                  :array ""
                                                  :storage-class :register
                                                  :hash 0
                                                  :reqs nil)))))

(deftest type-from-trace-string-test ()
  (is (equalp (type-from-trace-string "int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array ""
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "*int")
              (make-clang-type :name "int"
                               :pointer t
                               :array ""
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "[]int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array "[]"
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "[5]int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array "[5]"
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "*[]int")
              (make-clang-type :name "int"
                               :pointer t
                               :array "[]"
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "*[5]int")
              (make-clang-type :name "int"
                               :pointer t
                               :array "[5]"
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "const int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array ""
                               :const t
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "volatile int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array ""
                               :volatile t
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "*restrict int")
              (make-clang-type :name "int"
                               :pointer t
                               :array ""
                               :restrict t
                               :storage-class :None
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "auto int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array ""
                               :storage-class :Auto
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "static int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array ""
                               :storage-class :Static
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "extern int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array ""
                               :storage-class :Extern
                               :hash 0
                               :reqs nil)))
  (is (equalp (type-from-trace-string "register int")
              (make-clang-type :name "int"
                               :pointer nil
                               :array ""
                               :storage-class :Register
                               :hash 0
                               :reqs nil))))

(deftest type-decl-string-test ()
  (is (equalp "int"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer nil
                                                 :array ""
                                                 :storage-class :None
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "int *"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer t
                                                 :array ""
                                                 :storage-class :None
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "const int"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer nil
                                                 :array ""
                                                 :const t
                                                 :storage-class :None
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "volatile int"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer nil
                                                 :array ""
                                                 :volatile t
                                                 :storage-class :None
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "restrict int"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer nil
                                                 :array ""
                                                 :restrict t
                                                 :storage-class :None
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "auto int"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer nil
                                                 :array ""
                                                 :storage-class :Auto
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "static int"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer nil
                                                 :array ""
                                                 :storage-class :Static
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "extern int"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer nil
                                                 :array ""
                                                 :storage-class :Extern
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "register int"
              (type-decl-string (make-clang-type :name "int"
                                                 :pointer nil
                                                 :array ""
                                                 :storage-class :Register
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "struct struct_type"
              (type-decl-string (make-clang-type :name "struct_type"
                                                 :pointer nil
                                                 :array ""
                                                 :storage-class :None
                                                 :decl "struct struct_type;"
                                                 :hash 0
                                                 :reqs nil))))
  (is (equalp "union union_type"
              (type-decl-string (make-clang-type :name "union_type"
                                                 :pointer nil
                                                 :array ""
                                                 :storage-class :None
                                                 :decl "union union_type;"
                                                 :hash 0
                                                 :reqs nil)))))
