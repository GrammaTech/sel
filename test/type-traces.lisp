;;;; type-traces.lisp --- Types and traces tests.
(defpackage :software-evolution-library/test/type-traces
  (:nicknames :sel/test/type-traces)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :software-evolution-library/stefil-plus
   :software-evolution-library
   :software-evolution-library/software/clang)
  (:export :test-type-traces))
(in-package :software-evolution-library/test/type-traces)
(in-readtable :curry-compose-reader-macros)
(defsuite test-type-traces "Types and traces tests." (clang-available-p))

(deftest type-trace-string-test ()
  (is (equalp "int"
              (nest (type-trace-string)
                    (make-instance 'ct+ :storage-class :none :type)
                    (make-instance 'clang-type :qual "int" :name "int"))))
  (is (equalp "*int"
              (nest (type-trace-string)
                    (make-instance 'ct+ :storage-class :none :type)
                    (make-instance 'clang-type
                      :qual "int *"
                      :modifiers +pointer+
                      :name "int"))))
  (is (equalp "[]int"
              (nest (type-trace-string)
                    (make-instance 'ct+ :storage-class :none :type)
                    (make-instance 'clang-type
                      :qual "int []"
                      :array "[]"
                      :name "int"))))
  (is (equalp "[5]int"
              (nest (type-trace-string)
                    (make-instance 'ct+ :storage-class :none :type)
                    (make-instance 'clang-type
                      :qual "int [5]"
                      :array "[5]"
                      :name "int"))))
  (is (equalp "*[]int"
              (nest (type-trace-string)
                    (make-instance 'ct+ :storage-class :none :type)
                    (make-instance 'clang-type
                      :qual "int *[]"
                      :array "[]"
                      :modifiers +pointer+
                      :name "int"))))
  (is (equalp "*[5]int"
              (nest (type-trace-string)
                    (make-instance 'ct+ :storage-class :none :type)
                    (make-instance 'clang-type
                      :qual "int *[5]"
                      :array "[5]"
                      :modifiers +pointer+
                      :name "int"))))
  (is (equalp "const int"
              (nest (type-trace-string)
                    (make-instance 'ct+ :storage-class :none :type)
                    (make-instance 'clang-type
                      :qual "const int"
                      :modifiers +const+
                      :name "int"))))
  (is (equalp "volatile int"
              (nest (type-trace-string)
                    (make-instance 'ct+ :storage-class :none :type)
                    (make-instance 'clang-type
                      :qual "volatile int"
                      :modifiers +volatile+
                      :name "int"))))
  (is (equalp "*restrict int"
              (nest (type-trace-string)
                    (make-instance 'ct+ :storage-class :none :type)
                    (make-instance 'clang-type
                      :qual "restrict int *"
                      :modifiers (logior +pointer+ +restrict+)
                      :name "int"))))
  (is (equalp "auto int"
              (nest (type-trace-string)
                    (make-instance 'ct+ :storage-class :auto :type)
                    (make-instance 'clang-type :qual "int" :name "int"))))
  (is (equalp "static int"
              (nest (type-trace-string)
                    (make-instance 'ct+ :storage-class :static :type)
                    (make-instance 'clang-type :qual "int" :name "int"))))
  (is (equalp "extern int"
              (nest (type-trace-string)
                    (make-instance 'ct+ :storage-class :extern :type)
                    (make-instance 'clang-type :qual "int" :name "int"))))
  (is (equalp "register int"
              (nest (type-trace-string)
                    (make-instance 'ct+ :storage-class :register :type)
                    (make-instance 'clang-type :qual "int" :name "int")))))

(deftest trace-string-to-type-alist-test ()
  (is (equalp (trace-string-to-type-alist "int")
              (list (cons :pointer nil)
                    (cons :array "")
                    (cons :const nil)
                    (cons :volatile nil)
                    (cons :restrict nil)
                    (cons :storage-class :none)
                    (cons :name "int"))))
  (is (equalp (trace-string-to-type-alist "*int")
              (list (cons :pointer t)
                    (cons :array "")
                    (cons :const nil)
                    (cons :volatile nil)
                    (cons :restrict nil)
                    (cons :storage-class :none)
                    (cons :name "int"))))
  (is (equalp (trace-string-to-type-alist "[]int")
              (list (cons :pointer nil)
                    (cons :array "[]")
                    (cons :const nil)
                    (cons :volatile nil)
                    (cons :restrict nil)
                    (cons :storage-class :none)
                    (cons :name "int"))))
  (is (equalp (trace-string-to-type-alist "[5]int")
              (list (cons :pointer nil)
                    (cons :array "[5]")
                    (cons :const nil)
                    (cons :volatile nil)
                    (cons :restrict nil)
                    (cons :storage-class :none)
                    (cons :name "int"))))
  (is (equalp (trace-string-to-type-alist "*[]int")
              (list (cons :pointer t)
                    (cons :array "[]")
                    (cons :const nil)
                    (cons :volatile nil)
                    (cons :restrict nil)
                    (cons :storage-class :none)
                    (cons :name "int"))))
  (is (equalp (trace-string-to-type-alist "*[5]int")
              (list (cons :pointer t)
                    (cons :array "[5]")
                    (cons :const nil)
                    (cons :volatile nil)
                    (cons :restrict nil)
                    (cons :storage-class :none)
                    (cons :name "int"))))
  (is (equalp (trace-string-to-type-alist "const int")
              (list (cons :pointer nil)
                    (cons :array "")
                    (cons :const t)
                    (cons :volatile nil)
                    (cons :restrict nil)
                    (cons :storage-class :none)
                    (cons :name "int"))))
  (is (equalp (trace-string-to-type-alist "volatile int")
              (list (cons :pointer nil)
                    (cons :array "")
                    (cons :const nil)
                    (cons :volatile t)
                    (cons :restrict nil)
                    (cons :storage-class :none)
                    (cons :name "int"))))
  (is (equalp (trace-string-to-type-alist "*restrict int")
              (list (cons :pointer t)
                    (cons :array "")
                    (cons :const nil)
                    (cons :volatile nil)
                    (cons :restrict t)
                    (cons :storage-class :none)
                    (cons :name "int"))))
  (is (equalp (trace-string-to-type-alist "auto int")
              (list (cons :pointer nil)
                    (cons :array "")
                    (cons :const nil)
                    (cons :volatile nil)
                    (cons :restrict nil)
                    (cons :storage-class :auto)
                    (cons :name "int"))))
  (is (equalp (trace-string-to-type-alist "static int")
              (list (cons :pointer nil)
                    (cons :array "")
                    (cons :const nil)
                    (cons :volatile nil)
                    (cons :restrict nil)
                    (cons :storage-class :static)
                    (cons :name "int"))))
  (is (equalp (trace-string-to-type-alist "extern int")
              (list (cons :pointer nil)
                    (cons :array "")
                    (cons :const nil)
                    (cons :volatile nil)
                    (cons :restrict nil)
                    (cons :storage-class :extern)
                    (cons :name "int"))))
  (is (equalp (trace-string-to-type-alist "register int")
              (list (cons :pointer nil)
                    (cons :array "")
                    (cons :const nil)
                    (cons :volatile nil)
                    (cons :restrict nil)
                    (cons :storage-class :register)
                    (cons :name "int")))))

(deftest type-decl-string-test ()
  (is (equalp "int"
              (nest (type-decl-string)
                    (make-instance 'ct+ :storage-class :none :type)
                    (make-instance 'clang-type :qual "int"))))
  (is (equalp "int *"
              (nest (type-decl-string)
                    (make-instance 'ct+ :storage-class :none :type)
                    (make-instance 'clang-type
                      :qual "int *"
                      :modifiers +pointer+))))
  (is (equalp "const int"
              (nest (type-decl-string)
                    (make-instance 'ct+ :storage-class :none :type)
                    (make-instance 'clang-type
                      :qual "const int"
                      :modifiers +const+))))
  (is (equalp "volatile int"
              (nest (type-decl-string)
                    (make-instance 'ct+ :storage-class :none :type)
                    (make-instance 'clang-type
                      :qual "volatile int"
                      :modifiers +volatile+))))
  (is (equalp "restrict int"
              (nest (type-decl-string)
                    (make-instance 'ct+ :storage-class :none :type)
                    (make-instance 'clang-type
                      :qual "restrict int"
                      :modifiers +restrict+))))
  (is (equalp "auto int"
              (nest (type-decl-string)
                    (make-instance 'ct+ :storage-class :auto :type)
                    (make-instance 'clang-type :qual "int"))))
  (is (equalp "static int"
              (nest (type-decl-string)
                    (make-instance 'ct+ :storage-class :static :type)
                    (make-instance 'clang-type :qual "int"))))
  (is (equalp "extern int"
              (nest (type-decl-string)
                    (make-instance 'ct+ :storage-class :extern :type)
                    (make-instance 'clang-type :qual "int"))))
  (is (equalp "register int"
              (nest (type-decl-string)
                    (make-instance 'ct+ :storage-class :register :type)
                    (make-instance 'clang-type :qual "int"))))
  (is (equalp "struct struct_type"
              (nest (type-decl-string)
                    (make-instance 'ct+ :storage-class :none :type)
                    (make-instance 'clang-type :qual "struct struct_type"))))
  (is (equalp "union union_type"
              (nest (type-decl-string)
                    (make-instance 'ct+ :storage-class :none :type)
                    (make-instance 'clang-type :qual "union union_type")))))
