;;;; adaptive-mutation.lisp --- Adaptive-mutation tests.
(defpackage :software-evolution-library/test/adaptive-mutation
  (:nicknames :sel/test/adaptive-mutation)
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
  (:export :adaptive-mutation))
(in-package :software-evolution-library/test/adaptive-mutation)
(in-readtable :curry-compose-reader-macros)
(defsuite adaptive-mutation)

(deftest bad-cut-changes-mutation-probability ()
  (let* ((*mutation-results-queue* #((cut . :worse) (cut . :dead)))
         (muts-0 '((cut . 1/2) (swap . 1)))
         (muts-1 (update-mutation-types muts-0))
         (muts-2 (update-mutation-types muts-1)))
    (is (< (aget 'cut muts-1) (aget 'cut muts-0))
        "Bad mutations lose probability.")
    (is (< (aget 'cut muts-2) (aget 'cut muts-1))
        "Bad mutations continue to lose probability.")))

(deftest mutation-queue-wraps-as-expected ()
  (let ((*mutation-results-queue*
         (make-array 100
                     :element-type '(cons symbol symbol)
                     :initial-element (cons :nothing :nothing)))
        (sel/sw/adaptive-mutation::*mutation-results-queue-next* 0))
    (dotimes (n 100)
      (queue-mutation 'cut :dead))
    (is (every [{equal 'cut} #'car] *mutation-results-queue*)
        "`queue-mutation' fills `*mutation-results-queue*' as expected.")
    (queue-mutation 'swap :better)
    (is (equalp (cons 'swap :better) (aref *mutation-results-queue* 0))
        "`queue-mutation' wraps `*mutation-results-queue*' as expected.")))

(deftest update-mutation-types-returns-list-when-mutation-queue-unpopulated ()
  "Ensure update-mutation-types returns its first argument when the
*mutation-results-queue* is unpopulated"
  (let ((*mutation-results-queue*
         (copy-seq +initial-mutation-results-queue+))
        (sel/sw/adaptive-mutation::*mutation-results-queue-next* 0)
        (mutation-types (copy-seq *clang-mutation-types*)))
    (is (equalp mutation-types
                (update-mutation-types mutation-types)))))

(deftest update-mutation-types-returns-list-when-mutation-queue-populated ()
  "Ensure update-mutation-types returns a list when the
*mutation-results-queue* is populated"
  (let ((*mutation-results-queue*
         (copy-seq +initial-mutation-results-queue+))
        (sel/sw/adaptive-mutation::*mutation-results-queue-next* 0)
        (mutation-types (copy-seq *clang-mutation-types*)))
    (dotimes (n (length +initial-mutation-results-queue+))
      (queue-mutation 'cut :dead))
    (is (listp (update-mutation-types mutation-types)))))

(deftest adaptive-analyze-mutation-updates-results-queue-properly ()
  (let ((*fitness-predicate* #'<)
        (sel/sw/adaptive-mutation::*mutation-results-queue-next* 0)
        (*mutation-results-queue*
         (copy-seq +initial-mutation-results-queue+))
        (parent-a (make-clang :fitness 2))
        (parent-b (make-clang :fitness 2))
        (crossed  (make-clang :fitness 1))
        (mutant   (make-clang :fitness 0)))
    (adaptive-analyze-mutation mutant
                               `(clang-cut ,parent-a 0
                                           ,crossed ,parent-b 0)
                               {fitness})
    (is (equal :better (cdr (aref *mutation-results-queue* 0))))))
