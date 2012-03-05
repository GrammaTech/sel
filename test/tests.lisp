;;; tests.lisp --- tests for the `software-evolution' package

;; Copyright (C) 2011  Eric Schulte

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:
(in-package :software-evolution)
(require :stefil)
(use-package :stefil)
(defsuite software-evolution-test)
(in-suite software-evolution-test)
(defvar *genome*  nil "Genome used in tests.")
(defvar *soft*    nil "Software used in tests.")
(defvar *gcd*     nil "Holds the gcd software object.")
(defvar *gcd-dir* "test/gcd" "Location of the gcd example directory")
(defun gcd-dir (filename)
  (concatenate 'string *gcd-dir* "/" filename))

(defixture vector-genome
  (:setup (setf *genome* (coerce (loop for i from 0 to 9 collect i) 'vector)))
  (:teardown))

(defixture soft
  (:setup (setf *soft* (make-instance 'software
                         :genome (coerce (loop for i from 0 to 9 collect i)
                                         'vector))))
  (:teardown))

(defixture tree-genome
  (:setup (setf *genome* (to-tree '(1 2 3 (4 5) 6))))
  (:teardown))

(defixture gcd-asm
  (:setup (setf *gcd* (asm-from-file (gcd-dir "gcd.s"))))
  (:teardown))

(defixture gcd-lisp
  (:setup (setf *gcd* (lisp-from-file (gcd-dir "gcd.lisp"))))
  (:teardown))

(defixture population
  (:setup (setf *population* (loop for i from 1 to 9
                                collect (make-instance 'software
                                          :genome (loop for j from 0 to i
                                                     collect j)
                                          :fitness i))))
  (:teardown (setf *population* nil)))


;;; vector genome
(deftest ind-vector ()
  (with-fixture vector-genome
    (is (= 1 (ind *genome* 1)))))

(deftest inds-vector ()
  (with-fixture vector-genome
    (is (equal-it (inds *genome*) (coerce *genome* 'list)))))

(deftest setf-ind-vector ()
  (with-fixture vector-genome
    (setf (ind *genome* 1) :foo)
    (is (equal-it *genome* #(0 :FOO 2 3 4 5 6 7 8 9)))))

(deftest cut-vector ()
  (with-fixture vector-genome
    (is (= 9 (length (cut *genome*))))))

(deftest insert-vector ()
  (with-fixture vector-genome
    (is (= 11 (length (insert *genome*))))
    (is (= 10 (length (remove-duplicates (insert *genome*)))))))

(deftest swap-vector ()
  (with-fixture vector-genome
    (is (= 10 (length (swap *genome*))))))

(deftest copy-soft ()
  (with-fixture soft
    (let ((new (copy *soft*)))
      (is (equal-it (genome new) (genome *soft*)))
      (cut new)
      (is (< (length (genome new))
             (length (genome *soft*)))))))

(deftest edit-same-is-zero ()
  (with-fixture soft
    (is (zerop (edit-distance *soft* *soft*)))))


;;; tree genome
(deftest list-to-tree ()
  (with-fixture tree-genome
    (is (equal-it (to-tree '(1 2 3 (4 5) 6))
                  *genome*))))

(deftest tree-to-list-conversion ()
  (with-fixture tree-genome
    (is (equal-it (to-list (to-tree *genome*))
                  *genome*))))

(deftest ind-tree ()
  (with-fixture tree-genome
    (is (equal-it (ind *genome* 3) (to-tree '(4 5))))))

(deftest inds-tree ()
  (with-fixture tree-genome
    (is (equal-it (inds *genome*) '(0 1 2 3 4 5)))))

(deftest setf-ind-tree ()
  (with-fixture tree-genome
    (is (equal (setf (ind *genome* 2) (make-tree :data :foo))
               (ind *genome* 2)))))


;;; List genome
(deftest simple-inds-on-lisp-genome ()
  (is (= 9 (length (inds '(1 2 3 4))))))

(deftest get-inds-on-list ()
  (is (= 2 (ind '(1 2 3 4) '(:d :a)))))

(deftest setf-inds-on-list ()
  (let ((genome '(1 2 3 4 5)))
    (setf (ind genome '(:d :d :a)) 9)
    (is (equal-it '(1 2 9 4 5) genome))))

(deftest another-setf-inds-on-list ()
  (let ((genome '(1 2 3 4 5)))
    (setf (ind genome '(:d :d :a)) '(1 2 3))
    (is (equal-it '(1 2 (1 2 3) 4 5) genome))))

(deftest del-ind-on-list ()
  (is (equal-it '(1 2 4)
                (let ((genome (list 1 2 3 4)))
                  (del-ind genome '(:d :d :a))
                  genome)))
  (is (equal-it '(2 3 4)
                (let ((genome (list 1 2 3 4)))
                  (del-ind genome '(:a))
                  genome))))

(deftest maintain-proper-list-setf ()
  (is (proper-list-p (let ((it '(1 2)))
                       (setf (ind it '(:d)) :foo)
                       it))))

(deftest maintain-proper-list-del-ind ()
  (is (proper-list-p (let ((it '(1 2)))
                       (del-ind it '(:d))
                       it))))


;;; ASM representation
(deftest simple-read ()
  (with-fixture gcd-asm
    (is (equal 'asm (type-of *gcd*)))))

(deftest idempotent-read-write ()
  (let ((a (temp-file-name)))
    (unwind-protect
         (with-fixture gcd-asm
           (asm-to-file *gcd* a)
           (multiple-value-bind (out err ret)
               (shell "diff ~s/gcd.s ~a" *gcd-dir* a)
             (declare (ignorable out err))
             (is (= 0 ret))))
      (delete-file a))))

(deftest idempotent-copy ()
  (with-fixture gcd-asm
   (is (equal-it *gcd* (copy *gcd*)))))

(deftest idempotent-read-copy-write ()
  (let ((a (temp-file-name)))
    (unwind-protect
         (with-fixture gcd-asm
           (asm-to-file (copy *gcd*) a)
           (multiple-value-bind (out err ret)
               (shell "diff ~s/gcd.s ~a" *gcd-dir* a)
             (declare (ignorable out err))
             (is (= 0 ret))))
      (delete-file a))))

(deftest simple-fitness ()
  (let ((*pos-test-num* 5)
        (*neg-test-num* 1)
        (*test-script* (gcd-dir "test.sh")))
    (with-fixture gcd-asm
      (is (= 5 (fitness *gcd*)))
      (is (= 5 (fitness (copy *gcd*)))))))

(deftest edit-of-copy-does-not-change-original ()
  (with-fixture gcd-asm
    (let ((orig-hash (sxhash (genome *gcd*)))
          (ant (copy *gcd*)))
      (mutate ant)
      (is (not (equal-it (genome ant) (genome *gcd*))))
      (is (equal orig-hash (sxhash (genome *gcd*)))))))

(deftest edit-of-different-is-more-than-zero ()
  (with-fixture gcd-asm
    (let ((ant (copy *gcd*)))
      (mutate ant)
      (is (> (edit-distance ant *gcd*) 0)))))


;;; Lisp representation
(deftest simple-read-lisp-from-file ()
  (with-fixture gcd-lisp
    (is (eq 'defun (caar (genome *gcd*))))))

(deftest idempotent-read-write-lisp ()
  (let ((a (temp-file-name)))
    (unwind-protect
         (with-fixture gcd-lisp
           (lisp-to-file *gcd* a)
           (multiple-value-bind (out err ret)
               (shell "tail -8 ~s/gcd.lisp |diff -wB ~a -" *gcd-dir* a)
             (declare (ignorable out err))
             (is (= 0 ret))))
      (delete-file a))))

(deftest cut-on-list ()
  (with-fixture gcd-lisp
    (is (> (length (inds (genome *gcd*)))
           (progn (cut *gcd*)
                  (length (inds (genome *gcd*))))))))

(deftest insert-on-list ()
  (with-fixture gcd-lisp
    (is (< (length (inds (genome *gcd*)))
           (progn (insert *gcd*)
                  (length (inds (genome *gcd*))))))))

(deftest swap-on-list ()
  (with-fixture gcd-lisp
    (is (not (equal-it (history *gcd*)
                       (progn (swap *gcd*)
                              (history *gcd*)))))))

(deftest crossover-on-list ()
  (with-fixture gcd-lisp
    (is (equal-it (genome *gcd*)
                  (genome (crossover *gcd* *gcd*))))))

(deftest evaluate-lisp-program ()
  (with-fixture gcd-lisp
    (let ((*test-script* (gcd-dir "test-lisp.sh"))
          (*pos-test-num* 10)
          (*neg-test-num* 1))
      (is (= 10 (evaluate *gcd*))))))


;;; Population tests
(deftest evict-population ()
  (with-fixture population
    (let ((before (length *population*)))
      (is (> before (length (progn (evict) *population*)))))))

(deftest incorporate-population ()
  (with-fixture population
    (let* ((before (length *population*))
           (*max-population-size* (+ 1 before)))
      (is (< before (length (progn (incorporate (make-instance 'software))
                                   *population*)))))))
