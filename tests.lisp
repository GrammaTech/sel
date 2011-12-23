;;; tests.lisp --- tests for the `soft-ev' package

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
(in-package :soft-ev)
(require :stefil)
(use-package :stefil)
(defsuite soft-ev-test)
(in-suite soft-ev-test)
(defvar *genome* nil "Genome used in tests.")
(defvar *soft*   nil "Software used in tests.")


;;; list genome
(defixture list-genome
  (:setup (setf *genome* (loop for i from 0 to 9 collect i)))
  (:teardown))

(defixture soft
  (:setup (setf *soft* (make-instance 'soft
                         :genome (loop for i from 0 to 9 collect i))))
  (:teardown))

(deftest ind-list ()
  (with-fixture list-genome
    (is (= 1 (ind *genome* 1)))))

(deftest inds-list ()
  (with-fixture list-genome
    (is (equal-it (inds *genome*) *genome*))))

(deftest setf-ind-list ()
  (with-fixture list-genome
    (setf (ind *genome* 1) :foo)
    (is (equal-it *genome* '(0 :FOO 2 3 4 5 6 7 8 9)))))

(deftest cut-list ()
  (with-fixture list-genome
    (is (= 9 (length (cut *genome*))))))

(deftest insert-list ()
  (with-fixture list-genome
    (is (= 11 (length (insert *genome*))))
    (is (= 10 (length (remove-duplicates (insert *genome*)))))))

(deftest swap-list ()
  (with-fixture list-genome
    (is (= 10 (length (swap *genome*))))))

(deftest copy-soft ()
  (with-fixture soft
    (let ((new (copy *soft*)))
      (is (equal-it (genome new) (genome *soft*)))
      (cut new)
      (is (< (length (genome new))
             (length (genome *soft*)))))))


;;; tree genome
(defixture tree-genome
  (:setup (setf *genome* (to-tree '(1 2 3 (4 5) 6))))
  (:teardown))

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


;;; ASM representation
(defvar *gcd* nil)

(defixture gcd-soft
  (:setup (setf *gcd* (asm-from-file "gcd.s")))
  (:teardown))

(deftest simple-read ()
  (with-fixture gcd-soft
    (is (equal 'soft-asm (type-of *gcd*)))))

(deftest idempotent-read-write ()
  (let ((a (temp-file-name)))
    (with-fixture gcd-soft
      (asm-to-file *gcd* a)
      (multiple-value-bind (out err ret) (shell "diff gcd.s ~a" a)
        (declare (ignorable out err))
        (is (= 0 ret))))))

(deftest idempotent-copy ()
  (with-fixture gcd-soft
   (is (equal-it *gcd* (copy *gcd*)))))

(deftest idempotent-read-copy-write ()
  (with-fixture gcd-soft
    (let ((a (temp-file-name)))
      (asm-to-file (copy *gcd*) a)
      (multiple-value-bind (out err ret) (shell "diff gcd.s ~a" a)
        (declare (ignorable out err))
        (is (= 0 ret))))))

(deftest simple-fitness ()
  (let ((*pos-test-num* 5)
        (*neg-test-num* 1)
        (*test-script* "./test.sh"))
    (with-fixture gcd-soft
      (is (= 5 (fitness *gcd*)))
      (is (= 5 (fitness (copy *gcd*)))))))


;;; Population tests
(defixture population
  (:setup (setf *population* (loop for i from 1 to 9
                                collect (make-instance 'soft
                                          :genome (loop for j from 0 to i
                                                     collect j)
                                          :fitness i))))
  (:teardown))

(deftest evict-population ()
  (with-fixture population
    (let ((before (length *population*)))
      (is (> before (length (progn (evict) *population*)))))))

(deftest incorporate-population ()
  (with-fixture population
    (let* ((before (length *population*))
           (*max-population-size* (+ 1 before)))
      (is (< before (length (progn (incorporate (make-instance 'soft))
                                   *population*)))))))
