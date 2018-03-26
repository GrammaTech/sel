#||
*******************************************************************************
*
* This software is furnished under a license and/or other restrictive terms and may
* be used and copied only in accordance with such terms and the inclusion of the
* below copyright notice. This software or any other copies thereof may not be
* provided or otherwise made available to any other person without the express
* written consent of an authorized reprsentative of GrammaTech, Inc. Title to,
* ownership of, and all rights in the software is retained by GrammaTech, Inc.
*
* Any reproduction of the software or portions thereof marked with this legend
* must also reproduce the following markings.
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* Distribution Statement B: Distribution authorized to U.S. Government agencies only;
* Export Controlled and Proprietary Information: DFARS SBIR Data Rights, 09/15/2017.
* Other requests for this document shall be referred to the DoD Controlling
* Office or the DoD SBIR Program Office.
*
* SBIR Data Rights
*
* Contract Number: D17PC00096
* Contractor Name: GrammaTech, Inc.
* Contractor Address: 531 Esty Street, Ithaca, NY  14850
* Expiration Date: April 30, 2024
*
* The Governmentís rights to use, modify, reproduce, release, perform, display, or disclose
* technical data or computer software contained in this report are restricted by paragraph
* (b)(4) of the Rights in Noncommercial Technical Data and Computer Software - Small Business
* Innovation Research (SBIR) Program clause (DFARS 252.227-7018 (FEB 2014)) contained in the
* above identified contract. No restrictions apply after the expiration date shown above. Any
* reproduction of technical data, computer software, or portions thereof marked with this
* legend must also reproduce the markings.
*
* (c) GrammaTech, Inc. 2018.  All other rights reserved.
*
*******************************************************************************
||#
;;; lisp.lisp --- software representation of lisp code
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;; Tree actions
(defun tree-size (tree)
  "Return the number of cons cells in TREE."
  (if (and tree (consp tree))
      (+ 1 (tree-size (car tree)) (tree-size (cdr tree)))
      0))

(defun subtree (tree index)
  "Return the INDEX cons cell in TREE in depth first order."
  (if (zerop index)
      (values tree index)
      (flet ((descend (branch)
               (when (consp branch)
                 (multiple-value-bind (new-tree new-index)
                     (subtree branch (1- index))
                   (if (= new-index 0)
                       (return-from subtree (values new-tree new-index))
                       (setf index new-index))))))
        (descend (car tree))
        (descend (cdr tree))
        (values nil index))))

(defun find-subtree-if (predicate tree)
  "DOCFIXME

* PREDICATE DOCFIXME
* TREE DOCFIXME
"
  (when (and tree (listp tree))
    (if (funcall predicate tree)
        tree
        (or (find-subtree-if predicate (car tree))
            (find-subtree-if predicate (cdr tree))))))

(defmacro set-subtree (tree index new)
  "Feels like some heuristics here (why `rplaca' instead of `rplacd')."
  `(if (zerop ,index)
       (setf ,tree ,new)
       (rplaca (subtree ,tree ,index) ,new)))

(defsetf subtree set-subtree)


;;; Lisp software object
(define-software lisp (simple)
  ((genome :initarg :genome :accessor genome :initform nil :copier copy-tree))
  (:documentation "Common Lisp source represented naturally as lists of code."))

(defmethod from-file ((lisp lisp) file)
  "DOCFIXME

* LISP DOCFIXME
* FILE DOCFIXME
"
  (with-open-file (in file)
    (setf (genome lisp)
          (loop :for form := (read in nil :eof)
             :until (eq form :eof)
             :collect form)))
  lisp)

(declaim (inline genome-string))
(defmethod genome-string ((lisp lisp) &optional stream)
  "DOCFIXME

* LISP DOCFIXME
* STREAM DOCFIXME
"
  (format stream "~&~{~S~^~%~}~%" (genome lisp)))

(defmethod to-file ((lisp lisp) path)
  "DOCFIXME

* LISP DOCFIXME
* PATH DOCFIXME
"
  (with-open-file (out path :direction :output :if-exists :supersede)
    (genome-string lisp out)))

(defmethod size ((lisp lisp))
  "DOCFIXME"
  (tree-size (genome lisp)))

(defmethod filter-subtrees (predicate (lisp lisp))
  "DOCFIXME

* PREDICATE DOCFIXME
* LISP DOCFIXME
"
  (remove-if-not [predicate {subtree (genome lisp)}]
                 (iter (for i below (size lisp)) (collect i))))


;;; Mutations
(define-mutation lisp-cut (mutation)
  ((targeter :initform #'pick-bad))
  (:documentation "DOCFIXME"))

(define-mutation lisp-replace (mutation)
  ((targeter :initform #'pick-bad-good))
  (:documentation "DOCFIXME"))

(define-mutation lisp-swap (mutation)
  ((targeter :initform #'pick-bad-good))
  (:documentation "DOCFIXME"))

(defvar *lisp-mutation-types*
  ;; TODO: Fix `lisp-cut' before adding back to this list.
  '(lisp-replace lisp-swap)
  "DOCFIXME")

(defmethod pick-mutation-type ((obj lisp))
  "DOCFIXME"
  (random-elt *lisp-mutation-types*))

(defmethod mutate ((lisp lisp))
  "DOCFIXME"
  (unless (> (size lisp) 0)
    (error (make-condition 'mutate :text "No valid IDs" :obj lisp)))
  (let ((mutation (make-instance (pick-mutation-type lisp)
                                 :object lisp)))
    (apply-mutation lisp mutation)
    (values lisp mutation)))

(defmethod apply-mutation ((lisp lisp) (mutation lisp-cut))
  "DOCFIXME

* LISP DOCFIXME
* MUTATION DOCFIXME
"
  ;; TODO: Fix.
  (with-slots (genome) lisp
    (let* ((s1 (targets mutation))
           (st (subtree genome s1)))
      (let ((prev (find-subtree-if [{eq st} #'cdr] genome)))
        (if prev
            ;; Middle of a subtree: snap cdr to remove
            (setf (cdr prev) (cdr st))
            ;; Beginning of a subtree.
            (setf (subtree genome s1)
                  (copy-tree (cdr (subtree genome s1))))))))
  lisp)

(defmethod apply-mutation ((lisp lisp) (mutation lisp-replace))
  "DOCFIXME

* LISP DOCFIXME
* MUTATION DOCFIXME
"
  (bind (((s1 s2) (targets mutation)))
    (with-slots (genome) lisp
      (setf (subtree genome s1)
            (copy-tree (car (subtree genome s2))))))
  lisp)

(defmethod apply-mutation ((lisp lisp) (mutation lisp-swap))
  "DOCFIXME

* LISP DOCFIXME
* MUTATION DOCFIXME
"
  (bind (((s1 s2) (targets mutation)))
    (let ((s1 (max s1 s2))
          (s2 (min s1 s2)))
      (with-slots (genome) lisp
        (let ((left  (car (subtree genome s1)))
              (right (car (subtree genome s2))))
          (setf (subtree genome s1) (copy-tree right))
          (setf (subtree genome s2) (copy-tree left))))))
  lisp)

(defmethod crossover ((a lisp) (b lisp))
  "DOCFIXME

* A DOCFIXME
* B DOCFIXME
"
  (let ((range (min (size a) (size b))))
    (if (> range 0)
        (let ((points (sort (loop :for i :below 2 :collect (random range)) #'<))
              (new (copy a)))
          (setf (subtree (genome new) (first points))
                (copy-tree (subtree (genome b) (second points))))
          (values new points))
        (values (copy a) nil))))

#||
*******************************************************************************
*
* This software is furnished under a license and/or other restrictive terms and may
* be used and copied only in accordance with such terms and the inclusion of the
* below copyright notice. This software or any other copies thereof may not be
* provided or otherwise made available to any other person without the express
* written consent of an authorized reprsentative of GrammaTech, Inc. Title to,
* ownership of, and all rights in the software is retained by GrammaTech, Inc.
*
* Any reproduction of the software or portions thereof marked with this legend
* must also reproduce the following markings.
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* Distribution Statement B: Distribution authorized to U.S. Government agencies only;
* Export Controlled and Proprietary Information: DFARS SBIR Data Rights, 09/15/2017.
* Other requests for this document shall be referred to the DoD Controlling
* Office or the DoD SBIR Program Office.
*
* SBIR Data Rights
*
* Contract Number: D17PC00096
* Contractor Name: GrammaTech, Inc.
* Contractor Address: 531 Esty Street, Ithaca, NY  14850
* Expiration Date: April 30, 2024
*
* The Governmentís rights to use, modify, reproduce, release, perform, display, or disclose
* technical data or computer software contained in this report are restricted by paragraph
* (b)(4) of the Rights in Noncommercial Technical Data and Computer Software - Small Business
* Innovation Research (SBIR) Program clause (DFARS 252.227-7018 (FEB 2014)) contained in the
* above identified contract. No restrictions apply after the expiration date shown above. Any
* reproduction of technical data, computer software, or portions thereof marked with this
* legend must also reproduce the markings.
*
* (c) GrammaTech, Inc. 2018.  All other rights reserved.
*
*******************************************************************************
||#
