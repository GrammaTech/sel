;;; elf-x86.lisp --- software representation of x86 ELF files

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary:

;;; Code:
(in-package :software-evolution)


;;; elf software objects
(defclass elf-x86 (elf)
  ((addresses :initarg :addresses :accessor addresses :initform nil)))

(defvar x86-nop #x90)

(defmethod elf ((elf elf-x86))
  (let ((new (copy-elf (base elf))))
    (setf (data (named-section new ".text"))
          (coerce (mapcan [#'cdr {assoc :bytes}] (copy-tree (genome elf)))
                  'vector))
    new))

(defun by-instruction (section &optional objdump)
  (let* ((objdump (or objdump (objdump-parse (objdump section))))
         (data (data section))
         (offsets (mapcar [{- _ (address (sh section))} #'car]
                          (mapcan #'cdr objdump))))
    (mapcar (lambda (start end) (coerce (subseq data start end) 'list))
            offsets
            (append (cdr offsets) (list nil)))))

(defmethod from-file ((elf elf-x86) path)
  (setf (base elf) (read-elf path))
  (let* ((text (named-section (base elf) ".text"))
         (objdump (objdump-parse (objdump text))))
    (setf (genome elf) (mapcar [#'list {cons :bytes}]
                               (by-instruction text objdump)))
    (setf (addresses elf) (mapcar #'car (mapcan #'cdr (copy-tree objdump)))))
  elf)

(defmethod apply-mutation ((elf elf-x86) mut)
  (flet ((byte-count (genome)
           (reduce #'+ (mapcar [#'length {aget :bytes}] genome))))
    (let ((starting-bytes (byte-count (genome elf))))
      (setf (genome elf)
            (case (car mut)
              (:cut    (elf-cut elf (second mut)))
              (:insert (elf-insert elf (second mut)
                                   (nth (third mut) (genome elf))))
              (:swap   (elf-swap elf (second mut) (third mut)))))
      (unless (= (byte-count (genome elf)) starting-bytes)
        (error 'mutate
               :text
               (format nil "mutation ~S changed size of genome [~S -> ~S]"
                       mut starting-bytes (byte-count (genome elf)))
               :obj elf)))))

(defun elf-padd (genome place num-bytes flags)
  (let ((base (cons (list :bytes x86-nop) (remove :bytes flags :key #'car))))
    (append (subseq genome 0 place)
            (loop :for i :below num-bytes :collect (copy-tree base))
            (subseq genome place))))

(defun elf-strip (genome place num-bytes)
  (let ((length (length genome)))
    (flet ((nop-p (n genome)
             (tree-equal (list x86-nop) (aget :bytes (nth n genome))))
           (del (n)
             (decf num-bytes) (decf length)
             (setf genome (delete-if (constantly t) genome :start n :count 1))))
      (loop :for i :upto (max place (- length place)) :while (> num-bytes 0)
         :do
         (let ((f  (+ place i)) (b (- place i)))
           (loop :while (and (> num-bytes 0) (< f length) (nop-p f genome))
              :do (del f))
           (loop :while (and (> num-bytes 0) (> b 0) (nop-p b genome))
              :do (del b))))
      (values genome num-bytes))))

(defun elf-replace (elf s1 value)
  (with-slots (genome) elf
    (let* ((prev (nth s1 genome))
           (out-bytes (length (aget :bytes prev)))
           (in-bytes (length (aget :bytes value))))
      (assert (assoc :bytes prev) (prev)
              "attempt to replace genome element with no bytes: ~S" prev)
      (let ((genome (append (subseq genome 0 s1)
                            (list value)
                            (subseq genome (1+ s1)))))
        (if (> out-bytes in-bytes)
            (values (elf-padd genome s1 (- out-bytes in-bytes) prev) 0)
            (multiple-value-call #'values
              (elf-strip genome s1 (- in-bytes out-bytes))))))))

(defmethod elf-cut ((elf elf-x86) s1)
  (with-slots (genome) elf
    (let ((prev (nth s1 genome)))
      (assert (assoc :bytes prev) (prev)
              "attempt to cut genome element with no bytes: ~S" prev)
      (elf-padd (append (subseq genome 0 s1) (subseq genome (1+ s1)))
                s1 (length (aget :bytes prev))
                (remove :bytes (copy-tree prev) :key #'car)))))

(defmethod elf-insert ((elf elf-x86) s1 val)
  (with-slots (genome) elf
    (assert (assoc :bytes val) (val)
            "attempt to insert genome element with no bytes: ~S" val)
    (setf genome (append (subseq genome 0 s1) (list val) (subseq genome s1)))
    (elf-strip genome s1 (length (aget :bytes val)))))

(defmethod elf-swap ((elf elf-x86) s1 s2)
  (assert (every {assoc :bytes} (mapcar {nth _ (genome elf)} (list s1 s2)))
          (s1 s2) "attempt to swap genome elements w/o bytes: ~S" (cons s1 s2))
  (unless (= s1 s2)
    ;; drop in a place holders marking what we want to change
    (push (list :s1) (nth s1 (genome elf)))
    (push (list :s2) (nth s2 (genome elf)))
    ;; remove any leftover bytes
    (mapc
     (lambda-bind ((place . num-bytes))
       (setf (genome elf) (elf-strip (genome elf) place num-bytes)))
     (mapcar (lambda-bind ((place . value))
               (let ((point (position-if {assoc place} (genome elf))))
                 ;; clean out placeholder
                 (setf (nth point (genome elf))
                       (remove place (nth point (genome elf)) :key #'car))
                 ;; perform replacement
                 (multiple-value-bind (genome left) (elf-replace elf point value)
                   (setf (genome elf) genome)
                   ;; pass along any extra bytes for later removal
                   (cons point (or left 0)))))
             ;; both markers with their values, sorted to operate on
             ;; the smaller (by byte width) instruction first
             (sort (mapcar #'cons
                           (list :s1 :s2)
                           (mapcar [#'cdr #'copy-tree {nth _ (genome elf)}]
                                   (list s2 s1)))
                   #'< :key [#'length #'cdr]))))
  (genome elf))

(defmethod crossover ((a elf-x86) (b elf-x86))
  "One point crossover."
  (flet ((borders (elf)
           (let ((counter 0))
             (cdr (reverse (reduce (lambda (ac el) (cons (cons (+ el (caar ac))
                                                               (incf counter))
                                                         ac))
                                   (mapcar #'length (genome elf))
                                   :initial-value '((0))))))))
    (let ((point (random-elt (mapcar #'cdr (intersection (borders a) (borders b)
                                                         :key #'car))))
          (new (copy a)))
      (setf (genome new) (append (subseq (genome a) 0 point)
                                 (subseq (genome b) point)))
      new)))

(defmethod apply-path ((elf elf-x86) key addresses &aux applied)
  (loop :for el :in addresses :as i :from 0 :do
     (let* ((addr  (if (consp el) (car el) el))
            (val   (if (consp el) (cdr el) t))
            (place (position addr (addresses elf))))
       (when place
         (push (cons key val) (nth place (genome elf)))
         (push (list i key val) applied))))
  (reverse applied))

(defmethod lines ((elf elf-x86))
  (map 'list {aget :bytes} (genome elf)))

(defmethod (setf lines) (new (elf elf-x86))
  (setf (genome elf) (coerce (map 'vector [#'list {cons :bytes}] new) 'list)))
