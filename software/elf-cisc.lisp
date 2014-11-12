;;; elf-cisc.lisp --- software representation of cisc ELF files

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary:

;;; Code:
(in-package :software-evolution)


;;; elf software objects
(defclass elf-cisc (elf)
  ((addresses :initarg :addresses :accessor addresses :initform nil)))

(defclass elf-csurf (elf-cisc)
  ((project :initarg :project :accessor project :initform nil)))

(defvar elf-cisc-nop #x90)

(defmethod elf ((elf elf-cisc))
  (let ((new (copy-elf (base elf))))
    (setf (data (named-section new ".text"))
          (coerce (mappend [#'cdr {assoc :code}] (genome elf)) 'vector))
    new))

(defun parse-disasm (elf section)
  (let ((disasm (disassemble-section (base elf) section)))
    (values
     (mapcar #'car disasm)
     (setf (genome elf) (mapcar (lambda-bind ((address bytes disasm))
                                  (declare (ignorable address))
                                  `((:code . ,bytes) (:disasm . ,disasm)))
                                disasm)))))

(defmethod from-file ((elf elf-cisc) path)
  (setf (base elf) (read-elf path 'objdump))
  (multiple-value-bind (addresses genome) (parse-disasm elf ".text")
    (setf (addresses elf) addresses)
    (setf (genome elf) genome))
  elf)

(defmethod from-file ((elf elf-csurf) path)
  (setf (base elf) (read-elf path 'csurf))
  (setf (project (base elf)) (project elf))
  (multiple-value-bind (addresses genome) (parse-disasm elf ".text")
    (setf (addresses elf) addresses)
    (setf (genome elf) genome))
  elf)

(defmethod apply-mutation ((elf elf-cisc) mut)
  (flet ((byte-count (genome)
           (reduce #'+ (mapcar [#'length {aget :code}] genome))))
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
  (let ((base (cons (list :code elf-cisc-nop) (remove :code flags :key #'car))))
    (append (subseq genome 0 place)
            (loop :for i :below num-bytes :collect (copy-tree base))
            (subseq genome place))))

(defun elf-strip (genome place num-bytes)
  (let ((length (length genome)))
    (flet ((nop-p (n genome)
             (tree-equal (list elf-cisc-nop) (aget :code (nth n genome))))
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

(defgeneric elf-replace (elf s1 value)
  (:documentation "Replace the contents of ELF at S1 with VALUE."))

(defmethod elf-replace ((elf elf) s1 value)
  (let ((genome (genome elf)))
    (let* ((prev (nth s1 genome))
           (out-bytes (length (aget :code prev)))
           (in-bytes (length (aget :code value))))
      (assert (assoc :code prev) (prev)
              "attempt to replace genome element with no bytes: ~S" prev)
      (let ((genome (append (subseq genome 0 s1)
                            (list value)
                            (subseq genome (1+ s1)))))
        (if (> out-bytes in-bytes)
            (values (elf-padd genome s1 (- out-bytes in-bytes) prev) 0)
            (multiple-value-call #'values
              (elf-strip genome s1 (- in-bytes out-bytes))))))))

(defmethod elf-cut ((elf elf-cisc) s1)
  (let ((genome (genome elf)))
    (let ((prev (nth s1 genome)))
      (assert (assoc :code prev) (prev)
              "attempt to cut genome element with no bytes: ~S" prev)
      (elf-padd (append (subseq genome 0 s1) (subseq genome (1+ s1)))
                s1 (length (aget :code prev))
                (remove :code (copy-tree prev) :key #'car)))))

(defmethod elf-insert ((elf elf-cisc) s1 val)
  (let ((genome (genome elf)))
    (assert (assoc :code val) (val)
            "attempt to insert genome element with no bytes: ~S" val)
    (setf (genome elf)
          (append (subseq genome 0 s1) (list val) (subseq genome s1)))
    (elf-strip (genome elf) s1 (length (aget :code val)))))

(defmethod elf-swap ((elf elf-cisc) s1 s2)
  (assert (every {assoc :code} (mapcar {nth _ (genome elf)} (list s1 s2)))
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

(defmethod crossover ((a elf-cisc) (b elf-cisc))
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

(defmethod apply-path ((elf elf-cisc) key addresses &aux applied)
  (loop :for el :in addresses :as i :from 0 :do
     (let* ((addr  (if (consp el) (car el) el))
            (val   (if (consp el) (cdr el) t))
            (place (position addr (addresses elf))))
       (when place
         (push (cons key val) (nth place (genome elf)))
         (push (list i key val) applied))))
  (reverse applied))

(defmethod lines ((elf elf-cisc))
  (map 'list {aget :code} (genome elf)))

(defmethod (setf lines) (new (elf elf-cisc))
  (setf (genome elf) (coerce (map 'vector [#'list {cons :code}] new) 'list)))
