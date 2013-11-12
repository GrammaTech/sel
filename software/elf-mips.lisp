;;; elf-mips.lisp --- software representation of mips ELF files

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Commentary:

;;; Code:
(in-package :software-evolution)


;;; elf software objects
(defclass elf-mips-sw (elf-sw) ())

(defvar mips-nop #x0)

(defmethod elf ((elf elf-mips-sw))
  (with-slots (base genome) elf
    (let ((new (copy-elf base))
          (offset 0))
      (mapc (lambda (sec)
              (setf (data sec)
                    (map 'vector {aget :bytes}
                         (subseq genome offset (incf offset (elf:size sec))))))
            (remove-if-not [{eql :load}  #'elf:type]
                           (sections base)))
      new)))

(defmethod from-file ((elf elf-mips-sw) path)
  (with-slots (base genome) elf
    (setf base (read-elf path)
          genome
          (coerce
           (mapcar [#'list {cons :bytes}]
                   (apply #'concatenate 'list
                          (mapcar #'data
                                  (remove-if-not [{eql :load}  #'elf:type]
                                                 (sections base)))))
           'vector)))
  elf)

(defmethod apply-mutation ((elf elf-mips-sw) mut)
  (let ((starting-length (length (genome elf))))
    (setf (genome elf)
          (ecase (car mut)
            (:cut    (elf-cut elf (second mut)))
            (:insert (elf-insert elf (second mut)
                                 (cdr (assoc :bytes
                                             (aref (genome elf) (third mut))))))
            (:swap   (elf-swap elf (second mut) (third mut)))))
    (assert (= (length (genome elf)) starting-length)
            (elf) "mutation ~S changed size of genome [~S -> ~S]"
            mut starting-length (length (genome elf)))))

(defmethod elf-cut ((elf elf-mips-sw) s1)
  (with-slots (genome) elf
    (setf (cdr (assoc :bytes (aref genome s1))) mips-nop)
    genome))

;; Thanks to the uniform width of MIPS instructions, this is the only
;; operation which requires any bookkeeping.  We'll try to 
(defvar elf-mips-max-displacement nil
  "Maximum range that `elf-insert' will displace instructions.
This is the range within which insertion will search for a nop to
delete, if none is found in this range insertion becomes replacement.
A value of nil means never replace.")

(defmethod elf-insert ((elf elf-mips-sw) s1 val)
  (with-slots (genome) elf
    (let* ((borders (reduce (lambda (offsets ph)
                              (cons (+ (car offsets) (filesz ph))
                                    offsets))
                            (program-table (base cgi)) :initial-value '(0)))
           (backwards-p t) (forwards-p t)
           (nop-location               ; find the nearest nop in range
            (loop :for i :below (or elf-mips-max-displacement infinity) :do
               (cond
                 ;; don't cross borders
                 ((member (+ s1 i) borders) (setf forwards-p nil))
                 ((member (- s1 i) borders) (setf backwards-p nil))
                 ((and (not forwards-p) (not backwards-p)) (return nil))
                 ;; continue search forwards and backwards
                 ((and forwards-p
                       (= mips-nop (cdr (assoc :bytes (aref genome (+ s1 i))))))
                  (return (+ s1 i)))
                 ((and backwards-p
                       (= mips-nop (cdr (assoc :bytes (aref genome (- s1 i))))))
                  (return (- s1 i)))))))
      (if nop-location                 ; displace all bytes to the nop
          (reduce (lambda (previous i)
                    (let ((current (cdr (assoc :bytes (aref genome i)))))
                      (setf (cdr (assoc :bytes (aref genome i))) previous)
                      current))
                  (range s1 nop-location) :initial-value val)
          (setf (cdr (assoc :bytes (aref genome s1))) val)))
    genome))

(defmethod elf-swap ((elf elf-mips-sw) s1 s2)
  (with-slots (genome) elf
    (let ((left-bytes  (copy-tree (cdr (assoc :bytes (aref genome s1)))))
          (right-bytes (copy-tree (cdr (assoc :bytes (aref genome s2))))))
      (setf (cdr (assoc :bytes (aref genome s1))) right-bytes
            (cdr (assoc :bytes (aref genome s2))) left-bytes))
    genome))

(defmethod crossover ((a elf-mips-sw) (b elf-mips-sw))
  "One point crossover."
  (let ((point (random (length (genome a))))
        (new (copy a)))
    (setf (genome new) (append (subseq (genome a) 0 point)
                               (subseq (genome b) point)))
    new))
