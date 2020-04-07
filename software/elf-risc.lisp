;;; elf-risc.lisp --- software representation of risc ELF files
(defpackage :software-evolution-library/software/elf-risc
  (:nicknames :sel/software/elf-risc :sel/sw/elf-risc)
  (:use :gt/full
        :elf
        :software-evolution-library
        :software-evolution-library/software/elf)
  (:shadowing-import-from :elf
                          :int-to-bytes
                          :size
                          :type
                          :ph
                          :insert
                          :ordering
                          :data)
  (:export :elf-risc
           :risc-nop
           :elf-risc-max-displacement))
(in-package :software-evolution-library/software/elf-risc)
(in-readtable :curry-compose-reader-macros)


;;; elf software objects
(defvar risc-nop (coerce (elf:int-to-bytes #x0 1) 'list))

(define-software elf-risc (elf)
  ((nop :initarg :nop :reader nop :initform risc-nop))
  (:documentation "DOCFIXME"))

(defmethod elf ((elf elf-risc))
  "DOCFIXME"
  (let ((genome (genome elf)))
    (with-slots (base) elf
      (let ((new (copy-elf base))
            (offset 0))
        ;; If this file has a .text section, simply replace the contents
        ;; of that section.
        (if (named-section base ".text")
            (setf (data (named-section base ".text"))
                  (coerce (mappend {aget :code} (coerce genome 'list)) 'vector))
            ;; Otherwise split up the genome among all loadable
            ;; sections.
            (mapc
             (lambda (sec)
               (setf (data sec)
                     (coerce
                      (mappend {aget :code}
                               (coerce (subseq genome offset
                                               (incf offset (elf:size sec)))
                                       'list))
                      'vector)))
             (remove-if-not [{eql :load}  #'elf:type]
                            (sections new))))
        new))))

(defun risc-genome-from-elf (elf)
  "DOCFIXME"
  (map 'vector
       [#'list {cons :code} #'list]
       (or
        ;; When initializing the genome, first try to
        ;; read a .text section if present and named.
        (coerce (data (named-section elf ".text")) 'list)
        ;; Otherwise we assume that the elf file is
        ;; stripped.  In this later case, collect all
        ;; sections with program headers of type :LOAD.
        (apply
         #'concatenate 'list
         (mapcar #'data
                 (remove-if-not
                  [{eql :load}  #'elf:type #'elf:ph]
                  (remove-if-not #'elf:ph (sections elf))))))))

(defmethod from-file ((elf elf-risc) path)
  "DOCFIXME"
  (setf (base elf) (read-elf path))
  (setf (genome elf) (risc-genome-from-elf (base elf)))
  elf)

(defmethod lines ((elf elf-risc))
  "DOCFIXME"
  (mappend {aget :code} (coerce (genome elf) 'list)))

(defmethod (setf lines) (new (elf elf-risc))
  "DOCFIXME"
  (setf (genome elf) (map 'vector [#'list {cons :code}] new)))

(defmethod apply-mutation ((elf elf-risc) mut)
  "DOCFIXME"
  (let ((starting-length (length (genome elf))))
    ;; NOTE: it is important here that elements of genome are not
    ;;       changed, rather the genome should *only* be changed by
    ;;       setting the genome *accessor* directly.  I.e., avoid
    ;;       forms like the following as they will change the
    ;;       reference value of diff objects (see diff.lisp).
    ;;
    ;;           (setf (car (genome elf)) ...)
    ;;
    ;;       This applies to `elf-cut', `elf-insert', and `elf-swap'.
    (setf (genome elf)
          (ecase (car mut)
            (:cut    (elf-cut elf (second mut)))
            (:insert (elf-insert elf (second mut)
                                 (cdr (assoc :code
                                             (elt (genome elf) (third mut))))))
            (:swap   (elf-swap elf (second mut) (third mut)))))
    elf
    (assert (= (length (genome elf)) starting-length)
            (elf) "mutation ~S changed size of genome [~S -> ~S]"
            mut starting-length (length (genome elf)))))

(defmethod elf-replace ((elf elf-risc) s1 value)
  "DOCFIXME"
  (let ((genome (genome elf)))
    (map (class-of genome)
         (lambda (pair)
           (destructuring-bind (index element) pair
             (if (= s1 index)
                 (let ((copy (copy-tree element)))
                   (setf (cdr (assoc :code copy)) value)
                   copy)
                 element)))
         (indexed (coerce genome 'list)))))

(defmethod elf-cut ((elf elf-risc) s1)
  "DOCFIXME"
  ;; NOTE: see the note above in the body of `apply-mutation'.
  (elf-replace elf s1 (nop elf)))

;; Thanks to the uniform width of RISC instructions, this is the only
;; operation which requires any bookkeeping.  We'll try to
(defvar elf-risc-max-displacement nil
  "Maximum range that `elf-insert' will displace instructions.
This is the range within which insertion will search for a nop to
delete, if none is found in this range insertion becomes replacement.
A value of nil means never replace.")

(defmethod elf-insert ((elf elf-risc) s1 val)
  "DOCFIXME"
  ;; NOTE: see the note above in the body of `apply-mutation'.
  (let ((genome (genome elf)))
    (with-slots (base nop) elf
      (let* ((borders
              ;; Only return the borders between sections if this is the
              ;; genome has been concatenated from multiple ELF
              ;; sections.  We check this by looking for a .text
              ;; section, and if one is found we know that it is the
              ;; sole section in the genome, so no borders are
              ;; necessary.
              (if (named-section base ".text")
                  nil
                  (reduce (lambda (offsets ph)
                            (cons (+ (car offsets) (filesz ph))
                                  offsets))
                          (program-table (base elf)) :initial-value '(0))))
             (backwards-p t) (forwards-p t)
             (nop-location               ; find the nearest nop in range
              (loop :for i :below (or elf-risc-max-displacement (length genome))
                 :do
                 (cond
                   ;; don't cross borders or leave the genome
                   ((or (member (+ s1 i) borders) (>= (+ s1 i) (length genome)))
                    (setf forwards-p nil))
                   ((or (member (- s1 i) borders) (< (- s1 i) 0))
                    (setf backwards-p nil))
                   ((and (not forwards-p) (not backwards-p)) (return nil))
                   ;; continue search forwards and backwards
                   ((and forwards-p
                         (equal nop (cdr (assoc :code (elt genome (+ s1 i))))))
                    (return (+ s1 i)))
                   ((and backwards-p
                         (equal nop (cdr (assoc :code (elt genome (- s1 i))))))
                    (return (- s1 i)))))))
        (if nop-location                 ; displace all bytes to the nop
            (let ((previous val))
              (map (class-of genome)
                   (lambda (pair)
                     (destructuring-bind (i element) pair
                       (if (and (<= s1 i) (<= i nop-location))
                           (let ((copy (copy-tree (elt genome i))))
                             (setf
                              (cdr (assoc :code copy)) previous
                              previous (copy-tree
                                        (cdr (assoc :code (elt genome i)))))
                             copy)
                           element)))
                   (indexed (coerce genome 'list))))
            (elf-replace elf s1 val))))))

(defmethod elf-swap ((elf elf-risc) s1 s2)
  "DOCFIXME"
  ;; NOTE: see the note above in the body of `apply-mutation'.
  (let* ((genome (genome elf))
         (tmp (copy-seq (genome elf))))
    (setf (elt tmp s1) (elt genome s2))
    (setf (elt tmp s2) (elt genome s1))
    tmp))

(defmethod crossover ((a elf-risc) (b elf-risc))
  "One point crossover."
  (let ((point (random (length (genome a))))
        (new (copy a)))
    (setf (genome new) (concatenate 'vector
                         (subseq (genome a) 0 point)
                         (subseq (genome b) point)))
    new))
