;;; elf-cisc.lisp --- software representation of cisc ELF files
(defpackage :software-evolution-library/software/elf-cisc
  (:nicknames :sel/software/elf-cisc :sel/sw/elf-cisc)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :elf
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/elf)
  (:shadowing-import-from :elf :type)
  (:shadowing-import-from :software-evolution-library :size)
  (:export :elf-cisc
	   :elf-csurf
           :elf-x86
           :elf-arm
           :elf-replace
           :pad-nops
           :nop-p
           :disasm
           :addresses))
(in-package :software-evolution-library/software/elf-cisc)
(in-readtable :curry-compose-reader-macros)


;;; elf software objects
(define-software elf-cisc (elf)
  ((addresses :initarg :addresses :accessor addresses :initform nil
              :copier copy-seq))
  (:documentation
   "Executable Linkable Format (ELF) binaries in complex instruction set architectures."))

(define-software elf-csurf (elf-cisc)
  ((sw-project :initarg :sw-project :accessor sw-project :initform nil))
  (:documentation "DOCFIXME"))

(define-software elf-x86 (elf-cisc) ()
  (:documentation "DOCFIXME"))

;; Yes, ARM is a RISC architecture, but it is not a fixed-width
;; instruction, which means we treat it as CISC here.
(defclass elf-arm (elf-cisc) ()
  (:documentation "DOCFIXME"))

(defgeneric pad-nops (elf num-bytes)
  (:documentation "Return NOP(s) sufficient to fill num-bytes"))

(defgeneric nop-p (elf bytes)
  (:documentation "Return non-nil if BYTES is a NOP for ELF."))

(defvar x86-nop (list #x90) "DOCFIXME")

(defmethod pad-nops ((elf elf-x86) num-bytes)
  "DOCFIXME"
  (loop :for i :below num-bytes :collect x86-nop))

(defvar arm-nops
  (loop :for i :below 3 :collect
     (let ((width (expt 2 i)))
       (coerce (elf::int-to-bytes
                (ecase width
                  (1 #x0)
                  (2 #x46C0)
                  (4 #xE1A00000))
                width)
               'list)))
  "DOCFIXME")

(defmethod pad-nops ((elf elf-arm) num-bytes)
  "DOCFIXME"
  ;; Pad an ARM elf file with appropriately sized nops.
  (flet ((arm-nop-for-width (width)
           (car (remove-if-not {= width} arm-nops :key #'length))))
    (loop :until (zerop num-bytes) :collect
       (let ((pad (cond
                    ((>= num-bytes 4) (arm-nop-for-width 4))
                    ((>= num-bytes 2) (arm-nop-for-width 2))
                    ((>= num-bytes 1) (arm-nop-for-width 1)))))
         (decf num-bytes (length pad))
         pad))))

(defmethod nop-p ((elf elf-x86) bytes)
  "DOCFIXME"
  (tree-equal x86-nop bytes))

(defmethod nop-p ((elf elf-arm) bytes)
  "DOCFIXME"
  (member bytes arm-nops :test #'tree-equal))

(defmethod elf ((elf elf-cisc))
  "DOCFIXME"
  (let ((new (copy-elf (base elf))))
    (setf (data (named-section new ".text")) (genome-bytes elf))
    new))

(defun parse-disasm (elf section)
  "DOCFIXME"
  (let ((disasm (disassemble-section (base elf) section)))
    (values
     (mapcar #'car disasm)
     ;; objdump will split the byte sequence for long instructions
     ;; over multiple lines. The additional lines have an empty
     ;; disassembly field, so we must accumulate the bytes in these
     ;; entries back onto the corresponding instruction's bytes.
     (setf (genome elf)
           (loop :for (op . ops)
              :on (mapcar (lambda (triple)
                            (destructuring-bind (address bytes disasm) triple
                              `((:address . ,address)
                                (:code . ,bytes)
                                (:disasm . ,disasm))))
                          (remove-if-not [{= 3} #'length]
                                         (mappend #'cdr disasm)))
              :when (aget :disasm op)
              :collecting
              `((:address . ,(aget :address op))
                (:code . ,(apply {concatenate 'list}
                            (mapcar
                             {aget :code}
                             (cons op (take-until {aget :disasm} ops)))))
                (:disasm . ,(aget :disasm op))))))))

(defmethod from-file ((elf elf-cisc) path)
  "DOCFIXME"
  (setf (base elf) (read-elf path 'objdump))
  (multiple-value-bind (addresses genome) (parse-disasm elf ".text")
    (setf (addresses elf) addresses)
    (setf (genome elf) genome))
  elf)

(defmethod from-file ((elf elf-csurf) path)
  "DOCFIXME"
  (setf (base elf) (read-elf path 'csurf))
  (setf (sw-project (base elf)) (sw-project elf))
  (multiple-value-bind (addresses genome) (parse-disasm elf ".text")
    (setf (addresses elf) addresses)
    (setf (genome elf) genome))
  elf)

(defmethod apply-mutation ((elf elf-cisc) mut)
  "DOCFIXME"
  ;; TODO: CISC mutations should update the `addresses' of the
  ;;       resulting ELF file.
  (flet ((byte-count (genome)
           (without-compiler-notes
               (reduce #'+ (mapcar [#'length {aget :code}] genome)))))
    (let ((starting-bytes (byte-count (genome elf))))
      (setf (genome elf)
            (case (car mut)
              (:cut    (elf-cut elf (second mut)))
              (:insert (elf-insert elf (second mut)
                                   (nth (third mut) (genome elf))))
              (:swap   (elf-swap elf (second mut) (third mut)))))
      (unless (= (byte-count (genome elf)) starting-bytes)
        (error (make-condition 'mutate
                 :text
                 (format nil "mutation ~S changed size of genome [~S -> ~S]"
                         mut starting-bytes (byte-count (genome elf)))
                 :obj elf
                 :operation mut)))
      elf)))

(defun elf-pad (elf genome place num-bytes flags)
  "DOCFIXME"
  (let ((flags (remove :code (copy-tree flags) :key #'car)))
    (append
     (subseq genome 0 place)
     (mapcar [{append flags} #'list {cons :code}] (pad-nops elf num-bytes))
     (subseq genome place))))

(defun elf-strip (elf genome place num-bytes)
  "DOCFIXME"
  (let ((length (length genome)))
    (flet ((nop-p (n genome)
             (nop-p elf (aget :code (nth n genome))))
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
  "DOCFIXME"
  (let ((genome (genome elf)))
    (let* ((prev (nth s1 genome))
           (out-bytes (length (aget :code prev)))
           (in-bytes (length (aget :code value))))
      (assert (assoc :code prev) (prev)
              "attempt to replace genome element with no bytes: ~S" prev)
      (assert (assoc :code value) (value)
              "attempt to insert genome element with no bytes: ~S" value)
      (let ((genome (append (subseq genome 0 s1)
                            (list value)
                            (subseq genome (1+ s1)))))
        (if (> out-bytes in-bytes)
            (values (elf-pad elf genome s1 (- out-bytes in-bytes)
                             ;; mention replace in flags
                             (cons '(:mutation . :replace)
                                   prev))
                    0)
            (multiple-value-call #'values
              (elf-strip elf genome s1 (- in-bytes out-bytes))))))))

(defmethod elf-cut ((elf elf-cisc) s1)
  "DOCFIXME"
  (let ((genome (genome elf)))
    (let ((prev (nth s1 genome)))
      (assert (assoc :code prev) (prev)
              "attempt to cut genome element with no bytes: ~S" prev)
      (elf-pad elf
               (append (subseq genome 0 s1) (subseq genome (1+ s1)))
               s1 (length (aget :code prev))
               ;; mention cut in flags
               (cons '(:mutation . :cut) prev)))))

(defmethod elf-insert ((elf elf-cisc) s1 val)
  "DOCFIXME"
  (let ((genome (genome elf)))
    (assert (assoc :code val) (val)
            "attempt to insert genome element with no bytes: ~S" val)
    (setf (genome elf)
          (append (subseq genome 0 s1) (list val) (subseq genome s1)))
    (elf-strip elf (genome elf) s1 (length (aget :code val)))))

(defmethod elf-swap ((elf elf-cisc) s1 s2)
  "DOCFIXME"
  (assert (every {assoc :code} (mapcar {nth _ (genome elf)} (list s1 s2)))
          (s1 s2) "attempt to swap genome elements w/o bytes: ~S" (cons s1 s2))
  (flet ((rep (point value)
           (setf (genome elf) (elf-replace elf point value))))
    (unless (= s1 s2)
      (let* ((s1-value (copy-tree (nth s1 (genome elf))))
             (s1-size  (length (cdr (assoc :code s1-value))))
             (s2-value (copy-tree (nth s2 (genome elf))))
             (s2-size  (length (cdr (assoc :code s2-value)))))
        ;; Stick the smaller instruction in the larger hole first,
        ;; ensuring that will free up bytes to later make up for placing
        ;; the larger instruction in the smaller hole.
        (if (> s1-size s2-size)
            (progn (rep s1 s2-value)
                   (rep s2 s1-value))
            (progn (rep s2 s1-value)
                   (rep s1 s2-value))))))
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
    (let ((intersection (mapcar #'cdr (intersection (borders a) (borders b)
                                                    :key #'car)))
          (new (copy a)))
      (when intersection
        (let ((point (random-elt intersection)))
          (setf (genome new) (append (subseq (genome a) 0 point)
                                     (subseq (genome b) point)))))
      new)))

(defmethod apply-path ((elf elf-cisc) key addresses &aux applied)
  "DOCFIXME"
  (loop :for el :in addresses :as i :from 0 :do
     (let* ((addr  (if (consp el) (car el) el))
            (val   (if (consp el) (cdr el) t))
            (place (position addr (addresses elf))))
       (when place
         (push (cons key val) (nth place (genome elf)))
         (push (list i key val) applied))))
  (reverse applied))

(defmethod lines ((elf elf-cisc))
  "DOCFIXME"
  (map 'list {aget :code} (genome elf)))

(defmethod (setf lines) (new (elf elf-cisc))
  "DOCFIXME"
  (setf (genome elf) (coerce (map 'vector [#'list {cons :code}] new) 'list)))
