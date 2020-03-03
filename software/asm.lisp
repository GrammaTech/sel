;;; asm.lisp --- software representation of Assembly code
;;;
;;; ASM is a minimal extension of simple.lisp to support assembler
;;; files in which each line is treated as an atomic unit.
;;; Mutations simply move around lines. Thus it works for any
;;; assembler language and the phenome methods are simple/generic
;;; enough to assemble any assembler with the appropriate linker and flags.
;;; This file also defines the asm-range class which is a more space
;;; efficient optimization of the asm class.
;;;
;;; ASM expects its linker slot (gcc by default) to be configured
;;; with a tool that will both assemble and link the file.
;;;
;;; @texi{asm}
(defpackage :software-evolution-library/software/asm
  (:nicknames :sel/software/asm :sel/sw/asm)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/simple)
  (:export :asm
           :asm-range
           :*asm-linker*
           :*asm-mutation-types*
           :addr-map
           :asm-replace-operand
           :asm-nth-instruction
           :asm-split-instruction
           :homologous-crossover))
(in-package :software-evolution-library/software/asm)
(in-readtable :curry-compose-reader-macros)


;;; asm software objects
(define-software asm (simple)
  ((addr-map :initarg :addr-map :accessor addr-map :initform nil
             :copier copy-tree)
   (linker   :initarg :linker   :accessor linker :initform nil)
   (flags    :initarg :flags    :accessor flags :initform nil))
  (:documentation
   "General assembler backend used to manipulate \".s\" text assembler."))

(defmethod from-file :after ((obj asm) file)
  "Ensure `number-genome' is called on all ASM files.
This adds :id fields to genome elements required by `homologous-crossover'."
  ;; Only run `number-genome' on asm objects with standard genomes.
  (declare (ignore file))
  (when (and (proper-list-p (genome obj))
             (proper-list-p (first (genome obj))))
    (number-genome obj)))

(defclass asm-range (sw-range asm)
  ((stats :initarg :stats :accessor stats :initform nil))
  (:documentation
   "Memory efficient alternative to `asm' mixing in `range'.
Some operations on the genome are more complicated but very large
memory savings may be realized by using this class."))

(defmethod to-asm-range ((asm asm))
  "Convert an `asm' software object to an `asm-range' object."
  (with-slots (flags linker genome) asm
    (make-instance 'asm-range
      :flags flags
      :linker linker
      :genome (list (cons 0 (1- (length genome))))
      :reference (coerce (lines asm) 'vector))))

(defmethod from-file ((asm asm-range) file)
  "Initialize an `asm-range' software object from a file.
Note this is required as no `from-file' method is defined on the
`range' software object class."
  (setf (lines asm) (split-sequence #\Newline (file-to-string file)))
  asm)

(defmethod copy ((asm asm-range) &key)
  "Customized copy for `asm-range' software objects.
Ensures deep copies are made of the genome (ranges) but shallow copies
are made of the reference base genome."
  (with-slots (genome linker flags reference) asm
    (make-instance (type-of asm)
      :fitness (fitness asm)
      :genome (copy-tree genome)
      :linker linker
      :flags flags
      :reference reference)))

(defvar *asm-linker* "gcc")

(defvar *asm-new-mutation-types* '(asm-replace-operand)
  "New mutations specific to asm software objects.")

(defvar *asm-mutation-types*
  (let ((orig (un-cumulative-distribution *simple-mutation-types*)))
    (cumulative-distribution
     (normalize-probabilities
      (append orig
              (mapcar {cons _ (/ (length *simple-mutation-types*))}
                      *asm-new-mutation-types*)))))
  "Add asm mutations with probability 1/N, where N is the number of possible
mutations (simple plus asm mutations).")

(defmethod pick-mutation-type ((asm asm))
  "Return a symbol naming a randomly selected ASM mutation."
  (random-pick *asm-mutation-types*))

(defmethod phenome ((asm asm) &key (bin (temp-file-name)))
  "Runs the `linker' for ASM, using the specified `flags' for ASM and returns
multiple values holding in order: (1) the binary path BIN to which the
executable was compiled, (2) the errno, or a numeric indication of success, of
the linking process, (3) STDERR of the linking process, (4) STDOUT of the
linking process, (5) the source file name used during linking."
  #-ccl (declare (values t fixnum string string string))
  (with-temp-file-of (src "s") (genome-string asm)
    (multiple-value-bind (stdout stderr errno)
        (shell "~a -no-pie -o ~a ~a ~{~a~^ ~}"
               (or (linker asm) *asm-linker*) bin src (flags asm))
      (declare (ignorable stdout ))
      (values bin errno stderr stdout src))))


;;; incorporation of oprofile samples
(defmethod apply-path ((asm asm) key addresses &aux applied)
  "Apply a list of sampled ADDRESSES to the ASM's genome behind KEY.
If each element of ADDRESSES is a cons cell then assume the car is the
address and the cdr is the value."
  (let ((map (addr-map asm)))
    (loop :for el :in addresses :as i :from 0 :do
       (let* ((addr (if (consp el) (car el) el))
              (val (if (consp el) (cdr el) t))
              (loc (gethash addr map)))
         (when loc
           (push (cons key val) (aref (genome asm) loc))
           (push (list i key val) applied)))))
  (reverse applied))


;;; Crossover
(defgeneric number-genome (asm)
  (:documentation
   "Number each element in the genome of ASM by adding an :ID association.")
  (:method ((obj asm))
    (iter (for line in (genome obj))
          (for i upfrom 0)
          (unless (aget :id line)
            (setf (elt (genome obj) i) (acons :id i line))))))

(defgeneric homologous-crossover (software-a software-b)
  (:documentation "Crossover at a similar point in both software objects.
After picking a crossover point in SOFTWARE-A, try to find the same point in
SOFTWARE-B. Ideally, if A and B are identical, `homologous-crossover' always
results in a genome that matches those of A and B."))

(defgeneric find-corresponding-point (point-a software-a software-b)
  (:documentation
   "Find and return a point in SOFTWARE-B matching POINT-A in SOFTWARE-A.
Used by `homologous-crossover'."))

(defmethod find-corresponding-point ((point-a integer) (a asm) (b asm))
  "Find and return a point in asm B matching POINT-A in asm A.
Used by `homologous-crossover'."
  (flet ((lookup-id (obj id)
           (aget :id (elt (genome obj) id))))
    (let* ((id-a (lookup-id a point-a))
           ;; adjust if id-a is beyond length of b
           (start (if (>= id-a (length (genome b)))
                      (1- (length (genome b)))
                      id-a))
           ;; max value we can add to start and still have a valid index in b
           (add-upper-bound (- (1- (length (genome b))) start))
           ;; keep track of closest difference in case there's no exact match
           (closest start)
           (closest-diff (abs (- id-a (lookup-id b start)))))
      ;; search above/below start simultaneously, adding and subtracting i
      ;; to get the indices to check (upper and lower, resp.)
      (iter (for i below (max start (1+ add-upper-bound)))
            (let ((lower (- start (min i start)))
                  (upper (+ start (min i add-upper-bound))))
              (cond
                ;; lower index contains an exact match
                ((eql id-a (lookup-id b lower))
                 (leave lower))
                ;; upper index contains an exact match
                ((eql id-a (lookup-id b upper))
                 (leave upper))
                ;; neither upper nor lower matches, check if either is close
                (t (when-let ((id-b-lower (lookup-id b lower)))
                     (when (> closest-diff (abs (- id-a id-b-lower)))
                       ;; id at lower is closer than closest yet found
                       (setf closest lower)
                       (setf closest-diff (abs (- id-a id-b-lower)))))
                   (when-let ((id-b-upper (lookup-id b upper)))
                     (when (> closest-diff (abs (- id-a id-b-upper)))
                       ;; id at upper is closer than closest yet found
                       (setf closest upper)
                       (setf closest-diff (abs (- id-a id-b-upper)))))))
              (finally (return closest)))))))

(defmethod find-corresponding-point
    ((point-a integer) (a asm-range) (b asm-range))
  "A very simplified implementation for ASM-RANGE which don't use support tags."
  (min point-a (size a) (size b)))

(defmethod homologous-crossover ((a asm) (b asm))
  "Crossover at a similar point in asm objects, A and B.
After picking a crossover point in A, try to find the same point in B. If A and
B are identical, this implementation always results in a genome that matches
those of A and B."
  ;; Assumes genome is numbered (see `number-genome')
  ;; TODO: Need to update so that we remove/reset ids after mutations because
  ;; lines copied or swapped from other locs will have a misleading id, most
  ;; likely causing a bad crossover. Changing ids like this will now require
  ;; creating deep copies when we copy the genome.
  (let ((point-a (random (length (genome a))))
        (new (copy a)))
    (let ((point-b (find-corresponding-point point-a a b)))
      (setf (genome new)
            (copy-seq (concatenate (class-of (genome a))
                                   ;; copy from beginning to point-a in A
                                   (subseq (genome a) 0 point-a)
                                   ;; and from point-b to end in B
                                   (subseq (genome b) point-b))))
      (values new point-a point-b))))

(defmethod crossover ((a asm) (b asm))
  "By default, crossover for ASM objects uses `homologous-crossover'."
  (homologous-crossover a b))


;;; ASM mutations
(define-mutation asm-replace-operand (simple-mutation)
  ((targeter :initform #'pick-bad-good))
  (:documentation "Select two instructions, and replace an operand in the first
with an operand in the second."))

(defun asm-split-instruction (instruction)
  "Split INSTRUCTION string on white space or commas. Return a list of strings."
  (nest (split "(\\s*,\\s*)|(\\s+)")
        (string-trim '(#\Space #\Tab #\Newline) instruction)))

(defun asm-nth-instruction (asm n)
  "Extract the string portion of the Nth instruction from the genome of ASM."
  (assoc-value (elt (genome asm) n) :code))

(defmethod apply-mutation ((asm asm) (mutation asm-replace-operand))
  "Apply an asm-replace-operand MUTATION to ASM, return the resulting software.
The MUTATION targets are a pair of instruction indices pointing to a \"bad\"
instruction (whose operand will be replaced) and a \"good\" instruction
(whose operand will be used as the replacement). If either instruction lacks an
operand, a `no-mutation-targets' condition is raised."
  (let ((bad-good (targets mutation)))
    (assert (listp bad-good) (mutation)
            "Requires mutations targets to be a list of two elements.")
    ;; NOTE: assumes instructions start with :code symbol
    (let* ((genome (genome asm))
           (bad (first bad-good))
           (bad-instr-string (asm-nth-instruction asm bad))
           (bad-operands (cdr (asm-split-instruction bad-instr-string)))
           (good-operands (nest (cdr)
                                (asm-split-instruction)
                                (asm-nth-instruction asm)
                                (second bad-good))))
      (when (or (null bad-operands) (null good-operands))
        (error (make-condition 'no-mutation-targets
                               :text "No operands in instruction(s)"
                               :obj asm)))
      (let ((bad-operand (random-elt bad-operands))
            (good-operand (random-elt good-operands)))
        (setf (genome asm)
              (concatenate (class-of genome)
                           (subseq genome 0 bad)
                           (list (acons :code
                                        (regex-replace-all
                                         (quote-meta-chars bad-operand)
                                         bad-instr-string
                                         good-operand)
                                        nil))
                           (subseq genome (1+ bad))))
        asm))))
