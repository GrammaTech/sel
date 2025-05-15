;;; asm-heap.lisp --- specialized representation of assembly code
;;;
;;; ASM-HEAP is another optimization of the ASM class (like
;;; ASM-RANGE) but with a different design utilizing a heap
;;; of the original lines. Mutations and copies can share those
;;; original lines, avoiding deep copies of large assembler
;;; object.
;;;
;;; This class assumes the assembler code is simple and does not use
;;; macros or custom declarations. This makes it useful for most
;;; assembler output from compilers or disassemblers.
;;; This allows some simple tokenization, and identifies function
;;; boundaries.
;;;
;;; The ASM-HEAP software class supports mutations on assembler
;;; source files. These can be in either Intel x86_64 syntax
;;; (as used with the Nasm assembler) or AT&T x86_64 syntax (as
;;; used with GAS, or Gnu Assembler).
;;;
;;; The ASM-SYNTAX property will be automatically set to either
;;; :INTEL or :ATT based on the lines which are loaded into the genome.
;;;
;;; In general, one line of the source file results in one element
;;; of the resulting genome. However, the line parser does these things:
;;;   a) separates label declarations onto their own line
;;;      e.g.  my_label: push rax
;;;      becomes "my_label:" and "push rax" as two lines.
;;;   b) lines either empty or consisting only of white space are removed
;;;      Lines containing only comments are not removed, they are kept as is,
;;;      but the ASM-LINE-INFO-TYPE is :EMPTY.
;;;
(defpackage :software-evolution-library/software/asm-heap
  (:nicknames :sel/software/asm-heap :sel/sw/asm-heap)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/simple
        :software-evolution-library/software/asm
        :software-evolution-library/software/super-mutant)
  (:import-from :software-evolution-library/software/asm
                flags
                linker)
  (:export :asm-heap
           :line-heap
           :function-index
           :function-bounds-file
           :super-owner
           :asm-syntax
           :asm-line-info
           :asm-line-info-text
           :asm-line-info-tokens
           :asm-line-info-type
           :asm-line-info-label
           :asm-line-info-opcode
           :asm-line-info-operands
           :asm-line-info-id
           :asm-line-info-orig-file
           :asm-line-info-orig-line
           :asm-line-info-address
           :asm-line-info-properties
           :function-index-entry
           :function-index-entry-name
           :function-index-entry-start-line
           :function-index-entry-start-address
           :function-index-entry-end-line
           :function-index-entry-end-address
           :function-index-entry-is-leaf
           :function-index-entry-declarations
           :insert-new-line
           :insert-new-lines
           :parse-asm-line
           :call-targets
           :intel-syntax-p
           :vector-cut))
(in-package :software-evolution-library/software/asm-heap)
(in-readtable :curry-compose-reader-macros)

(defstruct (asm-line-info (:copier copy-asm-line-info))
  (text nil)     ; original text
  (tokens nil)   ; list of tokens after parsing
  (type nil)     ; empty (white space/comments), decl, data, label-decl, op)
  (label nil)    ; for operations which refer to labels
  (opcode nil)   ; for operations
  (operands nil) ; the operands that are associated with the opcode
  (id nil)       ; unique index in heap, sequential starting at 0
  (orig-file nil)  ; path of the orignal .asm file that was loaded (if any)
  (orig-line nil) ; line number (0-based) of line in original .asm file (if any)
  (address nil)  ; original address of code or data
  (properties nil))   ; properties (used for custom purposes)

(defstruct function-index-entry
  "Entry into the index, which is a vector of all the functions in the file."
  (name nil)              ; function name
  (start-line nil)        ; first line index, zero-based, of genome
  (start-address nil)     ; address of first line
  (end-line nil)          ; last line index, zero-based, of genome
  (end-address nil)       ; address of last line
  (is-leaf nil)           ; true, if function does not make any calls
  (declarations nil))     ; list of declaration lines found for the function


(define-software asm-heap (asm)
  ((line-heap :initarg :line-heap :accessor line-heap)
   (function-index :initarg :function-index :initform nil
                   :reader function-index
                   :documentation "Create this on demand.")
   (function-bounds-file
    :initarg :function-bounds-file
    :initform nil :reader function-bounds-file
    :documentation "If this is present, use it to create function index")
   (super-owner :initarg :super-owner :accessor super-owner :initform nil
                :documentation
                "If present, contains asm-super-mutant instance.")
   (asm-syntax :initarg :asm-syntax :accessor asm-syntax :initform ':intel
                :documentation
                "Assembly syntax, either :att or :intel."))
  (:documentation
   "Structured software object for assembler source.

Alternative to ASM software objects which should use less memory
in many cases. Similar to RANGE, but allows for adding and mutating
lines, and should be able to handle any type of mutation we need.
The GENOME is a vector of references into the asm-heap.
Copying an ASM-HEAP is shallow by default, to avoid a large
performance penalty when making many copies. The line-heap holds
the original lines of the program (before any mutations, along
with any new or modified lines appended to the end of the heap.
All elements of the genome are references into this line-heap."))

;;; This read-table and package are used for parsing ASM instructions.
(defvar *assembler-x86-readtable*
  (let ((rt (copy-readtable)))
    (setf (readtable-case rt) :preserve) ;; preserve case of asm symbols
    rt))

(defpackage :software-evolution-library/asm (:nicknames :sel/asm))

;;
;; treat some characters such as : and , as special tokens
;;
(set-macro-character #\: (lambda (stream ch)(declare (ignore stream ch)) :colon)
                     nil
                     *assembler-x86-readtable*)

(set-macro-character #\, (lambda (stream ch)(declare (ignore stream ch)) :comma)
                     nil
                     *assembler-x86-readtable*)

(set-macro-character #\[ (lambda (stream ch)
                           (declare (ignore stream ch))
                           :\[)
                     nil
                     *assembler-x86-readtable*)

(set-macro-character #\] (lambda (stream ch)
                           (declare (ignore stream ch))
                           :\])
                     nil
                     *assembler-x86-readtable*)

(set-macro-character #\+ (lambda (stream ch)
                           (declare (ignore stream ch))
                           :+)
                     nil
                     *assembler-x86-readtable*)

(set-macro-character #\* (lambda (stream ch)
                           (declare (ignore stream ch))
                           :*)
                     nil
                     *assembler-x86-readtable*)

(set-macro-character #\# (lambda (stream ch)
                           (declare (ignore stream ch))
                           :#)
                     nil
                     *assembler-x86-readtable*)

;; If we encounter a single quote, look for the terminating quote.
(set-macro-character
 #\'
 (lambda (stream ch)
   (declare (ignore ch))
   (let ((chars nil)(eof (cons nil nil)))
     (flet ((get-char ()
              (let ((ch (read-char stream nil eof t)))
                (when (eq ch eof)
                  (error "Unterminated single-quoted string found"))
                ch)))
       (do ((ch (get-char) (get-char))
            (count 0 (1+ count)))
           ((char= ch #\')
            (make-array count
                        :element-type 'character
                        :initial-contents (nreverse chars)))
         ;; handle '\' as an escape character
         (push (if (char= ch #\\)(get-char) ch) chars)))))

 nil
 *assembler-x86-readtable*)

(defun intel-syntax-p (x)
  "True if syntax has been set to :intel.
The argument should be an asm-heap instance
or a symbol representing a valid asm-syntax slot value."
  (if (symbolp x)
      (eq x ':intel)
      (eq (asm-syntax x) ':intel)))

;;;
;;; The Lisp reader will handle intel comments (;) as we need them, so we
;;; don't do anything special about them.
;;; The reader will not correctly handle gas assembler comments (#) so
;;; we defined a special reader macro to turn them into :#, which basically
;;; signals the same thing as eof (end of line).
;;;
(defun tokenize-asm-line (line)
  "Take a line of text from a .asm file, and, and converts it to tokens."
  (with-input-from-string (s line)
    (do* ((*readtable* *assembler-x86-readtable*)
          (*package* (find-package :sel/asm))
          (result '())
          (eof (cons 0 0))
          (token (read s nil eof)(read s nil eof)))
         ((or (eq token eof)(eq token :#))(nreverse result))
      (if (symbolp token)
          (setf token (symbol-name token)))
      (push token result))))

(defun token-label-p (token)
  (and (stringp token)
       (or
        (starts-with-subseq "$" token)
        (starts-with-subseq ".L" token))))

(defun branch-op-p (token)
  "Returns true iff the token represents a jump operation. We assume it
is a jump operator if the first letter is #\j or #\J. For our purposes
we are excluding CALL instructions."
  (and (stringp token)
       (let ((tok (string-downcase token)))
         (or (starts-with-subseq "j" tok)
             (starts-with-subseq "call" tok)))))

(defun push-op-p (token)
  "Returns true iff the token represents a push operation."
  (and (stringp token)
       (starts-with-subseq "push" (string-downcase token))))

;;; Given a list of tokens representing the line, returns either of:
;;;     :nothing
;;;     :declaration
;;;     :data
;;;     :label
;;;     :operation
(defun parse-line-type-intel (tokens)
  (cond ((null tokens) ':empty)
        ((and (is-asm-symbol (first tokens)) ; first token valid symbol?
              (equalp (second tokens) "COLON"))  ; followed by a ':'?
         ':label-decl)
        ((some {member _ tokens :test #'equalp}
               (list "db" "dq" "dd" "dw"))
         ':data)
        ((member (first tokens)
                 '("align"
                   "section"
                   "extern"
                   "%define"
                   "global"
                   ".type"
                   ".text"
                   ".bss")
                 :test 'equalp)
         ':decl)
        ((and (token-label-p (first tokens))
              (equalp (second tokens) "equ"))
         ':decl)
        (t ':op)))     ;; use this as catch-all for anything else

(defun parse-line-type-att (tokens)
  (cond ((null tokens) ':empty)
        ((and (is-asm-symbol (first tokens)) ; first token valid symbol?
              (equalp (second tokens) "COLON"))  ; followed by a ':'?
         ':label-decl)
        ((some {member _ tokens :test #'equalp}
               (list ".quad" ".zero" ".byte" ".word" ".string"))
         ':data)
        ((member (first tokens)
                 '(".align"
                   ".section"
                   ".globl"
                   ".type"
                   ".text"
                   ".bss")
                 :test 'equalp)
         ':decl)
        ((and (= (length tokens) 1)
              (stringp (first tokens))
              (char= (char (first tokens) 0) #\.))
         ':decl)  ; gas style declaration starts with a "."
        (t ':op)))

(defun parse-line-type (tokens syntax)
  "From list of tokens, determine type of line."
  (if (intel-syntax-p syntax)
      (parse-line-type-intel tokens)
      (parse-line-type-att tokens)))

;;;
;;; takes a line of text from a .asm file, and, and returns 1 or 2
;;; asm-line-info structs.  If the line begins with a label, the line
;;; is split into two lines: the label, and the remaining
;;; text/tokens. In this case 2 asm-line-info structs are returned.
;;; Otherwise, as single asm-line-info is returned.
;;;
(defun parse-asm-line (line syntax)
  (let* ((tokens (tokenize-asm-line line))
         (info (make-asm-line-info :text line :tokens tokens :properties nil)))

    ;; see if there is a comment: "orig ea=0xnnnnnnnn" which specifies
    ;; the original address of code or data
    (let* ((addr-comment "orig ea=0x")
           (addr-pos (search addr-comment line :from-end t :test 'equal)))
      (if addr-pos
          (setf (asm-line-info-address info)
                (parse-integer line
                               :radix 16
                               :start (+ (length addr-comment) addr-pos)
                               :junk-allowed t))))

    ;; Determine type of line
    (let ((line-type (parse-line-type tokens syntax)))
      (setf (asm-line-info-type info) line-type)

      (case line-type
        (:label-decl
         (let* ((label (first tokens))
                (label-end (position #\: line))
                (line1 (subseq line 0 (+ label-end 1)))
                (line2 (concatenate 'string
                                    "       "
                                    (subseq line (+ label-end 1))))
                (next-info (parse-asm-line line2 syntax))) ;; recurse!
           (setf (asm-line-info-text info) line1)
           (setf (asm-line-info-tokens info) (list label ':colon))
           (setf (asm-line-info-label info) label)
           (if (and next-info (not (eq (asm-line-info-type
                                        (car next-info))
                                        ':empty)))
               ;; If an empty line follows the label, discard it.
	       (cons info next-info)
	       (progn
		 ;; Restore full text line.
		 (setf (asm-line-info-text info) line)
		 (list info)))))
	(:empty (list info))
	(:op (setf (asm-line-info-opcode info) (first tokens))
	     (let ((comma-pos (position ':comma (rest tokens))))
	       (setf (asm-line-info-operands info)
		     (if comma-pos
			 (list
			  (subseq (rest tokens) 0 comma-pos)
			  (subseq (rest tokens) (+ comma-pos 1)))
			 (list (rest tokens)))))
	     (list info))
	(:data (list info))
	(:decl (list info))))))

;;;
;;; Convert an operand (from ASM-LINE-INFO-OPERANDS list) to text string.
;;;
(defun format-asm-operand (op) ; list of tokens
  (format nil "~{~A~}"
	  (mapcar (lambda (x)
		    (cond
		      ((member x '("qword"
				 "dword"
				 "word"
				 "byte")
			     :test 'equalp)
		       (format nil "~A " x))
		      ((equalp x "COMMA") ", ")
		      (t x)))
		  op)))

(defun update-asm-line-info-text (asm-line)
  "Update the TEXT field of ASM-LINE-INFO after updated operation or operands."
  (when (eq (asm-line-info-type asm-line) ':op)
    (setf (asm-line-info-text asm-line)
	(format nil "~A ~{~A~^, ~}"
		(asm-line-info-opcode asm-line)
		(mapcar 'format-asm-operand
			(asm-line-info-operands asm-line))))))

(defmethod create-super ((variant asm-heap) &optional rest-variants)
  "Creates a ASM-SUPER-MUTANT and populates it with single variant."
  (let ((inst (copy (super-owner variant))))
    (setf (mutants inst)(cons variant rest-variants))
    (dolist (x (mutants inst))
      (setf (super-owner x) inst))  ; set owner of all mutants
    inst))

(defmethod function-index ((asm asm-heap))
  "If FUNCTION-INDEX slot contains an index, return it. Otherwise create
the index and return it."
  (or (slot-value asm 'function-index)
      (setf (slot-value asm 'function-index)
	    (if (function-bounds-file asm)
		(load-function-bounds asm (function-bounds-file asm))
                (create-asm-function-index asm)))))

(defmethod size ((asm asm-heap))
  "Return the number of lines in the program."
  (length (genome asm)))

(defun line-heap-copy (heap)
  (let ((new (make-array (length heap) :fill-pointer (length heap)
			 :adjustable t :initial-contents heap)))
    new))

(defmethod lines ((asm asm-heap) &key)
  "Return the list of text lines of the genome."
  (map 'list 'asm-line-info-text (genome asm)))

(defparameter *symbol-special-chars* '(#\$ #\. #\_))

(defun is-asm-symbol (s)
  "Returns true iif the passed string is a valid symbol."
  (and (stringp s)
       (> (length s) 0)
       (or (alpha-char-p (char s 0))
	   (member (char s 0) *symbol-special-chars*))
       (or (= (length s) 1)
	   (every
	    (lambda (c)
	      (or (alpha-char-p c)
		  (digit-char-p c)
		  (member c *symbol-special-chars*)))
	    (subseq s 1)))))

(defun is-empty (text)
  "Returns true iff the text is empty or all whitespace."
  (emptyp (trim-whitespace text)))

(defun comment-or-blank-line (text)
  "If text is a blank line or comment line, return true."
  (or (is-empty text)
      (let ((pos (position #\# text)))
	(and pos (comment-or-blank-line (subseq text 0 pos))))))

(defun intel-or-att (text-lines)
  "Heuristic: asm lines are intel or att syntax.
Returns :intel or :att.
If any line contains specified register name then consider them
all to be att syntax. If it can't be determined, returns nil."
  (dolist (line text-lines)
    (setf line (trim-whitespace line))
    (if (or
	 (search "%rbp" line)
	 (search "%rsp" line)
	 (search "%rax" line))
	(return-from intel-or-att ':att))
    (if (or
	 (search "rbp" line)
	 (search "rsp" line)
	 (search "rax" line))
	(return-from intel-or-att ':intel)))
  nil)

(defmethod (setf lines) (asm-lines (asm asm-heap))
  "Initializes the line-heap.
Does this by converting the passed list of lines to ASM-LINE-INFO
structs, and storing them in a vector on the LINE-HEAP"
  (let* ((asm-infos '())
	 (orig-line 0)
	 (id 0))
    (setf (asm-syntax asm)
          (intel-or-att asm-lines)) ; set syntax based on heuristic
    (dolist (line asm-lines)
      (unless (comment-or-blank-line line)
	(dolist (asm-info (parse-asm-line line (asm-syntax asm)))
	  (setf (asm-line-info-id asm-info) id)
	  (setf (asm-line-info-orig-line asm-info) orig-line)
	  (incf id)
	  (push asm-info asm-infos))
	(incf orig-line)))
    (setf asm-infos (nreverse asm-infos))
    (let* ((size (length asm-infos))
	   (heap (make-array size
                             :fill-pointer size :adjustable t
                             :initial-contents asm-infos)))
      (setf (line-heap asm) heap)
      ;; Make a copy for this instance.
      (setf (genome asm)(line-heap-copy heap))))
  ;; Create and cache function index.
  asm)

(defmethod from-file ((asm asm-heap) file)
  "Initialize an `asm-heap' software object from a file."
  (setf (lines asm) (split-sequence #\Newline (file-to-string file)))
  ;; tag all the heap entries with the original file name
  (map nil (lambda (info) (setf (asm-line-info-orig-file info) file))
       (line-heap asm))
  asm)

(defmethod to-file ((asm asm-heap) file)
  "Save the assembly for ASM to FILE."
  (with-open-file (out file :direction :output :if-exists :supersede)
    (genome-string asm out)))

(defmethod phenome ((asm asm-heap) &key (bin (temp-file-name)))
  "Runs the `linker' for ASM-HEAP, using the specified `flags' for ASM and returns
multiple values holding in order: (1) the binary path BIN to which the
executable was compiled, (2) the errno, or a numeric indication of success, of
the linking process, (3) STDERR of the linking process, (4) STDOUT of the
linking process, (5) the source file name used during linking."
  #-ccl (declare (values t fixnum string string string))

  ;; if intel (backward-compatibility) use phenome in ASM class
  (when (intel-syntax-p asm)
    (return-from phenome (call-next-method)))

  (with-temporary-file-of (:pathname src :type "s") (genome-string asm)
    (with-temporary-file (:pathname obj :type "o")
      ;; Assemble.
      (multiple-value-bind (stdout stderr errno)
          (shell "~a -o ~a ~a"
		 "as"
		 obj
		 src)
	(declare (ignorable stdout stderr))
        (restart-case
            (unless (zerop errno)
              (error (make-condition 'phenome :text stderr :obj asm :loc src)))
          (retry-project-build ()
            :report "Retry `phenome' assemble on OBJ."
            (phenome obj :bin bin))
          (return-nil-for-bin ()
            :report "Allow failure returning NIL for bin."
            (setf bin nil))))
      (setf (linker asm) "clang")
      (setf (flags asm)
	    '("-no-pie" "-O0" "-fnon-call-exceptions" "-g" "-lrt"))
      (multiple-value-bind (stdout stderr errno)
          (shell "~a -o ~a ~a ~{~a~^ ~}"
                 (linker asm)
                 bin
                 obj
                 (flags asm))
        (declare (ignorable stdout ))
        (without-compiler-notes
          (values bin errno stderr stdout src))))))

(defun vector-cut (a index)
  "Destructively remove and return an element from a vector with a fill pointer."
  (let ((deleted (aref a index)))
    (do ((i index (+ i 1)))
	((= i (- (length a) 1))(decf (fill-pointer a)))
      (setf (aref a i)(aref a (+ i 1))))
    deleted))

(defun vector-insert (a index val)
  "Destructively insert and return an object into a vector with a fill pointer."
  (vector-push-extend 0 a) ; Increase size by 1 (will be resized if necessary).
  (do ((i (- (length a) 1) (- i 1)))
      ((= i index)(setf (aref a i) val))
    (setf (aref a i)(aref a (- i 1)))))

;;;
;;; Given a textual line of assembler, parse it and add the resulting
;;; list of asm-line-info structs to the heap.
;;; Returns the list of new asm-line-info struct.
;;;
(defun parse-and-add-to-heap (asm-heap text)
  (let* ((info-list (parse-asm-line text (asm-syntax asm-heap)))
	 (id (length (line-heap asm-heap))))
    (dolist (info info-list)
      (setf (asm-line-info-id info) id)
      (incf id)
      (vector-push-extend info (line-heap asm-heap)))
    info-list))

(defun insert-new-line (asm-heap text
			&optional (index (length (genome asm-heap))))
  "Insert a line of assembler code into the genome.
Parses a new line of assembler, adds it to the heap, and inserts
it at index in the genome. Returns the number of lines inserted.
Note that one line may be split into multiple lines by the parser.
If index is not supplied, defaults to the end of the genome."
  (let ((info-list (parse-and-add-to-heap asm-heap text)))
    (dolist (info info-list)
      (vector-insert (genome asm-heap) index info)
      (incf index))
    (length info-list)))

(defun insert-new-lines (asm-heap line-list
			 &optional (index (length (genome asm-heap))))
  "Insert a list of asm lines into the genome.
Inserts list of lines (calling insert-new-line).
If index is not supplied, defaults to end of the genome."
  (dolist (x line-list)
    (incf index (insert-new-line asm-heap x index))))

(defmethod pick-good ((software asm-heap))
  (pick-bad software))

(defun pick-is-acceptable (asm n)
  (if (asm-line-info-label (elt (genome asm) n))
      nil   ;; don't mess with label lines
      t))

;;;
;;; TO DO: this can go into an infinite loop if every line in the
;;; genome is a label declaration. Should probably watch for this case.
;;;
(defmethod pick-bad  ((software asm-heap))
  (do* ((bad (random (size software))(random (size software))))
       ((pick-is-acceptable software bad) bad)))

(defmethod apply-mutation ((asm asm-heap) (mutation simple-cut))
  "Implement simple-cut mutation on ASM-HEAP."
  ;; if we our target cut is a label line, abort
  (when (and
	 (asm-line-info-label (elt (genome asm) (targets mutation)))
	 (find-restart 'try-another-mutation))
    (format t "skipping cut with label ~A~%"
	    (asm-line-info-label (elt (genome asm) (targets mutation))))
    (invoke-restart 'try-another-mutation))
  (unless (asm-line-info-label (elt (genome asm) (targets mutation)))
    (vector-cut (genome asm) (targets mutation)))
  asm)

(defmethod apply-mutation ((asm asm-heap) (mutation simple-insert))
  "Insert a value into location of ASM as specified by MUTATION."
  (let ((bad-good (targets mutation)))
    (assert (listp bad-good) (mutation)
            "Requires mutations targets to be a list of two elements.")
    (vector-insert (genome asm)
                   (first bad-good)
                   (aref (genome asm) (second bad-good))))
  asm)

(defmethod apply-mutation ((asm asm-heap) (mutation simple-swap))
  "Swap the values at two locations in ASM as specified by MUTATION."
  (let ((bad-good (targets mutation)))
    (assert (listp bad-good) (mutation)
            "Requires mutations targets to be a list of two elements.")
    (with-slots (genome) asm
      (rotatef (aref genome (first bad-good)) (aref genome (second bad-good))))
    asm))

(defun asm-heap-subseq (asm start &optional end)
  "Create a new mutated asm-heap.
The new genome contains only the elements in the designated range."
  (let* ((new (copy asm))
	 (genome (genome new))
	 (new-genome (line-heap-copy (subseq genome start end))))
    (setf (genome new) new-genome)
    new))

(defmethod apply-mutation ((asm asm-heap) (mutation asm-replace-operand))
  "Apply an asm-replace-operand MUTATION to ASM-HEAP, return the resulting
software. The MUTATION targets are a pair of instruction indices pointing
to a \"bad\" instruction (whose operand will be replaced) and
a \"good\" instruction (whose operand will be used as the replacement).
If either instruction lacks an operand, a `no-mutation-targets' condition
is raised."
  (let ((bad-good (targets mutation)))
    (assert (listp bad-good) (mutation)
            "Requires mutations targets to be a list of two elements.")
    ;; NOTE: assumes instructions start with :code symbol
    (let* ((genome (genome asm))
           (bad (first bad-good))
	   (good (second bad-good))
           (bad-instr (elt genome bad))
	   (good-instr (elt genome good))
           (bad-operands (asm-line-info-operands bad-instr))
           (good-operands (asm-line-info-operands good-instr)))
      (when (or (null bad-operands) (null good-operands))
        (error (make-condition 'no-mutation-targets
                               :text "No operands in instruction(s)"
                               :obj asm)))
      (let ((new-instr (copy-asm-line-info bad-instr)))
	;; update one of the operands with a randomly selected operand from
	;; the good statement
	(setf (elt (asm-line-info-operands new-instr)
		   (random (length (asm-line-info-operands good-instr))))
	      (random-elt good-operands))
	;; update the text since we changed the operand
	(update-asm-line-info-text new-instr)
	;; update the genome with the newly modified instruction
        (setf (elt genome bad) new-instr)
        asm))))

(defmethod pick-mutation-type ((asm asm-heap))
  (random-pick *simple-mutation-types*))

(defun find-labels (asm-heap)
  (let ((lab-list '()))
    (iter (for a in-vector (genome asm-heap))
	  (when (and
	       (eq (asm-line-info-type a) ':op)
	       (or (branch-op-p (first (asm-line-info-tokens a)))
                   (push-op-p (first (asm-line-info-tokens a)))))
	    (if (token-label-p (second (asm-line-info-tokens a)))
		(push (second (asm-line-info-tokens a)) lab-list))
	    (if (token-label-p (third (asm-line-info-tokens a)))
		(push (third (asm-line-info-tokens a)) lab-list))))
    lab-list))

(defun find-label-decls (asm-heap)
  "Find the labels declared in the genome, return the list."
  (let ((lab-list '()))
    (iter (for a in-vector (genome asm-heap))
	  (if (eq (asm-line-info-type a) ':label-decl)
	      (push (asm-line-info-label a) lab-list)))
    lab-list))

(defun labels-valid-p (asm-heap)
  "Returns true if all labels which are referenced by
operation lines in the genome can be found as label declarations
in the genome. There must be no more than one declaration of any
label. If any referenced label is not found, or is declared more
than once, returns false."
  (dolist (lab (find-labels asm-heap))
    (let ((count 0))
      (iter (for a in-vector (genome asm-heap))
	    (if (equalp (asm-line-info-label a) lab)
		(incf count)))
      (unless (= count 1) (return-from labels-valid-p nil))))
  ;; return false if any of the label declarations are duplicated
  (let ((declared-labels (find-label-decls asm-heap)))
    (= (length declared-labels)
       (length (remove-duplicates declared-labels :test 'equal)))))

;;;
;;; Issues with the searching method below. I think we we might get reasonable
;;; results simply by using the same point in both genomes. In case the
;;; point exceeds the size of b's genome, we truncate it to the largest
;;; possible point in b.
;;;
;;; TODO: figure out the best approach to implement this method
;;;
(defmethod find-corresponding-point ((point-a integer) (a asm-heap) (b asm-heap))
  "Find and return a point in asm B matching POINT-A in asm A.
Used by `homologous-crossover'."
  (if (>= point-a (size b))
      (1- (size b))
      point-a))

#+ignore
(defmethod find-corresponding-point ((point-a integer) (a asm-heap) (b asm-heap))
  "Find and return a point in asm B matching POINT-A in asm A.
Used by `homologous-crossover'."
  (flet ((lookup-id (obj id)
           (asm-line-info-id (elt (genome obj) id))))
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

(defmethod homologous-crossover ((a asm-heap) (b asm-heap))
  "Crossover at a similar point in asm objects, A and B.
After picking a crossover point in A, try to find the same point in B. If A and
B are identical, this implementation always results in a genome that matches
those of A and B."
  ;; ASM-HEAP genomes are automatically numbered

  ;; if either genome is 0-length, just return the other asm-heap object
  (cond ((zerop (length (genome a))) (values (copy b) 0 0))
	((zerop (length (genome b))) (values (copy a) 0 0))
	(t
	 (let ((point-a (random (length (genome a))))
	       (new (copy a)))
	   (let* ((point-b (find-corresponding-point point-a a b))
		  (new-seq (concatenate (class-of (genome a))
					; copy from beginning to point-a in A
					(subseq (genome a) 0 point-a)
					; and from point-b to end in B
					(subseq (genome b) point-b))))
	     (setf (genome new)
		   (make-array (length new-seq) :fill-pointer (length new-seq)
			       :adjustable t :initial-contents new-seq))
	     (if (labels-valid-p new)  ; filter out any crossover results which
		                       ; messup the labels
	         (values new point-a point-b)
		 (values (copy a) 0 0)))))))

(defun function-name-from-label (name asm)
  "Given a label like $FOO1, returns FOO1 (intel only).
The gtx disassembler decorates labels with $ prefix, but
the gtirb disassembler (for AT&T syntax) does not."
  (if (and (intel-syntax-p asm) (char= (char name 0) #\$))
      (subseq name 1)
      name))

(defun function-label-p (label-name)
  "Label represents a valid function name.
The heuristic is that, in Intel syntax, if it starts with #\$ and
doesn't start with the known prefixes that are auto-generated in
the code, we consider it a function name. In AT&T syntax the .L
prefix is used for auto-generated labels, and if it isn't one of
those we assume a function name."
  (let ((name (string-upcase label-name)))
    (and
     (is-asm-symbol name)
     (not
      (or (starts-with-subseq "$LOC_" name)
          (starts-with-subseq "BB_FALLTHROUGH_" name)
          (starts-with-subseq "UNK_" name)
          (starts-with-subseq ".L" name))))))

(defun line-is-function-label (asm-line-info)
  "Returns true if the passed line-info represents a name of a function."
  (and (asm-line-info-label asm-line-info)
       (function-label-p (asm-line-info-label asm-line-info))))

(defun extract-function-name-from-decl (decl-name)
  (subseq decl-name 1 (position #\. decl-name)))

(defun extract-function-declarations (asm)
  "Traverse the asm-heap, and collect declarations associated with
 each function. The function name is determined from the declaration.
 The declarations are stored in a hash-table, keyed to the function
 name. The resulting hash-table is returned."
  (let ((table (make-hash-table :test 'equalp))
	(genome (genome asm)))
    (iter (for x in-vector genome)
	  (if (and
	       (eq (asm-line-info-type x) ':decl)
	       (let* ((tok (first (asm-line-info-tokens x)))
		      (pos (position #\. tok)))
		 (and pos (> pos 1))))
	      (push
               x
               (gethash
                (extract-function-name-from-decl
                 (first (asm-line-info-tokens x)))
                table))))
    table))

(defun create-asm-function-index (asm)
  "Extracts a function index from an assembly file.
 Traverse the passed asm-heap, and collect a function-index-entry
 for each function. The result is a vector of function-index-entry."
  (let ((table (if (intel-syntax-p asm)
		   (extract-function-declarations asm)
		   (make-hash-table :test 'equalp)))
	(entries '())
	(i 0)
	(genome (genome asm)))
    (iter (while (< i (length genome)))
	  (let ((info (elt genome i)))
	    (if (line-is-function-label info)
		;; look for the end of the function
		(let* ((start-index i)
		       (name (function-name-from-label
                              (asm-line-info-label info) asm))
                       (start-addr (asm-line-info-address info))
                       (end-addr start-addr)
		       (leaf t))
		  (incf i)
		  (iter (while (< i (length genome)))
			(let ((info2 (elt genome i)))
                          ;; If the first line didn't have an address specified,
                          ;; get it from the first line that does have one.
                          (unless start-addr
                            (setf start-addr (asm-line-info-address info2)))
                          (if (asm-line-info-address info2)
                              (setf end-addr (asm-line-info-address info2)))
			  (if (member (asm-line-info-opcode info2)
                                      '("call" "callq") :test 'equalp)
			      (setf leaf nil)) ;found a call, so not a leaf
			  (when (or
                                 (= i (1- (length genome)))  ;; end of genome
				 (and
				  (eq (asm-line-info-type info2)
				      :decl)
				  (member (first (asm-line-info-tokens info2))
                                          '("align" ".align" ".text")
                                          :test 'equalp))
				 (and (line-is-function-label info2)
                                        ; ignore duplicate function labels
				      (not
				       (equalp (asm-line-info-label info)
					       (asm-line-info-label info2)))))
			    (push
			     (make-function-index-entry
			      :name name
			      :start-line start-index
			      :start-address start-addr
			      :end-line (if (= i (1- (length genome))) i (- i 1))
			      :end-address end-addr
			      :is-leaf leaf
			      :declarations (gethash name table)) entries)
			    (return)))
			(incf i)))))
	  (incf i))
    (if (intel-syntax-p asm)
	(setf entries
              (remove-if (lambda (x)
                           (or (null (function-index-entry-start-address x))
                               (null (function-index-entry-end-address x))))
                         (nreverse entries))))
    (make-array (length entries) :initial-contents entries)))

(defun asm-labels (asm)
  "Return a hashtable of the labels in the asm-heap"
  (let ((ht (make-hash-table :test 'equalp)))
      (iter (for x in-vector (genome asm))
	    (and (asm-line-info-label x)
		 (setf (gethash
			(function-name-from-label (asm-line-info-label x) asm)
			ht) t)))
      ht))

(defgeneric load-function-bounds (software-obj file)
  (:documentation
   "Load function boundaries into FUNCTION-BOUNDS for OBJ from FILE.")
  (:method ((asm asm-heap) file)
    (flet ((get-hex-val (s)
             (when (starts-with-subseq "0x" s)
               (values (parse-integer s :start 2 :radix 16))))
           (index-of-addr (asm addr)
             ;; Return the line index (0-based) of the line of the
             ;; genome which matches the passed address.
             (position addr (genome asm)
                       :test 'eql :key 'asm-line-info-address)))
      (when (probe-file file)
        (let ((valid-fn-labels (asm-labels asm))
	      (entries '())
	      (table (extract-function-declarations asm)))
          (with-open-file (in file)
            (iter (for line = (read-line in nil))
                  (while line)
                  ;; Line is formatted:
                  ;; fn-name start-ea end-ea leaf-or-non-leaf
                  (let ((split-line (split "\\s+" line)))
                    (when (>= (length split-line) 4)
                      (let ((fn-name (first split-line))
                            (start-ea (second split-line))
                            (end-ea (third split-line)))
                        ;; Check valid start/end EAs
                        (when (and (not (string= "BADEA" start-ea))
                                   (not (string= "BADEA" end-ea))
                                   ;; Only keep functions matching labels in OBJ
                                   (gethash fn-name valid-fn-labels))
		          (let ((start-addr (get-hex-val start-ea))
			        (end-addr (get-hex-val end-ea)))
			    (push
			     (make-function-index-entry
			      :name fn-name
			      :start-line (index-of-addr asm start-addr)
			      :start-address start-addr
			      :end-line (index-of-addr asm end-addr)
			      :end-address end-addr
			      :is-leaf (string= "leaf" (fourth split-line))
			      :declarations (gethash fn-name table))
			     entries))))))))
          (setf entries (remove-if
		         (lambda (x)
		           (or (null (function-index-entry-start-address x))
			       (null (function-index-entry-end-address x))))
		         (nreverse entries)))
          (make-array (length entries) :initial-contents entries))))))

(defun is-call-statement (info)
  (member (asm-line-info-opcode info) '("call" "callq") :test 'equalp))

;;;
;;; This is NASM Intel syntax-specific
;;; It assumes an external symbol looks like this:
;;;     $__ctype_b_loc@@GLIBC_2.3
;;;     or
;;;     memchr@PLT
;;;
(defun call-target (info)
  (and (is-call-statement info)
       (caar (asm-line-info-operands info))))

(defun is-extern-call-target (target)
  "Given the name of the target of a CALL, returns true iff it is an extern
   function."
  (and target (search "@" target)))

(defun is-extern-call-statement (info)
  (is-extern-call-target (call-target info)))

(defun call-targets (asm-heap)
  "Return the names of functions being called in the asm code.
The format is:
  (:name <string> :library <string-library-name> :full-name <qualified name>)."
  (iter
    (for info in-vector (genome asm-heap))
      (if (is-call-statement info)
        (let ((target (call-target info))
              (result nil))
          (if (is-extern-call-target target)
            (let* ((pos (search "@" target))
                   (name (and pos (subseq target 0 pos)))
                   (library
                    (and pos
                         (string-trim '(#\@) (subseq target (+ pos 2))))))
              (setf result
                (list ':name name ':library library ':full-name target)))
            (setf result
                (list ':name target ':library nil ':full-name target)))
          (collect result)))))

(defun create-ranked-function-index (asm)
  "Create a table of functions with ranking according to how
   many call statements are found in the function code."
  (let ((entries '())
	(i 0)
	(genome (genome asm)))
    (iter (while (< i (length genome)))
	  (let ((info (elt genome i)))
	    (if (line-is-function-label info)
		;; look for the end of the function
		(let ((name (function-name-from-label
			     (asm-line-info-label info)
			     asm))
		      (local-call-count 0)
		      (line-count 1)
		      (extern-call-count 0))
		  (incf i)
		  (iter (while (< i (length genome)))
			(incf line-count)
			(let ((info2 (elt genome i)))
			  (when
			      (is-call-statement info2)
			    (if (is-extern-call-statement info2)
				(incf extern-call-count)
				(incf local-call-count)))
			  (when (or
				 (member (asm-line-info-opcode info2)
					 '("ret" "retq" "hlt")
					 :test 'equalp)
				 (and
				  (eq
				   (asm-line-info-type info2)
				   :decl)
				  (member (first (asm-line-info-tokens info2))
				      '("align" ".align") :test 'equalp))
				 (and (line-is-function-label info2)
				      ; ignore duplicate function labels
				      (not
				       (equalp (asm-line-info-label info)
					       (asm-line-info-label info2)))))
			    (push
			     (list
			      (list ':name name)
			      (list ':local local-call-count)
			      (list ':extern extern-call-count)
			      (list ':total
				    (+ local-call-count extern-call-count))
			      (list ':size line-count))
			     entries)
			    (return)))
			(incf i)))))
	  (incf i))
    (sort entries '<
	  :key (lambda (x) (second (first (member ':total x :key 'car)))))))
