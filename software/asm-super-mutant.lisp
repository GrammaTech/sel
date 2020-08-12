;;; asm-super-mutant.lisp --- SUPER-MUTANT for single assembly functions
;;;
;;; @subsubsection ASM-SUPER-MUTANT Overview
;;;
;;; ASM-SUPER-MUTANT software objects are designed to provide the
;;; benefits of SUPER-MUTANT to assembly programs.  Its primary
;;; advantage is the ability to evaluate the fitness of large numbers
;;; of variants of a given target function, in a single process, in a
;;; minimal amount of time.  Avoiding the per-process overhead in
;;; fitness evaluations results in orders-of-magnitude improvements of
;;; efficiency.
;;;
;;; The ASM-SUPER-MUTANT has been tested with AT&T and Intel syntax
;;; and for nasm (Intel) and gas (AT&T) assemblers. Like ASM-HEAP,
;;; it won't work for arbitrary macro-heavy assembler code.
;;;
;;; The ASM-SUPER-MUTANT derives from an ASM-HEAP and also
;;; contains the SUPER-MUTANT methods.
;;;
;;; The ASM-SUPER-MUTANT initially contains a complete program
;;; executable (initialized from an asm format file).  In addition it
;;; contains a specification of a target function within the overall
;;; program.  The target function (specified either by name or as
;;; start and end addresses) is used to mark a range of lines within
;;; the overall program (returned by CREATE-TARGET).
;;;
;;; Like a SUPER-MUTANT, the ASM-SUPER-MUTANT maintains a collection
;;; of MUTANTS, each one being an instance of the ASM-HEAP software
;;; object.  Each MUTANT is a variant of the target function.
;;;
;;; @subsubsection Fitness Evaluation
;;;
;;; To perform fitness evaluation, the ASM-SUPERMUTANT performs the
;;; following steps:
;;;
;;; * The MUTANTS are combined into a single assembly file (.asm),
;;;   which also includes the data sections and declarations necessary
;;;   to be able to assemble. Every MUTANT is given a unique label
;;;   (variant_0, (variant_1, ... variant_n), and those labels are
;;;   exported.
;;;
;;; * Input/output data, see @pxref{i-o-file-format, I/O File Format},
;;;   is appended to the asm file, and is used to determine fitness of
;;;   each variant.  A set of I/O data (memory and register contents)
;;;   is provided for each test run of the target function.
;;;
;;; * The .asm file is assembled with NASM.
;;;
;;; * A C harness file (asm-super-mutant-fitness.c), which runs and
;;;   evaluates variants, is compiled and linked to the compiled
;;;   object file.  The C code provides functions to set up each test,
;;;   execute each variant, calculate the variant's fitness and
;;;   aggregate results.  Currently the fitness is set to the number
;;;   of executed instructions as collected by the linux Performance
;;;   Application Programming Interface (PAPI)
;;;   (@url{http://icl.utk.edu/papi/}) using the PAPI_TOT_INS event.
;;;   The harness contains some sandboxing code to handle differences
;;;   in execution address of the original test runs and the fitness
;;;   runs, manage heap memory pages, and trap and handle segment
;;;   violations and function crashes.
;;;
;;; * Each variant is assigned an array of fitness results, with each
;;;   item in in the array representing the number of instructions
;;;   required to execute the function on a given test run (i.e., I/O
;;;   pair).  If the variant did not properly pass the tests (I/O data
;;;   did not match) then MAXINT is assigned as the fitness variable
;;;   for that test run (since smaller is better, this is the worst
;;;   possible fitness).
;;;
;;; * The fitness harness writes an array of all test results for all
;;;   variants to the *STANDARD-OUTPUT* stream.
;;;
;;; * The compiled/linked fitness executable is executed by the
;;;   fitness test and the results (written to *STANDARD-OUTPUT*) are
;;;   parsed and stored in Lisp data structures (as an array of
;;;   arrays). This is cached on the ASM-SUPER-MUTANT instance, and
;;;   can be obtained with the FITNESS method.
;;;
;;; @subsubsection Tool Dependencies
;;;
;;; The ASM-SUPER-MUTANT currently depends upon:
;;; * Nasm to assemble the generated fitness program
;;; * Clang (version 6 or later) to compile and link the fitness program.
;;; * Assembly code or a high quality lifter (e.g., GrammaTech's
;;;   CodeSurfer for Binaries) to disassemble an original binary
;;;   program.
;;; * A method of collecting binary I/O pairs from a series of dynamic
;;;   tests in the format described below,
;;;   @pxref{i-o-file-format, I/O File Format}
;;;
;;; The ASM-HEAP, which is the basis for ASM-SUPER-MUTANT and all its
;;; variants, uses Intel-format assembly code (Nasm-compatible).
;;; In the near future we are planning to switch to AT&T assembler syntax
;;; and to replace Nasm with a more efficient assembler.
;;;
;;; @anchor{i-o-file-format}
;;; @subsubsection I/O File Format
;;;
;;; The file is ASCII.  In internal tests it s generated using the IBM
;;; PIN tool to observe the execution of a binary program on a test
;;; suite.
;;;
;;; An I/O file contains 1 or more test runs, and each test run
;;; comprises 2 sections: Input Data and Output Data.
;;;
;;; The format of Input Data is identical to Output Data, except that
;;; the set of registers that are included may differ.
;;;
;;; Each section (Input Data or Output Data) contains a section of
;;; register lines, followed by a section of memory lines.
;;;
;;; In the register section, the values of all significant registers
;;; are specified, one per line. This includes general-purpose
;;; registers (GPR) i.e. rax rbx, etc. followed by floating-point
;;; registers ymm0-15.
;;;
;;; Following the registers are values of relevant memory
;;; addresses. These include any memory addresses read or written by
;;; the function being tested.  If the function depends on a global
;;; variable for instance, it will be included.
;;;
;;; In the first section (registers) each line contains:
;;; * The name of the register, followed by the bytes in big-endian order,
;;; * The register name is separated from the bytes by whitespace, but any
;;;   other whitespace on the line should be ignored.
;;;
;;; For example, the line,
;;;
;;;     %rax    00 00 00 00 00 00 01 00
;;;
;;; indicates that register rax should contain the value 256.
;;;
;;; For the general-purpose registers rax, rcx, rdx, rbx, rsp, rbp,
;;; rsi, rdi, r8, r9, r10, r11, r12, r13, r14, and r15, all eight
;;; bytes will be explicitly included on the line.  For the SIMD
;;; registers ymm0-ymm15, all 32 bytes will be explicit.
;;;
;;; Memory would be specified with one 8-byte long per line, in
;;; big-endian order, consisting of an address, followed by a mask
;;; indicating which bytes are live, followed by the bytes
;;; themselves. The mask would be separated from the address and bytes
;;; by whitespace, but again any other whitespace on the line should
;;; be ignored.
;;;
;;; For example, the line,
;;;
;;;     00007fbb c1fcf768   v v v v v v v .   2f 04 9c d8 3b 95 7c 00
;;;
;;; indicates that the byte at address 0x7fbbc1fcf768 has value 0x00,
;;; the byte at 0x7fbbc1fcf769 has value 0x7c, and so forth
;;; (big-endian order).  Note that bytes 0x7fbbc1fcf769-0x7fbbc1fcf76f
;;; are live (indicated with "v") while 0x7fbbc1fcf768 is not
;;; (indicated with ".").
;;;
;;; @subsubsection Components
;;;
;;; ASM-SUPER-MUTANT software object consists of:
;;;
;;; * The ASM-HEAP, which is typically based on a file of assembly
;;;   source code.  The ASM-SUPER-MUTANT object contains the whole original
;;;   binary application (in assembler source format).
;;;
;;; * Input/output specifications, in a specific file format as
;;;   generated using the Intel monitoring application and our Python
;;;   scripts
;;;
;;; * Data object original addresses ("sanity file").
;;;
;;; * Function boundary deliminators which determines the target of
;;;   the mutation and evaluation.
;;;
;;; Note that the struct INPUT-SPECIFICATION is a bit of a misnomer as
;;; it is used here for both input specification and output
;;; specification (their formats are identical so we use the same
;;; struct for both).
;;;
;;; SUPER-MUTANT slots:
;;; * MUTANTS will contain a list of ASM-HEAP objects.
;;; * SUPER-SOFT caches a combined ASM-HEAP representing the output
;;;   fitness program.
;;; * PHENOME-RESULTS caches the results obtained from calling the PHENOME
;;;   method.
;;;
;;; @subsubsection Current Limitations
;;;
;;; * Functions which use floating point data as input or outputs will
;;;   not evaluate correctly. The fitness file that is generated does
;;;   not yet handle @code{ymm0}-@code{ymm15} registers, which are used to
;;;   pass floating point values in x86_64 binaries.
;;; * Currently only leaf functions (functions which do not call any other
;;;   functions) are supported.
;;; * The fitness test program does not do full sandboxing. It does protect
;;;   against segment violations (those will typically result in a
;;;   +worst-c-fitness+ rating) but a function variant could potentially write
;;;   on other code or data without triggering a segment violation, and this
;;;   will not get trapped. When that happens it will possibly invalidate
;;;   further fitness tests, or cause the whole fitness program to crash,
;;;   resulting in all variants to come back as +worst-c-fitness+.
;;;
;;; @subsubsection Installing PAPI on Ubuntu
;;; Fitness evaluation requires the PAPI component and the Linux Perf
;;; functionality. Building the fitness evaluation program (on the fly
;;; during fitness evaluation) requires a C program to compile and link
;;; to PAPI. This also requires a .h file to compile.
;;;
;;; To install papi:
;;;
;;;      sudo apt-get install papi-tools
;;;
;;; To install perf:
;;;
;;;     sudo apt-get install linux-tools-common linux-tools-generic linux-tools-`uname -r`
;;;
;;; Perf requires system permission to run. In linux, the value in
;;; /proc/sys/kernel/perf_event_paranoid should be set to 1
;;; (Disallow CPU event access by users without CAP_SYS_ADMIN).
;;; This is typically set to 3 (maximum paranoia) by default.
;;;
;;; This can be accomplished with the following:
;;;   sudo sh -c 'echo 1 >/proc/sys/kernel/perf_event_paranoid'
;;;   sudo sysctl -w kernel.perf_event_paranoid=1
;;;   You may need to use:
;;;    sudo sh -c 'echo kernel.perf_event_paranoid=1 > /etc/sysctl.d/local.conf'
;;;    and then reboot for it to take effect.
;;;
;;; It's also a good idea to turn off randomizing of address base
;;; (to get more consistency):
;;;   sudo sh -c 'echo 0 >/proc/sys/kernel/randomize_va_space'
;;;   sudo sysctl -w kernel.randomize_va_space=0
;;;   sudo sh -c 'echo kernel.randomize_va_space=0 >> /etc/sysctl.d/local.conf'
;;;
;;; To get necessary include (.h) file for compiling C harness with PAPI
;;; (needed to perform fitness evaluation):
;;;
;;;      sudo apt-get install libpapi-dev
;;;
;;; Then you should be able to enter
;;;
;;; papi_avail
;;;
;;; and see a list of available PAPI events.
;;; ASM-SUPER-MUTANT requires the use of the PAPI_TOT_INS event (total number
;;; of instructions executed).
;;; After installation, the PAPI library should be found in one of these
;;; locations:
;;;     /usr/lib/x86_64-linux-gnu/libpapi.so
;;;     /usr/lib/libpapi.so.5.6.1
;;;
;;; @texi{asm-super-mutant}

(defpackage :software-evolution-library/software/asm-super-mutant
  (:nicknames :sel/software/asm-super-mutant :sel/sw/asm-super-mutant)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/asm
        :software-evolution-library/software/asm-heap
        :software-evolution-library/software/super-mutant)
  (:export :asm-super-mutant
           :var-table
           :*lib-papi*
           :fitness-harness
           :load-io-file
           :target-function
           :target-function-name
           :create-all-simple-cut-variants
           :create-target
           :data-path
           :data-asm
           :target-info
           :evaluate-asm
           :eval-meta-results
           :leaf-functions
           :parse-sanity-file
           :restore-original-addresses
           :input-specification
           :input-spec
           :output-spec
           :compiler
           :static-link
           :untraced-call-spec
           :input-specification-regs
           :input-specification-mem
           :reg-contents
           :reg-contents-name
           :reg-contents-value
           :*optimize-included-lines*
           :*inline-included-lines*
           :*size-affects-fitness*
           :*bss-segment-start*
           :*data-segment-start*
           :*text-segment-start*
           :*seldata-segment-start*
           :*timeout-seconds*
           :*callee-saved-init-value*
           :*keep-assembly-files*
           :*keep-fitness-executables*))

(in-package :software-evolution-library/software/asm-super-mutant)
(in-readtable :curry-compose-reader-macros)
(define-software asm-super-mutant (asm-heap super-mutant)
  ((input-spec
    :initarg :input-spec
    :reader input-spec
    :initform (make-array 0 :fill-pointer 0 :adjustable t)
    :documentation
    "Vector of INPUT-SPECIFICATION structs, one for each test case.")
   (output-spec
    :initarg :output-spec
    :reader output-spec
    :initform (make-array 0 :fill-pointer 0 :adjustable t)
    :documentation
    "Vector of INPUT-SPECIFICATION structs, one for each test case.")
   (untraced-call-spec
    :initarg :untraced-call-spec
    :reader untraced-call-spec
    :initform (make-array 0 :fill-pointer 0 :adjustable t)
    :documentation
    "Vector of UNTRACED-CALL-SPECIFICATION structs, one for each test case.")
   (var-table
    :initarg :var-table
    :accessor var-table
    :initform nil
    :documentation "Vector of var-rec (data/address records)")
   (bss-segment
    :initarg :bss-segment
    :accessor bss-segment
    :initform nil
    :documentation "Address of bss segment in original executable")
   (target-name
    :initarg :target-name
    :accessor target-name
    :initform nil
    :documentation "Name of target function")
   (target-start-index
    :initarg :target-start-index
    :accessor target-start-index
    :documentation "Integer index represents the first line of target code.")
   (target-end-index
    :initarg :target-end-index
    :accessor target-end-index
    :documentation "Integer index represents the last line of target code.")
   (target-info
    :initarg :target-info
    :accessor target-info
    :initform nil
    :documentation "Function index entry of the target function")
   (target-lines
    :initarg :target-lines
    :accessor target-lines
    :documentation
    "Cache the lines of the target code, as they are used often.")
   (compiler
    :initarg :compiler
    :reader compiler
    :initform "clang"
    :documentation "C compiler to use for building fitness harness.")
   (static-link
    :initarg :static-link
    :reader static-link
    :initform nil
    :documentation "If true, attempt static link.")
   (io-dir
    :initarg :io-dir
    :reader io-dir
    :initform nil
    :documentation "Directory containing I/O files, named for the functions.")
   (io-file
    :initarg :io-file
    :accessor io-file
    :initform nil
    :documentation "If this is specified, use this file (ignore io-dir).")
   (data-path
    :initarg :data-path
    :accessor data-path
    :initform nil
    :documentation
    "If specified, refers to asm file containing static data definitions")
   (data-asm
    :accessor data-asm
    :initform nil
    :documentation
    "If present, contains the asm-heap corresponding to data-path")
   (static-names
    :accessor static-names
    :initform nil
    :documentation
    "If present, contains property list mapping static names to addresses")
   (page-block-list
    :initarg :page-block-list
    :accessor page-block-list
    :initform nil
    :documentation "Derived from io-file data, memory blocks which need to be
 allocated by the fitness executables")
   (fitness-harness
    :initarg :fitness-harness
    :accessor fitness-harness
    :initform "./asm-super-mutant-fitness.c"
    :documentation "Pathname to the fitness harness file (C program source)")
   (include-lines
    :initarg :include-lines
    :accessor include-lines
    :initform nil
    :documentation
    "Optional list of lines of assembler source to include in fitness file.")
   (include-funcs
    :initarg :include-funcs
    :accessor include-funcs
    :initform nil
    :documentation
    "Optional list of names of functions to include in fitness file.")
   (libraries
    :initarg :libraries
    :accessor libraries
    :initform nil
    :documentation
    "Optional list of names of functions to include in fitness file.")
   (max-asm-function-size
    :initarg :max-asm-function-size
    :accessor max-asm-function-size
    :initform 1000
    :documentation "Maximum number of lines in a function for optimization.")
   (eval-meta-results
    :initarg :eval-meta-results
    :accessor eval-meta-results
    :initform nil
    :documentation
    "After evaluation, may contain meta-information about results.")
  (linker-script
   :initarg :linker-script
   :accessor linker-script
   :initform nil
   :documentation "Create and cache the default linker script"))

  (:documentation
   "Combine SUPER-MUTANT capabilities with ASM-HEAP framework."))

;; C 64-bit unsigned long MAXINT, is the worst possible fitness score
(defconstant +worst-c-fitness+ #xffffffffffffffff)

;; maximum size of untraced-call-log (per variant)
(defconstant +max-untraced-call-results+ #x2000)

(defparameter *optimize-included-lines* nil
  "This option is no longer supported")

(defparameter *inline-included-lines* nil
  "Inline the additional lines of assembler.
 Inline them with the function being modified, so each variation
 contains these lines.")

(defparameter *timeout-seconds* 60
  "Number of seconds before the fitness process is timed out.")

(defparameter *reverse-bytes-in-io-file* nil
  "If true, reverse the bytes in the memory spec within the i/o file.")

(defparameter *inline-label-counter* 0
  "Used to generate unique labels for inlining.")

(defparameter *all-registers*
  '("rax"
    "rbx"
    "rcx"
    "rdx"
    "rsp"
    "rbp"
    "rsi"
    "rdi"
    "r8"
    "r9"
    "r10"
    "r11"
    "r12"
    "r13"
    "r14"
    "r15")
  "Set of all possible registers on x86_64.")

(defparameter +num-regs+ (length *all-registers*))

(defparameter *input-registers*
  '("rax"
    "rbx"
    "rcx"
    "rdx"
    "rsp"
    "rbp"
    "rsi"
    "rdi"
    "r8"
    "r9"
    "r10"
    "r12"
    "r13"
    "r14"
    "r15")
  "All registers which are live at input.")

(defparameter *output-registers*
  '("rax"
    "rbx"
    "rdx"
    "rsp"
    "rbp"
    "r12"
    "r13"
    "r14"
    "r15")
  "All registers which are live at output.")

(defparameter *break-on-fitness-failure* nil
  "If true, evolve stop with an error when the fitness executable fails.")

(defparameter *keep-fitness-executables* nil
  "If true, evaluate will keep generated fitness executables.")

(defparameter *keep-assembly-files* nil
  "If true, evaluate will keep generated assembly files.")

(defparameter *bss-segment-start* nil "If non-nil, address of .bss segment")
(defparameter *data-segment-start* nil "If non-nil, address of .data segment")
(defparameter *text-segment-start* #x400000 "If non-nil, base address")
(defparameter *seldata-segment-start* nil
  "If non-nil, address of .seldata segment")

(defconstant +page-size+ #x1000)
(defconstant +page-mask+ #xfffffffffffff000)
(defconstant +page-offset-mask+ (- +page-size+ 1))

(defun page-addr (addr)
  "Convert address to page address"
  (logand addr +page-mask+))
(defun page-offset (addr)
  "Return offset into page of addr"
  (logand addr +page-offset-mask+))

(defstruct page-record
  addr      ; address of page-aligned memory block
  size      ; length (in bytes)--should be multiple of +page-size+
  attr)     ; either "r" or "rw" -- for now, just supporting "rw"

(defmethod print-object ((pr page-record) stream)
  (format stream "#S(PAGE-RECORD :ADDR #x~X :SIZE #x~X :ATTR ~S)"
	  (page-record-addr pr)
	  (page-record-size pr)
	  (page-record-attr pr)))

;;;
;;; all the SIMD register names start with 'y'
;;;
(defun simd-reg-p (name) (char= (elt name 0) #\y))

(defstruct memory-spec
  (addr nil)   ; 64-bit address as an int
  (mask nil)   ; bit set for each live byte starting at addr,
					; low-bit (bit 0) = addr,
                                        ; bit 1 = addr+1, etc.
  (bytes nil)) ; 8 bytes starting at addr

(defun bytes-to-string (ba)
  (format nil "~{ ~2,'0X~}" (concatenate 'list ba)))

(defmethod print-object ((mem memory-spec) stream)
  (format stream "~16,'0X: ~T~A ~A"
	  (memory-spec-addr mem)
	  (memory-spec-mask mem)
	  (bytes-to-string (memory-spec-bytes mem))))

(defstruct reg-contents
  (name nil)     ; name of register (string) i.e. "rax", "ymm1", etc.
  (value nil))   ; integer value (64 bits for gen. purpose, 256 bit for SIMD)

(defmethod print-object ((reg reg-contents) stream)
  (format stream "~4A: ~A" (reg-contents-name reg)
	  (bytes-to-string (reg-contents-value reg))))

;;;
;;; This struct also is used to specify outputs.
;;;
(defstruct input-specification
  (regs nil)
  (simd-regs nil)
  (mem nil))   ;; vector of memory-spec to indicate all memory inputs

(defmethod print-object ((spec input-specification) stream)
  (print-unreadable-object (spec stream)
    (format
     stream
     "input-specification: ~D registers, ~D SIMD registers, ~D memory addrs"
     (length (input-specification-regs spec))
     (length (input-specification-simd-regs spec))
     (length (input-specification-mem spec)))))

(defstruct untraced-call
  name
  args    ; list of args passed
  result) ; returned value

(defstruct untraced-call-specification
  calls) ;; vector of UNTRACED-CALL

(defmethod initialize-instance :after ((instance asm-super-mutant)
				       &rest initargs)
  (declare (ignore initargs))
  ;; if a path was assigned to var-table
  ;; parse it and replace the value with the table
  (if (or (stringp (var-table instance)) (pathnamep (var-table instance)))
      (setf (var-table instance)
	    (parse-sanity-file (var-table instance)))))

(defmethod from-file :after ((asm asm-super-mutant) file)
  "Set function target after the file loads."
  ;; if target-name non-nil, set the target
  (declare (ignore file))
  (if (target-name asm)
      (target-function-name asm (target-name asm)))
  asm)

;;;
;;; Store name and address of data variables
;;;
(defstruct var-rec
  (name nil)     ; string, name of variable
  (type nil)     ; string, "b", "r", "d", "?"
  (address nil)) ; integer address

;;; whitespace handling
;;;
(defun is-whitespace (c)
  (member c '(#\space #\linefeed #\newline #\tab #\page)))

(defun get-next-line (input)
  (let ((line (read-line input nil 'eof)))
    (if (stringp line)
	(trim-whitespace line))))  ;; returns nil if end-of-file

(defparameter *fitness-harness* "./asm-super-mutant-fitness.c")

(defparameter *size-affects-fitness* nil)

;;;
;;; The string argument should consist only of hex digits (at least in the first
;;; num * 2 characters). The number argument is the number of bytes to parse.
;;; Returns a byte array.
;;;
(defun parse-bytes (num str)
  (let ((result (make-array num :element-type '(unsigned-byte 8))))
    (dotimes (i num)
      (setf (aref result i)
	    (+ (* (digit-char-p (char str (* i 2)) #x10) #x10)
	       (digit-char-p (char str (+ 1 (* i 2))) #x10))))
    result))


;;;
;;; Returns a 64-bit integer representing an address, and the line position
;;; immediately following the address. The address should be in the format:
;;;   xxxxxxxx xxxxxxxx
;;; (16 hex digits, with 8 digits + space + 8 digits = 17 chars)
;;;;
(defun parse-address (line)
  (let ((result 0))
    (dotimes (i 8)
      (setf result (+ (* result #x10)
		      (digit-char-p (char line i) #x10))))
    (dotimes (i 8)
      (setf result (+ (* result #x10)
		      (digit-char-p (char line (+ i 9)) #x10))))
    (values result 17)))

(defun parse-mem-spec (line)
  (multiple-value-bind (addr pos)
      (parse-address line)
    (iter (while (is-whitespace (char line pos))) (incf pos)) ; skip spaces
    (let ((b (make-array 8 :element-type 'bit)))
      (dotimes (i 8)
	(setf (bit b i)
	      (if (char= (char line pos) #\v) 1 0))
	(incf pos 2))
      (let ((bytes (parse-bytes 8 (remove #\space (subseq line pos)))))
        (if *reverse-bytes-in-io-file*
            (setf bytes (nreverse bytes)))
        (make-memory-spec :addr addr
                          :mask b
                          :bytes bytes)))))

(defun parse-reg-spec (line)
  (let* ((pos 0)
         (name               ; get the register name (string)
	  (do ((c (char line (incf pos))(char line (incf pos)))
	       (chars '()))
	      ((is-whitespace c)
	       (concatenate 'string (nreverse chars)))
	    (push c chars))))
    (if (simd-reg-p name)  ; was it a SIMD register?
        (make-reg-contents
	 :name name
	 :value (parse-bytes 32
			     (remove #\space (subseq line pos))))
	;; else a general-purpose register
	(make-reg-contents
	 :name name
	 :value (parse-bytes 8
			     (remove #\space (subseq line pos)))))))

(defparameter *untraced-call-regx*
  "([a-zA-Z0-9@]*)\\({1}([a-fA-F0-9\\, ]*)\\){1}( = [a-fA-F0-9]*)?"
  "Example: malloc@plt(1A) = 1A00000")

(defun parse-untraced-call (line)
  (multiple-value-bind (start end starts ends)
      (cl-ppcre:scan  *untraced-call-regx* line)
    (declare (ignore start end))
    (if (and (> (length starts) 1) (> (length ends) 1))
        (let ((name (subseq line (elt starts 0) (elt ends 0)))
              (args (subseq line (elt starts 1) (elt ends 1)))
              (result (if (integerp (elt starts 2))
                          (subseq line (+ (elt starts 2) 3) (elt ends 2)))))
          (make-untraced-call
           :name name
           :args (mapcar
                  (lambda (x) (parse-integer x :radix 16))
                  (split-sequence #\, args))
           :result (if result (parse-integer result :radix 16)))))))

(defun new-io-spec ()
  (make-input-specification
   :regs (make-array 16 :fill-pointer 0)
   :simd-regs (make-array 16 :fill-pointer 0)
   :mem (make-array 0 :fill-pointer 0 :adjustable t)))

(defun new-untraced-call-spec ()
  (make-untraced-call-specification
   :calls (make-array 8 :fill-pointer 0 :adjustable t)))

(defun sort-registers (reg-list)
  (let ((reg-values
         (iter (for i from 0)
               (for reg in *all-registers*)
               (collect (cons reg i)))))
    (sort (copy-list reg-list) '<
          :key (lambda (r)(cdr (find r reg-values :test 'equalp :key 'car))))))

(defun sort-reg-contents (reg-contents-vec)
  (let ((reg-values
         (iter (for i from 0)
               (for reg in *all-registers*)
               (collect (cons reg i)))))
    (sort (copy-array reg-contents-vec) '<
          :key (lambda (r)
                 (cdr (find (reg-contents-name r) reg-values
                            :test 'equalp
                            :key 'car))))))

(defparameter *callee-saved-init-value*
  (make-array 8
              :element-type '(unsigned-byte 8)
              :initial-contents '(#x0 #x0 #xF0 #xF #x0 #xF0 #xF #x0))
                                        ; #xf00f00f00f00
  "Arbitrary value to initialize callee-save registers which are
 not otherwise initialized by the io spec.")

(defparameter *callee-saved-registers*
  '("rsp" "rbx" "rbp" "r12" "r13" "r14" "r15")
  "Registers that must be preserved by any function.")

(defun create-required-reg-specs (input-specs output-specs)
  "Ensure callee-save registers are handled.
 Callee-save registers are: rsp, rbx, rbp, r12-r15."
  (flet ((add-input-reg (input-regs reg)
           (unless (find reg input-regs :key 'reg-contents-name :test 'equal)
             (vector-push
              (make-reg-contents :name reg :value *callee-saved-init-value*)
              input-regs)))
         (add-output-reg (input-regs output-regs reg)
           ;; if already there, don't add
           (unless (find reg output-regs :key 'reg-contents-name :test 'equal)
             (let ((reg-spec
                    (find reg input-regs :key 'reg-contents-name :test 'equal)))
               (vector-push
                (if reg-spec
                    (copy-reg-contents reg-spec)
                    (make-reg-contents :name reg
                                       :value *callee-saved-init-value*))
                output-regs)))))
    (dolist (reg *callee-saved-registers*)
      (add-input-reg (input-specification-regs input-specs) reg)
      (add-output-reg (input-specification-regs input-specs)
                      (input-specification-regs output-specs)
                      reg))))

(defun add-unread-mem-addresses (input-spec output-spec)
  "Ensure that any addresses which get written but not read are initialized
 at the start, so verify they are written correctly."
  ;; Subtract the set of input addresses from the set of output addresses
  ;; to obtain the set of addresses which need to be added to input
  ;; initialization.
  (let* ((input-mem-set
          (coerce (input-specification-mem input-spec) 'list))
         (output-mem-set
          (coerce (input-specification-mem output-spec) 'list))
         (diff-set (set-difference output-mem-set input-mem-set
                                   :key 'memory-spec-addr
                                   :test '=)))
    ;; add entries to the input spec which correspond with those
    ;; entries in the output spec which are written but not read
    (dolist (x diff-set)
      (vector-push-extend
       (make-memory-spec :addr (memory-spec-addr x)
                         :mask (memory-spec-mask x)
                         :bytes *callee-saved-init-value*)
       (input-specification-mem input-spec)))))

(defun load-io-file (super-asm filename)
  "Load the file containing input and output state information"
  (let ((input-spec (new-io-spec))
	(output-spec (new-io-spec))
        (untraced-call-spec (new-untraced-call-spec))
        (state :input))
    (with-open-file (input filename :direction :input)
      (do ((line (get-next-line input) (get-next-line input)))
	  ((null line)
           (when (or
                  (> (length (input-specification-regs output-spec)) 0)
                  (> (length (input-specification-regs input-spec)) 0)
                  (> (length (input-specification-mem output-spec)) 0)
                  (> (length (input-specification-mem input-spec)) 0))
             ;; ensure callee-saved registers are preserved
             (create-required-reg-specs input-spec output-spec)
             (setf (input-specification-regs input-spec)
                   (sort-reg-contents
                    (input-specification-regs input-spec)))
             (setf (input-specification-regs output-spec)
                   (sort-reg-contents
                    (input-specification-regs output-spec)))
             (add-unread-mem-addresses input-spec output-spec)
             (vector-push-extend input-spec (input-spec super-asm))
             (vector-push-extend output-spec (output-spec super-asm))
             (vector-push-extend untraced-call-spec
                                 (untraced-call-spec super-asm))))
	(cond ((zerop (length line))) ; do nothing, empty line
	      ((search "Input data" line :test 'string-equal)
               ;; store the previous input/output set (if any)
               (when (or
                      (> (length (input-specification-regs output-spec)) 0)
                      (> (length (input-specification-mem output-spec)) 0))
                 ;; ensure callee-saved registers are preserved
                 (create-required-reg-specs input-spec output-spec)
                 (setf (input-specification-regs input-spec)
                       (sort-reg-contents
                        (input-specification-regs input-spec)))
                 (setf (input-specification-regs output-spec)
                       (sort-reg-contents
                        (input-specification-regs output-spec)))
                 (add-unread-mem-addresses input-spec output-spec)
                 (vector-push-extend input-spec (input-spec super-asm))
                 (vector-push-extend output-spec (output-spec super-asm))
                 (vector-push-extend untraced-call-spec
                                     (untraced-call-spec super-asm))
                 (setf input-spec (new-io-spec))
	         (setf output-spec (new-io-spec))
                 (setf untraced-call-spec (new-untraced-call-spec)))
               (setf state :input))
	      ((search "Output data" line :test 'string-equal)
               (setf state :output))
              ((search "Untraced calls" line :test 'string-equal)
               (setf state :untraced-calls))
              ((eq state :untraced-calls)
               (let ((call-rec (parse-untraced-call line)))
                 (assert call-rec (call-rec)
                         (format nil "Invalid untraced call record: ~A"
                                 line))
                 (vector-push-extend
                  call-rec
                  (untraced-call-specification-calls untraced-call-spec))))
	      ((char= (char line 0) #\%) ; register spec?
               (unless (or (eq state :input) (eq state :output))
                 (error "Register spec not expected:~A"
                        line))
	       (let ((spec (parse-reg-spec line)))
		 (if (simd-reg-p (reg-contents-name spec))  ; SIMD register?
		     (vector-push
		      spec
		      (input-specification-simd-regs
		       (if (eq state :input) input-spec output-spec)))
                     ;; else a general-purpose register--don't collect duplicate
                     (unless (find (reg-contents-name spec)
                                   (input-specification-regs
                                    (if (eq state :input)
                                        input-spec
                                        output-spec))
                                   :key 'reg-contents-name :test 'equal)
                       (vector-push
                        spec
                        (input-specification-regs
                         (if (eq state :input) input-spec output-spec)))))))
	      (t ; assume memory specification
               (unless (or (eq state :input) (eq state :output))
                 (error "Memory spec not expected:~A"
                        line))
	       (vector-push-extend
		(parse-mem-spec line)
		(input-specification-mem
		 (if (eq state :input) input-spec output-spec)))))))
    (if (> (length (input-spec super-asm)) 0)
        (setf *input-registers*
              (sort-registers
               (iter (for x in-vector
                          (input-specification-regs
                           (aref (input-spec super-asm) 0)))
                     (collect (reg-contents-name x))))))
    (if (> (length (output-spec super-asm)) 0)
        (setf *output-registers*
              (sort-registers
               (iter (for x in-vector
                          (input-specification-regs
                           (aref (output-spec super-asm) 0)))
                     (collect (reg-contents-name x))))))
    (setf (page-block-list super-asm) (create-page-block-list super-asm))
    t))

;;;
;;; takes 8 bit mask and converts to 8-byte mask, with each
;;; 1-bit converted to 0xff to mask a full byte.
;;;
(defun create-byte-mask (bit-mask)
  (map 'vector (lambda (x)(if (zerop x) #x00 #xff)) bit-mask))

;;;
;;; assume bytes are in little-endian order
;;;
(defun bytes-to-qword (bytes)
  (let ((result 0))
    (iter (for i from 7 downto 0)
	  (setf result (+ (ash result 8) (aref bytes i))))
    result))

(defun be-bytes-to-qword (bytes)
  "Convert 8-byte vector to qword.
 Assumes bytes in big-endian order."
  (let ((result 0))
    (iter (for i from 0 to 7)
	  (setf result (+ (ash result 8) (aref bytes i))))
    result))

(defun qword-to-be-bytes (n)
  "Convert integer to vector of 8 bytes.
 Results will be in big-endian order."
  (let ((result (make-array 8 :element-type '(unsigned-byte 8)))
        (shift -64))
    (dotimes (i 8)
      (setf (elt result i) (logand (ash n (incf shift 8)) #xff)))
    result))

(defun get-function-lines (asm-super start-addr end-addr)
  "Return lines and start and end indices of START-ADDR END-ADDR in ASM-SUPER.
Given start and end addresses of a function, determine start line, end
line, and text of included lines. Returns 3 values: list of lines,
start line index and end line index."
  (let* ((genome (genome asm-super))
	 (start-index
	  (position start-addr genome
		    :key 'asm-line-info-address
		    :test (lambda (x y)(and y (= x y))))) ; Skip null address.
	 (end-index
	  (position end-addr genome
		    :key 'asm-line-info-address
		    :start (if start-index start-index 0)
		    :test (lambda (x y)(and y (= x y))))))
    (values
     (if (and start-index end-index)
	 (subseq genome start-index (+ 1 end-index))
	 nil)
     start-index
     end-index)))

(defun traverse-function-graph (asm asm-super ht)
  "Collect all the called functions (both extern and local) and store in
passed hash-table."
  (dolist (x (call-targets asm))
    (let ((name (getf x ':name)))
      (unless (gethash name ht)
	(setf (gethash name ht) x)
	(let ((child (make-instance 'asm-heap)))
	  (setf (lines child) (get-function-lines-from-name asm-super name))
	  (traverse-function-graph child asm-super ht))))))

(defun collect-local-funcs (asm-super)
  "For targeted function, collect all the function names being called,
either directly or indirectly."
  (let ((ht (make-hash-table :test 'equalp)))
    (traverse-function-graph (create-target asm-super) asm-super ht)
    (let ((funcs '()))
      (maphash
       (lambda (k v)
	 (declare (ignore k))
	 (unless (getf v ':library)
	   (push (getf v ':name) funcs)))
       ht)
      funcs)))

(defun collect-extern-funcs (asm-super)
  "For targeted function, collect all extern call targets for
the function or any local functions it directly or indirectly."
  (let ((ht (make-hash-table :test 'equalp)))
    (traverse-function-graph (create-target asm-super) asm-super ht)
    (let ((funcs '()))
      (maphash
       (lambda (k v)
	 (declare (ignore k))
	 (if (getf v ':library)
	     (push (getf v ':full-name) funcs)))
       ht)
      funcs)))

(defun target-function (asm-super start-addr end-addr)
  "Define the target function by specifying start address and end address"
  (multiple-value-bind (lines start-index end-index)
      (get-function-lines asm-super start-addr end-addr)
    ;; strip off the function label at the beginning, if present (could be 2)
    (if (sel/sw/asm-heap::line-is-function-label (elt lines 0))
	(setf lines (subseq lines 1)))
    (if (sel/sw/asm-heap::line-is-function-label (elt lines 0))
	(setf lines (subseq lines 1)))
    (setf (target-start-index asm-super) start-index)
    (setf (target-end-index asm-super) end-index)
    (setf (target-lines asm-super)
	  lines)))

(defun target-function-by-lines (asm-super start-line end-line)
  "Define the target function by line boundary."
  (let ((lines (subseq (genome asm-super) start-line (1+ end-line))))
    ;; strip off the function label at the beginning, if present (could be 2)
    (if (sel/sw/asm-heap::line-is-function-label (elt lines 0))
	(setf lines (subseq lines 1)))
    (if (sel/sw/asm-heap::line-is-function-label (elt lines 0))
	(setf lines (subseq lines 1)))
    (setf (target-start-index asm-super) start-line)
    (setf (target-end-index asm-super) end-line)
    (setf (target-lines asm-super)
	  lines)))

(defun target-function-name (asm function-name)
  "Specify target function by name. The name can be a symbol or a string. If
 a symbol, the SYMBOL-NAME of the symbol is used."
  (let* ((name
	  (if (stringp function-name)
	      function-name
	      (symbol-name function-name)))
	 (index-entry (find name (function-index asm)
			    :key 'function-index-entry-name
			    :test 'equalp)))
    (when index-entry
      (target-function-by-lines
       asm
       (function-index-entry-start-line index-entry)
       (function-index-entry-end-line index-entry))
      (load-io-file
       asm
       (or (and (io-file asm) (pathname (io-file asm)))
	   (merge-pathnames
	    (pathname (io-dir asm))
	    (make-pathname :name function-name))))
      (when (data-path asm)
        (setf (data-asm asm)
              (from-file (make-instance 'asm-heap)
                         (data-path asm)))
        (setf (static-names asm)
              (collect-static-names asm)))
      (setf (target-info asm) index-entry))))

(defun collect-static-names (asm)
  "If a data-asm heap is present, collect the label names and corresponding
 addresses as a property list to use as a mapping table."
  (let ((result '())
        (genome (genome (data-asm asm))))
    (dotimes (i (- (length genome) 1))
      (let ((info (elt genome i)))
        (if (asm-line-info-label info)  ; if it's a label, try to
                                        ; get address from following line
            (let ((addr (asm-line-info-address (elt genome (+ i 1)))))
              (if addr
                  (push (list (asm-line-info-label info) addr) result))))))
    (nreverse result)))

(defun find-main-line (asm-super)
  (find "$main:" (genome asm-super) :key 'asm-line-info-text :test 'equal))

(defun find-main-line-position (asm-super)
  (position "$main:" (genome asm-super) :key 'asm-line-info-text :test 'equal))

;;;
;;; Adds a variant-specific suffix to each local label in the text.
;;; A local label will begin with $ and ending with white space (intel)
;;; or begin with ".L_" (at&t). Adds suffix text to end of label
;;; (should be something like "_variant_1").
;;; Returns the result (does not modify passed text).
;;; Do not change labels which are used as data, i.e. referenced in non-branch
;;; instructions. For now--we will assume branch instructions are all
;;; ops which start with the letter "j". Do not modify call instructions.
;;; Also do not change labels used as branch targets if the name
;;; contains #\@ (signifies non-local label).
;;;
(defun add-label-suffix (text suffix asm-syntax asm-super)
  "In cases where a label needs to be unique per variant, add unique suffix."
  (let ((label-re
	 (if (intel-syntax-p asm-syntax)
	     "\\$[\\w@]+"
	     "\\.\\L\\_[\\w@]+")))
    (multiple-value-bind (start end register-match-begin register-match-end)
	(ppcre:scan label-re text)
      (declare (ignore register-match-begin register-match-end))
      (if (and (integerp start)
	       (integerp end)
               ;; don't add suffix to data labels
               (not (find (subseq text start end) (static-names asm-super)
                          :test 'equal :key 'first))
               ;; don't add suffix to external labels (with '@')
               (not (find #\@ text :start start :end end))
	       (let ((trimmed (string-trim '(#\space #\tab) text)))
                 (or
                  (starts-with-subseq "j" trimmed)
                 ; (starts-with-subseq "call" trimmed)
                  (starts-with-subseq "push" trimmed) ; Sometimes push a label
                  (starts-with-subseq "mov" trimmed)  ; Handle labels in offset
                                        ; calc
                  (if (intel-syntax-p asm-syntax)
                      (char= #\$ (char trimmed 0))
                      (char= #\. (char trimmed 0))))))
	  (concatenate 'string
		       (subseq text 0 end)
		       suffix
		       (subseq text end))
	  text))))

;;;
;;; Insert prolog code at the beginning of the file.
;;;
(defun add-prolog (asm num-variants index-info syntax)
  (if (intel-syntax-p syntax)
      (insert-new-lines
       asm
       (append
	(list
	 "; -------------- Globals (exported) ---------------"
	 "        global variant_table"
	 "        global input_regs"
	 "        global output_regs"
	 "        global input_mem"
	 "        global output_mem"
         "        global untraced_calls"
	 "        global num_tests"
         "        global save_rax"
	 "        global save_rbx"
         "        global save_rcx"
         "        global save_rdx"
         "        global save_rsp"
         "        global save_rbp"
         "        global save_rsi"
         "        global save_rdi"
         "        global save_r8"
         "        global save_r9"
         "        global save_r10"
         "        global save_r11"
         "        global save_r12"
         "        global save_r13"
         "        global save_r14"
         "        global save_r15"
         "        global temp_rbx"
	 "        global save_return_address"
	 "        global result_return_address"
	 "        global test_offset"
	 "        global _init_registers"
         "        global _restore_registers"
         "        global result_regs"
         "        global live_input_registers"
         "        global num_input_registers"
         "        global live_output_registers"
         "        global num_output_registers"
         "        global test_results"
         "        global untraced_call_results"
         "        global untraced_calls_offset"
         "        global jump_table_size"
         "        global jump_table")
	(iter (for i from 0 below num-variants)
	      (collect (format nil "        global variant_~D" i)))
        ;;
        ;; Use start_included_lines, end_included_lines to delineate
        ;; included code sections. The macros intentionally don't do anything,
        ;; they are just used as markers.
        ;;
        (list
         "        %define start_included_lines"
         "        %define end_included_lines")
	(list
	 ""
	 "; -------------- Stack Vars ---------------")
	(mapcar 'asm-line-info-text
		(function-index-entry-declarations index-info))
	(list
	 ""
	 "; -------------- Stack --------------"
	 "section .note.GNU-stack noalloc noexec nowrite progbits"
	 ""
	 "; ----------- Code & Data ------------"
	 "section .text exec nowrite  align=16"
	 "      align 8"))
       0)
      (insert-new-lines
       asm
       (append
	(list
	 "# -------------- Globals (exported) ---------------"
	 "        .globl variant_table"
	 "        .globl input_regs"
	 "        .globl output_regs"
	 "        .globl input_mem"
	 "        .globl output_mem"
         "        .globl untraced_calls"
	 "        .globl num_tests"
         "        .globl save_rax"
         "        .globl save_rbx"
         "        .globl save_rcx"
         "        .globl save_rdx"
         "        .globl save_rsp"
         "        .globl save_rbp"
         "        .globl save_rsi"
         "        .globl save_rdi"
         "        .globl save_r8"
         "        .globl save_r9"
         "        .globl save_r10"
         "        .globl save_r11"
         "        .globl save_r12"
         "        .globl save_r13"
         "        .globl save_r14"
         "        .globl save_r15"
         "        .globl temp_rbx"
	 "        .globl save_return_address"
	 "        .globl result_return_address"
	 "        .globl test_offset"
	 "        .globl _init_registers"
         "        .globl _restore_registers"
         "        .globl result_regs"
         "        .globl live_input_registers"
         "        .globl num_input_registers"
         "        .globl live_output_registers"
         "        .globl num_output_registers"
         "        .globl test_results"
         "        .globl untraced_call_results"
         "        .globl untraced_calls_offset"
         "        .globl jump_table_size"
         "        .globl jump_table")
	(iter (for i from 0 below num-variants)
	      (collect (format nil "        .globl variant_~D" i)))
        ;;
        ;; Use start_included_lines, end_included_lines to delineate
        ;; included code sections. The macros intentionally don't do anything,
        ;; they are just used as markers.
        ;;
        (list
         "        .macro start_included_lines"
         "        .endm"
         "        .macro end_included_lines"
         "        .endm")
	(list
	 ""
	 "# -------------- Stack Vars ---------------")
	(mapcar 'asm-line-info-text
		(function-index-entry-declarations index-info))
	(list
	 ""
	 "# -------------- Stack --------------"
	 ".section .note.GNU-stack , \"\", @progbits"
	 ""
	 "# ----------- Code & Data ------------"
	 ".section .text"
	 ".align 16"))
       0)))

(defun add-externs (asm asm-super)
  (if (intel-syntax-p asm-super)
      (insert-new-lines
       asm
       (append
        (list
         ""
         "; -------------- Externs ---------------")
        (iter (for x in (collect-extern-funcs asm-super))
              (collect (format nil "        extern ~A" x))
              (list ""))))))

;;;
;;; Replace a RET operation with:
;;;    	pop qword [result_return_address]
;;;	jmp qword [save_return_address]
;;;
;;; This accomplishes the same thing, but ensures that we will be returning
;;  to the correct address (in case stack is corrupted).
;;; It also caches the stack return value so the C harness can determine
;;; whether there was a problem with the stack.
;;; We only do this transform with the first RET. If additional code has been
;;; added on we don't want to transform those RET operations.
;;; To do: handle cases where the target function has more than one RET.
;;;
;;; The passed argument is a vector of asm-line-info, and this returns
;;; a list of asm-line-info.
;;; We don't convert any RET to JMP if it is part of an inlined function
;;; i.e. has the :inline property true.
;;;
(defun handle-ret-ops (asm-lines asm-syntax)
  (let ((new-lines '())
        (included-section nil))
    (if (intel-syntax-p asm-syntax)
	(iter (for line in-vector asm-lines)
              ;; watch for included-lines boundary macro
              (if (equalp (asm-line-info-opcode line) "start_included_lines")
                  (setf included-section t)
                  (if (equalp (asm-line-info-opcode line) "end_included_lines")
                      (setf included-section nil)))
	      (if (and
                   (not included-section)
                   (equalp (asm-line-info-opcode line) "ret")
                   (not (getf (asm-line-info-properties line) ':inline)))
		  (progn
		    (push (car (parse-asm-line
				"        pop qword [result_return_address]"
				asm-syntax))
			  new-lines)
		    (push (car (parse-asm-line
				"        jmp qword [save_return_address]"
				asm-syntax))
			  new-lines))
		  (push line new-lines)))
	(iter (for line in-vector asm-lines)
	      (if (and
                   (not included-section)
                   (equalp (asm-line-info-opcode line) "retq")
                   (not (getf (asm-line-info-properties line) ':inline)))
		  (progn
		    (push (car (parse-asm-line
				"        popq (result_return_address)"
				asm-syntax))
			  new-lines)
		    (push (car (parse-asm-line
				"        jmpq *(save_return_address)"
				asm-syntax))
			  new-lines))
		  (push line new-lines))))
    (nreverse new-lines)))

;;; Append a variant function, defined by the name and
;;; lines of assembler code,
;;;
(defun add-variant-func (asm-variant name asm-syntax lines asm-super)
  (let* ((suffix (format nil "_~A" name))
	 (localized-lines
	  (mapcar
	   (lambda (line)
	     (add-label-suffix line suffix asm-syntax asm-super))
	   lines)))
    (if (intel-syntax-p asm-syntax)
	(insert-new-lines
	 asm-variant
	 (append
	  (list
	   (format nil "~A:" name)  ; function name
	   "        pop qword [save_return_address]"
	   "        push qword [save_return_address]")
	  localized-lines
	  (list "ret"   ; probably redundant, already in lines
		"align 8")))
	;; att syntax
	(insert-new-lines
	 asm-variant
	 (append
	  (list
	   (format nil "~A:" name)  ; function name
	   "        popq (save_return_address)"
	   "        pushq (save_return_address)")
	  localized-lines
	  (list "retq"   ; probably redundant, already in lines
		".align 8"))))))

(defun format-reg-specs (io-spec asm-syntax)
  (iter (for reg-spec in-vector (input-specification-regs io-spec))
	(collect
	    (format nil
		    (if (intel-syntax-p asm-syntax)
			"    dq 0x~16,'0X  ; ~A"
			"    .quad 0x~16,'0X  # ~A")
		    (be-bytes-to-qword (reg-contents-value reg-spec))
		    (reg-contents-name reg-spec)))))

;;;
;;; for each memory entry, add three qwords: address, data, mask.
;;; The mask is in the format (eg.): 0xff00000000000000
;;; (this means the high byte only is used)
;;; The list is terminated with an address of 0.
;;;
(defun format-mem-specs (io-spec asm-syntax)
  (if (intel-syntax-p asm-syntax)
      (let ((lines
	     (iter (for spec in-vector (input-specification-mem io-spec))
		   (collect
		       (let ((addr (memory-spec-addr spec))
			     (mask (memory-spec-mask spec))
			     (bytes (memory-spec-bytes spec)))
			 (format
			  nil
			  "    dq 0x~16,'0X~%    dq 0x~16,'0X~%    dq 0x~A~%"
			  addr
			  (be-bytes-to-qword bytes)
			  (apply 'concatenate 'string
				 (map 'list
				      (lambda (x)
					(if (= x 1) "FF" "00"))
				      mask))))))))
	(append lines (list "    dq 0x0"))) ; terminate with 0 address
      ;; att syntax
      (let ((lines
	     (iter (for spec in-vector (input-specification-mem io-spec))
		   (collect
		       (let ((addr (memory-spec-addr spec))
			     (mask (memory-spec-mask spec))
			     (bytes (memory-spec-bytes spec)))
			 (format
			  nil
			  "  .quad 0x~16,'0X~%  .quad 0x~16,'0X~%  .quad 0x~A~%"
			  addr
			  (be-bytes-to-qword bytes)
			  (apply 'concatenate 'string
				 (map 'list
				      (lambda (x)
					(if (= x 1) "FF" "00"))
				      mask))))))))
	(append lines (list "    .quad 0x0"))))) ; terminate with zero address

(defun add-variant-table (asm num-variants asm-syntax)
  (insert-new-lines
   asm
   (list
    ""
    (if (intel-syntax-p asm-syntax)
	"section .selrodata progbits alloc noexec nowrite align=8"
	".section selrodata, \"a\", @progbits")
    (if (intel-syntax-p asm-syntax)
	";;;  table of function pointers, 0-terminated"
	"#    table of function pointers, 0-terminated")
    "variant_table:"))

  ;; 2 copies of original function at start
  (dotimes (i 2)
    (insert-new-line
     asm
     (format nil
             (if (intel-syntax-p asm-syntax)
                 "        dq original_~D"
                 "        .quad original_~D")
             i)))
  (dotimes (i num-variants)
    (insert-new-line
     asm
     (format nil
	     (if (intel-syntax-p asm-syntax)
		 "        dq variant_~D"
		 "        .quad variant_~D")
	     i)))
  (insert-new-line asm
		   (if (intel-syntax-p asm-syntax)
		       "        dq 0x0"
		       "        .quad 0x0")))

(defun format-reg-info (asm-variants spec-vec asm-syntax label)
  (insert-new-lines asm-variants (list "" label) (length (genome asm-variants)))
  (dotimes (i (length spec-vec))
    (insert-new-lines
     asm-variants
     (format-reg-specs (aref spec-vec i) asm-syntax))
    (insert-new-line asm-variants "")))

(defun format-mem-info (asm-variants spec-vec asm-syntax label)
  (insert-new-lines asm-variants (list "" label))
  (dotimes (i (length spec-vec))
    (insert-new-lines
     asm-variants
     (format-mem-specs (aref spec-vec i) asm-syntax)
     (length (genome asm-variants)))
    (insert-new-line asm-variants "")))

(defconstant untraced-call-type-nil 0 "No type")
(defconstant untraced-call-type-malloc 1 "malloc")
(defconstant untraced-call-type-free 2 "free")
(defconstant untraced-call-type-realloc 3 "realloc")

(defun encode-call-type (type-name)
  (let ((name (first (split-sequence #\@ type-name)))) ; remove @plt if present
    (cond ((null type-name) untraced-call-type-nil)
          ((string-equal name "malloc") untraced-call-type-malloc)
          ((string-equal name "free") untraced-call-type-free)
          ((string-equal name "realloc") untraced-call-type-realloc))))

;;;
;;; for each untraced call entry, add type, argument(s), and
;;; (optional) return-value quad words.
;;;
;;; Number of arguments and presence of return value depend on the type.
;;; All are formatted as quad-words.
;;;
;;; Types:
;;;   0 - Null terminator (end of untraced calls) -- 1 quad (type)
;;;   1 - result malloc(arg1) -- 3 quads (type + arg + result)
;;;   2 - void free(arg1) -- 2 quads (type + arg)
;;;   3 - void realloc(arg1,arg2) -- 4 quads (type + 2 args + result)
;;;
(defun format-untraced-call-specs (io-spec asm-syntax)
  (let ((quad-decl (if (intel-syntax-p asm-syntax) "dq" ".quad")))
    (let ((lines
           (iter (for spec in-vector
                      (untraced-call-specification-calls io-spec))
                 (collect
                     (format
                      nil
                      "    ~A 0x~16,'0X~{, 0x~16,'0X~}~{, 0x~16,'0X~}"
                      quad-decl
                      (encode-call-type (untraced-call-name spec))
                      (untraced-call-args spec)
                      (if (untraced-call-result spec)
                          (list (untraced-call-result spec))))))))
      (append
       lines
       (list (format nil "    ~A 0x0" quad-decl)))))) ; terminate with 0 address

(defun format-untraced-calls (asm-variants spec-vec asm-syntax label)
  (insert-new-lines asm-variants (list "" label))
  (dotimes (i (length spec-vec))
    (insert-new-lines
     asm-variants
     (format-untraced-call-specs (aref spec-vec i) asm-syntax)
     (length (genome asm-variants)))
    (insert-new-line asm-variants "")))

(defparameter *system-stack-size* #x400000)  ;; assume 4 meg stack for now

(defun remove-mem-below-sp (io-specs)
  "Filter out unnecessary memory specs.
 If any memory specifications are in the stack space
 but below the stack pointer (rsp) then they are considered
 junk and we will ignore them for testing."
  (iterate (for spec in-vector io-specs)
           (let* ((rsp-spec (find "rsp" (input-specification-regs spec)
                                  :key 'reg-contents-name :test 'equal))
                  (rsp-val
                   (and rsp-spec
                        (be-bytes-to-qword
                         (reg-contents-value rsp-spec)))))
             (if rsp-val
                 (setf (input-specification-mem spec)
                       (copy-array
                        (remove-if (lambda (mem-spec)
                                     (let ((addr (memory-spec-addr mem-spec)))
                                       (and (< addr rsp-val)
                                            (> addr (- rsp-val
                                                       *system-stack-size*)))))
                                   (input-specification-mem spec))
                        :fill-pointer t :adjustable t))))))

(defun add-seldata-header (asm-variants asm-super)
  (if (intel-syntax-p asm-super)
      (insert-new-lines
       asm-variants
       (list "section .seldata2 alloc noexec write align=4"))
      ;; att syntax
      (insert-new-lines
       asm-variants
       (list  ".section .seldata2, \"wa\""))))

(defun add-bss-section (asm-variants asm-super)
  ;; if bss section found, add it
  (let ((bss (extract-section asm-super ".BSS")))
    (if (intel-syntax-p asm-super)
	(if bss
	    (insert-new-lines
	     asm-variants
	     (cons "section .seldata nobits alloc noexec write align=4"
		   (cdr (lines (extract-section asm-super ".BSS")))))
	    (insert-new-lines
	     asm-variants
	     (list "section .seldata nobits alloc noexec write align=4"
		   "        resb 16"
		   "        resb 8"
		   "        resb 8"
		   "        resb 8")))
	;; att syntax
	(if bss
	    (insert-new-lines
	     asm-variants
	     (append
	      (list ".section .seldata, \"wa\", @nobits"
		    ".align 16")
	      (cdr (lines (extract-section asm-super ".BSS")))))
	    (insert-new-lines
	     asm-variants
	     (list  ".section .seldata, \"wa\", @nobits"
		   "    .zero 8"
		   "    .zero 8"
		   "    .zero 8"
		   "    .zero 4"
		   "    .zero 4"
		   "    .zero 8"))))))

(defun add-return-address-vars (asm-variants asm-syntax)
  (if (intel-syntax-p asm-syntax)
      (insert-new-lines
       asm-variants
       (list
	"        ; save address to return back to, in case the stack is corrupt"
	"        save_return_address: resb 8"
	"        ; save the address found on the stack (should be the same)"
	"        result_return_address: resb 8"
        "        ; local use only"
        "        temp_return_address: resb 8"
        "        save_rax: resb 8"
        "        save_rbx: resb 8"
        "        save_rcx: resb 8"
        "        save_rdx: resb 8"
        "        save_rsp: resb 8"
        "        save_rbp: resb 8"
        "        save_rsi: resb 8"
        "        save_rdi: resb 8"
        "        save_r8: resb 8"
        "        save_r9: resb 8"
        "        save_r10: resb 8"
        "        save_r11: resb 8"
        "        save_r12: resb 8"
        "        save_r13: resb 8"
        "        save_r14: resb 8"
        "        save_r15: resb 8"
        "        temp_rbx: resb 8"
        "        test_offset: resb 8"
        (format nil "        result_regs: resb 0x~X" (* +num-regs+ 8))
        ""))
      ;; att syntax
      (insert-new-lines
       asm-variants
       (list
	"        # save address to return back to, in case the stack is corrupt"
	"        save_return_address: .zero 8"
	"        # save the address found on the stack (should be the same)"
	"        result_return_address: .zero 8"
        "        temp_return_address: .zero 8"
        "        save_rax: .zero 8"
        "        save_rbx: .zero 8"
        "        save_rcx: .zero 8"
        "        save_rdx: .zero 8"
        "        save_rsp: .zero 8"
        "        save_rbp: .zero 8"
        "        save_rsi: .zero 8"
        "        save_rdi: .zero 8"
        "        save_r8:  .zero 8"
        "        save_r9:  .zero 8"
        "        save_r10: .zero 8"
        "        save_r11: .zero 8"
        "        save_r12: .zero 8"
        "        save_r13: .zero 8"
        "        save_r14: .zero 8"
        "        save_r15: .zero 8"
        "        temp_rbx: .zero 8"
        "        test_offset: .zero 8"
        (format nil "        result_regs: .zero 0x~X" (* +num-regs+ 8))
	""))))

(defun add-test-results-var (asm-variants asm-syntax num-tests num-variants)
  (if (intel-syntax-p asm-syntax)
      (insert-new-line
       asm-variants
       (format nil "        test_results: resb 0x~X"
               (* num-tests num-variants 8)))
      ;; att syntax
      (insert-new-line
       asm-variants
       (format nil "        test_results: .zero 0x~X"
               (* num-tests num-variants 8)))))

(defun add-untraced-call-results-var (asm-variants asm-syntax num-tests)
  (let ((decl-string (if (intel-syntax-p asm-syntax) "resb" ".zero")))
    (insert-new-line
     asm-variants
     (format nil "        untraced_call_results: ~A 0x~X"
             decl-string
             (* num-tests +max-untraced-call-results+)))
    (insert-new-line
     asm-variants
     (format nil "        untraced_calls_offset: ~A 0x~X"
             decl-string
             (* num-tests +max-untraced-call-results+)))))

(defun calc-live-regs (register-list)
  "Calculate live input register bit mask.
RAX=#x1, RBX=#x2,RCX=#x4,RDX=#x8,...,R15=#x8000."
  (iter
    (for x in register-list)
    (sum (ash 1 (position x *all-registers* :test 'equalp)))))

(defun add-io-tests (asm-super asm-variants)
  "Copy the I/O data from the asm-super into the asm-variants assembly file"
  (insert-new-lines
   asm-variants
   (list
    ""
    "num_tests:"))
  (insert-new-line
   asm-variants
   (format nil
	   (if (intel-syntax-p asm-super)
	       "        dq ~d"
	       "        .quad ~d")
	   (length (input-spec asm-super))))

  ;; add live register masks
  (if (intel-syntax-p asm-super)
      (insert-new-lines
       asm-variants
       (list
        (format nil "live_input_registers:  dq 0x~X"
                (calc-live-regs *input-registers*))
        (format nil "num_input_registers:   dq 0x~X"
                (length *input-registers*))
        (format nil "live_output_registers: dq 0x~X"
                (calc-live-regs *output-registers*))
        (format nil "num_output_registers:  dq 0x~X"
                (length *output-registers*))))
      (insert-new-lines
       asm-variants
       (list
        (format nil "live_input_registers:  .quad 0x~X"
                (calc-live-regs *input-registers*))
        (format nil "num_input_registers:   .quad 0x~X"
                (length *input-registers*))
        (format nil "live_output_registers: .quad 0x~X"
                (calc-live-regs *output-registers*))
        (format nil "num_output_registers:  .quad 0x~X"
                (length *output-registers*)))))

  (format-reg-info asm-variants (input-spec asm-super)
		   (asm-syntax asm-super)
		   "input_regs:")
  (format-reg-info asm-variants (output-spec asm-super)
		   (asm-syntax asm-super)
		   "output_regs:")
  (format-mem-info asm-variants
		   (input-spec asm-super)
		   (asm-syntax asm-super)
		   "input_mem:")
  (format-mem-info asm-variants
		   (output-spec asm-super)
		   (asm-syntax asm-super)
		   "output_mem:")
  (format-untraced-calls asm-variants
                         (untraced-call-spec asm-super)
                         (asm-syntax asm-super)
                         "untraced_calls:"))
#|
Jump table format
-----------------
.align 16
jump_table_size:
        .quad 2
.align 16
jump_table:
.align 16
        .quad <orig_child_func1_addr> # eg. 0x401051
        .quad child_func1
.align 16
        .quad <orig_child_func2_addr>
        .quad child_func2
|#
(defun add-leaf-jump-table (asm-super function-index)
  "Returns lines for jump table with addresses of local child functions.
 These will be relocated at run time by the fitness harness."
  (append
   (list (if (intel-syntax-p asm-super) "align 16" ".align 16")
         "jump_table_size:"
         (if (intel-syntax-p asm-super)
             (format nil "        dq ~A" (length function-index))
             (format nil "        .quad ~A" (length function-index)))
         (if (intel-syntax-p asm-super) "align 16" ".align 16")
         "jump_table:")
   (iter (for x in-vector function-index)
         (when (function-index-entry-start-address x)
             (collect (if (intel-syntax-p asm-super) "align 16" ".align 16"))
             (collect (if (intel-syntax-p asm-super)
                          (format nil "        dq 0x~X"
                                  (function-index-entry-start-address x))
                          (format nil "        .quad 0x~X"
                                  (function-index-entry-start-address x))))
             (collect (if (intel-syntax-p asm-super)
                          (format nil "        dq ~A"
                                  (function-index-entry-name x))
                          (format nil "        .quad ~A"
                                  (function-index-entry-name x))))))
   (list "")))

(defun add-included-lines (asm-super asm-variants)
  "If any extra lines were supplied, paste them in now."
  (if (include-lines asm-super)
      (let ((included (make-instance 'asm-heap)))
        (setf (lines included) (include-lines asm-super))
        ;; ensure any rip-relative addresses are converted to absolute
        (dotimes (i (length (genome included)))
          (convert-rip-relative-to-absolute included i asm-super)
          (convert-symbolic-address-to-absolute included i asm-super))
        (insert-new-lines
         asm-variants
         (lines included))
        (insert-new-lines
         asm-variants
         (add-leaf-jump-table asm-super (function-index included))))
      (insert-new-lines
       asm-variants
       (add-leaf-jump-table asm-super '()))))

(defun add-data-lines (asm-super asm-variants)
  "Add lines with static data if included."
  (if (data-asm asm-super)
        (insert-new-lines
         asm-variants
         (lines (data-asm asm-super)))))

(defun get-function-lines-from-name (asm-super name)
  "Given a function name, return the lines of that function (if found)."
  (let ((index-entry (find name (function-index asm-super)
			   :key 'function-index-entry-name
			   :test 'equalp)))
    (when index-entry
      (get-function-lines
	      asm-super
	      (function-index-entry-start-address index-entry)
	      (function-index-entry-end-address index-entry)))))

(defun add-included-funcs (asm-super asm-variants)
  "If any extra functions were included, paste them in now."
  (if (include-funcs asm-super)
      (dolist (func-name (include-funcs asm-super))
	(if (symbolp func-name)
	    (setf func-name (symbol-name func-name)))
	(let ((lines (get-function-lines-from-name asm-super func-name)))
	  (when lines
            (let ((included (make-instance 'asm-heap)))
              (setf (lines included) lines)
              ;; ensure any rip-relative addresses are converted to absolute
              (dotimes (i (length (genome included)))
                (convert-rip-relative-to-absolute included i asm-super)
                (convert-symbolic-address-to-absolute included i asm-super))
              (insert-new-lines
               asm-variants
               (lines included))))))))

(defun add-init-regs(asm-super asm-variants)
  "Add assembler code to initialize registers."
  (let ((rbx-pos nil))
    (insert-new-lines
     asm-variants
     (list
      "_init_registers: "
      (if (intel-syntax-p asm-super)
          "        pop qword [temp_return_address]"
          "        popq temp_return_address")))

    ;; save all the registers

    (insert-new-lines
     asm-variants
     (if (intel-syntax-p asm-super)
         (list
          "        mov qword [save_rax], rax"
          "        mov qword [save_rbx], rbx"
          "        mov qword [save_rcx], rcx"
          "        mov qword [save_rdx], rdx"
          "        mov qword [save_rsp], rsp"
          "        mov qword [save_rbp], rbp"
          "        mov qword [save_rsi], rsi"
          "        mov qword [save_rdi], rdi"
          "        mov qword [save_r8],  r8"
          "        mov qword [save_r9],  r9"
          "        mov qword [save_r10], r10"
          "        mov qword [save_r11], r11"
          "        mov qword [save_r12], r12"
          "        mov qword [save_r13], r13"
          "        mov qword [save_r14], r14"
          "        mov qword [save_r15], r15"
          "        mov rax, qword [test_offset]"
          "        lea rbx, [input_regs]"
          "        add rbx, rax")
         (list
          "        movq %rax, save_rax"
          "        movq %rbx, save_rbx"
          "        movq %rcx, save_rcx"
          "        movq %rdx, save_rdx"
          "        movq %rsp, save_rsp"
          "        movq %rbp, save_rbp"
          "        movq %rsi, save_rsi"
          "        movq %rdi, save_rdi"
          "        movq %r8,  save_r8"
          "        movq %r9,  save_r9"
          "        movq %r10, save_r10"
          "        movq %r11, save_r11"
          "        movq %r12, save_r12"
          "        movq %r13, save_r13"
          "        movq %r14, save_r14"
          "        movq %r15, save_r15"
          "        movq test_offset, %rax"
          "        leaq input_regs, %rbx"
          "        add %rax, %rbx")))

    ;; Initialize registers with input_regs data.
    ;; RBX now points to register data.
    (insert-new-lines
     asm-variants
     (iter
       (for x in *input-registers*)
       (for i from 0 by 8)
       (if (equalp x "RBX")
           (setf rbx-pos i)
           (collect
               (if (intel-syntax-p asm-super)
                   (format nil "        mov  ~A, [rbx + 0x~X]"
                           (string-downcase x)
                           i)
                   (format nil "        movq 0x~X(%rbx), %~A"
                           i
                           (string-downcase x)))))))

    (insert-new-line
     asm-variants
     (if (intel-syntax-p asm-super)
         "        add rsp, 8"
         "        add $8, %rsp"))

    (if rbx-pos ; if rbx is a live register
        (insert-new-line
         asm-variants
         (if (intel-syntax-p asm-super)
             (format nil "        mov rbx, qword [rbx + 0x~X]" rbx-pos)
             (format nil "        movq 0x~X(%rbx), %rbx" rbx-pos))))

    (insert-new-lines
     asm-variants
     (if (intel-syntax-p asm-super)
         (list
          "        push qword [temp_return_address]"
          "        ret"
          "        align 8")
         (list
          "        pushq temp_return_address"
          "        retq"
          "        .align 8")))))

(defun add-restore-regs(asm-super asm-variants)
  "Add assembler code to save and restore registers."
  ;; function name
  (let ((rbx-pos nil))
    (if (intel-syntax-p asm-super)
        (insert-new-lines
         asm-variants
         (append
          (list
           "_restore_registers: "
           "        mov qword [temp_return_address], rbx"
           "        lea rbx, [result_regs]")
          (iter
            (for x in *output-registers*)
            (for i from 0 by 8)
            (if (equalp x "RBX")
                (setf rbx-pos i)
                (collect
                    (format nil "        mov  qword [rbx + 0x~X], ~A"
                            i
                            (string-downcase x)))))
          (if rbx-pos ; if rbx is live, handle it specially
              (list
               "        mov rcx, rbx"
               "        mov rbx, qword [temp_rbx]"
               (format nil "        mov qword [rcx + 0x~X], rbx" rbx-pos)))
          (list
           "        mov rax, qword [save_rax]"
           "        mov rbx, qword [save_rbx]"
           "        mov rcx, qword [save_rcx]"
           "        mov rdx, qword [save_rdx]"
           "        mov rsp, qword [save_rsp]"
           "        mov rbp, qword [save_rbp]"
           "        mov rsi, qword [save_rsi]"
           "        mov rdi, qword [save_rdi]"
           "        mov r8,  qword [save_r8]"
           "        mov r9,  qword [save_r9]"
           "        mov r10, qword [save_r10]"
           "        mov r11, qword [save_r11]"
           "        mov r12, qword [save_r12]"
           "        mov r13, qword [save_r13]"
           "        mov r14, qword [save_r14]"
           "        mov r15, qword [save_r15]")
          (list
           "        push qword [temp_return_address]"
           "        ret"
           "        align 8")))
        (insert-new-lines
         asm-variants
         (append
          (list
           "_restore_registers: "
           "        movq %rbx, temp_return_address"
           "        lea result_regs, %rbx")
          (iter
            (for x in *output-registers*)
            (for i from 0 by 8)
            (if (equalp x "RBX")
                (setf rbx-pos i)
                (collect
                    (format nil "        movq %~A, 0x~X(%rbx)"
                            (string-downcase x)
                            i))))
          (if rbx-pos ; if rbx is live, handle it specially
              (list
               "        mov %rbx, %rcx"
               "        movq temp_rbx, %rbx"
               (format nil "        movq %rbx, 0x~X(%rcx)" rbx-pos)))
          (list
           "        movq save_rax, %rax"
           "        movq save_rbx, %rbx"
           "        movq save_rcx, %rcx"
           "        movq save_rdx, %rdx"
           "        movq save_rsp, %rsp"
           "        movq save_rbp, %rbp"
           "        movq save_rsi, %rsi"
           "        movq save_rdi, %rdi"
           "        movq save_r8,  %r8"
           "        movq save_r9,  %r9"
           "        movq save_r10, %r10"
           "        movq save_r11, %r11"
           "        movq save_r12, %r12"
           "        movq save_r13, %r13"
           "        movq save_r14, %r14"
           "        movq save_r15, %r15")
          (list
           "        pushq temp_return_address"
           "        retq"
           "        .align 8"))))))

(defun add-io-sections (asm-super asm-variants)
  "Append new sections required by io testcases"
  (iter (for p in (page-block-list asm-super))
        (for i from 0)
        (insert-new-lines
         asm-variants
         (if (intel-syntax-p (asm-syntax asm-super))
             (list
              (format nil "section .sel~D, \"wa\"" i)
              (format nil "    resb 0x~X" (page-record-size p))
              "")
             (list
              (format nil ".section .sel~D, \"wa\"" i)
              (format nil "    .zero 0x~X" (page-record-size p))
              "")))))

(defun set-text-section (asm asm-super)
  "After unknown asm lines get appended, reset to .text segment defaults."
  (insert-new-lines
   asm
   (if (intel-syntax-p (asm-syntax asm-super))
       (list
        "; ----------- Code & Data ------------"
        "section .text exec nowrite  align=16"
        "      align 16")
       (list
        "# ----------- Code & Data ------------"
        ".section .text"
        ".align 16"))))

;;;
;;; considers the variants have the same super-owner if its super-owner's
;;; genome is equalp to the target asm-super-mutant
;;;
(defun generate-file (asm-super output-path number-of-variants)
  (declare (special *asm-super*))
  (setf *asm-super* asm-super)
  (let ((asm-variants (make-instance 'asm-heap :super-owner asm-super)))
    (if (input-spec asm-super)
        (remove-mem-below-sp (input-spec asm-super)))
    (if (output-spec asm-super)
        (remove-mem-below-sp (output-spec asm-super)))
    (setf (lines asm-variants) (list))  ;; empty heap
    (add-prolog asm-variants number-of-variants (target-info asm-super)
		(asm-syntax asm-super))
    (add-externs asm-variants asm-super)

    ;; add additionally specified functions or code lines
    (add-data-lines asm-super asm-variants)
    (set-text-section asm-variants asm-super)

    (add-included-lines asm-super asm-variants)
    (add-included-funcs asm-super asm-variants)
    (set-text-section asm-variants asm-super)

    (add-init-regs asm-super asm-variants)
    (add-restore-regs asm-super asm-variants)

    ;; start with two copies of original function
    (dotimes (i 2)
      (let ((v (create-target asm-super)))
        ;; ensure any rip-relative addresses are converted to absolute
        (dotimes (j (length (genome v)))
          (convert-rip-relative-to-absolute v j asm-super)
          (convert-symbolic-address-to-absolute v j asm-super))
        (add-variant-func
         asm-variants
         (format nil "original_~D" i)
         (asm-syntax asm-super)
         (mapcar 'asm-line-info-text
                 (handle-ret-ops (genome v) (asm-syntax asm-super)))
         asm-super)))

    (let ((count 0))
      (dolist (v (mutants asm-super))
        ;; ensure any rip-relative addresses are converted to absolute
        (dotimes (i (length (genome v)))
          (convert-rip-relative-to-absolute v i asm-super)
          (convert-symbolic-address-to-absolute v i asm-super))

        (add-variant-func
	 asm-variants
	 (format nil "variant_~D" count)
	 (asm-syntax asm-super)
	 (mapcar 'asm-line-info-text
		 (handle-ret-ops (genome v) (asm-syntax asm-super)))
         asm-super)
	(incf count)))
    (add-variant-table asm-variants number-of-variants (asm-syntax asm-super))
    (add-seldata-header asm-variants asm-super)
    (add-io-tests asm-super asm-variants)
    (add-bss-section asm-variants asm-super)
    (add-return-address-vars asm-variants (asm-syntax asm-super))
    (add-test-results-var asm-variants
                          (asm-syntax asm-super)
                          (length (input-spec asm-super))
                          number-of-variants)
    (add-untraced-call-results-var
     asm-variants
     (asm-syntax asm-super)
     (length (input-spec asm-super)))
    ;; add page block list sections
    #+linker-script (add-io-sections asm-super asm-variants)

    (setf (super-soft asm-super) asm-variants)  ;; cache the asm-heap
    (with-open-file (os output-path :direction :output :if-exists :supersede)
      (dolist (line (lines asm-variants))
	(format os "~A~%" line)))
    ;; (format t "File ~A successfully created.~%" output-path)
    output-path))

(defun local-prefix (syntax)
  "Return local label prefix."
  (if (intel-syntax-p syntax)
      "$"
      ".L_"))

(defun update-label (asm old new)
  "Replace branch targets to old name with new name.
 Returns list of line indexes which are modified."
  (let ((updated-lines '()))
    (iter (for x in (lines asm))
          (for i from 0)
          (let ((pos (search old x)))
            (when (and pos (not (search new x)))
              (setf x
                    (concatenate 'string
                                 (subseq x 0 pos)
                                 new
                                 (subseq x (+ pos (length old)))))
              (setf
               (elt (genome asm) i)
               (first (parse-asm-line x (asm-syntax asm))))
              (push i updated-lines))))
    updated-lines))

(defun get-inline-targets (asm funcname)
  "Returns list of line indexes which call the named function."
  (let ((targets '()))
    (iter (for x in (lines asm))
          (for i from 0)
          (when
              (multiple-value-bind (start)
                (ppcre:scan (format nil "callq?\\s+~A" funcname) x)
                start)
              (push i targets)))
    targets))

#|
(defun add-inline-counter-to-label (line count asm)
  "If the line is a label, append counter. Return updated line."
  (if (starts-with-subseq (local-prefix (asm-syntax asm)) line)
      (cl-ppcre:regex-replace "([0-9A-Za-z_]+)(:)"
                              line
                              (format nil "\\{1}_~D\\{2}" count))
      line))
|#

(defun inline-func (asm index lines count asm-super)
  "Add inline asm lines at index."
  (let* ((next-label
          (format nil "~ANEXT_~D"
                  (local-prefix (asm-syntax asm))
                  count))
         (inserted-lines
          (append (list (format nil "        ~A  ~A~A"
                                (if (intel-syntax-p asm) "push" "pushq")
                                (if (intel-syntax-p asm) "" "$")
                                next-label))
                  (iter (for x in lines)
                        (collect
                            (add-label-suffix x
                                              (format nil "_~D" count)
                                              (asm-syntax asm)
                                              asm-super)))
                  (list (format nil "~A:" next-label)))))
    (vector-cut (genome asm) index)
    (insert-new-lines asm inserted-lines index)
    ;; set :inline property of RET operations, so they don't get translated
    ;; to jumps
    (iter (for i from index to (+ index (length inserted-lines)))
          (setf
           (getf (asm-line-info-properties (elt (genome asm) i)) :inline)
           t))))

(defun optimize-included-lines (asm lines limit asm-super)
  "Merge the included lines into target asm-heap. asm-heap is updated ~
   with additional lines, up to limit. ~
   Two values are returned: ~
   The number of potential inline targets, and the ~
   number actually inlined due to size limit."
  (when lines
    (let* ((temp (make-instance 'asm-heap))
           (prefix (local-prefix (asm-syntax asm)))
           (func-index nil)
           (flines nil)
           (updates nil)
           (num-potential-inlines 0)
           (num-actual-inlines 0))
      (setf (lines temp) lines)
      (setf func-index (function-index temp))
      (iter (for fie in-vector func-index) ; for each function
            (let* ((func-line-infos
                    (subseq (genome temp)
                            (function-index-entry-start-line fie)
                            (+ 1 (function-index-entry-end-line fie))))
                   (first-line-info (elt func-line-infos 0)))
              (setf flines (map 'list
                                'asm-line-info-text
                                func-line-infos))
              ;; trim off starting labels (1 or 2 lines)
              (if (eq (asm-line-info-type first-line-info) ':label-decl)
                  (unless ; only do this if it isn't already a local label
                      (starts-with-subseq prefix
                                          (asm-line-info-label first-line-info))
                    (let ((new-label
                           (format nil
                                   "~A~A"
                                   prefix
                                   (asm-line-info-label first-line-info))))
                      (setf flines
                            ;; if second line is a label, remove first 2 lines
                            (if (and (>= (length func-line-infos) 2)
                                     (eq (asm-line-info-type
                                          (elt func-line-infos 1))
                                         ':label-decl))
                                (cddr flines)
                                ;; else remove only first line
                                (rest flines)))
                      (push
                       (list (asm-line-info-label first-line-info)
                             new-label
                             flines)
                             updates))))))
      ;; now we need to update any branch targets with modified label names
      (let ((inline-targets '()))
        (iter
          (for update in updates)
          ;; get list of line indexes where function is called
          (let ((targets (get-inline-targets asm (first update))))
            (iter (for target in targets)
                  (push
                   (list target (second update)(third update))
                   inline-targets))
            ;; sort targets for inlining into descending order
            (setf inline-targets (sort inline-targets '> :key 'first))
            (incf num-potential-inlines (length inline-targets))
            (iter (for it in inline-targets)
                  (while (< (+ (size asm) (length (third it))) limit))
                  (inline-func asm (first it) (third it) *inline-label-counter*
                               asm-super)
                  (incf *inline-label-counter*)
                  (incf num-actual-inlines)))))
      (values num-potential-inlines num-actual-inlines))))

(defun create-target (asm-super)
  "Returns an ASM-HEAP software object which contains only the target lines."
  (let ((asm (make-instance 'asm-heap :super-owner asm-super)))
    (setf (lines asm)(map 'list 'asm-line-info-text (target-lines asm-super)))
    ;; add included lines if inlining
    (when *inline-included-lines*
      (let ((*inline-label-counter* 0))
        (multiple-value-bind (possible actual)
            (optimize-included-lines asm
                                     (include-lines asm-super)
                                     (max-asm-function-size asm-super)
                                     asm-super)
          (record-meta-results asm-super
                               (list :possible-inlines possible
                                     :actual-inlines actual)))))
    asm))

(defvar *lib-papi*
  (or (probe-file "/usr/lib/x86_64-linux-gnu/libpapi.so")
      (probe-file "/usr/local/lib/libpapi.so")
      (probe-file "/usr/lib/libpapi.so"))
  "Path to papi library.  See http://icl.cs.utk.edu/papi/.")

(defmethod phenome ((asm asm-super-mutant)
		    &key (bin (temp-file-name :type "out"))
		      (src-path (temp-file-name :type "asm"))
                      (keep-assembly-file nil))
  "Create ASM file, assemble it, and link to create binary BIN."
  (with-temporary-file (:pathname src-path :keep keep-assembly-file)
    (let ((src (generate-file asm src-path (length (mutants asm)))))
      (with-temporary-file (:pathname obj :type "o")
        ;; Assemble.
        (multiple-value-bind (stdout stderr errno)
            (shell "~a ~a -o ~a ~a"
                   (if (intel-syntax-p asm)
                       "nasm"
                       "as")
                   (if (intel-syntax-p asm)
                       "-f elf64"
                       "")
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
              (setf bin nil)))
          (when (zerop errno)
            ;; Link.
            #+linker-script
            (unless (linker-script asm)
              (setf (linker-script asm) (get-default-linker-script)))

            (let (#+linker-script
                  (linker-sections-path (output-linker-sections asm))
                  compiler-command)
              #+linker-script
              (setf compiler-command
                    (concatenate 'string
                                 "~a ~a"
                                 " -Wl,--script=~a"
                                 " -Wl,--script=~a"
                                 " -O0 -fnon-call-exceptions -g"
                                 " -Wno-deprecated"
                                 " -Wl,--wrap=malloc"
                                 " -Wl,--wrap=realloc"
                                 " -Wl,--wrap=free"
                                 " ~a ~a ~a ~a ~a -lrt -o ~a ~a ~a ~a ~a"))
              #-linker-script
              (setf compiler-command
                    (concatenate 'string
                                 "~a ~a"
                                 " -O0 -fnon-call-exceptions -g"
                                 " -Wno-deprecated"
                                 " -Wl,--wrap=malloc"
                                 " -Wl,--wrap=realloc"
                                 " -Wl,--wrap=free"
                                 " ~a ~a ~a ~a ~a -lrt -o ~a ~a ~a ~a ~a"))

              (multiple-value-bind (stdout stderr errno)
                  (shell
                   compiler-command
                   (compiler asm)
                   (if (static-link asm) "-fno-PIE" "-no-pie")
                   #+linker-script  (linker-script asm)
                   #+linker-script linker-sections-path
                   (if (static-link asm) "--static" "")
                   (if (bss-segment asm)
                       (format nil "-Wl,--section-start=.seldata=0x~x"
                               (bss-segment asm))
                       (if *seldata-segment-start*
                           (format nil "-Wl,--section-start=.seldata=0x~x"
                                   *seldata-segment-start*)
                           ""))
                   (if *bss-segment-start*
                       (format nil "-Wl,-Tbss=0x~x" *bss-segment-start*)
                       "")
                   (if *data-segment-start*
                       (format nil "-Wl,-Tdata=0x~x" *data-segment-start*)
                       "")
                   (if *text-segment-start*
                       (format nil "-Wl,-Ttext-segment=0x~x" *text-segment-start*)
                       "")
                   bin
                   (fitness-harness asm)
                   obj
                   *lib-papi*
                   (or (libraries asm) ""))
                (restart-case
                    (unless (zerop errno)
                      (error (make-condition 'phenome :text stderr
                                             :obj asm :loc obj)))
                  (retry-project-build ()
                    :report "Retry `phenome' link on OBJ."
                    (phenome obj :bin bin))
                  (return-nil-for-bin ()
                    :report "Allow failure returning NIL for bin."
                    (setf bin nil)))
                (setf (phenome-results asm)
                      (list bin errno stderr stdout src))
                (values bin errno stderr stdout src)))))))))

(defun record-meta-results (asm-super meta-results)
  (do* ((x meta-results (cddr x))
        (key (car x) (car x))
        (value (cadr x) (cadr x)))
       ((null x))
    (if (null (getf (eval-meta-results asm-super) key))
        (setf (getf (eval-meta-results asm-super) key) value)
        (if (integerp (getf (eval-meta-results asm-super) key))
            (incf (getf (eval-meta-results asm-super) key) value)))))

(defun got-here (i) (declare (ignore i)) nil)

(defmethod evaluate ((test symbol)(asm-super asm-super-mutant)
		     &rest extra-keys
		     &key
                       (keep-fitness-executable *keep-fitness-executables*)
                       (keep-assembly-file *keep-assembly-files*)
		       &allow-other-keys)
  "Create phenome (binary executable) and call it to generate fitness results.
The variants need to already be created (stored in mutants slot) and the io-file
needs to have been loaded, along with the var-table by PARSE-SANITY-FILE."
  (declare (ignore extra-keys test))  ; currently ignore the test argument

  (let* ((*fitness-predicate* #'<)    ; lower fitness number is better
	 (*worst-fitness* (worst-numeric-fitness))
         (phenome-create-error nil)
         (phenome-execute-error nil))
    (with-temporary-file (:pathname bin :keep keep-fitness-executable)
      (multiple-value-bind (bin-path phenome-errno stderr stdout src)
	  (phenome asm-super :bin bin
                   :keep-assembly-file keep-assembly-file)
	(declare (ignorable phenome-errno stderr stdout src))
        (let ((meta-results nil)
              (test-results nil))
          (unless (zerop phenome-errno)
            (setf phenome-create-error t))
          (got-here "a")
	  (if (zerop phenome-errno)
	      ;; run the fitness program
	      (multiple-value-bind (stdout stderr errno)
                  (shell "limit.py -t ~d ~a" *timeout-seconds* bin-path)
		(declare (ignorable stderr errno))
                (got-here "b")
		(if (/= errno 0)
		    (setf phenome-execute-error t))
                (let ((input-str (make-string-input-stream stdout)))
                  (setf meta-results (read input-str nil :eof))
                  (got-here "c")
                  (if (eq meta-results :eof)
                      (error "Fitness executable terminated unexpectedly"))
                  (setf test-results (read input-str nil nil))
                  (if (eq test-results :eof)
                      (error "Fitness executable terminated unexpectedly"))

                  (if test-results
                      (dotimes (i (length test-results))
                        (assert (> (elt test-results i) 0) (test-results)
                                "The fitness cannot be zero"))))))
          (got-here "d")
          (when (null meta-results)
            (setf meta-results '()))
          (got-here 1)

	  (when (null test-results)
            ;; create array of *worst-fitness*
            (setf test-results
                  (make-array (* (length (mutants asm-super))
                                 (length (input-spec asm-super)))
                              :initial-element *worst-fitness*)))
          ;; capture exit-reason
          (if (null (getf (eval-meta-results asm-super) :exit-reason))
              (setf (getf (eval-meta-results asm-super) :exit-reason)
                    (getf meta-results :exit-reason)))
          (if (and phenome-execute-error *break-on-fitness-failure*)
              (let ((errmsg
                       (format
                        t
                        "No results--all variants marked as worst fitness.
 bin: ~A, src: ~A, phenome-create-error: ~A, phenome-execute-error: ~A"
                        bin-path
                        src
                        phenome-create-error
                        phenome-execute-error)))
                (assert nil ()  errmsg)))
	  (let* ((num-tests (length (input-spec asm-super)))
		 (num-variants (/ (length test-results) num-tests))
		 (results '()))
            (got-here 2)
	    ;; any that came back +worst-c-fitness+ replace with *worst-fitness*
	    (dotimes (i (length test-results))
	      (let ((test-result (aref test-results i)))
	        (assert (> test-result 0) (test-result)
			"The fitness cannot be zero")
		(if (= (elt test-results i) +worst-c-fitness+)
		    (setf (elt test-results i) *worst-fitness*))))
	    ;; set fitness vector for each mutant
            (got-here 3)
	    (dotimes (i num-variants)
	      (let ((variant-results
		     (subseq test-results
			     (* i num-tests) (* (+ i 1) num-tests))))
                (if *size-affects-fitness*
                    (setf variant-results
                          (concatenate 'vector
                                       variant-results
                                       (vector
                                        (size (elt (mutants asm-super) i))))))
		(setf (fitness (elt (mutants asm-super) i))
		      variant-results)
		(push variant-results results)))
	    (setf test-results (nreverse results)))
          (got-here 4)
          (record-meta-results asm-super meta-results)
	  (setf (fitness asm-super) test-results))))))

(defun add-simple-cut-variant (asm-super i)
  (let* ((orig (create-target asm-super))
         (variant (apply-mutation orig
				  (make-instance 'sel/sw/simple::simple-cut
						 :object orig :targets i))))
    (push variant (mutants asm-super))))

;;;
;;; Returns a population of variants, to be added to the asm-super mutants list.
;;; It will not do simple-cut operations on label declaration lines (which will
;;; simply break compilation). It will also skip the first instruction which is
;;; typically "push rbp", as this will cause the return address to be lost and
;;; definitely break.
;;;
(defun create-all-simple-cut-variants (asm-super)
  (let* ((orig (create-target asm-super))
	 (lines (genome orig))
	 (variants '())
	 (index 0))
    (iter (for line in-vector lines)
	  (unless
	      (or ;(= index 14) ;; causes infinite loop
	       (eq (asm-line-info-type line) ':label-decl))
	    (push
	     (apply-mutation
	      (copy orig)
	      (make-instance 'sel/sw/simple::simple-cut
			     :object orig :targets index))
	     variants)
	    ;; (format t "Cutting index ~D, line: ~A~%" index
	    ;;    (asm-line-info-text line))
	    )
	  (incf index))
    (nreverse variants)))

(defun section-header-p (asm-info)
  "If the passed asm-line-info is a section header, returns the section name.
Else returns NIL."
  (if (and (eq (asm-line-info-type asm-info) ':decl)
	   (member (first (asm-line-info-tokens asm-info))
		   '("section" ".section")
		   :test 'equalp))
      (second (asm-line-info-tokens asm-info))
      (and (eq (asm-line-info-type asm-info) ':decl)
	   (member (first (asm-line-info-tokens asm-info))
		   '(".bss" ".data" ".text")
		   :test 'equalp)
	   (first (asm-line-info-tokens asm-info)))))


(defun find-named-section (asm-super name)
  "Returns the starting line (integer) of the named section or
NIL if not found."
  (position-if
   (lambda (x)
     (equalp (section-header-p x) name))
   (genome asm-super)))

(defun extract-section (asm-super section-name)
  "Given the name (string) of a section, extract all the lines from
the named section into a new asm-heap, and return that. If not found,
returns NIL."
  (let ((named-section-pos (find-named-section asm-super section-name)))
    (if named-section-pos
	(let ((end (position-if 'section-header-p (genome asm-super)
				:start (+ named-section-pos 1))))
	  (if (null end)
	      (setf end (length (genome asm-super))))
	  (let ((section (make-instance 'asm-heap :super-owner asm-super)))
	    (setf (lines section)
		  (map 'list 'asm-line-info-text
		       (subseq (genome asm-super) named-section-pos end)))
	    section)))))

(defun leaf-functions (asm-super)
  (map 'list
       (lambda (x)
	 (string-downcase (function-index-entry-name x)))
       (remove-if-not (lambda (x) (function-index-entry-is-leaf x))
		      (function-index asm-super))))

(defun parse-sanity-file (filename)
  "Parses the 'sanity' file which is output by the GTX disassembler.
It contains all the data variables and addresses (some of which are
not included in the disassembly file). Returns a vector of var-rec."
  (with-open-file (is filename)
    (do* ((recs '())
	  (line (read-line is nil nil) (read-line is nil nil)))
	 ((null line)(make-array (length recs)
				 :initial-contents (nreverse recs)))
      (let* ((tokens (split-sequence #\space line))
	     (name (first tokens))
	     (type (second tokens))
	     (address (parse-integer (third tokens) :radix 16)))
	(push (make-var-rec :name name :type type :address address) recs)))))

(defun bss-segment-address (asm-super)
  (or (bss-segment asm-super)
      (let ((first-bss-var
	     (find "b" (var-table asm-super) :test 'equal :key 'var-rec-type)))
	(var-rec-address first-bss-var))))

(defun restore-original-addresses (asm)
  "If any rip-relative addresses have been converted to
absolute, for evaluation, restore them to the original
instructions."
  (dotimes (i (length (genome asm)))
    (restore-rip-relative-address asm i)))

(defmethod to-file :before ((asm asm-heap) file)
  "Save the assembly for ASM to FILE.
If any rip-relative addresses have been converted to
absolute, for evaluation, restore them to the original
instructions."
  (declare (ignorable file))
  ;; ensure any rip-relative addresses are converted to absolute
  (restore-original-addresses asm))

(defparameter *rip-rel-pattern* "[0-9A-Za-z_\\.]*\\(\\%rip\\)"
  "Pattern matches rip-relative addresses in AT&T.
This pattern is specific to AT&T syntax and will only find
the pattern in AT&T syntax code. We don't yet support
rip-relative addresses in Intel syntax. This pattern will fail
to find a match in Intel assembler.")

(defun convert-rip-relative-to-absolute (asm line-index asm-super)
  "Convert any rip-relative address to absolute.
If the assembly operation contains a rip-relative address,
replace it with an absolute address calculated as
<offset of rip-relative address> + <orig-address of following line>.
This should work as long as the address fits in 32-bits, as x86-64
instruction set does not support absolute addresses over 32-bits."

  ;; skip all this if it isn't an op statement
  (let* ((genome (genome asm))
         (asm-line-info (elt genome line-index)))
    (if (eq (asm-line-info-type asm-line-info) ':op)
        (let* ((text (asm-line-info-text asm-line-info))
               (next-orig-addr
                (iter (for x from (+ 1 line-index) to (1- (length genome)))
                      (if (asm-line-info-address (elt genome x))
                          (leave (asm-line-info-address (elt genome x)))))))

          (multiple-value-bind (start end)
              (cl-ppcre:scan *rip-rel-pattern* text)
            (if (and start end)
                (let* ((offset-str
                        (subseq text start (position #\( text)))
                       (offset
                        (if (or
                             (starts-with-subseq "0x" offset-str)
                             (starts-with-subseq "0X" offset-str))
                            (ignore-errors
                              (parse-integer (subseq offset-str 2) :radix 16))
                            (ignore-errors
                              (parse-integer offset-str :radix 10))))
                       (abs-address
                        (if (numberp offset)
                            (if (numberp next-orig-addr)
                                (+ offset next-orig-addr))
                            ;; if it isn't a number, lookup symbol address
                            (second
                             (find offset-str (static-names asm-super)
                                   :test 'equal :key 'first))))
                       (new-line (and abs-address
                                      (cl-ppcre:regex-replace
                                       *rip-rel-pattern*
                                       text
                                       (format nil "0x~X" abs-address)))))
                  ;; We found an rip-relative address and we created a
                  ;; replacement asm-line-info, which uses an absolute address.
                  ;; Now we stash the original in the new one's properties
                  ;; (key=:orig) and then replace the statement in the genome
                  ;; with the new asm-line-info.
                  (if new-line
                      (let ((new-line-info
                             (first (parse-asm-line
                                     new-line
                                     (asm-syntax asm)))))
                        ;; copy properties from original to new
                        (symbol-macrolet ((props (asm-line-info-properties
                                                  new-line-info)))
                          (setf props (asm-line-info-properties asm-line-info))
                          ;; stash original if not already stashed
                          (unless (getf props :orig)
                            (setf (getf props :orig) asm-line-info))
                          (setf (elt (genome asm)
                                     line-index) new-line-info)))))))))))

(defun convert-symbolic-address-to-absolute (asm line-index asm-super)
  "Convert named addresses to their original absolute address."
  ;; skip this if it isn't an op statement
  (let* ((genome (genome asm))
         (asm-line-info (elt genome line-index)))
    (if (eq (asm-line-info-type asm-line-info) ':op)
        (let* ((orig-text (asm-line-info-text asm-line-info))
               (new-text orig-text))
          (iter (for var in (static-names asm-super))
                (multiple-value-bind (new-line updated-p)
                    (cl-ppcre:regex-replace-all
                     (format nil "([^a-zA-Z0-9_])~A([^a-zA-Z0-9_])" (first var))
                     new-text
                     (format nil "\\{1}0x~X\\{2}" (second var)))
                  (if updated-p
                      (setf new-text new-line))))
          (unless (eq new-text orig-text)
            (let ((new-line-info
                   (first (parse-asm-line
                           new-text
                           (asm-syntax asm)))))
              ;; copy properties from original to new
              (symbol-macrolet ((props (asm-line-info-properties
                                        new-line-info)))
                (setf props (asm-line-info-properties asm-line-info))

                (unless (getf props :orig)
                  (setf (getf props :orig) asm-line-info))
                (setf (elt (genome asm)
                           line-index) new-line-info))))))))

;;;
;;; If RIP-relative addressing was used, restore it.
;;;
(defun restore-rip-relative-address (asm line-index)
  (let* ((info (elt (genome asm) line-index))
         (orig (getf (asm-line-info-properties info) :orig)))
    (when orig
      (remf (asm-line-info-properties info) :orig)
      (setf (elt (genome asm) line-index) orig))))

(defun get-default-linker-script ()
  "Return the default linker script as a path namestring."
  (let* ((strs (uiop/utility:split-string (shell "ld --verbose")
                                          :separator (list #\Newline)))
         (start (position-if
                 (lambda (x) (starts-with-subseq "==========" x))
                 strs))
         (end (position-if
                 (lambda (x) (starts-with-subseq "==========" x))
                 strs :from-end t)))
    (let ((out-path (temp-file-name :type "script")))
      (with-open-file (os out-path :direction :output :if-exists :supersede)
        (format os "~{~A~%~}" (subseq strs (+ 1 start) end)))
      out-path)))

(defparameter *stack-size-before* #x100000
  "Number of bytes to allocate below stack pointer, should be multiple
 of page size")
(defparameter *stack-size-after*  #x100000
  "Number of bytes to allocate above stack pointer, should be multiple
 of page size")

(defun add-page-record (page-rec ht)
  "Add a page record to a hash-table. If it already exists, don't add.
 If it is contingent with another, and attributes match, merge it."
  (dotimes (i (/ (page-record-size page-rec) +page-size+))
    (let ((addr (+ (page-record-addr page-rec) (* i +page-size+))))
      (setf (gethash addr ht)
            (make-page-record :addr addr :size +page-size+
                           :attr (page-record-attr page-rec))))))

(defun collect-pages (asm-super)
  "Collect all virtual memory page addresses needed by testcases into hashtable"
  (let* ((ht (make-hash-table))
         (input-recs (input-spec asm-super))
         (output-recs (output-spec asm-super))
         (untraced-call-recs (untraced-call-spec asm-super)))
    (iter (for rec in-vector input-recs)
          ;; Allocate 1 meg of pages below and above initial rsp
          ;; for stack for each test.
          (let* ((regs (input-specification-regs rec))
                 (rsp-page
                  (page-addr
                   (be-bytes-to-qword
                    (reg-contents-value
                     (find "rsp" regs :test 'equalp
                           :key 'reg-contents-name))))))
            (add-page-record
             (make-page-record :addr (- rsp-page *stack-size-before*)
                               :size (+ *stack-size-before*
                                        *stack-size-after*)
                               :attr "rw")
             ht))
          (let* ((mems (input-specification-mem rec)))
            (iter (for mem in-vector mems)
                  (let* ((addr (page-addr (memory-spec-addr mem))))
                    (add-page-record
                     (make-page-record :addr addr :size +page-size+ :attr "rw")
                     ht)))))
    (iter (for rec in-vector output-recs)
          (let* ((mems (input-specification-mem rec)))
            (iter (for mem in-vector mems)
                  (let* ((addr (page-addr (memory-spec-addr mem))))
                    (add-page-record
                     (make-page-record :addr addr :size +page-size+ :attr "rw")
                     ht)))))
    (iter (for rec in-vector untraced-call-recs)
          (let* ((calls (untraced-call-specification-calls rec)))
            (iter (for call in-vector calls)
                  (cond ((string= (untraced-call-name call) "malloc")
                         (make-page-record :addr (untraced-call-result call)
                                           :size
                                           (first (untraced-call-args call))
                                           :attr "rw"))
                        ((string= (untraced-call-name call) "realloc")
                         (make-page-record :addr (untraced-call-result call)
                                           :size
                                           (second (untraced-call-args call))
                                           :attr "rw"))))))
    ht))

(defun page-merge (p1 p2)
  "Return merged page records if page records are mergeable, or NIL otherwise.
 Page records are mergeable if there are no gaps between them."
  (if (and
       (= (+ (page-record-addr p1) (page-record-size p1))
          (page-record-addr p2))
       (string= (page-record-attr p1) (page-record-attr p2)))
      (make-page-record :addr (page-record-addr p1)
                        :size (+ (page-record-size p1) (page-record-size p2))
                        :attr (page-record-attr p1))
      (if (and
           (= (+ (page-record-addr p2) (page-record-size p2))
              (page-record-addr p1))
           (string= (page-record-attr p2) (page-record-attr p1)))
          (make-page-record :addr (page-record-addr p2)
                            :size (+ (page-record-size p2)
                                     (page-record-size p1))
                            :attr (page-record-attr p2)))))

(defun create-page-block-list (asm-super)
  "Collect, sort and merge all the block records for the asm-super-mutant.
 Returns the resulting list of page-blocks."
  (let* ((ht (collect-pages asm-super))
         (recs (make-array (hash-table-count ht)
                            :initial-contents
                            (stable-sort (hash-table-values ht) '<
                                         :key 'page-record-addr))))
    ;; merge pages to create page blocks
    (let ((blocks '())
          (curr nil)
          (p nil))
      (iter (for rec in-vector recs)
            (cond ((null curr) (setf curr rec))
                  ((setf p (page-merge curr rec)) (setf curr p))
                  (t (push curr blocks) (setf curr rec))))
      (push curr blocks)
      (nreverse blocks))))

(defun output-linker-sections (asm-super)
  "Write a linker-script containing the SECTIONS needed by the iodata.
 Returns the path of the file created."
  (let ((out-path (temp-file-name :type "script")))
    (with-open-file (os out-path :direction :output :if-exists :supersede)
      (format os "SECTIONS~%{~%")
      (iter (for p in (page-block-list asm-super))
            (for i from 0)
            (format os "  .sel~D 0x~X :~%  {~%    *(.sel~D)~%  }~%"
                    i (page-record-addr p) i))
      (format os "}~%"))
    out-path))
