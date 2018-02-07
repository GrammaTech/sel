;;; csurf-asm.lisp --- Support for csurf-generated assembler files
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(defvar *gt-home* (getenv "GT_HOME")
  "Path to GT's trunk.")

(defvar *isa-nbits* 64
  "Indicate 32- or 64-bit architecture.")

;; Default location: $GT_HOME/libswyx/bin/
(defvar *elf-copy-redirect-path* "elf_copy_redirect"
  "Path to elf_copy_redirect (or just the name if it's on the path).")

;; Default location: $GT_HOME/libswyx/bin/
(defvar *elf-edit-symtab-path* (format nil "elf_edit_symtab~d" *isa-nbits*)
  "Path to elf_edit_symtab64 or 32 (or just the name if it's on the path).")

(defvar *linker-ignore-flags* (list "-o" ">" "1>" "2>" "&>")
  "List of flags to ignore when parsing linker commands.")

(define-software csurf-asm (asm)
  ((assembler
    :initarg :assembler :accessor assembler :initform "nasm"
    :documentation "Assembler to use for assembling.")
   (linker
    :initarg :linker :accessor linker :initform "ld"
    :documentation "Linker to use for linking.")
   (asm-flags
    :initarg :asm-flags :accessor asm-flags :initform nil
    :documentation "Flags to pass to assembler.")
   (redirect-file
    :initarg :redirect-file :accessor redirect-file :initform nil
    :documentation "CodeSurfer redirect file to redirect elf copy relocations.")
   (weak-symbols
    :initarg :weak-symbols :accessor weak-symbols :initform nil
    :copier :direct
    :documentation "Symbols to weaken with `elf-weaken-gmon-start'."))
  (:documentation "Software object for ASM generated by CodeSurfer."))

(defun elf-weaken-gmon-start (elf-objfile symbols)
  "Run elf_edit_symtab on ELF-OBJFILE and each symbol in SYMBOLS.
Reimplementation of CSURF elf:weaken-gmon-start.
* ELF-OBJFILE - an object file
* SYMBOLS - a list of strings representing symbols to mark as weakly required
Return a list of pairs whose first element is a symbol and whose second element
is the error number after running `*elf-edit-symtab-path*' on that symbol.
"
  (let ((cmd-path (namestring *elf-edit-symtab-path*)))
    (iter (for sym in symbols)
          (multiple-value-bind (stdout stderr errno)
              (shell "~a ~a ~a 2" cmd-path elf-objfile sym)
            (declare (ignorable stdout stderr))
            (unless (zerop errno)
              (collect (cons sym errno)))))))

(defun elf-copy-redirect (elf-file redirect-file)
  "Reimplementation of CSURF elf:copy-redirect.
Redirect ELF COPY relocations and associated symbols, for entries
described in redirect-file.  Requires GT_HOME environment variable to be
set."
  (shell "~a -v -s ~a ~a"
         (namestring *elf-copy-redirect-path*)
         redirect-file elf-file))

(defmethod phenome ((asm csurf-asm) &key (bin (temp-file-name)))
  "Assemble and link ASM into binary BIN.
1. Run `assembler' with `asm-flags'. If unsuccessful, return early.
2. If ASM contains `weak-symbols', mark them as weakly required
(see `elf-weaken-gmon-start').
3. Run `linker' with `flags'.
4. If `redirect-file' is specified, run `elf-copy-redirect'.
"
  ;; In CSURF-generated asm, mark some symbols, e.g.  __gmon_start__,
  ;; as weakly required.  The first value returned will be the name of
  ;; the binary on success, but may be the name of the object file if
  ;; there is a failure prior to linking.
  (with-temp-file-of (src "s") (genome-string asm)
    (with-temp-file (obj)
      ;; Assemble.
      (multiple-value-bind (stdout stderr errno)
          (shell "~a -o ~a ~a ~{~a~^ ~}"
                 (assembler asm) obj src (asm-flags asm))
        (if (not (zerop errno))
            (values obj errno stderr stdout src)
            ;; Mark __gmon_start__ et al. as weakly required.
            (if (elf-weaken-gmon-start obj (weak-symbols asm))
                ;; Errors in `elf-weaken-gmon-start'.
                (values obj (max errno 1) stderr stdout src)
                ;; Link.
                (multiple-value-bind (stdout stderr errno)
                    (shell "~a -o ~a ~a ~{~a~^ ~}"
                           (or (linker asm) *asm-linker*) bin obj (flags asm))
                  (if (or (not (zerop errno)) (not (redirect-file asm)))
                      ;; Errors linking.
                      (values bin (max errno 1) stderr stdout src)
                      ;; Elf copy-redirect
                      (multiple-value-bind (stdout stderr errno)
                          (elf-copy-redirect bin (redirect-file asm))
                        (values bin errno stderr stdout src))))))))))


;; Parsing csurf output (for asm/linker flags, weak symbols, redirects)
(defun is-program-cmd (program cmd)
  "Check if the command CMD is invoking program PROGRAM.
CMD is a list of strings representing a command. Check if the first element ends
with the string PROGRAM (to allow for qualified paths)."
  (and program
       (ends-with-subseq program (first cmd) :test #'equal)))

(defun extract-linker-flags (cmd)
  "Return a list of the flags for a linker command, extracted from CMD.
CMD is a list of strings representing a command. Remove the first element
(program name), any flags belonging to `*linker-ignore-flags*' and their
parameters (each is assumed to have exactly one), and any strings ending with
\".o\" (assuming that there is only one and it's the file for the software object
and so shouldn't be linked again)."
  ;; cdr to remove linker program name
  (iter (for str in (cdr cmd))
        (for i upfrom 0)
        (with next-removal = -1)
        ;; for strings that match a flag/redirect exactly, remove the
        ;; following element (the argument to the flag)
        (when (member str *linker-ignore-flags* :test #'equal)
          (setf next-removal (1+ i)))
        ;; keep only if the element isn't flagged for removal, doesn't start
        ;; with one of the ignore flags/redirects, and isn't a .o file
        (unless (or (= i next-removal)
                    (some {starts-with-subseq _ str} *linker-ignore-flags*)
                    (ends-with-subseq ".o" str :test #'equal))
          (collect str into extract-flags))
        (finally (return extract-flags))))

(defun parse-command (csurf-asm bracket-cmd)
  "Parse BRACKET-CMD to update fields of CSURF-ASM software object.
Used by `from-file'. Generally should not need to be invoked directly."
  ;; trim brackets
  (bind ((cmd (->> (subseq bracket-cmd 1 (1- (length bracket-cmd)))
                   (split "\\s+"))))
    (cond
      ;; assembler command
      ((is-program-cmd (assembler csurf-asm) cmd)
       (setf (asm-flags csurf-asm)
             ;; remove first argument (program name) and last (file name)
             (butlast (cdr cmd))))
      ;; mark symbols weakly required
      ((is-program-cmd *elf-edit-symtab-path* cmd)
       (setf (weak-symbols csurf-asm)
             ;; symbol is the second to last argument in elf_edit_symtab
             (adjoin (lastcar (butlast cmd))
                     (weak-symbols csurf-asm)
                     :test #'equal)))
      ;; linker cmd
      ((is-program-cmd (linker csurf-asm) cmd)
       (setf (flags csurf-asm)
             (extract-linker-flags cmd)))
      ;; elf copy redirect file
      ((is-program-cmd *elf-copy-redirect-path* cmd)
       (setf (redirect-file csurf-asm)
             (lastcar (butlast cmd))))
      ;; some other command
      (t nil))))

(defun parse-csurf-log (csurf-asm log-file)
  "Parse LOG-FILE to update fields of CSURF-ASM software object.
Used by `from-file'."
  (when (probe-file log-file)
    (with-open-file (in (probe-file log-file))
      (iter (for line = (ignore-errors (read-line in)))
            (while line)
            (multiple-value-bind (is-cmd bracket-cmd)
                (starts-with-subseq "#[subr system]" line :return-suffix t)
              (when is-cmd
                (parse-command csurf-asm bracket-cmd)))))))


(defmethod from-file ((asm csurf-asm) (paths list))
  "Update ASM using a CodeSurfer log file and an asm source file.
The first element of PATHS is the path to an ASM source file.
The second element is the path to the CodeSurfer log file generated by rewriting
a binary using the `-v' flag so that it includes all swyx commands issued
\(specifically to the assembler, elf_edidt_symtabXX, linker, and
elf_copy_redirect\)."
  (parse-csurf-log asm (second paths))
  (from-file asm (first paths)))
