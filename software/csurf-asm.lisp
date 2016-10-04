;;; csurf-asm.lisp --- Support for csurf-generated assembler files
(in-package :se)

(defvar *gt-home* (getenv "GT_HOME")
  "Path to GT's trunk.")

(defvar *isa-nbits* 64
  "Indicate 32- or 64-bit architecture.")

(define-software csurf-asm (asm)
  ((assembler :initarg :assembler :accessor assembler :initform "nasm")
   (asm-flags :initarg :asm-flags :accessor asm-flags :initform nil
              :documentation "Flags to pass to assembler.")
   (redirect-file :initarg :redirect-file :accessor redirect-file :initform nil
                  :documentation
                  "CodeSurfer redirect file to redirect elf copy relocations.")
   (weak-symbols :initarg :weak-symbols :accessor weak-symbols :copier :direct
                 :documentation
                 "Symbols to weaken with `elf-weaken-gmon-start'.")))

(defun elf-weaken-gmon-start (elf-objfile symbols)
  "Reimplementation of CSURF elf:weaken-gmon-start.
Mark a set of symbols, e.g. __gmon_start__, as weakly required.  Uses
`*gt-home*' and `*isa-nbits*'."
  (let ((cmd-path (format nil "~a/libswyx/src/exe_map/elf_edit_symtab~d"
                          *gt-home* *isa-nbits*)))
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
         (concatenate 'string *gt-home* "/libswyx/src/exe_map/elf_copy_redirect")
         redirect-file elf-file))

(defmethod phenome ((asm csurf-asm) &key (bin (temp-file-name)))
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
