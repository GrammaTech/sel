;;; clang-instrument --- Instrument C-language source files
(in-package :clang-instrument)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros :include-utf8))

(defmethod instrument ((obj clang) &optional trace-file)
  (when trace-file (warn "TODO: implement TRACE-FILE support."))
  (-<>> (asts obj)
        (remove-if-not {aget :full-stmt})
        ;; Remove statements if they are *not* in a compound statement.
        (remove-if-not
         (lambda (ast)
           (let ((parent-counter (aget :parent-counter ast)))
             (and (not (zerop parent-counter))
                  (string= "CompoundStmt"
                           (aget :ast-class (get-ast obj parent-counter)))))))
        (mapcar {aget :counter})
        (sort <> #'>)
        (reduce (lambda (variant counter)
                  (note 2 "Instrumenting AST#~d." counter)
                  (let* ((fmt "fputs(\"TRACE:~d\\n\", stderr);~%~a")
                         (text (peel-bananas
                                (aget :src-text (get-ast variant counter))))
                         (op `(:set
                               (:stmt1 . ,counter)
                               (:value1 . ,(format nil fmt counter text)))))
                    (setf (genome variant) (clang-mutate variant op)))
                  (update-asts variant)
                  variant)
                <> :initial-value (copy obj))))

(defmacro getopts (&rest forms)
  (let ((arg (gensym)))
    `(loop :for ,arg = (pop args) :while ,arg :do
        (cond
          ,@(mapcar (lambda-bind ((short long . body))
                      `((or (string= ,arg ,short) (string= ,arg ,long)) ,@body))
                    forms)
          (:otherwise (error "Unrecognized argument:~a" ,arg))))))

(setf *note-level* 1)

(defun main (args)
  "Interface to the command line instrumentation tool."
  (in-package :clang-instrument)
  (let ((self (pop args))
        (original (make-instance 'clang
                    :compiler (or (getenv "CC") "clang")
                    :flags (getenv "CFLAGS")))
        path out-dir name type trace-file out-file save-original)
    (when (or (not args)
              (< (length args) 1)
              (string= (subseq (car args) 0 2) "-h")
              (string= (subseq (car args) 0 3) "--h"))
      (format t "Usage: ~a SOURCE [OPTIONS]
 Instrument SOURCE along OPTIONS.

Options:
   ,--compiler CC ---------- use CC as the C compiler
                             (default to CC env. variable or clang)
 -f,--file FILE ------------ instrumented to write trace to fILE
                             (default to STDOUT)
 -F,--flags FLAGS ---------- comma-separated list of compiler flags
                             (default to CFLAGS env. variable)
 -o,--out-file FILE -------- write mutated source to FILE
                             (default STDOUT)
 -O,--orig ----------------- also save a copy of the original
 -q,--quiet ---------------- set verbosity level to 0
 -v,--verbose NUM ---------- verbosity level 0-4

Built with ~a version ~a.~%"
              self (lisp-implementation-type) (lisp-implementation-version))
      (sb-ext:exit))

    ;; Mandatory arguments.
    (setf path (pop args)
          out-dir (pathname-directory path)
          name (pathname-name path)
          type (pathname-type path)
          original (from-file original path))

    ;; Options.
    (getopts
      (nil "--compiler" (setf (compiler original) (pop args)))
      ("-f" "--file" (setf trace-file (pop args)))
      ("-F" "--flags" (setf (flags original) (pop args)))
      ("-o" "--out-file" (setf out-file (pop args)))
      ("-O" "--orig" (setf save-original t))
      ("-q" "--quiet" (setf *note-level* 0))
      ("-v" "--verbose"   (let ((lvl (parse-integer (pop args))))
                            (when (>= lvl 4) (setf *shell-debug* t))
                            (setf *note-level* lvl))))

    ;; Save original.
    (when save-original
      (let ((dest (make-pathname
                   :directory out-dir
                   :name (format nil "~a-original" name)
                   :type type)))
        (note 1 "Saving original to ~a." dest)
        (with-open-file (out dest :direction :output :if-exists :supersede)
          (genome-string original out))))

    ;; Instrument and save.
    (note 1 "Instrumenting ~a." path)
    (let ((dest (or out-file
                    (make-pathname
                     :directory out-dir
                     :name (format nil "~a-instrumented" name)
                     :type type)))
          (instrumented (clang-format (instrument original trace-file))))
      (note 1 "Writing instrumented to ~a." dest)
      (with-open-file
          (out dest :direction :output :if-exists :supersede)
        (genome-string instrumented out)))))
