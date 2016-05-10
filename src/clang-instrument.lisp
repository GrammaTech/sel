;;; clang-instrument --- Instrument C-language source files
(in-package :se)


;;;; Instrumentation
(defmethod instrument ((obj clang) &optional trace-file)
  (let ((log-var (if trace-file "__bi_mut_log_file" "stderr")))
    (-<>> (asts obj)
          (remove-if-not {aget :full-stmt})
          ;; Remove statements if they are *not* in a compound statement.
          (remove-if-not
           (lambda (ast)
             (let ((parent-counter (aget :parent-counter ast)))
               (and (not (zerop parent-counter))
                    (string=
                     "CompoundStmt"
                     (aget :ast-class (get-ast obj parent-counter)))))))
          (mapcar {aget :counter})
          (sort <> #'>)
          (reduce
           (lambda (variant counter)
             (note 2 "Instrumenting AST#~d." counter)
             (let* ((fmt "fputs(\"((:COUNTER . ~d))\\n\", ~a);~%~a")
                    (text (peel-bananas
                           (aget :src-text (get-ast variant counter))))
                    (op
                     `(:set
                       (:stmt1 . ,counter)
                       ,(cons :value1
                              (format nil fmt counter log-var text)))))
               (setf (genome variant) (clang-mutate variant op)))
             (update-asts variant)
             variant)
           <> :initial-value obj)
          (update-asts)
          (setf obj))
    (when trace-file
      (setf obj (log-to-filename obj log-var trace-file)))
    (clang-format obj)
    obj))

(defgeneric get-entry (software)
  (:documentation "Return the counter of the entry AST in SOFTWARE."))

(defmethod get-entry ((obj clang))
  (first (aget :stmt-range
               (find-if [{string= "main"} {aget :name}] (prototypes obj)))))

(defgeneric insert-at-entry (software ast)
  (:documentation "Insert AST at the entry point to SOFTWARE."))

(defmethod insert-at-entry ((obj clang) (ast string))
  ;; NOTE: The most robust way to perform this calculation is to grab
  ;; the whole text for the entry AST (i.e., "main"), and then to
  ;; insert the new text just after the first "{".
  (let* ((entry (get-entry obj))
         (old-text
          (peel-bananas (subseq (aget :src-text (get-ast obj entry)) 1))))
    (setf
     (genome obj)
     (clang-mutate obj
       `(:set
         (:stmt1 . ,entry)
         (:value1 . ,(concatenate 'string "{" ast old-text))))))
  obj)

(defmethod log-to-filename ((obj clang) log-variable filename)
  (setf obj
        (insert-at-entry obj (format nil "~a = fopen(~s, \"a\");~%"
                                     log-variable filename)))
  ;; Insert the declaration at the very top of the file.
  (setf (genome obj)
        (concatenate 'string
          (format nil "FILE *~a;~%" log-variable)
          (genome obj)))
  obj)


;;;; Command line
(defmacro getopts (&rest forms)
  (let ((arg (gensym)))
    `(loop :for ,arg = (pop args) :while ,arg :do
        (cond
          ,@(mapcar (lambda-bind ((short long . body))
                      `((or (string= ,arg ,short) (string= ,arg ,long)) ,@body))
                    forms)
          (:otherwise (error "Unrecognized argument:~a" ,arg))))))

(setf *note-level* 1)

(defun clang-instrument (args)
  "Interface to the command line instrumentation tool."
  (in-package :se)
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
 -c,--compiler CC ---------- use CC as the C compiler
                             (default to CC env. variable or clang)
 -F,--flags FLAGS ---------- comma-separated list of compiler flags
                             (default to CFLAGS env. variable)
 -o,--out-file FILE -------- write mutated source to FILE
                             (default STDOUT)
 -O,--orig ----------------- also save a copy of the original
 -q,--quiet ---------------- set verbosity level to 0
 -t,--trace-file FILE ------ instrumented to write trace to fILE
                             (default to STDERR)
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
      ("-c" "--compiler" (setf (compiler original) (pop args)))
      ("-F" "--flags" (setf (flags original) (pop args)))
      ("-o" "--out-file" (setf out-file (pop args)))
      ("-O" "--orig" (setf save-original t))
      ("-q" "--quiet" (setf *note-level* 0))
      ("-t" "--trace-file" (setf trace-file (pop args)))
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
