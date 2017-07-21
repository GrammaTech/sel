;;; clang-instrument --- Instrument C-language source files
(in-package :software-evolution)
(enable-curry-compose-reader-macros :include-utf8)


;;;; Instrumentation

(defvar *instrument-log-variable-name* "__sel_trace_file"
  "Variable used for instrumentation.")

(defvar *instrument-log-env-name* "__SEL_TRACE_FILE"
  "Default environment variable in which to store log file.")

(defgeneric instrument (obj &key points functions functions-after
                              trace-file trace-env
                              print-argv instrument-exit filter)
  (:documentation
   "Instrument OBJ to print AST index before each full statement.

The indices printed here are not clang-mutate counters, but rather the
position of the ast in (asts obj).

Keyword arguments are as follows:
  POINTS ----------- alist of additional strings to print at specific points
  FUNCTIONS -------- functions to calculate instrumentation at each point
  FUNCTIONS-AFTER -- functions to calculate instrumentation after each point
  TRACE-FILE ------- file for trace output
  TRACE-ENV -------- trace output to file specified by ENV variable
  PRINT-ARGV ------- print program arguments on startup
  INSTRUMENT-EXIT -- print counter of function body before exit
  FILTER ----------- function to select a subset of ASTs for instrumentation
"))

(defmethod instrument
    ((obj clang) &key points functions functions-after
                   trace-file trace-env print-argv instrument-exit
                   (filter #'identity))
  ;; Send object through clang-mutate to get accurate counters
  (update-asts obj)

  ;; Default value for TRACE-ENV is `*instrument-log-env-name*'.  This
  ;; allows users to specify tracing from the environment without
  ;; having to export this above value (which we'd prefer not be
  ;; modified as it's assumed elsewhere).
  (when (eq trace-env t) (setf trace-env *instrument-log-env-name*))
  (let ((log-var (if (or trace-file trace-env)
                     *instrument-log-variable-name*
                     "stderr"))
        ;; Promote every counter key in POINTS to the enclosing full
        ;; statement with a CompoundStmt as a parent.  Otherwise they
        ;; will not appear in the output.
        (points
         (remove nil
           (mapcar (lambda-bind ((ast . value))
                     (let ((parent (enclosing-traceable-stmt obj ast)))
                       (if parent (cons parent value)
                           (warn "Point ~s doesn't match traceable AST."
                                 ast))))
                   points)))
        ;; Hash table mapping ASTs to indices. Values are the same as
        ;; index-of-ast, but without the linear search. Use
        ;; ast-ref-path for the key: it will be unique within a single
        ;; genome and is faster than doing equalp comparisons on whole
        ;; ASTs.
        (ast-numbers (alist-hash-table
                      (iter (for ast in (asts obj))
                            (for i upfrom 0)
                            (collect (cons (ast-ref-path ast) i)))
                      :test #'equalp)))
    (labels
        ((escape (string)
           (regex-replace (quote-meta-chars "%") (format nil "~S" string) "%%"))
         (last-traceable-stmt (proto)
           (->> (function-body obj proto)
                (get-immediate-children obj)
                (lastcar)
                (enclosing-traceable-stmt obj)))
         (first-traceable-stmt (proto)
           (first (get-immediate-children obj (function-body obj proto))))
         (instrument-ast (ast counter formats-w-args formats-w-args-after
                              trace-strings)
           ;; Given an AST and list of TRACE-STRINGS, return
           ;; instrumented source.
           (let* ((function (function-containing-ast obj ast))
                  (wrap (and (not (traceable-stmt-p obj ast))
                             (can-be-made-traceable-p obj ast)))
                  (return-void (ast-void-ret function))
                  (skip
                   (or (ast-in-macro-expansion ast)
                       (eq :NullStmt (ast-class ast))
                       ;; Might be null, short circuit and don't
                       ;; filter if so.
                       (when (or trace-file trace-env)
                         ;; TODO: This is gross, we should track that
                         ;;       this has been inserted instead of
                         ;;       searching the source for the string.
                         (search (file-open-str
                                  log-var :file trace-file :env trace-env)
                                 (source-text ast))))))

             ;; Insertions in bottom-up order
             (append
              ;; Function exit instrumentation
              (when (and instrument-exit
                         (equalp ast (last-traceable-stmt function)))
                `((:insert-value-after
                   (:stmt1 . ,(ast-counter
                               (ancestor-after obj (function-body obj function)
                                               ast)))
                   (:value1 .
                            ,(format nil
                                     "inst_exit:~%fputs(\"((:C . ~d)) \", ~a);~%~a"
                                     (gethash (ast-ref-path
                                               (function-body obj function))
                                              ast-numbers)
                                     log-var
                                     (if return-void "" "return _inst_ret;"))))))
              ;; Closing brace after the statement
              (when (and wrap (not skip))
                `((:insert-value-after (:stmt1 . ,counter)
                                       (:value1 . "}"))))
              ;; Temp variable for return value
              (when (and instrument-exit
                         (equalp ast (first-traceable-stmt function))
                         (not return-void))
                `((:insert-value
                   (:stmt1 . ,counter)
                   (:value1 .
                            ,(let ((type (find-type obj (ast-ret function))))
                               (format nil "~a~@[*~] _inst_ret"
                                       (type-name type)
                                       (type-pointer type)))))))
              ;; Transform return statement to temp assignment/goto
              (when (and instrument-exit
                         (eq (ast-class ast) :ReturnStmt))
                (let ((ret (unless return-void
                             (peel-bananas
                              (source-text
                               (first (get-immediate-children obj ast)))))))
                  `((:set
                     (:stmt1 . ,counter)
                     (:value1 .
                              ,(format nil "~@[_inst_ret = ~a;~] goto inst_exit;"
                                       ret))))))

              (when (and functions-after (not skip))
                `((:insert-value-after
                   (:stmt1 . ,counter)
                   (:value1 .
                            ,(format nil "~a~{~a~}~a~%"
                                     (format nil ; Start up alist w/counter.
                                             "fputs(\"((:C . ~d) \", ~a);~%"
                                             (gethash (ast-ref-path ast) ast-numbers)
                                             log-var)
                                     (mapcar
                                      {format nil ; Functions instrumentation.
                                              (format nil "fprintf(~a,\"~~a\"~~{,~~a~~});~%"
                                                      log-var)}
                                      (mapcar #'car formats-w-args-after)
                                      (mapcar #'cdr formats-w-args-after))
                                     (format nil "fputs(\")\", ~a);~% fflush(~a);~%"
                                             log-var log-var))))))

              ;; Opening brace and instrumentation code before
              (unless skip
                `((:insert-value
                   (:stmt1 . ,counter)
                   (:value1 .
                            ,(format nil "~:[~;{~]~{~a~}fputs(\")\\n\", ~a);~%"
                                     wrap
                                     (append
                                      (cons
                                       (format nil ; Start up alist w/counter.
                                               "fputs(\"((:C . ~d) \", ~a);~%"
                                               (gethash (ast-ref-path ast) ast-numbers)
                                               log-var)
                                       (when trace-strings
                                         (list
                                          (format nil ; Points instrumentation.
                                                  "fputs(\"(~{~a ~})\", ~a);~%"
                                                  (mapcar #'escape trace-strings) log-var))))
                                      (mapcar
                                       {format nil ; Functions instrumentation.
                                               (format nil "fprintf(~a,\"~~a\"~~{,~~a~~});~%"
                                                       log-var)}
                                       (mapcar #'car formats-w-args)
                                       (mapcar #'cdr formats-w-args))
                                      (list (format nil "fflush(~a);~%" log-var)))
                                     log-var)))))))))
      (-<>> (asts obj)
            (remove-if-not {can-be-made-traceable-p obj})
            (funcall filter)
            ;; Bottom up ensure earlier insertions don't invalidate
            ;; later counters.
            (sort <> #'> :key #'ast-counter)
            (mappend (lambda (ast)
                       (prog1
                           (instrument-ast ast
                                           (ast-counter ast)
                                           (mapcar {funcall _ obj ast}
                                                   functions)
                                           (mapcar {funcall _ obj ast}
                                                   functions-after)
                                           (aget ast points :test #'equalp))
                         (setf (aget ast points :test #'equalp) nil))))
            (apply-clang-mutate-ops obj)))

    ;; Warn about any leftover un-inserted points.
    (mapc (lambda (point)
            (warn "No insertion point found for pointer ~a." point))
          (remove-if-not #'cdr points))
    (when (and print-argv (get-entry obj))
      (print-program-input obj log-var))
    (when (or trace-file trace-env)
      (log-to-filename obj trace-file trace-env))
    (clang-format obj)
    obj))

(defun file-open-str (log-variable &key file env)
  (cond
    (file
     (format nil "  ~a = fopen(~s, \"a\");~%" log-variable (namestring file)))
    (env
     (format nil "  ~a = fopen(getenv(~s), \"a\");~%"
             log-variable (namestring env)))
    (t (error "`file-open-str' must specify :FILE or :ENV keyword."))))

(defun type-instrumentation-info (type print-strings)
  (let* ((c-type (if type
                     (concatenate 'string
                                  (when (type-pointer type) "*")
                                  (when (not (emptyp (type-array type))) "*")
                                  (type-name type))
                     ""))
         (stripped-c-type (regex-replace "\\**(unsigned )?" c-type ""))
         (fmt-code
          (switch (c-type :test #'string=)
            ("char"            "%d")
            ("int8_t"          "%d")
            ("wchar_t"         "%d")
            ("*char"           (if print-strings "\\\"%s\\\""  "#x%lx"))
            ("*wchar"          (if print-strings "\\\"%ls\\\"" "#x%lx"))
            ("unsigned char"   "%u")
            ("uint8_t"         "%u")
            ("short"           "%hi")
            ("int16_t"         "%hi")
            ("unsigned short"  "%hu")
            ("uint16_t"        "%hu")
            ("int"             "%i")
            ("int32_t"         "%i")
            ("unsigned int"    "%u")
            ("uint32_t"        "%u")
            ("long"            "%li")
            ("int64_t"         "%li")
            ("unsigned long"   "%lu")
            ("uint64_t"        "%lu")
            ("float"           "%f")
            ("double"          "%G")
            ("long double"     "%LG")
            ("size_t"          "%zu")
            (t (if (starts-with "*" c-type :test #'string=)
                   ;; NOTE: %lx is not guaranteed to be the right size for
                   ;; pointers. %p would be better, but it will typically print
                   ;; a leading "0x" which confuses the Lisp reader.
                   "#x%lx"
                   (error "Unrecognized C type ~S" c-type))))))
    (values c-type stripped-c-type fmt-code)))

(defgeneric var-instrument (software label key ast &key print-strings)
  (:documentation
   "Return a format string and variable list for variable instrumentation.
KEY should be a function used to pull the variable list out of AST in
SOFTWARE.  The result of KEY will appear behind LABEL in the trace
output."))

(defmethod var-instrument
    ((obj clang) label key (ast ast-ref) &key print-strings)
  (iter (for var in (funcall key ast))
        (multiple-value-bind (c-type stripped-c-type fmt-code)
            (type-instrumentation-info (find-var-type obj var) print-strings)
          (when (member stripped-c-type
                        (append +c-numeric-types+
                                '("int8_t" "int16_t" "int32_t" "int64_t"
                                  "wchar_t" "size_t"))
                        :test #'string=)
            (concatenating (format nil " (\\\"~a\\\" \\\"~a\\\" ~a)"
                                   (aget :name var) c-type fmt-code)
                           into format
                           initial-value (format nil "(~s" label))
            (collect (aget :name var) into vars)))
        (finally (return (cons (concatenate 'string format ")") vars)))))

(defgeneric get-entry (software)
  (:documentation "Return the entry AST in SOFTWARE."))

(defmethod get-entry ((obj clang))
  (&>> (find-if [{string= "main"} {ast-name}] (functions obj))
       (function-body obj)))

(defgeneric insert-at-entry (software ast)
  (:documentation "Insert AST at the entry point to SOFTWARE."))

(defmethod insert-at-entry ((obj clang) (ast string))
  ;; NOTE: The most robust way to perform this calculation is to grab
  ;; the whole text for the entry AST (i.e., "main"), and then to
  ;; insert the new text just after the first "{".
  (let* ((entry (get-entry obj))
         (old-text
          (peel-bananas (subseq (source-text entry) 1))))
    (setf
     (genome obj)
     (clang-mutate obj
       `(:set
         (:stmt1 . ,(ast-counter entry))
         (:value1 . ,(concatenate 'string "{" ast old-text))))))
  obj)

(defmethod print-program-input ((obj clang) log-variable)
  ;; Return a version of OBJ instrumented to print program input.
  (or (when-let* ((entry (get-entry obj))
                  (scope-vars (mapcar {aget :name}
                                      (get-vars-in-scope obj entry))))
        (when (and (member "argc" scope-vars :test #'string=)
                   (member "argv" scope-vars :test #'string=))
          (insert-at-entry obj
                           (format nil
                                   "fprintf(~a, \"((:INPUT \");
int __bi_mut_i_var;
for(__bi_mut_i_var = 0; __bi_mut_i_var < argc; ++__bi_mut_i_var) {
  fprintf(~a, \"\\\"%s\\\" \", argv[__bi_mut_i_var]);
}
fputs(\"))\\n\", ~a);"
                                   log-variable log-variable log-variable)))
        obj)
      (prog1 obj (warn "Unable to instrument program to print input."))))

;; Should only be called if you're sure obj is an entry-obj
(defun log-to-filename (obj file-name env-name)
  (assert (typep obj 'clang))
  (if (get-entry obj)
      ;; Object contains main(). Insert log variable definition and
      ;; initialization function. Use the "constructor" attribute to
      ;; run it on startup, before main() or C++ static initializers.
      (setf (genome obj)
            (format nil
                    "
#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
FILE *~a;
void __attribute__ (( constructor (101) )) __bi_setup_log_file() {
  ~a
}

~a
"
                    *instrument-log-variable-name*
                    (file-open-str *instrument-log-variable-name* :file file-name :env env-name)
                    (genome obj)))

      ;; Object does not contain main. Insert extern definition of log variable.
      (setf (genome obj)
            (concatenate 'string
              (format nil "#include <stdio.h>~%#include <stdlib.h>~%extern FILE *~a;~%"
                      *instrument-log-variable-name*)
              (genome obj))))

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
        path out-dir name type trace-file out-file save-original
        points functions print-strings print-argv instrument-exit)
    (when (or (not args)
              (< (length args) 1)
              (string= (subseq (car args) 0 (min 2 (length (car args))))
                       "-h")
              (string= (subseq (car args) 0 (min 3 (length (car args))))
                       "--h"))
      (format t "Usage: ~a SOURCE [OPTIONS]
 Instrument SOURCE along OPTIONS.

Options:
 -a,--print-argv -------- print program inputs (contents of argv) when present
 -c,--compiler CC ------- use CC as the C compiler
                          (default to CC env. variable or clang)
 -e,--exit -------------- instrument function exit
 -F,--flags FLAGS ------- comma-separated list of compiler flags
                          (default to CFLAGS env. variable)
 -o,--out-file FILE ----- write mutated source to FILE
                          (default STDOUT)
 -O,--orig -------------- also save a copy of the original
 -p,--point NUM,STRING -- instrument to print STRING at ast NUM
 -q,--quiet ------------- set verbosity level to 0
 -s,--strings ----------- trace string variable values, DANGEROUS
 -S,--scope ------------- write in-scope variables to trace
 -t,--trace-file FILE --- instrumented to write trace to fILE
                          (default to STDERR)
 -v,--variables --------- write unbound variables to trace
 -V,--verbose NUM ------- verbosity level 0-4

Built with ~a version ~a.~%"
              self (lisp-implementation-type) (lisp-implementation-version))
      (quit))

    ;; Mandatory arguments.
    (setf path (pop args)
          out-dir (pathname-directory path)
          name (pathname-name path)
          type (pathname-type path)
          original (from-file original path))

    ;; Options.
    (getopts
      ("-a" "--print-argv" (setf print-argv t))
      ("-c" "--compiler" (setf (compiler original) (pop args)))
      ("-e" "--exit" (setf instrument-exit t))
      ("-F" "--flags" (setf (flags original) (split-sequence #\, (pop args))))
      ("-o" "--out-file" (setf out-file (pop args)))
      ("-O" "--orig" (setf save-original t))
      ("-p" "--point" (destructuring-bind (counter string)
                          (split-sequence #\, (pop args))
                        (pushnew string (aget (parse-integer counter) points)
                                 :test #'string=)))
      ("-q" "--quiet" (setf *note-level* 0))
      ("-s" "--strings" (setf print-strings t))
      ("-S" "--scope" (push 'trace-scope-vars functions))
      ("-t" "--trace-file" (setf trace-file (pop args)))
      ("-v" "--variables" (push 'trace-unbound-vars functions))
      ("-V" "--verbose"   (let ((lvl (parse-integer (pop args))))
                            (when (>= lvl 4) (setf *shell-debug* t))
                            (setf *note-level* lvl))))

    ;; Set the functions.
    (when-let ((position (position 'trace-unbound-vars functions)))
      (setf (nth position functions)
            (lambda (obj ast)
              (var-instrument obj :unbound-vals
                              {get-unbound-vals obj} ast
                              :print-strings print-strings))))
    (when-let ((position (position 'trace-scope-vars functions)))
      (setf (nth position functions)
            (lambda (obj ast)
              (var-instrument
               obj :scopes
               [{apply #'append} {scopes obj}] ast
               :print-strings print-strings))))

    ;; Save original.
    (when save-original
      (let ((dest (make-pathname
                   :directory out-dir
                   :name (format nil "~a-original" name)
                   :type type)))
        (note 1 "Saving original to ~a." dest)
        (with-open-file (out dest :direction :output :if-exists :supersede)
          (genome-string (clang-format original) out))))

    ;; Instrument and save.
    (note 1 "Instrumenting ~a." path)
    (let ((dest (or out-file
                    (make-pathname
                     :directory out-dir
                     :name (format nil "~a-instrumented" name)
                     :type type)))
          (instrumented
           (handler-bind ((warning  ; Muffle warnings at low verbosity.
                           (if (> *note-level* 2)
                               #'identity
                               #'muffle-warning)))
             (clang-format
              (instrument original :trace-file trace-file
                          :points points
                          :functions functions
                          :print-argv print-argv
                          :instrument-exit instrument-exit)))))
      (note 1 "Writing instrumented to ~a." dest)
      (with-open-file
          (out dest :direction :output :if-exists :supersede)
        (genome-string instrumented out)))))
