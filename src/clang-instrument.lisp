;;; clang-instrument --- Instrument C-language source files
(in-package :software-evolution)
(enable-curry-compose-reader-macros :include-utf8)


;;;; Instrumentation

(defvar *instrument-log-variable-name* "__sel_trace_file"
  "Variable used for instrumentation.")

(defvar *instrument-log-env-name* "__SEL_TRACE_FILE"
  "Default environment variable in which to store log file.")

(defun ast-counters-equal (ast1 ast2)
  "Return true if ast1 and ast2 have the same counters"
  (equal (ast-counter ast1) (ast-counter ast2)))

(define-custom-hash-table-constructor make-ast-ht
  :test ast-counters-equal :hash-function ast-counter)

(defgeneric instrument (obj &key points functions functions-after
                                 trace-file trace-env
                                 print-argv instrument-exit
                                 filter postprocess-functions
                                 instrumenter)
  (:documentation
   "Instrument OBJ to print AST index before each full statement.

The indices printed here are not clang-mutate counters, but rather the
position of the ast in (asts obj).

Keyword arguments are as follows:
  POINTS --------------- alist of additional strings to print at specific points
  FUNCTIONS ------------ functions to calculate instrumentation at each point
  FUNCTIONS-AFTER ------ functions to calculate instrumentation after each point
  TRACE-FILE ----------- file for trace output
  TRACE-ENV ------------ trace output to file specified by ENV variable
  PRINT-ARGV ----------- print program arguments on startup
  INSTRUMENT-EXIT ------ print counter of function body before exit
  FILTER --------------- function to select a subset of ASTs for instrumentation
  POSTPROCESS-FUNCTIONS  functions to execute after instrumentation
"))

(define-constant +write-trace-include+
  "
#define _GNU_SOURCE
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>

enum trace_entry_tag {
    END_ENTRY = 0,
    STATEMENT_ID,
    VARIABLE,
    BUFFER_SIZE,
    AUXILIARY,
    TRACE_TAG_ERROR,
    /* Returned at EOF, should not appear in trace */
    END_OF_TRACE
};

#define WRITE_TRACE_VARIABLE(out, name_index, type_index, var)        \\
    do {                                                              \\
        uint16_t val;                                                 \\
        fputc(VARIABLE, out);                                         \\
        val = name_index; fwrite(&val, sizeof(val), 1, out);          \\
        val = type_index; fwrite(&val, sizeof(val), 1, out);          \\
        fwrite(&var, sizeof(var), 1, out);                            \\
    } while(0)

#define WRITE_TRACE_BLOB(out, name_index, type_index, size, ptr)        \\
    do {                                                                \\
        uint16_t val;                                                   \\
        fputc(VARIABLE, out);                                           \\
        val = name_index; fwrite(&val, sizeof(val), 1, out);            \\
        val = type_index; fwrite(&val, sizeof(val), 1, out);            \\
        val = size; fwrite(&val, sizeof(val), 1, out);                  \\
        fwrite(ptr, size, 1, out);                                      \\
    } while (0)

static void write_trace_id(FILE *out, uint32_t statement_id)
{
    fputc(STATEMENT_ID, out);
    fwrite(&statement_id, sizeof(statement_id), 1, out);
}

static void write_trace_aux(FILE *out, uint64_t value)
{
    fputc(AUXILIARY, out);
    fwrite(&value, sizeof(value), 1, out);
}

static void write_end_entry(FILE *out)
{
    fputc(END_ENTRY, out);
    fflush(out);
}
"
  :test #'string=
  :documentation "C code to include in all instrumented files.")


(define-constant +write-trace-impl+
  "
enum type_format {
    UNSIGNED,                   /* unsigned integer */
    SIGNED,                     /* signed integer */
    FLOAT,                      /* floating point */
    POINTER,                    /* unsigned, interpret as address */
    BLOB,                       /* arbitrary bytes, do not interpret */
    INVALID_FORMAT
};

typedef struct {
    /* Index into the string dictionary which gives the name of the type. */
    uint16_t name_index;
    /* Data format */
    enum type_format format;
    /* Size in bytes. 0 indicates a variable-sized object. */
    uint8_t size;
} type_description;


void write_trace_header(FILE *out, const char **names, uint16_t n_names,
                        const type_description *types, uint16_t n_types)
{
    /* Dictionary of names, as a sequence of NULL-terminated strings */
    uint16_t total_size = 0;
    for (uint16_t i = 0; i < n_names; i++) {
        total_size += strlen(names[i]) + 1;
    }
    fwrite(&total_size, sizeof(total_size), 1, out);

    for (uint16_t i = 0; i < n_names; i++) {
        fputs(names[i], out);
        fputc(0, out);
    }

    /* Dictionary of types */
    fwrite(&n_types, sizeof(n_types), 1, out);
    fwrite(types, sizeof(*types), n_types, out);
}
"
  :test #'string=
  :documentation "C code which implements trace writing.")

(defclass instrumenter ()
  ((software :accessor software :initarg :software :initform nil)
   (names :accessor names
          :initform (make-array 16 :fill-pointer 0 :adjustable t))
   (types :accessor types
          :initform (make-array 16 :fill-pointer 0 :adjustable t))
   (type-descriptions :accessor type-descriptions
          :initform (make-array 16 :fill-pointer 0 :adjustable t))
   (ast-ids :accessor ast-ids :initform nil)))

(defmethod get-name-index ((instrumenter instrumenter) name)
  (or (position name (names instrumenter) :test #'string=)
      (vector-push-extend name (names instrumenter))))

(defun array-or-pointer-type (type)
  ;; array or pointer, but not array of pointers
  (xor (not (emptyp (type-array type)))
       (type-pointer type)))

(defmethod get-type-index ((instrumenter instrumenter) type print-strings)
  (labels
      ((string-type-p (type)
         (or (string= (type-name type) "string")
             (and (string= (type-name type) "char")
                  (array-or-pointer-type type))))

       (type-description-struct (print-strings type)
         (let* ((c-type (if type (type-trace-string type) ""))
                (name-index (get-name-index instrumenter c-type)))
           (cond
             ;; String
             ((and print-strings (string-type-p type))
              (format nil "{~d, BLOB, 0}" name-index))
             ;; Pointer
             ;; Use sizeof(void*) in case underlying type is not yet declared.
             ((or (starts-with "*" c-type :test #'string=)
                  (starts-with "[" c-type :test #'string=))
              (format nil "{~d, POINTER, sizeof(void*)}" name-index))
             ;; Signed integers
             ((member c-type
                      '("char" "int8_t" "wchar_t" "short" "int16_t" "int"
                        "int32_t" "long" "int64_t")
                      :test #'string=)
              (format nil "{~d, SIGNED, sizeof(~a)}"
                      name-index
                      (type-decl-string type)))
             ;; Unsigned integers
             ((member c-type
                      '("unsigned char" "uint8_t" "unsigned short" "uint16_t"
                        "unsigned int" "uint32_t" "unsigned long" "uint64_t"
                        "size_t")
                      :test #'string=)
              (format nil "{~d, UNSIGNED, sizeof(~a)}"
                      name-index
                      (type-decl-string type)))
             ((string= c-type "float")
              (format nil "{~d, FLOAT, sizeof(float)}" name-index))
             ((string= c-type "double")
              (format nil "{~d, FLOAT, sizeof(double)}" name-index))
             ;; Otherwise no instrumentation
             (t nil)))))
    (let ((type-id (cons (and print-strings (string-type-p type)) type)))
      (or (position type-id (types instrumenter) :test #'equal)
          ;; Adding new type: generate header string
          (when-let ((description (type-description-struct print-strings type)))
            (vector-push-extend description (type-descriptions instrumenter))
            (vector-push-extend type-id (types instrumenter)))))))

(defmethod get-ast-id ((instrumenter instrumenter) ast)
  (gethash ast (ast-ids instrumenter)))

(defmethod initialize-instance :after ((instance instrumenter) &key)
  ;; Values are the same as index-of-ast, but without the linear
  ;; search.
  (when (software instance)
    (setf (ast-ids instance)
          (iter (for ast in (asts (software instance)))
                (for i upfrom 0)
                (with ht = (make-ast-ht))
                (setf (gethash ast ht) i)
                (finally (return ht))))))

(defmethod instrument
    ((obj clang) &key points functions functions-after
                      trace-file trace-env print-argv instrument-exit
                      (postprocess-functions (list #'clang-format))
                      (instrumenter (make-instance 'instrumenter :software obj))
                      (filter #'identity))
  ;; Send object through clang-mutate to get accurate counters
  (update-asts obj)

  ;; Default value for TRACE-ENV is `*instrument-log-env-name*'.  This
  ;; allows users to specify tracing from the environment without
  ;; having to export this above value (which we'd prefer not be
  ;; modified as it's assumed elsewhere).
  (when (eq trace-env t) (setf trace-env *instrument-log-env-name*))
  (let ((entry (get-entry obj))
        (log-var *instrument-log-variable-name*)
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
                   points))))
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
         (instrument-ast (ast counter extra-stmts extra-stmts-after
                              trace-strings)
           ;; Given an AST and list of TRACE-STRINGS, return
           ;; instrumented source.
           (let* ((function (function-containing-ast obj ast))
                  (wrap (and (not (traceable-stmt-p obj ast))
                             (can-be-made-traceable-p obj ast)))
                  (return-void (ast-void-ret function))
                  (skip
                   (or (ast-in-macro-expansion ast)
                       (eq :NullStmt (ast-class ast)))))

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
                                     "inst_exit:
write_trace_id(~a, ~du);
write_end_entry(~a);
~a"
                                     log-var
                                     (get-ast-id instrumenter
                                                 (function-body obj function))
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
                                             "write_trace_id(~a, ~du);~%"
                                             log-var
                                             (get-ast-id instrumenter ast))
                                     extra-stmts-after
                                     (format nil "write_end_entry(~a)"
                                             log-var))))))

              ;; Opening brace and instrumentation code before
              (unless skip
                `((:insert-value
                   (:stmt1 . ,counter)
                   (:value1 .
                            ,(format nil "~:[~;{~]~{~a~};~%"
                                     wrap
                                     (append
                                      (cons
                                       (format nil ; Start up alist w/counter.
                                               "write_trace_id(~a, ~du);~%"
                                               log-var
                                               (get-ast-id instrumenter ast))
                                       (when trace-strings
                                         (list
                                          (format nil ; Points instrumentation.
                                                  "fputs(\"(~{~a ~})\", ~a);~%"
                                                  (mapcar #'escape trace-strings) log-var))))
                                      extra-stmts
                                      (list (format nil "write_end_entry(~a)"
                                                    log-var))))))))))))
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
                                           (mappend {funcall _ instrumenter ast}
                                                    functions)
                                           (mappend {funcall _ instrumenter ast}
                                                    functions-after)
                                           (when points
                                             (aget ast points :test #'equalp)))
                         (when points
                           (setf (aget ast points :test #'equalp) nil)))))
            (apply-clang-mutate-ops obj)))

    ;; Warn about any leftover un-inserted points.
    (mapc (lambda (point)
            (warn "No insertion point found for pointer ~a." point))
          (remove-if-not #'cdr points))
    (when (and print-argv entry)
      (print-program-input obj log-var))
    (initialize-tracing obj trace-file trace-env entry instrumenter)
    (when entry
      (prepend-to-genome obj +write-trace-impl+))
    (prepend-to-genome obj +write-trace-include+)

    (when postprocess-functions
      (mapcar {funcall _ obj} postprocess-functions))

    obj))

(defun file-open-str (&key file env)
  (cond
    (file
     (format nil "fopen(~s, \"w\")" (namestring file)))
    (env
     (format nil "fopen(getenv(~s), \"w\")" env))
    (t (error "`file-open-str' must specify :FILE or :ENV keyword."))))

(defgeneric var-instrument (key instrumenter ast &key print-strings)
  (:documentation
   "Return C statements for variable instrumentation.
INSTRUMENTER contains instrumentation state. KEY should be a function
used to pull the variable list out of AST."))

(defmethod var-instrument
    (key (instrumenter instrumenter) (ast ast-ref) &key print-strings)
  (iter (for var in (funcall key ast))
        (when-let* ((type (find-var-type (software instrumenter) var))
                    (type-index (get-type-index instrumenter
                                                type print-strings)))
          (for name-index = (get-name-index instrumenter (aget :name var)))
          (collect
              (cond
                ;; C string
                ((and print-strings
                      (array-or-pointer-type type)
                      (string= "char" (type-name type)))
                 (format nil "WRITE_TRACE_BLOB(~a, ~d, ~d, strlen(~a), ~a);"
                         *instrument-log-variable-name*
                         name-index
                         type-index
                         (aget :name var)
                         (aget :name var)))

                ;; C++ string
                ((string= "string" (type-name type))
                 (format nil
                         "WRITE_TRACE_BLOB(~a, ~d, ~d, ~a.length(), ~a.c_str());"
                         *instrument-log-variable-name*
                         name-index
                         type-index
                         (aget :name var)
                         (aget :name var)))

                ;; Normal variable
                (t
                 (format nil "WRITE_TRACE_VARIABLE(~a, ~d, ~d, ~a);"
                         *instrument-log-variable-name*
                         name-index
                         type-index
                         (aget :name var))))))))

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

(defun initialize-tracing (obj file-name env-name contains-entry
                           instrumenter)
  (assert (typep obj 'clang))

  (prepend-to-genome
   obj
   (if contains-entry
       ;; Object contains main(). Insert log variable definition and
       ;; initialization function. Use the "constructor" attribute to
       ;; run it on startup, before main() or C++ static initializers.
       (format nil
               "
FILE *~a;
void __attribute__((constructor(101))) __bi_setup_log_file() {
  ~a = ~a;
  const char *names[] = {~a};
  const type_description types[] = {~a};
  write_trace_header(~a, names, ~d, types, ~d);
}
"
               *instrument-log-variable-name*
               *instrument-log-variable-name*
               (if (or file-name env-name)
                   (file-open-str :file file-name :env env-name)
                   "stderr")
               (format nil "~{~s, ~}" (coerce (names instrumenter) 'list))
               (format nil "~{~a, ~}"
                       (coerce (type-descriptions instrumenter) 'list))
               *instrument-log-variable-name*
               (length (names instrumenter))
               (length (types instrumenter)))

       ;; Object does not contain main. Insert extern definition of log variable.
       (format nil "extern FILE *~a;~%"
               *instrument-log-variable-name*)))

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
            (lambda (instrumenter ast)
              (var-instrument {get-unbound-vals (software instrumenter)}
                              instrumenter ast
                              :print-strings print-strings))))
    (when-let ((position (position 'trace-scope-vars functions)))
      (setf (nth position functions)
            (lambda (instrumenter ast)
              (var-instrument {get-vars-in-scope (software instrumenter)}
                              instrumenter ast
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
