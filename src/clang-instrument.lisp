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
                                 trace-file trace-env instrument-exit
                                 filter postprocess-functions)
  (:documentation
   "Instrument OBJ to print AST index before each full statement.

The indices printed here are not clang-mutate counters, but rather the
position of the ast in (asts obj).

Keyword arguments are as follows:
  POINTS --------------- alist of additional values to print at specific points
  FUNCTIONS ------------ functions to calculate instrumentation at each point
  FUNCTIONS-AFTER ------ functions to calculate instrumentation after each point
  TRACE-FILE ----------- file for trace output
  TRACE-ENV ------------ trace output to file specified by ENV variable
  INSTRUMENT-EXIT ------ print counter of function body before exit
  FILTER --------------- function to select a subset of ASTs for instrumentation
  POSTPROCESS-FUNCTIONS  functions to execute after instrumentation
"))

(define-constant +write-trace-include+
  "
#define _GNU_SOURCE
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

enum type_format {
    UNSIGNED,                   /* unsigned integer */
    SIGNED,                     /* signed integer */
    FLOAT,                      /* floating point */
    POINTER,                    /* unsigned, interpret as address */
    BLOB,                       /* arbitrary bytes, do not interpret */
    INVALID_FORMAT
};

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

__attribute__((unused))
static void write_trace_id(FILE *out, uint32_t statement_id)
{
    fputc(STATEMENT_ID, out);
    fwrite(&statement_id, sizeof(statement_id), 1, out);
}

__attribute__((unused))
static void write_trace_aux(FILE *out, uint64_t value)
{
    fputc(AUXILIARY, out);
    fwrite(&value, sizeof(value), 1, out);
}

__attribute__((unused))
static void write_end_entry(FILE *out)
{
    fputc(END_ENTRY, out);
    fflush(out);
}

__attribute__((unused))
static void write_trace_variables(FILE *out, size_t n_vars, ...)
{
    va_list ap;

    va_start (ap, n_vars);
    for (size_t i = 0; i < n_vars; i++) {
        uint16_t name_index = va_arg(ap, int);
        uint16_t type_index = va_arg(ap, int);
        uint16_t size = va_arg(ap, int);
        enum type_format format = (enum type_format)va_arg(ap, int);

        fputc(VARIABLE, out);
        fwrite(&name_index, sizeof(name_index), 1, out);
        fwrite(&type_index, sizeof(type_index), 1, out);

        /* This code is tricky because va_args are subject to standard
         promotions: smaller integers become ints, and floats become
         doubles. Other types are left alone.
        */
        switch (format) {
        case UNSIGNED:
        case SIGNED:
          switch (size) {
          case 1:
              {
                  char val = va_arg(ap, int);
                  fwrite(&val, sizeof(val), 1, out);
                  break;
              }
          case 2:
              {
                  int16_t val = va_arg(ap, int);
                  fwrite(&val, sizeof(val), 1, out);
                  break;
              }
          case 4:
              {
                  int32_t val = va_arg(ap, int);
                  fwrite(&val, sizeof(val), 1, out);
                  break;
              }
          case 8:
              {
                  int64_t val = va_arg(ap, int);
                  fwrite(&val, sizeof(val), 1, out);
                  break;
              }
          }
          break;
        case FLOAT:
          if (size == 4) {
              float val = (float)va_arg(ap, double);
              fwrite(&val, sizeof(val), 1, out);
              break;
          }
          else {
              double val = va_arg(ap, double);
              fwrite(&val, sizeof(val), 1, out);
              break;
          }
          break;
        case POINTER:
            {
                void *val = va_arg(ap, void*);
                fwrite(&val, sizeof(val), 1, out);
            }
            break;
        case BLOB:
        case INVALID_FORMAT:
        default:
          break;
        }
    }
}

__attribute__((unused))
static void write_trace_blobs(FILE *out, size_t n_vars, ...)
{
    va_list ap;

    va_start (ap, n_vars);
    for (size_t i = 0; i < n_vars; i++) {
        uint16_t name_index = va_arg(ap, int);
        uint16_t type_index = va_arg(ap, int);
        uint16_t size = va_arg(ap, int);
        void *value = va_arg(ap, void*);

        fputc(VARIABLE, out);
        fwrite(&name_index, sizeof(name_index), 1, out);
        fwrite(&type_index, sizeof(type_index), 1, out);
        fwrite(&size, sizeof(size), 1, out);
        fwrite(value, size, 1, out);
    }
}
"
  :test #'string=
  :documentation "C code to include in all instrumented files.")


(define-constant +write-trace-impl+
  "
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
(defclass clang-instrumenter (instrumenter) ())

(defun array-or-pointer-type (type)
  ;; array or pointer, but not array of pointers
  (xor (not (emptyp (type-array type)))
       (type-pointer type)))

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

(defmethod instrument ((obj clang) &rest args)
    ;; Send object through clang-mutate to get accurate counters
  (update-asts obj)
  (apply #'instrument (make-instance 'clang-instrumenter :software obj)
         args))

(defmethod instrument
    ((instrumenter clang-instrumenter)
     &key points functions functions-after trace-file trace-env instrument-exit
       (postprocess-functions (list #'clang-format)) (filter #'identity))
  ;; Default value for TRACE-ENV is `*instrument-log-env-name*'.  This
  ;; allows users to specify tracing from the environment without
  ;; having to export this above value (which we'd prefer not be
  ;; modified as it's assumed elsewhere).
  (when (eq trace-env t) (setf trace-env *instrument-log-env-name*))
  (let* ((obj (software instrumenter))
         (entry (get-entry obj))
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
                              aux-values)
           ;; Return clang-mutate ops to instrument AST
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
                                  *instrument-log-variable-name*
                                  (get-ast-id instrumenter
                                              (function-body obj function))
                                  *instrument-log-variable-name*
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
                                           *instrument-log-variable-name*
                                           (get-ast-id instrumenter ast))
                                   extra-stmts-after
                                   (format nil "write_end_entry(~a)"
                                           *instrument-log-variable-name*))))))

              ;; Opening brace and instrumentation code before
              (unless skip
                `((:insert-value
                   (:stmt1 . ,counter)
                   (:value1 .
                      ,(format
                        nil "~:[~;{~]~{~a~};~%"
                        wrap
                        (append
                         (cons
                          (format nil ; Start up alist w/counter.
                                  "write_trace_id(~a, ~du);~%"
                                  *instrument-log-variable-name*
                                  (get-ast-id instrumenter ast))
                          (mapcar {format nil
                                          "write_trace_aux(~a, ~a);~%"
                                          *instrument-log-variable-name*}
                                  aux-values))
                         extra-stmts
                         `(,(format nil "write_end_entry(~a)"
                                    *instrument-log-variable-name*))))))))))))
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
            (warn "No insertion point found for point ~a." point))
          (remove-if-not #'cdr points))
    (initialize-tracing obj trace-file trace-env entry instrumenter)
    (when entry
      (prepend-to-genome obj +write-trace-impl+))
    (prepend-to-genome obj +write-trace-include+)

    (when postprocess-functions
      (mapcar {funcall _ obj} postprocess-functions))

    obj))

(defmethod instrument-c-exprs ((instrumenter clang-instrumenter)
                               exprs-and-types print-strings)
  "Generate C code to print the values of expressions.

EXPRS-AND-TYPES is a list of (string . clang-type) pairs.

Returns a list of strings containing C source code.
"
  (labels
      ((string-type-p (type)
         (or (string= (type-name type) "string")
             (and (string= (type-name type) "char")
                  (array-or-pointer-type type))))

       (type-format-and-size (type print-strings)
         (let ((unqualified-c-type (type-trace-string type :qualified nil)))
           (cond
             ;; String
             ((and print-strings (string-type-p type))
              '(:BLOB "0"))
             ;; Pointer
             ;; Use sizeof(void*) in case underlying type is not yet declared.
             ((or (starts-with "*" unqualified-c-type :test #'string=)
                  (starts-with "[" unqualified-c-type :test #'string=))
              '(:POINTER "sizeof(void*)"))
             ;; Signed integers
             ((member unqualified-c-type
                      '("char" "int8_t" "wchar_t" "short" "int16_t" "int"
                        "int32_t" "long" "int64_t")
                      :test #'string=)
              (list :SIGNED (format nil "sizeof(~a)"
                                    (type-decl-string type :qualified nil))))
             ;; Unsigned integers
             ((member unqualified-c-type
                      '("unsigned char" "uint8_t" "unsigned short" "uint16_t"
                        "unsigned int" "uint32_t" "unsigned long" "uint64_t"
                        "size_t")
                      :test #'string=)
              (list :UNSIGNED (format nil "sizeof(~a)"
                                      (type-decl-string type :qualified nil))))
             ((string= unqualified-c-type "float")
              '(:FLOAT "sizeof(float)"))
             ((string= unqualified-c-type "double")
              '(:FLOAT "sizeof(double)"))
             ;; Otherwise no instrumentation
             (t '(nil nil)))))

       (get-name-index (name)
         (or (position name (names instrumenter) :test #'string=)
             (vector-push-extend name (names instrumenter))))

       (type-description-struct (c-type format size)
         (format nil "{~a, ~a, ~a}"
                 (get-name-index c-type)
                 format
                 size))

       (get-type-index (type format size print-strings)
         (let* ((c-type (type-trace-string type))
                (type-id (cons (and print-strings (string-type-p type))
                               c-type)))
           (or (position type-id (types instrumenter) :test #'equal)
               ;; Adding new type: generate header string
               (when-let ((description (type-description-struct c-type
                                                                format size)))
                 (vector-push-extend description
                                     (type-descriptions instrumenter))
                 (vector-push-extend type-id
                                     (types instrumenter)))))))

    (iter (for (expr . type) in exprs-and-types)
          (destructuring-bind (format size)
              (type-format-and-size type print-strings)
            (when format
              (let ((type-index (get-type-index type format size print-strings))
                    (name-index (get-name-index expr)))
                (cond
                  ;; C string
                  ((and print-strings
                        (array-or-pointer-type type)
                        (string= "char" (type-name type)))
                   (collect (format nil "~d, ~d, strlen(~a), ~a"
                                    name-index type-index expr expr)
                     into blob-snippets))

                  ;; C++ string
                  ((and print-strings
                        (string= "string" (type-name type)))
                   (collect (format nil
                                    "~d, ~d, (~a).length(), (~a).c_str()"
                                    name-index type-index expr expr)
                     into blob-snippets))

                  ;; Normal variable
                  (t
                   (collect
                       (format nil "~a, ~a, ~a, ~a, ~a"
                               name-index type-index size format expr)
                     into var-snippets))))))
          (finally
           (return
             (append (&>> var-snippets
                          (format nil
                                  "write_trace_variables(~a, ~d, ~{~a~^,~%~});~%"
                                  *instrument-log-variable-name*
                                  (length var-snippets))
                          (list))
                     (&>> blob-snippets
                          (format nil
                                  "write_trace_blobs(~a, ~d, ~{~a~^,~%~});~%"
                                  *instrument-log-variable-name*
                                  (length blob-snippets))
                          (list))))))))

(defgeneric var-instrument (key instrumenter ast &key print-strings)
  (:documentation
   "Return C statements for variable instrumentation.
INSTRUMENTER contains instrumentation state. KEY should be a function
used to pull the variable list out of AST."))

(defmethod var-instrument
    (key (instrumenter instrumenter) (ast ast-ref) &key print-strings)
  (iter (for var in (funcall key ast))
        (when-let* ((type (find-var-type (software instrumenter) var))
                    (name (aget :name var))
                    ;; Don't instrument nameless variables
                    (has-name (not (emptyp name))))
          (collect (cons name type) into names-and-types))
        (finally
         (return (instrument-c-exprs instrumenter names-and-types
                                     print-strings)))))

(defgeneric get-entry (software)
  (:documentation "Return the entry AST in SOFTWARE."))

(defmethod get-entry ((obj clang))
  (&>> (find-if [{string= "main"} {ast-name}] (functions obj))
       (function-body obj)))

(defun initialize-tracing (obj file-name env-name contains-entry
                           instrumenter)
  (assert (typep obj 'clang))

  (flet
      ((file-open-str ()
         (cond
           (file-name
            (format nil "fopen(~s, \"w\")" (namestring file-name)))
           (env-name
            (format nil "fopen(getenv(~s) ? getenv(~s) : \"/dev/null\", \"w\")"
                    env-name env-name))
           (t "stderr"))))

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
                 (file-open-str)
                 (format nil "~{~s, ~}" (coerce (names instrumenter) 'list))
                 (format nil "~{~a, ~}"
                         (coerce (type-descriptions instrumenter) 'list))
                 *instrument-log-variable-name*
                 (length (names instrumenter))
                 (length (types instrumenter)))

         ;; Object does not contain main. Insert extern definition of log variable.
         (format nil "extern FILE *~a;~%"
                 *instrument-log-variable-name*))))

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
        points functions print-strings instrument-exit)
    (when (or (not args)
              (< (length args) 1)
              (string= (subseq (car args) 0 (min 2 (length (car args))))
                       "-h")
              (string= (subseq (car args) 0 (min 3 (length (car args))))
                       "--h"))
      (format t "Usage: ~a SOURCE [OPTIONS]
 Instrument SOURCE along OPTIONS.

Options:
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
                          :instrument-exit instrument-exit)))))
      (note 1 "Writing instrumented to ~a." dest)
      (with-open-file
          (out dest :direction :output :if-exists :supersede)
        (genome-string instrumented out)))))

(defmethod instrument ((clang-project clang-project) &rest args)
  "Instrument a project. Arguments are passed through to instrument on
the underlying software objects."
  (let ((instrumenter (make-instance 'clang-instrumenter))
        (files (if (current-file clang-project)
                   (list (current-file clang-project))
                   (mapcar #'cdr (evolve-files clang-project)))))
    (labels
        ((check-ids (file-id ast-id)
           (assert (< file-id (ash 1 +trace-id-file-bits+)))
           (assert (< ast-id (ash 1 +trace-id-statement-bits+))))
         (instrument-file (obj index)
           ;; Send object through clang-mutate to get accurate counters
           (update-asts obj)
           (setf (software instrumenter) obj)

           ;; Set AST ids for new file
           (setf (ast-ids instrumenter) (make-ast-ht))
           (iter (for ast in (asts obj))
                 (for ast-i upfrom 0)
                 (check-ids index ast-i)
                 (setf (gethash ast (ast-ids instrumenter))
                       (logior (ash 1 31)                            ; flag bit
                               (ash index +trace-id-statement-bits+) ; file ID
                               ast-i)))                              ; AST ID
           (apply #'instrument instrumenter args)))

      ;; Fully instrument evolve-files
      ;; Defer any files with main() to the end, because they need
      ;; code for trace headers which depends on the instrumentation
      ;; of other files.
      (iter (for obj in files)
            (for i upfrom 0)
            (unless (get-entry obj) (instrument-file obj i)))
      (iter (for obj in files)
            (for i upfrom 0)
            (when (get-entry obj) (instrument-file obj i))))

    ;; Insert log setup code in other-files
    (iter (for obj in (mapcar #'cdr (other-files clang-project)))
          (setf (software instrumenter) obj)
          (when-let ((entry (get-entry obj)))
            (prepend-to-genome obj +write-trace-impl+)
            (initialize-tracing obj
                                (plist-get :trace-file args)
                                (plist-get :trace-env args)
                                entry
                                instrumenter)
            (prepend-to-genome obj +write-trace-include+))))

  clang-project)
