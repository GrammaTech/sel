;;; clang-instrument --- Instrument C-language source files
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;;; Instrumentation
(define-constant +write-trace-include+
  "
#ifndef __GT_TRACEDB_INCLUDE
#define __GT_TRACEDB_INCLUDE
#define _GNU_SOURCE
#include <pthread.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

enum type_format {
    __GT_TRACEDB_UNSIGNED,       /* unsigned integer */
    __GT_TRACEDB_SIGNED,         /* signed integer */
    __GT_TRACEDB_FLOAT,          /* floating point */
    __GT_TRACEDB_POINTER,        /* unsigned, interpret as address */
    __GT_TRACEDB_BLOB,           /* arbitrary bytes, do not interpret */
    __GT_TRACEDB_INVALID_FORMAT
};

enum trace_entry_tag {
    __GT_TRACEDB_END_ENTRY = 0,
    __GT_TRACEDB_STATEMENT_ID,
    __GT_TRACEDB_VARIABLE,
    __GT_TRACEDB_BUFFER_SIZE,
    __GT_TRACEDB_AUXILIARY,
    __GT_TRACEDB_TRACE_TAG_ERROR,
    /* Returned at EOF, should not appear in trace */
    __GT_TRACEDB_END_OF_TRACE
};

__attribute__((unused))
static void write_trace_id(FILE *out, pthread_mutex_t *lock,
                           uint64_t statement_id)
{
    /* Write trace point as single transaction */
    pthread_mutex_lock(lock);

    fputc(__GT_TRACEDB_STATEMENT_ID, out);
    fwrite(&statement_id, sizeof(statement_id), 1, out);
}

__attribute__((unused))
static void write_trace_aux(FILE *out, uint64_t value)
{
    fputc(__GT_TRACEDB_AUXILIARY, out);
    fwrite(&value, sizeof(value), 1, out);
}

__attribute__((unused))
static void write_end_entry(FILE *out, pthread_mutex_t *lock)
{
    fputc(__GT_TRACEDB_END_ENTRY, out);
    fflush(out);

    /* Finished writing trace point */
    pthread_mutex_unlock(lock);
}

__attribute__((unused))
static void write_trace_variables(FILE *out, uint32_t n_vars, ...)
{
    va_list ap;
    uint32_t i;

    va_start (ap, n_vars);
    for (i = 0; i < n_vars; i++) {
        uint32_t name_index = va_arg(ap, uint32_t);
        uint32_t type_index = va_arg(ap, uint32_t);
        uint32_t size = va_arg(ap, uint32_t);
        enum type_format format = (enum type_format)va_arg(ap, int);

        fputc(__GT_TRACEDB_VARIABLE, out);
        fwrite(&name_index, sizeof(name_index), 1, out);
        fwrite(&type_index, sizeof(type_index), 1, out);

        /* This code is tricky because va_args are subject to standard
         promotions: smaller integers become ints, and floats become
         doubles. Other types are left alone.
        */
        switch (format) {
        case __GT_TRACEDB_UNSIGNED:
        case __GT_TRACEDB_SIGNED:
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
                  int64_t val = va_arg(ap, int64_t);
                  fwrite(&val, sizeof(val), 1, out);
                  break;
              }
          }
          break;
        case __GT_TRACEDB_FLOAT:
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
        case __GT_TRACEDB_POINTER:
            {
                void *val = va_arg(ap, void*);
                fwrite(&val, sizeof(val), 1, out);
            }
            break;
        case __GT_TRACEDB_BLOB:
        case __GT_TRACEDB_INVALID_FORMAT:
        default:
          break;
        }
    }
}

__attribute__((unused))
static void write_trace_blobs(FILE *out, uint32_t n_vars, ...)
{
    va_list ap;
    uint32_t i;

    va_start (ap, n_vars);
    for (i = 0; i < n_vars; i++) {
        uint32_t name_index = va_arg(ap, uint32_t);
        uint32_t type_index = va_arg(ap, uint32_t);
        uint32_t size = va_arg(ap, uint32_t);
        void *value = va_arg(ap, void*);

        fputc(__GT_TRACEDB_VARIABLE, out);
        fwrite(&name_index, sizeof(name_index), 1, out);
        fwrite(&type_index, sizeof(type_index), 1, out);
        fwrite(&size, sizeof(size), 1, out);
        fwrite(value, size, 1, out);
    }
}
#endif
"
  :test #'string=
  :documentation "C code to include in all instrumented files.")


(define-constant +write-trace-impl+
  "
#ifndef __GT_TRACEDB_IMPL
#define __GT_TRACEDB_IMPL
typedef struct {
    /* Index into the string dictionary which gives the name of the type. */
    uint32_t name_index;
    /* Data format */
    enum type_format format;
    /* Size in bytes. 0 indicates a variable-sized object. */
    uint32_t size;
} type_description;


void write_trace_header(FILE *out, pthread_mutex_t *lock,
                        const char **names, uint32_t n_names,
                        const type_description *types, uint32_t n_types)
{
    /* Dictionary of names, as a sequence of NULL-terminated strings */
    uint64_t total_size = 0;
    uint32_t i = 0;

    /* Write trace header as single transaction */
    pthread_mutex_lock(lock);

    for (i = 0; i < n_names; i++) {
        total_size += strlen(names[i]) + 1;
    }
    fwrite(&total_size, sizeof(total_size), 1, out);

    for (i = 0; i < n_names; i++) {
        fputs(names[i], out);
        fputc(0, out);
    }

    /* Dictionary of types */
    fwrite(&n_types, sizeof(n_types), 1, out);
    fwrite(types, sizeof(*types), n_types, out);

    /* Finished writing trace header */
    pthread_mutex_unlock(lock);
}
#endif
"
  :test #'string=
  :documentation "C code which implements trace writing.")

(define-constant +write-trace-initialization+
  (concatenate 'string "
#include <unistd.h>
void __attribute__((constructor(101))) __bi_setup_log_file() {
  FILE *handshake_file = NULL;
  const char *handshake_file_path = getenv(\"~a\");
  char buffer[1024] = \"/dev/null\";
  if (handshake_file_path) {
    while (access(handshake_file_path, 0) != 0) { sleep(1); }
    handshake_file = fopen(handshake_file_path, \"r\");
    while (fgets(buffer, 1024, handshake_file) == NULL) { sleep(1); }
    buffer[strcspn(buffer, \"\\n\")] = 0;
    fclose(handshake_file);
    unlink(handshake_file_path);
  }
  " +instrument-log-variable-name+ " = ~a;
  const char *names[] = {~{~s, ~}};
  const type_description types[] = {~{~a, ~}};
  pthread_mutex_init(&" +instrument-log-lock-variable-name+ ", NULL);
  write_trace_header(" +instrument-log-variable-name+ ",
                     &" +instrument-log-lock-variable-name+",
                     names, ~d, types, ~d);
}
")
  :test #'string=
  :documentation "C code which initializes the trace file")

(define-constant +write-trace-file-definition+
  (format nil "FILE *~a;~%" +instrument-log-variable-name+)
  :test #'string=
  :documentation "C code which defines the trace file")

(define-constant +write-trace-file-declaration+
  (format nil "extern FILE *~a;~%" +instrument-log-variable-name+)
  :test #'string=
  :documentation "C code which declares the trace file")

(define-constant +write-trace-file-lock-definition+
  (format nil "pthread_mutex_t ~a;~%" +instrument-log-lock-variable-name+)
  :test #'string=
  :documentation "C code which defines the trace file lock")

(define-constant +write-trace-file-lock-declaration+
  (format nil "extern pthread_mutex_t ~a;~%" +instrument-log-lock-variable-name+)
  :test #'string=
  :documentation "C code which declares the trace file lock")

(defclass clang-instrumenter (instrumenter)
  ((names :accessor names
          :initform (make-hash-table :test #'equal))
   (types :accessor types
          :initform (make-hash-table :test #'equal))
   (type-descriptions :accessor type-descriptions
          :initform (make-hash-table :test #'equal))
   (ast-ids :accessor ast-ids :initform nil))
  (:documentation "Handles instrumentation for clang software objects."))

(defmethod initialize-instance :after ((instance clang-instrumenter) &key)
  ;; Values are the same as index-of-ast, but without the linear
  ;; search.
  (when (software instance)
    (setf (ast-ids instance)
          (iter (for ast in (asts (software instance)))
                (for i upfrom 0)
                (with ht = (make-hash-table :test #'eq))
                (setf (gethash ast ht) i)
                (finally (return ht))))))


(defun array-or-pointer-type (type)
  "Is TYPE an array or pointer (but not an array of pointers)?
* TYPE a type object
"
  ;; array or pointer, but not array of pointers
  (xor (not (emptyp (type-array type)))
       (type-pointer type)))

(defun get-ast-id (instrumenter ast)
  "Return the trace ID for AST
* INSTRUMENTER current instrumentation state
* AST the AST to instrument
"
  (gethash ast (ast-ids instrumenter)))


(defgeneric write-trace-id (instrumenter ast)
  (:documentation "Generate ASTs which write statement ID to trace.
* INSTRUMENTER current instrumentation state
* AST the AST to instrument
"))

(defmethod write-trace-id ((instrumenter clang-instrumenter) ast)
  "Generate ASTs which write statement ID to trace.
* INSTRUMENTER current instrumentation state
* AST the AST to instrument
"
  (make-call-expr "write_trace_id"
                  (list (make-var-reference +instrument-log-variable-name+ nil)
                        (make-var-reference
                          (format nil "&~a"
                                  +instrument-log-lock-variable-name+)
                          nil)
                        (make-literal (get-ast-id instrumenter ast) :unsigned))
                  :fullstmt
                  :full-stmt t
                  :aux-data '((:instrumentation t))))

(defgeneric write-trace-aux (instrumenter value)
  (:documentation "Generate ASTs which write aux entries to trace.
* INSTRUMENTER current instrumentation state
* VALUE the auxiliary value to write
"))

(defmethod write-trace-aux ((instrumenter clang-instrumenter) value)
  "Generate ASTs which write aux entries to trace.
* INSTRUMENTER current instrumentation state
* VALUE the auxiliary value to write
"
  (declare (ignorable instrumenter))
  (make-call-expr "write_trace_aux"
                  (list (make-var-reference +instrument-log-variable-name+ nil)
                        (make-literal value))
                  :fullstmt
                  :full-stmt t
                  :aux-data '((:instrumentation t))))

(defgeneric write-end-entry (instrumenter)
  (:documentation "Generate ASTs which write end-entry flag to trace.
* INSTRUMENTER current instrumentation state
"))

(defmethod write-end-entry ((instrumenter clang-instrumenter))
  "Generate ASTs which write end-entry flag to trace.
* INSTRUMENTER current instrumentation state
"
  (declare (ignorable instrumenter))
  (make-call-expr "write_end_entry"
                  (list (make-var-reference +instrument-log-variable-name+ nil)
                        (make-var-reference
                          (format nil "&~a"
                                  +instrument-log-lock-variable-name+)
                          nil))
                  :fullstmt
                  :full-stmt t
                  :aux-data '((:instrumentation t))))

(defgeneric instrument-return (instrumenter return-stmt return-void)
  (:documentation "Generate ASTs which instrument RETURN-STMT.
The generated ASTs will store the return value in a temporary variable
and jump to the exit point, allowing instrumentation of function exit.
* INSTRUMENTER current instrumentation state
* RETURN-STMT the AST for a return statement
* RETURN-VOID does this function return void?
"))

(defmethod instrument-return ((instrumenter clang-instrumenter)
                              return-stmt return-void)
  "Generate ASTs which instrument RETURN-STMT.
The generated ASTs will store the return value in a local variable
and jump to the exit point, allowing instrumentation of function exit.
* INSTRUMENTER current instrumentation state
* RETURN-STMT the AST for a return statement
* RETURN-VOID does this function return void?
"
  (if return-void
      `(,(make-statement :gotostmt :fullstmt '("goto inst_exit")
                         :full-stmt t
                         :aux-data '((:instrumentation t))))
      `(,(make-operator :fullstmt "="
                        (list (make-var-reference "_inst_ret" nil)
                              (car (get-immediate-children
                                     (software instrumenter)
                                     return-stmt)))
                        :full-stmt t
                        :aux-data '((:instrumentation t)))
        ,(make-statement :gotostmt :fullstmt '("goto inst_exit")
                         :full-stmt t
                         :aux-data '((:instrumentation t))))))

(defgeneric instrument-exit (instrumenter function return-void)
  (:documentation "Generate ASTs to instrument exit from FUNCTION.

The generated ASTs add a label which can be jumped to from return
statements, and return a value stored in a local variable, allowing
instrumentation of function exit.

* INSTRUMENTER current instrumentation state
* FUNCTION the AST for the current function
* RETURN-VOID does this function return void?
"))

(defmethod instrument-exit ((instrumenter clang-instrumenter)
                            function return-void)
  "Generate ASTs to instrument exit from FUNCTION.

The generated ASTs add a label which can be jumped to from return
statements, and return a value stored in a local variable, allowing
instrumentation of function exit.

* INSTRUMENTER current instrumentation state
* FUNCTION the AST for the current function
* RETURN-VOID does this function return void?
"
  (let ((obj (software instrumenter)))
    `(,(make-label "inst_exit"
                   ;; ast-id hash table uses eq, but function-body will
                   ;; generate a new ast. Search for the original in
                   ;; order to look up the id.
                   (write-trace-id instrumenter
                                   (find-if {equalp (function-body obj
                                                                   function)}
                                            (asts obj)))
                   :aux-data '((:instrumentation t)))
      ,(write-end-entry instrumenter)
      ,(make-statement :ReturnStmt :fullstmt
                       (cons "return "
                             (when (not return-void)
                               (list (make-var-reference "_inst_ret"
                                                         nil))))
                       :full-stmt t
                       :aux-data '((:instrumentation t))))))

(defmethod instrumented-p ((clang clang))
  "Return true if CLANG is instrumented
* CLANG a clang software object
"
  (search +instrument-log-variable-name+ (genome clang)))

(defmethod instrument ((obj clang) &rest args)
  "Instrumentation for clang software objects.
Creates a CLANG-INSTRUMENTER for OBJ and calls its instrument method.

* OBJ the software object to instrument
* ARGS additional arguments are passed through to the instrumenter method.
"
  (apply #'instrument (make-instance 'clang-instrumenter :software obj)
         args))

(defmethod instrument
    ((instrumenter clang-instrumenter)
     &key points functions functions-after trace-file trace-env instrument-exit
       (filter #'identity))
  "Use INSTRUMENTER to instrument a clang software object.

* INSTRUMENTER current instrumentation state
* POINTS alist of additional values to print at specific points
* FUNCTIONS  functions to calculate instrumentation at each point
* FUNCTIONS-AFTER functions to calculate instrumentation after each point
* TRACE-FILE file or stream (stdout/stderr) for trace output
* TRACE-ENV trace output to file specified by ENV variable
* INSTRUMENT-EXIT print counter of function body before exit
* FILTER function to select a subset of ASTs for instrumentation
* POSTPROCESS-FUNCTIONS functions to execute after instrumentation"
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
        ((last-traceable-stmt (proto)
           (->> (function-body obj proto)
                (get-immediate-children obj)
                (lastcar)
                (enclosing-traceable-stmt obj)))
         (first-traceable-stmt (proto)
           (first (get-immediate-children obj (function-body obj proto))))
         (instrument-asts (obj)
           "Generate instrumentation for all ASTs in OBJ.  As a side-effect,
update POINTS after instrumenting ASTs."
           (-<>> (asts obj)
                 (remove-if-not {can-be-made-traceable-p obj})
                 (funcall filter)
                 (sort <> #'ast-later-p)
                 ;; Generate all instrumentation before applying changes
                 (mapcar
                   (lambda (ast)
                     (prog1
                         (instrument-ast ast
                                         (mappend {funcall _ instrumenter ast}
                                                  functions)
                                         (mappend {funcall _ instrumenter ast}
                                                  functions-after)
                                         (when points
                                           (aget ast points :test #'equalp)))
                       (when points
                         (setf (aget ast points :test #'equalp) nil)))))))
         (instrument-ast (ast extra-stmts extra-stmts-after aux-values)
           "Generate instrumentation for AST.
Returns a list of (AST RETURN-TYPE INSTRUMENTATION-BEFORE INSTRUMENTATION-AFTER).
"

           (let* ((function (when instrument-exit
                              (function-containing-ast obj ast)))
                  (return-type (when (and function
                                          (not (ast-void-ret function)))
                                 (find-type obj (ast-ret function)))))

             `(,ast
               ,return-type
               ;; Instrumentation before
               (;; Temp variable for return value
                ,@(when (and instrument-exit
                             (equalp ast (first-traceable-stmt function))
                             return-type)
                        `(,(make-var-decl "_inst_ret" return-type nil
                                          :aux-data '((:instrumentation t)))))
                ;; Instrument before
                ,(write-trace-id instrumenter ast)
                ,@(mapcar {write-trace-aux instrumenter} aux-values)
                ,@extra-stmts
                ,(write-end-entry instrumenter))
               (;; Instrumentation after
                ,@(when functions-after
                        `(,(write-trace-id instrumenter ast)
                          ,@extra-stmts-after
                          ,(write-end-entry instrumenter)))
                ;; Function exit instrumentation
                ,@(when (and instrument-exit
                             (equalp ast (last-traceable-stmt function)))
                        (instrument-exit instrumenter function
                                         (null return-type)))))))
         (add-semicolon (ast semi-position)
           (cond ((eq semi-position :before)
                  (labels ((add-semi-before (ast children)
                             (if (stringp (car children))
                                 (copy ast :children
                                           (cons (concatenate 'string ";"
                                                              (car children))
                                                 (cdr children)))
                                 (copy ast :children
                                           (cons (add-semi-before
                                                   (car children)
                                                   (ast-children (car children)))
                                                 (cdr children))))))
                    (add-semi-before ast (ast-children ast))))
                 ((eq semi-position :after)
                  (copy ast :children (append (ast-children ast) (list ";"))))
                 ((eq semi-position :both)
                  (-> (add-semicolon ast :before)
                      (add-semicolon :after)))
                 (t ast)))
         (create-value (obj ast return-type before after)
           (let* (;; Look up AST again in case its children have been
                  ;; instrumented
                  (wrap (not (traceable-stmt-p obj ast)))
                  (new-ast (get-ast obj (ast-path ast)))
                  (stmts (append (mapcar {add-semicolon _ :after} before)
                                 (if (and instrument-exit
                                          (eq (ast-class ast) :ReturnStmt))
                                     (->> (instrument-return instrumenter
                                                             new-ast
                                                             (null return-type))
                                          (mapcar {add-semicolon _ :both}))
                                     (list new-ast))
                                 (mapcar {add-semicolon _ :both} after))))
             ;; Wrap in compound statement if needed
             (if wrap
                 (make-statement
                   :CompoundStmt
                   :FullStmt
                   `("{" ,@(interleave stmts ";") ";}")
                   :full-stmt t
                   :aux-data '((:instrumentation t)))
                 stmts))))

    ;; Apply mutations to instrument OBJ.
    ;; Note: These mutations are sent as a list to `apply-mutation-ops`
    ;; and are performed sequently.  This offers a performance improvement
    ;; over calling `apply-mutation-ops` multiple times.
    (apply-mutation-ops
      obj
      (iter (for (ast return-type before after) in (instrument-asts obj))
            (collect (if (not (traceable-stmt-p obj ast))
                         `(:set (:stmt1 . ,ast)
                                (:value1 . ,{create-value obj
                                                          ast
                                                          return-type
                                                          before
                                                          after}))
                         `(:splice (:stmt1 . ,ast)
                                   (:value1 . ,{create-value obj
                                                             ast
                                                             return-type
                                                             before
                                                             after})))))))

    ;; Warn about any leftover un-inserted points.
    (mapc (lambda (point)
            (warn "No insertion point found for point ~a." point))
          (remove-if-not #'cdr points))

    ;; Add support code for tracing to obj
    (initialize-tracing instrumenter trace-file trace-env entry)
    (when entry
      (prepend-to-genome obj +write-trace-impl+))
    (prepend-to-genome obj +write-trace-include+)

    ;; Add flag to allow building with pthreads
    (appendf (flags obj) (list "-lpthread"))

    obj))

(defmethod uninstrument ((clang clang))
  "Remove instrumentation from CLANG"
  (labels ((uninstrument-genome-prologue (clang)
             (with-slots (ast-root) clang
               (setf ast-root
                 (copy ast-root
                       :children
                       (append
                         (-> (ast-children ast-root)
                             (first)
                             (replace-all +write-trace-impl+
                                          "")
                             (replace-all +write-trace-include+
                                          "")
                             (replace-all +write-trace-file-declaration+
                                          "")
                             (replace-all +write-trace-file-definition+
                                          "")
                             (replace-all +write-trace-file-lock-declaration+
                                          "")
                             (replace-all +write-trace-file-lock-definition+
                                          "")
                             (list))
                         (cdr (ast-children ast-root)))))))
           (uninstrument-genome-epilogue (clang)
             (with-slots (ast-root) clang
               (setf ast-root
                 (copy ast-root
                       :children
                       (let ((last-child (-> (ast-root clang)
                                             (ast-children)
                                             (lastcar))))
                         (if (stringp last-child)
                             (append (butlast (ast-children (ast-root clang)))
                                     (-> (subseq
                                           last-child
                                           0
                                           (-<>> (split-sequence
                                                   #\Newline
                                                   +write-trace-initialization+)
                                                 (take 2)
                                                 (format nil "~{~a~%~}")
                                                 (search <> last-child)))
                                         (list)))
                             (ast-children (ast-root clang)))))))))

    ;; Remove instrumentation setup code
    (uninstrument-genome-prologue clang)
    (uninstrument-genome-epilogue clang)

    ;; Remove instrumented ASTs - blocks first, then individual statements
    (let ((*matching-free-var-retains-name-bias* 1.0)
          (*matching-free-function-retains-name-bias* 1.0))
      (apply-mutation-ops
        clang
        (iter (for ast in (->> (asts clang)
                               (remove-if-not [{aget :instrumentation}
                                               {ast-aux-data}])
                               (remove-if-not {block-p clang})
                               (reverse)))
              (collect `(:set (:stmt1 . ,ast)
                              (:value1 .
                               ,{get-ast clang
                                         (->> (get-ast clang (ast-path ast))
                                               (get-immediate-children clang)
                                               (remove-if
                                                 [{aget :instrumentation}
                                                  {ast-aux-data}])
                                               (first)
                                               (ast-path))})))))

      (apply-mutation-ops
        clang
        (iter (for ast in (->> (asts clang)
                               (remove-if-not [{aget :instrumentation}
                                               {ast-aux-data}])
                               (remove-if {block-p clang})
                               (reverse)))
              (collect `(:splice (:stmt1 . ,ast) (:value1 . nil)))))))

  ;; Return the software object
  clang)

(defmethod instrumentation-files ((clang-project clang-project))
  "Return files in CLANG-PROJECT in the order which they would be instrumented
* CLANG-PROJECT an instrumented project
"
  (append (remove-if {get-entry}
                     (evolve-files clang-project)
                     :key #'cdr)
          (remove-if [#'not {get-entry}]
                     (evolve-files clang-project)
                     :key #'cdr)))

(defmethod instrumented-p ((clang-project clang-project))
  "Return true if CLANG-PROJECT is instrumented
* CLANG-PROJECT a clang-project software object
"
  (some #'instrumented-p (mapcar #'cdr (evolve-files clang-project))))

(defmethod instrument ((clang-project clang-project) &rest args)
  "Instrument CLANG-PROJECT to print AST index before each full statement.

* CLANG-PROJECT the project to instrument
* ARGS passed through to the instrument method on underlying software objects.
"
  (let ((instrumenter (make-instance 'clang-instrumenter))
        (files (if (current-file clang-project)
                   (list (current-file clang-project))
                   (mapcar #'cdr (instrumentation-files clang-project)))))
    (labels
        ((check-ids (file-id ast-id)
           (assert (< file-id (ash 1 +trace-id-file-bits+)))
           (assert (< ast-id (ash 1 +trace-id-statement-bits+))))
         (instrument-file (obj index)
           ;; Send object through clang-mutate to get accurate counters
           (setf (software instrumenter) obj)

           ;; Set AST ids for new file
           (setf (ast-ids instrumenter) (make-hash-table :test #'eq))
           (iter (for ast in (asts obj))
                 (for ast-i upfrom 0)
                 (check-ids index ast-i)
                 (setf (gethash ast (ast-ids instrumenter))
                       (logior (ash 1 63)                            ; flag bit
                               (ash index +trace-id-statement-bits+) ; file ID
                               ast-i)))                              ; AST ID
           (apply #'instrument instrumenter args)))

      ;; Fully instrument evolve-files
      ;; Defer any files with main() to the end, because they need
      ;; code for trace headers which depends on the instrumentation
      ;; of other files.
      (iter (for obj in files)
            (for i upfrom 0)
            (instrument-file obj i))

      ;; Insert log setup code in other-files
      (iter (for obj in (mapcar #'cdr (other-files clang-project)))
            (setf (software instrumenter) obj)
            (when-let ((entry (get-entry obj)))
              (initialize-tracing instrumenter
                                  (plist-get :trace-file args)
                                  (plist-get :trace-env args)
                                  entry)
              (prepend-to-genome obj +write-trace-impl+)
              (prepend-to-genome obj +write-trace-include+)))))

  clang-project)

(defmethod uninstrument ((clang-project clang-project))
  "Remove instrumentation from CLANG-PROJECT"
  (iter (for (src-file . obj) in
             (append (instrumentation-files clang-project)
                     (remove-if-not [{get-entry} #'cdr]
                                    (other-files clang-project))))
        (declare (ignorable src-file))
        (uninstrument obj))
  clang-project)

(defgeneric instrument-c-exprs (instrumenter exprs-and-types print-strings)
  (:documentation "Generate C code to print the values of expressions.
EXPRS-AND-TYPES is a list of (string . clang-type) pairs.
Returns a list of strings containing C source code."))

(defmethod instrument-c-exprs ((instrumenter clang-instrumenter)
                               exprs-and-types print-strings)
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
              '(:__GT_TRACEDB_BLOB "0"))
             ;; Pointer
             ;; Use sizeof(void*) in case underlying type is not yet declared.
             ((or (starts-with "*" unqualified-c-type :test #'string=)
                  (starts-with "[" unqualified-c-type :test #'string=))
              '(:__GT_TRACEDB_POINTER "sizeof(void*)"))
             ;; Signed integers
             ((member unqualified-c-type
                      '("char" "int8_t" "wchar_t" "short" "int16_t" "int"
                        "int32_t" "long" "int64_t")
                      :test #'string=)
              (list :__GT_TRACEDB_SIGNED
                    (format nil "sizeof(~a)"
                                (type-decl-string type :qualified nil))))
             ;; Unsigned integers
             ((member unqualified-c-type
                      '("unsigned char" "uint8_t" "unsigned short" "uint16_t"
                        "unsigned int" "uint32_t" "unsigned long" "uint64_t"
                        "size_t")
                      :test #'string=)
              (list :__GT_TRACEDB_UNSIGNED
                    (format nil "sizeof(~a)"
                                (type-decl-string type :qualified nil))))
             ((string= unqualified-c-type "float")
              '(:__GT_TRACEDB_FLOAT "sizeof(float)"))
             ((string= unqualified-c-type "double")
              '(:__GT_TRACEDB_FLOAT "sizeof(double)"))
             ;; Otherwise no instrumentation
             (t '(nil nil)))))

       (get-name-index (name)
         (or (gethash name (names instrumenter))
             (setf (gethash name (names instrumenter))
                   (hash-table-count (names instrumenter)))))

       (type-description-struct (c-type format size)
         (format nil "{~a, ~a, ~a}"
                 (get-name-index c-type)
                 format
                 size))

       (get-type-index (type format size print-strings)
         (let* ((c-type (type-trace-string type))
                (type-id (cons (and print-strings (string-type-p type))
                               c-type)))
           (or (gethash type-id (types instrumenter))
               (progn
                 ;; Adding new type: generate header string
                 (setf (gethash type-id (type-descriptions instrumenter))
                       (type-description-struct c-type format size))
                 (setf (gethash type-id (types instrumenter))
                       (hash-table-count (types instrumenter)))))))
       (make-instrumentation-ast (function-name function-args)
         (when function-args
           (make-statement :CallExpr :FullStmt
                           `(,(make-statement :ImplictCastExpr :generic
                                (list (make-statement :DeclRefExpr :generic
                                                      (list function-name))))
                             ,(format nil "(~a, ~d, ~{~a~^, ~})"
                                          +instrument-log-variable-name+
                                          (length function-args)
                                          function-args))
                           :full-stmt t
                           :aux-data '((:instrumentation t))))))

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
                     into blob-args))

                  ;; C++ string
                  ((and print-strings
                        (string= "string" (type-name type)))
                   (collect (format nil
                                    "~d, ~d, (~a).length(), (~a).c_str()"
                                    name-index type-index expr expr)
                     into blob-args))

                  ;; Normal variable
                  (t
                   (collect
                       (format nil "~a, ~a, ~a, ~a, ~a"
                               name-index type-index size format expr)
                     into var-args))))))
          (finally
           (return
             (->> (append (&> (make-instrumentation-ast "write_trace_variables"
                                                        var-args)
                              (list))
                          (&> (make-instrumentation-ast "write_trace_blobs"
                                                        blob-args)
                              (list)))
                  (remove-if #'null)))))))

(defgeneric var-instrument (key instrumenter ast &key print-strings)
  (:documentation
   "Generate ASTs for variable instrumentation.
* KEY a function used to pull the variable list out of AST
* INSTRUMENTER current instrumentation state
* AST the AST to instrument
"))

(defmethod var-instrument
  (key (instrumenter instrumenter) (ast clang-ast) &key print-strings)
  "Generate ASTs for variable instrumentation.
* KEY a function used to pull the variable list out of AST
* INSTRUMENTER current instrumentation state
* AST the AST to instrument
"
  (iter (for var in (funcall key ast))
        (when-let* ((software (software instrumenter))
                    (type (&>> (find-var-type software var)
                               (typedef-type software)))
                    (name (aget :name var))
                    ;; Don't instrument nameless variables
                    (has-name (not (emptyp name))))
          (collect (cons name type) into names-and-types))
        (finally
         (return (instrument-c-exprs instrumenter names-and-types
                                     print-strings)))))

(defgeneric get-entry (software)
  (:documentation
   "Return the AST of the entry point (main function) in SOFTWARE."))

(defmethod get-entry ((obj clang))
  "Return the AST of the entry point (main function) in SOFTWARE.

OBJ a clang software object
"
  (when-let* ((main (find-if [{string= "main"} {ast-name}]
                             (functions obj)))
              (_1 (equal :Function (ast-class main)))
              (_2 (and (ast-ret main)
                       (member (type-name (find-type obj (ast-ret main)))
                               '("int" "void") :test #'string=))))
    (function-body obj main)))

(defun initialize-tracing (instrumenter file-name env-name contains-entry
                           &aux (obj (software instrumenter)))
  "Insert code to initialize tracing and/or define the log variable.

* INSTRUMENTER current instrumentation state with software object to instrument
* FILE-NAME fixed name for the trace output file or stream (:stdout/:stderr)
* ENV-NAME environment variable from which to read the trace output file
* CONTAINS-ENTRY does this object contain the entry point?
"
  (assert (typep obj 'clang))

  (labels ((file-open-str ()
             ;; Open the file at FILE-NAME or in the environment variable
             ;; ENV-NAME.  If neither is given, read the file to open
             ;; from the buffer containing the contents of the
             ;; SEL_HANDSHAKE_FILE.
             (cond
               ((eq file-name :stderr) "stderr")
               ((eq file-name :stdout) "stdout")
               (file-name (format nil "fopen(~s, \"w\")" (namestring file-name)))
               ((stringp env-name) (format nil "fopen(getenv(~a), \"w\")" env-name))
               (t (format nil "fopen(buffer, \"w\")")))))

    (if contains-entry
        ;; Object contains main() so insert setup code. The goal is to
        ;; insert this exactly once in each executable while avoiding
        ;; link problems. It doesn't need to be in the same file as
        ;; main() but that provides a good heuristic.

        ;; The setup function uses a "constructor" attribute to run
        ;; before any other code. It optionally performs a handshake
        ;; with the trace collector, then opens the trace file and
        ;; writes the header.
        (progn
          (prepend-to-genome obj +write-trace-file-lock-definition+)
          (prepend-to-genome obj +write-trace-file-definition+)
          (append-to-genome  obj
                             (format nil +write-trace-initialization+
                                     *instrument-handshake-env-name*
                                     (file-open-str)
                                     (-<>> (names instrumenter)
                                           (hash-table-alist)
                                           (sort <> #'< :key #'cdr)
                                           (mapcar #'car))
                                     (-<>> (type-descriptions instrumenter)
                                           (hash-table-alist)
                                           (sort <> #'<
                                                 :key [{gethash _
                                                        (types instrumenter)}
                                                       #'car])
                                           (mapcar #'cdr))
                                     (hash-table-count (names instrumenter))
                                     (hash-table-count (types instrumenter)))))

        ;; Object does not contain main. Insert extern declaration of
        ;; log variable and its lock.
        (progn
          (prepend-to-genome obj +write-trace-file-lock-declaration+)
          (prepend-to-genome obj +write-trace-file-declaration+))))

  obj)


;;;; Command line
(defun run-clang-instrument ()
  "Run `clang-instrument' on *COMMAND-LINE-ARGUMENTS*."
  (clang-instrument (cons (argv0) *command-line-arguments*)))

(defun clang-instrument (args)
  "Interface to the command line instrumentation tool.
* ARGS command-line arguments
"
  (in-package :sel)
  (let ((self (pop args))
        (original (make-instance 'clang
                    :compiler (or (getenv "CC") "clang")
                    :flags (getenv "CFLAGS")))
        (trace-file :stderr)
        path out-dir name type out-file save-original
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

Built with SEL version ~a, and ~a version ~a.~%"
              self +software-evolution-library-version+
              (lisp-implementation-type) (lisp-implementation-version))
      (quit))

    ;; Mandatory arguments.
    (setf path (pop args)
          out-dir (pathname-directory path)
          name (pathname-name path)
          type (pathname-type path)
          original (from-file original path))

    ;; Options.
    (getopts (args)
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
      ("-t" "--trace-file" (let ((arg (pop args)))
                             (setf trace-file
                                   (cond ((string= "stdout" arg) :stdout)
                                         ((string= "stderr" arg) :stderr)
                                         (t arg)))))
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
