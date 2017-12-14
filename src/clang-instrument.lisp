;;; clang-instrument --- Instrument C-language source files
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;;; Instrumentation

(defvar *instrument-log-variable-name* "__sel_trace_file"
  "Variable used for instrumentation.")

(defvar *instrument-log-env-name* "__SEL_TRACE_FILE"
  "Default environment variable in which to store log file.")

(defvar *instrument-handshake-env-name* "__SEL_HANDSHAKE_FILE"
  "Default environment variable in which to store log file.")

(defgeneric instrumented-p (obj)
  (:documentation "Return true if OBJ is instrumented"))

(defgeneric instrument (obj &key points functions functions-after
                                 trace-file trace-env instrument-exit filter)
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
"))

(defgeneric uninstrument (obj)
  (:documentation "Remove instrumentation from OBJ"))

(define-constant +write-trace-include+
  "
#ifndef __GT_TRACEDB_INCLUDE
#define __GT_TRACEDB_INCLUDE
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
static void write_trace_id(FILE *out, uint64_t statement_id)
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

        fputc(VARIABLE, out);
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


void write_trace_header(FILE *out, const char **names, uint32_t n_names,
                        const type_description *types, uint32_t n_types)
{
    /* Dictionary of names, as a sequence of NULL-terminated strings */
    uint64_t total_size = 0;
    uint32_t i = 0;
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
}
#endif
"
  :test #'string=
  :documentation "C code which implements trace writing.")

(define-constant +write-trace-initialization+
  "
#include <unistd.h>
void __attribute__((constructor(101))) __bi_setup_log_file() {
  const char *handshake_file = getenv(\"~a\");
  if (handshake_file) {
    while (access(handshake_file, 0) != 0) { sleep(1); }
    unlink(handshake_file);
  }
  ~a = ~a;
  const char *names[] = {~{~s, ~}};
  const type_description types[] = {~{~a, ~}};
  write_trace_header(~a, names, ~d, types, ~d);
}
"
  :test #'string=
  :documentation "C code which initializes the trace file")

(define-constant +write-trace-file-definition+
  "FILE *~a;~%"
  :test #'string=
  :documentation "C code which defines the trace file")

(define-constant +write-trace-file-declaration+
  "extern FILE *~a;~%"
  :test #'string=
  :documentation "C code which declares the trace file")

(defclass instrumenter ()
  ((software :accessor software :initarg :software :initform nil)
   (names :accessor names
          :initform (make-hash-table :test #'equal))
   (types :accessor types
          :initform (make-hash-table :test #'equal))
   (type-descriptions :accessor type-descriptions
          :initform (make-hash-table :test #'equal))
   (ast-ids :accessor ast-ids :initform nil)))
(defclass clang-instrumenter (instrumenter) ())

(defun array-or-pointer-type (type)
  ;; array or pointer, but not array of pointers
  (xor (not (emptyp (type-array type)))
       (type-pointer type)))

(defun get-ast-id (instrumenter ast)
  (gethash ast (ast-ids instrumenter)))

(defmethod initialize-instance :after ((instance instrumenter) &key)
  ;; Values are the same as index-of-ast, but without the linear
  ;; search.
  (when (software instance)
    (setf (ast-ids instance)
          (iter (for ast in (asts (software instance)))
                (for i upfrom 0)
                (with ht = (make-hash-table :test #'eq))
                (setf (gethash ast ht) i)
                (finally (return ht))))))

(defgeneric write-trace-id (instrumenter ast)
  (:documentation "Generate ASTs which write statement ID to trace."))

(defmethod write-trace-id ((instrumenter clang-instrumenter) ast)
  (make-call-expr "write_trace_id"
                  (list (make-var-reference *instrument-log-variable-name* nil)
                        (make-literal :unsigned (get-ast-id instrumenter ast)))
                  :fullstmt
                  :full-stmt t
                  :aux-data '((:instrumentation t))))

(defgeneric write-trace-aux (instrumenter value)
  (:documentation "Generate ASTs which write aux entries to trace."))

(defmethod write-trace-aux ((instrumenter clang-instrumenter) value)
  (declare (ignorable instrumenter))
  (make-call-expr "write_trace_aux"
                  (list (make-var-reference *instrument-log-variable-name* nil)
                        (make-literal :integer value))
                  :fullstmt
                  :full-stmt t
                  :aux-data '((:instrumentation t))))

(defgeneric write-end-entry (instrumenter)
  (:documentation "Generate ASTs which write end-entry flag to trace."))

(defmethod write-end-entry ((instrumenter clang-instrumenter))
  (declare (ignorable instrumenter))
  (make-call-expr "write_end_entry"
                  (list (make-var-reference *instrument-log-variable-name* nil))
                  :fullstmt
                  :full-stmt t
                  :aux-data '((:instrumentation t))))

(defgeneric instrument-return (instrumenter return-stmt return-void)
  (:documentation "Generic ASTs which instrument RETURN-STMT."))

(defmethod instrument-return ((instrumenter clang-instrumenter)
                              return-stmt return-void)
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
  (:documentation "Generate ASTs to instrument exit from FUNCTION."))

(defmethod instrument-exit ((instrumenter clang-instrumenter)
                            function return-void)
  (let ((obj (software instrumenter)))
    `(,(make-label "inst_exit"
                   ;; ast-id hash table uses eq, but function-body will
                   ;; generate a new ast-ref. Search for the original in
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
  (search *instrument-log-variable-name* (genome clang)))

(defmethod instrument ((obj clang) &rest args)
  (apply #'instrument (make-instance 'clang-instrumenter :software obj)
         args))

(defmethod instrument
    ((instrumenter clang-instrumenter)
     &key points functions functions-after trace-file trace-env instrument-exit
       (filter #'identity))
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
         (ast-ref->ast (ast-ref &key (semi nil)
                        &aux (ast (copy-list (ast-ref-ast ast-ref))))
           (cond ((eq semi :before)
                  (labels ((add-semi-before (ast)
                             (if (stringp (second ast))
                                 (setf (second ast)
                                       (concatenate 'string ";"
                                                    (second ast)))
                                 (add-semi-before (second ast)))))
                    (add-semi-before ast)
                    ast))
                 ((eq semi :after)
                  (append ast (list ";")))
                 ((eq semi :both)
                  (-<>> (make-ast-ref :ast ast)
                        (ast-ref->ast <> :semi :before)
                        (make-ast-ref :ast)
                        (ast-ref->ast <> :semi :after)))
                 (t ast)))
         (apply-instrumentation (ast return-type before after)
           "Insert instrumentation around AST."
           (let* ((*matching-free-var-retains-name-bias* 1.0)
                  (*matching-free-function-retains-name-bias* 1.0)
                  (wrap (not (traceable-stmt-p obj ast)))
                  ;; Look up AST again in case its children have been
                  ;; instrumented
                  (new-ast (make-ast-ref :path (ast-ref-path ast)
                                         :ast (get-ast obj
                                                       (ast-ref-path ast))))
                  (stmts (append (mapcar {ast-ref->ast _ :semi :after} before)
                                 (if (and instrument-exit
                                          (eq (ast-class ast) :ReturnStmt))
                                     (->> (instrument-return instrumenter
                                                             new-ast
                                                             (null return-type))
                                          (mapcar {ast-ref->ast _ :semi :both}))
                                     (->> (ast-ref->ast new-ast)
                                          (list)))
                                 (mapcar {ast-ref->ast _ :semi :both} after))))
             ;; Wrap in compound statement if needed
             (if wrap
                 (apply-mutation-ops
                  obj
                  `((:set (:stmt1 . ,ast)
                          (:value1 . ,(make-statement
                                        :CompoundStmt
                                        :FullStmt
                                        `("{" ,@(interleave stmts ";") ";}")
                                        :full-stmt t
                                        :aux-data '((:instrumentation t)))))))
                 (apply-mutation-ops
                  obj
                  `((:splice (:stmt1 . ,ast)
                             (:value1 . ,stmts))))))))
      (-<>> (asts obj)
            (remove-if-not {can-be-made-traceable-p obj})
            (funcall filter)
            (sort <> #'ast-later-p)
            ;; Generate all instrumentation before applying changes
            (mapcar (lambda (ast)
                      (prog1
                          (instrument-ast ast
                                          (mappend {funcall _ instrumenter ast}
                                                   functions)
                                          (mappend {funcall _ instrumenter ast}
                                                   functions-after)
                                          (when points
                                            (aget ast points :test #'equalp)))
                        (when points
                          (setf (aget ast points :test #'equalp) nil)))))
            (mapc {apply #'apply-instrumentation})))

    ;; Warn about any leftover un-inserted points.
    (mapc (lambda (point)
            (warn "No insertion point found for point ~a." point))
          (remove-if-not #'cdr points))

    ;; Add support code for tracing to obj
    (initialize-tracing obj trace-file trace-env entry instrumenter)
    (when entry
      (prepend-to-genome obj +write-trace-impl+))
    (prepend-to-genome obj +write-trace-include+)

    obj))

(defmethod uninstrument ((clang clang))
  "Remove instrumentation from CLANG"
  (labels ((uninstrument-genome-prologue (clang)
             (with-slots (ast-root) clang
               (setf ast-root
                 (destructuring-bind (first second . rest) (ast-root clang)
                   (list* first
                          (-> second
                              (replace-all
                                +write-trace-impl+
                                "")
                              (replace-all
                                +write-trace-include+
                                "")
                              (replace-all
                                (format nil
                                        +write-trace-file-declaration+
                                        *instrument-log-variable-name*)
                                "")
                              (replace-all
                                (format nil
                                        +write-trace-file-definition+
                                        *instrument-log-variable-name*)
                                ""))
                          rest)))))
           (uninstrument-genome-epilogue (clang)
             (with-slots (ast-root) clang
               (setf ast-root
                 (if (stringp (lastcar (ast-root clang)))
                     (append (butlast (ast-root clang))
                             (-> (subseq (lastcar (ast-root clang))
                                         0
                                         (-<>> (split-sequence
                                                 #\Newline
                                                 +write-trace-initialization+)
                                               (take 2)
                                               (format nil "~{~a~%~}")
                                               (search <>
                                                       (lastcar
                                                         (ast-root clang)))))
                                 (list)))
                     (ast-root clang))))))

    ;; Remove instrumentation setup code
    (uninstrument-genome-prologue clang)
    (uninstrument-genome-epilogue clang)

    ;; Remove instrumented ASTs - blocks first, then individual statements
    (let ((*matching-free-var-retains-name-bias* 1.0)
          (*matching-free-function-retains-name-bias* 1.0))
      (iter (for ast in (->> (asts clang)
                             (remove-if-not [{aget :instrumentation}
                                             {ast-aux-data}])
                             (remove-if-not {block-p clang})
                             (reverse)))
            (apply-mutation-ops clang
                                `((:set (:stmt1 . ,ast)
                                        (:value1 .
                                         ,(->> (get-ast clang (ast-ref-path ast))
                                               (make-ast-ref
                                                 :path (ast-ref-path ast)
                                                 :ast)
                                               (get-immediate-children clang)
                                               (remove-if [{aget :instrumentation}
                                                           {ast-aux-data}])
                                               (first)))))))

      (iter (for ast in (->> (asts clang)
                             (remove-if-not [{aget :instrumentation}
                                             {ast-aux-data}])
                             (remove-if {block-p clang})
                             (reverse)))
            (apply-mutation-ops clang `((:splice (:stmt1 . ,ast)
                                                 (:value1 . nil)))))))

  ;; Return the software object
  clang)

(defgeneric instrumentation-files (project)
  (:documentation
   "Return files in PROJECT in the order which they would be instrumented"))

(defmethod instrumentation-files ((clang-project clang-project))
  (append (remove-if {get-entry}
                     (evolve-files clang-project)
                     :key #'cdr)
          (remove-if [#'not {get-entry}]
                     (evolve-files clang-project)
                     :key #'cdr)))

(defmethod instrumented-p ((clang-project clang-project))
  (some #'instrumented-p (mapcar #'cdr (evolve-files clang-project))))

(defmethod instrument ((clang-project clang-project) &rest args)
  "Instrument a project. Arguments are passed through to instrument on
the underlying software objects."
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
              (initialize-tracing obj
                                  (plist-get :trace-file args)
                                  (plist-get :trace-env args)
                                  entry
                                  instrumenter)
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
                                          *instrument-log-variable-name*
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
   "Return C statements for variable instrumentation.
INSTRUMENTER contains instrumentation state. KEY should be a function
used to pull the variable list out of AST."))

(defmethod var-instrument
    (key (instrumenter instrumenter) (ast ast-ref) &key print-strings)
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
  (:documentation "Return the entry AST in SOFTWARE."))

(defmethod get-entry ((obj clang))
  (&>> (find-if [{string= "main"} {ast-name}] (functions obj))
       (function-body obj)))

(defun initialize-tracing (obj file-name env-name contains-entry
                           instrumenter)
  (assert (typep obj 'clang))

  (labels ((file-open-str ()
             ;; Default value for ENV-NAME is `*instrument-log-env-name*'.  This
             ;; allows users to specify tracing from the environment without
             ;; having to export this above value (which we'd prefer not be
             ;; modified as it's assumed elsewhere).
             (cond
               (file-name
                (format nil "fopen(~s, \"w\")" (namestring file-name)))
               (env-name
                (format nil "fopen(getenv(~s) ? getenv(~s) : \"/dev/null\", ~
                             \"w\")"
                        (if (eq t env-name)
                            (namestring *instrument-log-env-name*)
                            (namestring env-name))
                        (if (eq t env-name)
                            (namestring *instrument-log-env-name*)
                            (namestring env-name))))
               (t "stderr"))))

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
          (prepend-to-genome obj
                             (format nil +write-trace-file-definition+
                                     *instrument-log-variable-name*))
          (append-to-genome  obj
                             (format nil +write-trace-initialization+
                                     *instrument-handshake-env-name*
                                     *instrument-log-variable-name*
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
                                     *instrument-log-variable-name*
                                     (hash-table-count (names instrumenter))
                                     (hash-table-count (types instrumenter)))))

        ;; Object does not contain main. Insert extern declaration of
        ;; log variable.
        (prepend-to-genome obj
                           (format nil +write-trace-file-declaration+
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

(defun run-clang-instrument ()
  "Run `clang-instrument' on *COMMAND-LINE-ARGUMENTS*."
  (clang-instrument (cons (argv0) *command-line-arguments*)))

(defun clang-instrument (args)
  "Interface to the command line instrumentation tool."
  (in-package :sel)
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
