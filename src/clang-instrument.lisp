;;; clang-instrument --- Instrument C-language source files
(in-package :se)


;;;; Instrumentation

(defgeneric instrument (obj &key points functions trace-file
                              print-argv instrument-exit entry-obj
                              filter)
  (:documentation
   "Instrument OBJ to print AST counter before each full statement.
option
Keyword arguments are as follows:
  POINTS ----------- alist of additional strings to print at specific points
  FUNCTIONS -------- functions to generate additional instrumentation at each point
  TRACE-FILE ------- file for trace output
  PRINT-ARGV ------- print program arguments on startup
  INSTRUMENT-EXIT -- print counter of function body before exit
  ENTRY-OBJ -------- object containing main function
  FILTER ----------- function to select a subset of ASTs for instrumentation
"))


(defmethod instrument ((obj clang) &key points functions trace-file
                                     print-argv instrument-exit (entry-obj obj)
                                     (filter #'identity))
  (let ((log-var (if trace-file "__bi_mut_log_file" "stderr"))
        ;; Promote every counter key in POINTS to the enclosing full
        ;; statement with a CompoundStmt as a parent.  Otherwise they
        ;; will not appear in the output.
        (points
         (remove nil
           (mapcar (lambda-bind ((counter . value))
                    (let ((parent (enclosing-traceable-stmt obj counter)))
                      (if parent (cons (aget :counter parent) value)
                          (warn "Point ~d doesn't match traceable AST."
                                counter))))
                   points))))
    (labels
        ((escape (string)
           (regex-replace (quote-meta-chars "%") (format nil "~S" string) "%%"))
         (outer-parent (stmt function)
           ;; Outermost ancestor of stmt, not including the function body.
           (let ((parent (aget :parent-counter (get-ast obj stmt))))
             (if (eq parent (aget :body function))
                 stmt
                 (outer-parent parent function))))
         (last-traceable-stmt (proto)
           (enclosing-traceable-stmt obj
                                     (second (aget :stmt-range proto))))
         (first-traceable-stmt (proto)
           (first (get-immediate-children obj (aget :body proto))))
         (instrument-ast (ast counter formats-w-args trace-strings)
           ;; Given an AST and list of TRACE-STRINGS, return
           ;; instrumented source.
           (let* ((function (function-containing-ast obj counter))
                  (wrap (and (not (aget :full-stmt ast))
                             (can-be-made-full-p obj ast)))
                  (return-void (aget :void-ret function))
                  (skip (or (aget :in-macro-expansion ast)
                            (string= "NullStmt" (aget :ast-class ast)))))
             ;; Insertions in bottom-up order
             (append
              ;; Function exit instrumentation
              (when (and instrument-exit
                         (eq ast (last-traceable-stmt function)))
                `((:insert-value-after
                   (:stmt1 . ,(outer-parent counter function))
                   (:value1 .
                        ,(format nil
                                 "inst_exit:~%fputs(\"((:C . ~d)) \", ~a);~%~a"
                                 (aget :body function) log-var
                                 (if return-void "" "return _inst_ret;"))))))
              ;; Closing brace after the statement
              (when (and wrap (not skip))
                `((:insert-value-after (:stmt1 . ,counter)
                                       (:value1 . "}"))))
              ;; Temp variable for return value
              (when (and instrument-exit
                         (eq ast (first-traceable-stmt function))
                         (not return-void))
                `((:insert-value
                   (:stmt1 . ,counter)
                   (:value1 .
                      ,(let ((type (find-type obj (aget :ret function))))
                            (format nil "~a~@[*~] _inst_ret"
                                    (aget :type type)
                                    (aget :pointer type)))))))
              ;; Transform return statement to temp assignment/goto
              (when (and instrument-exit
                         (string= (aget :ast-class ast) "ReturnStmt"))
                (let ((ret (unless return-void
                             (peel-bananas
                              (aget :src-text
                                   (first (get-immediate-children obj ast)))))))
                  `((:set
                     (:stmt1 . ,counter)
                     (:value1 .
                              ,(format nil "~@[_inst_ret = ~a;~] goto inst_exit;"
                                       ret))))))
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
                                  counter log-var)
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
            (remove-if-not {can-be-made-full-p obj})
            (funcall filter)
            (mapcar {aget :counter})
            ;; Bottom up ensure earlier insertions don't invalidate
            ;; later counters.
            (sort <> #'>)
            (mapcar (lambda (counter)
                      (prog1
                          (let ((ast (get-ast obj counter)))
                            (instrument-ast
                             ast
                             counter
                             (mapcar {funcall _ ast} functions)
                             (aget counter points)))
                        (setf (aget counter points) nil))))
            ;; Each AST generates a list of operations. Flatten them
            ;; to a single level.
            (apply #'append)
            (apply-mutation-ops obj)))

    ;; Warn about any leftover un-inserted points.
    (mapc (lambda (point)
            (warn "No insertion point found for pointer ~a." point))
          (remove-if-not #'cdr points))
    (when print-argv
      (print-program-input entry-obj log-var))
    (when trace-file
      (log-to-filename obj entry-obj log-var trace-file))
    (clang-format obj)
    (unless (eq obj entry-obj)
      (clang-format entry-obj))
    obj))

(defgeneric var-instrument (software label key ast &key print-strings)
  (:documentation
   "Return a format string and variable list for variable instrumentation.
KEY should be a function used to pull the variable list out of AST in
SOFTWARE.  The result of KEY will appear behind LABEL in the trace
output."))

(defmethod var-instrument
    ((obj clang) label key (ast list) &key print-strings)
  (flet ((fmt-code (c-type)
           (switch (c-type :test #'string=)
             ("char"            "%c")
             ("*char"           (if print-strings "\"%s\"" "%p"))
             ("unsigned char"   "%u")
             ("short"           "%hi")
             ("unsigned short"  "%hu")
             ("int"             "%i")
             ("unsigned int"    "%u")
             ("long"            "%li")
             ("unsigned long"   "%lu")
             ("float"           "%f")
             ("double"          "%G")
             ("long double"     "%LG")
             ("size_t"          "%zu")
             (t (if (starts-with "*" c-type :test #'string=)
                    "%p"
                    (error "Unrecognized C type ~S" c-type))))))
    (iter (for var in (funcall key ast))
          (let* ((type (type-of-var obj var))
                 (c-type (concatenate 'string
                           (if (or (aget :pointer type)
                                   (not (emptyp (aget :array type))))
                               "*" "")
                           (aget :type type)))
                 (stripped-c-type (regex-replace "\\**(unsigned )?" c-type "")))
            (when (or (member stripped-c-type +c-numeric-types+ :test #'string=)
                      (string= stripped-c-type "size_t"))
              (concatenating (format nil " (\\\"~a\\\" \\\"~a\\\" ~a)"
                                     var c-type (fmt-code c-type))
                             into format
                             initial-value (format nil "(~s" label))
              (collect var into vars)))
          (finally (return (cons (concatenate 'string format ")") vars))))))

(defgeneric get-entry (software)
  (:documentation "Return the counter of the entry AST in SOFTWARE."))

(defmethod get-entry ((obj clang))
  (first (aget :stmt-range
               (find-if [{string= "main"} {aget :name}] (functions obj)))))

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

(defmethod print-program-input ((obj clang) log-variable)
  ;; Return a version of OBJ instrumented to print program input.
  (or (flet ((deeper-string-search (i ls)
               (member i ls :test #'string=)))
        (when-let* ((entry (get-entry obj))
                    (entry-ast (get-ast obj entry))
                    (scope-vars (aget :scopes entry-ast)))
          (when (and (member "argc" scope-vars :test #'deeper-string-search)
                     (member "argv" scope-vars :test #'deeper-string-search))
            (insert-at-entry obj
                             (format nil
                                     "fprintf(~a, \"((:INPUT \");
int __bi_mut_i_var;
for(__bi_mut_i_var = 0; __bi_mut_i_var < argc; ++__bi_mut_i_var) {
  fprintf(~a, \"\\\"%s\\\" \", argv[__bi_mut_i_var]);
}
fputs(\"))\\n\", ~a);"
                                     log-variable log-variable log-variable)))
          obj))
      (prog1 obj (warn "Unable to instrument program to print input."))))

(defmethod log-to-filename ((obj clang) entry-obj log-variable filename)
  (setf entry-obj
        (insert-at-entry entry-obj (format nil "~a = fopen(~s, \"a\");~%"
                                           log-variable (namestring filename))))
  (setf (genome obj)
        (concatenate 'string
                     (format nil "#include <stdio.h>~%FILE *~a;~%" log-variable)
                     (genome obj)))
  (unless (eq obj entry-obj)
    (setf (genome entry-obj)
          (concatenate 'string
                       (format nil "#include <stdio.h>~%extern FILE *~a;~%"
                               log-variable)
                       (genome entry-obj))))
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
      (sb-ext:exit))

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
                          (split-sequence #\Comma (pop args))
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
            (lambda (ast)
              (var-instrument
               original :unbound-vals
               [{mapcar [#'peel-bananas #'car]} {aget :unbound-vals}] ast
               :print-strings print-strings))))
    (when-let ((position (position 'trace-scope-vars functions)))
      (setf (nth position functions)
            (lambda (ast)
              (var-instrument
               original :scopes
               [{apply #'append} {aget :scopes}] ast
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
