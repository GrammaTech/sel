;;; clang-instrument --- Instrument C-language source files
(in-package :se)


;;;; Instrumentation
(defmethod instrument ((obj clang) &key points functions trace-file)
  (let ((log-var (if trace-file "__bi_mut_log_file" "stderr"))
        ;; Promote every counter key in POINTS to the enclosing full
        ;; statement with a CompoundStmt as a parent.  Otherwise they
        ;; will not appear in the output.
        (points
         (remove nil
           (let ((inc 0))
             (mapcar (lambda-bind ((counter . value))
                       (multiple-value-bind (parent madep)
                           (get-make-parent-full-stmt obj (+ inc counter))
                         (when madep (incf inc))
                         (if parent (cons (aget :counter parent) value)
                             (warn "Point ~d doesn't match traceable AST."
                                   counter))))
                     points)))))
    (flet ((instrument-ast (ast orig-counter formats-w-args trace-strings)
             ;; Given an AST and list of TRACE-STRINGS, return
             ;; instrumented source.
             (format nil "~{~a~}fputs(\")\\n\", ~a);~%~a~%"
                     (append
                      (cons
                       (format nil      ; Start up alist w/counter.
                               "fputs(\"((:C . ~d) \", ~a);~%"
                               orig-counter log-var)
                       (when trace-strings
                         (list
                          (format nil   ; Points instrumentation.
                                  "fputs(\"(~{~a~^ ~})\", ~a);~%"
                                  trace-strings log-var))))
                      (mapcar
                       {format nil      ; Functions instrumentation.
                               (format nil "fprintf(~a,\"~~a\"~~{,~~a~~});~%"
                                       log-var)}
                       (mapcar #'car formats-w-args)
                       (mapcar #'cdr formats-w-args)))
                     log-var
                     ;; Text of instrumented statement.
                     (peel-bananas (aget :src-text ast)))))
      (-<>> (asts obj)
            (remove-if-not {can-be-made-full-p obj})
            (mapcar {aget :counter})
            ;; Bottom up ensure earlier insertions don't invalidate
            ;; later counters.
            (sort <> #'>)
            ;; Setup to hold (updated-value . original-value) to
            ;; accommodate inserted CompoundStmts.
            (mapcar (lambda (counter) (cons counter counter)))
            ;; Collect/create parents, when needed update counters.
            (reduce (lambda-bind (acc (counter . orig-counter))
                      (multiple-value-bind (parent madep)
                          (get-make-parent-full-stmt obj counter)
                        ;; As we add CompoundStmts, we increment
                        ;; all subsequent pointers...
                        (when madep
                          (setf points
                                (mapcar (lambda-bind ((p . rest))
                                          (cons (if (>= p counter)
                                                    (1+ p) p)
                                                rest))
                                        points)))
                        (cons (cons (aget :counter parent) orig-counter)
                              ;; and counters.
                              (if madep
                                  (mapcar (lambda-bind ((counter . orig-counter))
                                            (cons (1+ counter) orig-counter))
                                          acc)
                                  acc))))
                    <> :initial-value '())
            (nreverse) ; The prior reduce reverses the order, this fixes that.
            (mapc      ; Actually insert the instrumentation.
             (lambda-bind ((counter . orig-counter))
               (setf
                (genome obj)
                (clang-mutate obj
                  `(:set (:stmt1 . ,counter)
                         ,(cons :value1
                                (let ((ast (get-ast obj counter)))
                                  (instrument-ast
                                   ast
                                   orig-counter
                                   (mapcar {funcall _ ast} functions)
                                   (aget counter points)))))))
               (setf (aget counter points) nil)
               (update-asts obj)
               obj))))
    ;; Warn about any leftover un-inserted points.
    (mapc (lambda (point)
            (warn "No insertion point found for pointer ~a." point))
          (remove-if-not #'cdr points))
    (when trace-file
      (setf obj (log-to-filename obj log-var trace-file)))
    (clang-format obj)
    obj))

(defgeneric unbound-var-instrument (software ast &key print-strings)
  (:documentation
   "Return a format string and variable list for variable instrumentation."))

(defmethod unbound-var-instrument ((obj clang) (ast list) &key print-strings)
  (flet ((fmt-code (c-type)
           (eswitch (c-type :test #'string=)
             ("char"          "%c")
             ("*char"         (if print-strings "\"%s\"" "%p"))
             ("short"         "%hi")
             ("*short"        "%p")
             ("int"           "%i")
             ("*int"          "%p")
             ("long"          "%li")
             ("*long"         "%p")
             ("float"         "%f")
             ("*float"        "%p")
             ("double"        "%G")
             ("*double"       "%p")
             ("long double"   "%LG")
             ("*long double"  "%p")))) 
    (iter (for var in (mapcar [#'peel-bananas #'car]
                              (aget :unbound-vals ast)))
          (let* ((type (type-of-var obj var))
                 (c-type (concatenate 'string
                           (if (or (aget :pointer type)
                                   (not (emptyp (aget :array type))))
                               "*" "")
                           (aget :type (type-of-var obj var)))))
            (when (member c-type +c-numeric-types+ :test #'string=)
              (concatenating (format nil " (:~a ~a ~a)"
                                     var c-type (fmt-code c-type))
                             into format initial-value "(:V")
              (collect var into vars)))
          (finally (return (cons (concatenate 'string format ")") vars))))))

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
                                     log-variable (namestring filename))))
  (setf (genome obj)
        (concatenate 'string
          (format nil "#include <stdio.h>~%FILE *~a;~%" log-variable)
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
        path out-dir name type trace-file out-file save-original
        points functions print-strings)
    (when (or (not args)
              (< (length args) 1)
              (string= (subseq (car args) 0 2) "-h")
              (string= (subseq (car args) 0 3) "--h"))
      (format t "Usage: ~a SOURCE [OPTIONS]
 Instrument SOURCE along OPTIONS.

Options:
 -c,--compiler CC ------- use CC as the C compiler
                          (default to CC env. variable or clang)
 -F,--flags FLAGS ------- comma-separated list of compiler flags
                          (default to CFLAGS env. variable)
 -o,--out-file FILE ----- write mutated source to FILE
                          (default STDOUT)
 -O,--orig -------------- also save a copy of the original
 -p,--point NUM,STRING -- instrument to print STRING at ast NUM
 -q,--quiet ------------- set verbosity level to 0
 -s,--strings ----------- trace string variable values, DANGEROUS
 -t,--trace-file FILE --- instrumented to write trace to fILE
                          (default to STDERR)
 -v,--variables --------- write numeric variables to trace
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
      ("-c" "--compiler" (setf (compiler original) (pop args)))
      ("-F" "--flags" (setf (flags original) (pop args)))
      ("-o" "--out-file" (setf out-file (pop args)))
      ("-O" "--orig" (setf save-original t))
      ("-p" "--point" (destructuring-bind (counter string)
                          (split-sequence #\Comma (pop args))
                        (pushnew string (aget (parse-integer counter) points)
                                 :test #'string=)))
      ("-q" "--quiet" (setf *note-level* 0))
      ("-s" "--strings" (setf print-strings t))
      ("-t" "--trace-file" (setf trace-file (pop args)))
      ("-v" "--variables" (push 'trace-vars functions))
      ("-V" "--verbose"   (let ((lvl (parse-integer (pop args))))
                            (when (>= lvl 4) (setf *shell-debug* t))
                            (setf *note-level* lvl))))

    ;; Set the functions.
    (setf (nth (position 'trace-vars functions) functions)
          (lambda (ast) (unbound-var-instrument original ast print-strings)))

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
                          :functions functions)))))
      (note 1 "Writing instrumented to ~a." dest)
      (with-open-file
          (out dest :direction :output :if-exists :supersede)
        (genome-string instrumented out)))))
