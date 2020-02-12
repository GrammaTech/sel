;;; javascript-instrument.lisp -- Instrument javascript-language source files
(defpackage :software-evolution-library/components/javascript-instrument
  (:nicknames :sel/components/javascript-instrument
              :sel/cp/javascript-instrument)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/parseable
        :software-evolution-library/software/source
        :software-evolution-library/software/project
        :software-evolution-library/software/javascript
        :software-evolution-library/software/javascript-project
        :software-evolution-library/components/instrument)
  (:export :javascript-instrumenter))
(in-package :software-evolution-library/components/javascript-instrument)
(in-readtable :curry-compose-reader-macros)


;;;; Instrumentation
(define-constant +javascript-trace-code+
  "if (typeof __sel_trace_file === 'undefined') {
    const __fs = require('fs');
    __sel_trace_file = __fs.createWriteStream(process.env.__SEL_TRACE_FILE ||
                                              \"/dev/null\")

    __sel_trace_point = function(file_counter, child_counter, ...variables) {
        __sel_trace_file.cork();
        __sel_trace_file.write(\"(\");

        __sel_trace_file.write(\"(:C . \" + child_counter + \")\");
        if (file_counter !== null) {
            __sel_trace_file.write(\"(:F . \" + file_counter + \")\");
        }

        if (variables.length > 0) {
            __sel_trace_file.write(\"(:SCOPES \");
            for (let i = 0; i < variables.length; i++) {
                let variable = variables[i];
                let variable_name = variable.name;
                let variable_type = (typeof variable.value);
                let variable_value = variable.value;

                if (variable_type  === \"boolean\" ||
                    variable_type  === \"undefined\" ||
                    variable_type  === \"number\" ||
                    variable_type  === \"string\" ||
                    variable_type  === \"symbol\" ||
                    variable_value === null) {
                    __sel_trace_file.write(\"#(\");
                    __sel_trace_file.write(\"\\\"\" + variable_name + \"\\\"\");
                    __sel_trace_file.write(\" \");
                    __sel_trace_file.write(\"\\\"\" + variable_type + \"\\\"\");
                    __sel_trace_file.write(\" \");
                    if (variable_value === null) {
                        __sel_trace_file.write(\"nil\");
                    }
                    else if (variable_type === \"string\") {
                        __sel_trace_file.write(
                            \"\\\"\" +
                            encodeURI(variable_value) +
                            \"\\\"\");
                    }
                    else {
                        __sel_trace_file.write(
                            encodeURI(String(variable_value)));
                    }
                    __sel_trace_file.write(\" \");
                    __sel_trace_file.write(\"nil\");
                    __sel_trace_file.write(\")\");
                }
            }
            __sel_trace_file.write(\")\");
        }

        __sel_trace_file.write(\")\\\n\");
        __sel_trace_file.uncork();
    }
}
"
  :test #'string=
  :documentation "JavaScript code for writing trace information.")

(defclass javascript-instrumenter (instrumenter)
  ((ast-ids :accessor ast-ids
            :initarg :ast-ids
            :initform (make-hash-table :test #'eq)
            :documentation "Mapping of ASTs to trace ids.")
   (file-id :reader file-id
            :initarg :file-id
            :initform nil))
  (:documentation "Handles instrumentation for JAVA software objects."))

(defmethod initialize-instance :after
    ((instrumenter javascript-instrumenter) &key)
  (iter (for ast in (asts (software instrumenter)))
        (for ast-i upfrom 0)
        (setf (gethash ast (ast-ids instrumenter)) ast-i)))

(defmethod instrumented-p ((obj javascript))
  "Return true if OBJ is instrumented.
* OBJ a javascript software object
"
  (search *instrument-log-env-name* (genome obj)))

(defmethod instrument ((obj javascript) &rest args)
  "Instrumentation for javascript software objects.
Creates a JAVASCRIPT-INSTRUMENTER for OBJ and calls its instrument method.

* OBJ the software object to instrument
* ARGS additional arguments are passed through to the instrumenter method.
"
  (apply #'instrument (make-instance 'javascript-instrumenter :software obj)
         args))

(defmethod instrument
  ((instrumenter javascript-instrumenter)
   &key points functions functions-after trace-file trace-env instrument-exit
     (filter (constantly t)) (num-threads 1)
  &aux (obj (software instrumenter)))
  "Use INSTRUMENTER to instrument a javascript software object.

* INSTRUMENTER current instrumentation state
* POINTS alist of additional values to print at specific points (ignored)
* FUNCTIONS  functions to calculate instrumentation at each point
* FUNCTIONS-AFTER functions to calculate instrumentation after each point
* TRACE-FILE file or stream (stdout/stderr) for trace output (ignored)
* TRACE-ENV trace output to file specified by ENV variable (ignored)
* INSTRUMENT-EXIT print counter of function body before exit (ignored)
* FILTER function to select a subset of ASTs for instrumentation
         function should take a software object and an AST parameters,
         returning nil if the AST should be filtered from instrumentation
* NUM-THREADS number of threads to use for instrumentation"
  (declare (ignorable points trace-file trace-env instrument-exit num-threads))

  (labels ((get-ast-id (ast)
             (gethash ast (ast-ids instrumenter)))
           (instrument-before (instrumenter ast)
             (to-ast 'javascript-ast
                     `(:ExpressionStatement
                        :aux-data ((:instrumentation . t))
                        (:CallExpression
                          "__sel_trace_point("
                          (:Literal
                            ,(if (file-id instrumenter)
                                 (format nil "~d" (file-id instrumenter))
                                 "null"))
                          ", "
                          (:Literal
                            ,(format nil "~d" (get-ast-id ast)))
                          ", "
                          ,@(interleave
                              (mappend {funcall _ instrumenter ast}
                                       functions)
                              ",")
                          ");"))))
           (instrument-after (instrumenter ast)
             (when functions-after
               (to-ast 'javascript-ast
                       `(:ExpressionStatement
                          :aux-data ((:instrumentation . t))
                          (:CallExpression
                            "__sel_trace_point("
                            (:Literal
                              ,(if (file-id instrumenter)
                                   (format nil "~d" (file-id instrumenter))
                                   "null"))
                            ", "
                            (:Literal
                              ,(format nil "~d" (get-ast-id ast)))
                            ", "
                            ,@(interleave
                                (mappend {funcall _ instrumenter ast}
                                         functions-after)
                                ",")
                            ");")))))
           (instrument-ast (instrumenter ast)
             (list (instrument-before instrumenter ast)
                   ast
                   (instrument-after instrumenter ast)))
           (instrument-asts (instrumenter)
             (let ((obj (software instrumenter)))
               (-<>> (asts obj)
                     (remove-if-not {traceable-stmt-p obj})
                     (remove-if-not {funcall filter obj})
                     (sort <> #'ast-later-p)
                     (mapcar {instrument-ast instrumenter}))))
           (create-value (obj before ast after)
             (remove-if #'null
                        (append (list before)
                                (list (get-ast obj (ast-path ast)))
                                (list after)))))
    (apply-mutation-ops
      obj
      (iter (for (before ast after) in (instrument-asts instrumenter))
            (collect `(:splice (:stmt1 . ,ast)
                               (:value1 . ,{create-value obj
                                                         before
                                                         ast
                                                         after}))))))

  (append-to-genome-preamble obj +javascript-trace-code+)

  obj)

(defmethod var-instrument
  (key (instrumenter instrumenter) (ast javascript-ast) &key print-strings)
  "Generate ASTs for variable instrumentation.
* KEY a function used to pull the variable list out of AST
* INSTRUMENTER current instrumentation state
* AST the AST to instrument
"
  (declare (ignorable print-strings))
  (iter (for var in (funcall key ast))
        (collect (to-ast 'javascript-ast
                         `(:ObjectExpression
                            "{"
                            (:Property
                              (:Identifier "name")
                              ": "
                              (:Literal ,(format nil "\"~a\"" (aget :name var))))
                            ", "
                            (:Property
                              (:Identifier "value")
                              ": "
                              (:Identifer ,(aget :name var)))
                            "}")))))

(defmethod instrument ((javascript-project javascript-project) &rest args)
  "Add instrumentation to JAVASCRIPT-PROJECT.
* JAVASCRIPT-PROJECT the project to instrument
* ARGS passed through to the instrument method on underlying software objects.
"
  (task-map (or (plist-get :num-threads args) 1)
            (lambda (instrumenter)
              (apply #'instrument instrumenter args))
            (iter (for (file . obj) in
                       (instrumentation-files javascript-project))
                  (for file-id upfrom 0)
                  (declare (ignorable file))
                  (collect (make-instance 'javascript-instrumenter
                             :software obj
                             :file-id file-id))))
  javascript-project)

(defmethod uninstrument ((obj javascript) &key (num-threads 1))
  "Remove instrumentation from OBJ.
* OBJ javascript software object to uninstrument
"
  (declare (ignorable num-threads))
  (with-slots (ast-root) obj
    (setf ast-root
          (copy ast-root
                :children (append (-> (ast-children ast-root)
                                      (first)
                                      (replace-all +javascript-trace-code+ "")
                                      (list))
                                  (cdr (ast-children ast-root))))))
  (apply-mutation-ops
    obj
    (iter (for ast in (->> (asts obj)
                           (remove-if-not [{aget :instrumentation}
                                           {ast-aux-data}])
                           (reverse)))
          (collect `(:cut (:stmt1 . ,ast)))))
  obj)
