;;; condition-synthesis.lisp --- synthesize condition expressions

#|
*************************************************************************************
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* UNLIMITED RIGHTS
*
* The Government's rights to use, modify, reproduce, release, perform, display, or
* disclose this software are governed by DFARS 252.227-7013, RIGHTS IN TECHNICAL DATA
* --NONCOMMERCIAL ITEMS, and DFARS 252.227-7014 RIGHTS IN NONCOMMERCIAL SOFTWARE AND
* NONCOMMERCIAL COMPUTER SOFTWARE DOCUMENTATION.
*
*************************************************************************************
*
* All GrammaTech IP (sole or co-developed) needs to include the GrammaTech copyright.
*
* (c) 2016 GrammaTech, Inc.  All rights reserved.
*
* Such IP is also subject to the terms of the Prioprietary Information Agreement (PIA)
* executed between BAE Systems Information and Electronics Systems Integration Inc.
* and GrammaTech, Inc. dated April 21, 2015
*
*************************************************************************************
|#

#|

To use condition synthesis, first create an instance of one of the
synthesis mutations defined below, and choose a target location to
apply it.

Then call synthesize-condition, passing a software object, the
mutation, and a test suite. This will run the synthesis algorithm,
returning a mutated program, or NIL if no condition can be found which
improves synthesis.

Synthesis uses comparisons between integer (or std::string) variables
and constants. Other types can be used by providing a comparison
function and calling instrumentation-exprs to generate extra
instrumentation.

|#

(in-package :software-evolution)


;;; Synthesis mutations

;; Helper functions
(defun guard-statements (software)
  (remove-if-not {aget :guard-stmt} (bad-stmts software)))

(defun pick-target-condition (software)
  "Pick a condition to target for refinement."
  (aget :counter (random-elt (guard-statements software))))

(defun pick-if-stmt (software)
  (let ((stmts (remove-if-not [{equal "IfStmt"} {aget :ast-class } ]
                              (bad-stmts software))))
    (and stmts (list (aget :counter (random-elt stmts))))))

;; refine-condition: add an additional boolean clause to an if condition
(define-mutation refine-condition (clang-mutation)
  ((targeter :initform (lambda (software)
                         (pick-target-condition software)))
   (connector :reader connector)
   (abst-cond :accessor abst-cond :initform nil)))

(define-mutation tighten-condition (refine-condition)
  ((targeter :initform (lambda (software)
                         (pick-target-condition software)))
   (connector :reader connector :initform "&& !")))

(define-mutation loosen-condition (refine-condition)
  ((targeter :initform (lambda (software)
                         (pick-target-condition software)))
   (connector :reader connector :initform "|| ")))

(defgeneric valid-targets (mutation software)
  (:documentation "List of locations where this mutation can be applied."))

(defmethod valid-targets ((mutation refine-condition) software)
  (remove-if {aget :in-macro-expansion} (guard-statements software)))

(defun refined-condition (mutation target-ast)
  (format nil "(~a) ~a~a"
          (peel-bananas (aget :src-text target-ast))
          (connector mutation)
          (or (abst-cond mutation) "abst_cond()")))

(defmethod build-op ((mutation refine-condition) software)
  (bind ((target (targets mutation))
         (target-ast (get-ast software target))
         (abst-cond-expr (or (abst-cond mutation) "abst_cond()")))
    `((:set (:stmt1 . ,target)
            (:value1 .
                     ((:src-text . ,(refined-condition mutation
                                                       target-ast))
                      (:includes . "\"abst_cond.h\"")))))))

;; add-condition: wrap a statement in an if
(define-mutation add-condition (clang-mutation)
  ((targeter
    :initform
    (lambda (software)
      (aget :counter
            (random-elt
             (->> (asts software)
                  (remove-if-not {aget :full-stmt})
                  (remove-if [{equal "Function"} {aget :ast-class}])
                  (remove-if [{equal "DeclStmt"} {aget :ast-class}]))))))
   (abst-cond :accessor abst-cond :initform nil)))

(defmethod build-op ((mutation add-condition) software)
  (let* ((target (targets mutation))
         (software (wrap-ast software (get-ast software target)))
         (target-ast (get-ast software target))
         (abst-cond-expr (or (abst-cond mutation) "abst_cond()")))
    `((:set (:stmt1 . ,target)
            (:value1 .
                     ((:src-text . ,(format nil "if (~a) ~a"
                                            abst-cond-expr
                                            (peel-bananas
                                             (aget :src-text target-ast))))
                      (:includes . "\"abst_cond.h\"")))))))

;; if-to-while
;; replace an if statement with a while using the same condition
(define-mutation if-to-while (clang-mutation)
  ((targeter :initform #'pick-if-stmt)))

;; if-to-while-tighten-condition
;; Combine if-to-while and synthesize a refined condition
(define-mutation if-to-while-tighten-condition (if-to-while tighten-condition)
  ())

(defmethod build-op ((mutation if-to-while-tighten-condition) software)
  (when (targets mutation)
    (let* ((stmt (targets mutation))
           (ast (get-ast software stmt))
           (condition-ast (get-ast software (first (aget :children ast))))
           (text (replace-one
                  (peel-bananas
                   (replace-one (aget :src-text ast)
                                (aget :src-text condition-ast)
                                (refined-condition mutation
                                                   condition-ast)))
                  "if" "while")))
      `((:set (:stmt1 . ,stmt)
              (:value1 . ((:src-text . ,text)
                          (:includes . "\"abst_cond.h\""))))))))

;; insert-else-if
;; insert an else-if clause for an existing if, insert abstract conditional
;; return the 2nd child (then branch) for an If
(define-mutation insert-else-if (tighten-condition)
  ((targeter :initform #'pick-if-stmt)))

(defun replace-one (string part replacement)
  "Returns a new string in which the first occurence of the part
is replaced with replacement."
  (let ((pos (search part string)))
    (if pos
        (with-output-to-string (out)
          (write-string string out :start 0 :end pos)
          (write-string replacement out)
          (write-string string out :start (+ pos (length part))))
        string)))

(defmethod build-op ((mutation if-to-while) software)
  (when (targets mutation)
    (let* ((stmt (first (targets mutation)))
           (ast (get-ast software stmt)))
      `((:set (:stmt1 . ,stmt)
              (:value1 . ,(acons :src-text
                                 (replace-one (aget :src-text ast)
                                              "if" "while")
                                 ast)))))))

;; Get second child (then branch), put new else-if text at the end of
;; the existing text (with abs_cond as conditional), it SHOULD parse
;; out correctly.
(defmethod build-op ((mutation insert-else-if) software)
  (when (targets mutation)
    (let* ((if-stmt (targets mutation))
           ;; here we take the "then" branch of the target IfStmt
           (stmt (second (aget :children (get-ast software if-stmt))))
           (target-ast (get-ast software stmt))
           (abst-cond-expr (or (abst-cond mutation) "abst_cond()")))
      `((:set
         (:stmt1 . ,stmt)
         (:literal1 . ,(format nil "~a else if (~a) { /* empty for now */ }"
                               (peel-bananas (aget :src-text target-ast))
                               abst-cond-expr))
         (:includes . "\"abst_cond.h\""))))))

(defmethod valid-targets ((mutation if-to-while) software)
  (remove-if {aget :in-macro-expansion}
             (remove-if-not [{equal "IfStmt"} {aget :ast-class}]
                            (bad-stmts software))))

(defmethod valid-targets ((mutation insert-else-if) software)
  (remove-if {aget :in-macro-expansion}
             (remove-if-not [{equal "IfStmt"} {aget :ast-class}]
                            (bad-stmts software))))


;;; Condition synthesis implementation.
(defvar *trace-file* nil)
(defvar *max-trace-length* 1000)

(define-constant +abst-cond-source+
    "
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

/* Return either the next value in _abst_cond_results if there are values
   remaining, or the default abst_cond value.*/

int abst_cond_initialized = 0;
int abst_cond() {
    static const char *return_vals = NULL;
    static int default_return = 0;
    static int loop_count = -1;
    static int loop_phase = 0;
    if (!abst_cond_initialized) {
        abst_cond_initialized = 1;
        const char *default_env = getenv(\"ABST_COND_DEFAULT\");
        if (default_env)
            default_return = atoi(default_env);
        return_vals = getenv(\"ABST_COND_VALUES\");
        const char *loop_env = getenv(\"ABST_COND_LOOP_COUNT\");
        if (loop_env)
          loop_count = atoi(loop_env);
    }

    int result = default_return;

    if (loop_count != -1) {
        loop_phase++;
        if (loop_phase > loop_count) {
            loop_phase = 0;
            result = 1;
        }
    }
    else if(return_vals && *return_vals) {
        result = *return_vals - '0';
        return_vals++;
    }
    fprintf(__bi_mut_log_file, \"((:ABST-COND . \\\"%d\\\"))\\n\", result);
    return result;
}
"
  :test #'string=
  :documentation "C code for abst_cond() function.")

(defparameter *abst-cond-attempts* 10
  "Maximum number of combinations for abst_cond() values to attempt before
discarding a repair.")

(defparameter *synth-condition-attempts* 20
  "Maximum number of synthesized conditions to test before discarding a
repair.")

(defgeneric contains-abstract-condition (mutation)
  (:documentation "Returns t or nil indicating whether the mutation has an
abstract condition."))

(defmethod contains-abstract-condition ((mut mutation))
  (declare (ignorable mut))
  nil)

(defmethod contains-abstract-condition ((mut refine-condition))
  (declare (ignorable mut))
  t)

(defmethod contains-abstract-condition ((mut add-condition))
  (declare (ignorable mut))
  t)

(defgeneric contains-loop-condition (mutation)
  (:documentation "Does the mutation contain an abstract loop condition?"))

(defmethod contains-loop-condition (mutation)
  (declare (ignorable mutation))
  nil)
(defmethod contains-loop-condition ((mutation if-to-while))
  (declare (ignorable mutation))
  t)

(defun get-abst-cond-locs (software)
  "Identify guard statements containing a call to abst_cond(), and return a list
of lists containing the AST counter along with start and end index of
abst_cond() in the source text."
  (let ((asts (remove-if-not {aget :guard-stmt} (asts software))))
    (iter (for ast in asts)
          (multiple-value-bind (match-start match-end reg-start reg-end)
              (scan "abst_cond\\(\\)" (peel-bananas (aget :src-text ast)))
            (declare (ignorable reg-start reg-end))
            (when match-start
              (collect (aget :counter ast) into counters)
              (collect match-start into starts)
              (collect match-end into ends))
            (finally (return (mapcar #'list counters starts ends)))))))

(defun get-parent-control-stmt (clang guard-counter)
  "Returns the AST structure of the enclosing if statement for a guard."
  (let ((stmt (car (remove-if-not
                    [{member _ '("IfStmt" "WhileStmt" "ForStmt" "DoStmt"
                                 "SwitchStmt")
                             :test #'string=}
                     {aget :ast-class}]
                    (get-parent-asts clang (get-ast clang guard-counter))))))
    (assert stmt)
    stmt))

;; FIXME: mostly copied from var-instrument in clang-instrument.lisp
(defun instrument-values (obj ast &key print-strings extra-exprs)
  (flet ((fmt-code (c-type)
           (switch (c-type :test #'string=)
             ("char"            "%c")
             ("*char"           (if print-strings "\\\"%s\\\"" "%p"))
             ("string"          (if print-strings "\\\"%s\\\"" "%p"))
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
    (let ((exprs
           (remove nil
             (append (mapcar (lambda (v)
                               (handler-case ; Sometimes fails for weird types.
                                   (cons v (type-of-var obj v))
                                 (error () nil)))
                             (apply #'append (butlast (aget :scopes ast))))
                     extra-exprs))))
      (iter (for (var . type) in exprs)
            (let* ((c-type (concatenate 'string
                             (if (or (aget :pointer type)
                                     (not (emptyp (aget :array type))))
                                 "*" "")
                             (aget :type type)))
                   (stripped-c-type
                    (regex-replace "\\**(unsigned )?" c-type "")))
              (when (or (member stripped-c-type +c-numeric-types+
                                :test #'string=)
                        (string= stripped-c-type "size_t"))
                (concatenating (format nil " (\\\"~a\\\" \\\"~a\\\" ~a)"
                                       var c-type (fmt-code c-type))
                               into format
                               initial-value (format nil "(:scopes"))
                (collect var into vars))
              (when (and print-strings
                         (string= stripped-c-type "string"))
                (concatenating (format nil " (\\\"~a\\\" \\\"~a\\\" ~a)"
                                       var c-type (fmt-code c-type))
                               into format
                               initial-value (format nil "(:scopes"))
                (collect (format nil "~a.c_str()" var) into vars)))
            (finally (return (cons (concatenate 'string format ")") vars)))))))

(defun instrument-abst-cond-traces (software trace-file-name extra-exprs)
  "Add instrumentation before the enclosing if statement for the guard
statement(s) in the repair targets."

  ;; Look for repair-locs only in the main target (e.g. for clang-project)
  (let* ((repair-locs (mapcar [{aget :counter}
                               {get-parent-control-stmt software}
                               #'car]
                              (get-abst-cond-locs software)))
         (inst-functions (mapcar {function-containing-ast software}
                                 repair-locs))
         ;; Instrument the enclosing control statement and all its
         ;; full-statement children. For loops, the latter ensures
         ;; that we record the environment on each iteration. This is
         ;; more instrumentation than we need but it's easier than
         ;; accurately determining the minimal instrumentation, and
         ;; the overhead is not significant.
         (inst-locs (remove-if-not
                     (lambda (ast)
                       (and (full-stmt-p software ast)
                            (some (lambda (loc) (ancestor-of software loc ast))
                                  repair-locs)))
                     (mapcar {aget :counter} (asts software)))))

    ;; sometimes inst-locs comes up nil (no full statement children),
    ;; set to enclosing full stmt, if so
    (unless inst-locs
      (setf inst-locs (mapcar {enclosing-full-stmt software } repair-locs)))

    (let ((inst-reps (lambda (ast)
                       (when (member (aget :counter ast) inst-locs)
                         (instrument-values software ast
                                            :print-strings t
                                            :extra-exprs extra-exprs)))))
      ;; Instrument the original object
      (instrument software :trace-file trace-file-name
                  :functions (list inst-reps)
                  ;; Only instrument within relevant functions
                  :filter {remove-if-not [{member _ inst-functions}
                                          {function-containing-ast software}
                                          {aget :counter}]}))))

(defun read-abst-conds-and-envs (trace-results-file)
  "For a trace file, read in the environments and recorded abst-cond decisions."
  (if (not (probe-file trace-results-file))
      ;; File doesn't exist
      (values nil nil)
      ;; File exists
      (with-open-file (trace-output trace-results-file)
        (iter (for sexpr = (ignore-errors (read trace-output)))
              ;; iter can exhaust the stack on long traces. Bail out
              ;; before that happens.
              (for i below *max-trace-length*)
              (with prev-env = nil)
              (while sexpr)
              (when (aget :scopes sexpr)
                (setf prev-env (aget :scopes sexpr)))
              (when (aget :abst-cond sexpr)
                (collect prev-env into envs)
                (collect (aget :abst-cond sexpr) into abst-conds))
              (finally (return (values (apply {concatenate 'string} abst-conds)
                                       (apply #'append envs))))))))

(defun synthesize-conditions (envs)
  "For each assignment in each environment, generate conditions representing
(x == v) and !(x == v).

envs is a list of environments, each of which contains a list of triples:
(\"var\" \"type\" \"value\"). The result is a list of synthesized conditions,
where each condition is a triple (\"var\" \"value\" [:eq|:neq])."
  (iter (for env in envs)
        (collect
         (iter (for assmt in env)
               (collect (list (first assmt) (third assmt) :eq)
                        into assmts)
               (collect (list (first assmt) (third assmt) :neq)
                        into assmts)
               (finally (return assmts)))
         into assmts)
        (finally (return (remove-duplicates
                          (apply #'append assmts)
                          :test #'equal)))))

(defun entails (env condition result)
  "Check whether the condition's value in the environment is consistent with the
desired result.

The env is a list of triples (\"var\" \"type\" \"value\"), the condition is a
triple (\"var\" \"value\" [:eq|:neq]), and result is the string \"0\" or \"1\".
The condition representing \"(x == v)\" in an environment where x is assigned
the value v is consistent with the result . Similarly, the condition
representing \"!(x == v)\" in an environment where x is assigned the value v is
consistent with the result ."
  ;; Look up var from condition in the env.
  (let ((bound-val (second (aget (first condition) env :test #'string=))))
    (case (third condition)
      (:eq (if (equal bound-val (second condition))
               ;; If it's an equality condition matching the env, it's
               ;; consistent with a result of 1.
               (equal result "1")
               (equal result "0")))
      (:neq (if (not (equal bound-val (second condition)))
                ;; An inequality condition that is satisfied under the
                ;; env is consistent with a result of 1.
                (equal result "1")
                (equal result "0")))
      (t nil))))

(defun find-best-condition (recorded-results envs conditions)
  "Find the condition which correctly matches the largest number of recorded
results when evaluated under the corresponding environment."
  (iter (for condition in conditions)
        (finding condition maximizing
                 (iter (for result in recorded-results)
                       (for env in envs)
                       (sum (if (entails env condition result) 1 0))))))

(defun run-test-case (test bin test-suite
                      &key default abst-conds loop-count)
  "Run a test case, returning the fitness score, condition values, and
environments."
  (ignore-errors (delete-file *trace-file*))
  (bind ((env (append (when default `(("ABST_COND_DEFAULT" ,default)))
                      (when abst-conds `(("ABST_COND_VALUES" ,abst-conds)))
                      (when loop-count `(("ABST_COND_LOOP_COUNT" ,loop-count)))
                      ))
         (output (run-test test-suite bin test
                           :environment env))
         ((:values conds envs) (read-abst-conds-and-envs *trace-file*)))
    (values output conds envs)))

(defun flip (bit-str)
  "For strings of 0s and 1s, drop trailing 1s and change last 0 to a 1"
  (cond
    ((emptyp bit-str) bit-str)
    ((ends-with "1" bit-str :test #'string=)
     (flip (subseq bit-str 0 (1- (length bit-str)))))
    (t (concatenate 'string (subseq bit-str 0 (1- (length bit-str))) "1"))))

(define-condition build-failed (error)
  ((stdout :initarg :stdout :initform nil :reader stdout)
   (stderr :initarg :stderr :initform nil :reader stderr)
   (exit-code :initarg :exit-code :initform nil :reader exit-code))
  (:report (lambda (condition stream)
             (format stream "Build failed with status ~a: ~%~a~%~a~%"
                     (exit-code condition) (stdout condition)
                     (stderr condition)))))

(defun build (software bin)
  (multiple-value-bind (bin errno stderr stdout) (phenome software :bin bin)
    (if (zerop errno)
        bin
        (error (make-condition 'build-failed
                 :stdout stdout
                 :stderr stderr
                 :exit-code errno)))))

(defun build-with-abst-cond (software repair-mutation bin extra-exprs)
  (apply-mutation software repair-mutation)
  ;; include abst_cond implementation
  (setf (genome software)
        (concatenate 'string +abst-cond-source+ (genome software)))
  ;; instrument to output in-scope vars at abst_cond() invocations
  (instrument-abst-cond-traces software *trace-file* extra-exprs)
  ;; Build the whole project
  (build software bin))

(defun collect-negative-conds-and-envs (bin neg-tests test-suite loop-condition)
  "For each negative test case, try to identify a set of 0/1 values
for abst_cond() that would improve fitness. If one is found, return the
recorded decisions and environments."

  ;; There are two different search strategies here for if conditions
  ;; and loop conditions. In the former case, we match SPR's
  ;; algorithm: start with all zeroes (preserving the original
  ;; behavior), record conditions from that run, and then apply the
  ;; "flip" function to the sequence to search variations near the end
  ;; of the run. After a fixed number of attempts, try all ones in
  ;; case the original condition is wrong all the time.

  ;; For loop conditions we try to find a loop count that fixes the
  ;; test. The assumption is that the original condition will loop
  ;; infinitely (nothing in the body changes it). An abstract
  ;; condition value of 0 will continue the loop, and a value of 1
  ;; will exit it (we only do loops with the form
  ;; while (orig && !abst_cond()) { ... }

  ;; So we give abst_cond() periodic behavior: where every Nth value
  ;; is a 1. This works as long the original condition stays true once
  ;; the loop is entered, and all executions of the loop need the same
  ;; count. We may need something more clever in the long run, but
  ;; this does the job for now.

  (iter (for test in neg-tests)
        (multiple-value-bind (test-result recorded-conds envs)
            ;; Run test, preserving original behavior
            (run-test-case test bin test-suite
                           :default 0
                           :loop-count (when loop-condition 0))
          (dotimes (count (1+ *abst-cond-attempts*))
            ;; If the test passes, break out of the dotimes
            (when (> test-result (case-fitness test-suite test))
              (return))
            (if (and (not loop-condition) (equal count *abst-cond-attempts*))
                ;; On the last try, run test with all abst_cond() returning 1
                (multiple-value-bind (tr2 rc2 e2)
                    (run-test-case test bin
                                   test-suite :default 1)
                  (setf test-result tr2)
                  (setf recorded-conds rc2)
                  (setf envs e2))
                ;; Otherwise, try "flipping" the last recorded result
                (multiple-value-bind (tr2 rc2 e2)
                    (run-test-case test bin test-suite
                                   :abst-conds (unless loop-condition
                                                 (flip recorded-conds))
                                   :loop-count (when loop-condition (1+ count)))
                  (setf test-result tr2)
                  (setf recorded-conds rc2)
                  (setf envs e2))))
          ;; If the test succeeded, save the recorded conditions and envs
          (when (> test-result (case-fitness test-suite test))
            (collect recorded-conds into recorded-cond-results)
            (collect envs into env-results))
          (finally
           (return (values recorded-cond-results env-results))))))

(defun collect-positive-conds-and-envs (bin pos-tests test-suite
                                        loop-condition)
  (iter (for test in pos-tests)
        (multiple-value-bind (test-result recorded-conds envs)
            ;; Run test, setting abst_cond() to preserve original
            ;; behavior, and save the environments.
            (run-test-case test bin test-suite
                           :default (if loop-condition 1 0))
          (when test-result
            (collect recorded-conds into recorded-cond-results)
            (collect envs into env-results))
          (finally (return (values recorded-cond-results env-results))))))

(defun make-source (condition)
  "Take a condition of the form \(\"x\" \"val\" :eq\) and return the
corresponding source code condition: \(x == val\) or !\(x == val\)"
  (let* ((base (second condition))
         (val (if (stringp base)
                  (format nil "\"~a\"" base)
                  base)))
    (case (third condition)
      (:eq (format nil "\(~a == ~a\)" (first condition) val))
      (:neq (format nil "!\(~a == ~a\)" (first condition) val))
      (t ""))))

(defun apply-best-cond-mutation (software mutation best-condition)
  (setf (abst-cond mutation) (make-source best-condition))
  (apply-mutation software mutation)
  software)

(defun collect-tests (software test-suite)
  "Make test suite and divide it into positive and negative cases."
  (with-temp-file (bin)
    (build software bin)
    (iter (for case in (test-cases test-suite))
          (let ((fitness (run-test test-suite bin case)))
            (setf (case-fitness test-suite case) fitness)
            (if (>= fitness 1.0)
                (collect case into positive)
                (collect case into negative)))
          (finally (return (values positive negative))))))

(defun improves-fitness (new-fitness test-suite)
  "Are NEW-FITNESS scores and improvement over the current fitness of
TEST-SUITE?"
  (let ((orig-fitness (mapcar {case-fitness test-suite}
                              (test-cases test-suite))))
    (and (every (lambda (new old) (>= new old)) new-fitness orig-fitness)
         (some (lambda (new old) (> new old)) new-fitness orig-fitness))))

(defun synthesize-condition (software test-suite repair-mutation
                             &key extra-instrumentation-exprs)
  "For a program, a set of positive and negative test cases, and a repair
mutation, return t or nil indicating if the repair was successful.

FITNESS-COMMAND-FUN is a function which returns the shell command for
testing fitness. It should take two parameters, the executable and a
list of command-line arguments.

The fitness script itself must accept the following arguments:
 --count: return the total number of test cases
 --which N: run a single test case

By default, the fitness script should run all tests and print the
overall fitness score.

extra-instrumentation-exprs is a list of additional expressions to
print at each instrumentation point. It should have the form
'((expr . ((:type . \"typename\"))))
"

  ;; only applies if repair mutation introduced abst_cond()
  (with-temp-file (trace-file)
    (with-temp-file (bin)
      (when (contains-abstract-condition repair-mutation)
        (let ((*trace-file* trace-file)
              (loop-condition (contains-loop-condition repair-mutation))
              (cur-best ()))
          (build-with-abst-cond (copy software) repair-mutation bin
                                extra-instrumentation-exprs)
          ;; Process negative (failing) test cases
          (bind
              (((:values pos-tests neg-tests)
                (collect-tests software test-suite))
               ((:values neg-conds neg-envs)
                (collect-negative-conds-and-envs bin neg-tests
                                                 test-suite
                                                 loop-condition))
               ((:values pos-conds pos-envs)
                (collect-positive-conds-and-envs bin pos-tests
                                                 test-suite
                                                 loop-condition))
               (conditions (synthesize-conditions (append neg-envs
                                                          pos-envs))))
            (dotimes (count *synth-condition-attempts*)
              (when (not conditions)
                (return nil))
              (let* ((best-cond
                      (find-best-condition (append neg-conds pos-conds)
                                           (append neg-envs pos-envs)
                                           conditions))
                     (test-repair (apply-best-cond-mutation
                                   (copy software) repair-mutation best-cond)))
                (setf conditions (remove best-cond conditions :test #'equal))
                (build test-repair bin)
                (let ((new-fitness (mapcar {run-test test-suite bin}
                                           (test-cases test-suite))))
                  (when (improves-fitness new-fitness test-suite)
                    (setf cur-best test-repair)
                    (loop for test in (test-cases test-suite)
                       for f in new-fitness do
                         (setf (case-fitness test-suite test) f))
                                        ; repair found
                    (when (every {>= _ 1.0} new-fitness)
                      (return-from synthesize-condition cur-best)))))))
          cur-best)))))

(defun tails (lst)
  (when lst (cons lst (tails (cdr lst)))))

(defun instrumentation-exprs (obj point type)
  "Generate additional expressions to instrument during condition
synthesis. Finds all expressions of the given type which are in scope
at the repair point, and generates comparisons for all of them. These
expressions can be passed as EXTRA-INSTRUMENTATION-EXPRS to
synthesize-condition.

OBJ ---------- software object
POINT -------- repair point
TYPE --------- type description alist (:types :array :pointer :compare)
               Where :compare is a format template for the comparison
               expressions, and the other fields are matched against
               the type DB.
"
  (let* ((type-hash (aget :hash (find-if {types-equal type} (types obj))))
         ;; FIXME: should only grab expressions that are valid at
         ;; point. But this is tricky with anything beyond simple
         ;; DeclRefs.
         (scope (aget :body (function-containing-ast obj point)))
         (exprs (remove-duplicates
                 (remove-if-not
                  (lambda (a)
                    (and (eql type-hash (aget :expr-type a))
                         ;; Skip macro bodies
                         (not (aget :in-macro-expansion a))
                         ;; Avoid side effects by excluding calls and
                         ;; assignment operators.
                         (not (string= "CallExpr" (aget :ast-class a)))
                         (not (string= "BinaryOperator" (aget :ast-class a)))
                         ;; Check scope
                         (ancestor-of obj scope (aget :counter a))))
                  (asts obj))
                 :test #'string= :key {aget :src-text})))
    (loop for (a . rest) in (tails exprs)
       appending
         (loop for b in rest
            collecting
              `(,(format nil (aget :compare type)
                         (peel-bananas (aget :src-text a))
                         (peel-bananas (aget :src-text b)))
                 . ((:type . "int")))))))

(defun types-equal (a b)
  (and (string= (aget :type a) (aget :type b))
       (string= (aget :array a) (aget :array b))
       (eq (aget :pointer a) (aget :pointer b))))
