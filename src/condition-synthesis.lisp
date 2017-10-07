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

;;; See the "Condition Synthesis" section in the SEL manual.

(in-package :software-evolution)
(enable-curry-compose-reader-macros :include-utf8)


;;; Synthesis mutations

;; Helper functions
(defun guard-statements (software)
  (remove-if-not #'ast-guard-stmt (bad-stmts software)))

(defun pick-target-condition (software)
  "Pick a condition to target for refinement."
  (ast-counter (random-elt (guard-statements software))))

(defun pick-if-stmt (software)
  (let ((stmts (remove-if-not [{eq :IfStmt} #'ast-class ]
                              (bad-stmts software))))
    (and stmts (random-elt stmts))))

(defun or-connector (left right)
  (make-operator :generic "||" (list left right) :guard-stmt t))

(defun and-not-connector (left right)
  (make-operator :generic "&&"
                 (list left
                       (make-operator :generic "!" (list right)))
                 :guard-stmt t))

;; refine-condition: add an additional boolean clause to an if condition
(define-mutation refine-condition (clang-mutation)
  ((targeter :initform (lambda (software)
                         (pick-target-condition software)))
   (connector :reader connector)
   (abst-cond :accessor abst-cond :initform nil)))

(define-mutation tighten-condition (refine-condition)
  ((targeter :initform (lambda (software)
                         (pick-target-condition software)))
   (connector :reader connector :initform #'and-not-connector)))

(define-mutation loosen-condition (refine-condition)
  ((targeter :initform (lambda (software)
                         (pick-target-condition software)))
   (connector :reader connector :initform #'or-connector)))

(defgeneric valid-targets (mutation software)
  (:documentation "List of locations where this mutation can be applied."))

(defmethod valid-targets ((mutation refine-condition) software)
  (remove-if #'ast-in-macro-expansion (guard-statements software)))

(defun abst-cond-expr ()
  (make-statement :CallExpr :generic
                  (list
                   (make-statement
                    :ImplicitCastExpr :generic
                    (list
                     (make-statement :DeclRefExpr :generic
                                     '("(|abst_cond|)")
                                     :unbound-funs '(("(|abst_cond|)"
                                                      nil nil 0)))))
                   "()")))

(defun refined-condition (mutation target-ast)
  (funcall (connector mutation)
           (make-parens (list target-ast))
           (or (abst-cond mutation) (abst-cond-expr))))

(defmethod build-op ((mutation refine-condition) software)
  (declare (ignorable software))
  (let ((target-ast (targets mutation)))
    `((:set (:stmt1 . ,target-ast)
            (:literal1 . ,(refined-condition mutation
                                             target-ast))))))

;; add-condition: wrap a statement in an if
(define-mutation add-condition (clang-mutation)
  ((targeter
    :initform
    (lambda (software)
      (ast-counter
            (random-elt
             (->> (asts software)
                  (remove-if-not #'ast-full-stmt)
                  (remove-if [{eq :Function} #'ast-class])
                  (remove-if [{eq :DeclStmt} #'ast-class]))))))
   (abst-cond :accessor abst-cond :initform nil)))

(defmethod build-op ((mutation add-condition) software)
  (declare (ignorable software))
  (let* ((target-ast (targets mutation))
         (abst-cond (or (abst-cond mutation) (abst-cond-expr)))
         (body (if (or (ends-with (source-text target-ast) #\;)
                       (ends-with (source-text target-ast) #\}))
                   (list target-ast)
                   (list target-ast ";"))))
    `((:set (:stmt1 . ,target-ast)
            (:literal1 . ,(make-if-stmt abst-cond
                                        (make-block body)))))))

(defmethod valid-targets ((mutation add-condition) software)
  (remove-if «or {aget :in-macro-expansion}
                 [#'not {full-stmt-p software}]»
              (bad-stmts software)))

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
    (bind ((ast (targets mutation))
           (children (get-immediate-children software ast))
           (replacement (make-while-stmt (ast-syn-ctx ast)
                                        (refined-condition mutation
                                                           (first children))
                                        (second children))))
      `((:set (:stmt1 . ,ast)
              (:literal1 . ,replacement))))))

;; insert-else-if
;; insert an else-if clause for an existing if, insert abstract conditional
;; return the 2nd child (then branch) for an If
(define-mutation insert-else-if (tighten-condition)
  ((targeter :initform #'pick-if-stmt)))

;; FIXME: no longer needed
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
    (bind ((ast (targets mutation))
           (children (get-immediate-children software ast))
           (replacement (make-while-stmt (ast-syn-ctx ast)
                                         (first children)
                                         (second children))))
      `((:set (:stmt1 . ,ast)
              (:literal1 . ,replacement))))))

(defmethod build-op ((mutation insert-else-if) software)
  (when (targets mutation)
    (let* ((if-stmt (targets mutation))
           (children (get-immediate-children software if-stmt))
           (else-if (make-if-stmt (or (abst-cond mutation) (abst-cond-expr))
                                  (make-block '("/* empty for now */"))
                                  (third children))))
      `((:set
         (:stmt1 . ,if-stmt)
         (:literal1 . ,(make-if-stmt (first children)
                                     (second children)
                                     else-if))
         (:includes . "\"abst_cond.h\""))))))

(defmethod valid-targets ((mutation if-to-while) software)
  (remove-if #'ast-in-macro-expansion
             (remove-if-not [{eq :IfStmt} #'ast-class]
                            (bad-stmts software))))

(defmethod valid-targets ((mutation insert-else-if) software)
  (remove-if #'ast-in-macro-expansion
             (remove-if-not [{eq :IfStmt} #'ast-class]
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

int abst_cond() {
    static int abst_cond_initialized = 0;
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
    fprintf(__sel_trace_file, \"((:ABST-COND . \\\"%d\\\"))\\n\", result);
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
  (iter (for ast in (remove-if-not #'ast-guard-stmt (asts software)))
        (multiple-value-bind (match-start match-end reg-start reg-end)
            (scan "abst_cond\\(\\)" (peel-bananas (source-text ast)))
          (declare (ignorable reg-start reg-end))
          (when match-start
            (collect ast into asts)
            (collect match-start into starts)
            (collect match-end into ends))
          (finally (return (mapcar #'list asts starts ends))))))

(defun get-parent-control-stmt (clang guard-counter)
  "Returns the AST structure of the enclosing if statement for a guard."
  (let ((stmt (car (remove-if-not
                    [{member _ '(:IfStmt :WhileStmt :ForStmt :DoStmt
                                 :SwitchStmt)}
                     #'ast-class]
                    (get-parent-asts clang guard-counter)))))
    (assert stmt)
    stmt))

;; FIXME: mostly copied from var-instrument in clang-instrument.lisp
(defun instrument-values (obj ast &key print-strings extra-exprs)
  (let ((exprs
         (remove nil
                 (append (mapcar (lambda (v)
                                   (cons (aget :name v)
                                         (find-var-type obj v)))
                                 (get-vars-in-scope obj ast))
                         extra-exprs))))
    (iter (for (var . type) in exprs)
          (multiple-value-bind (c-type fmt-code)
              (type-instrumentation-info type print-strings)
            ;; Special case for C++ strings
            (when (and print-strings
                       (string= c-type "string"))
              (concatenating (format nil " (\\\"~a\\\" \\\"~a\\\" \\\"%s\\\")"
                                     var c-type)
                             into format
                             initial-value (format nil "(:scopes"))
              (collect (format nil "~a.c_str()" var) into vars))
            ;; Standard types
            (when fmt-code
              (concatenating (format nil " (\\\"~a\\\" \\\"~a\\\" ~a)"
                                     var c-type fmt-code)
                             into format
                             initial-value (format nil "(:scopes"))
              (collect var into vars))

            )
          (finally (return (cons (concatenate 'string format ")") vars))))))

(defun instrument-abst-cond-traces (software trace-file-name extra-exprs)
  "Add instrumentation before the enclosing if statement for the guard
statement(s) in the repair targets."

  ;; Look for repair-locs only in the main target (e.g. for clang-project)
  (let* ((repair-locs (mapcar [{get-parent-control-stmt software} #'car]
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
                     (asts software))))

    ;; sometimes inst-locs comes up nil (no full statement children),
    ;; set to enclosing full stmt, if so
    (unless inst-locs
      (setf inst-locs (mapcar {enclosing-full-stmt software } repair-locs)))

    (let ((inst-reps (lambda (obj ast)
                       (when (member ast inst-locs :test #'equalp)
                         (instrument-values obj ast
                                            :print-strings t
                                            :extra-exprs extra-exprs)))))
      ;; Instrument the original object
      (instrument software :trace-file trace-file-name
                  :functions (list inst-reps)
                  ;; Only instrument within relevant functions
                  :filter {remove-if-not [{member _ inst-functions
                                                  :test #'equalp}
                                       {function-containing-ast software}]}))))

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
               (collect (cons :eq assmt) into assmts)
               (collect (cons :neq assmt) into assmts)
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
  (destructuring-bind (comparison-type name type base) condition
    (declare (ignorable type))
    (let ((bound-val (second (aget name env :test #'string=))))
      (case comparison-type
        (:eq (if (equal bound-val base)
                 ;; If it's an equality condition matching the env, it's
                 ;; consistent with a result of 1.
                 (equal result "1")
                 (equal result "0")))
        (:neq (if (not (equal bound-val base))
                  ;; An inequality condition that is satisfied under the
                  ;; env is consistent with a result of 1.
                  (equal result "1")
                  (equal result "0")))
        (t nil)))))

(defun find-best-condition (recorded-results envs conditions)
  "Find the condition which correctly matches the largest number of recorded
results when evaluated under the corresponding environment."
  (iter (for condition in conditions)
        (finding condition maximizing
                 (iter (for result in recorded-results)
                       (for env in envs)
                       (sum (if (entails env condition result) 1 0))))))

(defun run-test-case (test bin &key default abst-conds loop-count)
  "Run a test case, returning the fitness score, condition values, and
environments."
  (ignore-errors (delete-file *trace-file*))
  (bind ((env (append (when default `(("ABST_COND_DEFAULT" .
                                       ,(write-to-string default))))
                      (when abst-conds `(("ABST_COND_VALUES" .
                                          ,(write-to-string abst-conds))))
                      (when loop-count `(("ABST_COND_LOOP_COUNT" .
                                          ,(write-to-string loop-count))))))
         (output (evaluate bin test :env env
                           :output :stream
                           :error :stream))
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

(defun collect-negative-conds-and-envs (bin neg-tests loop-condition)
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
            (run-test-case test bin
                           :default 0
                           :loop-count (when loop-condition 0))
          (dotimes (count (1+ *abst-cond-attempts*))
            ;; If the test passes, break out of the dotimes
            (when (> test-result (fitness test))
              (return))
            (if (and (not loop-condition) (equal count *abst-cond-attempts*))
                ;; On the last try, run test with all abst_cond() returning 1
                (multiple-value-bind (tr2 rc2 e2)
                    (run-test-case test bin
                                   :default 1)
                  (setf test-result tr2)
                  (setf recorded-conds rc2)
                  (setf envs e2))
                ;; Otherwise, try "flipping" the last recorded result
                (multiple-value-bind (tr2 rc2 e2)
                    (run-test-case test bin
                                   :abst-conds (unless loop-condition
                                                 (flip recorded-conds))
                                   :loop-count (when loop-condition (1+ count)))
                  (setf test-result tr2)
                  (setf recorded-conds rc2)
                  (setf envs e2))))
          ;; If the test succeeded, save the recorded conditions and envs
          (when (> test-result (fitness test))
            (collect recorded-conds into recorded-cond-results)
            (collect envs into env-results))
          (finally
           (return (values recorded-cond-results env-results))))))

(defun collect-positive-conds-and-envs (bin pos-tests loop-condition)
  (iter (for test in pos-tests)
        (multiple-value-bind (test-result recorded-conds envs)
            ;; Run test, setting abst_cond() to preserve original
            ;; behavior, and save the environments.
            (run-test-case test bin
                           :default (if loop-condition 1 0))
          (when test-result
            (collect recorded-conds into recorded-cond-results)
            (collect envs into env-results))
          (finally (return (values recorded-cond-results env-results))))))

(defun make-source (software condition)
  "Take a condition of the form \(\"x\" \"val\" :eq\) and return the
corresponding source code condition: \(x == val\) or !\(x == val\)"
  (bind (((comparison-type name type base) condition)
         (var (make-var-reference name
                                  (find-or-add-type software type)))
         (val (if (stringp base)
                  (make-literal :string base)
                  (make-literal :integer base)))
         (eq-op (make-parens
                 (list (make-operator :generic "==" (list var val))))))
    (case comparison-type
      (:eq eq-op)
      (:neq (make-operator :generic "!" (list eq-op)))
      (t ""))))

(defun apply-best-cond-mutation (software mutation best-condition)
  (setf (abst-cond mutation) (make-source software best-condition))
  (apply-mutation software mutation)
  software)

(defun collect-tests (software test-suite)
  "Build SOFTWARE and run TEST-SUITE, dividing the test cases into positive and
negative tests."
  (with-temp-file (bin)
    (build software bin)
    (iter (for case in (test-cases test-suite))
          (let ((test-fitness (evaluate bin case
                                        :output :stream
                                        :error :stream)))
            (setf (fitness case) test-fitness)
            (if (>= test-fitness 1.0)
                (collect case into positive)
                (collect case into negative)))
          (finally (return (values positive negative))))))

(defun improves-fitness (new-fitness test-suite)
  "Are NEW-FITNESS scores an improvement over the current fitness of
TEST-SUITE?"
  (let ((orig-fitness (mapcar #'fitness (test-cases test-suite))))
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
                (collect-negative-conds-and-envs bin neg-tests loop-condition))
               ((:values pos-conds pos-envs)
                (collect-positive-conds-and-envs bin pos-tests loop-condition))
               (conditions (synthesize-conditions (append neg-envs pos-envs))))
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
                ;; If the build fails, ignore it and continue to the next
                ;; condition.
                (let ((build-result
                        (handler-case (build test-repair bin)
                          (build-failed (e)
                            (note 3 "Build failed (~a) applying condition ~a: ~a"
                                  (exit-code e) best-cond (stderr e))
                            nil))))
                  (when build-result
                    (let ((new-fitness (mapcar
                                        (lambda (test-case)
                                          (evaluate bin test-case :output :stream
                                                    :error :stream))
                                        (test-cases test-suite))))
                      (when (improves-fitness new-fitness test-suite)
                        (setf cur-best test-repair)
                        (loop for test in (test-cases test-suite)
                           for f in new-fitness do
                             (setf (fitness test) f))
                                        ; repair found
                        (when (every {>= _ 1.0} new-fitness)
                          (return-from synthesize-condition cur-best)))))))))
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
  (let* ((type-hash
          (when-let ((clang-type (find-if {types-equal type}
                                          (hash-table-values (types obj)))))
            (type-hash clang-type)))
         ;; FIXME: should only grab expressions that are valid at
         ;; point. But this is tricky with anything beyond simple
         ;; DeclRefs.
         (scope (function-body obj (function-containing-ast obj point)))
         (exprs (remove-duplicates
                 (remove-if-not
                  (lambda (a)
                    (and (eql type-hash (ast-expr-type a))
                         ;; Skip macro bodies
                         (not (ast-in-macro-expansion a))
                         ;; Avoid side effects by excluding calls and
                         ;; assignment operators.
                         (not (eq :CallExpr (ast-class a)))
                         (not (eq :BinaryOperator (ast-class a)))
                         ;; Check scope
                         (ancestor-of obj scope a)))
                  (stmt-asts obj))
                 :test #'string= :key #'source-text)))
    (loop for (a . rest) in (tails exprs)
       appending
         (loop for b in rest
            collecting
              (cons (format nil (aget :compare type)
                         (peel-bananas (source-text a))
                         (peel-bananas (source-text b)))
                    (make-clang-type :name "int" :array "" :hash 0))))))

(defun types-equal (snippet type)
  "Does SNIPPET (an alist) represent the same type as TYPE (a type AST)?"
  (and (string= (aget :type snippet) (type-name type))
       (string= (aget :array snippet) (type-array type))
       (eq (aget :pointer snippet) (type-pointer type))))
