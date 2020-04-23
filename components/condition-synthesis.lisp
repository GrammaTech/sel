;;;; condition-synthesis.lisp --- synthesize condition expressions
;;;
;;; See the "Condition Synthesis" section in the SEL manual.
;;;
(defpackage :software-evolution-library/components/condition-synthesis
  (:nicknames :sel/components/condition-synthesis :sel/cp/condition-synthesis)
  (:use :gt/full
        :metabang-bind
        :trace-db
        :software-evolution-library
        :software-evolution-library/utility/debug
        :software-evolution-library/software/parseable
        :software-evolution-library/software/source
        :software-evolution-library/software/clang
        :software-evolution-library/components/test-suite
        :software-evolution-library/components/instrument
        :software-evolution-library/components/clang-instrument)
  (:import-from :arrow-macros :some->>) ; FIXME: Remove.
  (:export :instrumentation-exprs
           :synthesize-condition
           :synthesize-conditions
           :entails
           :find-best-condition
           :add-condition
           :flip-bit-str
           :tighten-condition
           :loosen-condition
           :refine-condition
           :valid-targets
           :if-to-while
           :if-to-while-tighten-condition
           :insert-else-if
           :*abst-cond-attempts*
           :*synth-condition-attempts*))
(in-package :software-evolution-library/components/condition-synthesis)
(in-readtable :curry-compose-reader-macros)


;;; Synthesis mutations

;; Helper functions
(defun guard-statements (software)
  "Return a list of guard-statement ASTs in SOFTWARE."
  (remove-if-not #'ast-guard-stmt (bad-stmts software)))

(defun pick-target-condition (software)
  "Return the AST counter of a randomly selected guard statement in SOFTWARE.
* SOFTWARE software object with conditional AST(s)"
  (1+ (index-of-ast software (random-elt (guard-statements software)))))

(defun pick-if-stmt (software)
  "Return the AST for a randomly selected `if' statement in SOFTWARE.
* SOFTWARE software object with if statement AST(s)"
  (when-let ((stmts (remove-if-not [{eq :IfStmt} #'ast-class ]
                              (bad-stmts software))))
    (random-elt stmts)))

(defun or-connector (left right)
  "Return an AST connecting the LEFT and RIGHT ASTs as an expression
`LEFT || RIGHT'.
* LEFT AST to use for the left-hand-side of an OR expression
* RIGHT AST to use for the right-hand-side of an OR expression"
  (make-operator :generic "||" (list left right) :guard-stmt t))

(defun and-not-connector (left right)
  "Return an AST connecting the LEFT and RIGHT ASTs as an expression
`LEFT && !RIGHT'.
* LEFT AST to use for the left-hand-side of an AND expression
* RIGHT AST to use for the right-hand-side of an AND expression"
  (make-operator :generic "&&"
                 (list left
                       (make-operator :generic "!" (list right)))
                 :guard-stmt t))

(define-mutation refine-condition (clang-mutation)
  ((targeter :initform (lambda (software)
                         (pick-target-condition software)))
   (connector :reader connector)
   (abst-cond :accessor abst-cond :initform nil))
  (:documentation "Add an additional Boolean clause to an if condition."))

(define-mutation tighten-condition (refine-condition)
  ((targeter :initform (lambda (software)
                         (pick-target-condition software)))
   (connector :reader connector :initform #'and-not-connector))
  (:documentation "Add an abstract condition to make a guard less permissive.
E.g., if(foo) becomes if(foo && !abst_cond())."))

(define-mutation loosen-condition (refine-condition)
  ((targeter :initform (lambda (software)
                         (pick-target-condition software)))
   (connector :reader connector :initform #'or-connector))
  (:documentation "Add an abstract condition to make a guard more permissive.
E.g., if(foo) becomes if(foo || abst_cond())."))


(defgeneric valid-targets (mutation software)
  (:documentation "Return a list of the locations in SOFTWARE where MUTATION can be applied."))


(defmethod valid-targets ((mutation refine-condition) software)
  "Return a list of the locations in SOFTWARE where `refine-condition' MUTATION can be applied."
  (remove-if #'ast-in-macro-expansion (guard-statements software)))


(defun abst-cond-expr ()
  "Return an AST for a function call to abst_cond()."
  (make-statement :CallExpr :generic
                  (list
                   (make-statement
                    :ImplicitCastExpr :generic
                    (list
                     (make-statement :DeclRefExpr :generic '("abst_cond"))))
                   "()")))


(defun refined-condition (mutation target-ast)
  "Return an AST with TARGET-AST and an additional boolean clause
* MUTATION mutation defining the type or boolean clause to add
* TARGET-AST AST to add an additional boolean clause to"
  (funcall (connector mutation)
           (make-parens (list target-ast))
           (or (abst-cond mutation) (abst-cond-expr))))

(defmethod build-op ((mutation refine-condition) software)
  "Return an association list with the operations to apply
a refine-condition MUTATION to SOFTWARE."
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
      (nest (1+)
            (index-of-ast software)
            (random-elt)
            (remove-if [{eq :DeclStmt} #'ast-class])
            (remove-if [{eq :Function} #'ast-class])
            (remove-if-not #'ast-full-stmt)
            (asts software))))
   (abst-cond :accessor abst-cond :initform nil))
  (:documentation "Guard a statement with an abstract condition.
E.g., foo\; becomes if(abst_cond()) foo\;."))


(defmethod build-op ((mutation add-condition) software)
  "Return an association list with the operations to apply
a add-condition MUTATION to SOFTWARE."
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
 "Return a list of the locations in SOFTWARE where `add-condition' MUTATION can be applied."
  (remove-if «or {aget :in-macro-expansion}
                 [#'not #'ast-full-stmt]»
		 (bad-stmts software)))


;; if-to-while
;; replace an if statement with a while using the same condition
(define-mutation if-to-while (clang-mutation)
  ((targeter :initform #'pick-if-stmt))
  (:documentation "Replace an if statement with a while on the same condition.
E.g., if(foo) becomes while(foo)."))


;; if-to-while-tighten-condition
;; Combine if-to-while and synthesize a refined condition
(define-mutation if-to-while-tighten-condition (if-to-while tighten-condition)
  ()
  (:documentation "Combine if-to-while replacement with a tighter condition.
E.g., if(foo) becomes while(foo && !abst_cond())."))


(defmethod build-op ((mutation if-to-while-tighten-condition) software)
  "Return an association list with the operations to apply
an if-to-while-tighten-condition MUTATION to SOFTWARE."
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
  ((targeter :initform #'pick-if-stmt))
  (:documentation "insert an (empty) else-if clause after an existing if, e.g., if(foo) bar; becomes if(foo) bar; else if(abst_cond()) ;."))


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
  "Return an association list with the operations to apply
an if-to-while MUTATION to SOFTWARE"
  (when (targets mutation)
    (bind ((ast (targets mutation))
           (children (get-immediate-children software ast))
           (replacement (make-while-stmt (ast-syn-ctx ast)
                                         (first children)
                                         (second children))))
      `((:set (:stmt1 . ,ast)
              (:literal1 . ,replacement))))))


(defmethod build-op ((mutation insert-else-if) software)
  "Return an association list with the operations to apply
an insert-else-if MUTATION to SOFTWARE."
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
 "Return a list of the locations in SOFTWARE where `if-to-while' MUTATION can be applied."
  (remove-if #'ast-in-macro-expansion
             (remove-if-not [{eq :IfStmt} #'ast-class]
                            (bad-stmts software))))

(defmethod valid-targets ((mutation insert-else-if) software)
 "Return a list of the locations in SOFTWARE where `insert-else-if' MUTATION can be applied."
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
    __write_trace_aux(__sel_trace_file, result);
    return result;
}
"
  :test #'string=
  :documentation "C code for abst_cond() function.")

(defparameter *abst-cond-attempts* 10
  "Maximum number of combinations for abst_cond() values to attempt before
discarding a repair.")

(defparameter *synth-condition-attempts* 20
  "Maximum number of synthesized conditions to test before discarding a repair.")

(defgeneric contains-abstract-condition (mutation)
  (:documentation "Return T if MUTATION contains an abstract condition,
NIL otherwise."))

(defmethod contains-abstract-condition ((mut mutation))
  "Return T if `mutation' MUT contains an abstract condition, NIL otherwise"
  (declare (ignorable mut))
  nil)

(defmethod contains-abstract-condition ((mut refine-condition))
  "Return T if `refine-condition' MUT contains an abstract condition, NIL
otherwise."
  (declare (ignorable mut))
  t)

(defmethod contains-abstract-condition ((mut add-condition))
  "Return T if `add-condition' MUT contains an abstract condition, NIL
otherwise."
  (declare (ignorable mut))
  t)

(defgeneric contains-loop-condition (mutation)
  (:documentation "Return T if MUTATION contains an abstract loop condition, NIL
otherwise."))

(defmethod contains-loop-condition (mutation)
  "Return NIL as general MUTATIONs do not have an abstract loop condition."
  (declare (ignorable mutation))
  nil)

(defmethod contains-loop-condition ((mutation if-to-while))
  "Return T as if-to-while MUTATIONs have an abstract loop condition."
  (declare (ignorable mutation))
  t)

(defun get-abst-cond-locs (software)
  "Identify guard statements in SOFTWARE that contain a call to
abst_cond(), and return a list of lists containing the AST counter
along with start and end index of abst_cond() in the source text.
"
  (iter (for ast in (remove-if-not #'ast-guard-stmt (asts software)))
        (multiple-value-bind (match-start match-end reg-start reg-end)
            (scan "abst_cond\\(\\)" (source-text ast))
          (declare (ignorable reg-start reg-end))
          (when match-start
            (collect ast into asts)
            (collect match-start into starts)
            (collect match-end into ends))
          (finally (return (mapcar #'list asts starts ends))))))


(defun get-parent-control-stmt (clang guard-ast)
  "Return the AST structure of the enclosing if statement for a guard.
* CLANG software object to search for parent control statement
* GUARD-AST AST with a guard statement
"
  (let ((stmt (car (remove-if-not
                    [{member _ '(:IfStmt :WhileStmt :ForStmt :DoStmt
                                 :SwitchStmt)}
                     #'ast-class]
                    (get-parent-asts clang guard-ast)))))
    (assert stmt)
    stmt))


(defun instrument-values (instrumenter ast &key extra-exprs)
  "Return a list of ASTs to print the values of variables in scope at AST
and EXTRA-EXPRS
* INSTRUMENTER for generating instrumentation code
* AST ast to instrument
* EXTRA-EXPRS additional expressions to include in instrumentation
"
  (let ((exprs (nest (remove nil)
                     (append extra-exprs)
                     (mapcar (lambda (v)
                               (cons (aget :name v)
                                     (find-var-type (software instrumenter) v)))
                             (get-vars-in-scope (software instrumenter) ast)))))
    (instrument-c-exprs instrumenter exprs t)))

(defun instrument-abst-cond-traces (software trace-file-name extra-exprs)
  "Return software with instrumentation before the enclosing if statement
for the guard statement(s) in the repair targets.
* SOFTWARE software to add instrumentation to
* TRACE-FILE-NAME file to write traces created by instrumentation
* EXTRA_EXPRS additional expressions to include in instrumentation
"
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
                       (and (ast-full-stmt ast)
                            (some (lambda (loc) (ancestor-of software loc ast))
                                  repair-locs)))
                     (asts software))))

    ;; sometimes inst-locs comes up nil (no full statement children),
    ;; set to enclosing full stmt, if so
    (unless inst-locs
      (setf inst-locs (mapcar {enclosing-full-stmt software } repair-locs)))

    (let ((inst-reps (lambda (instrumenter ast)
                       (when (member ast inst-locs :test #'equalp)
                         (instrument-values instrumenter ast
                                            :extra-exprs extra-exprs)))))
      ;; Instrument the original object
      (instrument software :trace-file trace-file-name
                  :functions (list inst-reps)
                  ;; Only instrument within relevant functions
                  :filter (lambda (obj ast)
                            (member (function-containing-ast obj ast)
                                    inst-functions
                                    :test #'equalp))))))

(defun read-abst-conds-and-envs (trace-results-file)
  "For trace file TRACE-RESULTS-FILE, read in the environments and
recorded abst-cond decisions.
Return two values: a list of strings of the abst-cond decisions and the list of
environments."
  (when (probe-file trace-results-file)
    (let ((prev-env nil))
      (iter (for sexpr in (read-binary-trace trace-results-file))
            ;; iter can exhaust the stack on long traces. Bail out
            ;; before that happens.
            (for i below *max-trace-length*)
            (while sexpr)
            (when (aget :scopes sexpr)
              (setf prev-env (aget :scopes sexpr)))
            (when (aget :aux sexpr)
              ;; Convert var infos from vectors to lists
              (collect (mapcar [{take 3} {coerce _ 'list}] prev-env) into envs)
              (collect (aref (aget :aux sexpr) 0) into abst-conds))
            (finally (return (values (format nil "~{~a~}" abst-conds)
                                     (apply #'append envs))))))))

(defun synthesize-conditions (envs)
  "For each assignment in each environment in ENVS, create a list of condition
triples representing (x == v) and !(x == v).
Return a list of synthesized condition triples, by prepending :eq or :neq to each
assignment in the environments in ENV.
* ENVS a list of environments, each of which contains a list of triples:
(\"var\" \"type\" \"value\")."
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
  "Check whether the value of CONDITION in environment ENV is consistent
with the desired RESULT, returning T if so and NIL otherwise.

* ENV a list of assignment triples (\"var\" \"type\" \"value\")
* CONDITION a 4-tuple ([:eq|:neq] \"var\" \"type\" \"value\")
* RESULT a string: either \"0\" or \"1\".

The condition representing \"(x == v)\" in
an environment where x is assigned the value v is consistent with the
result. Similarly, the condition representing \"!(x == v)\" in an
environment where x is assigned the value v is not consistent with the
result."
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
  "Return the condition which correctly matches the largest number of recorded
results when evaluated under the corresponding environment.
* RECORDED-RESULTS list of strings \"0\" or \"1\" indicating values of abstract
conditions.
* ENVS list of environments, which are a list of assignment triples
\(\"var\" \"type\" \"value\"\)
* CONDITIONS a 4-tuple \([:eq|:neq] \"var\" \"type\" \"val\"\) representing a
condition \(var == val\) or !\(var == val\)
"
  (iter (for condition in conditions)
        (finding condition maximizing
                 (iter (for result in recorded-results)
                       (for env in envs)
                       (sum (if (entails env condition result) 1 0))))))

(defun run-test-case (test bin &key default abst-conds loop-count)
  "Run a `test-case', returning the fitness score, condition values, and
environments.
* TEST the `test-case' to run
* BIN binary to use in test execution
* DEFAULT single value to be returned by all abstract conditions
* ABST-CONDS series of return values to be used in order by abstract conditions
* LOOP-COUNT for loops, exit the loop after at most LOOP-COUNT iterations
Otherwise, return DEFAULT or 0 if default is unspecified.
"
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


(defun flip-bit-str (bit-str)
  "Return the string obtained by dropping all trailing 1s from BIT-STR and then
changing the final 0 to a 1.
* BIT-STR a string containing only \"0\" and \"1\"
"
  (cond
    ((emptyp bit-str) bit-str)
    ((ends-with "1" bit-str :test #'string=)
     (flip-bit-str (subseq bit-str 0 (1- (length bit-str)))))
    (t (concatenate 'string (subseq bit-str 0 (1- (length bit-str))) "1"))))



(defun build-with-abst-cond (software repair-mutation bin extra-exprs)
  "Apply REPAIR-MUTATION to SOFTWARE, instrument with EXTRA-EXPRS, save as BIN.
* SOFTWARE software object to mutate and instrument
* REPAIR-MUTATION mutation to insert an abstract condition
* BIN destination to compile the intrumented software object
* EXTRA-EXPRS additional expressions to include in instrumentation
"
  (apply-mutation software repair-mutation)
  ;; include abst_cond implementation

  (setf (genome software)
        (concatenate 'string +abst-cond-source+ (genome software)))
  ;; instrument to output in-scope vars at abst_cond() invocations
  (instrument-abst-cond-traces software *trace-file* extra-exprs)
  ;; Build the whole project
  (phenome software :bin bin))


(defun collect-negative-conds-and-envs (bin neg-tests loop-condition)
  "For each negative test case in NEG-TESTS, try to identify a set of
0/1 values for abst_cond() that would improve fitness. If one is
found, return the recorded decisions and environments.
* BIN binary to test
* NEG-TESTS a list of `test-case' objects containing only failing tests
* LOOP-CONDITION T if the abstract condition is for a loop, NIL otherwise
"
  ;; There are two different search strategies here for if conditions
  ;; and loop conditions. In the former case, we match SPR's
  ;; algorithm: start with all zeroes (preserving the original
  ;; behavior), record conditions from that run, and then apply the
  ;; "flip-bit-str" function to the sequence to search variations near
  ;; the end of the run. After a fixed number of attempts, try all
  ;; ones in case the original condition is wrong all the time.

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
                                                 (flip-bit-str recorded-conds))
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
  "For each positive test case in POS-TESTS, record the values for abstract
conditions that cause the tests to pass.
* BIN binary to test
* POS-TESTS a list of `test-case' objects containing only passing tests
* LOOP-CONDITION T if the abstract condition is for a loop, NIL otherwise
"
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
  "Return an AST for the source code corresponding to CONDITION in SOFTWARE.
* SOFTWARE a software object
* CONDITION a condition of the form \(:eq \"x\" \"type\" \"val\"\),
\(:neq \"x\" \"type\" \"val\"\), or T.
"
  (bind (((comparison-type name type base) condition)
         (var (make-var-reference name
                                  (find-or-add-type software type)))
         (val (if (stringp base)
                  (make-literal base)
                  (make-literal base)))
         (eq-op (make-parens
                 (list (make-operator :generic "==" (list var val))))))
    (case comparison-type
      (:eq eq-op)
      (:neq (make-operator :generic "!" (list eq-op)))
      (t ""))))


(defun apply-best-cond-mutation (software mutation best-condition)
  "Apply MUTATION to SOFTWARE, using BEST-CONDITION to replace the abstract
condition.
* SOFTWARE software object to have MUTATION applied to
* MUTATION mutation to apply to SOFTWARE
* BEST-CONDITION synthesized condition to replace an abstract condition
"
  (setf (abst-cond mutation) (make-source software best-condition))
  (apply-mutation software mutation)
  software)


(defun collect-tests (software test-suite)
  "Build SOFTWARE and run TEST-SUITE, dividing the test cases into positive and
negative tests."
  (with-temporary-file (:pathname bin)
    (phenome software :bin bin)
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
  "Apply REPAIR-MUTATION to SOFTWARE, attempting to synthesize a
condition which increases the number of tests in TEST-SUITE that pass.
Return a copy of SOFTWARE with a synthesized condition inserted at the
target of REPAIR-MUTATION.

* TEST-SUITE a `test-suite' containing both positive and negative test cases.
* REPAIR-MUTATION a `mutation' that inserts an abstract condition.
* EXTRA-INSTRUMENTATION-EXPRS a list of additional expressions to
  print at each instrumentation point. It should have the form '((expr
  . ((:type . \"typename\")))). These expressions can be generated
  with `instrumentation-exprs'.
"
  ;; only applies if repair mutation introduced abst_cond()
  (with-temporary-file (:pathname trace-file)
    (with-temporary-file (:pathname bin)
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
                        (handler-bind
                            ((phenome
                               (lambda (e)
                                 (note 3
                                       "Build failed applying condition ~a: ~a"
                                       best-cond (text e))
                                 (invoke-restart 'return-nil-for-bin))))
                          (phenome test-repair :bin bin))))
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

(defun instrumentation-exprs (obj point type)
  "Return a list of additional expressions to instrument during condition
synthesis. Finds all expressions of the given type which are in scope
at the repair point, and generates comparisons for all of them. These
expressions can be passed as EXTRA-INSTRUMENTATION-EXPRS to
`synthesize-condition'.

* OBJ software object
* POINT repair point for which to generate instrumentation
* TYPE type description alist (:types :array :pointer :compare)
       Where :compare is a format template for the comparison
       expressions, and the other fields are matched against
       the type DB.
"
  (let* ((type-hash
          (some->> (find-if {types-equal (funcall #'from-alist 'clang-type type)}
                            (hash-table-values (types obj)))
                   (type-hash)))
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
    (loop for (a . rest) in (maplist #'identity exprs)
       appending
         (loop for b in rest
            collecting
              (cons (format nil (aget :compare type)
                            (source-text a)
                            (source-text b))
                    (make-instance 'ct+
                      :type (make-instance 'clang-type :qual "int")))))))

(defun types-equal (type1 type2)
  "Return T if TYPE1 represents the same type as TYPE2.
Return NIL otherwise."
  (and (string= (type-name type1) (type-name type2))
       (string= (type-array type1) (type-array type2))
       (eq (type-pointer type1) (type-pointer type2))))
