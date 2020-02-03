;;; test.lisp --- tests for the `software-evolution-library' package
(defpackage :software-evolution-library/test
  (:nicknames :sel/test)
  (:use :common-lisp
        :software-evolution-library/stefil-plus
        #+gt :testbot
        :software-evolution-library/test/adaptive-mutation.lisp
        :software-evolution-library/test/asm-super-mutant.lisp
        :software-evolution-library/test/asm.lisp
        :software-evolution-library/test/bear.lisp
        :software-evolution-library/test/cl.lisp
        :software-evolution-library/test/clang-ancestry.lisp
        :software-evolution-library/test/clang-crossover.lisp
        :software-evolution-library/test/clang-expression.lisp
        :software-evolution-library/test/clang-instrumentation.lisp
        :software-evolution-library/test/clang-mutations.lisp
        :software-evolution-library/test/clang-project.lisp
        :software-evolution-library/test/clang-scopes-and-types.lisp
        :software-evolution-library/test/clang-super-mutants.lisp
        :software-evolution-library/test/clang-syntactic-contexts.lisp
        :software-evolution-library/test/clang-tokenizer.lisp
        :software-evolution-library/test/clang-utility.lisp
        :software-evolution-library/test/clang-w-fodder.lisp
        :software-evolution-library/test/clang.lisp
        :software-evolution-library/test/command-line.lisp
        :software-evolution-library/test/condition-synthesis.lisp
        :software-evolution-library/test/conflict-ast.lisp
        :software-evolution-library/test/coq.lisp
        :software-evolution-library/test/cpp-scan.lisp
        :software-evolution-library/test/csurf-asm-ancestry.lisp
        :software-evolution-library/test/csurf-asm.lisp
        :software-evolution-library/test/database.lisp
        :software-evolution-library/test/declaration-type-databases.lisp
        :software-evolution-library/test/diff.lisp
        :software-evolution-library/test/elf.lisp
        :software-evolution-library/test/fix-compilation.lisp
        :software-evolution-library/test/instrumentation.lisp
        :software-evolution-library/test/java-instrumentation.lisp
        :software-evolution-library/test/java.lisp
        :software-evolution-library/test/javascript-project.lisp
        :software-evolution-library/test/javascript.lisp
        :software-evolution-library/test/misc-mutations.lisp
        :software-evolution-library/test/mutation-analysis.lisp
        :software-evolution-library/test/new-clang-debug.lisp
        :software-evolution-library/test/new-clang-round-trip.lisp
        :software-evolution-library/test/population.lisp
        :software-evolution-library/test/range-representation.lisp
        :software-evolution-library/test/rest.lisp
        :software-evolution-library/test/selection.lisp
        :software-evolution-library/test/serapi.lisp
        :software-evolution-library/test/sexp.lisp
        :software-evolution-library/test/style-features.lisp
        :software-evolution-library/test/task-runner.lisp
        :software-evolution-library/test/traceable.lisp
        :software-evolution-library/test/type-traces.lisp
        :software-evolution-library/test/utility.lisp)
  (:export :test :batch-test :testbot-test))
(in-package :software-evolution-library/test)
(named-readtables:in-readtable :sel-readtable)

#-gt
(progn
  ;;; External replacement for GT-specific test submission helpers
  (defvar *success* nil "Variable indicating test success or failure.")
  (defun batch-test (test project branch &optional args)
    "Run tests in 'batch' mode, printing results as a string."
    (declare (ignorable project branch args))

    (let* ((stefil::*test-progress-print-right-margin* (expt 2 20))
           (failures (coerce (stefil::failure-descriptions-of
                              (without-debugging (funcall test)))
                             'list)))
      (setf *success*
            (if failures
                (prog1 nil
                  (format *error-output* "FAILURES~%")
                  (mapc [{format *error-output* "  ~a~%"}
                         #'stefil::name-of
                         #'stefil::test-of
                         #'car #'stefil::test-context-backtrace-of]
                        failures))
                (prog1 t
                  (format *error-output* "SUCCESS~%")))))))

(defun run-batch (&rest a)
  (declare (ignorable a))
  #+ccl (setf ccl::*interactive-streams-initialized* nil)
  (batch-test #'test "SEL" +software-evolution-library-branch+))

(defun test ())



;;; ASM representation.
(defsuite asm "ASM representation." #+windows nil)


;;; ASM-SUPER-MUTANT representation.
(defsuite asm-super-mutant
    "ASM-SUPER-MUTANT representation."
  ;; Only run these tests if we found libpapi.so.
  *lib-papi*)

;;; Command Line tests
(defsuite cl "Command Line tool tests.")


;;; REST tests
#-windows
(defsuite rest "REST API tests.")


;;; CSURF-ASM representation.
#-windows
(progn

  (defsuite csurf-asm "CSURF-ASM representation."))


;;;
;;; Test the TASK-RUNNER modules.
;;;
(defsuite task-runner "TASK-RUNNER tests.")


;;; ELF representation.
;;;
;;; NOTE: Currently failing because not populating .text section.
;;;
(defsuite elf "ELF representation." :silent)

(defsuite clang "Clang representation." (clang-mutate-available-p))


;;; Detailed clang mutation tests
;;;
;;; These all run the entire mutate method, rather that just
;;; apply-mutation, adjusting the good and bad picks to get
;;; predictable results. And they check the results of each mutation
;;; in as much detail as possible.
(defsuite clang-mutations "Detailed clang mutation tests."

  
;;;; Clang w/ mutation fodder representation.
  (defsuite clang-w-fodder "Clang w/ mutation fodder representation."

  
;;;; Clang utility methods.
  (defsuite clang-utility "Clang utility methods."

  (defsuite java "JAVA representation."

  (defsuite javascript "Javascript representation." (acorn-available-p))

(defsuite javascript-project "Javascript project."

  
;;;; Range representation.
  (defsuite range-representation "Range representation.")


;;;; Mutation analysis and statistics collection tests.
(defsuite mutation-analysis
    "Mutation analysis and statistics collection tests."

  
;;;; Ancestry tests.
  (defsuite clang-ancestry "Ancestry tests."

  (defsuite csurf-asm-ancestry "Ancestry tests.")


;;;; Diff tests.
(defsuite diff "Diff tests.")


;;;; Population tests.
(defsuite population "Population tests."

  
;;;; Fix compilation tests.
  (defsuite fix-compilation "Fix compilation tests."

  
;;;; Crossover tests.
  (defsuite clang-crossover "Crossover tests."

  
;;;; Misc. mutation tests.
  (defsuite misc-mutations "Misc. mutation tests."

  
;;;; Adaptive-mutation tests.
  (defsuite adaptive-mutation "Adaptive-mutation tests.")


;;;; Database tests.
(defsuite database "Database tests.")


;;;; Instrumentation tests.
(defsuite instrumentation "Instrumentation tests."

  (defsuite clang-instrumentation "Tests for Clang instrumentation."

  (defsuite java-instrumentation "Tests for Java instrumentation."
    ;; (zerop (nth-value 2 (shell "which java-mutator")))
    :silent)


;;;; Traceable tests.
(defsuite traceable "Traceable tests."

  
;;;; Tests of declaration and type databases on clang objects.
  (defsuite declaration-type-databases
      "Tests of declaration and type databases on clang objects."

  
;;;; Lisp representation.
  (defsuite sexp "Sexp representation.")


;;;; Mutations and evaluation of clang expressions in Lisp form.
(defsuite clang-expression
    "Mutation and evaluation of clang expressions in Lisp form."

  
;;;; Utility tests.
  (defsuite utility "Utility tests.")


;;;; Command-line tests.
(defsuite command-line "Command line tests")


;;;; Project tests.
(defsuite clang-project "Project tests."

  
;;;; Tests that require bear.
  (defsuite bear "Clang representation."

  
  #-windows
  (progn
;;;; Condition synthesis tests.
    (defsuite condition-synthesis "Condition synthesis tests."

  
;;;; Selection tests.
  (defsuite selection "Selection tests.")


;;;; Style features tests.
(defsuite style-features "Style features tests."

  
;;;; Clang syntactic contexts.
  (defsuite clang-syntactic-contexts "Clang syntactic contexts."

  
;;;; Clang scope and type tests.
  (defsuite clang-scopes-and-types "Clang scope and type tests."
    (clang-mutate-available-p))


;;;; Types and traces tests.
(defsuite type-traces "Types and traces tests."

  
;;;; Clang tokenizer tests.
  (defsuite clang-tokenizer "Clang tokenizer tests."

  (defsuite clang-super-mutants "Super mutants of clang objects."

  (defsuite test-serapi "Coq SerAPI interaction." (serapi-available-p))


;;; Test Coq software objects
(defsuite test-coq "Coq software object tests." (serapi-available-p))



;;; conflict asts
(defsuite conflict-ast "Conflict ast tests.")

(defsuite cpp-scan "Tests of CPP-SCAN")
