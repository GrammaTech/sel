;;;; instrumentation.lisp --- Instrumentation tests.
(defpackage :software-evolution-library/test/instrumentation
  (:nicknames :sel/test/instrumentation)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :software-evolution-library/stefil-plus
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang
   :software-evolution-library/components/traceable
   :software-evolution-library/components/instrument
   :software-evolution-library/components/clang-instrument)
  (:import-from :trace-db :read-binary-trace)
  (:shadowing-import-from :uiop/run-program :run-program)
  (:export :test-instrumentation))
(in-package :software-evolution-library/test/instrumentation)
(in-readtable :curry-compose-reader-macros)
(defsuite test-instrumentation
    "Tests for Clang instrumentation."
  (clang-available-p))

(defixture c-strings
  (:setup
   (setf *soft*
         (from-file (make-instance 'clang)
                    (strings-dir "c-strings.c"))))
  (:teardown
   (setf *soft* nil)))

(define-constant +shadow-dir+ (append +etc-dir+ (list "shadow"))
  :test #'equalp
  :documentation "Path to the shadow example.")

(defun shadow-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +shadow-dir+))

(defixture shadow-clang
  (:setup
   (setf *soft*
         (from-file (make-instance 'clang)
                    (shadow-dir "shadow.c"))))
  (:teardown
   (setf *soft* nil)))

(defun do-multi-threaded-instrument-clang-test (obj)
  (let ((st-instrumented
         (instrument (copy obj)
                     :functions
                     (list (lambda (instrumenter ast)
                             (var-instrument {get-vars-in-scope
                                              (software instrumenter)}
                                             instrumenter
                                             ast)))
                     :num-threads 1))
        (mt-instrumented
         (instrument (copy obj)
                     :functions
                     (list (lambda (instrumenter ast)
                             (var-instrument {get-vars-in-scope
                                              (software instrumenter)}
                                             instrumenter
                                             ast)))
                     :num-threads 4)))
    (is (equalp (mapcar #'ast-class (asts st-instrumented))
                (mapcar #'ast-class (asts mt-instrumented)))
        "`instrument` should yield the same ASTs regardless of the ~
         number of threads utilized.")

    (uninstrument st-instrumented :num-threads 1)
    (uninstrument mt-instrumented :num-threads 4)
    (is (equalp (mapcar #'ast-class (asts st-instrumented))
                (mapcar #'ast-class (asts mt-instrumented)))
        "`uninstrument` should yield the same ASTs regardless of the ~
         number of threads utilized.")))

(deftest (multi-threaded-clang-instrument-test :long-running) ()
  (with-fixture clang-project
    (do-multi-threaded-instrument-clang-test *project*))
  (with-fixture grep-bear-project
    (do-multi-threaded-instrument-clang-test *project*)))

(defun count-traceable (obj)
  "Return a count of full statements parented by compound statements"
  (count-if {can-be-made-traceable-p obj} (asts obj)))

(defun get-gcd-trace (bin)
  (with-temporary-file (:pathname trace-file)
    (let ((errno (nth-value 2 (run-program (format nil "~a 4 8 2>~a"
                                                   bin trace-file)))))
      (is (zerop errno))
      (let ((trace (read-binary-trace trace-file)))
        (is (listp trace))
        trace))))

(deftest instrumented-p-test ()
  (with-fixture gcd-clang
    (is (not (instrumented-p *gcd*)))
    (is (instrumented-p (instrument (copy *gcd*))))))

(deftest (instrumentation-insertion-test :long-running) ()
  (with-fixture gcd-clang
    (let ((instrumented (instrument (copy *gcd*) :trace-file :stderr)))
      ;; Do we insert the right number of printf statements?
      (is (<= (count-traceable *gcd*)
              (count-traceable instrumented)))
      ;; Instrumented compiles and runs.
      (with-temporary-file (:pathname bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (every {aget :c} trace))
          (is (= 15 (length trace))))))))

(deftest (instrumentation-insertion-w-filter-test :long-running) ()
  (with-fixture gcd-clang
    (let ((instrumented (instrument (copy *gcd*)
                                    :filter (lambda (obj ast)
                                              (eq 92 (index-of-ast obj ast)))
                                    :trace-file :stderr)))
      ;; Instrumented compiles and runs.
      (with-temporary-file (:pathname bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (every {aget :c} trace))
          (is (= 1 (length trace))))))))

(deftest (instrumentation-insertion-w-function-exit-test :long-running) ()
  (with-fixture gcd-clang
    (let ((instrumented (instrument (copy *gcd*)
                                    :instrument-exit t
                                    :trace-file :stderr)))
      ;; Do we insert the right number of printf statements?
      (is (<= (count-traceable *gcd*)
              (count-traceable instrumented)))

      ;; Is function exit instrumented?
      (is (stmt-with-text
           instrumented
           (format
            nil
            "__write_trace_id(__sel_trace_file, __sel_trace_file_lock, ~du)"
            (position (function-body *gcd* (first (functions *gcd*)))
                      (asts *gcd*)
                      :test #'equalp))
           :no-error t))

      ;; Instrumented compiles and runs.
      (with-temporary-file (:pathname bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (every {aget :c} trace))
          (is (= 16 (length trace))))))))

(deftest (instrumentation-insertion-w-points-test :long-running) ()
  (with-fixture gcd-clang
    (let ((instrumented
           (handler-bind ((warning #'muffle-warning))
             (instrument (copy *gcd*)
                         :points
                         (iter (for ast in (stmt-asts *gcd*))
                               (for i upfrom 0)
                               (collect (cons ast (if (evenp i) '(1 2) '(3 4) ))))
                         :trace-file :stderr))))
      (is (scan (quote-meta-chars "__write_trace_aux(__sel_trace_file")
                (genome-string instrumented))
          "We find code to print auxiliary values in the instrumented source.")
      ;; Instrumented compiles and runs.
      (with-temporary-file (:pathname bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (every [«or {equalp #(1 2)} {equalp #(3 4)}»
                      {aget :aux}]
                     trace)))))))

(deftest (instrumentation-insertion-w-trace-file-test :long-running) ()
  (with-fixture gcd-clang
    (with-temporary-file (:pathname trace)
      (with-temporary-file (:pathname bin)
        (let ((instrumented
               (instrument (copy *gcd*) :trace-file trace)))
          (is (scan (quote-meta-chars trace) (genome-string instrumented)))
          (is (zerop (nth-value 1 (ignore-phenome-errors
                                   (phenome instrumented :bin bin)))))
          (is (probe-file bin))
          (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
            (declare (ignorable stdout stderr))
            (is (zerop errno))
            (is (probe-file trace))))))))

(deftest (instrumentation-handles-missing-curlies-test :long-running) ()
  (with-fixture gcd-wo-curlies-clang
    (let ((instrumented (instrument (copy *gcd*) :trace-file :stderr)))
      ;; Ensure we were able to instrument an else branch w/o curlies.
      (let* ((else-counter (index-of-ast *gcd*
                                         (stmt-with-text *gcd* "b = b - a;")))
             (matcher (format nil "__write_trace_id\\(.*~du\\)"
                              else-counter)))
        (is (scan matcher (genome instrumented)))
        ;; The next line (after flushing) should be the else branch.
        (let ((location (position-if {scan matcher} (lines instrumented))))
          (is (scan (quote-meta-chars "b = b - a;")
                    (nth location (lines instrumented))))))
      ;; Finally, lets be sure we still compile.
      (with-temporary-file (:pathname bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (is (not (emptyp (get-gcd-trace bin))))))))

(deftest
    (instrumentation-insertion-w-points-and-added-blocks-test :long-running) ()
  (with-fixture gcd-wo-curlies-clang
    (let* ((cookie 1234)
           (instrumented
            (instrument (copy *gcd*)
                        :points
                        `((,(stmt-with-text *gcd* "b - a") ,cookie))
                        :trace-file :stderr)))
      ;; Instrumented program holds the value 1234.
      (is (scan (quote-meta-chars (format nil "~d" cookie))
                (genome-string instrumented))
          "The point trace value ~S appears in the instrumented program text."
          cookie)
      ;; Instrumented compiles and runs.
      (with-temporary-file (:pathname bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (let ((trace (get-gcd-trace bin)))
          (is (find-if [{equalp `#(,cookie)} {aget :aux}]
                       trace)
              "The point trace value ~S appears in the trace" cookie))))))

(deftest (instrumentation-after-insertion-mutation-test :long-running) ()
  "Ensure after applying an insert mutation, the instrumented software
prints unique counters in the trace"
  (with-fixture gcd-clang
    (let* ((*matching-free-var-retains-name-bias* 1.0)
           (variant (copy *gcd*))
           (instrumented (copy *gcd*))
           (stmt1 (stmt-with-text variant "a = atoi(argv[1]);"))
           (stmt2 (stmt-with-text variant "a = atoi(argv[1]);")))
      (apply-mutation variant
                      `(clang-insert (:stmt1 . ,stmt1) (:stmt2 . ,stmt2)))
      (instrument instrumented)

      (with-temporary-file (:pathname bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
          (declare (ignorable stdout stderr))
          (is (zerop errno))
          (is (not (equal (find-if [{string= "a = atoi(argv[1]);"}
                                    #'source-text]
                                   (asts variant)
                                   :from-end nil)
                          (find-if [{string= "a = atoi(argv[1]);"}
                                    #'source-text]
                                   (asts variant)
                                   :from-end t)))
              "a = atoi(argv[1]) was not inserted into the genome")
          (is (search
               (format
                nil
                "__write_trace_id(__sel_trace_file, __sel_trace_file_lock, ~du)"
                (nest (index-of-ast variant)
                      (find-if [{string= "a = atoi(argv[1]);"} #'source-text]
                               (asts variant)
                               :from-end nil)))
               (genome instrumented))
              "instrumentation was not added for the inserted statement")
          (is (search
               (format
                nil
                "__write_trace_id(__sel_trace_file, __sel_trace_file_lock, ~du)"
                (nest (index-of-ast variant)
                      (find-if [{string= "a = atoi(argv[1]);"} #'source-text]
                               (asts variant)
                               :from-end t)))
               (genome instrumented))
              "instrumentation was not added for the original statement"))))))

(deftest (instrumentation-print-unbound-vars :long-running) ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *gcd* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-unbound-vals
                                           (software instrumenter)}
                                          instrumenter
                                          ast)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_variables(__sel_trace_file")
              (genome-string *gcd*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temporary-file (:pathname bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *gcd* :bin bin))))
          "Successfully compiled instrumented GCD.")
      (let ((trace (get-gcd-trace bin)))
        (is (= (length trace) (count-if {assoc :c} trace))
            "Counter in every trace element.")
        (is (> (count-if {assoc :scopes} trace) 0)
            "Variable list in some trace elements.")
        (is (> (length trace) (count-if {aget :scopes} trace))
            "Variable list not populated in every trace element.")))))

(deftest (instrumentation-print-in-scope-vars :long-running) ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *gcd* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                           (software instrumenter)}
                                          instrumenter
                                          ast)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_variables(__sel_trace_file")
              (genome-string *gcd*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temporary-file (:pathname bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *gcd* :bin bin))))
          "Successfully compiled instrumented GCD.")
      (let ((trace (get-gcd-trace bin)))
        (is (listp trace) "We got a trace.")
        (is (= (length trace) (count-if {assoc :c} trace))
            "Counter in every trace element.")
        (is (= (length trace) (count-if {assoc :SCOPES} trace))
            "Variable list in every trace element.")
        (is (= (length trace) (count-if {aget :SCOPES} trace))
            "Variable list populated in every trace element.")
        (is (not (null (mappend {aget :SCOPES} trace)))
            "Variable list not always empty.")))))

(defun trace-var-equal (var-name value scopes)
  (let ((var (find-if [{string= var-name} {aref _ 0}] scopes)))
    (or (null var)
        (equal (aref var 2) value))))

(deftest (instrumentation-print-strings :long-running) ()
  (with-fixture c-strings
    (handler-bind ((warning #'muffle-warning))
      (instrument *soft* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                           (software instrumenter)}
                                          instrumenter ast
                                          :print-strings t)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_blobs(__sel_trace_file")
              (genome-string *soft*))
        "We find code to print strings in the instrumented source.")
    (with-temporary-file (:pathname bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *soft* :bin bin))))
          "Successfully compiled instrumented SOFT.")
      (let ((trace (get-gcd-trace bin)))
        (is (listp trace) "We got a trace.")
        (is (every [{trace-var-equal "test" "test"} {aget :scopes}]
                   trace)
            "Variable 'test' always has expected value.")
        (is (every [{trace-var-equal "x" "4"} {aget :scopes}]
                   trace)
            "Variable 'x' always has expected value.")))))

(deftest (instrumentation-print-cpp-strings :long-running) ()
  (with-fixture cpp-strings
    (handler-bind ((warning #'muffle-warning))
      (instrument *soft* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                           (software instrumenter)}
                                          instrumenter ast
                                          :print-strings t)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_blobs(__sel_trace_file")
              (genome-string *soft*))
        "We find code to print strings in the instrumented source.")
    (with-temporary-file (:pathname bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *soft* :bin bin))))
          "Successfully compiled instrumented SOFT.")
      (let ((trace (get-gcd-trace bin)))
        (is (listp trace) "We got a trace.")
        (is (every [{trace-var-equal "x" "4"} {aget :scopes}]
                   trace)
            "Variable 'x' always has expected value.")))))

(deftest (instrumentation-print-vars-after :long-running) ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *gcd* :functions-after
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                           (software instrumenter)}
                                          instrumenter
                                          ast)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_variables(__sel_trace_file")
              (genome-string *gcd*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temporary-file (:pathname bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *gcd* :bin bin))))
          "Successfully compiled instrumented GCD.")
      (let ((trace (get-gcd-trace bin)))
        (is (listp trace) "We got a trace.")
        (is (not (null (mappend {aget :SCOPES} trace)))
            "Variable list not always empty.")))))

(deftest (instrumentation-print-vars-handles-shadowing :long-running) ()
  (with-fixture shadow-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *soft* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                           (software instrumenter)}
                                          instrumenter
                                          ast)))
                  :trace-file :stderr))
    (is (scan (quote-meta-chars "__write_trace_variables(__sel_trace_file")
              (genome-string *soft*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temporary-file (:pathname bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *soft* :bin bin))))
          "Successfully compiled instrumented program.")
      (let ((trace (get-gcd-trace bin)))
        (without-compiler-notes
          (is (every [{eql 1} #'length {aget :scopes}]
                     trace)
              "No duplicate variables."))

        (is (every [«or {equalp '(#("x" "int" 1 nil))}
                        {equalp '(#("x" "short" 0 nil))}»
                    {aget :scopes}]
                   trace)
            "Variables have correct type and value.")))))

(deftest instrumentation-handles-binary-search ()
  (with-fixture binary-search-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *binary-search* :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-unbound-vals
                                           (software instrumenter)}
                                          instrumenter
                                          ast)))))))

(deftest (can-instrument-clang-project :long-running) ()
  (with-fixture clang-project
    (instrument *project* :functions
                (list (lambda (instrumenter ast)
                        (var-instrument {get-unbound-vals
                                         (software instrumenter)}
                                        instrumenter
                                        ast)))
                :trace-file :stderr)
    (with-temporary-file (:pathname bin)
      (is (zerop (nth-value 1 (ignore-phenome-errors
                               (phenome *project* :bin bin))))
          "Successfully compiled instrumented project.")
      (with-temporary-file (:pathname trace-file)
        (let ((errno (nth-value 2 (run-program (format nil "~a 2>~a"
                                                       bin trace-file)))))
          (is (zerop errno))
          (let ((trace (read-binary-trace trace-file)))
            (is (listp trace))
            (is (not (emptyp trace)))
            (is (every «and {aget :c} {aget :f}» trace))
            (is (some «and [{equalp '(#("x" "int" 0 nil))} {aget :scopes}]
                           [{eq 0} {aget :f}]»
                      trace))
            (is (some «and [{equalp '(#("y" "int" 1 nil))} {aget :scopes}]
                           [{eq 1} {aget :f}]»
                      trace))))))))

(deftest instrumentation-skips-nameless-variable ()
  (handler-bind ((mutate ; Ignore obvious problem in following genome.
                  (lambda (e)
                    (if (find-restart 'keep-partial-asts)
                        (invoke-restart 'keep-partial-asts)
                        (error e)))))
    (let ((soft (make-instance 'clang
                 :genome "int test(int) { return 1; }")))
      (instrument soft :functions
                  (list (lambda (instrumenter ast)
                          (var-instrument {get-vars-in-scope
                                           (software instrumenter)}
                                          instrumenter
                                          ast))))
      (is (not (scan (quote-meta-chars
                      "__write_trace_variables(__sel_trace_file")
                     (genome soft)))
          "No code to print variables in the instrumented source."))))

(deftest (instrumentation-preserves-aux-data :long-running) ()
  (with-fixture gcd-clang
    (let* ((stmt (stmt-starting-with-text *gcd* "if (a == 0)"))
           (index (index-of-ast *gcd* stmt)))
      (apply-mutation *gcd*
                      `(clang-replace
                        (:stmt1 . ,stmt)
                        (:value1 . ,(copy stmt :aux-data '((:foo . t))))))

      (instrument *gcd* :functions
                  (list (lambda (instrumenter ast)
                          (when (aget :foo (ast-aux-data ast))
                            (var-instrument {get-vars-in-scope
                                             (software instrumenter)}
                                            instrumenter
                                            ast))))
                  :trace-file :stderr)

      (with-temporary-file (:pathname bin)
        (is (zerop (nth-value 1 (ignore-phenome-errors
                                 (phenome *gcd* :bin bin))))
            "Successfully compiled instrumented GCD.")
        (is (member '(:foo . t)
                    (mappend #'ast-aux-data (asts *gcd*))
                    :test #'equalp))
        (let ((trace (get-gcd-trace bin)))
          (is (listp trace) "We got a trace.")
          (is (some {aget :scopes} trace))
          (is (every «or [#'not {aget :scopes}]
                         [{eq index} {aget :c}]»
                     trace)))))))

(deftest (uninstrument-instrument-is-identity :long-running) ()
  (with-fixture gcd-clang
    (let ((orig (copy *gcd*))
          (instrumented (copy *gcd*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                             (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome orig) (genome (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture gcd-wo-curlies-clang
    (let ((orig (copy *gcd*))
          (instrumented (copy *gcd*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                             (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome orig) (genome (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture clang-project
    (let ((orig (copy *project*))
          (instrumented (copy *project*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                             (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome orig) (genome (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture shadow-clang
    (let ((orig (copy *soft*))
          (instrumented (copy *soft*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                             (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome orig) (genome (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture binary-search-clang
    (let ((orig (copy *binary-search*))
          (instrumented (copy *binary-search*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                             (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome orig) (genome (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture c-strings
    (let ((orig (copy *soft*))
          (instrumented (copy *soft*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                             (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome orig) (genome (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity")))
  (with-fixture cpp-strings
    (let ((orig (copy *soft*))
          (instrumented (copy *soft*)))
      (handler-bind ((warning #'muffle-warning))
        (instrument instrumented :functions
                    (list (lambda (instrumenter ast)
                            (var-instrument {get-vars-in-scope
                                             (software instrumenter)}
                                            instrumenter
                                            ast)))))
      (is (equal (genome orig) (genome (uninstrument instrumented)))
          "(uninstrument (instrument obj ...)) is not an identity"))))
