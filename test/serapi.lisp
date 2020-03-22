;;;; serapi.lisp --- Coq SerAPI interaction.
(defpackage :software-evolution-library/test/serapi
  (:nicknames :sel/test/serapi)
  (:use
   :gt/full
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   :software-evolution-library
   :software-evolution-library/components/serapi-io
   :software-evolution-library/software/coq)
  (:export :test-serapi
           :serapi
           :serapi-available-p
           :coq-test-dir))
(in-package :software-evolution-library/test/serapi)
(in-readtable :serapi-readtable)

(defun serapi-available-p ()
  (set-serapi-paths)
  (zerop (nth-value 2 (shell "which ~a" *sertop-path*))))

(setf *serapi-timeout* 10)

(define-constant +coq-test-dir+ (append +etc-dir+ (list "coq"))
  :test #'equalp
  :documentation "Path to Coq test examples.")

(defun coq-test-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +coq-test-dir+))

(defsuite test-serapi "Coq SerAPI interaction." (serapi-available-p))

(defixture serapi
  (:setup (sleep 0.1)
          (setf *serapi-process* (make-serapi))
          (handler-bind ((serapi-timeout-error
                          (lambda (c)
                            (declare (ignorable c))
                            (invoke-restart 'use-empty-response))))
            ;; read two-line prologue
            (sel/cp/serapi-io::read-with-timeout *serapi-process* 2 0.1)
            (sel/cp/serapi-io::read-with-timeout *serapi-process* 2 0.1)))
  (:teardown
   (kill-serapi *serapi-process*)
   (setf *serapi-process* nil)))

(deftest can-start-end-serapi ()
  (is *sertop-path*)
  (let ((serapi (make-serapi)))
    (is serapi)
    (is (serapi-process-alive-p serapi))
    (kill-serapi serapi)
    (sleep 0.1)
    (is (not (serapi-process-alive-p serapi)))))

(deftest (with-serapi-creates-new-process :long-running) ()
  (is (not *serapi-process*))
  (with-serapi ()
    ;; locally binds `*serapi-process*'
    (write-to-serapi *serapi-process*
                     #!`((Test1 (Query () (Vernac "Print nat.")))))
    (is (member (mapcar {intern _ :sel/cp/serapi-io}
                        (list "Answer" "Test1" "Ack"))
                (read-serapi-response *serapi-process*)
                :test #'equal)))
  ;; `*serapi-process*' goes out of scope at end
  (is (not *serapi-process*)))

(deftest (with-serapi-can-capture-process :long-running) ()
  (with-fixture serapi
    (is (serapi-process-alive-p))
    (let ((serproc (make-serapi)))
      ;; serproc is a different process than *serapi-process*
      (is (not (eq serproc *serapi-process*)))
      (with-serapi (serproc)
        ;; *serapi-process is rebound to serproc inside with-serapi
        (is (eq serproc *serapi-process*))
        (write-to-serapi *serapi-process*
                         #!'((Test1 (Query () (Vernac "Print nat.")))))
        ;; writing to *serapi-process* also writes to serproc
        (is (member (mapcar {intern _ :sel/cp/serapi-io}
                            (list "Answer" "Test1" "Ack"))
                    (read-serapi-response serproc)
                    :test #'equal)))
      ;; serproc isn't killed after with-serapi ends
      (is (serapi-process-alive-p serproc))
      (kill-serapi serproc))
    ;; *serapi-process isn't killed after with-serapi ends
    (is (serapi-process-alive-p))))

(deftest serapi-special-character-handling ()
  (let* ((src0 "[[\\\"expr\\\" ::== \\\"coords\\\" \\\\n || \\\"coords\\\" \\\\n \\\"expr\\\" <{< fun x _ y => Row x y >}>]] ;;.")
         (sanitized0 (sanitize-process-string src0))
         ;; a /\\n b /\n c should differentiate \n from an extraneous n
         ;; preceded by backslashes
         (src1 "a /\\\\\\n b /\\\\n c")
         (sanitized1 (sanitize-process-string src1)))
    (is (equal sanitized0
               "[[\\\\\\\"expr\\\\\\\" ::== \\\\\\\"coords\\\\\\\" \\\\\\\\n || \\\\\\\"coords\\\\\\\" \\\\\\\\n \\\\\\\"expr\\\\\\\" <{< fun x _ y => Row x y >}>]] ;;."))
    (is (equal (sel/cp/serapi-io::unescape-coq-string sanitized0)
               "[[\\\"expr\\\" ::== \\\"coords\\\" \\\\n || \\\"coords\\\" \\\\n \\\"expr\\\" <{< fun x _ y => Row x y >}>]] ;;."))
    (is (equal sanitized0 (sel/cp/serapi-io::escape-coq-string sanitized0)))
    (is (equal sanitized1 "a /\\\\\\\\  b /\\\\\\\\n c"))
    (is (equal sanitized1 (sel/cp/serapi-io::escape-coq-string sanitized1)))
    (is (equal (sel/cp/serapi-io::unescape-coq-string sanitized1)
               "a /\\\\  b /\\\\n c"))))

(deftest can-read-write-serapi ()
  (with-fixture serapi
    ;; write basic "Print nat." query and read response
    (write-to-serapi *serapi-process*
                     #!`((TestQ (Query () (Vernac "Print nat.")))))
    (let ((response (read-serapi-response *serapi-process*)))
      (is response)
      (is (= 5 (length response)))
      (is (member (mapcar {intern _ :sel/cp/serapi-io}
                          (list "Answer" "TestQ" "Ack"))
                  response
                  :test #'equal))
      (is (member (mapcar {intern _ :sel/cp/serapi-io}
                          (list "Answer" "TestQ" "Completed"))
                  response
                  :test #'equal)))))

(deftest can-run-coq-vernacular ()
  (with-fixture serapi
    (let ((vernac "Print nat."))
      (write-to-serapi *serapi-process*
                       #!`((TestQ (Query () (Vernac ,VERNAC)))))
      (let ((resp1 (read-serapi-response *serapi-process*))
            (resp2 (run-coq-vernacular vernac :qtag #!'TestQ)))
        (is (equal resp1 resp2))))))

(deftest serapi-is-type-works ()
  (with-fixture serapi
    (let* ((resp (run-coq-vernacular "Print nat." :qtag #!'TestQ))
           ;; (Answer TestQ Ack)
           (ack (nth 0 resp))
           ;; (Feedback (id 1) (route 0) (contents Processed))
           (feedback (nth 1 resp))
           (answer-sym (intern "Answer" :sel/cp/serapi-io))
           (feedback-sym (intern "Feedback" :sel/cp/serapi-io)))
      (is (sel/cp/serapi-io::is-type answer-sym ack))
      (is (sel/cp/serapi-io::is-type feedback-sym feedback))
      (is (not (sel/cp/serapi-io::is-type answer-sym feedback)))
      (is (not (sel/cp/serapi-io::is-type feedback-sym ack))))))

(deftest serapi-feedback-parsing-works ()
  (with-fixture serapi
    (let* ((resp (run-coq-vernacular "Print nat." :qtag #!'TestQ))
           (ack (nth 0 resp))
           (feedback (nth 1 resp)))
      (is (not (sel/cp/serapi-io::feedback-id ack)))
      (is (not (sel/cp/serapi-io::feedback-route ack)))
      (is (not (sel/cp/serapi-io::feedback-contents ack)))
      (is (eql 1 (sel/cp/serapi-io::feedback-id feedback)))
      (is (eql 0 (sel/cp/serapi-io::feedback-route feedback)))
      (is (eql (intern "Processed" :sel/cp/serapi-io)
               (sel/cp/serapi-io::feedback-contents feedback))))))

(deftest serapi-message-content-works ()
  (let ((*package* (find-package 'sel/cp/serapi-io)))
    (let ((resp1
           (list (intern "Feedback")
                 (list (list (intern "id") 1)
                       (list (intern "route") 0)
                       (list (intern "contents")
                             (list (intern "Message")
                                   (intern "Notice")
                                   ()
                                   (list (intern "Some")
                                         (list (intern "AST")
                                               (list (intern "tree")))
                                         (intern "here")))))))
          (resp2 (mapcar #'intern (list "Answer" "TestQ" "Ack"))))
      (is (equal (list (intern "Some")
                       (list (intern "AST")
                             (list (intern "tree")))
                       (intern "here"))
                 (sel/cp/serapi-io::message-content
                  (sel/cp/serapi-io::feedback-contents resp1))))
      (is (not (sel/cp/serapi-io::message-content
                (sel/cp/serapi-io::feedback-contents resp2)))))))

(deftest serapi-message-level-works ()
  (with-fixture serapi
    (let* ((resp (run-coq-vernacular "Print nat." :qtag #!'TestQ))
           ;; (Answer TestQ Ack)
           (ack (nth 0 resp))
           ;; (Feedback (id 1) (route 0) (contents (Message Notice () ...)))
           (message (nth 2 resp)))
      (is (eql (intern "Notice" :sel/cp/serapi-io)
               (sel/cp/serapi-io::message-level
                (sel/cp/serapi-io::feedback-contents message))))
      (is (not (sel/cp/serapi-io::message-level
                (sel/cp/serapi-io::feedback-contents ack)))))))

(deftest (serapi-answer-parsing-works :long-running) ()
  (with-fixture serapi
    (let ((ids (add-coq-string "Definition test := true.")))
      (write-to-serapi *serapi-process*
                       #!`((TestQ (Query () (Ast ,(FIRST IDS))))))
      (let* ((resp1 (mapcar #'tag-loc-info
                            (read-serapi-response *serapi-process*)))
             ;; (Answer TestQ Ack)
             (ack (nth 0 resp1))
             ;; (Answer TestQ (ObjList ((CoqAst ...))))
             (objlist-ast (nth 1 resp1))
             (resp2 (run-coq-vernacular "Check test." :qtag #!'TestC))
             ;; (Feedback (id 2) (route 0) (contents Processed))
             (feedback (nth 1 resp2))
             (none (write-to-serapi *serapi-process*
                                    #!`((TestP (Query ((pp ((pp_format PpStr))))
                                                      (Ast ,(FIRST IDS)))))))
             (resp3 (read-serapi-response *serapi-process*))
             ;; (Answer TestP (ObjList ((CoqString Definition test := true.))))
             (objlist-string (nth 1 resp3)))
        (declare (ignorable none))
        ;; verify answer-content
        (is (equal (list (lastcar ack) (lastcar objlist-ast) nil
                         (lastcar objlist-string))
                   (mapcar #'sel/cp/serapi-io::answer-content
                           (list ack objlist-ast feedback objlist-string))))
        ;; verify answer-string
        (is (equal (list nil nil nil "Definition test := true.")
                   (mapcar [#'sel/cp/serapi-io::answer-string
                            #' sel/cp/serapi-io::answer-content]
                           (list ack objlist-ast feedback objlist-string))))
        ;; verify answer-ast is nil for non-AST answers
        (is (equal (list nil nil nil)
                   (mapcar [#'sel/cp/serapi-io::answer-ast
                            #'sel/cp/serapi-io::answer-content]
                           (list ack feedback objlist-string))))
        ;; verify answer-ast is a VernacDefinition
        (is (sel/cp/serapi-io::is-type
             (intern "VernacDefinition" :sel/cp/serapi-io)
             (nth 1 (sel/cp/serapi-io::answer-ast
                     (sel/cp/serapi-io::answer-content objlist-ast)))))))))

(deftest serapi-end-of-response-parsing-works ()
  (let ((resp1 (mapcar {intern _ :sel/cp/serapi-io}
                       (list "Answer" "TestQ" "Completed")))
        (resp2 (list (intern "Sexplib.Conv.Of_sexp_error" :sel/cp/serapi-io)
                     (list (intern "Failure" :sel/cp/serapi-io)
                           "Failure message")
                     (intern "etc" :sel/cp/serapi-io)))
        (resp3 (mapcar {intern _ :sel/cp/serapi-io}
                       (list "Answer" "TestQ" "Ack"))))
    (is (equal (list t t nil)
               (mapcar #'is-terminating (list resp1 resp2 resp3))))
    (is (equal (list nil t nil)
               (mapcar #'is-error (list resp1 resp2 resp3))))))

(deftest (can-add-and-lookup-coq-string :long-running) ()
  (with-fixture serapi
    (let* ((add-str "Inductive test :=   | T1 : test   | T2 : test.")
           (id (add-coq-string add-str)))
      (is (= 1 (length id)))
      (is (integerp (first id)))
      (let ((lookup-str (lookup-coq-string (first id))))
        (is (equal add-str lookup-str))))))

(deftest (can-convert-coq-ast-to-string :long-running) ()
  (with-fixture serapi
    (let* ((add-str "Inductive test :=   | T1 : test   | T2 : test.")
           (id (add-coq-string add-str)))
      (is (= 1 (length id)))
      (is (integerp (first id)))
      (let* ((lookup-ast (lookup-coq-ast (first id)))
             (ast-str (lookup-coq-string lookup-ast)))
        (is (equal add-str ast-str))))))

(deftest (can-lookup-coq-pretty-printed-repr-1 :long-running) ()
  (with-fixture serapi
    (let ((add-str "Inductive test :=   | T1 : test   | T2 : test."))
      (add-coq-string add-str)
      (write-to-serapi *serapi-process* #!`((Query () (Vernac "Print test."))))
      (let ((resp1 (read-serapi-response *serapi-process*))
            (resp2 (lookup-coq-pp "test")))
        (is (equal resp2 (coq-message-contents resp1)))
        (is (some {eql (intern "Notice" :sel/cp/serapi-io) }
                  (coq-message-levels resp1)))))))

(deftest (can-load-coq-file :long-running) ()
  (with-fixture serapi
    (let ((ids (load-coq-file (coq-test-dir "NatBinop.v"))))
      (is (equal '(2 3 4) ids)))))

(deftest can-tokenize-coq-types ()
  (let ((test1 "test1 : nat -> bool")
        (test2 "test2 : (bool -> (bool -> bool)) -> (bool -> nat) -> nat)")
        (test3 "test3 : a -> ((b -> (c -> d) -> e) -> f) -> g"))
    (is (equal (tokenize-coq-type test1)
               '("test1" :COLON "nat" :-> "bool")))
    (is (equal (tokenize-coq-type test2)
               '("test2" :COLON ("bool" :-> ("bool" :-> "bool")) :->
                 ("bool" :-> "nat") :-> "nat")))
    (is (equal (tokenize-coq-type test3)
               '("test3" :COLON "a" :->
                 (("b" :-> ("c" :-> "d") :-> "e") :-> "f")
                 :-> "g")))))

(deftest (can-lookup-coq-types :long-running) ()
  (with-fixture serapi
    ;; Look up some built-in Coq types
    (is (equal (check-coq-type "True") '("True" :COLON "Prop")))
    (is (equal (check-coq-type "true") '("true" :COLON "bool")))
    (is (equal (check-coq-type "nat")  '("nat" :COLON "Set")))
    (is (equal (check-coq-type "7")    '("7" :COLON "nat")))
    (is (equal (check-coq-type "negb") '("negb" :COLON "bool" :-> "bool")))
    (is (equal (check-coq-type "plus")
               '("Nat.add" :COLON "nat" :-> "nat" :-> "nat")))))

(deftest (can-search-coq-types :long-running) ()
  ;; NOTE: may need to change if Coq version or default load libraries change.
  (with-fixture serapi
    (let* ((fns (search-coq-type "nat -> nat -> nat -> nat"))
           (fn-names (mapcar #'car fns)))
      (is (= 3 (length fns)))
      (is (every {member _ '("Nat.log2_iter" "Nat.sqrt_iter" "Nat.bitwise")
                         :test #'equal}
                 fn-names)))))

(deftest (can-create-coq-expressions-1 :long-running) ()
  (with-fixture serapi
    ;; Nat.add 5 3
    (let ((e-ast1 (make-coq-application (make-coq-var-reference #!'plus)
                                        (make-coq-integer 5)
                                        (make-coq-integer 3)))
          (e-ast2 (make-coq-match
                   #!'RegularStyle
                   (make-coq-application (make-coq-var-reference #!'S)
                                         (make-coq-var-reference #!'O))
                   (make-coq-case-pattern "CPatAtom" #!'O)
                   (make-coq-integer 0)
                   (make-coq-case-pattern
                    "CPatCstr"
                    (make-coq-ident #!'S)
                    (make-coq-case-pattern "CPatAtom" #!'_))
                   (make-coq-integer 1))))
      (let ((e-str1 (lookup-coq-string e-ast1 :input-format #!'CoqExpr))
            (e-str2 (lookup-coq-string e-ast2 :input-format #!'CoqExpr)))
        (is (equal e-str1 "plus 5 3"))
        (is (equal e-str2 "match S O with | O => 0 | S _ => 1 end"))))))
