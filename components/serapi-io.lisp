;;; serapi-io.lisp --- Serialization interface for Coq.
;;;
;;; @subsection Coq Module Organization
;;;
;;; The `coq' module is split into two layers. The lower layer, implemented in
;;; `serapi-io.lisp', is strictly responsible for serialization of Coq abstract
;;; syntax trees (ASTs). This is done with Coq SerAPI
;;; @url{https://github.com/ejgallego/coq-serapi}, specifically `sertop', which
;;; allows for querying of Coq documents and serializes ASTs to S-expressions.
;;;
;;; The `coq' and `coq-project' software objects, implemented in `coq.lisp' and
;;; `coq-project.lisp', respectively, are higher-level abstractions built on
;;; `serapi-io'. Ideally, clients should only have to construct software objects
;;; anduse API functions provided by `coq.lisp' or `coq-project.lisp' without
;;; having to worry about the lower-level functions.
;;;
;;; @subsection Setting up SerAPI
;;;
;;; In order to construct a Coq software object, coq-serapi must be installed.
;;; Refer to its GitHub project page for instructions. The variables
;;; `*sertop-path*' and `*sertop-args*' must then be set. This can be done
;;; either with `setf' or by setting the environment variables `SERAPI' and
;;; `COQLIB' and invoking `set-serapi-paths' to update the Lisp variables.
;;; `SERAPI' and `*sertop-path*' should point to the sertop executable.
;;;
;;; If you intend to use the Coq standard library, you will need to ensure that
;;; sertop is started with the `--coqlib' flag pointing to the coq/lib
;;; directory. This can be done either by setting the environment variable
;;; `COQLIB' to that path or by setting `*sertop-args*' to
;;; @code{(list ``--coqlib=/path/to/coq/lib/'')}. Other sertop parameters may
;;; also be added to `*sertop-args*'.
;;;
;;; @texi{serapi-io}
(defpackage :software-evolution-library/components/serapi-io
  (:nicknames :sel/components/serapi-io :sel/cp/serapi-io)
  (:use
   :gt/full
   :fare-quasiquote
   :software-evolution-library/software/sexp
   :software-evolution-library/utility/process
   :software-evolution-library/utility/debug)
  (:shadowing-import-from :fare-quasiquote :quasiquote :unquote
                          :unquote-splicing :unquote-nsplicing)
  (:shadowing-import-from :arrow-macros :-<>> :-<> :<>) ; FIXME: Remove.
  (:export :set-serapi-paths
           :insert-reset-point
           :reset-serapi-process
           :make-serapi
           :with-serapi
           :kill-serapi
           :write-to-serapi
           :sanitize-process-string
           :read-serapi-response
           :*sertop-path*
           :*sertop-args*
           :*serapi-timeout*
           :*serapi-process*
           :coq-message-contents
           :coq-message-levels
           :is-terminating
           :is-error
           :is-loc-info
           :coq-import-ast-p
           :coq-module-ast-p
           :coq-section-ast-p
           :coq-assumption-ast-p
           :coq-definition-ast-p
           :lookup-coq-pp
           :lookup-coq-string
           :add-coq-string
           :add-coq-lib
           :lookup-coq-ast
           :coq-ast-to-string
           :cancel-coq-asts
           :run-coq-vernacular
           :check-coq-type
           :search-coq-type
           :tokenize-coq-type
           :load-coq-file
           :serapi-readtable
           ;; Error handling
           :serapi-timeout-error
           :use-empty-response
           :retry-read
           :serapi-timeout
           :serapi-error
           :serapi-error-ast
           :serapi-backtrace
           :serapi-error-text
           :serapi-error-proc
           :cancel-ast
           :kill-serapi-process
           ;; Building Coq expressions
           :wrap-coq-constr-expr
           :make-coq-integer
           :make-coq-ident
           :make-coq-var-reference
           :make-coq-application
           :make-coq-name
           :make-coq-lname
           :make-coq-local-binder-exprs
           :make-coq-lambda
           :make-coq-if
           :make-coq-case-pattern
           :make-coq-match
           :make-coq-definition
           :coq-function-definition-p))
(in-package :software-evolution-library/components/serapi-io)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-readtable :serapi-readtable)
    ;; Ensure the two packages which compose the fare-quasiquote-extras
    ;; package are loaded before we use the fare-quasiquote readtable.
    (require :fare-quasiquote-readtable)
    (require :fare-quasiquote-optima)
    (defun read-preserving-case (stream char n)
      (declare (ignorable char) (ignorable n))
      (let ((*readtable* (copy-readtable nil)))
        (setf (readtable-case *readtable*) :preserve)
        (read stream t nil t)))
    (defreadtable :serapi-readtable
      ;; Must fuse FQ after SEL because otherwise the SBCL quasiquoter
      ;; clobbers the FQ quasiquoter. Alt. fix is for CCRM and SEL to
      ;; define a mixin readtable as done by FQ.
      (:fuse :curry-compose-reader-macros :fare-quasiquote-mixin)
      (:dispatch-macro-char #\# #\! #'read-preserving-case)))
  (in-readtable :serapi-readtable))

(defvar *sertop-path* "sertop" "Path to sertop program.")
(defvar *sertop-args* nil      "Arguments to sertop program.")

;;;; If threads don't have their own copy, they don't have a way to ensure that
;;;; the response they're reading corresponds to the query they wrote and that
;;;; they aren't consuming the response to a different thread's query.
(defvar *serapi-process* nil
  "Main SerAPI process. Ensure new threads each have their own copy or else.")

(defun set-serapi-paths ()
  "Function to set `*sertop-path*' and `*sertop-args*' automatically.
Uses SERAPI environment variable or \"which sertop.native\" to set
`*sertop-path*'. Uses COQLIB environment variable to add \"--coqlib\" flag
to `*sertop-args*'."
  (when-let ((sertop-env (getenv "SERAPI")))
    (setf *sertop-path* sertop-env))
  (when (and
         (not *sertop-path*)
         (zerop (nth-value 2 (shell "which sertop"))))
    (setf *sertop-path* "sertop"))
  (when (and
         (not *sertop-path*)
         (zerop (nth-value 2 (shell "which sertop.native"))))
    (setf *sertop-path* "sertop.native"))
  (when (getenv "COQLIB")
    (setf *sertop-args*
          (list (format nil "--coqlib=~a" (getenv "COQLIB")))))
  (when (getenv "SERTOP_ARGS")
    (setf *sertop-args*
          (append *sertop-args*
                  (split "\\s+" (getenv "SERTOP_ARGS"))))))

;;;; For timeout, we have a longer *serapi-timeout* - the max time (in seconds)
;;;; to wait before determining that a read can't happen - and
;;;; *serapi-sleep-interval*, which specifies that we may check if a read is
;;;; possible every *serapi-sleep-interval* seconds until the timeout has
;;;; elapsed. E.g., for timeout of 1 and sleep-interval 0.1, we would try to
;;;; read every .1s until either 1s elapsed or it was possible to read
(defparameter *serapi-timeout* 10
  "Maximum amount of time to wait before determining that no data can be read.")

(defparameter *serapi-sleep-interval* 0.1
  "Length of interval at which to check if data can be read.")

(defvar *dummy-stmt* "Inspect 0."
  "Coq no-op statement which may be inserted to serve as a reset point.
See `insert-reset-point'.")

(defclass serapi-process (process)
  ((reset-points :accessor reset-points :initform nil
                 :documentation "List of points to which a reset will return."))
  (:documentation "A SerAPI process."))

(defun make-serapi (&optional (program *sertop-path*) (args *sertop-args*))
  "Start up SerAPI and return a PROCESS object to interact with."
  (note 3 "Creating new serapi instance.")
  #-windows
  (make-instance 'serapi-process
                 :os-process
                 (apply #'uiop:launch-program (cons program args)
                        (append
                         (list
                          :input :stream
                          :output :stream
                          :wait nil)
                         #+sbcl (list :search t)))))

(define-condition serapi-error (error)
  ((text :initarg :text :initform nil :accessor serapi-error-text)
   (serapi :initarg :serapi :initform nil :reader serapi-error-proc)
   (error-ast :initarg :error-ast :initform nil :accessor serapi-error-ast)
   (backtrace :initarg :backtrace :initform nil :accessor serapi-backtrace))
  (:report (lambda (condition stream)
             (format stream "SerAPI or Coq exception~a: ~a~%~a"
                     (if (serapi-error-ast condition)
                         (format nil " in AST ~a" (serapi-error-ast condition))
                         "")
                     (serapi-error-text condition)
                     (serapi-backtrace condition))))
  (:documentation
   "Condition raised if errors are detected in the SerAPI process."))

(define-condition serapi-timeout-error (error)
  ((text :initarg :text :initform nil :accessor serapi-error-text)
   (serapi-timeout :initarg :serapi-timeout :initform nil
                   :reader serapi-timeout))
  (:report (lambda (condition stream)
             (format stream "Time limit of ~a seconds exceeded: ~a."
                     (serapi-timeout condition) (serapi-error-text condition))))
  (:documentation
   "Condition raised if timeout is exceeded."))

(defun escape-coq-string (str)
  "Escape special chars in STR: whitespace, quotes, ~, and backslashes in /\\."
  (regex-replace-all "/\\\\(?!\\\\)" (escape-string str) "/\\\\\\\\"))

(defun unescape-coq-string (str)
  "Remove extra escape characters from STR prior to writing to screen or file.
Backslashes (for whitespace characters or /\\) and double quotes (\") are
preceded by an extra pair of backslashes. See also `escape-coq-string'."
  (-<> str
    ;; change \\\\foo to \\foo
    (regex-replace-all "\\\\\\\\" <> "\\")
    ;; change \\\" to \"
    (regex-replace-all "\\\\\\\"" <> "\"")))

(defun escape-cmd (sexpr)
  "Return a copy of SEXPR with special characters reformatted for SerAPI.
Ensure that control characters are properly escaped and that NIL is rewritten as
a left paren symbol followed by a right paren symbol.
* SEXPR - a list of forms"
  (labels ((escape-strs (sexpr)
             (cond
               ((stringp sexpr)
                (-<>> (escape-coq-string sexpr)
                      (coerce <> 'list)
                      (cons #\")
                      (append <> (list #\"))
                      (coerce <> 'string)
                      (list)))
               ;; Rewite NIL as a pair of paren symbols. Otherwise the Lisp
               ;; formatter tends to write it as "NIL", which SerAPI doesn't
               ;; properly recognize as an empty list.
               ((null sexpr) (list '\( '\)))
               ((listp sexpr)
                (list (mappend #'escape-strs sexpr)))
               (t (list sexpr)))))
    (mappend #'escape-strs #!`,SEXPR)))

(defmethod write-to-serapi ((serapi process) (forms list))
  "Write FORMS to the SERAPI process over its input stream.
Set READTABLE-CASE to :PRESERVE to ensure printing in valid case-sensitive
format."
  (let ((*readtable* (copy-readtable nil))
        (format-string "~{~a~^~%~}~%"))
    (setf (readtable-case *readtable*) :preserve)
    (let ((full-string (format nil format-string (escape-cmd forms))))
      (when (and *shell-debug*
                 (not (eq *standard-output* (process-input-stream serapi))))
        (format t (concatenate 'string "  cmd: " full-string)))
      (format (process-input-stream serapi) full-string)
      (finish-output (process-input-stream serapi)))))

(defun sanitize-process-string (string &aux (last nil))
  "Ensure that special characters are properly escaped in STRING.
Backslashes and quotation marks are escaped, and whitespace characters (\\n,
\\t, \\r, \\b) are replaced with a single space."
  (with-output-to-string (s)
    (iter (for char in (coerce string 'list))
          (cond
            ((and (eql last #\\) (eql char #\\))
             ;; If we find \\ escape them as \\\\
             (mapc {write-char _ s} (list #\\ #\\ #\\ #\\)))
            ((and (eql last #\\) (eql char #\"))
             ;; If we find \", make sure it's escaped as \\\"
             (mapc {write-char _ s} (list #\\ #\\ #\\ char)))
            ((and (eql last #\\) (member char '(#\n #\t #\r #\b)))
             ;; If we find whitespace \n, \t, \r, \b, replace with a space
             (write-char #\Space s))
            ((eql last #\\)
             ;; If we find \[c] and c isn't \, ", n, t, r, or b: escape as \\c
             (mapc {write-char _ s} (list #\\ #\\ char)))
            (t (unless (eql char #\\) (write-char char s))))
          ;; If prev char was \, it's now escaped and the current character is
          ;; output. Otherwise, we might still need to escape the current char
          ;; if it's a \.
          (if (eql last #\\)
              (setf last nil)
              (setf last char)))))

(defmethod read-process-string ((process process)
                                &optional (eof-error-p t) eof-value)
  "Read the next line from the PROCESS output stream. Return a string."
  (sanitize-process-string
   (read-line (process-output-stream process) eof-error-p eof-value)))

(defmethod read-with-timeout ((process process) timeout interval
                              &optional (eof-error-p t) eof-value)
  "Read from PROCESS output stream, waiting up to TIMEOUT seconds for output.
Use `listen' to check if data is available in the PROCESS output stream before
reading. If no data is available, sleep INTERVAL seconds and check again up to a
maximum of TIMEOUT seconds. Return NIL if no data becomes available."
  (or (iter (for i below (floor (/ timeout interval)))
            (when (listen (process-output-stream process))
              (leave (read-process-string process eof-error-p eof-value)))
            (sleep interval))
      (when (listen (process-output-stream process))
        (read-process-string process eof-error-p eof-value))
      (restart-case
          (error (make-condition
                  'serapi-timeout-error
                  :serapi-timeout timeout
                  :text
                  (format nil "failed to read a response from process ~a."
                          process)))
        (use-empty-response ()
          :report "Return nil and continue."
          (return-from read-with-timeout nil))
        (retry-read ()
          :report "Retry `read-with-timeout'."
          (return-from read-with-timeout
            (read-with-timeout process timeout interval
                               eof-error-p eof-value))))))

(defgeneric is-error (output)
  (:documentation "Check if OUTPUT is an error response from SerAPI."))

(defgeneric is-terminating (output)
  (:documentation "Check if OUTPUT is indicates the end of a SerAPI response."))

(defmethod is-error ((str string))
  "Check if STR is an error message.
Errors may result from either an improperly formed SerAPI command or an invalid
Coq command."
  (or (starts-with-subseq "(Sexplib.Conv.Of_sexp_error" str)
      (search "CoqExn" str :from-end t)))

(defmethod is-terminating ((str string))
  "Check if STR is the final message in a SerAPI response.
For successful responses, the string will contain the word \"Completed\", and
for failed responses it will be an error string."
  (or (search "Completed" str :from-end t)
      (is-error str)))

(defmethod pre-process ((str string))
  "Pre-process STR read in from SerAPI to ensure it's a valid LISP object."
  (-<>> (replace-all str "." "\\.")
        ;; ensure Pp_string items are always wrapped in ""
        (regex-replace-all "\\(Pp_string\\s+([^\\\"].*?)\\)"
                          <>
                          "(Pp_string \"\\1\")")
        ;; ensure CoqString items are always wrapped in ""
        (regex-replace-all "\\(CoqString\\s+([^\\\"].*?)\\)"
                          <>
                          "(CoqString \"\\1\")")))

(defmethod read-serapi-response ((serapi process))
  "Read FORMS from the SERAPI process over its output stream.
Sets READTABLE-CASE to :PRESERVE to ensure printing in valid case-sensitive
format."
  (let ((*readtable* (copy-readtable nil)))
    (setf (readtable-case *readtable*) :preserve)
    ;; read all of the lines from the SerAPI response as strings
    (let ((strings
           (iter (for str = (read-with-timeout serapi *serapi-timeout*
                                               *serapi-sleep-interval*))
                 (while str)
                 (collect str into strings)
                 (when (and (is-terminating str)
                            (not (listen (process-output-stream serapi))))
                   (leave strings))
                 (finally (return strings)))))
      (when *shell-debug*
        (format t "stdout: ~{~a~^~%~}~%" strings))
      ;; Ensure all symbols are interned into serapi-io package (for match)
      (let ((*package* (find-package 'sel/cp/serapi-io)))
        ;; Attempt to parse the strings as Lisp objects.
        ;; Do this after all strings are read so that the SerAPI process is left
        ;; in a nice, ready state even if some of the strings are bad.
        (iter (for str in (mapcar #'pre-process strings))
              (with-input-from-string (in str)
                (let ((value (read in)))
                  (when (and (listp value) (is-error value))
                    (restart-case
                        (error (parse-error-message
                                (make-condition 'serapi-error
                                                :serapi serapi)
                                value))
                      (cancel-ast (ast)
                        :report "Cancel ASTs and continue."
                        (cancel-coq-asts ast))
                      (kill-serapi-process ()
                        :report "Kill SerAPI process."
                        (kill-serapi serapi))))
                  (collect value into values)))
              (finally (return values)))))))

;; We use `insert-reset-point' and `reset-serapi-process' to maintain a sentinel
;; AST ID to which we can reset the SerAPI process. We need this because when we
;; cancel an AST ID, it's gone and the next AST ID is not the same as the one
;; that was canceled, but instead is the next integer after the last assigned ID
;; (including all canceled IDs; i.e., it's monotonically increasing).
(defun insert-reset-point ()
  "Insert a reset point so `*serapi-process*' may be reset to its current state.
Add Coq `*dummy-stmt*' to `*serapi-process*' and update `reset-points' with the
AST ID. See also `reset-serapi-process'."
  (let ((ast-ids (add-coq-string *dummy-stmt*)))
    ;; Adding dummy statement will often require running Exec in cases where
    ;; Message output would be generated. Running Exec when it's not needed
    ;; isn't a problem, but failure to run it when it is needed causes
    ;; Message output to be included with the following query which would likely
    ;; cause confusion and/or errors.
    (mapc (lambda (ast-id)
            (write-to-serapi *serapi-process* #!`((Exec ,AST-ID)))
            (read-serapi-response *serapi-process*))
            ast-ids)
    (if (listp ast-ids)
        ;; if there's more than one ast-id, we reset to the smallest (first)
        (push (first ast-ids) (reset-points *serapi-process*))
        (push ast-ids (reset-points *serapi-process*)))))

(defun reset-serapi-process ()
  "Reset `*serapi-process*' to the last reset point in its `reset-points'.
See also `insert-reset-point'."
  (when-let ((reset-pt (pop (reset-points *serapi-process*))))
    (cancel-coq-asts reset-pt)))

(defmethod kill-serapi ((serapi serapi-process) &key (signal 9))
  "Ensure SerAPI process is terminated."
  (kill-process serapi :urgent (eql signal 9))
  (setf (reset-points serapi) nil))

(defmacro with-serapi ((&optional (process *serapi-process*))
                       &body body)
  "Macro to create a serapi process bound to SERAPI.
Uses default path and arguments for sertop (`*sertop-path*' and
`*sertop-args*')."
  (let ((old-proc (gensym)))
    `(let ((,old-proc
            ;; save old process
            (when (and ,process (process-running-p ,process))
              ,process)))
       (let ((*serapi-process* (or ,old-proc (make-serapi))))
         (unwind-protect
              (progn (unless ,old-proc
                       (handler-bind
                           ;; try to read in case there's any output when
                           ;; the process is created, but don't propagate the
                           ;; error if there isn't.
                           ((serapi-timeout-error
                             (lambda (c)
                               (declare (ignorable c))
                               (invoke-restart 'use-empty-response))))
                         ;; read 2 prologue lines if present
                         ;; (this is not a complete response, so should not use
                         ;; read-serapi-response)
                         (read-with-timeout *serapi-process* *serapi-timeout*
                                            *serapi-sleep-interval*)
                         (read-with-timeout *serapi-process* *serapi-timeout*
                                            *serapi-sleep-interval*)))
                     ,@body)
           (unless ,old-proc (kill-serapi *serapi-process*)))))))


(defmethod is-type ((type symbol) sexpr)
  "Check if the tag (i.e., the first element) of SEXPR is TYPE."
  (match sexpr
    (`(,x . ,_) (eql x type))))

;;;; Ideally, we'd like to have these matching functions look more like this:
;;;; (defun feedback-id (sexpr)
;;;;   (match sexpr
;;;;     (`(|Feedback| ((|id| ,id) ,_ ,_)) id)))
;;;; but we can't do that because symbols are implicitly prefixed with the
;;;; package name, so such a definition actually matches against the qualified
;;;; symbol 'evo-rings::|Feedback| and would only work within the :evo-rings
;;;; package. To circumvent this issue, we intern the symbols we want to match
;;;; against into `sel/cp/serapi-io' (this package).
(defun feedback-id (sexpr)
  "For a Feedback response from SerAPI, SEXPR, return the id.
Return NIL if SEXPR is not a Feedback response."
  (match sexpr
    (`(,fb-sym ((,id-sym ,id) ,_ ,_))
      (and (eql (intern "Feedback" :sel/cp/serapi-io) fb-sym)
           (eql (intern "id" :sel/cp/serapi-io) id-sym)
           id))))

(defun feedback-route (sexpr)
  "For a Feedback response from SerAPI, SEXPR, return the route
Return NIL if SEXPR is not a Feedback response."
  (match sexpr
    (`(,fb-sym (,_ (,route-sym ,r) ,_))
      (and (eql (intern "Feedback" :sel/cp/serapi-io) fb-sym)
           (eql (intern "route" :sel/cp/serapi-io) route-sym)
           r))))

(defun feedback-contents (sexpr)
  "For a Feedback response from SerAPI, SEXPR, return the contents.
Return NIL if SEXPR is not a Feedback response."
  (match sexpr
    (`(,fb-sym (,_ ,_ (,contents-sym ,c)))
      (and (eql (intern "Feedback" :sel/cp/serapi-io) fb-sym)
           (eql (intern "contents" :sel/cp/serapi-io) contents-sym)
           c))))

(defun message-content (message)
  "For a Message response from SerAPI, MESSAGE, return the content.
Return NIL if MESSAGE is not a Message response."
  (match message
    (`(,msg-sym ,_ ,_ ,m)
      (and (eql (intern "Message" :sel/cp/serapi-io) msg-sym)
           m))))

(defun message-level (message)
  "For a Message response from SerAPI, MESSAGE, return the message level.
Return NIL if MESSAGE is not a Message response. As of Coq 8.7.2, message levels
defined in coq/lib/feedback.ml include Debug, Info, Notice, Warning, Error."
  (match message
    (`(,msg-sym ,lvl ,_ ,_)
      (and (eql (intern "Message" :sel/cp/serapi-io) msg-sym)
           lvl))))

(defun coq-message-contents (response)
  "For a SerAPI response, return the contents of any `Message' structures.
This is useful for fetching results of Coq vernacular queries."
  (iter (for sexpr in response)
        (when (is-type (intern "Feedback" :sel/cp/serapi-io) sexpr)
          (let ((contents (feedback-contents sexpr)))
            (when (is-type (intern "Message" :sel/cp/serapi-io) contents)
              (collecting (message-content contents)))))))

(defun coq-message-levels (response)
  "For a SerAPI response, return the level of the first `Message' structure.
This is useful for verifying success of a Coq vernacular query."
  (iter (for sexpr in response)
        (when (is-type (intern "Feedback" :sel/cp/serapi-io) sexpr)
          (let ((contents (feedback-contents sexpr)))
            (when (is-type (intern "Message" :sel/cp/serapi-io) contents)
              (collecting (message-level contents)))))))

(defun answer-content (sexpr)
  "For an Answer response from SerAPI, SEXPR, return the content.
Return NIL if SEXPR is not an Answer response."
  (match sexpr
    (`(,answer-sym ,_ ,content)
      (and (eql (intern "Answer" :sel/cp/serapi-io) answer-sym)
           content))))

(defun answer-string (sexpr)
  "For an ObjList in SEXPR, a response from SerAPI, return the CoqString string.
Return NIL if SEXPR is not an ObjList or does not contain a CoqString."
  (match sexpr
    (`(,obj-list-sym ((,coq-str-sym ,cstr) . ,_))
      (when (and (eql (intern "ObjList"  :sel/cp/serapi-io) obj-list-sym)
                 (eql (intern "CoqString" :sel/cp/serapi-io) coq-str-sym))
        (if (stringp cstr)
            cstr
            (write-to-string cstr))))))

(defun answer-ast (sexpr)
  "For an ObjList response SEXPR, a response from SerAPI, return the CoqAst list.
Return NIL if SEXPR is not an ObjList or does not contain a CoqAst."
  (match sexpr
    (`(,obj-list-sym ((,coq-ast-sym ,ast) . ,_))
      (and (eql (intern "ObjList" :sel/cp/serapi-io) obj-list-sym)
           (eql (intern "CoqAst" :sel/cp/serapi-io) coq-ast-sym)
           ast))))

(defun added-id (sexpr)
  "For an Added response SEXPR, a response from SerAPI, return the AST ID added.
Return NIL if SEXPR is not an Added response."
  (match sexpr
    (`(,added-sym ,id . ,_)
      (and (eql (intern "Added" :sel/cp/serapi-io) added-sym)
           id))))

(defmethod is-terminating ((sexpr list))
  "Return T if SEXPR is the final message in a SerAPI response, NIL otherwise.
For successful responses, the list will be tagged as \"Completed\", and
for failed responses it will be tagged as an error."
  (let ((res (answer-content sexpr)))
    (or (eql res (intern "Completed" :sel/cp/serapi-io))
        (is-error sexpr))))

(defmethod is-error ((sexpr list))
  "Return T if SEXPR is an error message from either an improperly formed SerAPI
command or an invalid Coq command, NIL otherwise."
  (or (is-type (intern "Sexplib.Conv.Of_sexp_error" :sel/cp/serapi-io) sexpr)
      (is-type (intern "CoqExn" :sel/cp/serapi-io) (answer-content sexpr))
      (equal (intern "Error" :sel/cp/serapi-io) (message-level sexpr))))

(defun parse-error-message (condition sexpr)
  (cond
    ((is-type (intern "Sexplib.Conv.Of_sexp_error" :sel/cp/serapi-io) sexpr)
     (setf (serapi-error-text condition) sexpr))
    ((is-type (intern "CoqExn" :sel/cp/serapi-io) (answer-content sexpr))
     (let* ((err-info (answer-content sexpr))
            (info-len (length err-info)))
       (when err-info
         ;; Set error-ast if present
         (when (and (>= info-len 3) (nth 2 err-info))
           ;; NOTE: Based on CoqExn Stateid's in serapi_protocol.mli
           ;; The ID of the earliest AST in the problem range (if known).
           (setf (serapi-error-ast condition) (caar (nth 2 err-info))))
         ;; Set backtrace info if present
         (when-let ((backtrace (and (>= info-len 4) (nth 3 err-info))))
           (when (and (is-type (intern "Backtrace" :sel/cp/serapi-io) backtrace)
                      (lastcar backtrace))
             (setf (serapi-backtrace condition) (lastcar backtrace))))
         (when (>= info-len 5)
           (setf (serapi-error-text condition) (nth 5 err-info))))))
    (t nil))
  condition)

(defun is-loc-info (sexpr)
  "Return T if SEXPR is a list of Coq location info, NIL otherwise."
  (and (listp sexpr)
       (match sexpr
         (`(((,fname . ,_)
             (,line-nb . ,_)
             (,bol-pos . ,_)
             (,line-nb-last . ,_)
             (,bol-pos-last . ,_)
             (,bp . ,_)
             (,ep . ,_)))
           (and (eql (intern "fname" :sel/cp/serapi-io) fname)
                (eql (intern "line_nb" :sel/cp/serapi-io) line-nb)
                (eql (intern "bol_pos" :sel/cp/serapi-io) bol-pos)
                (eql (intern "line_nb_last" :sel/cp/serapi-io) line-nb-last)
                (eql (intern "bol_pos_last" :sel/cp/serapi-io) bol-pos-last)
                (eql (intern "bp" :sel/cp/serapi-io) bp)
                (eql (intern "ep" :sel/cp/serapi-io) ep)
                t))
         (otherwise nil))))

(defun coq-import-ast-p (sexpr)
  "Return T if SEXPR is tagged as VernacImport or VernacRequire, NIL otherwise."
  ;; ASTs for imports have length 2 where the first element is located info
  ;; and the second is either VernacImport or VernacRequire.
  (and (= 2 (length sexpr))
       (is-loc-info (first sexpr))
       (or (is-type (intern "VernacImport" :sel/cp/serapi-io)
                    (second sexpr))
           (is-type (intern "VernacRequire" :sel/cp/serapi-io)
                    (second sexpr)))))

(defun coq-module-ast-p (sexpr)
  "Return module name if SEXPR is tagged as defining a module, NIL otherwise.
Module tags include VernacDefineModule, VernacDeclareModule,
VernacDeclareModuleType."
  (and (listp sexpr)
       (match sexpr
         ((guard `(,_ (,vernac-tag ,_ (,_ (,id-tag ,module-name)) . ,_))
                 (and (or (eql vernac-tag (intern "VernacDefineModule"
                                                  :sel/cp/serapi-io))
                          (eql vernac-tag (intern "VernacDeclareModule"
                                                  :sel/cp/serapi-io)))
                      (eql id-tag (intern "Id" :sel/cp/serapi-io))))
          module-name)
         ((guard `(,_ (,vernac-tag (,_ (,id-tag ,module-name)) . ,_))
                 (and (eql vernac-tag (intern "VernacDeclareModuleType"
                                              :sel/cp/serapi-io))
                      (eql id-tag (intern "Id" :sel/cp/serapi-io))))
          module-name)
         (otherwise nil))))

(defun coq-section-ast-p (sexpr)
  "Return section name if SEXPR is tagged as VernacBeginSection, NIL otherwise."
  (and (listp sexpr)
       (match sexpr
         ((guard `(,_ (,section-tag (,_ (,id-tag ,section-name))))
                 (and (eql section-tag (intern "VernacBeginSection"
                                               :sel/cp/serapi-io))
                      (eql id-tag (intern "Id" :sel/cp/serapi-io))))
          section-name)
         (otherwise nil))))

(defun coq-assumption-ast-p (sexpr)
  "Return name of Parameter, Variable, etc. if SEXPR is tagged VernacAssumption.
Otherwise return NIL."
  (flet ((parse-assumption-ids (sexpr)
           (and (listp sexpr)
                (match sexpr
                  ((guard `(,_ ((((,_ (,id-tag ,name)) ,_)) . ,_))
                          (eql id-tag (intern "Id" :sel/cp/serapi-io)))
                   name)
                  (otherwise nil)))))
    (and (listp sexpr)
         (match sexpr
           ((guard `(,_ (,assumption-tag ,_ ,_ ,ls))
                   (eql assumption-tag (intern "VernacAssumption"
                                               :sel/cp/serapi-io)))
            (first (mapcar #'parse-assumption-ids ls)))
           (otherwise nil)))))

(defun coq-definition-ast-p (sexpr)
  "Return definition name if SEXPR is tagged as a definition, NIL otherwise.
Definition tags include VernacDefinition, VernacInductive, VernacFixpoint,
VernacCoFixpoint."
  ;; NOTE: may want to add VernacStartTheoremProof for theorems and lemmas.
  ;; See coq/stm/vernac_classifier.ml for grammar hints.
  (flet ((parse-inductive-ids (sexpr)
           (and (listp sexpr)
                (match sexpr
                  ((guard `(((,_ ((,_ (,id-tag ,name)) ,_)) . ,_) ,_)
                          (eql id-tag (intern "Id" :sel/cp/serapi-io)))
                   name)
                  (otherwise nil))))
         (parse-fixpoint-ids (sexpr)
           (and (listp sexpr)
                (match sexpr
                  ((guard `((((,_ (,id-tag ,name)) ,_) . ,_) ,_)
                          (eql id-tag (intern "Id" :sel/cp/serapi-io)))
                   name)
                  (otherwise nil)))))
    (and (listp sexpr)
         (match sexpr
           ((guard `(,_ (,fixpoint-tag ,_ ,ls))
                   (or (eql fixpoint-tag (intern "VernacFixpoint"
                                                 :sel/cp/serapi-io))
                       (eql fixpoint-tag (intern "VernacCoFixpoint"
                                                 :sel/cp/serapi-io))))
            (first (mapcar #'parse-fixpoint-ids ls)))
           ((guard `(,_ (,definition-tag ,_ ((,_ (,id-tag ,name)) ,_) ,_))
                   (and (eql definition-tag (intern "VernacDefinition"
                                                    :sel/cp/serapi-io))
                        (eql id-tag (intern "Id" :sel/cp/serapi-io))))
            name)
           ((guard `(,_ (,inductive-tag ,_ ,_ ,_ ,ls))
                   (eql inductive-tag (intern "VernacInductive"
                                              :sel/cp/serapi-io)))
            (first (mapcar #'parse-inductive-ids ls)))
           (otherwise nil)))))

(defun run-coq-vernacular (vernac &key (qtag (gensym "q")))
  (write-to-serapi *serapi-process* #!`((,QTAG (Query () (Vernac ,VERNAC)))))
  (read-serapi-response *serapi-process*))

(defgeneric lookup-coq-pp (ast-name &key qtag)
  (:documentation
   "Look up and return a Coq representation of AST-NAME.
Optionally, tag the lookup query with QTAG."))

(defmethod lookup-coq-pp ((ast-name string) &key (qtag (gensym "q")))
  (let ((vernac (format nil "Print ~a." ast-name)))
    (coq-message-contents (run-coq-vernacular vernac :qtag qtag))))

(defgeneric check-coq-type (coq-expr &key qtag)
  (:documentation "Look up and return the type of a Coq expression COQ-EXPR.
Optionally use QTAG to tag the query."))

(defmethod check-coq-type (coq-expr &key (qtag (gensym "q")))
  "Look up and return the type of a COQ-EXPR using Coq's `Check'.
COQ-EXPR will be coerced to a string via the `format' ~a directive."
  (let ((response (run-coq-vernacular (format nil "Check ~a." coq-expr)
                                      :qtag qtag)))
    (if (member (intern "Error" :sel/cp/serapi-io)
                (coq-message-levels response))
        (error (make-condition 'serapi-error
                               :text (coq-message-contents response)
                               :serapi *serapi-process*))
        (-<> (coq-message-contents response)
          (remove (intern "Pp_empty" :sel/cp/serapi-io) <>)
          (car)
          (lookup-coq-string <> :input-format #!'CoqPp)
          (tokenize-coq-type)))))

(defun find-coq-string-in-objlist (response)
  "For SerAPI response list RESPONSE, return a CoqString answer if there is one.
Return NIL otherwise. Some responses may contain two newlines followed by a
statement such as \"For X: Argument Scope is [Y]\". In this case, drop
everything after the newlines so we don't return Coq code that can't be
re-submitted."
  (iter (for sexpr in response)
        (when-let ((answer (answer-content sexpr)))
          (when (is-type (intern "ObjList" :sel/cp/serapi-io) answer)
            (when-let ((answer-str (answer-string answer)))
              ;; drop everything after two consecutive newlines, add period
              ;; so that we drop the trailing "\n\nFor X: Argument scope is [Y]"
              ;; TODO: May break if we don't strip out prints from CoqPp and
              ;; we retrieve a definition that legitimately contains newlines.
              (let ((double-newline (scan "\\\\n\\\\n" answer-str)))
                (leave
                 (if double-newline
                     ;; remove everything after \n\n
                     (concatenate 'string
                                  (subseq answer-str 0 double-newline))
                     ;; no \n\n, so just keep the string as-is
                     answer-str))))))))

(defgeneric lookup-coq-string (id &key qtag input-format)
  (:documentation "Look up the Coq string for ID.
If needed specify the INPUT-FORMAT of the ID. Optionally,  provide a QTAG for
the lookup query.

The ID may be either an integer AST ID or one of the list representations,
CoqPp, CoqAst, etc."))

(defmethod lookup-coq-string ((pp-string list) &key (qtag (gensym "p"))
                                                 (input-format '|CoqAst|))
  "Look up the Coq string for PP-STRING.
If PP-STRING is not a CoqAst expression, specify the INPUT-FORMAT. See the
coq_object type in the SerAPI file serapi/serapi_protocol.mli for a complete
list. Optionally, provide a QTAG for the lookup query."
  ;; input-format: CoqPp, CoqAst, etc.
  (let ((print #!`((,QTAG (Print ((pp_format PpStr))
                                 (,INPUT-FORMAT ,PP-STRING))))))
    (write-to-serapi *serapi-process* print)
    (find-coq-string-in-objlist (read-serapi-response *serapi-process*))))

(defmethod lookup-coq-string ((ast-id integer) &key (qtag (gensym "q"))
                                                 input-format)
  "Look up the Coq string for ID in PROCESS.
Optionally, provide a QTAG for the lookup query. INPUT-FORMAT is ignored."
  (declare (ignorable input-format))
  (let ((query #!`((,QTAG (Query ((pp ((pp_format PpStr)))) (Ast ,AST-ID))))))
    (write-to-serapi *serapi-process* query)
    (find-coq-string-in-objlist (read-serapi-response *serapi-process*))))

(defmethod lookup-coq-string ((raw-string string) &key qtag input-format)
  "RAW-STRING is already a Coq string. Return it without doing a lookup."
  (declare (ignorable qtag input-format))
  raw-string)

(defun add-coq-string (coq-string &key (qtag (gensym "a")))
  "Submit COQ-STRING as a new statement to add to the Coq document.
Return a list of the AST-IDs created as a result. Optionally, provide a QTAG for
the submission."
  (unless (emptyp coq-string)
    ;; ensure coq string ends with .
    (let ((add #!`((,QTAG (Add () ,COQ-STRING)))))
      ;; submit to SerAPI
      (write-to-serapi *serapi-process* add)

      ;; read/parse response
      (let ((added-ids
             (iter (for sexpr in (read-serapi-response *serapi-process*))
                   (when-let ((answer (answer-content sexpr)))
                     (when (is-type (intern "Added" :sel/cp/serapi-io)
                                    answer)
                       (collecting (added-id answer)))))))
        (iter (for added in added-ids)
              (write-to-serapi *serapi-process* #!`((,QTAG (Exec ,ADDED))))
              (read-serapi-response *serapi-process*))
        added-ids))))

(defun add-coq-lib (path &key lib-name (has-ml "true") (qtag (gensym "l")))
  "Add directory PATH to the Coq and, optionally, ML load paths in SerAPI.
Return the SerAPI response.
* LIB-NAME an optional prefix to use when importing from the directory. E.g.,
for LIB-NAME Foo, file PATH/Bar.vo could be imported with
\"Require Import Foo.Bar.\").
* HAS-ML the string \"true\" or \"false\", indicating whether PATH should or
should not be added to the ML load path in addition to the Coq load path. For
convenience, the default is \"true\".
* QTAG optionally, a tag to use for the SerAPI command.
"
  (let ((cmd #!`((,QTAG (LibAdd ,(WHEN LIB-NAME (LIST LIB-NAME))
                                ,PATH
                                ,HAS-ML)))))
    (write-to-serapi *serapi-process* cmd)
    (read-serapi-response *serapi-process*)))

(defgeneric lookup-coq-ast (ast-id &key qtag)
  (:documentation
   "Look up and return the AST identified by AST-ID.
Optionally, use QTAG to tag the query."))

(defmethod lookup-coq-ast ((ast-id integer) &key (qtag (gensym "q")))
  "Look up and return the AST whose ID is the integer AST-ID.
Optionally, use QTAG to tag the query."
  (let ((query #!`((,QTAG (Query () (Ast ,AST-ID))))))
    (write-to-serapi *serapi-process* query)
    (iter (for sexpr in (read-serapi-response *serapi-process*))
          (when-let ((answer (answer-content sexpr)))
            (when (is-type (intern "ObjList" :sel/cp/serapi-io) answer)
              (leave (answer-ast answer)))))))

(defun cancel-coq-asts (ast-ids &key (qtag (gensym "c")))
  "Cancel the ASTs identified by AST-IDS and return the response.
Optionally, use QTAG to tag the query. Canceling means that the ASTs will no
longer be defined and other definitions added in the interim will also be
removed."
  (let* ((ast-ids (if (listp ast-ids) ast-ids (list ast-ids)))
         (cancel #!`((,QTAG (Cancel ,AST-IDS)))))
    (write-to-serapi *serapi-process* cancel)
    (read-serapi-response *serapi-process*)))

(defun load-coq-file (file-name &key (qtag (gensym "a")))
  "Load Coq file FILE-NAME. Optionally, use QTAG to tag the query.
Return a list of IDs for the ASTs that were added."
  ;; TODO: with newer version of SerAPI, may be able to use ReadFile instead.
  (with-open-file (in file-name)
    (let ((lines (iter (for line = (read-line in nil nil))
                       (while line)
                       (collect (format nil "~a~%" line) into results)
                       (finally (return (apply #'concatenate 'string
                                               results))))))
      (add-coq-string lines :qtag qtag))))

(defun list-coq-libraries (&key (imported-only t)
                             (keep-prefix "Coq.")
                             (qtag (gensym "v")))
  (let ((all-loaded
         (nest ;; remove leading "Loaded and imported" string
               (cdr)
               ;; remove empty strings
               (remove-if #'emptyp)
               ;; remove spaces
               (mapcar [#'trim-whitespace #'second])
               ;; filter message contents to just include strings
               (filter-subtrees
                [{eql (intern "Pp_string" :sel/cp/serapi-io)} #'car])
               (coq-message-contents)
               (run-coq-vernacular (format nil "Print Libraries.") :qtag qtag)))
        (end-of-imports-str "Loaded and not imported"))
    (cond
      ;; keep only loaded and imported library names that start with prefix
      ((and imported-only keep-prefix)
       (remove-if-not
        {starts-with-subseq keep-prefix}
        (cdr (take-until {starts-with-subseq end-of-imports-str} all-loaded))))
      ;; keep only loaded and imported library names
      (imported-only
       (cdr (take-until {starts-with-subseq end-of-imports-str} all-loaded)))
      ;; keep all loaded library names that start with prefix
      (keep-prefix
       (remove-if-not {starts-with-subseq keep-prefix} all-loaded))
      ;; list all loaded library names
      (t (remove-if {starts-with-subseq end-of-imports-str} all-loaded)))))


;; Type lookup and search
(defun tokenize-coq-type (type-string)
  (labels ((find-matching-paren (str pos depth)
             (cond
               ((equal pos (length str)) nil)
               ((and (equal (elt str pos) #\)) (zerop depth))
                pos)
               ((equal (elt str pos) #\))
                (find-matching-paren str (1+ pos) (1- depth)))
               ((equal (elt str pos) #\()
                (find-matching-paren str (1+ pos) (1+ depth)))
               (t (find-matching-paren str (1+ pos) depth))))
           (tokenize (str)
             (let ((str (trim-whitespace str))
                   (split-tokens (list #\: #\( #\) #\[ #\] #\Space #\Tab
                                       #\Newline)))
               (cond
                 ((emptyp str) nil)
                 ((starts-with-subseq "->" str)
                  (cons :-> (tokenize (subseq str 2))))
                 ((starts-with-subseq "forall " str)
                  (cons :forall (tokenize (subseq str 6))))
                 ((starts-with-subseq "exists " str)
                  (cons :exists (tokenize (subseq str 6))))
                 ((starts-with-subseq ":" str)
                  (cons :colon (tokenize (subseq str 1))))
                 ((or (starts-with-subseq "[" str)
                      (starts-with-subseq "]" str))
                  (cons (subseq str 0 1) (tokenize (subseq str 1))))
                 ((starts-with-subseq "(" str)
                  (let ((end-paren (find-matching-paren str 1 0)))
                    (cons (tokenize (subseq str 1 end-paren))
                          (tokenize (subseq str (1+ end-paren))))))
                 ((starts-with-subseq ")" str) nil)
                 (t
                  (let ((next-token (position-if {member _ split-tokens} str)))
                    (if next-token
                        (cons (subseq str 0 next-token)
                              (tokenize (subseq str next-token)))
                        (list (subseq str 0)))))))))
    (tokenize type-string)))

(defun search-coq-type (coq-type &key module (qtag (gensym "q")))
  "Return a list of tokenized types of values whose \"final\" type is COQ-TYPE.
Results include functions parameterized by values of types other than COQ-TYPE
as long as the final result returned by the function is of type COQ-TYPE.
E.g., if COQ-TYPE is bool, functions of type nat->bool will be included."
  (flet ((parenthesize (str)
           (let ((str (trim-whitespace str)))
             (if (and (starts-with-subseq "(" str)
                      (ends-with-subseq ")" str))
                 str
                 (format nil "(~a)" str)))))
    ;; Translates to vernacular:
    ;; SearchPattern (coq-type) inside module. or
    ;; SearchPattern (coq-type).
    (nest (mapcar [#'tokenize-coq-type
                   {lookup-coq-string _ :input-format '|CoqPp|}])
          (coq-message-contents)
          (run-coq-vernacular
           (format nil "SearchPattern ~a ~a"
                   (parenthesize coq-type)
                   (if module
                       (format nil "inside ~a." module)
                       (format nil ".")))
           :qtag qtag))))


;;; Building Coq expressions

(defun wrap-coq-constr-expr (expr)
  "Create a Coq ConstrExpr by wrapping EXPR with v and loc tags.
Generally, this will be used by the `make-coq-*' functions and won't need to be
invoked directly."
  #!`((v ,EXPR) (loc ())))

(defun make-coq-integer (x)
  "Wrap integer X in CPrim and Numeral tags."
  (wrap-coq-constr-expr #!`(CPrim (Numeral ,X true))))

(defun make-coq-ident (id)
  "Wrap ID in Ident and Id tags."
  #!`(Ident (() (Id ,ID))))

(defun make-coq-var-reference (id)
  "Wrap Coq variable ID in CRef and either Ident or Qualid tags.
Use Ident for unqualified names and Qualid for qualified names."
  (let ((quals (reverse (split-sequence #\. (string id)))))
    (flet ((make-dirpath-ls (quals)
             (mapcar [{list (intern "Id" :sel/cp/serapi-io) }
                      {intern _  :sel/cp/serapi-io } ]
                     (cdr quals))))
      (when (and quals (listp quals))
        (if (= 1 (length quals))
            (wrap-coq-constr-expr #!`(CRef ,(MAKE-COQ-IDENT ID) ()))
            (wrap-coq-constr-expr
             #!`(CRef
                 (Qualid (() (Ser_Qualid
                              (DirPath ,(MAKE-DIRPATH-LS QUALS))
                              (Id ,(INTERN (CAR QUALS) :SEL/CP/SERAPI-IO)))))
                 ())))))))

(defun make-coq-application (fun &rest params)
  "Wrap Coq function FUN and parameters PARAMS in a CApp tag.
Assumes that FUN and all PARAMS are appropriately wrapped (e.g., with
`make-coq-var-reference' or similar)."
  (wrap-coq-constr-expr
   #!`(CApp (() ,FUN)
            ,(MAPCAR #'LIST
                     PARAMS
                     (REPEATEDLY (LENGTH PARAMS) ())))))

(defun make-coq-name (id)
  "Wrap ID in Name and Id tags."
  #!`(Name (Id ,ID)))

(defun make-coq-lname (id)
  (list () (make-coq-name id)))

(defun make-coq-local-binder-exprs (type params &optional param-types)
  "Create a list of local binder expressions.
If present, PARAM-TYPES is the same length as PARAMS and indicates the type of
each param. If absent, a CHole tag is used to indicate that the type is
unspecified."
  ;; assumes (length params) == (length param-types)
  (let ((num-param-types (length param-types))
        (param-types (coerce param-types 'vector)))
    (iter (for param in params)
          (for i upfrom 0)
          (let ((param-type (or (and (> num-param-types i)
                                     (elt param-types i))
                                #!`(CHole ((BinderType ,(MAKE-COQ-NAME PARAM)))
                                          IntroAnonymous
                                          ()))))
            (collecting #!`(,TYPE (,(MAKE-COQ-LNAME PARAM))
                                  (Default Explicit)
                                  ,PARAM-TYPE))))))

;; TODO: Maybe switch to using `make-coq-local-binder-exprs'
(defun make-coq-lambda (params body)
  "Create a Coq lambda expression from PARAMS and BODY."
  (if (null params)
      body
      (let ((lname (make-coq-lname (car params)))
            (chole (wrap-coq-constr-expr
                    #!`(CHole ((BinderType ,(MAKE-COQ-NAME (CAR PARAMS))))
                              IntroAnonymous
                              ()))))
        (wrap-coq-constr-expr
         #!`(CLambdaN (((,LNAME)
                        (Default Explicit)
                        ,CHOLE))
                      ,(MAKE-COQ-LAMBDA (CDR PARAMS) BODY))))))

(defun make-coq-if (test then else)
  "Create a Coq If expression: if TEST then THEN else ELSE."
  (wrap-coq-constr-expr
   #!`(CIf ,TEST
           (() ())
           ,THEN
           ,ELSE)))

(defun make-coq-case-pattern (pattern-type &rest pattern-args)
  "Create cases for a Coq match statement."
  (wrap-coq-constr-expr
   (match (intern pattern-type :sel/cp/serapi-io)
     ;; CPatAtom
     ((guard cpatatom (eql cpatatom (intern "CPatAtom" :sel/cp/serapi-io)))
      (cond
        ((equal (first pattern-args) (intern "_" :sel/cp/serapi-io))
         #!`(CPatAtom ()))
        ((first pattern-args)
         #!`(CPatAtom (,(MAKE-COQ-IDENT (FIRST PATTERN-ARGS)))))
        (t nil)))
     ;; CPatNotation
     ((guard cpatnotation (eql cpatnotation (intern "CPatNotation"
                                                    :sel/cp/serapi-io)))
      (let ((notation (first pattern-args))
            (notation-subs (cdr pattern-args)))
        #!`(CPatNotation ,NOTATION
                         (,NOTATION-SUBS ())
                         ())))
     ((guard cpatcstr (eql cpatcstr (intern "CPatCstr" :sel/cp/serapi-io)))
      (let ((ref (first pattern-args))
            (exprs (cdr pattern-args)))
        #!`(CPatCstr ,REF
                     ()
                     ,EXPRS)))
     (otherwise ()))))

(defun make-coq-match (style test &rest branches)
  "Create a Coq match expression.
STYLE indicates the style of the match (e.g., #!'RegularStyle).
TEST is the expression being matched against.
BRANCHES are the match cases and will generally be constructed with
`make-coq-case-pattern'."
  (let ((split-branches (iter (for (pat result) on branches by #'cddr)
                              (collecting #!`(() (((()  (,PAT))) ,RESULT))))))
    (wrap-coq-constr-expr
     #!`(CCases ,STYLE
                ()
                ;; assume TEST is just a single item and no in/as clauses
                ((,TEST () ()))
                ,SPLIT-BRANCHES))))

(defun make-coq-definition (name params return-type body)
  "Create a Coq function definition.
NAME - the name of the function.
PARAMS - a list of pairs indicating the name and type of each input parameter to
the function.
RETURN-TYPE - the function's return type.
BODY - the function body."
  (let ((binders (make-coq-local-binder-exprs
                  #!'CLocalAssum
                  (mapcar #'first params)
                  (mapcar #'second params))))
    #!`(() (VernacDefinition
            (() Definition)
            ((() (Id ,NAME)) ())
            (DefineBody
                ,BINDERS
                ()
              ,BODY
              ,(WHEN RETURN-TYPE
                 (LIST RETURN-TYPE)))))))

(defun coq-function-definition-p (sexpr &optional function-name)
  "Return T if SEXPR is a Coq VernacDefinition expression.
If FUNCTION-NAME is specified, only return T if SEXPR defines a function
named FUNCTION-NAME."
  (match sexpr
    ((guard `(,_ (,vernac-def ,_ ((,_ (,id ,name)) ,_) ,_))
            (and (eql vernac-def
                      (intern "VernacDefinition" :sel/cp/serapi-io))
                 (eql id (intern "Id" :sel/cp/serapi-io))))
     (or (not function-name)
         (and function-name (eql (intern function-name :sel/cp/serapi-io)
                                 name))))))

(defun lookup-qualified-coq-names (coq-type)
  "For a Coq type COQ-TYPE, look up the qualified names (using Locate).
Return a list of strings representing the possible qualified names.
COQ-TYPE may be either a string or a symbol."
  (let* ((resp (run-coq-vernacular (format nil "Locate ~a." coq-type)))
         (strings (mapcar #'second (filter-subtrees
                                    [{eql (intern "Pp_string"
                                                  :sel/cp/serapi-io)}
                                     #'car]
                                    resp))))
    (unless (starts-with-subseq "No object" (car strings))
      ;; TODO: May not be correct for all types. Assumes that results are things
      ;; like "Constant qualid" or "Notation qualid", so the qualids are the
      ;; even elements of the list of strings in the result.
      (iter (for even in (cdr strings) by #'cddr)
            (collecting (if (starts-with-subseq "SerTop." even)
                            (subseq even (length "SerTop."))
                            even))))))

(defun parse-coq-function-locals (sexpr)
  ;; NOTE: may need to add VernacStartTheoremProof for theorems and lemmas.
  ;; See coq/stm/vernac_classifier.ml for grammar hints.
  (when (listp sexpr)
    (when-let ((local-assums (filter-subtrees
                              [{eql (intern "CLocalAssum" :sel/cp/serapi-io)}
                               #'car]
                              sexpr))
               (fn-name (coq-definition-ast-p sexpr)))
      (flet ((parse-ids (ids-ls)
               (iter (for the-id in ids-ls)
                     (collecting
                      (match the-id
                        ((guard `(,_ (,name-tag (,id-tag ,name)))
                                (and (eql name-tag
                                          (intern "Name" :sel/cp/serapi-io))
                                     (eql id-tag
                                          (intern "Id" :sel/cp/serapi-io))))
                         name))))))
        (iter (for assum in local-assums)
              (appending
               (match assum
                 ((guard `(,c-local-assum ,ids-ls ,_ ((,_ ,type) ,_))
                         (eql c-local-assum (intern "CLocalAssum"
                                                    :sel/cp/serapi-io)))
                  (let ((ids (parse-ids ids-ls)))
                    (mapcar #'list
                            ids
                            (repeatedly (length ids) type)))))))))))
