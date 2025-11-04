(defpackage :software-evolution-library/software/tree-sitter-dev
  (:use :software-evolution-library/software/tree-sitter)
  (:import-from :software-evolution-library/software/directory)
  (:import-from :software-evolution-library/software/tree-sitter-code-gen)
  (:import-from :cmd)
  (:import-from :stefil+)
  (:documentation "Load this package for tree-sitter dev and debug tools."))
(in-package :software-evolution-library/software/tree-sitter)

(defvar *grammar* nil
  "JSON list representation of the grammar.")
(defvar *rules* nil
  "The rules of the grammar in JSON.")
(defvar *transformed* nil
  "The transformed representation of the rules")
(defvar *types* nil
  "The types associated with the grammar.")
(defvar *pruned-rules* nil
  "The pruned verson of the rules")
(defvar *collapsed-rules* nil
  "The collapsed representation of the rules.")
(defvar *expanded-rules* nil
  "The epanded versions of each rule.")

(defun dump-tree-sitter-grammar-json (name
                                      &aux (*json-identifier-name-to-lisp*
                                            (lambda (string)
                                              (convert-name name string))))
  "Dump the grammar for language NAME."
  (destructuring-bind (name grammar-file nodes-file)
      (or (find name
                *tree-sitter-language-files*
                :key 'car :test #'string-equal)
          (error "Unknown language: ~a" name))
    (declare (ignore name nodes-file))
    (decode-json-from-source (pathname grammar-file))))

(defun dump-tree-sitter-grammar-json-rules (name)
  "Dump the rules for language NAME."
  (aget :rules (dump-tree-sitter-grammar-json name)))

(defun json-rule-ref (language rule)
  "Look up RULE in the rules for language NAME."
  (aget rule (dump-tree-sitter-grammar-json-rules language)))

(defun set-grammar (name)
  (setf *grammar* (dump-tree-sitter-grammar-json name)))

(defun set-rules (name)
    (setf *rules* (aget :rules (set-grammar name))))

(defun set-transformed-json (name)
  (setf *transformed*
        (mapcar (lambda (rule &aux (rule-name (car rule)))
                  (cons rule-name
                        (transform-json-rule
                         name (cdr rule) *grammar* (list rule-name))))
                (set-rules name))))

(defun set-types (name)
  (setf *types*
        (destructuring-bind (name grammar-file nodes-file)
            (or (find name
                      *tree-sitter-language-files*
                      :key 'car :test #'string-equal)
                (error "Unknown language: ~a" name))
          (declare (ignore name grammar-file))
          (decode-json-from-source (pathname nodes-file)))))

(defun set-pruned-rules (name)
  (setf *pruned-rules*
        (mapcar (op (list (car _) (prune-rule-tree (cdr _1))))
                (set-transformed-json name))))

(defun set-collapsed-rules (name)
  (setf *collapsed-rules*
        (mapcar (op (list (car _) (collapse-rule-tree (cadr _1))))
                (set-pruned-rules name))))

#+broken
(defun set-expanded-rules (name)
  (set-collapsed-rules name)
  (setf *expanded-rules*
        (mapcar (lambda (json)
                  (expand-choice-branches (aget (car json) *pruned-rules*) (cdr json)))
                *transformed*)))

(defun set-everything (name)
  "Set all of the special variables to their relevant value."
  #+broken
  (set-expanded-rules name)
  (set-types name)
  (set-collapsed-rules name))

(defun get-problematic-rules (name)
  "Get the problematic rules in the grammar specified by NAME."
  (mapcar (lambda (collapsed pruned)
            ;; This will map rules to whether they're problematic or not.
            (list (car collapsed)
                  (structured-rule-p (cadr collapsed) (cadr pruned))))
          (set-collapsed-rules name)
          *pruned-rules*))

(defun update-class-allocated-slots ()
  "For all classes exported by sel/sw/ts, update any class-allocated slots according to their initforms.

\(Note that in CL class-allocated slots are *not* reinitialized if the
class definition changes.)

Return the number of slots re-initialized."
  (let* ((symbols (package-exports :sel/sw/ts))
         (classes (filter-map (op (find-class _ nil)) symbols))
         (update-count 0))
    (dolist (class classes update-count)
      (finalize-inheritance class)
      (when-let* ((slot-defs (class-slots class))
                  (class-slot-defs
                   (keep :class slot-defs :key #'slot-definition-allocation))
                  (instance (allocate-instance class)))
        (dolist (slot-def class-slot-defs)
          (and-let* ((slot (slot-definition-name slot-def))
                     ((slot-boundp instance slot))
                     ;; This is wrong in the general case (the
                     ;; initform should be evaluated in its original
                     ;; lexical environment) but all the initforms
                     ;; used by tree-sitter are constant so it doesn't
                     ;; matter here.
                     (initform (eval (slot-definition-initform slot-def)))
                     ((not (equal initform (slot-value instance slot)))))
            (setf (slot-value instance slot) initform)
            (incf update-count)))))))

(defun get-ts-files (dir)
  "List TypeScript files in DIR, excluding test files, node_modules,
and .d.ts files."
  (mapcar #'pathname
          (lines (cmd:$cmd "find" (ensure-directory-pathname dir)
                           "-name '*.ts' | grep -Ev"
                           (list "node_modules|/test|\.d\.ts$")))))

;;; TODO This should not be necessary.
(defun untabify (string)
  (string-replace-all
   (string #\Tab)
   string
   (make-string 4 :initial-element #\Space)))

(defun test-project-parsing (class dir files &key (round-trip t) shuffle)
  "Attempt to parse and print every file in FILES, proving a restart
to advance to the next file."
  (declare (optimize debug))
  (do-each (file (if shuffle (reshuffle files) files))
    (with-simple-restart (continue "Next")
      (if round-trip
          (round-trip-file class dir file)
          (stefil:finishes
           (source-text (genome (from-file class file))))))))

(defun round-trip-file (class dir file)
  (let* ((orig (untabify (read-file-into-string file)))
         (new
          (stefil:finishes
           (source-text (genome (from-string class orig))))))
    (stefil:is (equal orig new)
               "Mismatch in ~a:~%Line: ~a~%Original: ~s~%New: ~s"
               (path-join dir file)
               (1+ (count #\Newline orig :end (mismatch orig new)))
               (take 20 (drop (max 0 (- (mismatch orig new) 5)) orig))
               (take 20 (drop (max 0 (- (mismatch orig new) 5)) new)))))

(defun problematic-classes (class dir files &rest args
                            &key dont-catch &allow-other-keys)
  "Parse and print FILES, collecting a list of problematic classes."
  (hash-table-alist
   (frequencies
    (with-collectors (collect)
      (handler-bind ((sel/sw/ts::parse-tree-matching-error
                      (lambda (e)
                        (let ((class
                               (parse-tree-matching-error-superclass e)))
                          (collect class)
                          (unless (member class dont-catch)
                            (invoke-restart 'continue)))))
                     (sel/sw/ts::rule-matching-error
                      (lambda (e)
                        (let ((class
                               (type-of
                                (rule-matching-error-ast
                                 e))))
                          (collect class)
                          (unless (member class dont-catch)
                            (invoke-restart 'continue))))))
        (apply #'test-project-parsing class dir files
               :allow-other-keys t args))))))

(defun problematic-classes-in-project (type dir extension &rest args
                                       &key &allow-other-keys)
  (let* ((files (split-sequence #\Null
                                (cmd:$cmd "find" dir
                                          "-name"
                                          (list (string+ "*." extension))
                                          "-type f"
                                          "-print0")
                                :remove-empty-subseqs t)))
    (apply #'problematic-classes type dir files args)))

(defgeneric format-tree (stream root &rest rest &key level format-fn
                         &allow-other-keys)
  (:documentation "Format ROOT and write to STREAM.
:FORMAT-FN is a function that accepts a stream, ast, and current delimiter
string.")
  (:method (stream (root functional-tree-ast) &rest rest
            &key (level 0) format-fn (format-delimiter "--|")
            &allow-other-keys)
    (if format-fn
        (funcall format-fn stream root
                 (repeat-sequence
                  format-delimiter (* level (length format-delimiter))))
        (format stream "~a~a~%" (repeat-sequence format-delimiter level)
                (type-of root)))
    (iter
      (for child in (children root))
      (apply #'format-tree stream child :level (1+ level) rest)))
  (:method (stream (root tree-sitter-ast) &rest rest
            &key (level 0) format-fn (format-delimiter "--|")
            &allow-other-keys)
    (if format-fn
        (funcall format-fn stream root
                 (repeat-sequence
                  format-delimiter (* level (length format-delimiter))))
        (format stream "~a~a~%" (repeat-sequence format-delimiter level)
                (type-of root)))
    (iter
      (for child in (remove-if #'listp (parse-order root)))
      (apply #'format-tree stream child :level (1+ level) rest))))

(defun summarize-ast (ast &key (stream t) (indent 2))
  "Print a quick summary of an AST as a tree."
  (with-string (out stream)
    (labels ((source-text* (ast)
               (handler-case (source-text ast)
                 (error () "UNKNOWN")))
             (summarize-ast (stream ast indent-string)
               (let ((lines (lines (source-text* ast) :count 2)))
                 (format stream "~&~a~a ~a~@[...~]"
                         indent-string
                         (type-of ast)
                         (first lines)
                         (rest lines)))))
      (format-tree
       out ast
       :format-delimiter (make-string indent :initial-element #\Space)
       :format-fn #'summarize-ast))))

(defun ambiguous-asts (software)
  "Return a list that contains a list of an ambiguous AST and its contextualized
representation."
  (let (contextualization-stack)
    (labels ((contextualization-stack-handler (ast)
               "Push information onto the contextualization stack if AST needs to
                be contextualized."
               (let ((contextualized-ast
                      (contextualize-ast software ast)))
                 (unless (eq contextualized-ast ast)
                   (push (list ast contextualized-ast)
                         contextualization-stack)))))
      (mapc #'contextualization-stack-handler (genome software)))
    contextualization-stack))

(defun ambiguities-in-project (project)
  "Return a mapping of software objects in PROJECT to their ambiguities."
  (iter
    (for (nil . obj) in (sel/sw/project:evolve-files project))
    (when-let ((ambiguities (ambiguous-asts obj)))
      (collect (list obj ambiguities)))))

(defun trim-prefix (prefix symbol)
  (make-keyword
   (drop-prefix (string+ prefix "-") (string symbol))))

(defun get-default-slot-value (ast slot)
  (slot-definition-initform
   (find slot (class-slots (class-of ast))
         :key #'slot-definition-name)))

(defgeneric convert-to-list-specification (prefix ast &key)
  (:documentation
   "Convert AST to a list specification that can be used with #'convert.
Can pass :NORMALIZE-INDENTATION to normalize the indentation for being placed
in an existing AST.")
  (:method (prefix ast &key &allow-other-keys)
    ast)
  (:method (prefix (list list)
            &rest keys
            &key)
    (iter
      (for item in list)
      (collect (apply #'convert-to-list-specification prefix item keys))))
  (:method :around (prefix (ast functional-tree-ast)
                    &key &allow-other-keys
                    &aux (specification (call-next-method)))
    ;; NOTE: any car that doesn't have a slot corresponding slot will be put
    ;;       in the annotations slot.
    (with-slots (annotations) ast
      (if annotations
          (append specification annotations)
          specification)))
  (:method :around (prefix (ast text-fragment) &key &allow-other-keys)
    (cons (cons :text (text ast))
          (call-next-method)))
  (:method :around (prefix (ast indentation)
                    &key normalize-indentation &allow-other-keys
                    &aux (specification (call-next-method)))
    (with-slots (indent-children indent-adjustment) ast
      (if-let ((indentation-slots
                `(,@(when indent-children
                      `((:indent-children . ,(if normalize-indentation
                                                 t
                                                 indent-children))))
                  ,@(when indent-adjustment
                      `((:indent-adjustment . ,indent-adjustment))))))
        (append specification indentation-slots)
        specification)))
  (:method :around (prefix (ast structured-text)
                    &key &allow-other-keys
                    &aux (specification (call-next-method)))
    (with-slots (before-text after-text) ast
      (if-let ((text-slots
                `(,@(when (not (emptyp before-text))
                      `((:before-text . ,before-text)))
                  ,@(when (not (emptyp after-text))
                      `((:after-text . ,after-text))))))
        (append specification text-slots)
        specification)))
  (:method :around (prefix (ast tree-sitter-ast) &key &allow-other-keys)
    (cons (cons :class (trim-prefix prefix (type-of ast)))
          (call-next-method)))
  (:method :around (prefix (ast computed-text)
                    &key &allow-other-keys
                    &aux (specification (call-next-method))
                      (text (text ast)))
    (cond
      ((slot-value ast 'children) specification)
      ((equal text (get-default-slot-value ast 'text)) specification)
      (t (append1 specification
                  (cons :text (text ast))))))
  (:method (prefix (ast tree-sitter-ast) &rest keys &key &allow-other-keys)
    (iter
      (for (slot . nil) in (child-slots ast))
      (for slot-value = (apply #'convert-to-list-specification
                               prefix
                               (slot-value ast slot)
                               keys))
      (when slot-value
        (collect
            (cons (trim-prefix prefix slot) slot-value))))))

(defun validate-symtab (symtab)
  "Check for unreachable symbol table entries."
  (iter outer
        (for (ns map) in-map symtab)
        (iter (for ast-list in-set (fset:range map))
              (iter (for ast in ast-list)
                    (unless (reachable? ast :use-attrs t)
                      (in outer (collecting (cons ns ast))))))))

(defun check-symbol-table (symtab &optional message)
  (when-let (extra (validate-symtab symtab))
    (cerror "Return the symbol table"
            "Invalid symtab:~@[ ~a:~]~%~a"
            message extra))
  symtab)

(defun get-file-dependencies ()
  (let* ((root (ft/attrs:attrs-root*))
         (deps (ft/attrs::attrs.subroot->deps ft/attrs:*attrs*)))
    (with-collectors (collect-deps)
      (do-hash-table (subroot dep-list deps)
        (let ((subroot.deps
                (cons subroot
                      (filter-map #'tg:weak-pointer-value dep-list))))
          (collect-deps
           (mapcar (lambda (x)
                     (when (subroot? x)
                       (find-enclosing 'sel/sw/directory:file-ast root x)))
                   subroot.deps)))))))
