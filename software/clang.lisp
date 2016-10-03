;;; clang.lisp --- clang software representation

;; Copyright (C) 2012 Eric Schulte

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(in-package :software-evolution)

(define-software clang (ast)
  ((compiler :initarg :compiler :accessor compiler :initform "clang")
   (asts :initarg :asts :initform nil :copier :direct
         :type (list (cons keyword *) *) :documentation
         "Association List of body ASTs.")
   (functions :initarg :functions :initform nil :copier :direct
               :documentation "Complete functions with bodies.")
   (prototypes :initarg :prototypes :initform nil :copier :direct
               :documentation "Function prototypes.")
   (includes :initarg :includes :copier copy-seq
             :initform nil :type (list string *)
             :documentation "Names of included includes.")
   (types :initarg :types :copier copy-seq
          :initform nil :type (list (cons keyword *) *)
          :documentation "Association list of types keyed by HASH id.")
   (declarations :initarg :declarations :copier :direct
                 :initform (make-hash-table :test #'equal) :type hash-table
                 :documentation
                 "Hash of variable declarations keyed by variable name.")
   (macros :initarg :macros :copier copy-seq
           :initform nil :type (list (cons string string) *)
           :documentation "Association list of Names and values of macros.")
   (globals :initarg :globals :accessor globals :copier copy-seq
            :initform nil :type (list (cons string string) *)
            :documentation "Association list of names and values of globals.")))


;;; Handling header information (formerly "Michondria")
(defgeneric add-type (software type)
  (:documentation "Add TYPE to `types' of SOFTWARE, unique by hash."))
(defmethod add-type ((obj clang) type)
  (unless (or (null type) (member type (types obj) :key {aget :hash}))
    (setf (genome obj)
          (concatenate 'string
            (aget :decl type)
            (genome obj)))
    (push type (types obj)))
  obj)

(defmethod find-type ((obj clang) hash)
  (find-if {= hash} (types obj) :key {aget :hash}))

(defgeneric add-macro (software name body)
  (:documentation "Add the macro if NAME is new to SOFTWARE."))
(defmethod add-macro ((obj clang) (name string) (body string))
  (unless (member name (macros obj) :test #'string= :key #'car)
    (setf (genome obj)
          (concatenate 'string
            (format nil "#define ~a~&" body)
            (genome obj)))
    (push (cons name body) (macros obj)))
  obj)

(defgeneric add-include (software include)
  (:documentation "Add an #include directive for a INCLUDE to SOFTWARE."))
(defmethod add-include ((obj clang) (include string))
  (unless (member include (includes obj) :test #'string=)
    (with-slots (genome) obj
      (setf genome (concatenate 'string
                     (format nil "#include ~a~&" include)
                     (genome obj))))
    (push include (includes obj)))
  obj)


;;; Constants
(define-constant +c-numeric-types+
    '("char" "short" "int" "long" "float" "double" "long double")
  :test #'equalp
  :documentation "C Numeric type names.")

(define-constant +c-relational-operators+
    '("<" "<=" "==" "!=" ">=" ">")
  :test #'equalp
  :documentation "C Relational operators.")

(define-constant +c-arithmetic-binary-operators+
    '("+" "-" "*" "/" "%")
  :test #'equalp
  :documentation "C arithmetic operators on two arguments.")

(define-constant +c-arithmetic-assignment-operators+
    '("+=" "-=" "*=" "/=" "%=")
  :test #'equalp
  :documentation "C arithmetic assignment operators.")

(define-constant +c-bitwise-binary-operators+
    '("&" "|" "^" "<<" ">>")
  :test #'equalp
  :documentation "C bitwise operators on two arguments.")

(define-constant +c-bitwise-assignment-operators+
    '("&=" "|=" "^=" "<<=" ">>=")
  :test #'equalp
  :documentation "C bitwise assignment operators.")

(define-constant +c-arithmetic-unary-operators+
    '("++" "--")
  :test #'equalp
  :documentation "C arithmetic operators on one arguments.")

(define-constant +c-bitwise-unary-operators+
    '("~" "!")
  :test #'equalp
  :documentation "C bitwise operators on one arguments.")

(define-constant +c-sign-unary-operators+
    '("+" "-" )
  :test #'equalp
  :documentation "C sign operators on one arguments.")

(define-constant +c-pointer-unary-operators+
    '("&" "*" )
  :test #'equalp
  :documentation "C pointer operators on one arguments.")

(define-constant +c-variable-modifiers+
    '("const" "enum" "extern" "long" "register" "short" "signed"
      "static" "struct" "unsigned" "volatile")
  :test #'equalp
  :documentation "C variable modifiers.")


;; Targeting functions
(defun restrict-targets (software full-stmt same-class)
  "Return good, bad, then callbacks with full-stmt/same-class
restrictions. For use by targeter functions/execute picks."
  (labels ((filter (asts) (if full-stmt
                              (remove-if-not {aget :full-stmt} asts)
                              asts)))
    (let* ((then (if same-class
                     (lambda (stmt asts)
                       (or (with-class-filter (get-ast-class software stmt)
                             asts)
                           (with-class-filter (get-ast-class software stmt)
                             (asts software))))
                     (lambda (stmt asts) (declare (ignorable stmt)) asts)))
           (good (lambda () (or (filter (good-stmts software))
                                (stmts software))))
           (bad  (lambda () (or (filter (bad-stmts software))
                                (stmts software)))))
      (list good bad then))))

(defun pick-bad-good (software &optional full-stmt same-class)
  "Pick a bad AST, then a good one."
  (destructuring-bind (good bad then)
      (restrict-targets software full-stmt same-class)
    (execute-picks bad then good)))

(defun pick-bad-bad (software &optional full-stmt same-class)
  "Pick a bad AST, then another bad one."
  (destructuring-bind (good bad then)
      (restrict-targets software full-stmt same-class)
    (declare (ignorable good))
    (execute-picks bad then bad)))

(defun pick-bad-only (software &optional full-stmt)
  "Pick a bad AST."
  (destructuring-bind (good bad then)
      (restrict-targets software full-stmt nil)
    (declare (ignorable good then))
    (execute-picks bad)))


;;; Mutations
(defclass clang-mutation (mutation) ())

(defgeneric build-op (mutation software)
  (:documentation "Build clang-mutate operation from a mutation."))

;; Insert
(define-mutation clang-insert (clang-mutation)
  ((targeter :initform #'pick-bad-good)))

(defmethod build-op ((mutation clang-insert) software)
  `((:insert-value . ,(targets mutation))))

(define-mutation clang-insert-full (clang-insert)
  ((targeter :initform {pick-bad-good _ t nil})))

(define-mutation clang-insert-same (clang-insert)
  ((targeter :initform {pick-bad-good _ nil t})))

(define-mutation clang-insert-full-same (clang-insert)
  ((targeter :initform {pick-bad-good _ t t})))

;;; Swap
(define-mutation clang-swap (clang-mutation)
  ((targeter :initform #'pick-bad-bad)))

(defmethod build-op ((mutation clang-swap) software)
  ;; Sort in reverse AST order so operations won't step on each other
  (sort `((:set (:stmt1 . ,(aget :stmt1 (targets mutation)))
                (:stmt2 . ,(aget :stmt2 (targets mutation))))
          (:set (:stmt1 . ,(aget :stmt2 (targets mutation)))
                (:stmt2 . ,(aget :stmt1 (targets mutation)))))
        #'> :key [{aget :stmt1} #'cdr]))

(define-mutation clang-swap-full (clang-swap)
  ((targeter :initform {pick-bad-bad _ t nil})))

(define-mutation clang-swap-same (clang-swap)
  ((targeter :initform {pick-bad-bad _ nil t})))

(define-mutation clang-swap-full-same (clang-swap)
  ((targeter :initform {pick-bad-bad _ t t})))

;;; Replace
(define-mutation clang-replace (clang-mutation)
  ((targeter :initform #'pick-bad-good)))

(defmethod build-op ((mutation clang-replace) software)
  `((:set . ,(targets mutation))))

(define-mutation clang-replace-full (clang-replace)
  ((targeter :initform {pick-bad-good _ t nil})))

(define-mutation clang-replace-same (clang-replace)
  ((targeter :initform {pick-bad-good _ nil t})))

(define-mutation clang-replace-full-same (clang-replace)
  ((targeter :initform {pick-bad-good _ t t})))

;;; Cut
(define-mutation clang-cut (clang-mutation)
  ((targeter :initform #'pick-bad-only)))

(defmethod build-op ((mutation clang-cut) software)
  `((:cut . ,(targets mutation))))

(define-mutation clang-cut-full (clang-cut)
  ((targeter :initform {pick-bad-only _ t})))

;;; Set Range
(define-mutation clang-set-range (clang-mutation) ())

(defmethod build-op ((mutation clang-set-range) software)
  `((:set-range . ,(targets mutation))))

;;; Nop
(define-mutation clang-nop (clang-mutation) ())

(defmethod build-op ((mutation clang-nop) software)
  nil)

;; The -same variants only exist for symmetry (which makes it easier to
;; build the CDF). Since cut only picks one AST the same-class
;; constraint has no effect.
(define-mutation clang-cut-same (clang-cut)
  ((targeter :initform #'pick-bad-only)))
(define-mutation clang-cut-full-same (clang-cut-full)
  ((targeter :initform {pick-bad-only _ t})))

;;; Cut Decl
(define-mutation cut-decl (clang-mutation)
  ((targeter :initform #'pick-cut-decl)))

(defun pick-cut-decl (clang)
  (let ((decls (with-class-filter "DeclStmt" (asts clang))))
    (if (not decls)
        'did-nothing
        `((:stmt1 . ,(random-ast decls))))))

(defmethod build-op ((mutation cut-decl) clang)
  (if (not (eq (targets mutation) 'did-nothing))
      (let* ((decl (aget :stmt1 (targets mutation)))
             (the-block (enclosing-block clang decl))
             (old-names (aget :declares (get-ast clang decl)))
             (uses (mappend (lambda (x) (get-children-using clang x the-block))
                            old-names))
             (vars (remove-if {find _ old-names :test #'equal}
                              (get-vars-in-scope clang
                                (if uses (car uses) the-block))))
             (var (mapcar (lambda (old-name)
                            (declare (ignorable old-name))
                            (if vars
                                (random-elt vars)
                                "/* no vars before first use of cut-decl */"))
                          old-names)))
        (delete-decl-stmts clang the-block `((,decl . ,var))))))

;;; Swap Decls
(define-mutation swap-decls (clang-swap)
  ((targeter :initform #'pick-swap-decls)))

(defmethod build-op :around ((mutation swap-decls) software)
  (if (not (eq (targets mutation) 'did-nothing))
      (call-next-method)))

(defun pick-two (things)
  (let ((this (random-elt things))
        (that (random-elt things)))
    (if (equal this that)
        (pick-two things)
        (values this that))))

(defun pick-swap-decls (clang)
  (labels
      ((pick-from-block (the-block)
         (if (equal the-block 0)
             'did-nothing
             (let ((decls
                    (mapcar {aget :counter}
                            (with-class-filter "DeclStmt"
                              (mapcar {get-ast clang}
                                      (aget :stmt-list
                                            (get-ast clang the-block)))))))
               (if (> 2 (length decls))
                   (pick-from-block (enclosing-block clang the-block))
                   (multiple-value-bind (stmt1 stmt2) (pick-two decls)
                     `((:stmt1 . ,stmt1) (:stmt2 . ,stmt2))))))))
    (pick-from-block (enclosing-block clang
                                      (random-ast (bad-stmts clang))))))

;;; Rename variable
(define-mutation rename-variable (clang-mutation)
  ((targeter :initform #'pick-rename-variable)))

(defun pick-rename-variable (clang)
  (let* ((stmt (random-ast (bad-stmts clang)))
         (used (get-used-variables clang stmt)))
    (if used
        (let* ((old-var (random-elt used))
               (new-var (random-elt
                         (or (remove-if {equal old-var}
                                        (get-vars-in-scope clang stmt))
                             (list old-var))))
               (stmt1 (enclosing-full-stmt-or-block clang stmt)))
          `((:stmt1 . ,stmt1) (:old-var . ,old-var) (:new-var . ,new-var)))
        'did-nothing)))

(defmethod build-op ((mutation rename-variable) software)
  (if (not (eq (targets mutation) 'did-nothing))
      (let ((stmt1 (aget :stmt1 (targets mutation)))
            (old-var (aget :old-var (targets mutation)))
            (new-var (aget :new-var (targets mutation))))
        `((:set
           (:stmt1 . ,stmt1)
           (:literal1 . ,(rebind-uses software
                                      stmt1
                                      (list (cons old-var new-var)))))))))


;;; Clang methods
(defvar *clang-max-json-size* 104857600
  "Maximum size of output accepted from `clang-mutate'.")

(defgeneric update-body (software &key)
  (:documentation "Update SOFTWARE body from ASTs."))

(defgeneric update-asts (software &key)
  (:documentation "Update the store of asts associated with SOFTWARE."))

(defgeneric asts (software)
  (:documentation "Return a list of all asts in SOFTWARE."))

(defgeneric stmts (software)
  (:documentation "Return a list of all statement asts in SOFTWARE."))

(defgeneric good-stmts (software)
  (:documentation "Return a list of all good statement asts in SOFTWARE."))

(defgeneric bad-stmts (software)
  (:documentation "Return a list of all bad statement asts in SOFTWARE."))

(defgeneric get-ast (software id)
  (:documentation "Return the statement in SOFTWARE indicated by ID."))

(defgeneric mutation-types-clang (clang)
  (:documentation "Return a list of mutation types for the CLANG software
object as well as their relative probabilities"))

(defgeneric pick-mutation-type (clang)
  (:documentation "Pick the type of mutation to be performed by the CLANG
software object"))

(defmethod size ((obj clang))
  (with-slots (asts) obj
    (unless asts (update-asts obj))
    (length asts)))

(defvar *clang-json-required-fields*
  '(:ast-class          :counter           :unbound-vals
    :unbound-funs       :types             :stmt-list
    :src-text           :parent-counter    :macros
    :guard-stmt         :full-stmt         :begin-src-line
    :end-src-line       :begin-src-col     :end-src-col
    :begin-addr         :end-addr          :includes
    :declares           :scopes            :is-decl
    :in-macro-expansion :opcode            :children)
  "JSON database entry fields required for clang software objects.")

(defvar *clang-json-required-aux*
  '(:asts :types :decls)
  "JSON database AuxDB entries required for clang software objects.")

(defmethod (setf genome) :before (new (obj clang))
  (with-slots (asts fitness) obj
    (setf asts nil
          fitness nil)))

(defmethod update-asts ((obj clang)
                        &key clang-mutate-args
                        &aux (decls (make-hash-table :test #'equal)))
  (with-slots
        (asts macros includes types functions prototypes declarations genome)
      obj
    ;; incorporate ASTs.
    (iter (for ast in (restart-case
                          (clang-mutate obj
                            (list* :sexp
                                   (cons :fields *clang-json-required-fields*)
                                   (cons :aux *clang-json-required-aux*)
                                   clang-mutate-args))
                        (dummy-asts ()
                          :report "Return dummy ASTs"
                          '(((:counter . 0) (:ast-class "Failure"))))))
          ;; NOTE: Relies on the invariant that the ASTs returned by
          ;; clang-mutate are in sorted order.
          (mapc (lambda (var) (nconcf (gethash var decls nil) (list ast)))
                (ast-declares ast))
          (cond
            ((aget :counter ast) (collect ast into body))
            ((ast-type-p ast) (collect ast into m-types))
            ((aget :decl-name ast)
             (nconcf (gethash (aget :decl-name ast) decls nil) (list ast)))
            (:otherwise (error "Unrecognized ast.~%~S" ast)))
          (when (aget :body ast) (collect ast into funs))
          (when (string= "Function" (aget :ast-class ast))
            (collect ast into protos))
          (mapc (lambda (macro)
                  (adjoining (cons (first macro) (second macro)) into m-macros
                             test (lambda (a b) (string= (car a) (car b)))))
                (aget :macros ast))
          (mapc (lambda (include)
                  (adjoining include into m-includes test #'string=))
                (aget :includes ast))
          (finally (setf asts (coerce body 'vector)
                         types m-types
                         macros m-macros
                         includes m-includes
                         functions funs
                         prototypes protos
                         declarations decls))))
  obj)

(defmethod update-body ((obj clang) &key)
  ;; Program body.
  ;;
  ;; Generate the bulk of the program text by joining all global
  ;; declarations (including functions) together in the same order
  ;; they originally appeared.
  (setf (genome obj)
        (mapconcat {aget :decl-text}
                   (sort
                    (remove-if-not {aget :decl-text} (asts obj))
                    (lambda (x y)
                      (or (< (first x) (first y))
                          (and (= (first x) (first y))
                               (< (second x) (second y)))))
                    :key «{aget :begin-src-line} {aget :begin-src-col}»)
                   (string #\Newline))))

(defgeneric from-file-exactly (software path)
  (:documentation
   "Initialize SOFTWARE from PATH as done by `from-string-exactly'."))

(defmethod from-file-exactly ((obj clang) path)
  (setf (ext obj) (pathname-type (pathname path)))
  (from-string-exactly obj (file-to-string path)))

(defmethod from-file ((obj clang) path)
  (setf (ext obj) (pathname-type (pathname path)))
  (from-string obj (file-to-string path))
  obj)

(defgeneric from-string-exactly (software string)
  (:documentation
   "Create a clang software object from a given C file's string representation.
The software object's genome will exactly match the input file's
contents aside from some simple transformations to ease downstream
processing.

Currently the only such transformation is to split variable
declarations onto multiple lines to ease subsequent decl mutations."))

(defun balanced-parens (pos &aux (deep 0))
  (iter (while (< pos (length ppcre::*string*)))
        (as char = (aref ppcre::*string* pos))
        (case char
          (#\( (incf deep))
          (#\) (when (zerop deep) (return-from balanced-parens nil))
               (decf deep))
          (#\,
           (when (zerop deep) (return-from balanced-parens pos)))
          (#\;
           (return-from balanced-parens pos)))
        (incf pos))
  (if (zerop deep) pos nil))

(defun split-balanced-parens (string &aux (start 0) (str-len (length string)))
  (->> (iter (for (values match-start match-end match-begs match-ends)
                  = (scan '(:register (:filter balanced-parens)) string
                          :start start))
             (declare (ignorable match-start))
             (while (and match-end (< start str-len)))
             (collecting (subseq string (aref match-begs 0)
                                 (aref match-ends 0)))
             (if (< start match-end)
                 (setf start match-end)
                 (incf start)))
       (remove-if [#'zerop #'length])))

(defmethod from-string-exactly ((obj clang) string &aux (index 0))
  ;; TODO: Improve clang-mutate so we no longer need this hack.
  ;; Find every probable multi-variable declaration, then split.
  (let ((regex
         (create-scanner
          `(:sequence                   ; Preceding newline.
            (:char-class #\{ #\} #\;)
            (:greedy-repetition 0 nil :whitespace-char-class) #\newline
            (:register                  ; Type name.
             (:sequence
              (:greedy-repetition 0 nil :whitespace-char-class)
              (:greedy-repetition
               0 nil
               (:sequence               ; Type name modifiers.
                (:alternation ,@+c-variable-modifiers+)
                :whitespace-char-class))
              (:greedy-repetition 1 nil :word-char-class)
              (:greedy-repetition 0 1 #\*)
              (:greedy-repetition 1 nil :whitespace-char-class)))
            (:register                  ; All variables.
             (:sequence
              (:greedy-repetition       ; First variable.
               1 nil
               (:char-class #\* :whitespace-char-class :word-char-class))
              (:greedy-repetition
               0 1
               (:register               ; Optional initialization.
                (:sequence #\=
                           (:greedy-repetition 0 nil :whitespace-char-class)
                           (:filter balanced-parens))))
              #\, (:greedy-repetition 1 nil (:inverted-char-class #\;))
              (:greedy-repetition       ; Subsequent variables.
               0 1
               (:register
                (:sequence #\=          ; Optional initialization.
                           (:greedy-repetition 0 nil :whitespace-char-class)
                           (:filter balanced-parens))))))
            #\; (:greedy-repetition 0 nil :whitespace-char-class) #\newline))))
    (iter (for (values match-start match-end match-type match-vars)
               = (scan regex string :start index))
          (while match-end)
          ;; C has strict limits on valid strings, so we don't have to
          ;; worry about being inside of a string.
          (concatenating
           (concatenate 'string
             (subseq string index (aref match-type 0))
             (let* ((type
                     (string-right-trim (list #\Space #\Tab #\Newline)
                                        (subseq string
                                                (aref match-type 0)
                                                (aref match-type 1)))))
               (mapconcat [{concatenate 'string type " "}
                           {string-left-trim (list #\Space #\Tab #\Newline)}]
                          (split-balanced-parens
                           (subseq string (aref match-vars 0)
                                   (aref match-vars 1)))
                          (coerce (list #\Semicolon #\Newline) 'string))))
           into out-str)
          (setf index (aref match-vars 1))
          (finally (setf (genome obj)
                         (concatenate 'string
                           out-str (subseq string index))))))
  obj)

(defgeneric ast-type-p (ast)
  (:documentation "Check if AST is a type ast."))

(defmethod ast-type-p ((ast list))
  (and (assoc :hash ast)
       (assoc :reqs ast)
       (assoc :type ast)))

(defmethod from-string ((obj clang) string)
  ;; Load the raw string and generate a json database
  (from-string-exactly obj string)
  obj)

(defmethod asts ((obj clang))
  (with-slots (asts) obj
    (unless asts (update-asts obj))
    (coerce asts 'list)))

(defmethod (setf asts) (new (obj clang))
  (with-slots (asts) obj (setf asts new)))

(defmethod get-ast ((obj clang) (id integer))
  (with-slots (asts) obj
    (unless asts (update-asts obj))
    (aref asts (1- id))))

(defmethod functions ((obj clang))
  (with-slots (asts functions) obj
    (unless asts (update-asts obj))
    functions))

(defmethod prototypes ((obj clang))
  (with-slots (asts prototypes) obj
    (unless asts (update-asts obj))
    prototypes))

(defmethod includes ((obj clang))
  (with-slots (asts includes) obj
    (unless asts (update-asts obj))
    includes))

(defmethod (setf includes) (new (obj clang))
  (with-slots (includes) obj (setf includes new)))

(defmethod macros ((obj clang))
  (with-slots (asts macros) obj
    (unless asts (update-asts obj))
    macros))

(defmethod (setf macros) (new (obj clang))
  (with-slots (macros) obj (setf macros new)))

(defmethod types ((obj clang))
  (with-slots (asts types) obj
    (unless asts (update-asts obj))
    types))

(defmethod (setf types) (new (obj clang))
  (with-slots (types) obj (setf types new)))

(defmethod declarations ((obj clang))
  (with-slots (asts declarations) obj
    (unless asts (update-asts obj))
    declarations))

(defmethod (setf declarations) (new (obj clang))
  (with-slots (declarations) obj (setf declarations new)))

(defmethod recontextualize ((clang clang) snippet pt)
  (let ((text (bind-free-vars clang snippet pt)))
    (if (full-stmt-p clang pt)
        (format nil "~a~%" (add-semicolon-if-needed text))
        (format nil "~a" text))))

(defun do-not-filter ()
  (lambda (asts) asts))

(defun with-class-filter (class asts)
  (remove-if-not [{equal class} {aget :ast-class } ] asts))

(defmethod get-parent-decls ((clang clang) ast)
  (remove-if-not {aget :is-decl} (get-parent-asts clang ast)))

(defmethod stmts ((clang clang))
  "Remove each AST which is a decl or has a non-function parent decl"
  (remove-if (lambda (ast)
               (or (aget :is-decl ast)
                   (remove-if [{equal "Function"}{aget :ast-class}]
                              (get-parent-decls clang ast))))
             (asts clang)))

(defmethod good-stmts ((clang clang))
  (stmts clang))

(defmethod bad-stmts ((clang clang))
  (stmts clang))

(defun random-ast (asts)
  (aget :counter (random-elt asts)))

(defmethod pick-good ((clang clang))
  (random-ast (good-stmts clang)))

(defmethod pick-bad ((clang clang))
  (random-ast (bad-stmts clang)))

(defmethod get-ast-class ((clang clang) stmt)
  (aget :ast-class (get-ast clang stmt)))

(defun execute-picks (get-asts1 &optional connector get-asts2)
  (let* ((stmt1 (when get-asts1
                  (random-ast (funcall get-asts1))))
         (stmt2 (when get-asts2
                  (random-ast (funcall connector stmt1
                                       (funcall get-asts2))))))
    (acons :stmt1 stmt1
       (if stmt2 (acons :stmt2 stmt2 nil) nil))))

(defvar *clang-full-stmt-bias* 0.75
  "The probability that a mutation will operate on a full statement.")

(defvar *clang-same-class-bias* 0.75
  "The probability that a mutation uses AST class matching.")

(defvar *decl-mutation-bias* 0.1
  "The probability that a mutation will target a variable declaration.")

(defvar *clang-mutation-types*
  `(clang-cut      clang-cut-same     clang-cut-full      clang-cut-full-same
    clang-insert   clang-insert-same  clang-insert-full   clang-insert-full-same
    clang-swap     clang-swap-same    clang-swap-full     clang-swap-full-same
    clang-replace  clang-replace-same clang-replace-full  clang-replace-full-same
    cut-decl swap-decls rename-variable))

(defvar *free-var-decay-rate* 0.3
  "The decay rate for choosing variable bindings.")

(defvar *matching-free-var-retains-name-bias* 0.75
  "The probability that if a free variable's original name matches a name
already in scope, it will keep that name.")

(defvar *matching-free-function-retains-name-bias* 0.95
  "The probability that if a free functions's original name matches a name
already in scope, it will keep that name.")

(defvar *crossover-function-probability* 0.25
  "The probability of crossing a function during whole-program crossover.")

(defun combine-with-bias (bias heads tails)
  (append
   (mapcar (lambda (pair) (cons (car pair) (* (cdr pair) bias))) heads)
   (mapcar (lambda (pair) (cons (car pair) (* (cdr pair) (- 1 bias)))) tails)))

(defmethod decl-mutation-types-clang ((clang clang))
  (uniform-probability '(cut-decl swap-decls rename-variable)))

(defmethod basic-mutation-types-clang ((clang clang))
  (let* ((weights
          (remove-if #'null
           (loop :for mutation-type :in *clang-mutation-types*
              :collecting
                (cond ((member mutation-type
                               `(clang-cut-full-same clang-insert-full-same
                                 clang-swap-full-same clang-replace-full-same))
                       (cons mutation-type
                             (* *clang-full-stmt-bias*
                                *clang-same-class-bias*)))
                      ((member mutation-type
                               `(clang-cut-full  clang-insert-full
                                 clang-swap-full clang-replace-full))
                       (cons mutation-type
                             (* *clang-full-stmt-bias*
                                (- 1 *clang-same-class-bias*))))
                      ((member mutation-type
                               `(clang-cut-same  clang-insert-same
                                 clang-swap-same clang-replace-same))
                       (cons mutation-type
                             (* *clang-same-class-bias*
                                (- 1 *clang-full-stmt-bias*))))
                      ((member mutation-type
                               `(clang-cut clang-insert
                                 clang-swap clang-replace))
                       (cons mutation-type
                             (* (- 1 *clang-same-class-bias*)
                                (- 1 *clang-full-stmt-bias*))))
                      (t nil)))))
         (total-weight (reduce #'+ weights :key #'cdr)))
    (mapc (lambda (w) (setf (cdr w) (/ (cdr w) total-weight))) weights)))

(defmethod mutation-types-clang ((clang clang))
  (combine-with-bias *decl-mutation-bias*
                     (decl-mutation-types-clang clang)
                     (basic-mutation-types-clang clang)))

(defmethod pick-mutation-type ((clang clang))
  (random-pick (cdf (mutation-types-clang clang))))

(defmethod mutate ((clang clang))
  (unless (> (length (stmts clang)) 0)
    (error (make-condition 'mutate :text "No valid statements" :obj clang)))

  (let ((mutation (make-instance (pick-mutation-type clang) :object clang)))
    (apply-mutation clang mutation)
    (values clang mutation)))

(defmethod recontextualize-mutation ((clang clang) mutation)
  (loop :for (op . properties) :in (build-op mutation clang)
     :collecting
     (let ((stmt1  (aget :stmt1  properties))
           (stmt2  (aget :stmt2  properties))
           (value1 (aget :value1 properties))
           (literal1 (aget :literal1 properties)))
       (case op
         ((:cut :set :insert-value)
          (cons op
                (cons (cons :stmt1 stmt1)
                      (if (or stmt2 value1 literal1)
                          `((:value1 .
                             ,(or literal1
                                  (recontextualize clang
                                                   (if stmt2
                                                       (get-ast clang stmt2)
                                                     value1)
                                                   stmt1))))))))
         ;; Other ops are passed through without changes
         (otherwise (cons op properties))))))

(defun apply-mutation-ops (software ops)
  "Run clang-mutate with a list of mutation operations, and update the
genome."

  ;; If we multiplex multiple software objects onto one
  ;; clang-mutate invocation, they will need to track their own TU
  ;; ids.  With one software object, it will always be TU 0.
  (let ((tu 0))
    (setf (genome software)
          (clang-mutate software '(:scripted) :script
                        (format nil "reset ~a; ~{~a; ~}preview ~a"
                                tu
                                (mapcar {mutation-op-to-cmd tu} ops)
                                tu)))
    software))

(defmethod apply-mutation ((software clang)
                           (mutation clang-mutation))
  (restart-case
      (apply-mutation-ops software
                          (recontextualize-mutation software mutation))
    (skip-mutation ()
      :report "Skip mutation and return nil"
      (values nil 1))
    (tidy ()
      :report "Call clang-tidy before re-attempting mutation"
      (clang-tidy software)
      (apply-mutation software mutation))
    (mutate ()
      :report "Apply another mutation before re-attempting mutations"
      (mutate software)
      (apply-mutation software mutation))))

;; Convenience form for compilation fixers, crossover, etc
(defmethod apply-mutation ((clang clang) (op list))
  (apply-mutation clang (make-instance (car op) :targets (cdr op))))

(defmethod mutation-key ((obj clang) op)
  ;; Return a list of the mutation type, and the classes of any stmt1 or
  ;; stmt2 arguments.
  (cons
   (type-of op)
   (mapcar [{aget :ast-class} {get-ast obj} #'cdr]
           (remove-if-not [#'numberp #'cdr]
                          (remove-if-not [{member _ (list :stmt1 :stmt2)} #'car]
                                         (remove-if-not #'consp (targets op)))))))

(defun mutation-op-to-cmd (tu op)
  (labels ((ast (tag) (format nil "~a.~a" tu (aget tag (cdr op))))
           (str (tag) (json:encode-json-to-string (aget tag (cdr op)))))
    (ecase (car op)
      (:cut
       (format nil "cut ~a" (ast :stmt1)))
      (:insert
       (format nil "get ~a as $stmt; before ~a $stmt"
               (ast :stmt1) (ast :stmt2)))
      (:insert-value
       (format nil "before ~a ~a" (ast :stmt1) (str :value1)))
      (:insert-value-after
       (format nil "after ~a ~a" (ast :stmt1) (str :value1)))
      (:swap
       (format nil "swap ~a ~a" (ast :stmt1) (ast :stmt2)))
      (:set
       (format nil "set ~a ~a" (ast :stmt1) (str :value1)))
      (:set2
       (format nil "set ~a ~a ~a ~a"
               (ast :stmt1) (str :value1)
               (ast :stmt2) (str :value2)))
      (:set-range
       (format nil "set-range ~a ~a ~a"
               (ast :stmt1) (ast :stmt2) (str :value1)))
      (:set-func
       (format nil "set-func ~a ~a" (ast :stmt1) (str :value1)))
      (:ids
       (format nil "ids ~a" tu))
      (:list
       (format nil "list ~a" tu))
      (:sexp
       (let ((aux (if (aget :aux (cdr op))
                      (format nil "aux=~{~a~^,~}" (aget :aux (cdr op)))
                      ""))
             (fields (if (aget :fields (cdr op))
                         (format nil "fields=~{~a~^,~}" (aget :fields (cdr op)))
                         "")))
         (if (aget :stmt1 (cdr op))
             (format nil "ast ~a ~a" (ast :stmt1) fields)
             (format nil "sexp ~a ~a ~a" (ast :stmt1) fields aux)))))))

(defmethod clang-mutate ((obj clang) op
                         &key script
                         &aux value1-file value2-file)
  (assert (ext obj) (obj)
          "Software object ~a has no extension, required by clang-mutate."
          obj)
  (with-temp-file-of (src-file (ext obj)) (genome obj)
    (labels ((command-opt (command)
               (ecase command
                 (:cut "-cut")
                 (:insert "-insert")
                 (:insert-value "-insert-value")
                 (:swap "-swap")
                 (:set "-set")
                 (:set2 "-set2")
                 (:set-range "-set-range")
                 (:set-func  "-set-func")
                 (:ids "-ids")
                 (:list "-list")
                 (:sexp "-sexp")
                 (:scripted "-interactive -silent")))
             (option-opt (pair)
               (let ((option (car pair))
                     (value (cdr pair)))
                 (ecase option
                   (:stmt1 (format nil "-stmt1=~d" value))
                   (:stmt2 (format nil "-stmt2=~d" value))
                   (:fields (format nil "-fields=~a"
                                    (mapconcat #'field-opt value ",")))
                   (:aux (format nil "-aux=~a"
                                 (mapconcat #'aux-opt value ",")))
                   (:value1
                    (setf value1-file (temp-file-name))
                    (string-to-file value value1-file)
                    (format nil "-file1=~a" value1-file))
                   (:value2
                    (setf value2-file (temp-file-name))
                    (string-to-file value value2-file)
                    (format nil "-file2=~a" value2-file))
                   (:bin (format nil "-binary=~a" value))
                   (:dwarf-src-file-path
                    (format nil "-dwarf-filepath-mapping=~a=~a"
                            value src-file))
                   (:cfg "-cfg"))))
             (field-opt (field)
               (ecase field
                 (:counter "counter")
                 (:declares "declares")
                 (:is-decl "is_decl")
                 (:parent-counter "parent_counter")
                 (:ast-class "ast_class")
                 (:src-file-name "src_file_name")
                 (:begin-src-line "begin_src_line")
                 (:begin-src-col "begin_src_col")
                 (:end-src-line "end_src_line")
                 (:end-src-col "end_src_col")
                 (:src-text "src_text")
                 (:guard-stmt "guard_stmt")
                 (:full-stmt "full_stmt")
                 (:unbound-vals "unbound_vals")
                 (:unbound-funs "unbound_funs")
                 (:macros "macros")
                 (:types "types")
                 (:stmt-list "stmt_list")
                 (:binary-file-path "binary_file_path")
                 (:scopes "scopes")
                 (:begin-addr "begin_addr")
                 (:end-addr "end_addr")
                 (:includes "includes")
                 (:opcode "opcode")
                 (:children "children")
                 (:successors "successors")
                 (:begin-off "begin_off")
                 (:end-off "end_off")
                 (:begin-norm-off "begin_norm_off")
                 (:end-norm-off "end_norm_off")
                 (:orig-text "orig_text")
                 (:binary-contents "binary_contents")
                 (:base-type "base_type")
                 (:bit-field-width "bit_field_width")
                 (:array-length "array_length")
                 (:in-macro-expansion "in_macro_expansion")
                 (:expr-type "expr_type")))
             (aux-opt (aux)
               (ecase aux
                 (:types "types")
                 (:asts "asts")
                 (:decls "decls")
                 (:none "none"))))
    (let ((clang-mutate-outfile (temp-file-name))
          (json:*identifier-name-to-key* 'se-json-identifier-name-to-key))
      (unwind-protect
        (multiple-value-bind (stdout stderr exit)
            (shell-with-input script
                              "clang-mutate ~a ~{~a~^ ~} ~a -- ~{~a~^ ~} > ~a"
                              (command-opt (car op))
                              (mapcar #'option-opt (cdr op))
                              src-file
                              (flags obj)
                              clang-mutate-outfile)
          (declare (ignorable stdout stderr))
          ;; NOTE: The clang-mutate executable will sometimes produce
          ;;       usable output even on a non-zero exit, e.g., usable
          ;;       json or successful mutations but an exit of 1
          ;;       because of compiler errors.  To ensure these cases
          ;;       are still usable, we only signal mutation errors on
          ;;       specific exit values.
          (when (find exit '(131 132 134 136 139))
            (error
             (make-condition 'mutate
               :text (format nil "clang-mutate core dump, ~d," exit)
               :obj obj :op op)))
          ;; NOTE: If clang-mutate output exceeds 10 MB, this is likely due
          ;; to an insertion which is technically legal via the standard,
          ;; but is actually meaningless.  This tends to happen with array
          ;; initialization forms (e.g { 254, 255, 256 ... }) being inserted
          ;; and interpreted as a block.  Throw an error to clear the genome.
          (with-open-file (clang-mutate-out clang-mutate-outfile
                           :element-type '(unsigned-byte 8))
            (when (> (file-length clang-mutate-out) *clang-max-json-size*)
              (error (make-condition 'mutate
                       :text (format nil "clang-mutate output exceeds 100 MB.")
                       :obj obj :op op))))
          (values
           (case (car op)
             (:sexp
              (with-open-file (clang-mutate-out clang-mutate-outfile)
                (read clang-mutate-out)))
             ((:ids :list) (file-to-string clang-mutate-outfile))
             (t (file-to-string clang-mutate-outfile)))
           exit))
      ;; Cleanup forms.
      (when (probe-file clang-mutate-outfile)
        (delete-file clang-mutate-outfile))
      (when (and value1-file (probe-file value1-file))
        (delete-file value1-file))
      (when (and value2-file (probe-file value2-file))
        (delete-file value2-file)))))))


;;; AST Utility functions
(defun ast-to-source-range (ast)
  "Convert AST to pair of SOURCE-LOCATIONS."
  (when ast
    (make-instance 'source-range
      :begin (make-instance 'source-location
               :line (aget :begin-src-line ast)
               :column (aget :begin-src-col ast))
      :end (make-instance 'source-location
             :line (aget :end-src-line ast)
             :column (aget :end-src-col ast)))))

(defmethod asts-containing-source-location ((obj clang) (loc source-location))
  (when loc
    (remove-if-not [{contains _ loc} #'ast-to-source-range] (asts obj))))

(defmethod asts-contained-in-source-range ((obj clang) (range source-range))
  (when range
    (remove-if-not [{contains range} #'ast-to-source-range] (asts obj))))

(defmethod asts-intersecting-source-range ((obj clang) (range source-range))
  (when range
    (remove-if-not [{intersects range} #'ast-to-source-range] (asts obj))))

(defmethod line-breaks ((clang clang))
  (cons 0 (loop :for char :in (coerce (genome clang) 'list) :as index
                :from 0
                :when (equal char #\Newline) :collect index)))

(defgeneric parent-ast-p (software possible-parent-ast ast)
  (:documentation
   "Check if POSSIBLE-PARENT-AST is a parent of AST in SOFTWARE."))

(defmethod parent-ast-p ((clang clang) possible-parent-ast ast)
  (cond ((= (aget :counter possible-parent-ast)
            (aget :counter ast)) t)
        ((= (aget :parent-counter ast) 0) nil)
        (t (parent-ast-p clang
                         possible-parent-ast
                         (get-ast clang (aget :parent-counter ast))))))

(defmacro define-ast-number-or-nil-default-dispatch (symbol &rest additional)
  "Define default dispatch for clang ast methods when ast is an integer or nil."
  (let ((additional-args
         (mapcar (lambda (a) (if (listp a) (car a) a))
                 (remove-if {member _ '(&optional &key)} additional))))
    `(progn
       (defmethod ,symbol ((obj clang) (ast integer) ,@additional)
         (unless (zerop ast)
           (,symbol obj (get-ast obj ast) ,@additional-args)))
       (defmethod ,symbol ((obj clang) (ast (eql nil)) ,@additional)
         (declare (ignorable ,@additional-args))
         nil))))

(define-ast-number-or-nil-default-dispatch get-parent-ast)
(defmethod get-parent-ast ((obj clang) (ast list))
  (let ((parent-counter (aget :parent-counter ast)))
    (and (not (zerop parent-counter)) (get-ast obj parent-counter))))

(defmethod get-parent-asts ((clang clang) (ast list))
  (cond ((= (aget :parent-counter ast) 0) (list ast))
        (t  (append (list ast)
                    (get-parent-asts
                     clang
                     (get-ast clang (aget :parent-counter ast)))))))

(defgeneric get-immediate-children (sosftware ast)
  (:documentation "Return the immediate children of AST in SOFTWARE."))

(defmethod get-immediate-children ((clang clang) (ast list))
  (if (member :children *clang-json-required-fields*)
      (mapcar {get-ast clang} (aget :children ast))
      (get-immediate-children clang (aget :counter ast))))

(defmethod get-immediate-children ((clang clang) (ast integer))
  (if (member :children *clang-json-required-fields*)
      (mapcar {get-ast clang} (aget :children (get-ast clang ast)))
      (remove-if-not [{= ast} {aget :parent-counter}] (asts clang))))

(defgeneric get-parent-full-stmt (software ast)
  (:documentation
   "Return the first ancestor of AST in SOFTWARE which is a full stmt.
Returns nil if no full-stmt parent is found."))

(define-ast-number-or-nil-default-dispatch get-parent-full-stmt)
(defmethod get-parent-full-stmt ((clang clang) (ast list))
  (cond ((aget :full-stmt ast) ast)
        (ast (get-parent-full-stmt clang (get-parent-ast clang ast)))))

(defgeneric wrap-ast (software ast)
  (:documentation "Wrap AST in SOFTWARE in a block."))

(define-ast-number-or-nil-default-dispatch wrap-ast)
(defmethod wrap-ast ((obj clang) (ast list))
  (let* ((old-text (peel-bananas (aget :src-text ast)))
         (new-text (concatenate 'string
                     "{" old-text
                     ;; Check if a semicolon is needed.
                     (if (scan "}\\s*$" old-text) "}" ";}"))))
    (setf (genome obj)
          (clang-mutate obj
                        `(:set (:stmt1 . ,(aget :counter ast))
                               (:value1 . ,new-text)))))
  (update-asts obj)
  obj)

(define-constant +clang-wrapable-parents+
    '("WhileStmt" "IfStmt" "ForStmt")
  :test #'equalp
  :documentation "Types which can be wrapped.")

(defgeneric wrap-child (software ast index)
  (:documentation "Wrap INDEX child of AST in SOFTWARE in blocks."))

(define-ast-number-or-nil-default-dispatch wrap-child (index integer))
(defmethod wrap-child ((obj clang) (ast list) (index integer))
  (if (member (aget :ast-class ast) +clang-wrapable-parents+
              :test #'string=)
      (wrap-ast obj (nth index (get-immediate-children obj ast)))
      (error "Will not wrap children of type ~a, only useful for ~a."
             (aget :ast-class ast) +clang-wrapable-parents+))
  obj)

(defgeneric can-be-made-full-p (software ast)
  (:documentation "Check if AST can be made a full statement in SOFTWARE."))

(define-ast-number-or-nil-default-dispatch can-be-made-full-p)
(defmethod can-be-made-full-p ((obj clang) (ast list))
  (or (and (aget :full-stmt ast)
           (not (zerop (aget :parent-counter ast)))
           (not (member (aget :ast-class ast) '("Function")
                        :test #'string=))
           (let ((parent-class (aget :ast-class (get-parent-ast obj ast))))
             (and
              ;; NOTE: Work around clang-mutate bug in which the
              ;; "CompoundStmt" holding a full function body are both
              ;; considered to be full statements.
              (not (string= "Function" parent-class)))))
      (unless (or (aget :guard-stmt ast)  ; Don't wrap guard statements.
                  (string= "CompoundStmt" ; Don't wrap CompoundStmts.
                           (aget :ast-class ast)))
        (when-let ((parent (get-parent-ast obj ast)))
          ;; Is a child of a statement which might have a hanging body.
          (member (aget :ast-class parent) +clang-wrapable-parents+
                  :test #'string=)))))

(defgeneric enclosing-traceable-stmt (software ast)
  (:documentation
   "Return the first ancestor of AST in SOFTWARE which may be a full stmt.
If a statement is reached which is not itself full, but which could be
made full by wrapping with curly braces, return that."))

(define-ast-number-or-nil-default-dispatch enclosing-traceable-stmt)
(defmethod enclosing-traceable-stmt ((obj clang) (ast list))
  (cond
    ((aget :full-stmt ast) ast)
    ;; Wrap AST in a CompoundStmt to make it full.
    ((can-be-made-full-p obj ast) ast)
    (ast (enclosing-traceable-stmt obj (get-parent-ast obj ast)))
    (:otherwise (values nil nil))))

(defmethod nesting-depth ((clang clang) index &optional orig-depth)
  (let ((depth (or orig-depth 0)))
    (if (= 0 index)
        depth
        (nesting-depth clang (enclosing-block clang index) (1+ depth)))))

(defmethod enclosing-block ((clang clang) index &optional child-index)
  (if (= index 0) (values  0 child-index)
    (let* ((ast (get-ast clang index))
           (blockp (equal (aget :ast-class ast) "CompoundStmt")))
      (if (and blockp child-index)
          (values index child-index)
          (enclosing-block clang (aget :parent-counter ast) index)))))

(defmethod full-stmt-p ((clang clang) stmt)
  ;; NOTE: This assumes that the :full-stmt tag is always populated.
  (aget :full-stmt (get-ast clang stmt)))

(defmethod enclosing-full-stmt ((clang clang) index &optional child-index)
  (if (or (null index) (= index 0)) nil
    (let* ((ast (get-ast clang index))
           (blockp (equal (aget :ast-class ast) "CompoundStmt")))
      (if (and blockp child-index)
          child-index
          (enclosing-full-stmt clang (aget :parent-counter ast) index)))))

(defun get-entry-after (item list)
  (cond ((null list) nil)
        ((not (equal (car list) item)) (get-entry-after item (cdr list)))
        ((null (cdr list)) nil)
        (t (cadr list))))

(defun get-entry-before (item list &optional saw)
  (cond ((null list) nil)
        ((equal (car list) item) saw)
        (t (get-entry-before item (cdr list) (car list)))))

(defmethod block-successor ((clang clang) raw-index)
  (let* ((index (enclosing-full-stmt clang raw-index))
         (block-index (enclosing-block clang index))
         (the-block (get-ast clang block-index))
         (the-stmts (if (= 0 block-index) nil
                        (aget :stmt-list the-block))))
    (get-entry-after index the-stmts)))

(defmethod block-predeccessor ((clang clang) raw-index)
  (let* ((index (enclosing-full-stmt clang raw-index))
         (block-index (enclosing-block clang index))
         (the-block (get-ast clang block-index))
         (the-stmts (if (= 0 block-index) nil
                        (aget :stmt-list the-block))))
    (get-entry-before index the-stmts)))

(defmethod get-ast-text ((clang clang) stmt)
  (aget :src-text (get-ast clang stmt)))

(defun add-semicolon-if-needed (text)
  (if (equal text "") ";"
      ;; Add a semicolon unless the text ends in a } (CompoundStmts, etc)
      ;; or already includes a semicolon (only seen for DeclStmts).
      (if (find (char text (1- (length text)))
                (list #\} #\;))
          text
          (concatenate 'string text ";"))))

(defun process-full-stmt-text (snippet)
  (add-semicolon-if-needed (aget :src-text snippet)))

(defmethod full-stmt-text ((clang clang) raw-index)
  (process-full-stmt-text (get-ast clang
                                   (enclosing-full-stmt clang raw-index))))

(defmethod full-stmt-info ((clang clang) raw-index)
  (let* ((index (enclosing-full-stmt clang raw-index)))
    (if (or (null index) (= 0 index))
        nil
        (get-ast clang index))))

(defmethod full-stmt-successors
    ((clang clang) index &optional do-acc acc blocks)
  (if (or (null index) (= 0 index))
      ;; We've made it to the top-level scope; return the accumulator.
      (reverse (if (null acc)
                   blocks
                   (cons acc blocks)))
      ;; Not at the top-level scope yet; accumulate this statement/block.
      (let* ((next-stmt (block-successor clang index))
             (snippet (full-stmt-info clang index))
             (new-acc (if do-acc (cons snippet acc) acc)))
        (if next-stmt
            ;; We're not the last statement of the block. Accumulate
            ;; this snippet and move on to the next one.
            (full-stmt-successors clang next-stmt t
                                  new-acc
                                  blocks)
            ;; We are the last statement in this block; move up a
            ;; scope and push the accumulated statements onto the
            ;; block stack.
            (full-stmt-successors
             clang (enclosing-full-stmt clang (enclosing-block clang index)) nil
             '()
             (cons (reverse new-acc) blocks))))))

(defun create-sequence-snippet (scopes &optional replacements)
  (let (decls stmts types macros funcs vars)
    (iter (for scope in scopes) (as scope-depth from 0)
          (iter (for stmt in scope)
                (pushnew (aget :counter stmt) stmts)
                (dolist (decl (aget :declares stmt))
                  (pushnew decl decls :test #'string=))
                (dolist (type (aget :types stmt))
                  (pushnew type types))
                (dolist (macro (aget :macros stmt))
                  (pushnew macro macros :test #'string= :key #'car))
                (dolist (func (aget :unbound-funs stmt))
                  (pushnew func funcs :test #'string= :key #'car))
                (dolist (var-def (aget :unbound-vals stmt))
                  (let* ((v (first var-def))
                         (already-seen (aget v vars)))
                    (when (or (not already-seen)
                              (< already-seen scope-depth))
                      (setf (aget v vars :test #'string=) scope-depth)))))
          (finally
           (return
             (let ((declared (mapcar {format nil "(|~a|)"} decls)))
               `((:src-text
                  . ,(apply-replacements
                      (append replacements
                              (mapcar (lambda (d) (cons d (peel-bananas d)))
                                      declared))
                      (format nil "~{~a~^~%}~%~}"
                              (mapcar
                               [#'unlines {mapcar #'process-full-stmt-text}]
                               scopes))))
                 (:unbound-vals ; TODO: Switch :unbound-vals to alist.
                  . ,(mapcar
                      (lambda-bind ((a . b)) (list a b))
                      (remove-if [{find _ declared :test #'string=} #'car]
                                 vars)))
                 (:unbound-funs . ,funcs)
                 (:types . ,types)
                 (:macros . ,macros)
                 (:stmts . ,stmts))))))))

(defmethod update-headers-from-snippet ((clang clang) snippet type-database)
  (mapc {add-include clang} (aget :includes snippet))
  (mapc [{add-type clang} {find-type type-database}]
        (aget :types snippet))
  (mapc {apply #'add-macro clang} (aget :macros snippet))
  snippet)

(defun nonempty-lines (text)
  (remove-if (lambda (x) (string= x ""))
             (split-sequence #\Newline text)))

(defgeneric get-vars-in-scope (software ast &optional keep-globals)
  (:documentation "Return all variables in enclosing scopes."))
(define-ast-number-or-nil-default-dispatch get-vars-in-scope &optional keep-globals)
(defmethod get-vars-in-scope ((obj clang) (ast list) &optional keep-globals)
  (apply #'append (if keep-globals
                      (butlast (aget :scopes ast))
                      (aget :scopes ast))))

(defvar *allow-bindings-to-globals-bias* 1/5
  "Probability that we consider the global scope when binding
free variables.")

(defun random-function-name (protos &key original-name arity)
  (let ((matching '())
        (variadic '())
        (others   '())
        (saw-orig nil))
    (loop :for proto :in protos
       :do (let ((name (aget :name proto))
                 (args (length (aget :args proto))))
             (when (string= name original-name)
               (setf saw-orig t))
             (cond
               ((= args arity) (push name matching))
               ((and (< args arity)
                     (aget :varargs proto)) (push name variadic))
               (t (push name others)))))
    (if (and saw-orig (< (random 1.0) *matching-free-function-retains-name-bias*))
        original-name
        (random-elt (or matching variadic others '(nil))))))

(defmethod bind-free-vars ((clang clang) snippet pt)
  (let ((scope-vars (aget :scopes (get-ast clang pt))))

    (iter (for (scope vars) in (aget :scope-removals snippet))
          (setf (nth scope scope-vars)
                (remove-if {member _ vars :test #'string=}
                           (nth scope scope-vars))))
    (iter (for (scope vars) in (aget :scope-additions snippet))
          (appendf (nth scope scope-vars) vars))

    (let ((replacements
           (append
            (mapcar
             (lambda-bind ((var index))
               (cons var
                     (let ((in-scope
                            ;; NOTE: The :RESPECT-DEPTH field is
                            ;; populated by crossover and is
                            ;; presumably used for specific crossover
                            ;; purposes. In the crossover case the
                            ;; snippet is not arbitrary.
                            (if (aget :respect-depth snippet)
                                (nth index scope-vars)
                                ;; NOTE: Previously this took
                                ;; successive cdr's of scope-vars,
                                ;; which may be required for
                                ;; crossovers which increasingly lose
                                ;; nested scopes.  E.g., so that x in
                                ;; the following snippet
                                ;;
                                ;;             foo(x); }
                                ;;         bar(x); }
                                ;;     }
                                ;;
                                ;; isn't bound to a variable in the
                                ;; inner-most scope.
                                (apply #'append scope-vars))))
                       ;; If the variable's original name matches the
                       ;; name of a variable in scope, keep the original
                       ;; name with probability equal to
                       ;; *matching-free-var-retains-name-bias*
                       (or (when (and (< (random 1.0)
                                         *matching-free-var-retains-name-bias*)
                                      (find (peel-bananas var) in-scope
                                            :test #'equal))
                             (peel-bananas var))
                           (random-elt-with-decay
                            in-scope *free-var-decay-rate*)
                           "/* no bound vars in scope */"))))
             (aget :unbound-vals snippet))
            (mapcar
             (lambda-bind ((fun . fun-info))
               (cons fun
                     (or (random-function-name
                          (prototypes clang)
                          :original-name (peel-bananas fun)
                          :arity (third fun-info))
                         "/* no functions? */")))
             (aget :unbound-funs snippet)))))
      (values (apply-replacements replacements (aget :src-text snippet))
              replacements))))

(defun rebind-uses-in-snippet (snippet renames-list)
  (add-semicolon-if-needed
   (apply-replacements
    (mapcar (lambda (it)
              (let ((peeled (peel-bananas it)))
                (cons it (or (aget peeled renames-list :test #'string=)
                             peeled))))
            (mapcar #'car
                    (append
                     (aget :unbound-vals snippet)
                     (aget :unbound-funs snippet))))
    (aget :src-text snippet))))

(define-ast-number-or-nil-default-dispatch rebind-uses (renames-list list))
(defmethod rebind-uses ((obj clang) (ast list) (renames-list list))
  (if (string= (aget :ast-class ast) "CompoundStmt")
      (iter (for id in (aget :stmt-list ast))
            (concatenating
             (rebind-uses-in-snippet (get-ast obj id) renames-list))
            (concatenating (string #\Newline)))
      (rebind-uses-in-snippet ast renames-list)))

(defgeneric delete-decl-stmts (software block replacements)
  (:documentation
   "Return mutation ops applying REPLACEMENTS to BLOCK in SOFTWARE.
REPLACEMENTS is a list holding lists of an ID to replace, and the new
variables to replace use of the variables declared in stmt ID."))

(define-ast-number-or-nil-default-dispatch
    delete-decl-stmts (replacements list))
(defmethod delete-decl-stmts ((obj clang) (block list) (replacements list))
  (append
   ;; Remove the declaration.
   (mapcar [{list :cut} {cons :stmt1} #'car] replacements)
   ;; Rewrite those stmts in the BLOCK which use an old variable.
   (let* ((old->new      ; First collect a map of old-name -> new-name.
           (mappend (lambda-bind ((id . replacements))
                      (mapcar #'cons
                              (aget :declares (get-ast obj id))
                              replacements))
                    replacements))
          (old (mapcar #'car old->new)))
     ;; Collect statements using old
     (-<>> (aget :stmt-list block)
           (mapcar {get-ast obj})
           (remove-if-not (lambda (ast)      ; Only Statements using old.
                            (intersection
                             (mapcar [#'peel-bananas #'car]
                                     (append (aget :unbound-vals ast)
                                             (aget :unbound-funs ast)))
                             old :test #'string=)))
           (sort <> #'> :key {aget :counter}) ; Bottom up.
           (mapcar (lambda (ast)
                     (list :set (cons :stmt1 (aget :counter ast))
                           (cons :literal1
                                 (peel-bananas
                                  (apply-replacements
                                   old->new (aget :src-text ast)))))))))))

(defmethod get-declared-variables ((clang clang) the-block)
  (mappend [{aget :declares} {get-ast clang}]
           (aget :stmt-list (get-ast clang the-block))))

(defmethod get-used-variables ((clang clang) stmt)
  (mapcar [#'peel-bananas #'car] (aget :unbound-vals (get-ast clang stmt))))

(defmethod get-children-using ((clang clang) var the-block)
  (remove-if-not [(lambda (el) (find var el :test #'equal))
                  {get-used-variables clang}]
                 (aget :stmt-list (get-ast clang the-block))))

(defmethod nth-enclosing-block ((clang clang) depth stmt)
  (let ((the-block (enclosing-block clang stmt)))
    (if (>= 0 depth) the-block
        (nth-enclosing-block clang (1- depth) the-block))))

(defgeneric all-use-of-var (software variable-name)
  (:documentation "Return every place VARIABLE-NAME is used in SOFTWARE."))

(defmethod all-use-of-var ((obj clang) (variable-name string))
  (mapcar {aget :counter}
          (remove-if-not [{some {string= (format nil "(|~a|)" variable-name)}}
                          {mapcar #'car} {aget :unbound-vals}]
                         (asts obj))))

(defgeneric ast-declares (ast)
  (:documentation "Return the names of the variables that AST declares."))

(defmethod ast-declares ((ast list))
  ;; TODO: This should be updated to return the range and the type of
  ;;       the declaration.  This will simplify the implementation and
  ;;       improve the correctness of `declaration-of' and
  ;;       `type-of-var' down the line.
  (cond
    ((member (aget :ast-class ast) '("Var" "ParmVar")
             :test #'string=) ; Global variable or function arg.
     (list (caar (aget :scopes ast))))
    ((string= (aget :ast-class ast) "DeclStmt") ; Sub-function declaration.
     (aget :declares ast))
    ((string= (aget :ast-class ast) "Function") ; Sub-function declaration.
     (mapcar #'car (aget :args ast)))
    (:otherwise nil)))

(defgeneric declaration-of (software variable-name &optional line-number)
  (:documentation "Return the AST in SOFTWARE which declares VARIABLE-NAME.
Optionally supply a LINE-NUMBER to return the preceding declaration
closest to LINE-NUMBER."))

(defmethod declaration-of ((obj clang) (variable-name string)
                           &optional line-number)
  (let ((decls (gethash variable-name (declarations obj))))
    (if line-number
        (lastcar (take-while [{< _ line-number} {aget :begin-src-line}] decls))
        (car decls))))

(defgeneric declared-type (ast variable-name)
  (:documentation "Guess the type of the VARIABLE-NAME in AST.
VARIABLE-NAME should be declared in AST."))

(defmethod declared-type ((ast list) variable-name)
  ;; NOTE: This is very simple and probably not robust to variable
  ;; declarations which are "weird" in any way.
  (declare (ignorable variable-name))
  (first
   (split-sequence #\Space (aget :src-text ast) :remove-empty-subseqs t)))

(defgeneric type-of-var (software variable-name)
  ;; TODO: This should search in successive enclosing scopes of a
  ;;       specified start AST.
  (:documentation "Return the type of VARIABLE-NAME in SOFTWARE."))

(defmethod type-of-var ((obj clang) (variable-name string))
  (let ((declaration-ast (declaration-of obj variable-name)))
    ;; (assert declaration-ast (obj variable-name)
    ;;         "Can't find declaration of ~a in ~a." variable-name obj)
    (if declaration-ast
        (find-type obj
                   (if (string= "Function" (aget :ast-class declaration-ast))
                       (second (find variable-name (aget :args declaration-ast)
                                     :key #'car :test #'equal))
                       (nth (position-if {string= variable-name}
                                         (aget :declares declaration-ast))
                            (aget :types declaration-ast))))
        (warn "Can't find declaration of ~a in ~a." variable-name obj))))


;;; Crossover functions
(defmethod prepare-sequence-snippet ((clang clang) end depth full-seq
                                     &optional replacements)
  (let ((last-seq (if (null end)
                      nil
                      (remove-if [{>= end} {aget :counter}]
                                 (nth depth full-seq)))))
    (if (and (equal (length full-seq) 1) (null last-seq))
        `((:stmt2 . ,end) (:src-text . ""))
        (let* ((initial-seq (loop :for scope :in full-seq
                               :for i :from 0 :to (1- depth)
                               :collecting scope))
               (tail-size (length last-seq))
               (init (if (null initial-seq)
                         (car last-seq)
                         (caar initial-seq)))
               (last (loop :for stmt :in last-seq
                        :for i :from 1 :to tail-size
                        :collecting stmt)))
          (if (null init)
              `((:stmt2 . ,end) (:src-text . ""))
              (acons   :stmt1 (aget :counter init)
                (acons :stmt2 (if (= 0 tail-size)
                                  (if (= 0 depth)
                                      end
                                      (nth-enclosing-block clang (1- depth)
                                                           (aget :counter init)))
                                  (aget :counter (last-elt last)))
                       (acons :respect-depth t
                              (create-sequence-snippet
                               (append initial-seq (list last))
                               replacements)))))))))

;; Perform 2-point crossover. The second point will be within the same
;; function as the first point, but may be in an enclosing scope.
;; The number of scopes exited as you go from the first crossover point
;; to the second crossover point will be matched between a and b.
;; Free variables are rebound in such a way as to ensure that they are
;; bound to variables that are declared at each point of use.
;;
;; Modifies parameter A.
;;
(defmethod crossover-2pt-outward
    ((a clang) (b clang) a-begin a-end b-begin b-end)
  (let* ((depth (- (nesting-depth a a-begin) (nesting-depth a a-end)))
         (b-snippet (prepare-sequence-snippet b
                                              infinity
                       depth (full-stmt-successors b b-begin t))))
    ;; Now generate text for the recontextualized b-snippet.
    (update-headers-from-snippet a b-snippet b)
    (multiple-value-bind (text replacements)
        (bind-free-vars a b-snippet a-begin)
      (list (cons :src-text text)
            (cons :replacements replacements)
            (cons :stmt1 a-begin)
            (cons :stmt2 a-end)))))

(defmethod select-before ((clang clang) depth pt)
  (let ((the-block (enclosing-block clang
                                    (enclosing-full-stmt-or-block clang pt))))
    (cond ((= 0 the-block) pt)
          ((< 0 depth) (select-before clang (- depth 1) the-block))
          (t (let ((preds
                    (remove-if {< pt}
                               (aget :stmt-list (get-ast clang the-block)))))
               (if preds (random-elt preds) pt))))))

(defmethod parent-at-depth ((clang clang) depth pt)
  (let ((the-block (enclosing-block clang pt)))
    (if (= 0 depth)
        the-block
        (parent-at-depth clang (- depth 1) the-block))))

;; Find the ancestor of STMT that is a child of ANCESTOR.
;; On failure, just return STMT again.
(defmethod ancestor-after ((clang clang) ancestor stmt)
  (funcall [{car} {last} {cons stmt}]
    (remove-if {>= ancestor}
               (mapcar {aget :counter}
                       (get-parent-asts clang
                                        (get-ast clang stmt))))))

(defmethod stmt-text-minus ((clang clang) stmt child)
  (let ((haystack (get-ast-text clang stmt))
        (needle (get-ast-text clang child)))
    (apply-replacements (list (cons needle "")) haystack)))

(defmethod create-inward-snippet ((clang clang) stmt1 stmt2 &optional replacements)
  (if (or (null stmt1) (null stmt2))
      `((:stmt1             . ,stmt1)
        (:stmt2             . ,stmt2)
        (:scope-adjustments . ,(list nil))
        (:src-text          . ,""))
      (let ((compound-stmt1-p (equal (get-ast-class clang stmt1)
                                     "CompoundStmt")))
        (multiple-value-bind (text defns vals funs macros includes types)
            (prepare-inward-snippet clang stmt1 stmt2 '() 0)
          `((:stmt1        . ,(enclosing-full-stmt-or-block clang stmt1))
            (:stmt2        . ,(enclosing-full-stmt clang stmt2))
            (:src-text     . ,(apply-replacements replacements text))
            (:macros . ,(remove-duplicates macros :test #'equal :key #'car))
            (:includes     . ,(remove-duplicates includes :test #'equal))
            (:types        . ,(remove-duplicates types :test #'equal))
            (:unbound-vals . ,(remove-duplicates vals :test #'equal :key #'car))
            (:unbound-funs . ,(remove-duplicates funs :test #'equal :key #'car))
            (:scope-adjustments
             . ,(loop :for scoped-defns :on (reverse (if compound-stmt1-p
                                                         (cons '() defns)
                                                         defns))
                   :collecting (apply #'append scoped-defns))))))))

(defmethod prepare-inward-snippet
    ((clang clang) stmt1 stmt2 defns recursion-throttle)
  (cond
    ((< 100 recursion-throttle)
     (error
      (make-condition 'mutate
        :obj clang
        :text (format nil "no progress from stmt1=~a, stmt2=~a" stmt1 stmt2))))
    ((null stmt1)
     (values "" nil nil nil))
    ((and (not (= stmt1 stmt2))
          (equal (get-ast-class clang stmt1) "CompoundStmt"))
     (multiple-value-bind (text more-defns vals funs macros includes types)
         (prepare-inward-snippet clang
                                 (car (aget :stmt-list (get-ast clang stmt1)))
                                 stmt2
                                 defns
                                 (1+ recursion-throttle))
       (values (format nil "{~%~a" text) more-defns
               vals funs macros includes types)))
    (t
     (let* ((local-defns '())
            (local-free-vars '())
            (local-free-funs '())
            (local-macros '())
            (local-includes '())
            (local-types '())
            (the-block (enclosing-block clang stmt1))
            (full-stmt1 (enclosing-full-stmt clang stmt1))
            (stmt2-ancestor (ancestor-after clang the-block stmt2))
            (stmts (remove-if-not
                    (lambda (pt) (and (<= full-stmt1 pt)
                                      (< pt stmt2-ancestor)))
                    (aget :stmt-list (if (= 0 the-block)
                                         '()
                                         (get-ast clang the-block))))))
       (when (= the-block (enclosing-block clang stmt2))
         (appendf stmts (list stmt2)))
       (loop :for stmt :in stmts
          :when (aget :declares (get-ast clang stmt))
          :do (loop :for decl :in (aget :declares (get-ast clang stmt))
                 :do (progn
                       (push decl defns)
                       (push decl local-defns))))
       (loop :for stmt :in (cons stmt2-ancestor stmts)
          :do (let ((ast (get-ast clang stmt)))
                (loop :for var :in (aget :unbound-vals ast)
                   :when (not (find (peel-bananas (car var)) defns
                                    :test #'equal))
                   :do (push var local-free-vars))
                (setf local-free-funs (append local-free-funs
                                              (aget :unbound-funs ast)))
                (setf local-macros (append local-macros
                                           (aget :macros ast)))
                (setf local-includes (append local-includes
                                             (aget :includes ast)))
                (setf local-types (append local-types
                                          (aget :types ast)))))
       (let* ((defn-replacements
               (mapcar (lambda (x) (cons (format nil "(|~a|)" x) x)) defns))
              (stmts-text (loop :for stmt :in stmts
                               :collecting
                             (apply-replacements
                                defn-replacements
                                (process-full-stmt-text
                                      (get-ast clang
                                               (enclosing-full-stmt-or-block
                                                clang stmt))))))
              (text (if (= the-block (enclosing-block clang stmt2))
                        (unlines stmts-text)
                        (format nil "~{~a~%~}~a" stmts-text
                                (apply-replacements
                                 defn-replacements
                                 (stmt-text-minus clang stmt2-ancestor
                                                  (ancestor-after clang
                                                                  stmt2-ancestor
                                                                  stmt2)))))))
         (if (= the-block (enclosing-block clang stmt2))
             (values text
                     (list local-defns)
                     local-free-vars
                     local-free-funs
                     local-macros
                     local-includes
                     local-types)
             (multiple-value-bind (more-text more-defns)
                 (prepare-inward-snippet clang
                                         (ancestor-after clang stmt2-ancestor stmt2)
                                         stmt2 defns
                                         (1+ recursion-throttle))
               (values (concatenate 'string text more-text)
                       (cons local-defns more-defns)
                       local-free-vars
                       local-free-funs
                       local-macros
                       local-includes
                       local-types))))))))

(defmethod crossover-2pt-inward ((a clang) (b clang) a-range b-range
                                 &optional replacements)
  (let* ((a-begin (car a-range))
         (a-end (cdr a-range))
         (b-begin (car b-range))
         (b-end (cdr b-range))
         (a-snippet (create-inward-snippet a a-begin a-end replacements))
         (b-snippet (create-inward-snippet b b-begin b-end replacements))
         (succ (if a-snippet
                   (full-stmt-successors a
                                         (aget :stmt2 a-snippet)
                                         t)
                   nil))
         (removals (loop :for vars
                      :in (aget :scope-adjustments a-snippet)
                      :for index :from 0
                      :collecting (list index vars)))
         (additions (loop :for vars
                       :in (aget :scope-adjustments b-snippet)
                       :for index :from 0
                       :collecting (list index vars)))
         (b-data (multiple-value-bind (b-text b-repl)
                     (bind-free-vars a b-snippet (aget :stmt1 a-snippet))
                   (cons b-text b-repl)))
         (tail (acons   :respect-depth t
                (acons  :scope-removals removals
                 (acons :scope-additions additions
                   (prepare-sequence-snippet
                    a
                    a-end
                    (1- (length (aget :scope-adjustments b-snippet)))
                    (cons (cdar succ) (cdr succ))
                    (cdr b-data))))))
         (snippet
          `((:src-text
             . ,(format nil "~a~%~a"
                        (car b-data)
                        (bind-free-vars a
                                        tail
                                        (aget :stmt2 a-snippet))))
            (:macros       . ,(aget :macros b-snippet))
            (:includes     . ,(aget :includes b-snippet))
            (:types        . ,(aget :types b-snippet))
            (:unbound-vals . ,(aget :unbound-vals tail))
            (:unbound-funs . ,(aget :unbound-funs tail)))))
    (update-headers-from-snippet a snippet b)
    `((:src-text . ,(aget :src-text snippet))
      (:stmt1    . ,(aget :stmt1 a-snippet))
      (:stmt2    . ,(aget :stmt2 tail)))))

(defmethod common-ancestor ((clang clang) x y)
  (let* ((x-ancestry
           (get-parent-asts clang
             (get-ast clang
               (if (full-stmt-p clang x)
                   x
                   (enclosing-full-stmt clang x)))))
         (y-ancestry
           (get-parent-asts clang
             (get-ast clang
               (if (full-stmt-p clang y)
                   y
                   (enclosing-full-stmt clang y)))))
         (last 0))
    (loop
       :for xp :in (mapcar {aget :counter} (reverse x-ancestry))
       :for yp :in (mapcar {aget :counter} (reverse y-ancestry))
       :when (equal xp yp)
       :do (setf last xp))
    last))

(defmethod ancestor-of ((clang clang) x y)
  (= (common-ancestor clang x y) x))

(defmethod scopes-between ((clang clang) stmt ancestor)
  (length (remove-if
           (lambda (ast)
             (or (>= (aget :counter ast) stmt)
                 (< (aget :counter ast) ancestor)
                 (not (equal (aget :ast-class ast) "CompoundStmt"))))
           (get-parent-asts clang (get-ast clang stmt)))))

(defmethod nesting-relation ((clang clang) x y)
  (if (or (null x) (null y)) nil
      (let* ((ancestor (common-ancestor clang x y)))
        (cond
          ((= x ancestor) (cons 0 (scopes-between clang y ancestor)))
          ((= y ancestor) (cons (scopes-between clang x ancestor) 0))
          (t
           ;; If the two crossover points share a CompoundStmt as the
           ;; common ancestor, then you can get from one to the other
           ;; without passing through the final set of braces.  To
           ;; compensate, we subtract one from the number of scopes
           ;; that must be traversed to get from X to Y.
           (let ((correction (if (equal (get-ast-class clang ancestor)
                                        "CompoundStmt")
                                 1 0)))
             (cons (- (scopes-between clang x ancestor) correction)
                   (- (scopes-between clang y ancestor) correction))))))))

;; Split the path between two nodes into the disjoint union of
;; a path appropriate for across-and-out crossover, followed by a
;; path approppriate for across-and-in.  Returns the pair of
;; path descriptions, or NIL for a path that is not needed.
(defmethod split-vee ((clang clang) x y)
  (let* ((ancestor (common-ancestor clang x y))
         (stmt (ancestor-after clang ancestor x)))
    (cond
      ((= x y)
       (values nil (cons x y)))
      ((= y ancestor)
       (values (cons x y) nil))
      ((= x ancestor)
       (values nil (cons x y)))
      ((= x stmt)
       (values nil (cons x y)))
      (t
       (values (cons x stmt)
               (cons (block-successor clang stmt) y))))))

(defmethod enclosing-full-stmt-or-block ((clang clang) stmt)
  (cond ((= 0 stmt) 0)
        ((and (full-stmt-p clang stmt)
              (equal (get-ast-class clang stmt) "CompoundStmt")) stmt)
        (t (enclosing-full-stmt clang stmt))))

(defmethod match-nesting ((a clang) xs (b clang) ys)
  (let* (;; Nesting relationships for xs, ys
         (x-rel (nesting-relation a (car xs) (cdr xs)))
         (y-rel (nesting-relation b (car ys) (cdr ys)))
         ;; Parent statements of points in xs, ys
         (xps (cons (enclosing-full-stmt-or-block a
                      (aget :parent-counter (get-ast a (car xs))))
                    (enclosing-full-stmt-or-block a
                      (aget :parent-counter (get-ast a (cdr xs))))))
         (yps (cons (enclosing-full-stmt-or-block b
                      (aget :parent-counter (get-ast b (car ys))))
                    (enclosing-full-stmt-or-block b
                                         (aget :parent-counter (get-ast b (cdr ys)))))))
    ;; If nesting relations don't match, replace one of the points with
    ;; its parent's enclosing full statement and try again.
    (cond
      ((< (car x-rel) (car y-rel))
       (match-nesting a xs b (cons (car yps) (cdr ys))))
      ((< (cdr x-rel) (cdr y-rel))
       (match-nesting a xs b (cons (car ys) (cdr yps))))
      ((> (car x-rel) (car y-rel))
       (match-nesting a (cons (car xps) (cdr xs)) b ys))
      ((> (cdr x-rel) (cdr y-rel))
       (match-nesting a (cons (car xs) (cdr xps)) b ys))
      (t
       (multiple-value-bind (a-out a-in)
           (split-vee a (car xs) (cdr xs))
         (multiple-value-bind (b-out b-in)
             (split-vee b (car ys) (cdr ys))
           (values a-out b-out a-in b-in)))))))

(defmethod intraprocedural-2pt-crossover ((a clang) (b clang)
                                          a-begin a-end
                                          b-begin b-end)
  (let ((variant (copy a)))
    (multiple-value-bind (a-out b-out a-in b-in)
        (match-nesting a (cons a-begin a-end)
                       b (cons b-begin b-end))
      (let* ((outward-snippet
              (if (null b-out)
                  '((:src-text . ""))   ; No corresponding text from b
                  (crossover-2pt-outward variant b
                                         (car a-out) (cdr a-out)
                                         (car b-out) (cdr b-out))))
             (inward-snippet
              (if (or (null (cdr a-in)) (null (cdr b-in)))
                  '((:src-text . ""))   ; No corresponding text from b
                  (crossover-2pt-inward
                   variant b a-in b-in
                   (aget :replacements outward-snippet)))))
        (apply-mutation
         variant
         `(clang-set-range
           (:stmt1 . ,(or (aget :stmt1 outward-snippet)
                          (aget :stmt1 inward-snippet)))
           (:stmt2 . ,(or (aget :stmt2 inward-snippet)
                          (aget :stmt2 outward-snippet)))
           (:value1 . ,(concatenate 'string
                                    (aget :src-text outward-snippet)
                                    (aget :src-text inward-snippet)))))
        (values variant
                (cons a-begin a-end)
                (cons b-begin b-end)
                t
                (cons (or (car a-out) (car a-in))
                      (or (cdr a-in) (cdr a-out))))))))

(defmethod apply-fun-body-substitutions ((clang clang) substitutions)
  (let ((sorted (sort (copy-seq substitutions) #'> :key #'car))
        (changedp nil))
    (loop :for (body-stmt . text) :in sorted
       :do (progn (setf changedp t)
                  (apply-mutation clang
                    (list :set-func
                          (cons :stmt1  body-stmt)
                          (cons :value1 text)))))
    changedp))

(defmethod full-function-text ((clang clang) func)
  (format nil "~a~%~a"
          (aget :text func)
          (get-ast-text clang (aget :body func))))

(defmethod adjust-stmt-range ((clang clang) start end)
  "Adjust START and END so that they represent a valid range for set-range.
The values returned will be STMT1 and STMT2, where STMT1 and STMT2 are both
full statements, and the end point of STMT2 in the source is greater than or
equal to the end point of STMT1."
  (let ((stmt1 (enclosing-full-stmt-or-block clang start))
        (stmt2 (enclosing-full-stmt-or-block clang end)))
    (when (and stmt1 stmt2)
      (cond ((or (ancestor-of clang stmt1 stmt2)
                 (ancestor-of clang stmt2 stmt1))
             (values stmt1 stmt2))
            ((< stmt2 stmt1)
             (values stmt2 stmt1))
            (t
             (values stmt1 stmt2))))))

(defmethod random-point-in-function ((clang clang) proto)
  (let* ((first (1+ (first (aget :stmt-range proto))))
         (last  (if (< (second (aget :stmt-range proto)) first)
                    first
                    (second (aget :stmt-range proto)))))
    (+ first (random (1+ (- last first))))))

(defmethod select-intraprocedural-pair ((clang clang))
  ;; Select a statement uniformly first, then another statement from the
  ;; same function. Selecting the function first would bias crossover
  ;; towards ASTs in smaller functions.
  (let ((proto (random-elt (functions clang))))
    (values (random-point-in-function clang proto)
            (random-point-in-function clang proto)
            proto)))

(defmethod select-crossover-points ((a clang) (b clang))
  (multiple-value-bind (a-stmt1 a-stmt2)
      (select-intraprocedural-pair a)
    (multiple-value-bind (b-stmt1 b-stmt2)
        (select-intraprocedural-pair b)
      (values a-stmt1 a-stmt2 b-stmt1 b-stmt2))))

(defmethod select-crossover-points-with-corrections ((a clang) (b clang))
  (multiple-value-bind (a-pt1 a-pt2 b-pt1 b-pt2)
      ;; choose crossover points
      (select-crossover-points a b)
    (multiple-value-bind (a-stmt1 a-stmt2)
        ;; adjust ranges to be valid for use with set-range
        (adjust-stmt-range a a-pt1 a-pt2)
      (multiple-value-bind (b-stmt1 b-stmt2)
          (adjust-stmt-range b b-pt1 b-pt2)
        (values a-stmt1 a-stmt2 b-stmt1 b-stmt2)))))

(defmethod crossover ((a clang) (b clang))
  (multiple-value-bind (a-stmt1 a-stmt2 b-stmt1 b-stmt2)
      (select-crossover-points-with-corrections a b)
    (if (and a-stmt1 a-stmt2 b-stmt1 b-stmt2)
        (multiple-value-bind (crossed a-point b-point changedp)
            (intraprocedural-2pt-crossover
             a b a-stmt1 a-stmt2 b-stmt1 b-stmt2)
          (if changedp
              (values crossed a-point b-point)
              (values crossed nil nil)))
        ;; Could not find crossover point
        (values a nil nil))))

(defgeneric function-containing-ast (object ast)
  (:documentation "Return the ast for the function containing AST in OBJECT."))

(defmethod function-containing-ast ((clang clang) (stmt list))
  (function-containing-ast clang (aget :counter stmt)))

(defmethod function-containing-ast ((clang clang) (stmt number))
  (let ((body (aget :counter
                (car (last (remove-if-not
                            [{equal "CompoundStmt"} {aget :ast-class}]
                            (get-parent-asts clang (get-ast clang stmt))))))))
    (car (remove-if-not [{= body} {aget :body}] (functions clang)))))

(defmethod function-body-p ((clang clang) stmt)
  (find-if [{= stmt} {aget :body}] (functions clang)))


;;; Clang methods
(defgeneric clang-tidy (software)
  (:documentation "Apply the software fixing command line, part of Clang."))

(defmethod clang-tidy ((clang clang) &aux errno)
  (setf (genome clang)
        (with-temp-file-of (src (ext clang)) (genome clang)
          (multiple-value-bind (stdout stderr exit)
              (shell
               "clang-tidy -fix -fix-errors -checks=~{~a~^,~} ~a -- ~a 1>&2"
               '("-cppcore-guidelines-pro-bounds-array-to-pointer-decay"
                 "-google-build-explicit-make-pair"
                 "-google-explicit-constructor"
                 "-google-readability-namespace-comments"
                 "-google-readability-redundant-smartptr-get"
                 "-google-readability-runtime-int"
                 "-google-readability-readability-function-size"
                 "-llvm-namespace-commant"
                 "-llvm-include-order"
                 "-misc-mode-constructor-init"
                 "-misc-noexcept-move-constructor"
                 "-misc-uniqueptr-reset-release"
                 "-modernize*"
                 "-readability-container-size-empty"
                 "-readability-function-size"
                 "-readability-redundant-smart-ptr-get"
                 "-readability-uniqueptr-delete-release")
               src
               (mapconcat #'identity (flags clang) " "))
            (declare (ignorable stdout stderr))
            (setf errno exit)
            (if (zerop exit) (file-to-string src) (genome clang)))))
  (values clang errno))

(defmethod clang-format ((obj clang) &optional style &aux errno)
  (setf (genome obj)
        (with-temp-file-of (src (ext obj)) (genome obj)
          (multiple-value-bind (stdout stderr exit)
              (shell "clang-format ~a ~a"
                     (if style
                         (format nil "-style=~a" style)
                         (format nil
                                 "-style='{BasedOnStyle: Google,~
                                AllowShortBlocksOnASingleLine: false,~
                                AllowShortCaseLabelsOnASingleLine: false,~
                                AllowShortFunctionsOnASingleLine: false,~
                                AllowShortIfStatementsOnASingleLine: false,~
                                AllowShortLoopsOnASingleLine: false}'"))
                     src)
            (declare (ignorable stderr))
            (setf errno exit)
            (if (zerop exit) stdout (genome obj)))))
  (values obj errno))

(defun replace-fields-in-ast (ast field-replacement-pairs)
  "Given an AST and an association list in the form ((:field . <value>))
replace the entries in the AST with the given values."
  (loop :for pair
        :in field-replacement-pairs
        :do (let ((field (car pair))
                  (replacement (cdr pair)))
              (setf (cdr (assoc field ast))
                    replacement)))
  ast)
