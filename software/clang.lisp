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
   (asts :initarg :asts :initform nil :copier :direct)
   (ancestors :initarg :ancestors
              :accessor ancestors
              :initform nil
              :copier :direct)
   (prototypes :initarg :functions :initform nil :copier :direct)
   (mitochondria :initarg :mitochondria
                 :accessor mitochondria
                 :initform (make-instance 'clang-mito)
                 :copier copy)))

(defvar *ancestor-logging* nil
  "Enable ancestor logging")

(defvar *next-ancestry-id* 0
  "Unique identifier for ancestry.")

(defun get-fresh-ancestry-id ()
  (let ((id *next-ancestry-id*))
    (incf *next-ancestry-id*)
    id))

(defgeneric update-asts (software &key)
  (:documentation "Update the store of asts associated with SOFTWARE."))

(defgeneric asts (software)
  (:documentation "Return a list of all asts in SOFTWARE."))

(defgeneric good-asts (software)
  (:documentation "Return a list of all good asts in SOFTWARE."))

(defgeneric bad-asts (software)
  (:documentation "Return a list of all bad asts in SOFTWARE."))

(defgeneric get-ast (software id)
  (:documentation "Return the statement in SOFTWARE indicated by ID."))

(defgeneric mitochondria (clang)
  (:documentation "Additional 'foreign' genome required to build phenome."))

(defgeneric mutation-types-clang (clang)
  (:documentation "Return a list of mutation types for the CLANG software
object as well as their relative probabilities"))

(defgeneric pick-mutation-type (clang)
  (:documentation "Pick the type of mutation to be performed by the CLANG
software object"))

(defgeneric mutate-clang (clang mutation-type)
  (:documentation "Perform a mutation of the given MUTATION-TYPE on the
CLANG software object"))

(defmethod size ((obj clang))
  (with-slots (asts) obj (length asts)))

(defvar *clang-json-required-fields*
  '( :ast--class         :counter            :unbound--vals
     :unbound--funs      :types              :stmt--list
     :src--text          :parent--counter    :macros
     :guard--stmt        :full--stmt         :begin--src--line
     :end--src--line     :begin--src--col    :end--src--col
     :begin--addr        :end--addr          :includes
     :declares)
  "JSON database entry fields required for clang software objects.")

(defvar *clang-json-required-aux*
  '(:protos)
  "JSON database AuxDB entries required for clang software objects.")

(defmethod update-asts ((obj clang) &key clang-mutate-args)
  (with-slots (asts prototypes) obj
    (let ((json-db
           (handler-case
               ;; When clang-mutate errors we nullify the asts.
               ;; Otherwise we can end up in weird situations, like trying to
               ;; parse a mutation error condition as an alist of ASTS.
             (clang-mutate obj
                           (list* :json
                                  (cons :fields *clang-json-required-fields*)
                                  (cons :aux *clang-json-required-aux*)
                                  clang-mutate-args))
             (mutate (err) (declare (ignorable err)) nil))))
      (setf asts
            (coerce (remove-if-not {aget :counter} json-db) 'vector)
            prototypes
            (coerce (remove-if-not {aget :body} json-db) 'vector)))))

;; Create a clang software object from a given C file.
;; The software object's genome will exactly the input
;; file's contents, but the mitochondria will not reflect
;; the preprocessor directives and user-defined types
;; in the original program.
(defmethod from-file-exactly ((obj clang) path)
  (setf (genome-string obj) (file-to-string path))
  (when *ancestor-logging*
    (setf (ancestors obj) (list (alist :base (file-to-string path)
                                       :how 'from-file-exactly
                                       :id (get-fresh-ancestry-id)))))
  (setf (ext obj)  (pathname-type (pathname path)))
  obj)

(defmethod from-file ((obj clang) path)
  ;; Load the raw file and generate a json database
  (from-file-exactly obj path)
  (let* ((json-db (clang-mutate obj (list :json)))
         (type-db-mito (make-instance 'clang-mito)))

    ;; Populate a type database with the types found.
    (loop for type in json-db
       when (and (assoc :hash type)
                 (assoc :reqs type)
                 (assoc :type type)
                 (or (assoc :include type)
                     (assoc :decl type)))
       do (setf (gethash (aget :hash type) (types type-db-mito)) type))

    ;; Set the clang-mito's types. This will also populate any
    ;; #include directives needed for library typedefs.
    (loop for type in json-db
       when (aget :hash type)
       when (and (assoc :hash type)
                 (assoc :reqs type)
                 (assoc :type type)
                 (or (assoc :include type)
                     (assoc :decl type)))
       do (add-type (mitochondria obj) (aget :hash type) type-db-mito))

    ;; Add any macro definitions seen.
    (loop for snippet in json-db
      do (loop for macro in (aget :macros snippet)
           do (add-macro (mitochondria obj) (first macro) (second macro))))

    ;; Add any #includes needed.
    (loop for snippet in json-db
       do (loop for include in (aget :includes snippet)
             do (add-include (mitochondria obj) include)))

    ;; The clang-mito object is now fully initialized.
    ;; Next, generate the bulk of the program text by joining all global
    ;; declarations together in the same order they originally appeared.
    (setf (genome-string obj)
          (unlines
           (mapcar {aget :decl--text}
                   (sort (remove-if-not {aget :decl--text} json-db)
                         (lambda (x y)
                           (or (< (first x) (first y))
                               (and (= (first x) (first y))
                                    (< (second x) (second y)))))
                         :key (lambda (x)
                                (cons (aget :begin--src--line x)
                                      (aget :begin--src--col x))))))))
  (when *ancestor-logging*
    (setf (ancestors obj) (list (alist :base (file-to-string path)
                                       :how 'from-file
                                       :id (get-fresh-ancestry-id)))))
  obj)

(defmethod asts ((obj clang))
  (with-slots (asts) obj
    (coerce asts 'list)))

(defmethod (setf asts) (new (obj clang))
  (with-slots (asts) obj (setf asts new)))

(defmethod get-ast ((obj clang) id)
  (with-slots (asts) obj (aref asts (1- id))))

(defmethod prototypes ((obj clang))
  (with-slots (prototypes) obj
    (coerce prototypes 'list)))

(defmethod recontextualize ((clang clang) snippet pt)
  (let ((text (bind-free-vars clang snippet pt)))
    (if (full-stmt-p clang pt)
        (format nil "~a~%" (add-semicolon-if-needed text))
        (format nil "~a" text))))

(defun do-not-filter () (lambda (asts) asts))

(defun with-class-filter (class asts)
  (remove-if-not [{equal class} {aget :ast--class } ] asts))

(defun full-stmt-filter (asts)
  (remove-if-not { aget :full--stmt } asts))

(defmethod good-asts ((clang clang))
  (asts clang))

(defmethod bad-asts ((clang clang))
  (asts clang))

(defun random-stmt (asts)
  (aget :counter (random-elt asts)))

(defmethod pick-good ((clang clang))
  (random-stmt (good-asts clang)))

(defmethod pick-bad ((clang clang))
  (random-stmt (bad-asts clang)))

(defmethod get-ast-class ((clang clang) stmt)
  (aget :ast--class (get-ast clang stmt)))

(defun execute-picks (get-asts1 &optional connector get-asts2)
  (let* ((stmt1 (when get-asts1
                  (random-stmt (funcall get-asts1))))
         (stmt2 (when get-asts2
                  (random-stmt (funcall connector stmt1
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
  `(:cut      :cut-same     :cut-full        :cut-full-same
    :insert   :insert-same  :insert-full     :insert-full-same
    :swap     :swap-same    :swap-full       :swap-full-same
    :replace  :replace-same :replace-full    :replace-full-same
    :cut-decl :swap-decls   :rename-variable ))

(defvar *clang-crossover-cdf*
  (cdf (uniform-probability
        (list #'crossover-2pt-outward
              #'crossover-single-stmt
              #'crossover-all-functions)))
  "The crossover strategy probability distribution, as a CDF.")

(defvar *free-var-decay-rate* 0.3
  "The decay rate for choosing variable bindings.")

(defvar *matching-free-var-retains-name-bias* 0.75
  "The probability that if a free variable's original name matches a name
already in scope, it will keep that name.")

(defvar *matching-free-function-retains-name-bias* 0.75
  "The probability that if a free functions's original name matches a name
already in scope, it will keep that name.")

(defvar *crossover-function-probability* 0.25
  "The probability of crossing a function during whole-program crossover.")

(defvar *clang-format-after-mutation-chance* 0.125
  "The probability of applying clang-format on an object after mutation")

(defun combine-with-bias (bias heads tails)
  (append
   (mapcar (lambda (pair) (cons (car pair) (* (cdr pair) bias))) heads)
   (mapcar (lambda (pair) (cons (car pair) (* (cdr pair) (- 1 bias)))) tails)))

(defmethod decl-mutation-types-clang ((clang clang))
  (uniform-probability '(:cut-decl :swap-decls :rename-variable)))

(defmethod basic-mutation-types-clang ((clang clang))
  (remove-if #'null
    (loop for mutation-type in *clang-mutation-types*
       collecting
         (cond ((member mutation-type
                        `(:cut-full-same :insert-full-same
                          :swap-full-same :replace-full-same))
                (cons mutation-type
                      (/ (* *clang-full-stmt-bias*
                            *clang-same-class-bias*)
                         (/ (length *clang-mutation-types*) 4))))
               ((member mutation-type
                        `(:cut-full  :insert-full
                          :swap-full :replace-full))
                (cons mutation-type
                      (/ (* *clang-full-stmt-bias*
                            (- 1 *clang-same-class-bias*))
                         (/ (length *clang-mutation-types*) 4))))
               ((member mutation-type
                        `(:cut-same  :insert-same
                          :swap-same :replace-same))
                (cons mutation-type
                      (/ (* *clang-same-class-bias*
                            (- 1 *clang-full-stmt-bias*))
                         (/ (length *clang-mutation-types*) 4))))
               ((member mutation-type
                        `(:cut :insert
                          :swap :replace))
                (cons mutation-type
                      (/ (* (- 1 *clang-same-class-bias*)
                            (- 1 *clang-full-stmt-bias*))
                         (/ (length *clang-mutation-types*) 4))))
               (t nil)))))

(defmethod mutation-types-clang ((clang clang))
  (combine-with-bias *decl-mutation-bias*
                     (decl-mutation-types-clang clang)
                     (basic-mutation-types-clang clang)))

(defmethod pick-mutation-type ((clang clang))
  (random-pick (cdf (mutation-types-clang clang))))

(defmethod mutate ((clang clang))
  (unless (> (size clang) 0)
    (error (make-condition 'mutate :text "No valid IDs" :obj clang)))

  (mutate-clang clang (pick-mutation-type clang)))

(defmethod mutate-clang ((clang clang) mutation-type)
  (unless (member mutation-type *clang-mutation-types*)
    (error (make-condition 'mutate
             :text (format nil "Mutation type ~S not supported" mutation-type)
             :obj clang)))

  (if (member mutation-type
              '(:cut-decl :swap-decls :rename-variable))
    (decl-mutate-clang clang mutation-type))
    (labels ((filter (asts)(if (member mutation-type
                                     `(:cut-full     :cut-full-same
                                       :insert-full  :insert-full-same
                                       :swap-full    :swap-full-same
                                       :replace-full :replace-full-same))
                              (full-stmt-filter asts)
                              asts)))
      (let* ((then (if (member mutation-type
                              `(:cut-same     :cut-full-same
                                :insert-same  :insert-full-same
                                :swap-same    :swap-full-same
                                :replace-same :replace-full-same))
                       (lambda (stmt asts)
                         (or (with-class-filter (get-ast-class clang stmt)
                                                 asts)
                             (with-class-filter (get-ast-class clang stmt)
                                                (asts clang))))
                       (lambda (stmt asts) (declare (ignorable stmt)) asts)))
           (good (lambda () (or (filter (good-asts clang)) (asts clang))))
           (bad  (lambda () (or (filter (bad-asts clang)) (asts clang))))
           (op (cond ((member mutation-type `(:cut      :cut-same
                                              :cut-full :cut-full-same))
                        `(:cut . ,(apply #'execute-picks
                                         (list bad))))
                     ((member mutation-type `(:insert      :insert-same
                                              :insert-full :insert-full-same))
                        `(:insert . ,(apply #'execute-picks
                                            (list bad then good))))
                     ((member mutation-type `(:swap      :swap-same
                                              :swap-full :swap-full-same))
                        `(:swap . ,(apply #'execute-picks
                                          (list bad then bad))))
                     ((member mutation-type `(:replace      :replace-same
                                              :replace-full :replace-full-same))
                        `(:replace . ,(apply #'execute-picks
                                             (list bad then good)))))))

      (apply-mutation clang op)
      (values clang op))))

(defmethod decl-mutate-clang ((clang clang) mutation-type)
  (values clang
          (case mutation-type
            (:cut-decl
             (run-cut-decl
              clang
              (random-stmt (with-class-filter "DeclStmt" (asts clang)))))
            (:swap-decls
             (run-swap-decls
              clang
              (enclosing-block clang
                               (random-stmt (bad-asts clang)))))
            (:rename-variable
             (run-rename-variable
              clang
              (random-stmt (bad-asts clang)))))))

;; Replace the basic mutation operations with versions that
;; rebind free variables in the appropriate context.
(defmethod recontextualize-mutation-op ((clang clang) op)
  (let* ((mut (car op))
         (properties (cdr op))
         (stmt1  (aget :stmt1  properties))
         (stmt2  (aget :stmt2  properties))
         (value1 (aget :value1 properties)))

    (case mut
      (:insert
       (cons :insert-value
          (list (cons :stmt1 stmt1)
                (cons :value1
                      (recontextualize clang
                                       (if stmt2
                                           (get-ast clang stmt2)
                                           value1)
                                       stmt1)))))
      (:replace
       (cons :set
          (list (cons :stmt1 stmt1)
                (cons :value1
                      (recontextualize clang
                                       (if stmt2
                                           (get-ast clang stmt2)
                                           value1)
                                       stmt1)))))
      (:swap
       (cons :set2
          (list (cons :stmt1 stmt1)
                (cons :value1
                      (recontextualize clang
                                       (get-ast clang stmt2)
                                       stmt1))
                (cons :stmt2 stmt2)
                (cons :value2
                      (recontextualize clang
                                       (get-ast clang stmt1)
                                       stmt2)))))
      (otherwise op))))

(defmethod apply-mutation ((clang clang) op)
  (restart-case
    (clang-mutate clang (recontextualize-mutation-op clang op))
    (skip-mutation ()
      :report "Skip mutation and return nil"
      (values nil 1))
    (tidy ()
      :report "Call clang-tidy before re-attempting mutation"
      (clang-tidy clang)
      (apply-mutation clang op))
    (mutate ()
      :report "Apply another mutation before re-attempting mutations"
      (mutate clang)
      (apply-mutation clang op))))

(defmethod apply-mutation :around ((obj clang) op)
  (multiple-value-call (lambda (variant &rest rest)
                         (unless (member (car op) '(:ids :list :json))
                           (when *ancestor-logging*
                             (push (alist :mutant op
                                          :id (get-fresh-ancestry-id))
                                   (ancestors obj)))
                           (when (random-bool :bias
                                    *clang-format-after-mutation-chance*)
                             (clang-format obj))
                           (update-asts obj))
                         (apply #'values variant rest))
    (call-next-method)))

(defmethod mutation-key ((obj clang) op)
  ;; Return a list of the OP keyword, and the classes of any stmt1 or
  ;; stmt2 arguments.
  (cons
   (car op)
   (mapcar [{aget :ast--class} {get-ast obj} #'cdr]
           (remove-if-not [#'numberp #'cdr]
                          (remove-if-not [{member _ (list :stmt1 :stmt2)} #'car]
                                         (remove-if-not #'consp op))))))

(defvar *clang-genome-separator* "//===============^=================="
  "String used to separate the mito and full portions of a clang genome.")

(defun extract-clang-genome (full-genome)
  "If FULL-GENOME contains the magic separator return only the genome after.
Otherwise return the whole FULL-GENOME"
  ;; NOTE: This could potentially be faster if defined using cl-ppcre.
  (let* ((lines (split-sequence #\Newline full-genome))
         (at (position *clang-genome-separator* lines :test #'string=)))
    (if at
        (unlines (subseq lines (1+ at)))
        full-genome)))

(defmethod clang-mutate ((obj clang) op &aux value1-file value2-file)
  (with-temp-file-of (src-file (ext obj)) (genome-string obj)
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
                 (:json "-json")))
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
                     value src-file)))))
             (field-opt (field)
               (ecase field
                 (:counter "counter")
                 (:declares "declares")
                 (:parent--counter "parent_counter")
                 (:ast--class "ast_class")
                 (:src--file--name "src_file_name")
                 (:begin--src--line "begin_src_line")
                 (:begin--src--col "begin_src_col")
                 (:end--src--line "end_src_line")
                 (:end--src--col "end_src_col")
                 (:src--text "src_text")
                 (:guard--stmt "guard_stmt")
                 (:full--stmt "full_stmt")
                 (:unbound--vals "unbound_vals")
                 (:unbound--funs "unbound_funs")
                 (:macros "macros")
                 (:types "types")
                 (:stmt--list "stmt_list")
                 (:binary--file--path "binary_file_path")
                 (:scopes "scopes")
                 (:begin--addr "begin_addr")
                 (:end--addr "end_addr")
                 (:includes "includes")))
             (aux-opt (aux)
               (ecase aux
                 (:types "types")
                 (:protos "protos")
                 (:decls "decls")
                 (:none "none"))))
    (let ((clang-mutate-outfile (temp-file-name)))
      (unwind-protect
        (multiple-value-bind (stdout stderr exit)
          (shell "clang-mutate ~a ~{~a~^ ~} ~a -- ~{~a~^ ~} > ~a"
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
            (when (> (file-length clang-mutate-out) 10485760)
              (error (make-condition 'mutate
                       :text (format nil "clang-mutate output exceeds 10 MB.")
                       :obj obj :op op))))
          (values
           (case (car op)
             (:json
              (handler-case (json:decode-json-from-source
                              (file-to-string clang-mutate-outfile))
                (end-of-file (err)
                  (declare (ignorable err))
                  (error (make-condition 'mutate
                           :text "JSON decode error"
                           :obj obj :op op)))))
             ((:ids :list) (file-to-string clang-mutate-outfile))
             (t (extract-clang-genome (file-to-string clang-mutate-outfile))))
           exit))
      ;; Cleanup forms.
      (when (probe-file clang-mutate-outfile)
        (delete-file clang-mutate-outfile))
      (when (and value1-file (probe-file value1-file))
        (delete-file value1-file))
      (when (and value2-file (probe-file value2-file))
        (delete-file value2-file)))))))

(defun ast-to-source-range (ast)
  "Convert AST to pair of SOURCE-LOCATIONS."
  (when ast
    (make-instance 'source-range
      :begin (make-instance 'source-location
               :line (aget :begin--src--line ast)
               :column (aget :begin--src--col ast))
      :end (make-instance 'source-location
             :line (aget :end--src--line ast)
             :column (aget :end--src--col ast)))))

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
  (cons 0 (loop :for char :in (coerce (genome-string clang) 'list) :as index
                :from 0
                :when (equal char #\Newline) :collect index)))

(defgeneric parent-ast-p (software possible-parent-ast ast)
  (:documentation
   "Check if POSSIBLE-PARENT-AST is a parent of AST in SOFTWARE."))

(defmethod parent-ast-p ((clang clang) possible-parent-ast ast)
  (cond ((= (aget :counter possible-parent-ast)
            (aget :counter ast)) t)
        ((= (aget :parent--counter ast) 0) nil)
        (t (parent-ast-p clang
                         possible-parent-ast
                         (get-ast clang (aget :parent--counter ast))))))

(defmethod get-parent-asts((clang clang) ast)
  (cond ((= (aget :parent--counter ast) 0) nil)
         (t  (append (list ast)
                     (get-parent-asts
                       clang
                       (get-ast clang (aget :parent--counter ast)))))))

(defmethod get-immediate-children ((clang clang) ast)
  (remove-if-not (lambda (child-ast) (= (aget :parent--counter child-ast)
                                        (aget :counter ast)))
                 (asts clang)))

(defmethod get-parent-full-stmt((clang clang) ast)
  (cond ((aget :full--stmt ast) ast)
        (t (get-parent-full-stmt clang (get-ast clang
                                                (aget :parent--counter ast))))))

(defmethod nesting-depth ((clang clang) index &optional orig-depth)
  (let ((depth (or orig-depth 0)))
    (if (= 0 index)
        depth
        (nesting-depth clang (enclosing-block clang index) (1+ depth)))))

(defmethod enclosing-block ((clang clang) index &optional child-index)
  (if (= index 0) (values  0 child-index)
    (let* ((ast (get-ast clang index))
           (blockp (equal (aget :ast--class ast) "CompoundStmt")))
      (if (and blockp child-index)
          (values index child-index)
          (enclosing-block clang (aget :parent--counter ast) index)))))

(defmethod full-stmt-p ((clang clang) stmt)
  ;; NOTE: This assumes that the :full--stmt tag is always populated.
  (aget :full--stmt (get-ast clang stmt)))

(defmethod enclosing-full-stmt ((clang clang) index &optional child-index)
  (if (= index 0) nil
    (let* ((ast (get-ast clang index))
           (blockp (equal (aget :ast--class ast) "CompoundStmt")))
      (if (and blockp child-index)
          child-index
          (enclosing-full-stmt clang (aget :parent--counter ast) index)))))

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
                        (aget :stmt--list the-block))))
    (get-entry-after index the-stmts)))

(defmethod block-predeccessor ((clang clang) raw-index)
  (let* ((index (enclosing-full-stmt clang raw-index))
         (block-index (enclosing-block clang index))
         (the-block (get-ast clang block-index))
         (the-stmts (if (= 0 block-index) nil
                        (aget :stmt--list the-block))))
    (get-entry-before index the-stmts)))

(defmethod get-ast-text ((clang clang) stmt)
  (aget :src--text (get-ast clang stmt)))

(defun add-semicolon-if-needed (text)
  (if (equal text "") ";"
      ;; Add a semicolon unless the text ends in a } (CompoundStmts, etc)
      ;; or already includes a semicolon (only seen for DeclStmts).
      (if (find (char text (1- (length text)))
                (list #\} #\;))
          text
          (concatenate 'string text ";"))))

(defun process-full-stmt-text (snippet)
  (add-semicolon-if-needed (aget :src--text snippet)))

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

(defun create-sequence-snippet (scopes)
  (let ((funcs  (make-hash-table :test 'equal))
        (macros (make-hash-table :test 'equal))
        (types  (make-hash-table :test 'equal))
        (vars   (make-hash-table :test 'equal))
        (stmts  '())
        (source (intercalate (format nil "~%}~%")
                   (loop for scope in scopes for k from 0
                      collecting (unlines
                         (mapcar #'process-full-stmt-text scope))))))
    (loop for scope in scopes for scope-depth from 0 do (progn
      (loop for stmt in scope do (progn
        (setf stmts (cons (aget :counter stmt) stmts))
        (list->ht (aget :types         stmt) types)
        (list->ht (aget :macros        stmt) macros)
        (list->ht (aget :unbound--funs stmt) funcs :key #'car :value #'cdr)
        (loop for var-def in (aget :unbound--vals stmt)
           do (let* ((var (first var-def))
                     (already-seen (gethash var vars nil)))
                (when (not already-seen)
                  (setf (gethash var vars) scope-depth))))))))

    (alist :src--text source
           :unbound--vals (ht->list vars)
           :unbound--funs (ht->list funcs :merge-fn #'cons)
           :types  (ht->list types)
           :macros (ht->list macros)
           :stmts stmts)))

(defmethod update-mito-from-snippet ((clang clang) snippet type-database)
  (loop for f in (aget :INCLUDES snippet)
     do (add-include (mitochondria clang) f))
  (loop for type in (aget :TYPES snippet)
     do (add-type (mitochondria clang) type type-database))
  (let ((macros (aget :MACROS snippet)))
    (loop for macro in macros
       do (add-macro (mitochondria clang)
                     (first macro)
                     (second macro))))
  snippet)

(defun nonempty-lines (text)
  (remove-if (lambda (x) (string= x ""))
             (split-sequence #\Newline text)))

(defmethod get-vars-in-scope ((clang clang) pt &optional keep-globals)
  (gethash 0 (get-indexed-vars-in-scope clang pt keep-globals)))

(defmethod get-indexed-vars-in-scope ((clang clang) pt &optional keep-globals)
  (let ((index-table (make-hash-table :test 'equal))
        (max-index 0))
    (with-temp-file-of (src (ext clang)) (genome-string clang)
      (loop
         for scope in
           (aget :scopes (car
                          (handler-case ; When clang-mutate errors return nil.
                            (clang-mutate clang
                                          `(:json (:fields . (:scopes))
                                                  (:stmt1 . ,pt)))
                            (mutate (err) (declare (ignorable err)) nil))))
         for index from 0
         do (setf (gethash index index-table) scope
                  max-index index)))
    ;; Merge variables downward, so that every index-1 variable appears in
    ;; the index-0 list etc. Don't merge the outermost scope; we only want
    ;; to draw from the global scope in special cases.
    (when (and (< 1 max-index) (not keep-globals))
        (setf max-index (1- max-index)))
    (loop for index from max-index downto 1
       do (let ((vars-n (gethash index index-table))
                (vars-n-minus-1 (gethash (1- index) index-table)))
            (setf (gethash (1- index) index-table)
                  (concatenate 'list vars-n-minus-1 vars-n))))
    index-table))

(defun random-scoped-replacement (var in-scope)
  ;; If the variable's original name matches the name of a variable in
  ;; scope, keep the original name with probability equal to
  ;; *matching-free-var-retains-name-bias*
  (if (and (find (peel-bananas var) in-scope :test #'equal)
           (< (random 1.0) *matching-free-var-retains-name-bias*))
      (peel-bananas var)
      (random-elt-with-decay in-scope *free-var-decay-rate*)))

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
  (let* ((raw-code    (aget :src--text snippet))
         (free-vars   (make-hash-table :test 'equal))
         (free-funs   (make-hash-table :test 'equal))
         (respect-depth (aget :respect--depth snippet))
         (scope-vars (get-indexed-vars-in-scope
                       clang
                       pt
                       (random-bool :bias *allow-bindings-to-globals-bias*))))
    (list->ht (aget :unbound--vals snippet) free-vars)
    (list->ht (aget :unbound--funs snippet) free-funs :key #'car :value #'cdr)
    (apply-replacements
     (append
      (loop for var being the hash-keys of free-vars
         using (hash-value index)
         collecting
           (cons var (or (random-scoped-replacement
                          var
                          (gethash (if respect-depth index 0) scope-vars))
                         (format nil "/* no bound vars in scope at depth ~a */"
                                 index))))
      (loop for fun being the hash-keys of free-funs
         using (hash-value fun-info)
         collecting
           (cons fun
                 (or (random-function-name
                      (prototypes clang)
                      :original-name (peel-bananas fun)
                      :arity (third fun-info))
                     "/* no functions? */"))))
     raw-code)))

(defun rebind-uses-in-snippet (snippet renames-list)
  (let ((renames (make-hash-table :test 'equal)))
    (list->ht renames-list renames :key #'car :value #'cdr)
    (add-semicolon-if-needed
     (apply-replacements
      (append
       (loop :for var :in (mapcar #'car (aget :unbound--vals snippet))
          :collecting (cons var (gethash (peel-bananas var) renames
                                         (peel-bananas var))))
       (loop for fun :in (mapcar #'car (aget :unbound--funs snippet))
          :collecting (cons fun (gethash (peel-bananas fun) renames
                                         (peel-bananas fun)))))
      (aget :src--text snippet)))))

(defmethod rebind-uses ((clang clang) stmt renames-list)
  (if (equal (get-ast-class clang stmt) "CompoundStmt")
      (format nil "{~%~{~a~%~}}~%"
              (loop :for one-stmt
                 :in (aget :stmt--list (get-ast clang stmt))
                 :collecting
                 (rebind-uses-in-snippet (get-ast clang one-stmt)
                                         renames-list)))
      (rebind-uses-in-snippet (get-ast clang stmt)
                              renames-list)))

(defmethod delete-decl-stmts ((clang clang) the-block decl-replacements)
  (let ((renames-list
         (mapcar (lambda (pair)
                   (cons (aget :declares (get-ast clang (car pair)))
                         (cdr pair)))
                 decl-replacements))
        (decls (mapcar #'car decl-replacements)))
    (apply-mutation clang
                    `(:set (:stmt1 . ,the-block)
                           (:value1 . ,(rebind-uses
                                        clang
                                        the-block
                                        renames-list))))
    (loop :for decl :in (sort decls #'>)
       :do (apply-mutation clang `(:cut (:stmt1 . ,decl))))))

(defmethod rename-variable-near-use ((clang clang) use new-name)
  (let ((the-block (enclosing-block clang use))
        (old-name (peel-bananas (aget :src--text (get-ast clang use)))))
    (apply-mutation clang
                    `(:set (:stmt1 . ,the-block)
                           (:value1 . ,(rebind-uses
                                        clang
                                        the-block
                                        (list (cons old-name new-name))))))))

(defmethod get-declared-variables ((clang clang) the-block)
    (remove-if #'null
               (loop :for stmt :in (aget :stmt--list (get-ast clang the-block))
                  :collecting (aget :declares (get-ast clang stmt)))))

(defmethod get-used-variables ((clang clang) stmt)
  (mapcar [{peel-bananas} {car}] (aget :unbound--vals (get-ast clang stmt))))

(defmethod get-children-using ((clang clang) var the-block)
  (loop :for stmt :in (aget :stmt--list (get-ast clang the-block))
     :when (find var (get-used-variables clang stmt) :test #'equal)
     :collecting stmt))

(defmethod run-cut-decl ((clang clang) decl)
  (let* ((the-block (enclosing-block clang decl))
         (old-name (aget :declares (get-ast clang decl)))
         (uses (get-children-using clang old-name the-block))
         (vars (remove-if {equal old-name}
                          (get-vars-in-scope clang
                                             (if uses (car uses) the-block))))
         (var (if vars (random-elt vars)
                  "/* no vars available before first use of cut decl */")))
    (delete-decl-stmts clang the-block `((,decl . ,var)))
    (list :cut-decl decl old-name var)))

(defun pick-two (things)
  (let ((this (random-elt things))
        (that (random-elt things)))
    (if (equal this that)
        (pick-two things)
        (values this that))))

(defmethod run-swap-decls ((clang clang) the-block)
  (if (equal the-block 0)
      (list :swap-decls 'did-nothing)
      (let ((decls
             (mapcar {aget :counter}
                     (with-class-filter "DeclStmt"
                       (mapcar {get-ast clang}
                               (aget :stmt--list
                                     (get-ast clang the-block)))))))
        (if (> 2 (length decls))
            (run-swap-decls clang (enclosing-block clang the-block))
            (multiple-value-bind (stmt1 stmt2) (pick-two decls)
              (apply-mutation clang `(:swap (:stmt1 . ,stmt1)
                                            (:stmt2 . ,stmt2)))
              (list :swap-decls stmt1 stmt2))))))

(defmethod run-rename-variable ((clang clang) stmt)
  (let ((used (get-used-variables clang stmt)))
    (if used
        (let ((old-var (random-elt used))
              (new-var (random-elt (get-vars-in-scope clang stmt))))
          (rebind-uses clang (enclosing-block clang stmt)
                       (list (cons old-var new-var)))
          (list :rename-variable stmt old-var new-var))
        (list :rename-variable stmt 'did-nothing))))

(defmethod nth-enclosing-block ((clang clang) depth stmt)
  (let ((the-block (enclosing-block clang stmt)))
    (if (>= 0 depth) the-block
        (nth-enclosing-block clang (1- depth) the-block))))

(defmethod prepare-sequence-snippet ((clang clang) depth full-seq)
  (let* ((initial-seq (loop for scope in full-seq
                        for i from 0 to (1- depth)
                        collecting scope))
         (last-seq (nth depth full-seq))
         (tail-size (if (null initial-seq)
                        (1+ (random (length last-seq)))
                        (random (1+ (length last-seq)))))
         (init (if (null initial-seq)
                   (car last-seq)
                   (caar initial-seq)))
         (last (loop for stmt in last-seq
                  for i from 1 to tail-size
                  collecting stmt)))
    (acons   :stmt1 (aget :counter init)
      (acons :stmt2 (if (= 0 tail-size)
                        (nth-enclosing-block clang (1- depth)
                                             (aget :counter init))
                        (aget :counter (last-elt last)))
        (acons :respect--depth t
               (create-sequence-snippet (append initial-seq (list last))))))))

;; Perform 2-point crossover. The second point will be within the same
;; function as the first point, but may be in an enclosing scope.
;; The number of scopes exited as you go from the first crossover point
;; to the second crossover point will be matched between a and b.
;; Free variables are rebound in such a way as to ensure that they are
;; bound to variables that are declared at each point of use.
(defmethod crossover-2pt-outward ((a clang) (b clang))
  (let ((a-begin (enclosing-full-stmt a (pick-bad a)))
        (b-begin (enclosing-full-stmt b (pick-bad b)))
        (variant (copy a)))

    (if (and a-begin b-begin)
        ;; The selected initial crossover points are valid; choose a
        ;; nesting depth and find the second crossover points.
        (let* ((depth (random (min (nesting-depth a a-begin)
                                   (nesting-depth b b-begin))))
               (a-snippet (prepare-sequence-snippet a
                            depth (full-stmt-successors a a-begin t)))
               (b-snippet (prepare-sequence-snippet b
                            depth (full-stmt-successors b b-begin t))))

          ;; Now execute the crossover, replacing a-snippet in variant
          ;; with the recontextualized b-snippet.
          (update-mito-from-snippet variant b-snippet (mitochondria b))
          (apply-mutation
           variant
           (cons :set-range
                 (list
                  (cons :stmt1 (aget :stmt1 a-snippet))
                  (cons :stmt2 (aget :stmt2 a-snippet))
                  (cons :value1 (bind-free-vars variant b-snippet a-begin)))))
          (values variant
                  (list (aget :stmt1 a-snippet) (aget :stmt2 a-snippet))
                  (list (aget :stmt1 b-snippet) (aget :stmt2 b-snippet))
                  t))

        ;; The selected initial crossover points were not valid; return a
        ;; copy of a.
        (values variant nil nil nil))))

;; Perform crossover by selecting a single AST from a and b to cross.
;; Free variables are recontextualized to the insertion point.
(defmethod crossover-single-stmt ((a clang) (b clang))
  (let ((a-begin (enclosing-full-stmt a (pick-bad a)))
        (b-begin (enclosing-full-stmt b (pick-bad b)))
        (variant (copy a)))
    (if (and a-begin b-begin)
        (let ((b-snippet (create-sequence-snippet
                          (list (list (get-ast b b-begin))))))
          (update-mito-from-snippet variant b-snippet (mitochondria b))
          (apply-mutation
           variant
           (cons :set-range
                 (list
                  (cons :stmt1 a-begin)
                  (cons :stmt2 a-begin)
                  (cons :value1 (bind-free-vars variant b-snippet a-begin)))))
          (values variant a-begin b-begin t))
        (values variant nil nil nil))))

(defmethod apply-fun-body-substitutions ((clang clang) substitutions)
  (let ((sorted (sort (copy-seq substitutions) #'> :key #'car))
        (changedp nil))
    (loop for (body-stmt . text) in sorted
       do (progn (setf changedp t)
                 (apply-mutation clang
                                 (list :set-func
                                       (cons :stmt1  body-stmt)
                                       (cons :value1 text)))))
    changedp))

(defmethod full-function-text ((clang clang) func)
  (format nil "~a~%~a"
          (aget :text func)
          (get-ast-text clang (aget :body func))))

;; Perform crossover by choosing a function body at random from
;; either a or b.
(defmethod crossover-all-functions ((a clang) (b clang))
  (let ((common-funs (ht-intersect
                      (list->ht (prototypes a)
                                nil
                                :key {aget :name}
                                :value {aget :body})
                      (list->ht (prototypes b)
                                nil
                                :key {aget :name}
                                :value {full-function-text b})))
        (variant (copy a)))
    (union-mito (mitochondria variant) (mitochondria b))
    (values variant nil nil
            (apply-fun-body-substitutions
             variant
             (loop for func being the hash-keys of common-funs
                using (hash-value bodies)
                when (> *crossover-function-probability* (random 1.0))
                collect bodies)))))

(defmethod crossover ((a clang) (b clang))
  (let ((crossover-method (random-pick *clang-crossover-cdf*)))
    (multiple-value-bind (crossed a-point b-point changedp)
        (funcall crossover-method a b)
      (when (and changedp *ancestor-logging*)
        (push (alist :cross-with (ancestors b)
                     :crossover crossover-method
                     :id (get-fresh-ancestry-id))
              (ancestors crossed)))
      (if changedp
          (values crossed a-point b-point)
          (values crossed nil nil)))))

(defmethod genome-string-without-separator ((obj clang))
  (unlines (remove-if {string= *clang-genome-separator*}
                      (split-sequence #\Newline (genome-string obj)))))

(defmethod genome-string ((clang clang) &optional stream)
  (format stream "~a~%~a~%~a"
          (genome-string (mitochondria clang))
          *clang-genome-separator*
          (genome clang)))

(defmethod clang-tidy ((clang clang))
  (setf (genome-string clang)
        (with-temp-file-of (src (ext clang)) (genome-string clang)
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
            (if (zerop exit) (file-to-string src) (genome-string clang))))))

(defmethod clang-format ((obj clang) &optional style)
  (setf (genome-string obj)
        (with-temp-file-of (src (ext obj)) (genome-string obj)
          (multiple-value-bind (stdout stderr exit)
              (shell "clang-format ~a ~a "
                     (if style
                         (format nil "-style=~a" style)
                         (format nil
                           "-style='{BasedOnStyle: Google~
                                     AllowShortBlocksOnASingleLine: false~
                                     AllowShortCaseLabelsOnASingleLine: false~
                                     AllowShortFunctionsOnASingleLine: false~
                                     AllowShortIfStatementsOnASingleLine: false~
                                     AllowShortLoopsOnASingleLine: false}'"))
                     src)
            (declare (ignorable stderr))
            (if (zerop exit) stdout (genome-string obj))))))

(defmethod (setf genome-string) (text (clang clang))
  (setf (genome clang) (extract-clang-genome text)))

(defmethod (setf genome-string) :around (text (obj clang))
  (prog1
    (call-next-method)
    (setf (fitness obj) nil)
    (update-asts obj)))

(defmethod lines ((obj clang))
  (split-sequence '#\Newline (genome-string obj)))

(defmethod (setf lines) (new (obj clang))
  (setf (genome-string obj) (format nil "~{~a~^~%~}" new)))

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
