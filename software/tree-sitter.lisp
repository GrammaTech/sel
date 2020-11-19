;;;; tree-sitter.lisp --- software representations with a tree-sitter backend.
;;; NOTE: the grammar.json and node-types.json for each language are currently
;;;       expected to be in /usr/share/tree-sitter/$language/
(uiop:define-package :software-evolution-library/software/tree-sitter
  (:nicknames :sel/software/tree-sitter :sel/sw/tree-sitter
              :sel/software/ts :sel/sw/ts)
  (:use :gt/full
        :babel
        :cl-json
        :software-evolution-library
        :software-evolution-library/utility/json
        :software-evolution-library/utility/range
        :software-evolution-library/software/parseable
        :software-evolution-library/software/non-homologous-parseable
        :software-evolution-library/components/file
        :software-evolution-library/components/formatting)
  (:import-from :cffi :translate-camelcase-name :load-foreign-library-error)
  (:import-from :cl-tree-sitter :register-language)
  (:shadowing-import-from :cl-tree-sitter :parse-string)
  (:export :tree-sitter-ast
           :tree-sitter))
(in-package :software-evolution-library/software/tree-sitter)
(in-readtable :curry-compose-reader-macros)

(define-software tree-sitter (non-homologous-parseable) ()
  (:documentation "tree-sitter software representation."))


;;; Shared object set-up
(eval-always
  (defvar *superclass->language* (make-hash-table)
    "Maps an AST superclass to its tree-sitter language. When
convert is called, the superclass can then be used to look up
which language--and its relevant shared object--should be used
to parse the string.")

  (defvar *tree-sitter-language-directories*
    (or (when-let ((env (getenv "SEL_TREE_SITTER_LANGUAGE_DIR")))
          (split-sequence #\, env))
        '("/usr/share/tree-sitter/"))
    "A list of directories that hold directories of json files
defining supported tree-sitter languages.  These directories are
searched to populate `*tree-sitter-language-files*'.")

  (defun collect-tree-sitter-language-files
      (&optional (directories *tree-sitter-language-directories*) &aux results)
    "Collect tree-sitter language definition files."
    (dolist (dir directories results)
      (walk-directory
       dir
       (lambda (file)
         (let* ((base (pathname-directory-pathname file))
                (name (string-trim '(#\/) (pathname-relativize dir base))))
           (push (list name
                       (merge-pathnames "grammar.json" base)
                       (merge-pathnames "node-types.json" base))
                 results)))
       :test [{string= "node-types"} #'namestring #'pathname-name])))

  (defvar *tree-sitter-language-files* (collect-tree-sitter-language-files)
    "Files defining tree sitter languages.")

  (defvar *tree-sitter-superclasses*
    '((:c (:parseable-statement c--statement c-function-definition))
      (:java (:parseable-statement java-statement))
      (:javascript (:parseable-statement javascript--statement))
      (:python (:parseable-statement python--compound-statement python--simple-statement))))

  (defun tree-sitter-ast-classes (name grammar-file node-types-file)
    (nest
     (flet ((alternate-class-name (name)
              (string-case name
                ("GO" "GOLANG")
                (t name)))))
     (let* ((path-name (replace-all name "/" "-"))
            (class-name (alternate-class-name (string-upcase path-name)))
            (class-keyword (make-keyword class-name))))
     `((register-tree-sitter-language
        ,(string-join (list "tree-sitter" path-name) #\-)
        ,class-keyword
        ',(intern (string-join (list class-name "AST") #\-)
                  :software-evolution-library/software/tree-sitter))
       ,(apply #'create-tree-sitter-classes
               node-types-file
               grammar-file
               class-name
               (when-let ((superclasses
                           (aget class-keyword *tree-sitter-superclasses*)))
                 (list :superclass-to-classes superclasses)))))))

(defmacro register-tree-sitter-language (lib-name language ast-superclass)
  "Setup LANGUAGE to map to AST-SUPERCLASS and use LIB-NAME for parsing.
BODY is only executed if LIB-NAME is found. This is useful in cases where
a subset of the all the tree-sitter languages is present in an environment
and only one of them is desired and used."
  `(eval-always
     (handler-case
         (progn
           (register-language ,language ,lib-name)
           (setf (gethash ,ast-superclass *superclass->language*) ,language))
       (load-foreign-library-error ()
         (warn "Failed to load '~a'. Support for '~a' will not be available."
               ,lib-name ,language)))))


;;; Defining tree-sitter classes
(eval-always
  (defclass tree-sitter-ast (non-homologous-ast)
    ()
    (:documentation "AST for input from tree-sitter."))

  (defun convert-name (name-string)
    (camel-case-to-lisp (substitute #\- #\_  name-string)))

  (defun translate-to-slot-name (name prefix)
    "Translate NAME into a slot name that is unlikely
     to collide with inherited slot names by prepending
     PREFIX. If NAME is 'children', the prefix is not
     attached."
    (cond
      ((string= name :children)
       (symbolicate name))
      (t (symbolicate prefix '- name))))

  ;; NOTE: while a :child-order annotation is currently being generated
  ;;       for every ast converted from a string, having the slot order
  ;;       is useful for converting from a list where the :child-order
  ;;       annotation would need to be generated and slot order is likely
  ;;       already correct except in a few rare cases.
  (defun slot-order (name expected-fields grammar-rules
                     &aux dependencies fields
                       (expected-fields (mapcar #'car expected-fields)))
    "Return the slot order of the fields in the production specified
by NAME. If NIL is returned, there are either no fields or the order
of fields needs to be determined at parse-time."
    (labels ((add-dependency (preceding-fields field)
               "Add a dependency for each on each item
              in PRECEDING-FIELDS for field."
               ;; NOTE: this can potentially add duplicate dependencies
               ;;       though this likely isn't much of a problem.
               (mapc
                (lambda (preceding-field)
                  (unless (equal preceding-field field)
                    (push (list preceding-field field) dependencies)))
                preceding-fields))
             (add-field (name)
               "Add NAME to the list of used fields."
               ;; NOTE: avoid adding the same field more than once.
               ;;       This can occur with 'CHOICE' rules.
               (pushnew name fields :test #'equal))
             (handle-choice (rule &optional preceding-fields visited-rules)
               "Handle RULE as a 'CHOICE' rule."
               (remove-duplicates
                (iter
                  (for member in (aget :members rule))
                  (appending
                   (handle-rule member preceding-fields visited-rules)))
                :test #'equal))
             (handle-seq (rule &optional preceding-fields visited-rules)
               "Handle RULE as a 'SEQ' rule."
               (iter
                 (for member in (aget :members rule))
                 (for preceding
                      initially preceding-fields
                      then (or (handle-rule member preceding visited-rules)
                               preceding))
                 (finally (return preceding))))
             (handle-repeat (rule &optional preceding-fields visited-rules)
               "Handle RULE as a 'REPEAT' rule."
               ;; NOTE: perform twice to loop the ending field of the repeat
               ;;       back to the front of the repeat. This will create
               ;;       an inconsistency if one exists. Also note that
               ;;       a dependency of a field on itself is ignored.
               (iter
                 (repeat 2)
                 (for preceding
                      initially preceding-fields
                      then (or (handle-rule
                                (aget :content rule) preceding visited-rules)
                               preceding))
                 (finally (return preceding))))
             (handle-field (rule &optional preceding-fields
                            &aux (name (aget :name rule)))
               "Handle RULE as a 'FIELD' rule and add a dependency from
              the field to PRECEDING-FIELDS if it exists."
               (when (member (make-keyword (convert-name name)) expected-fields)
                 (add-field name)
                 (add-dependency preceding-fields name)
                 (list name)))
             (handle-rule (rule &optional preceding-fields visited-rules)
               "Handles dispatching RULE to its relevant rule handler."
               ;; NOTE: this will throw an error if the json schema for
               ;;       the grammar.json files has changed.
               (string-ecase (aget :type rule)
                 (("ALIAS" "BLANK" "IMMEDIATE_TOKEN" "TOKEN" "PATTERN" "STRING"))
                 ("CHOICE" (handle-choice rule preceding-fields visited-rules))
                 ("FIELD" (handle-field rule preceding-fields))
                 (("PREC" "PREC_DYNAMIC" "PREC_LEFT" "PREC_RIGHT")
                  ;; pass-through
                  (handle-rule
                   (aget :content rule) preceding-fields visited-rules))
                 (("REPEAT" "REPEAT1")
                  (handle-repeat rule preceding-fields visited-rules))
                 ("SEQ" (handle-seq rule preceding-fields visited-rules))
                 ("SYMBOL"
                  (let* ((name-string (aget :name rule))
                         (name (make-keyword (convert-name name-string))))
                    ;; NOTE: the rules starting with an #\_ are special
                    ;;       and are the only ones that should be considered
                    ;;       when searching for fields that may be down the line
                    ;;       in different rules.
                    (when-let ((name (and (eql #\_ (aref name-string 0))
                                          (not (member name visited-rules))
                                          (aget name grammar-rules))))
                      (handle-rule name
                                   preceding-fields
                                   (cons name visited-rules))))))))
      ;; NOTE: tree-sitter/cli/src/generate/grammar-schema.json
      ;;       The grammar schema contains information on the
      ;;       possible rule types.
      (let* ((name-keyword (make-keyword (convert-name name)))
             (name-rule (aget name-keyword grammar-rules)))
        (when name-rule
          (handle-rule name-rule nil (list name-keyword))
          (mapcar
           #'make-keyword
           (mapcar
            #'convert-name
            (handler-case (sort fields (toposort dependencies :test #'equal))
              (inconsistent-graph ()
                ;; NOTE: the order doesn't matter as a :child-order
                ;;       annotation will be used instead of it.
                ;;       This is only provided for #'sorted-children
                ;;       to use.
                fields))))))))

  (defun create-tree-sitter-classes
      (node-types-file grammar-file name-prefix
       &key superclass-to-classes
       &aux (subtype->supertypes (make-hash-table))
         (symbols-to-export (make-hash-table))
         (ast-superclass (symbolicate
                          name-prefix
                          "-"
                          (convert-name "ast"))))
    (labels ((initialize-subtype->supertypes ()
               "Initialize subtype->supertypes with the super types that
                aren't parsed from the json files."
               (mapc
                (lambda
                    (types-list &aux (supertype (symbolicate (car types-list))))
                  (mapc
                   (lambda (subtype)
                     (push supertype (gethash subtype subtype->supertypes)))
                   (cdr types-list)))
                superclass-to-classes)
               ;; Add super class into all of these to ensure it's present. When
               ;; add-supertypes-to-subtypes is called, it will need to remove
               ;; it.
               (maphash-keys
                (lambda (subtype)
                  (push ast-superclass (gethash subtype subtype->supertypes)))
                subtype->supertypes))
             (make-class-name (&optional name-string keep-underscores-p)
               "Create a class name based on NAME-STRING and add it to the
                symbols that need exported. If keep-underscores-p is provided,
                the underscores in NAME-STRING will be replaced with hyphens."
               ;; NOTE: this has the potential for name clashes
               ;;       though it's probably unlikely.
               (lret ((name
                       (if name-string
                           (symbolicate
                            name-prefix
                            "-"
                            (if keep-underscores-p
                                (string-upcase name-string)
                                (convert-name name-string)))
                           (symbolicate name-prefix))))
                 (ensure-gethash name symbols-to-export t)))
             (make-accessor-name (name-keyword)
               "Create an accessor name based on NAME-KEYWORD and add it to the
                symbols that need exported."
               (lret ((name (symbolicate
                             name-prefix
                             "-"
                             name-keyword)))
                 (ensure-gethash name symbols-to-export t)))
             (get-supertypes-for-type (type)
               "Retrieve the supertypes of TYPE."
               (gethash (make-class-name type) subtype->supertypes))
             (add-supertype-to-subtypes (supertype subtypes)
               "Add SUPERTYPE to the list of superclasses for
                each type in SUBTYPES."
               (mapc
                (lambda (subtype &aux (name (aget :type subtype)))
                  (symbol-macrolet ((subtype-hash (gethash (make-class-name name)
                                                           subtype->supertypes)))
                    (setf subtype-hash
                          (cons (make-class-name supertype)
                                ;; Remove ast-superclass to prevent
                                ;; circular class dependency.
                                (remove ast-superclass subtype-hash)))))
                subtypes))
             (create-slot (field &aux (name-keyword (car field)))
               "Create a slot based on FIELD."
               (let ((name (make-accessor-name name-keyword)))
                 `(,name :accessor ,name
                         :initarg ,name-keyword
                         :initform nil)))
             (create-slots (fields)
               "Create the slots for a new class based on FIELDS and CHILDREN.
                Currently, types aren't supported, but there is enough
                information to limit slots to certain types."
               ;; NOTE: there is a small possibility for name overlaps when
               ;;       generating these slots.
               (mapcar #'create-slot fields))
             (create-supertype-class (type subtypes
                                      &aux (class-name (make-class-name type)))
               "Create a new class for subtypes to inherit from."
               (add-supertype-to-subtypes type subtypes)
               `(defclass ,class-name
                    (,@(or (get-supertypes-for-type type)
                           `(,ast-superclass)))
                  ()
                  (:documentation ,(format nil "Generated for ~a." type))))
             (create-type-class (type fields children grammar-rules
                                 &aux (class-name (make-class-name type)))
               "Create a new class for TYPE using FIELDS and CHILDREN for slots."
               (let ((child-slot-order
                       (when fields
                         (mapcar
                          (lambda (slot-keyword)
                            (cons
                             (translate-to-slot-name slot-keyword name-prefix)
                             (if (aget :multiple (aget slot-keyword fields))
                                 0
                                 1)))
                          (slot-order type fields grammar-rules)))))
                 `(defclass ,class-name
                      (,@(or (get-supertypes-for-type type)
                             `(,ast-superclass)))
                    (,@(create-slots fields)
                     (child-slots
                      :initform
                      ',(if children
                            (append child-slot-order '((children . 0)))
                            child-slot-order)
                      :allocation :class))
                    ;; NOTE: this is primarily for determing which rule this
                    ;;       was generated for.
                    (:documentation ,(format nil "Generated for ~a." type)))))
             (create-terminal-symbol-class (type)
               "Create a new class that represents a terminal symbol.
                In the case that there's a non-terminal with the same name,
                append '-terminal' to the end of it."
               `(defclass ,(if (gethash (symbolicate name-prefix "-"
                                                     (string-upcase type))
                                        symbols-to-export)
                               (make-class-name
                                (symbolicate type "-" 'terminal) t)
                               (make-class-name type t))
                    ()
                  ()
                  (:documentation
                   ,(format nil "Generated for terminal symbol '~a'" type))))
             (create-node-class
                 (grammar-rules node-type
                  &aux (type (aget :type node-type))
                    (subtypes (aget :subtypes node-type))
                    (named-p (aget :named node-type)))
               "Create a class for  NODE-TYPE."
               (cond
                 (subtypes (create-supertype-class type subtypes))
                 (named-p
                  (create-type-class
                   type
                   (aget :fields node-type)
                   (assoc :children node-type)
                   grammar-rules))
                 ;; Terminal Symbol
                 (t (create-terminal-symbol-class type)))))
      (let* ((*json-identifier-name-to-lisp* #'convert-name)
             (node-types (decode-json-from-string
                          (file-to-string node-types-file)))
             (grammar-rules (aget
                             :rules
                             (decode-json-from-string
                              (file-to-string grammar-file)))))
        (initialize-subtype->supertypes)
        `(progn
           (eval-always
             ;; TODO IF EVER NEEDED:
             ;;   add a parameter for passing in extra super classes.
             ;;   This could be useful for mix-ins.
             (define-software ,(make-class-name) (tree-sitter)
               ()
               (:documentation
                ,(format nil "~a tree-sitter software representation."
                         name-prefix)))

             ;; TODO IF EVER NEEDED:
             ;;   add a parameter for passing in extra super classes.
             ;;   This could be useful for mix-ins.
             (defclass ,(make-class-name "ast") (tree-sitter-ast)
               ;; NOTE: ensure there is always a children slot.
               ;;       This is important for classes that don't have
               ;;       it but can have comments mixed in.
               ((children :accessor ,(make-accessor-name :children)
                          :initarg :children
                          :initform nil))
               (:documentation
                ,(format nil "AST for ~A from input via tree-sitter."
                         name-prefix)))

             ,@(mapcar {create-node-class grammar-rules} node-types)

             ;; NOTE: the following are to handle results returned from
             ;;       cl-tree-sitter.
             (defclass ,(make-class-name "comment") (,ast-superclass)
               ()
               (:documentation "Generated for parsed comments."))

             (defclass ,(make-class-name "error") (,ast-superclass)
               ((children :initarg :children :initform nil)
                (child-slots :initform '((children . 0))
                             :allocation :class))
               (:documentation "Generated for parsing errors."))

             (export ',(iter
                         (for (symbol) in-hashtable symbols-to-export)
                         (collect symbol))))

           (defmethod convert
               ((to-type (eql ',ast-superclass)) (spec ,ast-superclass)
                &key &allow-other-keys)
             spec)

           (defmethod convert ((to-type (eql ',ast-superclass)) (spec list)
                               &key string-pass-through &allow-other-keys)
             (convert 'tree-sitter-ast spec
                      :superclass to-type
                      :string-pass-through string-pass-through))

           (defmethod convert ((to-type (eql ',ast-superclass)) (string string)
                               &key &allow-other-keys)
             (convert 'tree-sitter-ast string :superclass to-type))

           (defmethod parse-asts ((obj ,(make-class-name))
                                  &optional (source (genome-string obj)))
             (convert ',(make-class-name "ast") source))

           (define-mutation ,(make-class-name "mutation") (parseable-mutation) ()
             (:documentation
              ,(format nil "Mutation interface for ~a software objects."
                       name-prefix))))))))


;;; tree-sitter parsing
(defun expected-slot-order-p (parsed-order expected-order)
  "Compare ORDER with child-slots of instance to see if
they should produce the same ordering."
  (labels ((consolidate-key (cons &aux (car (car cons)))
             "Access the key used for consolidating runs."
             (etypecase car
               (cons (car car))
               (symbol car)
               (integer 'children)))
           (consolidate-runs (order)
             "Consolidate all runs of the same slot into just one
              instance of the symbol that represents that slot, and
              flatten into a list of slots."
             (mapcar #'consolidate-key
                     (collapse-duplicates order :key #'consolidate-key)))
           (duplicate-exists-p (order)
             "Return T if a duplicate exists in ORDER."
             (not (equal order (remove-duplicates order))))
           (check-order (order expected-order)
             "Return T if ORDER is equivalent to EXPECTED-ORDER."
             (cond
               ((null order) t)
               ((null expected-order) nil)
               ((eql (car order) (car expected-order))
                (check-order (cdr order) (cdr expected-order)))
               (t (check-order order (cdr expected-order))))))
    (let ((order (consolidate-runs parsed-order)))
      (unless (duplicate-exists-p order)
        (check-order order (consolidate-runs expected-order))))))

(defun convert-initializer
    (spec prefix superclass
     &aux (*package* (symbol-package superclass))
       (class (symbolicate prefix '-
                           (let ((type (car spec)))
                             ;; The form can either be
                             ;; - :type
                             ;; - (:slot-name :type)
                             (if (listp type)
                                 (cadr type)
                                 type))))
       (instance (make-instance
                  class
                  :annotations
                  `((:range-start ,(caadr spec))
                    (:range-end . ,(cdadr spec)))))
       (error-p (eql class (symbolicate prefix '-error))))
  "Initialize an instance of SUPERCLASS with SPEC."
  (labels ((get-converted-fields ()
             "Get the value of each field after it's been converted
              into an AST."
             (iter
               (for field in (caddr spec))
               (for converted-field = (convert superclass field
                                               :string-pass-through t))
               (for slot-info = (car field))
               ;; cl-tree-sitter appears to put the
               ;; slot name first unless the list goes
               ;; into the children slot.
               (if (and (listp slot-info) (not error-p))
                   (collect (list (car slot-info)
                                  converted-field)
                     into fields)
                   (collect converted-field into children))
               (finally
                (return
                  (if children
                      (push `(:children ,children) fields)
                      fields)))))
           (merge-same-fields (field-list)
             "Merge all fields that belong to the same slot.
              This is used for setting slots with an arity of 0."
             (mapcar
              (lambda (grouping)
                (apply #'append
                       (list (caar grouping))
                       (mapcar #'cdr grouping)))
              (assort field-list :key #'car)))
           (set-slot-values (slot-values)
             "Set the slots in instance to correspond to SLOT-VALUES."
             (mapc
              (lambda (list)
                (setf (slot-value
                       instance (translate-to-slot-name (car list) prefix))
                      (if (null (cddr list))
                          (cadr list)
                          (cdr list))))
              slot-values))
           (update-slots-based-on-arity ()
             "Update any slot in instance that needs to be converted to a list
              to match its arity. This is primarily for #'sorted-children."
             (mapc
              (lambda (slot-arity
                       &aux (slot (car slot-arity)))
                (symbol-macrolet ((slot-value (slot-value instance slot)))
                  (unless (listp slot-value)
                    (setf slot-value (list slot-value)))))
              (remove-if-not
               {eql 0}
               (or (ast-annotation instance :child-order)
                   (slot-value instance 'child-slots))
               :key #'cdr)))
           (set-child-order-annotation ()
             "Set the child-order annotation of instance. This
              is currently set on every AST as some ASTs store
              things in the children slot instead of making a
              slot for it."
             (let* ((sorted-children
                      (sort (remove nil (children instance))
                            (lambda (start end)
                              (source-<
                               (make-instance
                                'source-location
                                :line (cadr start)
                                :column (car start))
                               (make-instance
                                'source-location
                                :line (cadr end)
                                :column (car end))))
                            :key
                            (lambda (child)
                              (car
                               (ast-annotation child :range-start)))))
                    (order (mapcar {position _ instance}
                                   sorted-children)))
               (unless (expected-slot-order-p order (child-slots instance))
                 (setf (slot-value instance 'annotations)
                       (cons (cons :child-order order)
                             (ast-annotations instance)))))))
    (set-slot-values (merge-same-fields (get-converted-fields)))
    (update-slots-based-on-arity)
    (set-child-order-annotation)
    instance))

(defun position-after-leading-newline (str &aux (pos 0))
  "Returns 1+ the position of the first newline in STR,
assuming it can be reached only by skipping over whitespace
or comments.  NIL if no such newline exists."
  (loop
    (when (>= pos (length str)) (return nil))
    (let ((c (elt str pos)))
      (case c
        (#\Newline (return (1+ pos)))
        ((#\Space #\Tab)
         (incf pos))
        (t (return nil))))))

(defun move-newlines-down (ast)
  "Destructively modify AST, pushing newlines down into child ASTs for
statement AST types."
  (iter
    (for child in (sorted-children ast))
    (for after-text in (cdr (interleaved-text ast)))
    (for i upfrom 1)
    (when-let ((pos (and (typep child 'parseable-statement)
                         (position-after-leading-newline after-text))))
      ;; Move the [0, pos) prefix of after-text containing the newline
      ;; down into the child node.
      (setf (slot-value child 'interleaved-text)
            (append (butlast (interleaved-text child))
                    (list (concatenate 'string
                                       (lastcar (interleaved-text child))
                                       (subseq after-text 0 pos)))))
      (setf (nth i (slot-value ast 'interleaved-text))
            (subseq after-text pos)))
    (finally (return ast))))

(defun fix-newlines (ast)
  "Fix newlines in ASTs by pushing newlines down into child
statements.  This allows for mutation operations to insert and
replace statements with newlines already present in the new
ASTs, obviating the need for fixups to add missing newlines.
This method is destructive and, therefore, can only be utilized
during AST creation to respect functional trees invariants."
  (mapcar #'move-newlines-down ast))

(defun convert-spec (spec prefix superclass
                     &aux (package (symbol-package superclass)))
  "Convert SPEC into an ast of type SUPERCLASS. PREFIX is used to find the
correct class name for subclasses of SUPERCLASS."
  (lret ((instance
          (make-instance
           (symbol-cat-in-package
            package
            prefix
            (let ((class (aget :class spec)))
              (if (stringp class)
                  (nest (make-keyword)
                        (string-upcase)
                        (translate-camelcase-name)
                        class)
                  class))))))
    (iter
      (iter:with child-types = (child-slots instance))
      (iter:with annotations = nil)
      (for (slot . value) in (adrop '(:class) spec))
      (for key = (format-symbol package "~a" slot))
      (for translated-key = (translate-to-slot-name key prefix))
      (cond
        ((slot-exists-p instance translated-key)
         (setf (slot-value instance translated-key)
               (if-let ((spec (find translated-key child-types :key #'car)))
                 (ematch spec
                   ;; (cons key arity)
                   ((cons _ 1) (convert superclass value))
                   ((cons _ 0) (iter (for item in value)
                                 (collect (convert superclass item)))))
                 value)))
        ;; Account for slots in superclasses.
        ((slot-exists-p instance key) (setf (slot-value instance key) value))
        (t (push (cons slot value) annotations)))
      (finally
       (with-slots ((annotations-slot annotations)) instance
         (setf annotations-slot (append annotations annotations-slot)))))))

(defun get-language-from-superclass (superclass)
  "Get the tree-sitter  language associated with SUPERCLASS."
  (or (gethash superclass *superclass->language*)
      (error "No tree-sitter language known for ~a." superclass)))

(defmethod convert ((to-type (eql 'tree-sitter-ast)) (spec tree-sitter-ast)
                    &key &allow-other-keys)
  "Pass thru an existing tree-sitter AST. This useful in manual AST creation."
  spec)

(defmethod convert ((to-type (eql 'tree-sitter-ast)) (spec list)
                    &key superclass string-pass-through &allow-other-keys)
  "Create a c-tree-sitter AST from the SPEC (specification) list."
  (if string-pass-through
      (convert-initializer
       spec (get-language-from-superclass superclass) superclass)
      (convert-spec
       spec (get-language-from-superclass superclass) superclass)))

(defmethod convert ((to-type (eql 'tree-sitter-ast)) (string string)
                    &key superclass &allow-other-keys
                    &aux (line-octets
                          (map
                           'vector
                           #'string-to-octets
                           (serapeum:lines string :keep-eols t))))
  (labels
      ((safe-subseq
           (start end
            &aux (start-loc
                  (make-instance
                   'source-location
                   :line (cadr start) :column (car start)))
              (end-loc
               (make-instance
                'source-location
                :line (cadr end) :column (car end))))
         "Return STRING in the range [START, END) or an empty string if
         the offsets are invalid."
         (if (source-< start-loc end-loc)
             (octets-to-string
              (source-range-subseq
               line-octets
               (make-instance 'source-range :begin start-loc :end end-loc)))
             ""))
       (start (ast)
         "Return the start offset into STRING from the AST representation."
         (car (ast-annotation ast :range-start)))
       (end (ast)
         "Return the end offset into STRING from the AST representation."
         (car (ast-annotation ast :range-end)))
       (ranges (children from to)
         "Return the offsets of the source text ranges between CHILDREN."
         (iter
           (for child in children)
           (for prev previous child)
           (collect (cons (if prev (end prev) from) (start child))
             into ranges)
           (finally
            (return (append ranges (list (cons (end child) to)))))))
       (w/interleaved-text (ast from to
                            &aux (children (sorted-children ast)))
         "Destructively modify AST to populate the INTERLEAVED-TEXT
         field with the source text to be interleaved between the
         children of AST."
         (if children
             (progn
               (setf (slot-value ast 'interleaved-text)
                     (mapcar (lambda (range)
                               (destructuring-bind (from . to) range
                                 (safe-subseq from to)))
                             (ranges children from to)))
               (mapc (lambda (child)
                       (w/interleaved-text child (start child) (end child)))
                     children))
             (setf (slot-value ast 'interleaved-text)
                   (list (safe-subseq (start ast) (end ast)))))
         (setf (slot-value ast 'annotations)
               (adrop '(:range-start :range-end) (slot-value ast 'annotations)))
         ast))
    (nest
     (fix-newlines)
     (w/interleaved-text
      (convert to-type
               (parse-string (get-language-from-superclass superclass) string)
               :superclass superclass
               :string-pass-through t)
      '(0 0)
      (if (emptyp line-octets)
          '(0 0)
          (list (length (last-elt line-octets))
                (1- (length line-octets))))))))


;;;; Tree-sitter language definitions.

(progn #.`(progn ,@(mappend {apply #'tree-sitter-ast-classes}
                            *tree-sitter-language-files*)))

(defmacro when-class-defined ((software-class) &body body)
  (when (handler-case (find-class software-class)
          (error ()))
    `(progn
       ,@body)))

(defun interpreted-phenome (obj bin)
  "Create a phenotype of the interpreted software OBJ."
  (to-file obj bin)
  (values bin 0 nil nil nil))

(defmethod get-parent-full-stmt (obj (ast tree-sitter-ast))
  (if (typep ast 'parseable-statement)
      ast
      (get-parent-full-stmt obj (get-parent-ast obj ast))))


;;;; Python
;;; Move this to its own file?
(when-class-defined (python)

  
  ;; Methods common to all software objects
  (defmethod phenome ((obj python) &key (bin (temp-file-name)))
    (interpreted-phenome obj bin))

  (defmethod enclosing-scope ((obj python) (ast python-ast))
    "Return the enclosing scope of AST in OBJ.
OBJ python software object
AST ast to return the enclosing scope for"
    (or (find-if (lambda (parent)
                   ;; Normal case: AST is a member of a class
                   ;; of ASTs defining a new scope.
                   (typep parent '(or
                                   python-function-definition
                                   python-class-definition
                                   python-lambda)))
                 (cdr (get-parent-asts obj ast)))
        (genome obj)))


  
  ;; Implement the generic format-genome method for python objects.
  (defmethod format-genome ((obj python) &key)
    "Format the genome of OBJ using YAPF (Yet Another Python Formatter)."
    (yapf obj)))


;;;; Javascript
(when-class-defined (python)

  
  ;; Methods common to all software objects
  (defmethod phenome ((obj javascript) &key (bin (temp-file-name)))
    (interpreted-phenome obj bin))

  (defmethod enclosing-scope ((obj javascript) (ast javascript-ast))
    "Return the enclosing scope of AST in OBJ.
OBJ javascript software object
AST ast to return the enclosing scope for"
    (or (find-if (lambda (ast)
                   (typep ast
                          '(or
                            javascript-statement-block
                            javascript-function
                            javascript-program
                            javascript-arrow-function
                            javascript-for-statement
                            javascript-for-in-statement)))
                 (cdr (get-parent-asts obj ast)))
        (genome obj))))
