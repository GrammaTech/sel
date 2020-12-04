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
           :tree-sitter
           :ast-type-to-rebind-p
           :get-vars
           :in-class-def-p
           :get-asts-in-namespace
           :collect-var-uses
           :collect-fun-uses))
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
        (remove-if-not
         #'probe-file `("/usr/share/tree-sitter/"
                        "/usr/local/share/tree-sitter/"
                        ,@(when-let (home (getenv "HOME"))
                            (list (string+ home "/.local/share/tree-sitter/")))))
        (prog1 nil (warn "No tree-sitter language directory found.")))
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

  (defvar *tree-sitter-software-superclasses*
    '((:python software-indentation)))

  (defvar *tree-sitter-base-ast-superclasses*
    '((:python indentation)))

  (defvar *tree-sitter-ast-superclasses*
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
       ,(create-tree-sitter-classes
         node-types-file
         grammar-file
         class-name
         :superclass-to-classes
         (aget class-keyword *tree-sitter-ast-superclasses*)
         :base-class-superclasses
         (aget class-keyword *tree-sitter-base-ast-superclasses*)
         :software-superclasses
         (aget class-keyword *tree-sitter-software-superclasses*))))))

(defmacro register-tree-sitter-language (lib-name language ast-superclass)
  "Setup LANGUAGE to map to AST-SUPERCLASS and use LIB-NAME for parsing."
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
       &key superclass-to-classes base-class-superclasses
         software-superclasses
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
             (create-slot (field)
               "Create a slot based on FIELD."
               (let ((name (make-accessor-name (car field))))
                 `(,name :accessor ,name
                         :initarg ,(make-keyword name)
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
             (define-software ,(make-class-name) (tree-sitter
                                                  ,@software-superclasses)
               ()
               (:documentation
                ,(format nil "~a tree-sitter software representation."
                         name-prefix)))

             ;; TODO IF EVER NEEDED:
             ;;   add a parameter for passing in extra super classes.
             ;;   This could be useful for mix-ins.
             (defclass ,(make-class-name "ast") (tree-sitter-ast
                                                 ,@base-class-superclasses)
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

             (define-mutation ,(make-class-name "mutation") (parseable-mutation)
               ()
               (:documentation
                ,(format nil "Mutation interface for ~a software objects."
                         name-prefix)))

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
             (convert ',(make-class-name "ast") source)))))))


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
             (equal order (sort order (ordering expected-order)))))
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
  "Checks if SOFTWARE-CLASS is defined at compile-time. If so,
it expands into BODY. Otherwise, the expansion is nil."
  (when (find-class software-class nil)
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

(defgeneric ast-type-to-rebind-p (ast)
  (:documentation "Return T if AST is of a type where its variables/functions
should be rebound.")
  (:method (ast) nil))

(defmethod rebind-vars ((ast tree-sitter-ast)
                        var-replacements fun-replacements)
  "Replace variable and function references, returning a new AST.
* AST node to rebind variables and function references for
* VAR-REPLACEMENTS list of old-name, new-name pairs defining the rebinding
* FUN-REPLACEMENTS list of old-function-info, new-function-info pairs defining
the rebinding"
  (if (ast-type-to-rebind-p ast)
      (copy ast :interleaved-text (mapcar {rebind-vars _ var-replacements
                                                         fun-replacements}
                                          (interleaved-text ast)))
      (apply #'copy ast
             (mappend (lambda (child-slot)
                        (destructuring-bind (name . arity) child-slot
                          (list (make-keyword name)
                                (cond ((= arity 0)
                                       (mapcar {rebind-vars _ var-replacements
                                                              fun-replacements}
                                               (slot-value ast name)))
                                      ((slot-value ast name)
                                       (rebind-vars (slot-value ast name)
                                                    var-replacements
                                                    fun-replacements))))))
                      (child-slots ast)))))


;;;; Python
;;; Move this to its own file?
(when-class-defined (python)

  (defmethod ast-type-to-rebind-p ((ast python-identifier)) t)

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
                 (get-parent-asts* obj ast))
        (genome obj)))

  (defmethod scopes ((obj python) (target-ast python-ast)
                     &aux (enclosing-scope (enclosing-scope obj target-ast)))
    "Return lists of variables in each enclosing scope of AST.
Each variable is represented by an alist containing :NAME, :DECL, and :SCOPE.
OBJ python software object
AST ast to return the scopes for"
    ;; NOTE: in the unlikely event this function becomes a bottleneck, it may
    ;;       make sense to cache the get-vars calls.
    (labels ((build-alist (ast name scope)
               "Return an alist containing :name, :decl, and :scope for the
                variable in AST."
               `((:decl . ,ast)
                 (:name . ,name)
                 (:scope . ,scope)))
             (build-alist* (get-vars-alist
                            &aux (definition (aget :definition get-vars-alist)))
               "Return an alist containing :name, :decl, and :scope for
                GET-VARS-ALIST."
               (build-alist
                (if (typep definition 'python-identifier)
                    (get-parent-full-stmt obj definition)
                    definition)
                (aget :name get-vars-alist)
                (aget :scope get-vars-alist)))
             (name-in-get-vars-p (obj ast name)
               "Return the variable alist that corresponds to
                NAME if it exists."
               (find-if
                (lambda (alist)
                  (equal name (aget :name alist)))
                (get-vars obj ast)))
             (find-get-vars-binding (obj ast enclosing-scope name)
               "Find a variable bound in AST that is named NAME and in
                ENCLOSING-SCOPE."
               (find-if
                (lambda (ast)
                  (eq (aget :scope (name-in-get-vars-p obj ast name))
                      enclosing-scope))
                ast))
             (find-declaration (function ast)
               "Find the declaration that is returned by FUNCTION
                starting at AST."
               (find-if
                (lambda (ast)
                  (when-let ((declaration (funcall function ast)))
                    (return-from find-declaration declaration)))
                ast))
             (find-nonlocal-binding* (name enclosing-scope)
               "Find the nonlocal binding for NAME in ENCLOSING-SCOPE."
               (find-declaration
                (lambda (ast)
                  (if (typep ast 'python-nonlocal-statement)
                      (when (and
                             (find-if
                              (lambda (identifier)
                                (equal name (source-text identifier)))
                              (remove-if-not {typep _ 'python-identifier}
                                             (python-children ast)))
                             (not (eq enclosing-scope (genome obj))))
                        (find-nonlocal-binding
                         name (enclosing-scope obj enclosing-scope)))
                      (find-get-vars-binding obj ast enclosing-scope name)))
                (get-asts-in-namespace obj enclosing-scope)))
             (find-nonlocal-binding (name enclosing-scope)
               "Find and build the alist for the nonlocal binding for NAME
                in ENCLOSING-SCOPE."
               (build-alist
                (find-nonlocal-binding* name enclosing-scope)
                name enclosing-scope))
             (find-global-binding
                 (identifier &aux (genome (genome obj))
                               (name (car (interleaved-text identifier))))
               "Find the global binding for NAME in ENCLOSING-SCOPE."
               (build-alist
                (find-declaration
                 (lambda (ast)
                   (find-get-vars-binding obj ast genome name))
                 (remove nil (children genome)))
                name genome))
             (find-enclosing-bindings (scope)
               "Find the enclosing bindings that occur in scope."
               (mapcar
                #'build-alist*
                (remove-if-not
                 (lambda (alist &aux (attributes (aget :attributes alist)))
                   (cond
                     ;; NOTE: imports behave differently than other bindings
                     ;;       that are available from enclosing scopes.
                     ((member :import attributes)
                      (not
                       (path-later-p obj (aget :definition alist) target-ast)))
                     ((intersection '(:class :function) attributes) t)))
                 (mappend {get-vars obj} (remove nil (children scope))))))
             (find-local-bindings ()
               "Find local bindings in scope. Returns the py-name
                objects associated with the bindings."
               ;; NOTE: this doesn't correctly return bindings
               ;;       that occur based on control flow like with if-else
               ;;       statements. This typically isn't something that can
               ;;       be accounted for before runtime.
               (remove-duplicates
                (mappend
                 (lambda (ast)
                   (remove-if-not
                    (lambda (alist)
                      ;; Check for child scopes allows for
                      ;; namespace bindings in list comps and
                      ;; such.
                      (shares-path-of-p
                       obj target-ast (aget :scope alist)))
                    (get-vars obj ast)))
                 ;; Remove ASTs after.
                 (remove-if
                  (lambda (ast)
                    (path-later-p obj ast target-ast))
                  (get-asts-in-namespace obj enclosing-scope)))
                :test (lambda (alist1 alist2)
                        (equal (aget :name alist1)
                               (aget :name alist2)))
                :from-end t))
             (get-global-bindings ()
               "Get the global bindings in scope."
               (mappend (lambda (ast)
                          (mapcar #'find-global-binding
                                  (remove-if-not {typep _ 'python-identifier}
                                                 (python-children ast))))
                        (remove-if-not
                         {typep _ 'python-global-statement}
                         (get-asts-in-namespace obj enclosing-scope))))
             (get-nonlocal-bindings ()
               "Get the nonlocal bindings in scope."
               (mappend (lambda (ast)
                          (mapcar
                           {find-nonlocal-binding
                            _ (enclosing-scope obj enclosing-scope)}
                           (mapcar
                            #'source-text
                            (remove-if-not {typep _ 'python-identifier}
                                           (python-children ast)))))
                        (remove-if-not
                         {typep _ 'python-nonlocal-statement}
                         (get-asts-in-namespace obj enclosing-scope))))
             (get-enclosing-bindings
                 (scope &aux (enclosing-scope (enclosing-scope obj scope))
                          (enclosing-bindings (find-enclosing-bindings scope)))
               "Get the enclosing bindings available in scope."
               (if (eq scope enclosing-scope)
                   enclosing-bindings
                   (append enclosing-bindings
                           (get-enclosing-bindings enclosing-scope))))
             (get-local-bindings ()
               "Get the local bindings available in scope."
               ;; Remove bindings after
               (remove-if-not
                (lambda (binding-alist)
                  (path-later-p obj target-ast (aget :decl binding-alist)))
                ;; build-alist
                (mapcar #'build-alist* (find-local-bindings))))
             (group-by-scope (bindings)
               "Group BINDINGS by scope."
               (assort bindings :key (lambda (alist) (aget :scope alist))))
             (sort-top->down (scopes)
               "Sort SCOPES from the top-most to the bottom-most."
               (sort scopes
                     (lambda (ast1 ast2)
                       (path-later-p obj ast2 ast1))
                     :key (lambda (list)
                            (aget :scope (car list))))))
      (sort-top->down
       (group-by-scope
        (remove-duplicates
         (remove-if
          #'null
          ;; NOTE: order of the append matters here for get-except-binding and
          ;;       get-local-bindings.
          (append (get-global-bindings)
                  (get-nonlocal-bindings)
                  (get-enclosing-bindings enclosing-scope)
                  (get-local-bindings)))
         :test (lambda (alist1 alist2)
                 (equal (aget :name alist1) (aget :name alist2)))
         :from-end t)))))

  (defmethod get-unbound-vals ((obj python) (ast python-ast))
    "Return all variables used (but not defined) within AST.
* OBJ python software object containing AST
* AST ast to retrieve unbound variables within"
    (labels ((call-name-p (parent name)
               "Return T if NAME is a function or method call."
               (typecase parent
                 (python-call
                  (let ((func (python-function parent)))
                    (typecase func
                      ;; free function
                      (python-identifier (eq func name))
                      ;; method call
                      (python-attribute (eq (python-attribute func) name)))))
                 (python-attribute
                  (call-name-p (get-parent-ast obj parent) name))))
             (bound-name-p (parent)
               (typep parent
                      '(or
                        python-function-definition
                        python-class-definition)))
             (get-unbound-vals-helper (obj parent ast)
               (remove-duplicates
                (apply #'append
                       (when (and (typep ast 'python-identifier)
                                  (not (or (bound-name-p parent)
                                           (call-name-p parent ast))))
                         (list (cons :name (source-text ast))))
                       (mapcar {get-unbound-vals-helper obj ast}
                               (remove nil (children ast))))
                :test #'equal)))
      (get-unbound-vals-helper obj (get-parent-ast obj ast) ast)))

  (defmethod get-unbound-funs ((obj python) (ast python-ast)
                               &aux (children (remove nil (children ast))))
    "Return all functions used (but not defined) within AST.  The returned
value will be of the form (list FUNCTION-ATTRS) where FUNCTION-ATTRS is a
list of form (FUNCTION-NAME UNUSED UNUSED NUM-PARAMS).

* OBJ python software object containing AST
* AST ast to retrieve unbound functions within"
    (remove-duplicates
     (apply #'append
            (when-let ((callee (and (typep ast 'python-call)
                                    (python-function ast))))
              (cond ((typep callee 'python-identifier)
                     ;; Free function call
                     (list (list (source-text callee)
                                 nil nil
                                 (length (python-children
                                          (python-arguments ast))))))
                    ((typep callee 'python-attribute)
                     ;; Member Function call
                     ;;
                     (list (list (source-text (python-attribute callee))
                                 nil nil
                                 (length
                                  (python-children (python-arguments ast))))))
                    (t nil)))
            (mapcar {get-unbound-funs obj} children))
     :test #'equal))

  ;; TODO: move into parseable?
  (-> find-if-in-scopes (function list) list)
  (defun find-if-in-scopes (predicate scopes)
    (mapc
     (lambda (scope)
       (when-let ((return-value (find-if predicate scope)))
         (return-from find-if-in-scopes return-value)))
     scopes)
    nil)

  (defmethod get-function-from-function-call ((obj python) (ast python-ast))
    (match ast
      ((python-call
        (python-function identifier))
       (when-let ((function-alist
                   (find-if-in-scopes
                    (lambda (scope)
                      (and (equal (source-text identifier)
                                  (aget :name scope))
                           (typep (aget :decl scope)
                                  'python-function-definition)))
                    (scopes obj ast))))
         (aget :decl function-alist)))))

  (defmethod map-arguments-to-parameters
      ((obj python) (funcall python-ast)
       &aux (function (get-function-from-function-call obj funcall)))
    ;; This method assumes a well-formed function and function call.
    (unless function
      ;; Exit early if the function isn't found to prevent errors.
      (return-from map-arguments-to-parameters nil))
    (labels ((get-parameter-alist (parameters)
               "Construct an alist of all the parameters."
               (let ((positional-only
                       ;; This is a hack around a lack of PEP 570 support
                       ;; in tree-sitter-python.
                       (position-if [{equal "/,"} {source-text}] parameters))
                     (list-splat
                       (position-if {typep _ 'python-list-splat-pattern}
                                    parameters))
                     (dictionary-splat
                       (position-if {typep _ 'python-dictionary-splat-pattern}
                                    parameters)))
                 `((:positional ,@(when positional-only
                                    (subseq parameters 0 positional-only)))
                   (:regular ,@(subseq parameters
                                       (or (and positional-only
                                                (1+ positional-only))
                                           0)
                                       (or list-splat dictionary-splat)))
                   (:list-splat ,(when-let ((list-splat-ast
                                             (and list-splat
                                                  (nth list-splat parameters))))
                                   (unless (equal (source-text list-splat-ast)
                                                  "*")
                                     list-splat-ast)))
                   (:keyword ,@(when list-splat
                                 (subseq
                                  parameters (1+ list-splat) dictionary-splat)
                                 (subseq parameters (1+ list-splat))))
                   (:dictionary-splat ,(when dictionary-splat
                                         (nth dictionary-splat parameters))))))
             (same-name-p (arg parameter)
               "Return T if PARAMETER1 and PARAMETER2 represent the same id."
               (equal (source-text arg) (source-text parameter)))
             (get-identifier (ast)
               "Get the relevant identifier for AST if one exists."
               (typecase ast
                 ((or python-default-parameter python-keyword-argument)
                  (python-name ast))
                 ((or python-list-splat-pattern python-dictionary-splat-pattern)
                  (car (python-children ast)))
                 (t ast)))
             (get-default-parameters (parameters)
               "Get a mapping of default parameters to their defaults."
               ;; NOTE: collect all defaults and remove the duplicates later.
               (mapcar
                (lambda (default)
                  (cons (python-value default)
                        (python-name default)))
                (remove-if-not {typep _ 'python-default-parameter}
                               parameters)))
             (get-positional-args-to-parameters (parameters-alist args-list)
               "Get a mapping of positional arguments to their parameters."
               (let ((positionals  (append (aget :positional parameters-alist)
                                           (aget :regular parameters-alist)))
                     (list-splat (car (aget :list-splat parameters-alist)))
                     (positional-args
                       (remove-if {typep _ 'python-keyword-argument} args-list)))
                 (append (mapcar
                          (lambda (arg parameter)
                            (cons arg (get-identifier parameter)))
                          positional-args positionals)
                         (when list-splat
                           (list
                            (cons (create-tuple
                                   (drop (length positionals) positional-args))
                                  (get-identifier list-splat)))))))
             (create-keyword-dictionary (keyword-args)
               "Create the dictionary created for '**' style parameters."
               (create-dictionary
                (mapcar
                 (lambda (arg)
                   (convert
                    'python-ast
                    `((:class . :string)
                      (:interleaved-text
                       ,(format nil "\"~a\""
                                (source-text
                                 (python-name arg)))))))
                 keyword-args)
                (mapcar #'python-value keyword-args)))
             (get-keyword-args-to-parameters (parameters-alist args-list)
               "Get a mapping of keyword arguments to their parameters."
               (iter
                 (iter:with
                  dict-splat = (car (aget :dictionary-splat parameters-alist)))
                 (iter:with dict-splat-args)
                 (for arg in (remove-if-not {typep _ 'python-keyword-argument}
                                            args-list))
                 (if-let ((parameter
                           (find-if
                            [{same-name-p (get-identifier arg)}
                             #'get-identifier]
                            ;; potential keywords
                            (append (aget :regular parameters-alist)
                                    (aget :keyword parameters-alist)))))
                   (collect (cons (python-value arg) (get-identifier parameter))
                     into mapping)
                   (push arg dict-splat-args))
                 (finally
                  (return
                    (if dict-splat
                        (cons (cons (create-keyword-dictionary dict-splat-args)
                                    (get-identifier dict-splat))
                              mapping)
                        mapping))))))
      (let* ((parameters (python-children (python-parameters function)))
             (parameters-alist (get-parameter-alist parameters))
             (args-list (python-children (python-arguments funcall))))
        ;; NOTE: all default parameters are returned by get-default-parameters.
        ;;       The defaults that are actually used need to be removed here.
        (remove-duplicates
         ;; NOTE: append order matters.
         (append (get-default-parameters parameters)
                 (get-positional-args-to-parameters
                  parameters-alist args-list)
                 (get-keyword-args-to-parameters
                  parameters-alist args-list))
         :test #'same-name-p :key #'cdr))))

  (defmethod assign-to-var-p ((ast python-ast) (identifier python-ast))
    ;; Return the python-identifier that matches in case the caller wants
    ;; to check if it is the same as identifier.
    (match ast
      ((python-augmented-assignment :python-left lhs)
       (and (identical-name-p identifier lhs) lhs))
      ((python-assignment
        :python-left lhs)
       (typecase lhs
         (python-identifier (identical-name-p identifier lhs))
         (python-pattern-list
          (find-if {identical-name-p identifier} (python-children lhs)))))))

  ;; Indentation
  (defmethod indentablep ((ast python-string)) nil)

  (defmethod source-text ((ast python-ast)
                          &key stream parents
                            ;; These are "boxed" values since they have
                            ;; to be propagated between adjacent
                            ;; siblings (not just down the stack).
                            (indent-p (box nil))
                            (indentation-ast (box nil))
                          &aux (root ast))
    ;; TODO: add support for Windows-style CRLF instead of just newlines.
    (labels ((ends-with-newline-p (string)
               "Return T if STRING ends with a newline."
               (unless (emptyp string)
                 (eql #\newline (last-elt string))))
             (make-indentation-string (indentation)
               "Create the string representation of INDENTATION.
              This handles converting spaces to tabs."
               (if *indent-with-tabs-p*
                   (mvlet ((tabs spaces (floor indentation *spaces-per-tab*)))
                     (concatenate
                      'string
                      (repeat-sequence "	" tabs)
                      (repeat-sequence " " spaces)))
                   (repeat-sequence " " indentation)))
             (indentation-length (ast parent-list)
               "Get the indentation at AST with its parents provided
              in PARENT-LIST."
               ;; Patch the indent-children slots of AST if
               ;; the value is T. The value T could be provided
               ;; from a #'convert invocation.
               (cond
                 ((typep ast 'indentation)
                  (when (eq t (indent-children ast))
                    (setf (indent-children ast)
                          (get-default-indentation ast parent-list)))
                  (get-indentation-at ast parent-list))
                 (t (get-indentation-at (car parent-list) (cdr parent-list)))))
             (patch-inner-indentation (text ast parents
                                       &aux (indentation
                                             (indentation-length ast parents))
                                         (split-text (split "\\n" text)))
               "Patch the newlines that occur inside interleaved text.
              This assumes that the indentation should be the same
              as the parent."
               ;; NOTE: this will likely indent comments incorrectly.
               ;;       This behavior is expected.
               (cond
                 ((not (indentablep ast)) text)
                 ((< 1 (length split-text))
                  (with-output-to-string (s)
                    (write-string (car split-text) s)
                    (dolist (subseq (cdr split-text))
                      (format s "~%~a~a"
                              (make-indentation-string
                               (if (emptyp subseq)
                                   0
                                   indentation))
                              subseq))
                    ;; Add the newline back in if there was one at the end
                    ;; of the string since it gets removed by #'split.
                    (format s "~a"
                            (if (ends-with-newline-p text) #\newline ""))))
                 (t text)))
             (handle-leading-newline (text)
               "If the first character in TEXT is a newline, reset the
              indentation variables so no indentation is printed."
               (when (and (not (emptyp text))
                          (eql #\newline (first text)))
                 (setf (unbox indent-p) nil
                       (unbox indentation-ast) nil)))
             (handle-trailing-newline (text ast indentablep)
               "If the last character in TEXT is a newline, set the
              indentation variables."
               (when (and (ends-with-newline-p text)
                          indentablep)
                 (setf (unbox indent-p) t
                       (unbox indentation-ast) ast)))
             (handle-indentation (text ast indentablep parents
                                  &key ancestor-check)
               "If indentation to be written to stream, handle
              writing it."
               (when (and (unbox indent-p)
                          indentablep
                          ;; Prevent indentation from being
                          ;; wasted on empty strings before it
                          ;; reaches a child. This is checking if
                          ;; it should not be skipped as opposed
                          ;; to the logic in process-indentation
                          ;; which is checking if it should be
                          ;; skipped.
                          (not (and ancestor-check
                                    (emptyp text)
                                    (ancestor-of-p
                                     root (unbox indentation-ast) ast))))
                 (setf (unbox indent-p) nil
                       (unbox indentation-ast) nil)
                 (write-string
                  (make-indentation-string (indentation-length ast parents))
                  stream)))
             (handle-text (text ast indentablep parents
                           &key ancestor-check)
               "Handle writing TEXT to stream, updating any indentation
              variables that need updated."
               ;; Suppress indentation if TEXT begins with a newline.
               (handle-leading-newline text)
               (handle-indentation text ast indentablep parents
                                   :ancestor-check ancestor-check)
               ;; Set indentation flag  when TEXT ends with a newline.
               (handle-trailing-newline text ast indentablep)
               (write-string
                (patch-inner-indentation text ast parents)
                stream)))
      ;; TODO: the checking for whether there's a string could
      ;;       be resolved by reconstructing a list of children.
      (let* ((stringp (stringp ast))
             (interleaved-text (unless stringp (interleaved-text ast)))
             (indentablep (indentablep ast)))
        (handle-text
         (if stringp ast (car interleaved-text))
         ast indentablep parents)
        (mapc (lambda (child text)
                (source-text child
                             :stream stream
                             :parents (cons ast parents)
                             :indent-p indent-p
                             :indentation-ast indentation-ast)
                (handle-text text ast indentablep parents
                             :ancestor-check t))
              (unless stringp (sorted-children ast))
              (cdr interleaved-text)))))

  (-> process-indentation (python-ast) python-ast)
  (defun process-indentation (root &aux indentation-carryover indentation-ast)
    ;; TODO: add support for Windows-style CRLF instead of just newlines.
    (labels ((adjusted-spaces-from-tabs
                 (subseq &aux (tab-count (count #\tab subseq)))
               "Return the number of spaces that are used for tabs minus
                the number of tabs."
               (- (* tab-count *spaces-per-tab*)
                  tab-count))
             (starts-with-indentation-p (string)
               "If STRING starts with indentation, return
                the first position without indentation."
               (when indentation-carryover
                 (mvlet ((start end (scan "^[ \\t]*" string)))
                   (declare (ignorable start))
                   end)))
             (ends-with-newline-p (string)
               "If STRING ends with a newline and optionally indentation,
                return the position of the newline."
               (iter
                 (for i from (1- (length string)) downto 0)
                 (for character = (aref string i))
                 (when (eql character #\newline)
                   (return i))
                 (while (member character '(#\space #\tab)))))
             (update-indentation-slots
                 (ast parents indentation text
                  &aux (parent (car parents))
                    (total-indentation (+ indentation indentation-carryover))
                    (inherited-indentation
                     (get-indentation-at ast parents)))
               "Patch either AST or PARENT to have INDENTATION for the
                relevant line or lines."
               (symbol-macrolet ((indent-children (indent-children parent))
                                 (indent-adjustment (indent-adjustment ast)))
                 (cond
                   ;; Avoid wasting the newline on empty text before
                   ;; reaching a child.
                   ((and (emptyp text) (ancestor-of-p root indentation-ast ast)))
                   ((and parent (not indent-children))
                    (setf indent-children (- total-indentation
                                             inherited-indentation)
                          indentation-carryover nil
                          indentation-ast nil))
                   (t (setf indent-adjustment (- total-indentation
                                                 inherited-indentation)
                            indentation-carryover nil
                            indentation-ast nil)))))
             (patch-leading-indentation
                 (text ast parents
                  &aux (indentation (starts-with-indentation-p text))
                    (not-empty-string-p (not (emptyp text))))
               "Return TEXT with the leading indentation removed and
                the relevant indentation slot updated."
               (cond-let leading-indentation
                 ((and indentation
                       (= indentation (length text))
                       not-empty-string-p)
                  (setf indentation-carryover
                        (+ indentation-carryover
                           indentation
                           (adjusted-spaces-from-tabs
                            (subseq text 0 indentation))))
                  "")
                 ;; This prevents weird indentation caused by
                 ;; missing the indentation that occurs after the newline.
                 ((and not-empty-string-p
                       (eql #\newline (first text)))
                  (setf indentation-carryover nil
                        indentation-ast nil)
                  text)
                 ((or indentation
                      ;; NOTE: check if text exists here so that
                      ;;       the inherited indentation can be
                      ;;       set to 0. This prevents back-propogation
                      ;;       of indentation to previous siblings.
                      (and indentation-carryover
                           (scan "[^ \\t\\n]" text)))
                  (update-indentation-slots
                   ast parents (+ leading-indentation
                                  (adjusted-spaces-from-tabs
                                   (subseq text 0 leading-indentation)))
                   text)
                  (subseq text leading-indentation))
                 (t text)))
             (patch-trailing-indentation (text ast)
               "Return TEXT with the trailing indentation removed and
                indentation-carryover updated."
               (cond-let trailing-indentation
                 ((ends-with-newline-p text)
                  (setf indentation-carryover
                        (+ (- (length text) (1+ trailing-indentation))
                           (adjusted-spaces-from-tabs
                            (subseq text trailing-indentation)))
                        indentation-ast ast)
                  (subseq text 0 (1+ trailing-indentation)))
                 (t text)))
             (patch-internal-indentation (text)
               "Return TEXT where all newlines proceeded by indentation
                are replaced with a newline."
               (cl-ppcre:regex-replace-all "\\n[ \\t]+" text (format nil "~%")))
             (patch-text (text ast parents)
               "Patch TEXT such that it useable for inherited indentation.
                Updates AST and PARENTS slots if necessary."
               (patch-internal-indentation
                (patch-trailing-indentation
                 (patch-leading-indentation text ast parents)
                 ast)))
             (process-indentation*
                 (ast &optional parents
                  &aux (interleaved-text (interleaved-text ast)))
               "Process the text of AST such that its indentation
                is in the indentation slots."
               (setf (car interleaved-text)
                     (patch-text (car interleaved-text) ast parents))
               (mapl (lambda (child-list text-list &aux (child (car child-list)))
                       ;; this prevents patching literals that have
                       ;; multiple newlines.
                       (unless (not (indentablep child))
                         (process-indentation* child (cons ast parents)))
                       (setf (car text-list)
                             (patch-text (car text-list) ast parents)))
                     (sorted-children ast)
                     (cdr interleaved-text))))
      (process-indentation* root)
      root))

  (defmethod get-default-indentation ((ast python-ast) (parents list))
    ;; Search for the first AST with a colon in the interleaved-text and
    ;; uses its indent-children value. If none are found, call-next-method.
    (labels ((indented-obj-p (ast &aux (indent-children (indent-children ast)))
               "Return T if AST is an obj that should have indented children."
               (and
                indent-children
                (not (eql t indent-children))
                (typep ast '(or
                             ;; TODO: at some point, add more classes here.
                             python-class-definition python-function-definition
                             python-if-statement python-while-statement
                             python-for-statement)))))
      (if-let ((indented-obj (find-if #'indented-obj-p (or (lastcar parents)
                                                           ast))))
        (indent-children indented-obj)
        (call-next-method))))

  ;; Helper functions
  (-> collect-var-uses (python python-ast) (values list &optional))
  (defun collect-var-uses (obj ast)
    "Collect uses of AST in OBJ."
    ;;TODO: at some point, expand this to work inside classes.
    ;;      This may require significat modifications to acount
    ;;      for 'self' variables.
    (labels ((same-name-p (ast name)
               "Return T if AST represents an AST that contains the same
                name as NAME."
               (typecase ast
                 (python-identifier (equal (source-text ast) name))
                 ((or python-global-statement
                      python-nonlocal-statement)
                  (find-if {same-name-p _ name} (python-children ast)))))
             (find-name-in-scopes (name scopes)
               "Search SCOPES for a variable named NAME."
               (mappend
                (lambda (scope)
                  (find-if
                   (lambda (var-info)
                     (equal name (aget :name var-info)))
                   scope))
                scopes))
             (get-analysis-set (scope first-occurrence name)
               "Collect all relevant asts with NAME in SCOPE. BINDING-CLASS
                determines whether 'global' or 'nonlocal' should be used to
                determine if NAME is in-scope for assignments."
               ;; Currently, python-identifier and either
               ;; python-nonlocal-statement or python-global-statement are
               ;; relevant.
               (remove-if-not
                (lambda (ast)
                  (or (same-name-p ast name) (eq ast first-occurrence)))
                (collect-if
                 (lambda (ast)
                   (typep ast 'python-identifier))
                 scope)))
             (find-var-uses (assorted-by-scope binding-class)
               "Search assorted-by-scope for usages of variables
                with the same name. BINDING-CLASS specifies whether
                the variable is global or local and provides the
                name of the class used for binding it to a scope."
               (iter
                 (iter:with out-of-scope = nil)
                 (iter:with
                  local-var-p = (eq binding-class 'python-nonlocal-statement))
                 (for vars-in-scope in assorted-by-scope)
                 (for scope = (enclosing-scope obj (car vars-in-scope)))
                 ;; Prune any scope that occurs after the local binding
                 ;; has been squashed.
                 (when out-of-scope
                   (if (shares-path-of-p obj scope out-of-scope)
                       (next-iteration)
                       (setf out-of-scope nil)))
                 (cond
                   ((find-if
                     (lambda (ast)
                       (find-if-in-parents {typep _ 'python-parameters} obj ast))
                     vars-in-scope)
                    ;; All nested scopes are out-of-scope.
                    (and local-var-p (setf out-of-scope scope)))
                   ((find-if
                     (lambda (var)
                       (find-if-in-parents {typep _ binding-class} obj var))
                     vars-in-scope)
                    (collect vars-in-scope))
                   ((find-if
                     (lambda (var)
                       (find-if-in-parents
                        (lambda (parent)
                          (and (typep parent 'python-assignment)
                               (assign-to-var-p parent var)))
                        obj var))
                     vars-in-scope)
                    ;; All nested scopes are out-of-scope.
                    (and local-var-p (setf out-of-scope scope)))))))
      (let* ((name (and (typep ast 'python-identifier) (source-text ast)))
             (var-info (find-name-in-scopes name (scopes obj ast)))
             (scope (or (aget :scope var-info) (enclosing-scope obj ast)))
             ;; The path will be nil when given the global scope.
             (binding-class (if (ast-path obj scope)
                                'python-nonlocal-statement
                                'python-global-statement))
             (assorted-by-scope
               ;; Make sure the top-most scope comes first.
               (sort
                (assort
                 (get-analysis-set scope (or (aget :decl var-info)
                                             (get-parent-full-stmt obj ast))
                                   name)
                 :key {enclosing-scope obj})
                (lambda (ast1 ast2)
                  (< (length (ast-path obj (enclosing-scope obj ast1)))
                     (length (ast-path obj (enclosing-scope obj ast2)))))
                :key #'car)))
        ;; Don't pass in the first scope of assorted-by-scope as the first
        ;; one may include a parameter which find-var-uses would misinterpret
        ;; as squashing the binding's scope.
        (flatten (cons (car assorted-by-scope)
                       (find-var-uses (cdr assorted-by-scope)
                                      binding-class))))))

  (-> collect-fun-uses (python python-ast) list)
  (defun collect-fun-uses (obj ast)
    (labels ((same-name-p (ast name)
               "Return T if AST represents an AST that contains the same
                name as NAME."
               (equal (source-text ast) name))
             (get-analysis-set (scope name)
               "Collect all relevant asts with NAME in SCOPE."
               ;; Currently, py-name, py-arg, and either py-nonlocal or py-global
               ;; are relevant.
               (mapcar
                ;; Map the ASTs into parents that are easier
                ;; to work with.
                (lambda (ast)
                  (cond-let result
                    ((find-if-in-parents {typep _ 'python-parameters} obj ast)
                     result)
                    ((find-if-in-parents {typep _ 'python-function-definition}
                                         obj ast)
                     (if (eq (python-name result) ast)
                         result
                         ast))
                    (t ast)))
                (remove-if-not
                 (lambda (ast)
                   (same-name-p ast name))
                 (collect-if
                  (lambda (ast)
                    (member
                     (type-of ast)
                     `(python-identifier python-function-definition)))
                  scope))))
             (get-shadowed-asts (analysis-set shadowing-ast)
               "Get the ASTs in ANALYSIS-SET that are shadowed by SHADOWING-AST."
               (intersection
                analysis-set
                (remove-if
                 (lambda (ast)
                   (path-later-p obj shadowing-ast ast))
                 (get-asts-in-namespace
                  obj (enclosing-scope obj shadowing-ast)))))
             (shadowing-ast-p (ast)
               "Return T if AST is an AST that shadows the function."
               (etypecase ast
                 ((or python-parameters python-function-definition) t)
                 (python-identifier
                  ;; TODO: at some point,for loops and other binding forms
                  ;;       can also shadow, but support for identifying this
                  ;;       still needs to be done.
                  (find-if-in-parents
                   (lambda (parent)
                     (assign-to-var-p parent ast))
                   obj ast))))
             (remove-shadowed-asts (analysis-set)
               "Remove all ASTs that are shadowing the target function
                from the analysis set."
               ;; The initial definition is seen as a shadowing ast,
               ;; so remove it from consideration and add it back
               ;; after analysis.
               (cons
                (car analysis-set)
                (iter
                  (iter:with shadowed-asts)
                  (for ast in (cdr analysis-set))
                  (when (shadowing-ast-p ast)
                    (setf shadowed-asts
                          (append shadowed-asts
                                  (get-shadowed-asts analysis-set ast))))
                  (unless (member ast shadowed-asts)
                    (collect ast))))))
      (when (typep ast 'python-function-definition)
        (remove-shadowed-asts
         (get-analysis-set
          (enclosing-scope obj ast)
          (source-text (python-name ast)))))))

  (defgeneric get-vars (obj ast)
    (:documentation "Get the variables that are bound by AST.")
    (:method ((obj python) ast) nil))

  (defun create-var-alist (obj definition name &key attributes scope)
    "Create an alist with information about a variable."
    `((:name . ,name)
      (:definition . ,definition)
      (:scope . ,(or scope (enclosing-scope obj definition)))
      ,@(when attributes
          (list (cons :attributes attributes)))))

  (defun get-vars-name-handler (obj ast &key scope)
    "Handle AST as a py-name object."
    (create-var-alist
     obj ast (source-text ast)
     :scope scope
     :attributes '(:variable)))

  (defun get-vars-name-or-tuple-handler (obj ast &key scope)
    "Handle AST as a py-name or a py-tuple object."
    (typecase ast
      ((or python-tuple python-pattern-list)
       (mapcar
        (lambda (element)
          (create-var-alist
           obj element (source-text element)
           :scope scope
           :attributes '(:variable)))
        (python-children ast)))
      ((or python-identifier python-dotted-name)
       (list
        (get-vars-name-handler obj ast :scope scope)))))

  (defmethod get-vars ((obj python) (ast python-for-statement))
    (get-vars-name-or-tuple-handler obj (python-left ast)))

  ;; TODO: python-except-clause doesn't appear to have any slots.
  ;;       This may be an issue to open in tree-sitter-python.
  (defmethod get-vars ((obj python) (ast python-except-clause))
    ;; NOTE: try except appears to unbind the variable in the namespace.
    ;;       This may be because the exception has been freed by the time
    ;;       it is out of the except handler.
    ;;       This may require a special attribute for handling it.
    (let ((name-ast (cadr (python-children ast))))
      (when (typep name-ast 'python-identifier)
        (list
         (create-var-alist
          obj ast (source-text name-ast)
          :attributes '(:variable))))))

  (defun get-vars-comprehension-handler (obj ast)
    ;; NOTE: this is tricky since there are essentially two ASTs
    ;;       that the variable binding is available in. The chances
    ;;       of this becoming an issue are probably slim.
    ;;
    ;;       x = [1, 2, 3, 4, 5]
    ;;       x = [x for x in x]
    (mappend
     (lambda (for-in-clause)
       (mapcar
        (lambda (name-or-tuple)
          (get-vars-name-or-tuple-handler
           obj name-or-tuple :scope ast))
        (python-left for-in-clause)))
     (python-children ast)))

  (defmethod get-vars ((obj python) (ast python-list-comprehension))
    (get-vars-comprehension-handler obj ast))

  (defmethod get-vars ((obj python) (ast python-set-comprehension))
    (get-vars-comprehension-handler obj ast))

  (defmethod get-vars ((obj python) (ast python-dictionary-comprehension))
    (get-vars-comprehension-handler obj ast))

  (defmethod get-vars ((obj python) (ast python-generator-expression))
    (get-vars-comprehension-handler obj ast))

  (defun get-vars-import-handler (obj ast)
    "Handle AST as a python-import-statement or python-import-from-statement."
    (mapcar
     (lambda (name)
       (create-var-alist
        obj ast
        (typecase name
          ;; TODO: at some point,figure out how we want to handle dotted names.
          (python-dotted-name
           (source-text name))
          (python-aliased-import
           (source-text (python-alias name))))
        :attributes '(:import)))
     (python-name ast)))

  (defmethod get-vars ((obj python) (ast python-import-statement))
    (get-vars-import-handler obj ast))

  (defmethod get-vars ((obj python) (ast python-import-from-statement))
    (get-vars-import-handler obj ast))

  (defmethod get-vars ((obj python) (ast python-with-statement))
    (remove
     nil
     (mapcar
      (lambda (item)
        (when-let ((var (python-alias item)))
          (get-vars-name-handler obj var)))
      (python-children ast))))

  (defmethod get-vars ((obj python) (ast python-assignment)
                       &aux (lhs (python-left ast)))
    (typecase lhs
      (python-pattern-list
       (mapcar
        (lambda (item)
          (create-var-alist
           obj ast (source-text item)
           :attributes '(:variable)))
        (python-children lhs)))
      (python-identifier
       (list
        (create-var-alist obj ast (source-text lhs) :attributes '(:variable))))))

  (defmethod get-vars ((obj python) (ast python-function-definition))
    (append
     (list (create-var-alist obj ast (source-text (python-name ast))
                             :attributes '(:function)))
     (when-let ((parameters (python-parameters ast)))
       (mapcar
        (lambda (parameter)
          (create-var-alist
           obj parameters (source-text parameter)
           :scope ast
           :attributes '(:variable)))
        (python-children parameters)))))

  (defmethod get-vars ((obj python) (ast python-lambda))
    (mapcar
     (lambda (parameter)
       (create-var-alist
        obj parameter (source-text parameter)
        :scope ast
        :attributes '(:variable)))
     (when-let ((parameters (python-parameters ast)))
       (python-children parameters))))

  (defmethod get-vars ((obj python) (ast python-class-definition))
    (unless (in-class-def-p obj ast)
      (list
       (create-var-alist
        obj ast (source-text (python-name ast))
        :attributes '(:class)))))

  (-> in-class-def-p (python python-ast)
    (values (or null python-class-definition) &optional))
  (defun in-class-def-p (obj ast)
    "Return the class definition if AST is inside one."
    (find-if-in-parents {typep _ 'python-class-definition} obj ast))

  (-> identical-name-p (python-ast python-ast) boolean)
  (defun identical-name-p (name1 name2)
    "Return T if the IDs of NAME1 and NAME2 are the same."
    (and (typep name1 'python-identifier)
         (typep name2 'python-identifier)
         (equal (source-text name1) (source-text name2))))

  (-> get-asts-in-namespace (python python-ast) list)
  (defun get-asts-in-namespace (obj ast)
    "Get all of the ASTs in AST which are considered to be
in the same namespace."
    ;; Note that with the first call to this function, AST should be the start
    ;; of a namespace.
    (labels ((new-namespace-p (ast)
               "Return T if AST starts a new namespace."
               ;; TODO: probably need to add some more types here.
               (typep
                ast '(or python-function-definition python-class-definition)))
             (collect-asts (namespace)
               "Collect the asts in NAMESPACE."
               (let ((children (remove nil (children namespace))))
                 (append children
                         (mappend (lambda (child)
                                    (unless (new-namespace-p child)
                                      (collect-asts child)))
                                  children)))))
      (cons ast (sort (collect-asts ast)
                      (lambda (ast1 ast2)
                        (path-later-p obj ast2 ast1))))))

  (-> create-tuple (list) (values python-ast &optional))
  (defun create-tuple (values &aux (length (length values)))
    "Create a new tuple AST that contains values."
    (convert 'python-ast
             `((:class . :tuple)
               (:interleaved-text
                ,@(cond
                    ((= 0 length) '("()"))
                    ((= 1 length) '("(" ",)"))
                    (t (append '("(")
                               (repeat-sequence '(", ") (1- length))
                               '(")")))))
               (:children . ,values))))

  (-> create-dictionary (list list) (values (or python-ast null) &optional))
  (defun create-dictionary (keys values &aux (length (length keys)))
    "Create a new dictionary AST that maps KEYS to VALUES.
Returns nil if the length of KEYS is not the same as VALUES'."
    (when (= length (length values))
      (convert 'python-ast
               `((:class . :dictionary)
                 (:interleaved-text
                  ,@(cond
                      ((= 0 length) '("{}"))
                      (t (append '("{")
                                 (repeat-sequence '(", ") (1- length))
                                 '("}")))))
                 (:children
                  ,@(mapcar
                     (lambda (key value)
                       (convert
                        'python-ast
                        `((:class . :pair)
                          (:interleaved-text "" " : " "")
                          (:python-key . ,key)
                          (:python-value . ,value))))
                     keys values))))))

  ;; Implement the generic format-genome method for python objects.
  (defmethod format-genome ((obj python) &key)
    "Format the genome of OBJ using YAPF (Yet Another Python Formatter)."
    (yapf obj)))


;;;; Javascript
(when-class-defined (javascript)

  (defmethod ast-type-to-rebind-p ((ast javascript-identifier)) t)

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
                            javascript-function-declaration
                            javascript-program
                            javascript-arrow-function
                            javascript-for-statement
                            javascript-for-in-statement)))
                 (get-parent-asts* obj ast))
        (genome obj)))

  (defgeneric statements-in-scope (obj scope ast)
    (:documentation "Return all child statements of SCOPE prior to AST.")
    (:method (obj (scope javascript-ast) (ast javascript-ast))
      (iter (for c in (remove nil (children scope)))
        (while (path-later-p obj ast c))
        (collect c))))

  (defgeneric identifiers (ast)
    (:documentation "Return all js-identifier nodes in AST and its children.")
    (:method ((ast javascript-ast))
      (remove-if-not {typep _ '(or
                                javascript-identifier
                                javascript-property-identifier
                                javascript-shorthand-property-identifier)}
                     (cons ast (child-asts ast :recursive t)))))

  (defgeneric inner-declarations (ast)
    (:documentation
     "Return a list of variable declarations affecting inner scopes.")
    (:method ((ast javascript-ast)) nil)
    (:method ((ast javascript-function-declaration))
      (mappend #'identifiers (javascript-children (javascript-parameters ast))))
    (:method ((ast javascript-arrow-function))
      (if-let ((parameter (javascript-parameter ast)))
          (identifiers parameter)
          (mappend #'identifiers (javascript-children (javascript-parameters ast)))))
    (:method ((ast javascript-for-in-statement))
      (identifiers (javascript-left ast)))
    (:method ((ast javascript-for-statement))
      (identifiers (javascript-initializer ast))))

  (defgeneric outer-declarations (ast)
    (:documentation
     "Return a list of variable declarations affecting outer scopes.")
    (:method ((ast javascript-ast)) nil)
    (:method ((ast javascript-variable-declaration))
      (mappend #'outer-declarations (javascript-children ast)))
    (:method ((ast javascript-object-pattern))
      (mappend #'identifiers (javascript-children ast)))
    (:method ((ast javascript-array-pattern))
      (mappend #'identifiers (javascript-children ast)))
    (:method ((ast javascript-rest-parameter))
      (identifiers ast))
    (:method ((ast javascript-variable-declarator))
      (identifiers (javascript-name ast))))

  (defmethod scopes ((obj javascript) (ast javascript-ast))
    (labels ((get-parent-decl (obj identifier)
               "For the given IDENTIFIER AST, return the parent declaration."
               (car (remove-if-not {typep _ 'javascript-variable-declaration}
                                   (get-parent-asts obj identifier))))
             (ast-to-scope (obj scope ast)
               `((:name . ,(source-text ast))
                 (:decl . ,(or (get-parent-decl obj ast) ast))
                 (:scope . ,scope))))
      (unless (null (ast-path obj ast))
        (let ((scope (enclosing-scope obj ast)))
          (cons (reverse
                 (mapcar {ast-to-scope obj scope}
                         (append (inner-declarations scope)
                                 (mappend #'outer-declarations
                                          (statements-in-scope obj scope ast)))))
                (scopes obj scope))))))

  (defmethod get-unbound-vals ((obj javascript) (ast javascript-ast))
    "Return all variables used (but not defined) within AST.
* OBJ javascript software object containing AST
* AST ast to retrieve unbound variables within"
    (labels ((get-unbound-vals-helper (obj parent ast)
               (remove-duplicates
                (apply #'append
                       (when (and (typep ast 'javascript-identifier)
                                  (not
                                   (typep parent
                                          '(or
                                            javascript-call-expression
                                            javascript-member-expression
                                            javascript-function-declaration
                                            javascript-arrow-function
                                            javascript-class-declaration
                                            javascript-meta-property
                                            javascript-break-statement
                                            javascript-class-declaration
                                            javascript-continue-statement
                                            javascript-labeled-statement
                                            javascript-import-specifier
                                            javascript-export-statement
                                            javascript-variable-declarator))))
                         (list (cons :name (source-text ast))))
                       (mapcar {get-unbound-vals-helper obj ast}
                               (children ast)))
                :test #'equal)))
      (get-unbound-vals-helper obj (get-parent-ast obj ast) ast)))

  (defmethod get-unbound-funs ((obj javascript) (ast javascript-ast)
                               &aux (children (remove nil (children ast)))
                                 (callee (first children)))
    "Return all functions used (but not defined) within AST.
* OBJ javascript software object containing AST
* AST ast to retrieve unbound functions within"
    (remove-duplicates
     (apply #'append
            (when (typep ast 'javascript-call-expression)
              (cond ((typep callee 'javascript-identifier)
                     ;; Free function call
                     (list (list (source-text callee)
                                 nil nil (length (cdr children)))))
                    ((typep callee 'javascript-member-expression)
                     ;; Member function call
                     (list (list (nest (source-text)
                                       (second)
                                       (children callee))
                                 nil nil (length (cdr children)))))
                    (t nil)))
            (mapcar {get-unbound-funs obj} children))
     :test #'equal))

  (defmethod get-parent-full-stmt ((obj javascript) (ast javascript-ast))
    (find-if #'is-stmt-p (get-parent-asts obj ast)))

  (defmethod get-function-from-function-call
      ((obj javascript) (callexpr javascript-ast))
    ;; NOTE: this currently only handles
    ;;       named functions declared with 'function'.
    (match callexpr
      ;; TODO: when needed, add support for member expression
      ;;       function calls.
      ((javascript-call-expression
        :javascript-function
        (javascript-identifier
         :interleaved-text (list name)))
       (enclosing-find-function obj callexpr name))))

;;; Helper Functions.
  (-> enclosing-find-function (javascript javascript-ast string)
    (values (or null javascript-ast) &optional))
  (defun enclosing-find-function (obj start-ast function-name)
    "Find the function with the name FUNCTION-NAME in OBJ that is in
scope of START-AST."
    ;; NOTE: this currently only handles
    ;;       named functions declared with 'function'.
    (flet ((target-function (ast)
             (match ast
               ((javascript-function-declaration
                 :javascript-name
                 (javascript-identifier
                  :interleaved-text (list name)))
                (equal name function-name)))))
      (find-if-in-scope #'target-function obj start-ast)))

;;; Implement the generic format-genome method for Javascript objects.
  (defmethod format-genome ((obj javascript) &key)
    (prettier obj)))
