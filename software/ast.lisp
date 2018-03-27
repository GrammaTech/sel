;;; ast.lisp --- Applicative ASTs structure

(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(defgeneric ast->snippet (ast)
  (:documentation "Convert AST to alist representation."))

(defstruct (ast-ref)
  "A reference to an AST at a particular location within the tree."
  (path nil :type list)
  (ast nil :type list))

(defmethod print-object ((obj ast-ref) stream)
  "Print a representation of the ast-ref OBJ to STREAM, including
the ast path and source text.
* OBJ ast-ref to print
* STREAM stream to print OBJ to
"
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (obj stream :type t)
        (format stream ":PATH ~s ~:_ :AST ~s ~:_ :TEXT ~s"
                (ast-ref-path obj) (car (ast-ref-ast obj))
                (source-text obj)))))

(defmacro define-ast (name options doc &rest fields)
  "Define an AST struct.

Form is similar to DEFSTRUCT, but each field can be described by a
single symbol, or a list containing a name and options.

Field options:
* KEY  override the key used for storing field in alists
* READER  call this function to transform values read from alists

This macro also creates AST->SNIPPET and SNIPPET->[NAME] methods.
"
  (labels ((splice (&rest symbols)
             "Splice symbols together."
             (intern (format nil "~{~a~}" symbols)))
           (field-name (field)
             "Raw name of a struct field (e.g. ast-name)"
             (if (listp field) (car field) field))
           (field-def (field)
             (if (listp field)
                 (list* (field-name field) ; name
                        nil                ; initform
                        (->> (cdr field)   ; options
                             (plist-drop :key)
                             (plist-drop :reader)))
                 field))
           (field-snippet-name (field)
             "Alist key for accessing a field within a snippet (e.g :name)."
             (or (and (listp field)
                      (plist-get :key field))
                 (make-keyword (field-name field))))
           (field-reader (field getter)
             "Code for transforming a field when building from an alist."
             (if-let ((reader (and (listp field)
                                   (plist-get :reader field))))
               `(funcall ,reader ,getter)
               getter))
           (field-accessor (field)
             "Name of the accessor function for a field (e.g. clang-ast-name)."
             (splice name "-" (field-name field)))
           (field-method (field)
             "Name of the accessor method for a field (e.g ast-name)."
             (splice (plist-get :conc-name options)
                     (field-name field))))
    `(progn
       ;; Struct definition
       (defstruct (,@(cons name (plist-drop :conc-name options)))
         ,doc
         ,@(mapcar #'field-def fields))

       (defmethod ast->snippet ((ast ,name))
         "Convert AST struct to alist."
         (list ,@(mapcar (lambda (f)
                           `(cons ,(field-snippet-name f)
                                  ,(list (field-accessor f) 'ast)))
                         fields)))

       (defun ,(splice 'snippet-> name) (snippet)
         "Convert alist to AST struct."
         ;; Read all fields from alist
         (,(splice 'make- name)
           ,@(iter (for f in fields)
                   (collect (make-keyword (field-name f)))
                   (collect (field-reader f
                                          `(aget ,(field-snippet-name f)
                                                 snippet))))))

       ;; Define getter and setter methods for all fields. These have
       ;; convenient names and (unlike the standard struct accessor
       ;; functions) can be overridden.
       ,@(iter (for f in fields)
               (collect `(defmethod ,(field-method f) ((obj ,name))
                           (,(field-accessor f) obj)))
               (collect `(defmethod (setf ,(field-method f)) (new (obj ,name))
                           (setf (,(field-accessor f) obj) new)))
               ;; Also define accessors on ast-refs
               (collect `(defmethod ,(field-method f) ((obj ast-ref))
                           (,(field-accessor f) (car (ast-ref-ast obj)))))
               (collect `(defmethod (setf ,(field-method f)) (new (obj ast-ref))
                           (setf (,(field-accessor f) (car (ast-ref-ast obj)))
                                 new)))))))

(defgeneric source-text (ast)
  (:documentation "Source code corresponding to an AST."))

(defmethod source-text ((ast ast-ref))
  "Return the source code corresponding to AST.
* AST ast-ref to retrieve source code for
"
  (source-text (ast-ref-ast ast)))

(defmethod source-text ((ast list))
  "Return the source code corresponding to AST.
* AST ast to retrieve source code for
"
  (format nil "~{~a~}"
          (iter (for c in (cdr ast))
                (collecting (if (stringp c)
                                c
                                (source-text c))))))

(defmethod source-text ((ast string))
  "Return the source code corresponding to AST.
* AST string to retrieve source code for
"
  ast)

(defun ast-later-p (ast-a ast-b)
  "Is AST-A later in the genome than AST-B?

Use this to sort AST asts for mutations that perform multiple
operations.
"
  (labels
      ((path-later-p (a b)
         (cond
           ;; Consider longer asts to be later, so in case of nested ASTs we
           ;; will sort inner one first. Mutating the outer AST could
           ;; invalidate the inner ast.
           ((null a) nil)
           ((null b) t)
           (t (bind (((head-a . tail-a) a)
                     ((head-b . tail-b) b))
                (cond
                  ((> head-a head-b) t)
                  ((> head-b head-a) nil)
                  (t (path-later-p tail-a tail-b))))))))
    (path-later-p (ast-ref-path ast-a) (ast-ref-path ast-b))))

