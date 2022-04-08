(defpackage :software-evolution-library/software/c-cpp-project
  (:nicknames :sel/software/c-cpp-project :sel/sw/c-cpp-project)
  (:use :gt/full
        :functional-trees/attrs
        :software-evolution-library
        :software-evolution-library/utility/include
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/c-cpp
        :software-evolution-library/software/c
        :software-evolution-library/software/cpp
        :software-evolution-library/software/parseable
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/project
        :software-evolution-library/software/compilable
        :software-evolution-library/software/directory)
  (:export :c/cpp-project
           :get-system-header
           :header-name
           :system-headers))

(in-package :software-evolution-library/software/c-cpp-project)
(in-readtable :curry-compose-reader-macros)

(defclass c/cpp-project
    (directory-project parseable-project compilable include-paths-mixin)
  ()
  (:documentation "Mixin for common project functionality between C and C++."))

(define-node-class c/cpp-root (functional-tree-ast)
  ((system-headers :accessor system-headers
                   :initarg :system-headers
                   :initform nil)
   (system-headers/string->ast :accessor system-headers/string->ast
                               :initform (make-hash-table :test #'equal))
   (project-directory :accessor project-directory
                      :initarg :project-directory
                      :initform nil)
   (child-slots :initform '((project-directory . 1) (system-headers . 0))
                :allocation :class))
  (:documentation "Node for c/cpp-project objects that allows for storing the
system-headers directly in the tree. Note that system headers are lazily added
by the symbol-table attribute."))

(define-node-class c/cpp-system-header (functional-tree-ast)
  ((header-name :initarg :header-name
                :accessor header-name)
   (children :initarg :children
             :accessor children
             :initform nil)
   (child-slots :initform '((children . 0))
                :allocation :class))
  (:documentation "Node for representing system headers."))

(defmethod print-object ((self c/cpp-system-header) stream)
  (print-unreadable-object (self stream :type t)
    (format stream "~a" (header-name self)))
  self)

(defgeneric get-system-header (project system-header-string)
  (:method (project system-header-string) nil)
  (:documentation "Get the system header indicated by SYSTEM-HEADER-STRING
and add it to PROJECT."))

#+(or :TREE-SITTER-C :TREE-SITTER-CPP)
(progn


;;; System Headers

(defun process-system-header
      (project path-string
       &aux (class-ast (format-symbol :sel/sw/ts "~a-AST"
                                      (component-class project))))
  (labels ((get-synopsis-string (path-string)
             "Get the synopsis string from the header represented
              by PATH-STRING."
             (handler-bind
                 ((#+sbcl sb-ext:file-does-not-exist #-sbcl file-error
                   (lambda (condition)
                     (declare (ignorable condition))
                     (return-from get-synopsis-string))))
               (extract-header-synopsis path-string)))
           (get-section (lines)
             "Returns as values the lines in the section that starts at the
              first items in LINES and the lines that are remaining."
             (assert (scan ".*:$" (first lines)))
             (iter
               (iter:with section)
               (iter:with lines = (drop 2 lines))
               (while (not (equal (car lines) "")))
               ;; NOTE: add semicolon to avoid parsing errors.
               ;;       The newline is to avoid putting the semicolon in
               ;;       comments.
               (push (format nil "~a~%;" (pop lines)) section)
               (finally (return (values section lines)))))
           (markup-section (comment lines)
             "Return as values a section retrieved from LINES that inserts
              comments"
             (mvlet ((section remaining-lines (get-section lines)))
               ;; NOTE: section is reversed here. The comments will be in the
               ;;       before-asts slot and can be used to further markup
               ;;       the AST.
               (values (append1 (intersperse comment section) comment)
                       remaining-lines)))
           (markup-synopsis (synopsis-string)
             "Mark up SYNOPSIS-STRING such that it can be parsed and transformed
              into something that can be referenced as an AST."
             (iter
               (iter:with markup-lines)
               (iter:with lines = (lines synopsis-string))
               (while lines)
               (for section-type =
                    (first (nth-value 1 (scan-to-strings "(Macros|Types):$"
                                                         (car lines)))))
               (if section-type
                   (mvlet* ((section-comment
                             (string-ecase section-type
                               ("Macros" "/*macro*/")
                               ("Types" "/*type*/")))
                            (section-lines
                             remaining-lines
                             (markup-section section-comment lines)))
                     (push section-lines markup-lines)
                     (setf lines remaining-lines))
                   (push (pop lines) markup-lines))
               (finally
                (return
                  (apply #'string+
                          (intersperse #.(format nil "~%")
                                       (reverse (flatten markup-lines))))))))
           (transform-into-forward-declaration (ast comment declaration-type)
             "Transforms AST into a forward declaration of type DECLARATION-TYPE
              if it has a preceding comment which is identical to COMMENT."
             (match ast
               ((c/cpp-expression-statement
                 (before-text before-text)
                 (after-text after-text)
                 (direct-children (list symbol-ast))
                 (before-asts
                  (list (c/cpp-comment
                         (text (equal comment))))))
                (convert class-ast `((:class . ,declaration-type)
                                     (:children ,symbol-ast)
                                     (:before-text . ,before-text)
                                     (:after-text . ,after-text))))
               ((c/cpp-primitive-type
                 (before-asts
                  (list (c/cpp-comment
                         (text (guard text (equal comment text))))))
                 (before-text before-text)
                 (after-text after-text))
                (convert class-ast `((:class . ,declaration-type)
                                     (:children
                                      ((:class . identifier)
                                       (:text . ,(text ast))))
                                     (:before-text . ,before-text)
                                     (:after-text . ,after-text))))))
           (transform-macro-ast (ast)
             (transform-into-forward-declaration
              ast "/*macro*/" 'macro-forward-declaration))
           (transform-type-ast (ast)
             (transform-into-forward-declaration
              ast "/*type*/" 'type-forward-declaration))
           (patch-system-header-ast (ast)
             "Patch all ASTs with a preceding comment to be forward
              declarations."
             ;; NOTE: primitive types are followed by an empty-statement. These
             ;;       need to be removed since the primitive types are being
             ;;       transformed and the empty-statements no long match
             ;;       correctly
             (remove-if (of-type 'c/cpp-empty-statement)
                        (mapcar
                         (op (or (transform-macro-ast _1)
                                 (transform-type-ast _1)
                                 _1))
                         ast))))
    (when-let* ((synopsis-string (get-synopsis-string path-string))
                (markup-synopsis (markup-synopsis synopsis-string))
                (root-ast (convert class-ast markup-synopsis)))
      (patch-system-header-ast root-ast))))

(defvar *system-header-cache* (dict)
  "Store system headers that have already been parsed.")

(defvar *system-header-symbol-table-cache* (dict)
  "Cache system header symbol tables.")

(defmethod get-system-header ((project c/cpp-project) (path-string string)
                              &aux (genome (genome project)))
  (symbol-macrolet ((header-hash (gethash
                                  path-string
                                  (system-headers/string->ast genome))))
    (labels ((populate-header-entry (project path-string)
               (ensure2 (gethash path-string *system-header-cache*)
                 (lret ((system-header
                         (make-instance
                             'c/cpp-system-header
                           :header-name path-string
                           :children
                           (ensure-list
                            (process-system-header project path-string)))))
                   (setf header-hash system-header)
                   (push system-header (system-headers genome))))))
      (or header-hash
          (populate-header-entry project path-string)))))

(defun trim-path-string (path-ast &aux (text (text path-ast)))
  "Return the text of PATH-AST with the quotes around it removed."
  (subseq text 1 (1- (length text))))

(defmethod from-file :around ((project c/cpp-project) file)
  (labels ((maybe-populate-header (ast)
             (match ast
               ((c/cpp-preproc-include
                 (c/cpp-path (and path (c/cpp-system-lib-string))))
                (get-system-header project (trim-path-string path))))))
    (let ((result (call-next-method)))
      (setf #1=(genome result) (make-instance 'c/cpp-root
                                              :project-directory #1#))
      (mapc #'maybe-populate-header project)
      result)))



;;; Symbol Table

(defmethod symbol-table-union ((root c/cpp-project) table-1 table-2 &key)
  (multi-map-symbol-table-union
   table-1 table-2
   :allow-multiple (multi-declaration-keys root)))

(defun find-symbol-table-from-include (project include-ast)
  (labels ((merge-cached-symbol-table (header)
             (let ((cached-table
                    (ensure2 (gethash header *system-header-symbol-table-cache*)
                      (with-attr-table header
                        (symbol-table header (empty-map))
                        (attrs-table *attrs*))))
                   (target-table (attrs-table *attrs*)))
               (do-hash-table (node alist cached-table)
                 (setf (gethash node target-table)
                       (if-let (old (gethash node target-table))
                         ;; Preserve any new attributes.
                         (append old alist)
                         alist)))))
           (process-system-header (project path-ast)
             (if-let ((system-header
                       (get-system-header
                        project (trim-path-string path-ast))))
               (progn
                 ;; (merge-cached-symbol-table system-header)
                 (symbol-table system-header (empty-map)))
               (empty-map)))
           (process-relative-header (path-ast)
             "Get the corresponding symbol table for the relative path
              represented by PATH-AST."
             (if-let ((software
                       (aget (trim-path-string path-ast)
                             (evolve-files project)
                             :test #'equal)))
               (symbol-table software (empty-map))
               (empty-map))))
    (ematch include-ast
      ((c/cpp-preproc-include
        (c/cpp-path (and path (c/cpp-string-literal))))
       (process-relative-header path))
      ((c/cpp-preproc-include
        (c/cpp-path (and path (c/cpp-system-lib-string))))
       (process-system-header project path)))))

(defmethod symbol-table ((node c/cpp-preproc-include) &optional in)
  (declare (ignore in))
  (let ((root (attrs-root *attrs*)))
    (if (typep root 'directory-project)
        (find-symbol-table-from-include root node)
        (call-next-method))))

(defmethod symbol-table ((node c/cpp-system-header) &optional in)
  (if-let ((root-ast (car (children node))))
    (symbol-table root-ast in)
    (empty-map)))

) ; #+(or :TREE-SITTER-C :TREE-SITTER-CPP)
