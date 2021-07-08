(defpackage :software-evolution-library/python/tree-sitter-interface
  (:nicknames :sel/py/ts-int)
  (:use :gt/full
        :cl-json
        :trivial-backtrace
        :usocket
        :software-evolution-library
        :software-evolution-library/command-line
        :software-evolution-library/software/parseable
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/c
        :software-evolution-library/software/cpp
        :software-evolution-library/software/python
        :software-evolution-library/software/javascript
        :software-evolution-library/utility/range)
  (:export :run-tree-sitter-interface))
(in-package :software-evolution-library/python/tree-sitter-interface)
(in-readtable :curry-compose-reader-macros)

;;;; Command line interface:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +interface-command-line-options+
    '((("stdio") :type boolean :optional t
       :documentation "communicate over standard input/output")
      (("port") :type integer :initial-value 4495
       :documentation "port to listen for requests on"))
    "tree-sitter-interface command line options."))

(defvar *external-asts* (make-hash-table)
  "Mapping of hashes to (AST . refcount) pairs for externally referenced ASTs.")

(defmacro with-muffled-warnings (&body body)
  "Execute BODY in an environment where warnings are muffled."
  `(handler-bind ((warning #'muffle-warning))
     (progn ,@body)))

(defmacro with-suppressed-output (&body body)
  "Execute BODY while suppressing output to standard output and standard error."
  `(with-muffled-warnings
     (let ((*standard-output* (make-string-output-stream))
           (*error-output* (make-string-output-stream)))
       ,@body)))

(defmacro with-error-logging (stream &body body)
  "Execute BODY in an environment where errors are caught and
reported back to the client in JSON form over STREAM."
  `(handler-case
       (progn ,@body)
     (condition (c)
       (format ,stream "~a~%"
               (nest (encode-json-to-string)
                     (list (cons :error
                                 (with-output-to-string (s)
                                   (print-condition c s)))))))))

(declaim (inline safe-intern))
(defun safe-intern (string) (intern (string-upcase string) :sel/py/ts-int))

;; (-> refcount (or ast integer) integer)
(defgeneric refcount (ast)
  (:documentation "Return the reference count of the ast, or 0 if not found.")
  (:method ((ast ast))
    (refcount (serial-number ast)))
  (:method ((handle integer))
    (or (cdr (gethash handle *external-asts*)) 0)))

;; (-> allocate-ast (ast) integer)
(defgeneric allocate-ast (ast)
  (:documentation "Allocate AST to the *external-asts* hashtable and
increment the reference counter to allow for its use externally without
garbage collection, returning the key used in the *external-asts* hashtable.")
  (:method ((ast ast) &aux (handle (serial-number ast)))
    (when (not (gethash handle *external-asts*))
      (setf (gethash handle *external-asts*) (cons ast 0)))
    (incf (cdr (gethash handle *external-asts*)))
    handle))

;; (-> deallocate-ast (or ast integer) boolean)
(defgeneric deallocate-ast (ast)
  (:documentation "Deallocate the AST from the *external-asts* hashtable
if its reference counter reaches zero, returning non-nil if a deallocation
was performed.")
  (:method ((ast ast))
    (deallocate-ast (serial-number ast)))
  (:method ((handle integer))
    (when (gethash handle *external-asts*)
      (let ((ref-count (decf (cdr (gethash handle *external-asts*)))))
        (when (zerop ref-count)
          (remhash handle *external-asts*))))))

;; (-> serialize (t) t)
(defgeneric serialize (it)
  (:documentation "Serialize IT to a form for use with the JSON text interface.")
  (:method :before ((it ast))
    (allocate-ast it))
  (:method ((it ast))
    `((:type . :ast) (:handle . ,(serial-number it))))
  (:method ((it list)) (mapcar-improper-list #'serialize it))
  (:method ((it t)) it))

;; (-> deserialize (t) t)
(defgeneric deserialize (it)
  (:documentation "Deserialize IT from a form used with the JSON text interface.")
  (:method ((it list))
    (if (and (every #'consp it) (aget :handle it))
        (car (gethash (aget :handle it) *external-asts*))
        (mapcar-improper-list #'deserialize it)))
  (:method ((it t)) it))

(-> handle-interface (list) t)
(defun handle-interface (json)
  "Handle a JSON input from the INTERFACE.  The JSON list should start with a
function name from the API followed by the arguments."
  (destructuring-bind (function-str . arguments) json
    (serialize
     (with-suppressed-output
      (apply (safe-intern (concatenate 'string "INT/" (string-upcase function-str)))
             (mapcar #'deserialize arguments))))))

(defgeneric read-request (input)
  (:documentation "Read a request from the given INPUT.")
  (:method ((stream stream))
    (read-line stream))
  (:method ((socket usocket))
    (read-request (socket-stream socket))))

(defgeneric handle-request (request output)
  (:documentation "Process the given REQUEST and write the response to OUTPUT.")
  (:method ((request string) (stream stream))
    (unwind-protect
        (with-error-logging stream
          (format stream "~a~%"
                  (nest (encode-json-to-string)
                        (handle-interface)
                        (decode-json-from-string request))))
      (finish-output stream)
      (unless (eq stream *standard-output*) (close stream))))
  (:method ((request string) (socket usocket))
    (handle-request request (socket-stream socket))))

(define-command tree-sitter-interface (&spec (append +common-command-line-options+
                                                     +interactive-command-line-options+
                                                     +interface-command-line-options+))
  "Tree-sitter command-line interface."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose load eval language manual))
  (when help (show-help-for-tree-sitter-interface) (exit-command tree-sitter-interface 0))
  (if stdio
      (iter (for request = (read-request *standard-input*))
            (until (equalp request "quit"))
            (handle-request request *standard-output*))
      (with-socket-listener (socket "localhost" port)
        (iter (for connection = (socket-accept socket))
              (unwind-protect
                  (let ((request (read-request connection)))
                    (until (equalp request "quit"))
                    (handle-request request connection))
                (socket-close connection))))))

;;;; API:
(-> int/ast (string string) (or ast null))
(defun int/ast (language source-text)
  (convert (safe-intern (concatenate 'string (string-upcase language) "-AST"))
           source-text))

(-> int/init (list) (or fixnum null))
(defun int/init (&rest args)
  (allocate-ast (apply #'int/ast args)))

(-> int/copy (ast) fixnum)
(defun int/copy (ast)
  (allocate-ast ast))

(-> int/del (ast) boolean)
(defun int/del (ast)
  (deallocate-ast ast))

(-> int/gc (list) null)
(defun int/gc (handles) (mapcar #'deallocate-ast handles) nil)

(-> int/ast-hash (ast) number)
(defun int/ast-hash (ast)
  (ast-hash ast))

(-> int/ast-equal (ast ast) boolean)
(defun int/ast-equal (ast1 ast2)
  (equal? ast1 ast2))

(-> int/parent (ast ast) ast)
(defun int/parent (root ast) (get-parent-ast root ast))

(-> int/children (ast) list)
(defun int/children (ast) (children ast))

(-> int/source-text (ast) string)
(defun int/source-text (ast) (source-text ast))

(-> int/child-slots (ast) list)
(defun int/child-slots (ast)
  (mapcar «list [#'symbol-name #'car] #'cdr» (child-slots ast)))

(-> int/child-slot (ast string) (or list ast))
(defun int/child-slot (ast slot-name) (slot-value ast (safe-intern slot-name)))

(-> int/ast-at-point (ast integer integer) ast)
(defun int/ast-at-point (ast line column)
  (lastcar (asts-containing-source-location ast
                                            (make-instance 'source-location
                                              :line line :column column))))

(-> int/ast-type (ast) string)
(defun int/ast-type (ast) (symbol-name (type-of ast)))

(-> int/ast-types (ast) list)
(defun int/ast-types (ast)
  (labels ((int/ast-types-helper (clazz)
             (unless (eq (class-name clazz) t)
               (cons (symbol-name (class-name clazz))
                     (mappend #'int/ast-types-helper
                              (class-direct-superclasses clazz))))))
    (remove-duplicates (int/ast-types-helper (find-class (type-of ast)))
                       :test #'equal)))

(-> int/ast-language (ast) string)
(defun int/ast-language (ast)
  (etypecase ast
    (sel/sw/ts::python-ast "PYTHON")
    (sel/sw/ts::javascript-ast "JAVASCRIPT")
    (sel/sw/ts::c-ast "C")
    (sel/sw/ts::cpp-ast "CPP")))

(-> int/imports (ast) list)
(defun int/imports (ast)
  (imports ast))

(-> int/function-asts (ast) list)
(defun int/function-asts (ast)
  (remove-if-not {typep _ 'function-ast} (convert 'list ast)))

(-> int/call-asts (ast) list)
(defun int/call-asts (ast)
  (remove-if-not {typep _ 'call-ast} (convert 'list ast)))

(-> int/function-name (ast) string)
(defun int/function-name (ast) (function-name ast))

(-> int/function-parameters (ast) list)
(defun int/function-parameters (ast) (function-parameters ast))

(-> int/function-body (ast) ast)
(defun int/function-body (ast) (function-body ast))

(-> int/provided-by (ast ast) string)
(defun int/provided-by (root ast) (provided-by root ast))

(-> int/call-function (ast) ast)
(defun int/call-function (ast) (call-function ast))

(-> int/call-arguments (ast) list)
(defun int/call-arguments (ast) (call-arguments ast))

(-> int/ast-refcount (ast) fixnum)
(defun int/ast-refcount (ast)
  (refcount ast))

(-> int/get-vars-in-scope (ast ast boolean) list)
(defun int/get-vars-in-scope (root ast keep-globals)
  (get-vars-in-scope (make-instance (safe-intern (int/ast-language ast))
                                    :genome root)
                     ast
                     keep-globals))

(-> int/ast-source-ranges (ast) list)
(defun int/ast-source-ranges (root)
  (iter (for (ast . range) in (ast-source-ranges root))
        (collect (list ast
                       (list (list (line (begin range))
                                   (column (begin range)))
                             (list (line (end range))
                                   (column (end range))))))))
