(defpackage :software-evolution-library/python/lisp/tree-sitter-interface
  (:nicknames :sel/py/lisp/ts-int)
  (:use :gt/full
        :babel
        :cl-base64
        :cl-json
        :trivial-backtrace
        :usocket
        :software-evolution-library
        :software-evolution-library/command-line
        :software-evolution-library/software/template
        :software-evolution-library/software/parseable
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/c
        :software-evolution-library/software/cpp
        :software-evolution-library/software/python
        :software-evolution-library/software/java
        :software-evolution-library/software/javascript
        :software-evolution-library/software/rust
        :software-evolution-library/software/typescript
        :software-evolution-library/utility/range
        :software-evolution-library/python/lisp/utility)
  (:import-from :software-evolution-library :oid)
  #-windows (:import-from :osicat)
  (:import-from :deploy :define-library)
  (:export :run-tree-sitter-interface))
(in-package :software-evolution-library/python/lisp/tree-sitter-interface)
(in-readtable :curry-compose-reader-macros)

;;;; Command line interface:
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter +interface-command-line-options+
    '((("port") :type integer
       :documentation "listen for requests on the given port"))
    "tree-sitter-interface command line options."))

#-windows
(define-library osicat-posix::librt :dont-open t :dont-deploy t)

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
(defun safe-intern (string)
  (intern (string-upcase string) :sel/py/lisp/ts-int))

(-> alist-pair-p (t) boolean)
(defun alist-pair-p (pair)
  "Return T if PAIR represents a potential pair in an alist."
  (and (consp pair) (atom (car pair)) (not (null (cdr pair)))))

(-> function-string-to-symbol (string) symbol)
(defun function-string-to-symbol (function-str)
  "Convert the python FUNCTION-STR to a symbol for the associated CL function."
  (nest (safe-intern)
        (concatenate 'string "INT/")
        (string-upcase)
        (replace-all function-str "_" "-")))

;; (-> refcount (or ast integer) integer)
(defgeneric refcount (ast)
  (:documentation "Return the reference count of the ast, or 0 if not found.")
  (:method ((ast ast))
    (refcount (oid ast)))
  (:method ((oid integer))
    (or (cdr (gethash oid *external-asts*)) 0)))

;; (-> allocate-ast (ast) integer)
(defgeneric allocate-ast (ast)
  (:documentation "Allocate AST to the *external-asts* hashtable and
increment the reference counter to allow for its use externally without
garbage collection, returning the key used in the *external-asts* hashtable.")
  (:method ((ast ast) &aux (oid (oid ast)))
    (symbol-macrolet ((ast-refcount-pair (gethash oid *external-asts*)))
      (when (not ast-refcount-pair) (setf ast-refcount-pair (cons ast 0)))
      (incf (cdr ast-refcount-pair)))
    oid))

;; (-> deallocate-ast (or ast integer) boolean)
(defgeneric deallocate-ast (ast)
  (:documentation "Deallocate the AST from the *external-asts* hashtable
if its reference counter reaches zero, returning non-nil if a deallocation
was performed.")
  (:method ((ast ast))
    (deallocate-ast (oid ast)))
  (:method ((oid integer))
    (when (gethash oid *external-asts*)
      (let ((ref-count (decf (cdr (gethash oid *external-asts*)))))
        (when (zerop ref-count)
          (remhash oid *external-asts*))))))

;; (-> serialize (t) t)
(defgeneric serialize (it)
  (:documentation "Serialize IT to a form for use with the JSON text interface.")
  (:method :before ((it ast))
    (allocate-ast it))
  (:method ((it ast))
    `((:type . ,(cl-to-python-type (type-of it)))
      (:oid . ,(oid it))))
  (:method ((it string))
    `((:type . "b64string")
      (:value . ,(usb8-array-to-base64-string (string-to-octets it)))))
  (:method ((it list)) (mapcar-improper-list #'serialize it))
  (:method ((it t)) it))

;; (-> deserialize (t) t)
(defgeneric deserialize (it)
  (:documentation "Deserialize IT from a form used with the JSON text interface.")
  (:method ((it list))
    (match it
      ((alist (:type . "ast") (:oid . oid))
       (car (gethash oid *external-asts*)))
      ((alist (:type . "b64string") (:value . b64string))
       (octets-to-string (base64-string-to-usb8-array b64string)))
      ((guard it (every #'alist-pair-p it))
       (mapcar (lambda (pair)
                 (cons (deserialize (car pair)) (deserialize (cdr pair))))
               it))
      ((list* _) (mapcar #'deserialize it))))
  (:method ((it t)) it))

(-> handle-interface (list) t)
(defun handle-interface (json)
  "Handle a JSON input from the INTERFACE.  The JSON list should start with a
function name from the API followed by the arguments."
  (destructuring-bind (message-id function-str . arguments) json
    (let ((result
            (serialize
             (with-suppressed-output
               (apply (function-string-to-symbol function-str)
                      (mapcar #'deserialize arguments))))))
      (list (list (cons :messageid message-id)) result))))

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
  #.(fmt "~%Built from SEL ~a, and ~a ~a.~%"
         +software-evolution-library-version+
         (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose load eval language manual))
  (when help (show-help-for-tree-sitter-interface) (exit-command tree-sitter-interface 0))
  (if port
      (with-socket-listener (socket "localhost" port)
        (iter (for connection = (socket-accept socket))
              (unwind-protect
                  (let ((request (read-request connection)))
                    (until (equalp request "quit"))
                    (handle-request request connection))
                (socket-close connection))))
      (iter (for request = (read-request *standard-input*))
            (until (equalp request "quit"))
            (handle-request request *standard-output*))))

;;;; API:
(-> int/from-string (string string boolean boolean) (values ast &optional))
(defun int/from-string (source-text language deepest use-variation-point-tree)
  (let ((*use-variation-point-tree* use-variation-point-tree)
        (*use-blotting* nil))
    (convert (language-to-ast-symbol language)
             source-text
             :deepest deepest)))

(-> int/--del-- (ast) t)
(defun int/--del-- (ast)
  (deallocate-ast ast))

(-> int/--copy-- (ast) (values fixnum &optional))
(defun int/--copy-- (ast)
  "Shallow copy AST in a manner conforming to python's `copy.copy`."
  (allocate-ast ast))

(-> int/copy (ast &rest list) (values ast &optional))
(defun int/copy (ast &rest args)
  "Copy AST with optional keyword ARGS mapping child slots to new values."
  (labels ((handle-slot-name-argument (arg)
             "Translate the python slot-name argument (ARG) to a
              common lisp slot-name."
             (cons (python-to-cl-slot-name ast (car arg)) (cdr arg)))
           (handle-args (args)
             "Translate copy args from python to common lisp."
             (mappend [#'handle-keyword-argument #'handle-slot-name-argument]
                      args)))
    (apply #'copy ast (handle-args args))))

(-> int/gc (list) null)
(defun int/gc (oids) (mapcar #'deallocate-ast oids) nil)

(-> int/parent (ast ast) (values (or ast null) &optional))
(defun int/parent (root ast) (get-parent-ast root ast))

(-> int/children (ast) (values list &optional))
(defun int/children (ast)
  (remove-if-not (of-type 'ast) (children ast)))

(-> int/ast-path (ast ast) (values list &optional))
(defun int/ast-path (root ast)
  (cl-to-python-ast-path root (ast-path root ast)))

(-> int/lookup (ast list) (values (or ast null) &optional))
(defun int/lookup (root path)
  (lookup root (python-to-cl-ast-path root path)))

(-> int/source-text (ast) (values string &optional))
(defun int/source-text (ast) (source-text ast))

(-> int/child-slots (ast boolean) (values list &optional))
(defun int/child-slots (ast include-internal-p)
  (mapcar «list [{cl-to-python-slot-name ast} #'car] #'cdr»
          (if include-internal-p
              (child-slots ast)
              (remove-if #'internal-child-slot-p (child-slots ast)))))

(-> int/child-slot (ast string) (or list ast))
(defun int/child-slot (ast slot-name)
  (when-let ((slot (find-symbol (python-to-cl-slot-name ast slot-name)
                                :sel/sw/tree-sitter)))
    (slot-value ast slot)))

(-> int/size (ast) (values number &optional))
(defun int/size (ast)
  (size ast))

(-> int/ast-at-point (ast integer integer) (values (or ast null) &optional))
(defun int/ast-at-point (ast line column)
  (lastcar (asts-containing-source-location ast
                                            (make-instance 'source-location
                                              :line line :column column))))

(-> int/language (ast) (or string null))
(defun int/language (ast)
  (when-let ((lang (ast-language ast)))
    (cl-to-python-ast-language lang)))

(-> int/imports (ast ast) (values list &optional))
(defun int/imports (root ast)
  (imports root ast))

(-> int/function-name (ast) (values string &optional))
(defun int/function-name (ast) (function-name ast))

(-> int/function-parameters (ast) (values list &optional))
(defun int/function-parameters (ast) (function-parameters ast))

(-> int/function-body (ast) (values ast &optional))
(defun int/function-body (ast) (function-body ast))

(-> int/provided-by (ast ast) (values (or string null) &optional))
(defun int/provided-by (root ast) (provided-by root ast))

(-> int/call-function (ast) (values ast &optional))
(defun int/call-function (ast) (call-function ast))

(-> int/call-arguments (ast) (values list &optional))
(defun int/call-arguments (ast) (call-arguments ast))

(-> int/refcount (ast) (values fixnum &optional))
(defun int/refcount (ast)
  (refcount ast))

(-> int/get-vars-in-scope (ast ast boolean) (values list &optional))
(defun int/get-vars-in-scope (root ast keep-globals)
  (get-vars-in-scope (make-instance (safe-intern (ast-language ast))
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

(-> int/cut (ast ast) (values ast &optional))
(defun int/cut (root pt)
  (if-let ((path (ast-path root pt)))
    (less root (ast-path root pt))
    (error "~a is not in ~a" pt root)))

(-> int/insert (ast ast ast) (values ast &optional))
(defun int/insert (root pt ast)
  (if-let ((path (ast-path root pt)))
    (insert root (ast-path root pt) (tree-copy ast))
    (error "~a is not in ~a" pt root)))

(-> int/replace (ast ast ast) (values ast &optional))
(defun int/replace (root pt ast)
  (if-let ((path (ast-path root pt)))
    (with root (ast-path root pt) (tree-copy ast))
    (error "~a is not in ~a" pt root)))

(-> int/ast-template (string string &rest list) (values ast &optional))
(defun int/ast-template (template language &rest args)
  (apply #'ast-template
         template
         (language-to-ast-symbol language)
         (if (keyword-arguments-p args)
             (mappend #'handle-keyword-argument args)
             args)))

(-> int/asts-from-template (string string &rest list) (values list &optional))
(defun int/asts-from-template (template language &rest args)
  (handler-bind ((trivia.level2.impl::wildcard
                   (lambda (c)
                     (declare (ignorable c))
                     (invoke-restart 'continue))))
    (let ((class (language-to-ast-symbol language))
          (temps (make-gensym-list (length args))))
      (eval `(ematch (ast-template ,template ',class ,@args)
               ((ast-template ,template ,class ,@temps)
                (list ,@temps)))))))

;;;; API Helpers:
(-> language-to-ast-symbol (string) symbol)
(defun language-to-ast-symbol (language)
  "Convert the given language string to the associated AST type symbol."
  (find-symbol (concatenate 'string
                            (string-upcase (python-to-cl-ast-language language))
                            "-AST")
               :sel/sw/tree-sitter))

(-> keyword-arguments-p (list) (values boolean &optional))
(defun keyword-arguments-p (args)
  "Returns true if ARGS represents a list of keyword arguments (kwargs)."
  (every «and #'alist-pair-p [#'stringp #'first]» args))

(-> handle-keyword-argument (cons) cons)
(defun handle-keyword-argument (arg)
  "Translate the python keyword argument (ARG) to a Lisp keyword argument."
  (cons (make-keyword (string-upcase (replace-all (car arg) "_" "-")))
        (cdr arg)))

(-> cl-to-python-ast-path (ast list) (values list &optional))
(defun cl-to-python-ast-path (root path)
  "Translate the Lisp AST PATH from ROOT to a python representation."
  (labels ((helper (part)
             (etypecase part
               (number part)
               (symbol (cl-to-python-slot-name root part))
               (cons (list (helper (car part)) (helper (cdr part)))))))
    (mapcar #'helper path)))

(-> python-to-cl-ast-path (ast list) (values list &optional))
(defun python-to-cl-ast-path (root path)
  "Translate the python AST PATH from ROOT to a Lisp representation."
  (labels ((helper (part)
             (etypecase part
               (number part)
               (string part (or (find-symbol (python-to-cl-slot-name root part))
                                (find-symbol (python-to-cl-slot-name root part)
                                             :sel/sw/tree-sitter)))
               (list (cons (helper (car part)) (helper (lastcar part)))))))
    (mapcar #'helper path)))
