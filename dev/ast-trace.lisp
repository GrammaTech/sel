;;; Methods for tracing various CLANG AST functions
;;; The purpose of this is to figure out whether these
;;; functions are being exercised and, if so, what they
;;; are doing, as part of the clang front end migration.
;;;
;;; Notes about the functions will be placed here in
;;; commends before the trace methods, and eventually
;;; cleaned up and used to document the real code itself.

(in-package :sel/sw/clang)

(defmacro with-trace (obj sym &optional fn)
  `(let ((result (call-next-method)))
     (when (and result ,@(when fn `((,fn result))))
       (format *trace-output* "~%~A: ~a => ~a~%" ',sym ,obj result)
       (format *trace-output* "Children: ~s~%" (ast-children ,obj)))
     result))

;; Appears straightforward
(defmethod ast-void-ret :around ((x ast))
  (call-next-method)
  ; (with-trace x ast-void-ret)
  )

;; Not exercised in unit tests
(defmethod ast-varargs :around ((x ast))
  (with-trace x ast-varargs))

(defmethod ast-args :around ((x ast))
  (call-next-method)
  ;; (with-trace x ast-args)
  )

;; Appears straightforward
;; Returns a list of strings; generalize this to a list
;; of "declare objects" (which could be strings)
(defmethod ast-declares :around ((x ast))
  (call-next-method)
  ;; (with-trace x ast-declares)
  )

;; Not exercised in unit tests
(defmethod ast-expr-type :around ((x ast))
  ;; (with-trace x ast-expr-type)
  (call-next-method)
  )

;; appears straightforward
(defmethod ast-full-stmt :around ((x ast))
  ;; (with-trace x ast-full-stmt)
  (call-next-method))

;; This may require some thought
;; Whether a node is a guard stmt depends on context
(defmethod ast-guard-stmt :around ((x ast))
  ;; (with-trace x ast-guard-stmt)
  (call-next-method)
  )

;; This should be done by labeling id declarations
;; with the file they occur in (.h file).  We need
;; a way to get the path to that file.
(defmethod ast-includes :around ((x ast))
  ;; (with-trace x ast-includes)
  (call-next-method)
  )

;; not exercise in unit tests
(defmethod ast-in-macro-expansion ((x ast))
  ;; (with-trace x ast-in-macro-expansion)
  (call-next-method)
  )

;; Appears straightforward
(defmethod ast-is-decl :around ((x ast))
  ;; (with-trace x ast-is-decl)
  (call-next-method)
  )

;; not exercise in unit tests
(defmethod ast-macros :around ((x ast))
  ;; (with-trace x ast-macros)
  (call-next-method)
  )

;; Appears to just be called on FUNCTION nodes, returning
;; the string that is the function name.  Generalize to
;; objects representing functions
(defmethod ast-name :around ((x ast))
  ;; (with-trace x ast-name)
  (call-next-method))

;;; Maps various OPERATOR class nodes to the string that is
;;; the operator.  Appears straightforward
(defmethod ast-opcode :around ((x ast))
  ;; (with-trace x ast-opcode)
  (call-next-method))

;; Return type of a FUNCTION.  Straightfoward.
;; Will want different representations of types (currently just
;; big integer hash codes.)
(defmethod ast-ret :around ((x ast))
  ;; (with-trace x ast-ret)
  (call-next-method))

;; This will have to be inferred somehow, perhaps from
;; context.  One of several keywords :braced, :unbracedbody,
;; :generic, :fullstmt
(defmethod ast-syn-ctx :around ((x ast))
  ;; (with-trace x ast-syn-ctx)
  (call-next-method)
  )

;;; List of types of VAR, PARMVAR nodes
;;; SHould be inferred (with caching)
;;; Types are big hash codes; this needs to be generalized
;; Not sure why it can get > 1 type -- the initializer(s)?
(defmethod ast-types :around ((x ast))
  ;; (with-trace x ast-types (lambda (z) (> (length z) 1)))
  (call-next-method)
  )

;; Is returnin non-nil on certain declrefexprs.
;; I assume the associated function is undeclared.
;; Try to reproduce in new Clang?  This would be
;; a referencedDecl that is never resolved.
;; Returns a list of four-element lists; figure out the meaning
(defmethod ast-unbound-funs :around ((x ast))
  ;; (with-trace x ast-unbound-funs)
  (call-next-method))

;; Returned on a macroexpansion, which may be wrong
;; Returns a list of variable names (generalize this to variable
;; objects).  Could happen with unresolved referencedDecls
(defmethod ast-unbound-vals :around ((x ast))
  (with-trace x ast-unbound-vals)
  ;; (call-next-method)
  )


  

           





