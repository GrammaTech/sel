;;;  Interface for all fodder database implementations.

(in-package :software-evolution)

(defclass fodder-database () ())

(defgeneric find-snippets (fodder-database
                           &key classes full-stmt n)
  (:documentation "Find snippets in the fodder database (optionally)
matching the keyword parameters CLASSES or FULL-STMT.

:CLASSES - AST Class(es) all snippets should match
:FULL-STMT - Limit results to full statements if non-nil.
:N <N> - Limit to N randomly drawn snippets"))

(defgeneric find-types (type-database &key hash)
  (:documentation "Find the types in the type database (optionally)
matching the keyword parameter HASH"))

(defgeneric find-types-kv (db kv)
  (:documentation
   "Find the types in the type database matching the predicate KV."))

(defgeneric find-snippets-kv (db kv &key)
  (:documentation
   "Find snippets in the Mongo database matching the predicate KV.
:N <N> - Limit to N randomly drawn snippets"))
