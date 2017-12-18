;;;  Interface for all fodder database implementations.

(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(defclass fodder-database (searchable) ())

(defgeneric database-emptyp (database)
  (:documentation "Return t if the database is empty, nil otherwise"))

(defgeneric find-snippets (database &key ast-class full-stmt decls limit)
  (:documentation "Find snippets in the fodder database DATABASE.

:AST-CLASS - AST class all snippets should match
:FULL-STMT - Limit results to full statements if non-nil.
:DECLS ----- Include decls in result if non-nil.
             Limit results to decls if the keyword :ONLY.
:LIMIT ----- Limit to N randomly drawn snippets"))

(defgeneric find-type (database hash)
  (:documentation "Find the type in the database
matching the parameter HASH"))

(defgeneric find-macro (database hash)
  (:documentation "Find the macro in the database
matching the parameter HASH"))
