;;; fodder-database.lisp --- Generic interface to database of software fodder
(defpackage :software-evolution-library/components/fodder-database
  (:nicknames :sel/components/fodder-database :sel/cp/fodder-database)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/components/searchable)
  (:export :fodder-database
           :database-emptyp
           :find-type
           :find-macro))
(in-package :software-evolution-library/components/fodder-database)
(in-readtable :curry-compose-reader-macros)

(defclass fodder-database (searchable)
  ()
  (:documentation "FIXME"))

(defgeneric database-emptyp (database)
  (:documentation "Return t if the database is empty, nil otherwise"))

(defgeneric find-type (database hash)
  (:documentation "Find the type in the database
matching the parameter HASH"))

(defgeneric find-macro (database hash)
  (:documentation "Find the macro in the database
matching the parameter HASH"))
