;;;  Interface for all fodder database implementations.

(in-package :software-evolution)

(defclass fodder-database () ())

(defgeneric find-snippets (fodder-database
                           &key classes full-stmt n)
  (:documentation "Find snippets in the fodder database (optionally)
matching the keyword parameters CLASSES or FULL-STMT.

:CLASSES - AST class(es) all snippets should match
:FULL-STMT - Limit results to full statements if non-nil.
:N <N> - Limit to N randomly drawn snippets"))

(defgeneric find-types (type-database &key hash)
  (:documentation "Find the types in the type database (optionally)
matching the keyword parameter HASH"))

(defgeneric byte-sorted-snippets (fodder-database
                                  target-bytes
                                  n-elems-to-return
                                  &key class k-elems-to-consider
                                       filter sort-predicate similarity-fn)
  (:documentation "Return the N-ELEMS-RETURN snippets closest to TARGET-BYTES
in FODDER-DATABASE.

:CLASS - AST class all snippets should match
:K-ELEMS-TO-CONSIDER - Limit search to K-ELEMS-TO-CONSIDER random snippets
:FILTER - Function to remove snippets from consideration
:SORT-PREDICATE - Function to compare two similarity scores to select
which is preferred.
:SIMILARITY-FN - Function to compute a similarity score between two sequences"))

(defgeneric disasm-sorted-snippets (fodder-database
                                    target-disasm
                                    n-elems-to-return
                                    &key class k-elems-to-consider
                                         filter sort-predicate similarity-fn)
  (:documentation "Return the N-ELEMS-RETURN snippets closest to TARGET-DISASM
in FODDER-DATABASE.

:CLASS - AST class all snippets should match
:K-ELEMS-TO-CONSIDER - Limit search to K-ELEMS-TO-CONSIDER random snippets
:FILTER - Function to remove snippets from consideration
:SORT-PREDICATE - Function to compare two similarity scores to select
which is preferred.
:SIMILARITY-FN - Function to compute a similarity score between two sequences"))
