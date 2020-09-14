;;;; indentation.lisp --- indentation that propagates to children
;;;
;;; The insertion and removal of ASTs in a software object often results
;;; in indentation that doesn't match the surrounding code. The indentation
;;; mix-in can be used to address these situations and help prevent
;;; undesirable indentation by maintaining the expected indentation when an AST
;;; is inserted into a new position. This is accomplished by storing the current
;;; line's indentation, and the amount of indentation that should be propagated
;;; to children and the difference between the indentation provided by all of
;;; the AST's parents. To simplify the implementation, all tabs are treated
;;; as spaces.
;;;
;;; @subheading Mix-ins
;;;
;;; The ability to reinsert tabs is provided through the software-indentation
;;; mix-in. The following slots are provided:
;;;
;;; @multitable {} {}
;;;
;;; @item spaces-per-tab @tab
;;; A number that indicates how many spaces each tab is worth. The initial form
;;; is 4.
;;;
;;; @item indent-with-tabs-p @tab
;;; A boolean value indicating whether tabs should be used over spaces. The
;;; initial form is nil.
;;;
;;; @end multitable
;;;
;;; The ast-indentation mix-in is used to add indentation information to an AST.
;;; It provided the following slots:
;;;
;;; @multitable {} {}
;;;
;;; @item indent-children @tab
;;; A number that indicates how many spaces should be added after newlines
;;; the precede a child of the AST. The value 'T can be provided,
;;; and the slot's value will be populated lazily with its parent's value or a
;;; sane default based on the rest of the file. The initial form is 0.
;;;
;;; @item indent-adjustment @tab
;;; A number that indicates the difference in indentation provided by an AST's
;;; parents and the spaces on its current line. This value is also added
;;; to the indentation given to children. The value 'MATCH-PARENT
;;; can be provided, and a value will instead be used that will match the
;;; indentation of the current line with that of its parent. The initial form is
;;; 0.
;;; @end multitable
;;;
;;; @subsubheading Parsing
;;; When using the indentation mix-in for software, the parser for the
;;; language needs to be modified to not insert whitespace after newlines,
;;; instead opting for storing this information in the indentation slots of an
;;; AST. This is accomplished by tracking the indentation that each AST provides
;;; to its children. When a newline is reached for a child, the current
;;; indentation provided by all of its parents is compared to the indentation
;;; provided for the child. If the indentation is different, the difference is
;;; used to to set either the parent's indent-children slot when it hasn't been
;;; changed from the default value or the indent-adjustment slot of the
;;; child otherwise.
;;;
;;; @subsubheading Converting to Text
;;; The source-text method also needs to be modified when using the indentation
;;; mix-in. While converting an AST to text, the interleaved text needs to be
;;; examined for newlines. When a newline is found in a string and nothing
;;; follows it, the next AST reached determines the amount of indentation
;;; needed. This will be the indentation provided by the indentation slots of
;;; all of its parents plus the indentation provided by its indent-adjustment
;;; slot. If a newline is found in a string and other text follows it,
;;; indentation is added that matches the current indentation of its parent.
;;;
;;; @texi{indentation}
(defpackage :software-evolution-library/software/indentation
  (:nicknames :sel/software/indentation :sel/sw/indentation)
  (:use :gt/full
        :software-evolution-library)
  (:export :indent-nothing))
(in-package :software-evolution-library/software/indentation)
(in-readtable :curry-compose-reader-macros)

(defvar indent-nothing nil
  "Currently the documentation requires an exported symbol to export a
  top comment.")
