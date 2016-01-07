;;; clang-w-fodder-and-binary --- clang software with binary information
;;; and a source fodder database

(in-package :software-evolution)

;; Diamond inheritance ...
(defclass clang-w-fodder-and-binary (clang-w-fodder clang-w-binary) ())

(defmethod pick-json ((obj clang-w-fodder-and-binary)
                      &key full class pt &aux target-binary-contents-list)
  ;; If PT given, then with probability *targeted-mutation-chance*
  ;; select an AST from the fodder database with the greatest
  ;; byte-similarity to PT in OBJ.  Otherwise call the next method.
  (declare (ignorable full class))
  (if (and (random-bool :bias *targeted-mutation-chance*)
           ;; Find the diffs corresponding the AST at the given point
           ;; which contain code (bytes) to be targeted.
           pt (setf target-binary-contents-list
                    (remove-if-not {aget :original-code}
                                   (get-diffs-intersecting-ast
                                    obj
                                    (get-nearest-ast-w-bytes
                                     obj (get-ast obj pt))))))
      ;; Perform a targetted mutation
      (extremum
       *json-database-binary-fodder* #'<
       :key [{diff-scalar _ (coerce (aget :original-code
                                          (random-elt
                                           target-binary-contents-list))
                                    'simple-vector)}
             {coerce _ 'simple-vector}
             {parse-numbers _ :radix 16}
             {aget :binary--contents}])
      ;; Perform an un-targeted mutation.
      (list *targeted-mutation-chance*
            (get-diffs-intersecting-ast
             obj
             (get-nearest-ast-w-bytes
              obj (get-ast obj pt))))))
