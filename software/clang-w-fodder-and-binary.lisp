;;; clang-w-fodder-and-binary --- clang software with binary information
;;; and a source fodder database

(in-package :software-evolution)

;; Diamond inheritance ... 
(defclass clang-w-fodder-and-binary (clang-w-fodder clang-w-binary) ())

(defmethod pick-full-stmt-json((obj clang-w-fodder-and-binary) pt 
                               &key (byte-similar-mutation-chance 0.75))
  "With probability *byte-similar-mutation-chance* select an AST from the 
   fodder database with the greatest byte-similary to target binary at the 
   point being mutated."

  ;; Find the diffs corresponding the AST at the given point which contain
  ;; code (bytes) to be targeted
  (let ((target-binary-contents-list 
          (remove-if-not {aget :original-code}
            (get-diffs-intersecting-ast 
               obj 
              (get-nearest-ast-w-bytes obj (get-ast obj pt))))))

    ;; Test if we could find diffs corresponding to the given AST.
    (if (and target-binary-contents-list
             (random-bool :bias byte-similar-mutation-chance))
        ;; Perform a targetted mutation
        (let* ((target-binary-content 
                 (aget :original-code (random-elt target-binary-contents-list)))
               (sorted-snippets
                 (sort 
                   *json-database-binary-fodder*
                   ;; Sort the fodder based on byte-similarity to the 
                   ;;target binary content
                   (lambda (candidate-ast1 candidate-ast2)
                       (< (diff-scalar (coerce (parse-numbers 
                                                 (aget :binary--contents 
                                                       candidate-ast1)
                                                 :radix 16)
                                               'simple-vector)
                                       (coerce target-binary-content
                                               'simple-vector))
                          (diff-scalar (coerce (parse-numbers 
                                                 (aget :binary--contents 
                                                       candidate-ast2)
                                                 :radix 16)
                                               'simple-vector)
                                       (coerce target-binary-content
                                               'simple-vector)))))))
           (prepare-code-snippet obj pt (car sorted-snippets)))
        ;; Do not perform a targetted mutation
        (call-next-method))))
