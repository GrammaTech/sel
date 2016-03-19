;;; Collection of utility functions for all database implementations.
(in-package :software-evolution)

(defun disasm-sorted-snippets-common (fodder target-disasm n
                                      &key (filter #'identity)
                                           (sort-predicate #'<)
                                           (similarity-fn #'diff-scalar))
  "Sort FODDER database snippets by disassembly similarity to TARGET-DISASM
and return the best N elements

FILTER - Function to remove snippets from consideration
SORT-PREDICATE - Function to compare two similarity scores to select
which is preferred.
SIMILARITY-FN - Function to compute a similarity score between two sequences"
  (let ((target-disasm-as-list (mapcar (lambda (instr)
                                         (append (list (elf:opcode instr))
                                                 (elf:operands instr)))
                                       target-disasm)))
    (take n (sort (remove-if-not [{(lambda (snippet) (funcall filter snippet))}
                                  {aget :disasm}] fodder)
                  sort-predicate
                  :key [{(lambda (candidate-disasm-as-list)
                           (funcall similarity-fn
                                      target-disasm-as-list
                                      candidate-disasm-as-list))}
                        {read-from-string}
                        {aget :disasm}]))))

(defun byte-sorted-snippets-common (fodder target-bytes n
                                    &key (filter #'identity)
                                         (sort-predicate #'<)
                                         (similarity-fn #'diff-scalar))
  "Sort FODDER database snippets by byte-similarity to TARGET-BYTES
and return the best N elements

FILTER - Function to remove snippets from consideration
SORT-PREDICATE - Function to compare two similarity scores to select
which is preferred.
SIMILARITY-FN - Function to compute a similarity score between two sequences"
  (take n (sort (remove-if-not [{(lambda (snippet) (funcall filter snippet))}
                                {aget :binary--contents}] fodder)
                sort-predicate
                :key [{(lambda (candidate-bytes)
                         (funcall similarity-fn target-bytes candidate-bytes))}
                      #'parse-binary-contents
                      {aget :binary--contents}])))

(defun parse-binary-contents (binary-contents-string)
  (mapcar {parse-integer _ :radix 16}
          (split-sequence #\Space binary-contents-string)))
