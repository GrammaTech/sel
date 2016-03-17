;;; Collection of utility functions for all database implementations.
(in-package :software-evolution)

(defun disasm-sorted-snippets-common (fodder target-disasm n)
  "Sort FODDER database snippets by disassembly similarity to TARGET-DISASM
and return the best N elements"
  (take n (sort (remove-if-not {aget :disasm} fodder)
                #'<
                :key [{diff-scalar _ (coerce
                                       (mappend
                                         (lambda (instr)
                                           (append (list (elf:opcode instr))
                                                   (elf:operands instr)))
                                         target-disasm)
                                       'simple-vector)}
                      {coerce _ 'simple-vector}
                      {mappend (lambda (line) line)}
                      {read-from-string}
                      {aget :disasm}])))

(defun byte-sorted-snippets-common (fodder target-bytes n)
  "Sort FODDER database snippets by byte-similarity to TARGET-BYTES
and return the best N elements"
  (take n (sort (remove-if-not {aget :binary--contents} fodder)
                #'<
                :key [{diff-scalar _ (coerce target-bytes 'simple-vector)}
                      {coerce _ 'simple-vector}
                      #'parse-binary-contents
                      {aget :binary--contents}])))

(defun parse-binary-contents (binary-contents-string)
  (map '(vector (unsigned-byte 8)) {parse-integer _ :radix 16}
       (split-sequence #\Space binary-contents-string)))
