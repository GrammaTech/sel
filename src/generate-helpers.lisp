#|
*************************************************************************************
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* UNLIMITED RIGHTS
*
* The Government's rights to use, modify, reproduce, release, perform, display, or
* disclose this software are governed by DFARS 252.227-7013, RIGHTS IN TECHNICAL DATA
* --NONCOMMERCIAL ITEMS, and DFARS 252.227-7014 RIGHTS IN NONCOMMERCIAL SOFTWARE AND
* NONCOMMERCIAL COMPUTER SOFTWARE DOCUMENTATION.
*
*************************************************************************************
*
* All GrammaTech IP (sole or co-developed) needs to include the GrammaTech copyright.
*
* (c) 2016 GrammaTech, Inc.  All rights reserved.
*
* Such IP is also subject to the terms of the Prioprietary Information Agreement (PIA)
* executed between BAE Systems Information and Electronics Systems Integration Inc.
* and GrammaTech, Inc. dated April 21, 2015
*
*************************************************************************************
|#


#| Automatic helper function generation

When a repair requires moving or inserting multiple related
statements, and no single mutation improves fitness in isolation, a
standard genetic search has a low probability of succeeding. We can
alleviate this problem by automatically creating helper functions from
groups of related statements, and adding calls to those functions to
the fodder database.

This file implements a simple approach to helper function generation,
based on n-grams extracted from execution traces.
|#

(in-package :software-evolution)
(enable-curry-compose-reader-macros :include-utf8)


;;; Helper function generation
(defun generate-helpers (software input ngram-lengths insert-location
                         &key (min-frequency 3))
  "Instrument software and generate fodder from execution traces.
Returns a new software containing the generated helper functions,
corresponding CallExpr ASTs to use as fodder, and a list of rejected
snippets."
  (let* ((trace (with-temp-files (bin trace-file)
                  (let ((instrumented (instrument (copy software)
                                                  :trace-file trace-file
                                                  :instrument-exit t)))
                    (build instrumented bin)
                    (shell "~a ~a" bin input)
                    (with-open-file (in trace-file)
                      (loop for trace-point = (ignore-errors (read in))
                         while trace-point
                         collect trace-point)))))
         (ngrams (count-occurences
                  (apply #'append
                         (mapcar {ngrams-trace software trace 2}
                                 ngram-lengths))
                  min-frequency)))
    (multiple-value-bind (new-software fodder bad-ngrams)
        (add-helper-functions-from-ngrams software insert-location
                                          ngrams)
      (values new-software fodder bad-ngrams))))

(defun snippet-from-ngram (obj ngram)
  "Concatenate an ngram of ASTs into a code snippet."
  (flet
      ((merge-lists (&rest lists) (remove-duplicates
                                   (apply #'append lists) :test #'equal)))
    (let ((snippet
           ;; skip ASTs whose parents are already included, to avoid
           ;; duplicate text
           (loop for counter in (car ngram)
              for ast = (get-ast obj counter)
              unless (some {member _ (get-parent-asts obj ast)} snippet)
              collect ast into snippet
              finally (return snippet))))
      `((:ast-class . "CompoundStmt")
        (:full-stmt . t)
        (:macros . ,(apply #'merge-lists (mapcar {aget :macros} snippet)))
        (:unbound-funs . ,(apply #'merge-lists
                                 (mapcar {aget :unbound-funs} snippet)))
        (:unbound-vals . ,(apply #'merge-lists
                                 (mapcar {aget :unbound-vals} snippet)))
        (:src-text .
                   ,(format nil "{~%~{~a~^~%~}~%}~%"
                            (mapcar #'se::process-full-stmt-text snippet)))))))

(defun type->string (type)
  "The name of a type, as it will appear in C source code."
  (if (type-pointer type)
      (format nil "~a* const" (type-name type))
      (format nil "const ~a" (type-name type))))

(defun helper-function-from-ngram (obj ngram name)
  "Turn an n-gram snippet into a helper function.
Returns the text of the function, and a CallExpr AST for calling it."
  (let* ((body (->> (snippet-from-ngram obj ngram)
                    (snippet->ast)
                    (source-text)
                    (peel-bananas)))
         (decls (apply #'append
                       (mapcar [{aget :declares} {get-ast obj}] (car ngram))))
         ;; Arguments are unbound vars that aren't declared within the
         ;; snippet
         (args (remove-if [{member _ decls :test #'string=} #'car]
                    (remove-duplicates
                     (loop for counter in (car ngram)
                        appending
                          (loop for var in
                               (aget :unbound-vals (get-ast obj counter))
                             collecting
                               (cons (peel-bananas (car var))
                                     (type-of-scoped-var obj counter var))))
                     :key #'car :test #'string=))))
    (values
     ;; Function body
     (format nil "static void ~a(~{~a~^, ~})~%~a~%"
             name
             (mapcar {format nil "~a ~a"}
                     (mapcar [#'type->string #'cdr] args)
                     (mapcar #'car args))
             body)
     ;; Function call
     `((:ast-class . "CallExpr")
       (:full-stmt . t)
       (:unbound-funs (,(format nil "(|~a|)" name) t nil ,(length args)))
       (:unbound-vals . ,(mapcar [{list _ 0} {format nil "(|~a|)"} #'car] args))
       (:src-text . ,(format nil "(|~a|)(~{(|~a|)~^, ~})"
                             name (mapcar #'car args)))))))

(defun try-to-add-helper (software location text)
  "Add a helper function and compile the resulting software.
If successful, return the new software, otherwise return the original.
Also returns a boolean indicating success or failure."
  (let ((new (copy software)))
    (apply-mutation new
                    `(insert-fodder (:stmt1 . ,location)
                                    (:value1 . ((:src-text . ,text)))))
    (with-temp-file (bin)
      (bind (((:values _ exit _ _ _) (phenome new :bin bin)))
        (if (zerop exit)
            (values new t)
            (values software nil))))))

(defun add-helper-functions-from-ngrams (software location ngrams)
  "Turn each ngram into a helper function and add it to the software.
Functions which don't compile are discarded. Returns the modified
software, a list of CallExpr ASTs for the functions added, and a list
of the discarded ngrams.  Software must contain full genomes rather
than clang-restrict instances."
  (let (good-calls
        bad-ngrams)
   (loop
      for n in ngrams
      for i upfrom 0
      do
        (format t "helper~a~%" i)
        (multiple-value-bind (definition call)
            (helper-function-from-ngram software n (format nil "helper~a" i))
          (multiple-value-bind (new success)
              (try-to-add-helper software location definition)
            (setf software new)
            (if success
                (push call good-calls)
                (push n bad-ngrams)))))
   (values software good-calls bad-ngrams)))


;;; N-gram analysis
(defun ngrams (sequence n)
  "All length-N contiguous sub-sequences of SEQUENCE."
  (let (tails)
    (dotimes (i n)
      (push sequence tails)
      (pop sequence))
    (apply {mapcar #'list} (reverse tails))))

(defun ngrams-trace (obj trace min-stack-level n)
  "Generate n-grams from an execution trace.
Omit statements in the deepest MIN-STACK-LEVEL levels of the call
stack."
  ;; The raw trace will step into function calls, which doesn't
  ;; generate useful code snippets. We want to generate n-grams only
  ;; for statements that are at the same level in the call stack. This
  ;; is handled by disect-trace.
  (apply #'append
         (mapcar (lambda (fragment)
                   (ngrams (mapcar [{aget :counter} {get-ast obj} {aget :c}]
                                   (cdr fragment))
                           n))
                 (remove-if [{< _ min-stack-level} {aget :nesting}]
                            (disect-trace obj trace)))))

(defun disect-trace (obj trace)
  "Rearrange trace into fragments.
Fragments skip over calls and trace execution within a single
function."
  (let (fragments)
    (labels
        ((top () (aget :c (car trace)))
         (trace-function ()
           (let ((function (function-containing-ast obj (top)))
                 (nesting 0)
                 fragment)
             (loop while (not (eq (aget :c (car trace))
                                  (aget :body function)))
                do
                ;; If next entry is in a different function, trace
                ;; until it returns. Note: this won't handle direct
                ;; recursion.
                  (if (eq (function-containing-ast obj (top))
                          function)
                      (push (pop trace) fragment)
                      (setf nesting (max (+ 1 (trace-function))
                                         nesting))))
             (pop trace)                ; pop function exit
             (push `((:nesting . ,nesting) . ,(reverse fragment))
                   fragments)
             nesting)))
      (trace-function)
      (assert (not trace))
      fragments)))

(defun count-occurences (sequence min-count)
  "Return alist of count occurences over MIN-COUNT sorted by frequency."
  (let ((counts (make-hash-table :test #'equalp)))
    (loop for item in sequence
       do (if (gethash item counts)
              (incf (gethash item counts))
              (setf (gethash item counts) 1)))
    (sort (remove-if [{< _ min-count} #'cdr]
                     (loop for g being the hash-keys in counts
                        using (hash-value count)
                        collecting (cons g count)))
          #'> :key #'cdr)))
