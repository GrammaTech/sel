#||
*******************************************************************************
*
* This software is furnished under a license and/or other restrictive terms and may
* be used and copied only in accordance with such terms and the inclusion of the
* below copyright notice. This software or any other copies thereof may not be
* provided or otherwise made available to any other person without the express
* written consent of an authorized reprsentative of GrammaTech, Inc. Title to,
* ownership of, and all rights in the software is retained by GrammaTech, Inc.
*
* Any reproduction of the software or portions thereof marked with this legend
* must also reproduce the following markings.
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* Distribution Statement B: Distribution authorized to U.S. Government agencies only;
* Export Controlled and Proprietary Information: DFARS SBIR Data Rights, 09/15/2017.
* Other requests for this document shall be referred to the DoD Controlling
* Office or the DoD SBIR Program Office.
*
* SBIR Data Rights
*
* Contract Number: D17PC00096
* Contractor Name: GrammaTech, Inc.
* Contractor Address: 531 Esty Street, Ithaca, NY  14850
* Expiration Date: April 30, 2024
*
* The Governmentís rights to use, modify, reproduce, release, perform, display, or disclose
* technical data or computer software contained in this report are restricted by paragraph
* (b)(4) of the Rights in Noncommercial Technical Data and Computer Software - Small Business
* Innovation Research (SBIR) Program clause (DFARS 252.227-7018 (FEB 2014)) contained in the
* above identified contract. No restrictions apply after the expiration date shown above. Any
* reproduction of technical data, computer software, or portions thereof marked with this
* legend must also reproduce the markings.
*
* (c) GrammaTech, Inc. 2018.  All other rights reserved.
*
*******************************************************************************
||#
;; Automatic helper function generation
;;
;; When a repair requires moving or inserting multiple related
;; statements, and no single mutation improves fitness in isolation, a
;; standard genetic search has a low probability of succeeding. We can
;; alleviate this problem by automatically creating helper functions from
;; groups of related statements, and adding calls to those functions to
;; the fodder database.
;;
;; This file implements a simple approach to helper function generation,
;; based on n-grams extracted from execution traces.
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)


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
                            (mapcar #'sel::process-full-stmt-text snippet)))))))

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
                                     (find-var-type obj var))))
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

#||
*******************************************************************************
*
* This software is furnished under a license and/or other restrictive terms and may
* be used and copied only in accordance with such terms and the inclusion of the
* below copyright notice. This software or any other copies thereof may not be
* provided or otherwise made available to any other person without the express
* written consent of an authorized reprsentative of GrammaTech, Inc. Title to,
* ownership of, and all rights in the software is retained by GrammaTech, Inc.
*
* Any reproduction of the software or portions thereof marked with this legend
* must also reproduce the following markings.
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* Distribution Statement B: Distribution authorized to U.S. Government agencies only;
* Export Controlled and Proprietary Information: DFARS SBIR Data Rights, 09/15/2017.
* Other requests for this document shall be referred to the DoD Controlling
* Office or the DoD SBIR Program Office.
*
* SBIR Data Rights
*
* Contract Number: D17PC00096
* Contractor Name: GrammaTech, Inc.
* Contractor Address: 531 Esty Street, Ithaca, NY  14850
* Expiration Date: April 30, 2024
*
* The Governmentís rights to use, modify, reproduce, release, perform, display, or disclose
* technical data or computer software contained in this report are restricted by paragraph
* (b)(4) of the Rights in Noncommercial Technical Data and Computer Software - Small Business
* Innovation Research (SBIR) Program clause (DFARS 252.227-7018 (FEB 2014)) contained in the
* above identified contract. No restrictions apply after the expiration date shown above. Any
* reproduction of technical data, computer software, or portions thereof marked with this
* legend must also reproduce the markings.
*
* (c) GrammaTech, Inc. 2018.  All other rights reserved.
*
*******************************************************************************
||#
