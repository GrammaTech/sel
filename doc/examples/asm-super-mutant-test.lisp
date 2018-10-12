;;; asm-super-mutant-test.lisp --- Demonstrate and tests ASM-SUPER-MUTANT
;;;
;;; To test function zerostat, first make sure parameters below are
;;; set correctly for your environment.  Then execute:
;;;
;;;    (evolve-function-test "<func-name>" :max-evals 10000)
;;;    ;; e.g., (evolve-function-test "zeroset" :max-evals 10000)
;;;
;;; This binds *SOFT* to the ASM-SUPER-MUTANT master software object.
;;; It binds SEL:*POPULATION* to the population currently being
;;; evaluated.  It binds *TR* to the TASK-RUNNER instance returned by
;;; EVOLVE-FUNCTION-TEST.
;;;
;;; You can query the current status of the EVOLVE process by:
;;;
;;;     TR
;;;
;;; The symbol TR expends to (FIRST (TASK-RUNNER-RESULTS *TR*)), which
;;; is the most recent progress update of EVOLVE. It will update every
;;; 1000 fitness tests.
;;;
;;; At any time you can use (SHOW-RESULTS *TR*) to get a report, or
;;; (SHOW-RESULTS *TR* :VERBOSE T) to get a longer report with complete
;;; progress history.
;;;
;;; The variable SEL:*RUNNING* is true if EVOLVE is continuing. You
;;; can cancel the process at any time by:
;;;
;;;     (SETF *RUNNING* NIL)
;;;
;;; If you cancel, (SHOW-RESULTS *TR*) will give a report of progress
;;; up until it was canceled.
;;;
;;; When the EVOLVE task completes successfully, it will generate a
;;; report in <*io-dir*>/<func-name>-report.txt. Subsequent EVOLVE
;;; runs of the same function will overwrite the report file.
;;;
;;; TODO: Turn this into a nightly test run from sel/test/bin/.  We
;;;       should commit the minimum required from grep.asm, the
;;;       zerostat I/O pairs, the address file, and the function
;;;       bounds file.
(in-package :software-evolution-library/test)


;;; Variables used in the below example usage and tests.
(defparameter asm-super-base-dir
  (pathname (concatenate 'string
                         (trim-whitespace (shell "pwd")) "/")))

(defparameter *asm-path*
  (merge-pathnames "grep-testing/grep.asm" asm-super-base-dir)
  "Path to master asm file i.e. grep")

(defparameter *io-dir*
  (merge-pathnames "grep-testing/io5/" asm-super-base-dir)
  "Directory containing i/o files for fitness testing")

(defparameter *var-address-file*
  (merge-pathnames "grep-testing/grep.null-sanity.nm" asm-super-base-dir)
  "Path to sanity file containing data addresses")

(defparameter *fitness-file*
  (merge-pathnames "software/asm-super-mutant-fitness.c"
                   (make-pathname :directory +software-evolution-library-dir+))
  "Path to directory containing fitness harness")

(defparameter *function-bounds-file*
  (merge-pathnames "grep-testing/grep.null.fn-bnds.txt" asm-super-base-dir)
  "Path to file containing functional bounds by address")

(defparameter *tr*
  nil
  "Stores the most recent task-runner instance")

(defparameter *best* nil "Store the best variant (lowest fitness score)")
(defparameter *master* nil "Store the master ASM-SUPER-MUTANT software")

(defparameter *soft-exp*
  nil
  "ASM-SUPER-MUTANT instance with alternate (expanded) tests applied")


;;; Utility functions for evaluating/evolving asm-super software objects.
(defun exclude-func (asm-super name)
  "Sets named function to non-leaf false (which excludes it)"
  (let ((fstat-func (find name (function-index asm-super)
			  :test 'equal
			  :key 'sel::function-index-entry-name)))
    (if fstat-func
	(setf (sel::function-index-entry-is-leaf fstat-func) nil))))

(defun test-all-leaf-functions (asm-path
				io-dir
				var-address-file
				fitness-file
				function-bounds-file)
  "Run TEST-FUNCTION on all leaf functions in the asm file (functions which
don't make any calls) and skip those functions for which no i/o data file 
is found."
  (let* ((asm-super
	  (from-file
	   (make-instance
	    'asm-super-mutant
	    :function-bounds-file function-bounds-file)
	   asm-path))
	 (sel::*fitness-predicate* #'<)    ; lower fitness number is better
	 (sel::*worst-fitness* (sel::worst-numeric-fitness)))
    (exclude-func asm-super "fstat")  ;; hack, workaround for now
    (iter (for x in (leaf-functions asm-super))
	  (if (probe-file
	       (merge-pathnames
		(pathname io-dir)
		(make-pathname :name x)))
	      (multiple-value-bind (info best orig)
		  (asm-super-mutant-test-function
		   asm-path
		   x
		   :mutants-generator 'sel::create-all-simple-cut-variants
		   :io-dir io-dir
		   :var-address-file var-address-file
		   :fitness-file fitness-file
		   :function-bounds-file function-bounds-file)
		(declare (ignore info))
		(format t "name: ~A, best: ~D, orig: ~D, change: ~5f%~%"
		        x
			best
			orig
			(if (or (= best sel::*worst-fitness*)
				(= orig sel::*worst-fitness*))
			    0
			    (* 100d0 (- (float (/ best orig)) 1d0)))))))))

(defun setup-test-function (asm-path func-name
			    &key
                              (mutants-generator
                               (lambda (asm-super)
                                 ;; Creates 1024 copies of the original.
                                 (iter (for _ from 0
					    to (or *max-population-size* 1024))
                                       (collect (create-target asm-super)))))
                              io-dir
			      var-address-file
			      fitness-file
			      function-bounds-file)
  "Create asm-super-mutant from .asm file at asm-path.
Load i/o file from <io/dir>/func-name.  Run fitness tests on specified
function, and return optimal variant and fitness result.  Returns the
created softare object."
  (let ((asm-super
	 (from-file
	  (make-instance 'asm-super-mutant
			 :io-dir io-dir
			 :function-bounds-file function-bounds-file
			 :fitness-harness fitness-file
			 :var-table (sel::parse-sanity-file var-address-file))
	  asm-path)))

    ;; Select the function we want to work on.
    (target-function-name asm-super func-name)

    ;; Add variants (by default *max-population-size* copies of the original).
    (setf (mutants asm-super)
          (coerce (funcall mutants-generator asm-super) 'list))

    asm-super))

(defun asm-super-mutant-test-function
    (asm-path func-name
     &key
       (mutants-generator nil mutants-generator-p)
       io-dir
       var-address-file
       fitness-file
       function-bounds-file)
  "Create asm-super-mutant from .asm file at asm-path.
Load i/o file from <io/dir>/func-name. Run fitness tests on specified
function, and return optimal variant and fitness result.
MUTANTS-GENERATOR should be a function which takes a fully set up
ASM-SUPER-MUTANT instance, and returns a sequence of ASM-HEAP
instances representing the MUTANTS (variants) of the target function."
  (let ((asm-super (apply #'setup-test-function
                          asm-path
		          func-name
		          :io-dir io-dir
		          :var-address-file var-address-file
		          :fitness-file fitness-file
		          :function-bounds-file function-bounds-file
                          (when mutants-generator-p
                            (list :mutants-generator mutants-generator)))))
    (evaluate nil asm-super)

    ;; Fitness is per variant/per test case--use lexicase to pick the
    ;; best variants.
    (let ((best (lexicase-select-best (mutants asm-super)))) ; Lower number is better.
      (values
       (first best)
       (reduce '+ (fitness (first best)))
       ;; Total of all tests from first best variant.
       (reduce '+ (fitness (elt (mutants asm-super) 0)))))))

(defun evolve-tests (asm-path func-name
		     &key (mutants-generator nil mutants-generator-p)
		       io-dir var-address-file fitness-file
		       function-bounds-file max-evals period period-fn)
  (let ((asm-super
	 (apply #'setup-test-function
		asm-path func-name
		:io-dir io-dir
		:var-address-file var-address-file
		:fitness-file fitness-file
		:function-bounds-file function-bounds-file
		(when mutants-generator-p
                            (list :mutants-generator mutants-generator)))))
    (setf *master* asm-super)  ;; copy to special variable for development

    (evaluate nil *soft*)
    (setf *population* (mutants *soft*))
    (setf *tournament-selector* #'lexicase-select-best)
    (setf *fitness-predicate* #'<)
    (setf *max-population-size* 1024)
    (setf *print-length* 10)
    (setf *fitness-evals* 0)
    (setf *cross-chance* 2/3)
    (setf *mut-rate* 2/3)
    (setf *simple-mutation-types* '((SIMPLE-CUT . 1/3) (SIMPLE-SWAP . 1)))

    (sel/utility::print-time)
    (force-output)

    (let ((*running* nil)
	  (*fitness-evals* 0))
    ;; if the size of a variant's genome is < 2 we should filter it out
      (evolve nil :super-mutant-count 400
	    :max-evals max-evals
	    :filter
	    (lambda (v)
	      (> (size v) 5))
	    :period period
	    :period-fn period-fn))))

(defun evolve-function-test (func-name
			     &key
			       (max-evals 1000)
			       (report-path
				(merge-pathnames
				 *io-dir*
				 (make-pathname
				  :name
				  (concatenate 'string func-name "-report")
				  :type "txt"))))
  (setf *tr*
	(sel/utility::run-as-task (task1 runner1)
	  (task-save-result runner1 "Started fitness tests")
	  (let ((start-time (get-internal-real-time)))
	    (evolve-tests
	     *asm-path*
	     func-name
	     :io-dir *io-dir*
	     :var-address-file *var-address-file*
	     :fitness-file *fitness-file*
	     :function-bounds-file *function-bounds-file*
	     :max-evals max-evals
	     :period 1000
	     :period-fn
	     (lambda ()
	       (task-save-result
		runner1
		(flet ((sum (x) (reduce #'+ x)))
                  (format
                   nil "~{~{~a~^:~}~^ ~}~%"
                   `((:evals ,*fitness-evals*)
                     (:min-size
                      ,(reduce 'min (mapcar 'size *population*)))
                     (:max-size
                      ,(reduce 'max (mapcar 'size *population*)))
                     (:min-fit
                      ,(reduce 'min (mapcar [#'sum #'fitness] *population*)))
                     (:max-fit
                      ,(reduce 'max (mapcar [#'sum #'fitness] *population*)))
                     (:orig-fit
                      ,(sum (fitness (first (mutants *master*)))))))))))
	    (task-save-result
	     runner1
	     (format nil "Elapsed time: ~G seconds"
		     (/ (float (- (get-internal-real-time) start-time))
			internal-time-units-per-second))))
	  (if report-path
	      (with-output-file (of report-path :if-exists :supersede)
		(show-results runner1 :stream of :verbose t))))))

(defun evaluate-expanded-tests (func-name alt-io-dir)
  "Run the best variant against an alternate set of tests"
  (let* ((best-variant (extremum *population* *fitness-predicate*
                                 :key [{reduce #'+} #'fitness]))
	 (*soft*
	  (setup-test-function
	   *asm-path*
	   func-name
	   :mutants-generator
	   (lambda (asm)
	     (let ((v (copy best-variant)))
	       (setf (sel::super-owner v) asm)
	       (list v)))
	   :io-dir alt-io-dir
	   :var-address-file *var-address-file*
	   :fitness-file *fitness-file*
	   :function-bounds-file *function-bounds-file*)))
    (evaluate nil *soft*)
    (setf *soft-exp* *soft*)
    (first (fitness *soft*))))

(defun show-results (task-runner
		     &key
		       (population *population*)
		       (super *soft*)
		       (verbose nil)
		       (stream *standard-output*))
  (format stream "Fitness evaluations completed for function ~A: ~A~%~%"
	  (sel::function-name-from-label
	   (asm-line-info-label (elt (genome (first (mutants super))) 0)))
	  *fitness-evals*)
  (let* ((min-size (reduce 'min (mapcar 'size population)))
	 (max-size (reduce 'max (mapcar 'size population)))
	 (best-variant (extremum *population* *fitness-predicate*
                                 :key [{reduce #'+} #'fitness]))
	 (orig (first (mutants super)))
	 (best-fitness (reduce '+ (fitness best-variant)))
	 (orig-fitness (reduce '+ (fitness orig))))
    (format stream "min: ~D, max: ~D, best-fitness: ~A, size-of-best: ~A~%"
	    min-size max-size best-fitness (size best-variant))
    (format stream "orig fitness: ~A, orig size: ~A~%~%"
	    orig-fitness (size orig))

    (format stream "~%Size reduction: ~6,2F%, fitness improvement: ~6,2F%~%"
	    (* 100 (- 1.0 (/ (size best-variant) (size orig))))
	    (* 100 (- 1.0 (/ best-fitness orig-fitness))))

    (format stream "~%~%Original function: ~%-----------------------~%")
    (dolist (x (lines (create-target super)))
      (format stream "        ~A~%" x))
    (format stream "~%Best variant: ~%-----------------------~%")
    (dolist (x (lines best-variant))
      (format stream "        ~A~%" x))

    ;; print progress results
    (when verbose
      (format stream "~%Evolve progress~%---------------------~%")
      (dolist (x (reverse (task-runner-results task-runner)))
	(format stream "~A~%" x)))

    (when verbose
      (format stream "~%Population fitness: ")
      (write
       (sort (mapcar (lambda (x) (reduce '+ (fitness x))) population) '<)
       :length 100 :stream stream)
      (terpri stream))))

;; Symbol TR expands to show most recent progress of task-runner *tr*.
(define-symbol-macro tr (first (task-runner-results *tr*)))


;;; Example usage of the above.

#+ignore
;; To evolve a function.
(evolve-function-test "nlscan" :max-evals 1000)

#+ignore
;; To output the results.
(show-results *tr*)

#+ignore
;; Stop all evolve search loops.
(setf *running* nil)

#+ignore
;; Query status of the task runner *tr*.
(show-results *tr*)

#+ignore
;; Test each leaf function in the function index.
(test-all-leaf-functions
 *asm-path*
 *io-dir*
 *var-address-file*
 *fitness-file*
 *function-bounds-file*)

#+ignore
;; Stop or debug a runaway thread.
(bt::interrupt-thread
 (first (task-runner-workers *tr*))
 (lambda ()
   (invoke-debugger
    (make-condition 'simple-error :format-control "interrupted"))))

#|

Results of playing with some leaf functions of grep.

leaf function
-------------
_fini
fstat    ; not really a leaf, jumps to a linked library function
cwexec
delete   ; quickly goes to all *worst-fitness*
merge    ; function is large, 198 lines, nasm is very slow
insert   ; good
copy
dfasyntax
equal
notset
zeroset  ; good
copyset  ; good
clrbit
setbit
tstbit
at_endline_loc_p
at_begline_loc_p
store_op1
re_set_syntax
nlscan  ; good
register_tm_clones
deregister_tm_clones

|#
