;;;
;;; Testing ASM-SUPER-MUTANT
;;;
;;; To test function zerostat, first make sure parameters below are
;;; set correctly for your environment.
;;; Then execute:
;;;
;;;     (evolve-function-test "<func-name>" :max-evals 10000)
;;;     eg: (evolve-function-test "zerostat" :max-evals 10000)
;;;
;;; This binds *SOFT* to the ASM-SUPER-MUTANT master software object.
;;; It binds SEL:*POPULATION* to the population currently being evaluated.
;;; It binds *TR* to the TASK-RUNNER instance returned by EVOLVE-FUNCTION-TEST.
;;;
;;; You can query the current status of the EVOLVE process by:
;;;
;;; TR
;;;
;;; The symbol TR expends to (FIRST (TASK-RUNNER-RESULTS *TR*)), which is the
;;; most recent progress update of EVOLVE. It will update every 1000
;;; fitness tests.
;;;
;;; At any time you can use (SHOW-RESULTS *TR*) to get a report, or
;;; (SHOW-RESULTS *TR* :VERBOSE T) to get a longer report with complete
;;; progress history.
;;;
;;; The variable SEL:*RUNNING* is true if EVOLVE is continuing. You can cancel
;;; the process at any time by:
;;;     (SETF *RUNNING* NIL)
;;;
;;; If you cancel, (SHOW-RESULTS *TR*) will give a report of progress up until
;;; it was canceled.
;;;
;;; When the EVOLVE task completes successfully, it will generate a report
;;; in <*io-dir*>/<func-name>-report.txt. Subsequent EVOLVE runs of the same
;;; function will overwrite the report file.
;;;

(in-package :sel/test)

(defparameter *asm-path* "~/tests/grep-testing/grep.asm"
  "Path to master asm file i.e. grep")

(defparameter *io-dir* "~/tests/grep-testing/io5/"
  "Directory containing i/o files for fitness testing")

(defparameter *var-address-file*
  "~/tests/grep-testing/grep.null-sanity.nm"
  "Path to sanity file containing data addresses")

(defparameter *fitness-file*
  "~/quicklisp/local-projects/sel/software/asm-super-mutant-fitness.c"
  "Path to directory containing fitness harness")

(defparameter *function-bounds-file*
  "~/tests/grep-testing/grep.null.fn-bnds.txt"
  "Path to file containing functional bounds by address")

(defparameter *tr*
  nil
  "Stores the most recent task-runner instance")

(defparameter *best* nil "Store the best variant (lowest fitness score)")
(defparameter *master* nil "Store the master ASM-SUPER-MUTANT software")

(defparameter *soft-exp*
  nil
  "ASM-SUPER-MUTANT instance with alternate (expanded) tests applied")

(defun simple-cut-generator (asm-super)
  "generates all single-cut variants of the target function"
  (cons (create-target asm-super)
	(create-all-simple-cut-variants asm-super)))

(defun create-1024-originals (asm-super)
  "Creates a list of 1024 copies of the original, target function"
  (let ((variants '()))
    (dotimes (i 1024)
      (push (create-target asm-super) variants))
    variants))

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
		   'mutant-generator
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

(defun evolve-tests (asm-path func-name mutant-generator
		     &key io-dir var-address-file fitness-file
		       function-bounds-file max-evals period period-fn)
  (let ((asm-super
	 (sel::setup-test-function asm-path func-name mutant-generator
				   :io-dir io-dir
				   :var-address-file var-address-file
				   :fitness-file fitness-file
				   :function-bounds-file function-bounds-file)))
    (setf *soft* asm-super)  ;; copy to special variable for development

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

    ;; if the size of a variant's genome is < 2 we should filter it out
    (evolve nil :super-mutant-count 400
	    :max-evals max-evals
	    :filter
	    (lambda (v)
	      (> (size v) 5))
	    :period period
	    :period-fn period-fn)))

(defun population-report ()
  (format
   nil
   "fitness evals: ~D, min-size: ~F, max-size: ~F, ~
                       min-fitness: ~F, orig-fitness: ~F"
   *fitness-evals*
   (reduce 'min (mapcar 'size *population*))
   (reduce 'max (mapcar 'size *population*))
   (reduce 'min (mapcar (lambda (x) (reduce '+ (fitness x))) *population*))
   (reduce '+ (fitness (first (mutants *soft*))))))

(defun output-population-status ()
  (format
   t
   "fitness evals: ~D, min-size: ~F, max-size: ~F, ~
                       min-fitness: ~F, orig-fitness: ~F~%"
   *fitness-evals*
   (reduce 'min (mapcar 'size *population*))
   (reduce 'max (mapcar 'size *population*))
   (reduce 'min (mapcar (lambda (x) (reduce '+ (fitness x))) *population*))
   (reduce '+ (fitness (first (mutants *soft*))))))
   
(defun evolve-function-test (func-name
			     &key
			       (max-evals 1000)
			       (report-path
			         (merge-pathnames *io-dir*
				   (make-pathname :name
						(concatenate 'string
							     func-name
							     "-report")
						:type "txt"))))
  (setf *tr*
	(sel/utility::run-as-task (task1 runner1)
	  (task-save-result runner1 "Started fitness tests")
	  (let ((start-time (get-internal-real-time)))
	    (evolve-tests
	     "~/synth/grep-single-file/grep.asm"
	     func-name
	     'create-1024-originals
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
		(population-report))))
	    (task-save-result
	     runner1
	     (format nil "Elapsed time: ~G seconds"
		     (/ (float (- (get-internal-real-time) start-time))
			internal-time-units-per-second))))
	  (if report-path
	      (with-output-file (of report-path :if-exists :supersede)
		(show-results runner1 :stream of :verbose t))))))

(defun get-best-variant (population)
  "Return the best variant from population (list of variants)"
  (first (sort (copy-list population) '<
	       :key (lambda (x)(reduce '+ (fitness x))))))

(defun evaluate-expanded-tests (func-name alt-io-dir)
  "Run the best variant against an alternate set of tests"
  (let* ((best-variant (get-best-variant *population*))
	 (*soft*
	  (sel::setup-test-function
	   *asm-path*
	   func-name
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
	 (best-variant (get-best-variant population))
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
    (dolist (x (lines (get-best-variant population)))
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

;; symbol TR expands to show most recent progress of task-runner *tr*
(define-symbol-macro tr (first (task-runner-results *tr*)))

#+ignore
;; to evolve a function
(evolve-function-test "nlscan" :max-evals 1000)

#+ignore
;; to output the results file
(with-output-file (of "~/synth/result.txt" :if-exists :supersede)
  (show-results *tr* :stream of))

#+ignore
;; to stop all evolve search loops
(setf *running* nil)

#+ignore
;; to query status of the task runner *tr*
(show-results *tr*)

#+ignore
(setf *soft*
      (sel::setup-test-function
       *asm-path*
       "nlscan"
       'create-1000-originals
       *io-dir*
       *var-address-file*
       *fitness-file*
       *function-bounds-file*))

#+ignore
;; run the fitness tests
(evaluate nil *soft*)

#+ignore
;; test each leaf function in the function index
(test-all-leaf-functions
 *asm-path*
 *io-dir*
 *var-address-file*
 *fitness-file*
 *function-bounds-file*)

#+ignore
;; to stop or debug a runaway thread
(bt::interrupt-thread
 (first (task-runner-workers *tr*))
 (lambda ()
   (invoke-debugger
    (make-condition 'simple-error :format-control "interrupted"))))

#|
leaf functions of grep
--------------
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

;; possible demo script
;; simple use of ASM-SUPER-MUTANT to do one generation
;;
#+step-1
(setf *master*
      (from-file
	  (make-instance 'asm-super-mutant
			 :io-dir *io-dir*
			 :function-bounds-file *function-bounds-file*
			 :fitness-harness *fitness-file*
			 :var-table (sel::parse-sanity-file *var-address-file*))
	  *asm-path*))

#+step-2
(target-function-name *master* "zeroset")

#+step-3
;; create the variants as children of the super
(setf (mutants *master*) (simple-cut-generator *master*))

#+step-4
(evaluate nil *master*)

#+step-5
(setf *best*
      (first
       (lexicase-select-best (mutants *master*)
			     :predicate (lambda (x y) (<= x y)))))

#+step-6
(fitness *best*)

#+step-7
(reduce '+ (fitness *best*))  ; overall best score

;; to demo EVOLVE either use the recommended method (top of this file)
;; or this method which is more granular

#+step-1
(setf *master*
      (from-file
	  (make-instance 'asm-super-mutant
			 :io-dir *io-dir*
			 :function-bounds-file *function-bounds-file*
			 :fitness-harness *fitness-file*
			 :var-table (sel::parse-sanity-file *var-address-file*))
	  *asm-path*))

#+step-2
(target-function-name *master* "zeroset")

#+step-3
(setf (mutants *master*) (create-1024-originals *master*))

#+step-4
(evaluate nil *master*)

#+step-5
(setf *population* (mutants *master*))

#+step-6
(progn ; configure special vars
  (setf *tournament-selector* #'lexicase-select-best)
  (setf *fitness-predicate* #'<)
  (setf *max-population-size* 1024)
  (setf *print-length* 10)
  (setf *fitness-evals* 0)
  (setf *cross-chance* 2/3)
  (setf *mut-rate* 2/3)
  (setf *simple-mutation-types* '((SIMPLE-CUT . 1/3) (SIMPLE-SWAP . 1))))

#+step-7
(evolve nil :super-mutant-count 400
	    :max-evals 10000
	    :filter
	    (lambda (v)
	      (> (size v) 5))
	    :period 1000
	    :period-fn #'output-population-status)

#+step-8
(setf *best*
      (first
       (lexicase-select-best *population*
			     :predicate (lambda (x y) (<= x y)))))

#+step-9
;; best overall fitness
(reduce '+ (fitness *best*))

#+ignore
;;; steps 1-9 above basically are similar to  executing this command:
(evolve-function-test "zeroset" :max-evals 10000)

#+ignore
(show-results *tr* :verbose t)
