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

(defparameter *asm-path* "~/synth/grep-single-file/grep.asm"
  "Path to master asm file i.e. grep")

(defparameter *io-dir* "~/synth/grep-single-file/io5/"
  "Directory containing i/o files for fitness testing")

(defparameter *var-address-file*
  "~/synth/grep-single-file/grep.null-sanity.nm"
  "Path to sanity file containing data addresses")

(defparameter *fitness-file-dir*
  "~/quicklisp/local-projects/sel/software/"
  "Path to directory containing fitness harness (asm-super-mutant-fitness.c)")

(defparameter *function-bounds-file*
  "~/synth/grep-single-file/grep.null.fn-bnds.txt"
  "Path to file containing functional bounds by address")

(defparameter *tr*
  nil
  "Stores the most recent task-runner instance")

(defun mutant-generator (asm-super)
  "generates all single-cut variants of the target function"
  (cons (create-target asm-super)
	(create-all-simple-cut-variants asm-super)))

(defun create-1000-originals (asm-super)
  "Creates a list of 1000 copies of the original, target function"
  (let ((variants '()))
    (dotimes (i 1000)
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
				fitness-file-dir
				function-bounds-file)
  "Run TEST-FUNCTION on all leaf functions in the asm file (functions which
don't make any calls) and skip those functions for which no i/o data file 
is found."
  (let* ((asm-super (from-file (make-instance 'asm-super-mutant) asm-path))
	 (sel::*fitness-predicate* #'<)    ; lower fitness number is better
	 (sel::*worst-fitness* (sel::worst-numeric-fitness)))
    (if function-bounds-file
	(setf
	 (function-index asm-super)
	 (sel::load-function-bounds asm-super function-bounds-file)))
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
		   :fitness-file-dir fitness-file-dir
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
		     &key io-dir var-address-file fitness-file-dir
		       function-bounds-file max-evals period period-fn)
  (let ((asm-super
	 (sel::setup-test-function asm-path func-name mutant-generator
				   :io-dir io-dir
				   :var-address-file var-address-file
				   :fitness-file-dir fitness-file-dir
				   :function-bounds-file function-bounds-file)))
    (setf *soft* asm-super)  ;; copy to special variable for development

    (evaluate nil *soft*)
    (setf *population* (mutants *soft*))
    (setf *tournament-selector* #'lexicase-select-best)
    (setf *fitness-predicate* #'<)
    (setf *max-population-size* 1000)
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
   "fitness evals: ~D, min-size: ~F, max-size: ~F,~
                       min-fitness: ~F, orig-fitness: ~F"
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
	  (let ((start-time (get-internal-real-time)))
	    (evolve-tests
	     "~/synth/grep-single-file/grep.asm"
	     func-name
	     'create-1000-originals
	     :io-dir *io-dir*
	     :var-address-file *var-address-file*
	     :fitness-file-dir *fitness-file-dir*
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
       *fitness-file-dir*
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
 *fitness-file-dir*
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
insert
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
