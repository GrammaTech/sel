(in-package :software-evolution-library)


;;; Three parts of the merge.
(defparameter *orig*
  (from-file (make-instance 'clang)
             (make-pathname :name "gcd-wo-curlies"
                            :type "c"
                            :directory sel/test::+gcd-dir+)))

(defparameter *fix*
  (from-file (make-instance 'clang)
             (make-pathname :name "gcd-wo-curlies-fix"
                            :type "c"
                            :directory sel/test::+gcd-dir+)))

(defparameter *prose*
  (from-file (make-instance 'clang)
             (make-pathname :name "gcd-wo-curlies-prose"
                            :type "c"
                            :directory sel/test::+gcd-dir+)))


;;; Three test suites.
(defun make-test (left right result)
  (lambda (bin)
    (zerop (nth-value 2 (shell "/tmp/limit ~a ~d ~d|grep -q ~d"
                               bin left right result)))))

(defparameter *orig-tests*
  (mapcar {apply #'make-test} '((1071 1029 21)
                                (555 666 111)
                                (678 987 3)
                                (8767 653 1)
                                (16777216 512 512)
                                (16 4 4)
                                (315 831 3)
                                (513332 91583315 1)
                                (112 135 1)
                                (310 55 5)
                                (55 0 55))))

(defparameter *fix-tests*
  (mapcar {apply #'make-test} '((0 55 55))))

(defparameter *prose-tests*
  (mapcar {apply #'make-test} '((2 4 "gcd"))))

(defparameter *suite* (append *orig-tests* *fix-tests* *prose-tests*))

(defun run-suite (obj)
  "Evaluate OBJ against the full `*suite*'."
  (with-temp-file (bin)
    (phenome obj :bin bin)
    (mapcar {funcall _ bin} *suite*)))


;;; Parameters
(setf *fitness-predicate* (lambda (a b) (and a (not b))) ; Prefer higher fitness.
      *max-population-size* (expt 2 10) ; Reasonable population size.
      *worst-fitness* (mapcar (constantly nil) *suite*)
      *target-fitness-p* [{equalp (mapcar (constantly t) *suite*)} #'fitness]
      *cross-chance* 2/3
      *population* (iter (for i below *max-population-size*)
                         (collect (copy (random-elt (list *orig* *fix* *prose*))))))


;;; Check sanity.
(assert (and (tree-equal (evaluate #'run-suite *orig*)
                         '(T T T T T T T T T T T NIL NIL))
             (tree-equal (evaluate #'run-suite *fix*)
                         '(T T T T T T T T T T T T NIL))
             (tree-equal (evaluate #'run-suite *prose*)
                         '(T T T T T T T T T T T NIL T))))

#+(or )        ; Don't run evolution every time we load/eval the file.
(handler-bind        ; Handle errors that might occur during mutation.
    ((no-mutation-targets
      (lambda (e)
        (declare (ignorable e))
        (invoke-restart 'try-another-mutation)))
     (phenome
      (lambda (e)
        (declare (ignorable e))
        (invoke-restart 'return-nil-for-bin)))
     (t (lambda (e)
          (cond
            ((find-restart 'try-another-mutation)
             (invoke-restart 'try-another-mutation))
            ((find-restart 'ignore-failed-mutation)
             (invoke-restart 'ignore-failed-mutation))
            (t (error e))))))
  (generational-evolve #'simple-reproduce
    {simple-evaluate #'run-suite}
    #'lexicase-select))


;;; Initial Results
;; #<CLANG {1004FC4223}>
;; SEL> (defvar result *)
;; RESULT
;; SEL> (genome result)
;; "#include <stdio.h>
;; #include <stdlib.h>
;;
;; int main(int argc, char *argv[]) {
;;     double a,b,c;
;;     double r1, r2;
;;     a = atoi(argv[1]);
;;     b = atoi(argv[2]);
;;
;;     if (a == 0) {
;;         printf(\"%g\\n\", b);
;;     } else {
;;         while (b != 0)
;;         if (a > b) a = a - b;
;;         else       b = b - a;
;;
;;     printf(\"gcd=%g\\n\", a);
;;
;;     return 0;
;; }
;;     printf(\"%g\\n\", a);
;;
;;     return 0;
;; }
;; "
;; SEL> (fitness result)
;; (T T T T T T T T T T T T T)
;; SEL> (store result "doc/examples/auto-merge.store")
;; #<CLANG {1004FC4223}>
;; SEL> *fitness-evals*
;; 800
;; SEL>

(defun eval-lines (lines &aux obj)
  (ignore-errors
    (with-temp-file (src "c")
      (string-to-file
       (mapconcat #'identity lines (string #\Newline)) src)
      (setf obj (from-file (make-instance 'clang) src)))
    (evaluate #'run-suite obj)
    (funcall *target-fitness-p* obj)))

;; SEL> (delta-debug:minimize (lines (copy result)) #'eval-lines)
;; ("int main(int argc, char *argv[]) {" "    double a,b,c;"
;;  "    a = atoi(argv[1]);" "    b = atoi(argv[2]);" "    if (a == 0) {"
;;  "        printf(\"%g\\n\", b);" "    } else {" "        while (b != 0)"
;;  "        if (a > b) a = a - b;" "        else       b = b - a;"
;;  "    printf(\"gcd=%g\\n\", a);" "}" "}")
;; SEL> (defvar lines *)
;; LINES
;; SEL> (defvar best (let ((best (copy result)))
;;                     (setf (genome best)
;;                           (mapconcat #'identity lines (string #\Newline)))
;;                     best))
;; BEST
;; SEL> (genome best)
;; "int main(int argc, char *argv[]) {
;;     double a,b,c;
;;     a = atoi(argv[1]);
;;     b = atoi(argv[2]);
;;     if (a == 0) {
;;         printf(\"%g\\n\", b);
;;     } else {
;;         while (b != 0)
;;         if (a > b) a = a - b;
;;         else       b = b - a;
;;     printf(\"gcd=%g\\n\", a);
;; }
;; }"
;; SEL> (clang-format best)
;; #<CLANG {1011721873}>
;; 0
;; SEL> (genome best)
;; "int main(int argc, char *argv[]) {
;;   double a, b, c;
;;   a = atoi(argv[1]);
;;   b = atoi(argv[2]);
;;   if (a == 0) {
;;     printf(\"%g\\n\", b);
;;   } else {
;;     while (b != 0)
;;       if (a > b)
;;         a = a - b;
;;       else
;;         b = b - a;
;;     printf(\"gcd=%g\\n\", a);
;;   }
;; }"
;; SEL>


;;; Now lets try to get this last bit.

#+(or )                                 ; Update the parameters.
((push (make-test 0 4 "gcd") *suite*)
 (count-if [{= 14} #'length #'fitness] *population*)
 (setf *max-population-size* (expt 2 10) ; Reasonable population size.
       *worst-fitness* (mapcar (constantly nil) *suite*)
       *target-fitness-p* [{equalp (mapcar (constantly t) *suite*)} #'fitness]))

#+(or )                              ; Second run to finish the merge.
(handler-bind        ; Handle errors that might occur during mutation.
    ((no-mutation-targets
      (lambda (e)
        (declare (ignorable e))
        (invoke-restart 'try-another-mutation)))
     (phenome
      (lambda (e)
        (declare (ignorable e))
        (invoke-restart 'return-nil-for-bin)))
     (t (lambda (e)
          (cond
            ((find-restart 'try-another-mutation)
             (invoke-restart 'try-another-mutation))
            ((find-restart 'ignore-failed-mutation)
             (invoke-restart 'ignore-failed-mutation))
            (t (error e))))))
  (progn
    ;; Reset everyone's fitness.
    (mapc (lambda (ind) (setf (fitness ind) (run-suite ind)))
          *population*)
    ;; Kickoff evolution again.
    (generational-evolve #'simple-reproduce
      {simple-evaluate #'run-suite}
      #'lexicase-select)))


;;; One more try.

#+(or )                                 ; Update the parameters.
((push (lambda (bin)
         (zerop (nth-value 2 (shell "/tmp/limit ~a ~d ~d|grep -vq \"^[[:digit:]]\""
                                    bin left right))))
       *suite*)
 (setf *max-population-size* (expt 2 10) ; Reasonable population size.
       *worst-fitness* (mapcar (constantly nil) *suite*)
       *target-fitness-p* [{equalp (mapcar (constantly t) *suite*)} #'fitness]))

#+(or )                              ; Second run to finish the merge.
(handler-bind        ; Handle errors that might occur during mutation.
    ((no-mutation-targets
      (lambda (e)
        (declare (ignorable e))
        (invoke-restart 'try-another-mutation)))
     (phenome
      (lambda (e)
        (declare (ignorable e))
        (invoke-restart 'return-nil-for-bin)))
     (t (lambda (e)
          (cond
            ((find-restart 'try-another-mutation)
             (invoke-restart 'try-another-mutation))
            ((find-restart 'ignore-failed-mutation)
             (invoke-restart 'ignore-failed-mutation))
            (t (error e))))))
  (progn
    ;; Reset everyone's fitness.
    (mapc (lambda (ind) (setf (fitness ind) (run-suite ind)))
          *population*)
    ;; Kickoff evolution again.
    (generational-evolve #'simple-reproduce
      {simple-evaluate #'run-suite}
      #'lexicase-select)))


;;;; Doing this in a reasonable way...
;;;
;;; 1. Extend impending super-mutant work (after getting the current
;;;    version finished and committed) by implementing a version of
;;;    the following general tree (S-expression) differencing
;;;    algorithm in SEL.  (I imagine this would be something we
;;;    implement in a self-contained ASDF system inside of SEL so
;;;    other people could use it for other things.)
;;;
;;;    http://thume.ca/2017/06/17/tree-diffing/#a-tree-diff-optimizer
;;;
;;;    This algorithm, when applied to C/C++ AST trees parsed with Clang,
;;;    should provide minimal differences.  This could be used for tighter
;;;    super-mutants.  It could also be used to provide higher quality
;;;    source differences than any current tools I'm aware of.  E.g.,
;;;
;;;    - A better code differencing tool.  There already exists syntax for
;;;      diff regions at the sub-line boundary used by tools like wdiff.  We
;;;      could use such a tree differences with our Clang ASTs to annotate
;;;      source differences which difference at the AST level instead of the
;;;      line level... This would be nice.  (I'd personally be more excited
;;;      about the S-expression level diffs for CL source review.)  Git
;;;      allows configurable differencing engines, so this could be easily
;;;      adopted pretty widely.  I think this is sort of exciting.
;;;
;;;    - Feature toggles and A/B testing support.  We could also use this
;;;      sort of differencing to turn two versions of a program into a
;;;      single version with a feature toggle.  This is basically exactly
;;;      what we would be doing with super mutants.  The difference is that
;;;      by performing the diffs more precisely we open the possibility that
;;;      developers might want to commit and maintain both sides of the
;;;      feature.  This sort of tool (with super-mutants) would also
;;;      automate the compilation of binaries to support A/B testing.
;;;
;;; 2. Fault localization (tarantula style).  We would have three sets of
;;;    tests; original tests O, and tests added by new branches A and B.
;;;    Thus every statement in the programs falls into the superset of
;;;    {O,A,B} = {∅, {A}, {B}, {O}, {A,B}, {A,O}, {B,O}, {A,B,O}}.
;;;
;;;    If the goal of the resolution is to pick the right content for every
;;;    diff region, we could...
;;;
;;;    - limit changes to diff regions between branches A and B
;;;    - take the A-side diff of the diff for statements in {A} and {A,O}
;;;    - take the B-side diff of the diff for statements in {B} and {B,O}
;;;    - for all others seed an initial population with random choices, or
;;;      both concatenate, or something...
;;;
;;; 3. Next would be determining a good fitness function, mutation
;;;    operations, etc...
;;;
;;; Questions:
;;; - Should we consider gum-tree as a diff alternative?  Are moves important?
;;; - Will we ever want to touch code outside of the difference areas.
;;; - Should we constrain ourselves to what e-diff provides?
;;;
