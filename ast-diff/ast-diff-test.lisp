;;; Snippets used to test ast-diff

(in-package :sel/ast-diff)

(defun make-test-input (n f l)
  (append (list f)
	  (iter (for i from 1 to n)
		(collect i))
	  (list l)))

(defun do-test (n)
  (let ((t1 (make-test-input n 'a 'b))
	(t2 (make-test-input n 'c 'd)))
    (time (ast-diff t1 t2))))

(defun diff-asts-old (a1 a2)
  (time (ast-diff-on-lists a1 a2)))

(defun diff-asts (a1 a2)
  (time (ast-diff a1 a2)))

(defun diff-files-old (f1 f2)
  (let* ((ast1 (sel:ast-root (sel:from-file (make-instance 'sel:clang) f1)))
	 (ast2 (sel:ast-root (sel:from-file (make-instance 'sel:clang) f2))))
    (time (ast-diff-on-lists ast1 ast2))))

(defun diff-files (f1 f2)
  (let* ((ast1 (sel:ast-root (sel:from-file (make-instance 'sel:clang) f1)))
	 (ast2 (sel:ast-root (sel:from-file (make-instance 'sel:clang) f2))))
    (time (ast-diff ast1 ast2))))

(defun diff-strings (s1 s2 &key (fn #'ast-diff))
  (flet ((%fs (s) (sel:ast-root (sel:from-string (make-instance 'sel:clang) s))))
    (let ((ast1 (%fs s1))
	  (ast2 (%fs s2)))
      (time (funcall fn ast1 ast2)))))

;;; Utility function to find nice primes for hashing
;;; Used this when putting together ast-hash

(defun is-prime? (n)
  (assert (integerp n))
  (and
   (not (<= n 1))
   (or (= n 2)
       (and (= (mod n 2) 1)
	    (let ((s (isqrt n)))
	      (iter (for i from 3 to s by 2)
		    (never (= (mod n i) 0))))))))

(defun find-prime (n)
  (assert (typep n '(integer 3)))
  (when (evenp n) (decf n))
  (iter (until (is-prime? n))
	(decf n 2))
  n)

(defun find-prime-up (n)
  (assert (typep n '(integer 3)))
  (when (evenp n) (incf n))
  (iter (until (is-prime? n)) (incf n 2))
  n)

;;; Random testing of ast-diff-on-lists, ast-diff

(defun random-partition (n m)
  "Produce a random partition of the integer N into M positive integers."
  (assert (>= n m 1))
  (let ((a (make-array m :initial-element 1)))
    (iter (repeat (- n m))
	  (incf (aref a (random m))))
    (coerce a 'list)))

(defun make-random-diff-input (n &key top)
  "Generate a random s-expression for use as input to ast-diff.  When TOP
is true don't generate a non-list."
  (if (<= n 1)
      (let ((x (case (random 4)
		 (0 (random 5))
		 (1 (random 10))
		 (2 (elt #(a b c d e) (random 5)))
		 (3 (string (elt "abcdefghij" (random 10)))))))
	(if top (list x) x))
      (let ((p (random-partition n (1+ (random (min 20 n))))))
	(mapcar #'make-random-diff-input p))
      ))

(defun mutate-diff-input (x &key top)
  "Cause a single random change to a diff input produced
by MAKE-RANDOM-DIFF-INPUT"
  (if (consp x)
      (let ((pos (random (length x))))
	(setf x (copy-seq x))
	(setf (subseq x pos (1+ pos))
	      (list (mutate-diff-input (elt x pos))))
	x)
      (make-random-diff-input 1 :top top)))

(defun random-test (n)
  "Test that the old and new diff algorithms do the same thing.
They sometimes won't, because the good enough algorithm doesn't
necessarily find the best diff."
  (let* ((x (make-random-diff-input n :top t))
	 (y (mutate-diff-input (mutate-diff-input x :top t) :top t)))
    (let ((result1 (multiple-value-list (ast-diff-on-lists x y)))
	  (result2 (multiple-value-list (ast-diff x y))))
      (if (equal (cadr result1) (cadr result2))
	  nil
	  (values :fail x y result1 result2)))))

(defun random-test-2 (n &optional (fn #'ast-diff))
  "Confirm on random input that the diff algorithm produces
a valid patch.  Return :FAIL (and other values) if not."
  (let* ((x (make-random-diff-input n :top t))
	 (y (mutate-diff-input (mutate-diff-input x :top t) :top t))
	 (diff (funcall fn x y))
	 (patched-x (ast-patch x diff)))
    (unless (equalp y patched-x)
      (values :fail x y patched-x diff))))

      
