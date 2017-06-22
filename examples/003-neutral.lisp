(in-package :software-evolution-example)
(enable-curry-compose-reader-macros :include-utf8)


(defvar variants nil "List to hold accumulated neutral variants.")

(let ((orig (from-file (make-instance 'asm) "../test/gcd/gcd.s")))
  (setf (fitness orig) (test orig))
  (do ((variant (mutate (copy orig))))
      ((>= (length variants) 10) variants)
    (setf (fitness variant) (test variant))
    (when (= (fitness variant) (fitness orig))
      (push variant variants))))
