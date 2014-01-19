(in-package :software-evolution-example)

(defun test (asm)                       ; (1)
  (ignore-errors
    (with-temp-file (bin)
      (phenome asm :bin bin)
      (count-if #'identity
                (loop :for i :below 10 :collect
                   (multiple-value-bind (stdout stderr errno)
                       (shell "../test/gcd/test.sh ~a ~d" bin i)
                     (declare (ignorable stdout stderr))
                     (zerop errno)))))))

(let* ((orig (from-file (make-instance 'asm) "../test/gcd/gcd.s")) ; (2)
       (variants ))
  (loop :for i :below 10 :do
     (multiple-value-bind (mutant edit) (mutate (copy orig)) ; (3)
       (setf (fitness mutant) (test mutant))                 ; (4)
       (format t "~2d fitness for edit ~S~%" (fitness mutant) edit)))) ; (5)
