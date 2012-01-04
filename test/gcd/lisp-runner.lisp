(defun main (argv)
  (load (second argv))
  (format t "~a~%" (euclids-gcd (read-from-string (third argv))
                                (read-from-string (fourth argv)))))
