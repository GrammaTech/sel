(defun main (argv)
  (format t "~a~%"
          (eval (read-from-string
                 (if (third argv)
                     (progn (load (second argv)) (third argv))
                     (second argv))))))
