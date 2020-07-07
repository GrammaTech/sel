(defun f (a &rest b
          &key ((:c d) 1)
          &aux (e (list a b d)))
  e)
