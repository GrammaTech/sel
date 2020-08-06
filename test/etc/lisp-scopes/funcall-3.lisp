(defun f (&key ((:a b) 1) (c 2) d e)
  (list b c d e))

(f :a 1 :c 2 :e 3)
