(defun f (&rest rest &key a b c)
  (values
   rest
   (list a b c)))

(f :a 1 :b 2 :c 3)
