(defun f (a &optional b &rest rest &key ((:z c)) d)
  (list a b c d e f rest))

(f 1 2 3 :z 4 :d 5)
