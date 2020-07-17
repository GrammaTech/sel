(flet ((f (a b &key c)
         (values a b c)))
  (funcall #'f 1 2 :c 3))
