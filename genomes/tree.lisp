;;; Tree Genomes
(defstruct (tree (:copier tree-copier))
  (data nil)
  (branches nil))

(defun to-tree (item)
  (if (consp item)
      (make-tree
       :data (car item)
       :branches (mapcar #'to-tree (cdr item)))
      (make-tree :data item)))

(defun to-list (tree)
  (if (tree-branches tree)
      (cons (tree-data tree)
            (mapcar #'to-list (tree-branches tree)))
      (tree-data tree)))

(defun map-tree (type fun tree)
  (let ((first (funcall fun tree))
        (rest (mapcar (lambda (branch) (map-tree type fun branch))
                      (tree-branches tree))))
    (case type
      (tree (make-tree :data first :branches rest))
      (list (if rest (cons first rest) first)))))

(defun accessors (tree &aux (ind -1))
  "Return a list of accessors to subtrees in BFS order."
  (cons 'it
        (mapcan (lambda (branch)
                  (incf ind)
                  (mapcar (lambda (ac) `(nth ,ind (tree-branches ,ac)))
                          (accessors branch)))
                (tree-branches tree))))

(defmethod inds ((genome tree) &aux (counter -1) inds)
  (map-tree 'list (lambda (_) (declare (ignorable _))
                     (push (incf counter) inds))
            genome)
  (reverse inds))

(defmethod ind ((genome tree) index &aux (counter -1) result)
  (map-tree 'tree (lambda (current)
                    (when (= (incf counter) index)
                      (setq result current))) genome)
  result)

(defmethod (setf ind) (new (genome tree) index)
  (if (= index 0)
      (progn
        (setf (tree-data genome) (tree-data new))
        (setf (tree-branches genome) (tree-branches new)))
      (let ((ac (nth index (accessors genome))))
        (eval `((lambda (it) (setf ,ac ,new)) ,genome)))))
