;;; ancestral --- class adding ancestry tracking to software
(in-package :se)

(defclass ancestral ()
  ((ancestors :initarg :ancestors :accessor ancestors :initform nil))
  (:documentation "Class adding ancestry tracking to software."))

(defvar *next-ancestry-id* 0
  "Unique identifier for ancestry.")

(defun get-fresh-ancestry-id ()
  (let ((id *next-ancestry-id*))
    (incf *next-ancestry-id*)
    id))

(defmethod from-file :before ((obj ancestral) path)
  (setf (ancestors obj) (list (alist :base path
                                     :how 'from-file
                                     :id (get-fresh-ancestry-id)))))

(defmethod from-string :before ((obj ancestral) string)
  (setf (ancestors obj) (list (alist :base string
                                     :how 'from-string-exactly
                                     :id (get-fresh-ancestry-id)))))

(defmethod apply-mutation :around ((obj ancestral) op)
  (multiple-value-call
      (lambda (variant &rest rest)
        (push (alist :mutant op :id (get-fresh-ancestry-id))
              (ancestors obj)))
    (call-next-method)))

(defmethod crossover :around ((a ancestral) (b ancestral))
  (multiple-value-bind (crossed a-point b-point) (call-next-method)
    (when a-point
      (push (alist :cross-with (ancestors b)
                   :crossover '2pt
                   :id (get-fresh-ancestry-id))
            (ancestors crossed)))
    (values crossed a-point b-point)))

(defmethod save-ancestry ((obj ancestral) dirname filename)
  (let ((dot (make-pathname :directory dirname
                            :name filename
                            :type "dot"))
        (svg (make-pathname :directory dirname
                            :name filename
                            :type "svg")))
    (with-open-file (*standard-output* dot
                     :direction :output :if-exists :supersede)
      (multiple-value-bind (nodes edges)
          (ancestry-graph (ancestors best))
        (graphviz nodes edges :name "Ancestry")))
    (shell "dot -Tsvg ~a > ~a" dot svg)))

(defun node-parents (x x-parent)
  (cond
    ((aget :how x)
     nil)
    ((aget :mutant x)
     (list (alist :ancestry x-parent
                  :id (aget :id (car x-parent))
                  :graphviz (alist :label (format nil "~(~a~)"
                                                  (car (aget :mutant x)))))))
    ((aget :crossover x)
     (list (alist :ancestry x-parent
                  :id (aget :id (car x-parent))
                  :graphviz (alist :label "crossover"))
           (alist :ancestry (aget :cross-with x)
                  :id (aget :id (car (aget :cross-with x)))
                  :graphviz (alist :label "crossover" :style "dashed"))))
    (t (error "unknown ancestry"))))

(defun fitness-color (fitness)
  (if fitness
      (format nil "~a ~a ~a"
              (clamp (+ 0.33 (* fitness -0.00556)) 0.0 1.0)
              1.0
              1.0)
      "lightgrey"))

(defun to-node-descr (x x-parent)
  (alist
   :id (aget :id x)
   :graphviz (let ((fitness (aget :fitness x)))
               (alist (if fitness :fillcolor :color) (fitness-color fitness)
                      :shape (cond
                               ((not fitness) "diamond")
                               ((aget :how x) "note")
                               (t             "ellipse"))
                      :style "filled"
                      :label (cond
                               ((not fitness) "")
                               ((aget :how x) (format nil "~a\\n~a"
                                                      (aget :how x)
                                                      fitness))
                               (t             (format nil "~a" fitness)))))
   :parents (node-parents x x-parent)))

(defun ancestry-graph (history &key (with-nodes nil) (with-edges nil))
  ;; Gather the ancestor graph
  (let ((edges (or with-edges (make-hash-table :test #'equal)))
        (nodes (or with-nodes (make-hash-table :test #'equal))))
    (labels ((add-edge (x y e)
               (let ((x-out-edges (gethash x edges)))
                 (setf (gethash x edges) (cons (cons y e) x-out-edges)))))
      (loop :for (item . rest) :on history
         :do (let ((descr (to-node-descr item rest)))
               (when (not (gethash (aget :id descr) nodes))
                 (loop :for parent :in (cdr (aget :parents descr))
                    :do (ancestry-graph (aget :ancestry parent)
                                        :with-nodes nodes
                                        :with-edges edges))
                 (assert (aget :id descr))
                 (setf (gethash (aget :id descr) nodes) (aget :graphviz descr))
                 (loop :for parent :in (aget :parents descr)
                    :do (add-edge (aget :id parent)
                                  (aget :id descr)
                                  (aget :graphviz parent)))))))
    ;; Back at top level of recursion, return the graph data
    (when (not with-nodes) (values nodes edges))))

(defun print-attr (stream kv colonp atsignp)
  (format stream "~(~a~)=\"~a\"" (car kv) (cdr kv)))

;; Generate graphviz code to render a graph.  Nodes are identified by
;; the keys in the hashtables NODES and EDGES.
;; The values in NODES are alists of graphviz attribute/value pairs.
;; The values in EDGES are pairs of the target node id and an alist
;; of attribute/value pair
(defun graphviz (nodes edges &key (name "G") (stream t) (rankdir 'LR))
  (format stream "digraph ~a {~%    rankdir=~a;~%" name rankdir)
  (loop for x being the hash-keys in nodes using (hash-value attrs)
     do (format stream "    n~a [~{~/se:print-attr/~^,~}];~%" x attrs))
  (loop for x being the hash-keys in edges using (hash-value out-edges)
     do (loop for (y . attrs) in out-edges
           do (format stream "    n~a -> n~a [~{~/se:print-attr/~^,~}];~%"
                      x y attrs)))
  (format stream "}~%"))

