;;; ancestral.lisp --- class adding ancestry tracking to software
;;;
;;; DOCFIXME Need a page or so introduction to ancestral software mixin.
;;;
;;; @texi{ancestral}
(defpackage :software-evolution-library/software/ancestral
  (:nicknames :sel/software/ancestral :sel/sw/ancestral)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/parseable)
  (:export :ancestral
           :ancestors
           :save-ancestry
           :reset-ancestry-id
           :get-fresh-ancestry-id
           :*next-ancestry-id*))
(in-package :software-evolution-library/software/ancestral)
(in-readtable :curry-compose-reader-macros)

(define-software ancestral ()
  ((ancestors :initarg :ancestors :accessor ancestors :initform nil))
  (:documentation "Class adding ancestry tracking to software."))

(defvar *next-ancestry-id* 0
  "Unique identifier for ancestry.")

(defun reset-ancestry-id ()
  "Reset the ancestry ID."
  (setf *next-ancestry-id* 0))

(defun get-fresh-ancestry-id ()
  "DOCFIXME"
  (1- (incf *next-ancestry-id*)))

(defmethod from-file :before ((obj ancestral) path)
  "DOCFIXME
* OBJ DOCFIXME
* PATH DOCFIXME
"
  (if (null (ancestors obj))
      (setf (ancestors obj)
            `((:base ,path
                     :how from-file
                     :id ,(get-fresh-ancestry-id))))
      (ancestors obj)))

(defmethod from-string :before ((obj ancestral) string)
  "DOCFIXME
* OBJ DOCFIXME
* STRING DOCFIXME
"
  (if (null (ancestors obj))
      (setf (ancestors obj) (list (list :base string
                                        :how 'from-string
                                        :id (get-fresh-ancestry-id))))
      (ancestors obj)))

(defmethod apply-mutation :before ((obj ancestral) op
                                   &aux (target (targets op)))
  "DOCFIXME
* OBJ DOCFIXME
* OP DOCFIXME
"
  (unless (member (type-of op)
                  ;; Inhibited operations.  E.g., we don't care about
                  ;; clang-set-range which is applied by crossover.
                  '(clang-set-range cons))
    (push (list :mutant (type-of op) :id (get-fresh-ancestry-id)
                :position (cond
                            ;; Certain parseable-mutations will have their
                            ;; targets in a list.
                            ((listp target)
                             (mapcar (lambda (item)
                                       (if (typep item 'ast)
                                           (ast-path obj item)
                                           item))
                                     (flatten target)))
                            ((typep target 'ast) (ast-path obj target))))
          (ancestors obj))))

(defmethod crossover :around ((a ancestral) (b ancestral))
  "DOCFIXME
* A DOCFIXME
* B DOCFIXME
"
  (multiple-value-bind (crossed a-point b-point) (call-next-method)
    (when a-point
      (push (list :cross-with (ancestors b)
                  :crossover '2pt
                  :id (get-fresh-ancestry-id))
            (ancestors crossed)))
    (values crossed a-point b-point)))

(defmethod (setf fitness-extra-data) :around (extra-data (obj ancestral))
  "DOCFIXME
* EXTRA-DATA DOCFIXME
* OBJ DOCFIXME
"
  (declare (ignorable extra-data))
  (when (ancestors obj)
    (setf (car (ancestors obj))
          (append (car (ancestors obj))
                  (list :fitness (funcall *fitness-scalar-fn* (fitness obj))))))
  (call-next-method))

(defmethod save-ancestry ((obj ancestral) directory filename)
  "DOCFIXME
* OBJ DOCFIXME
* DIRECTORY DOCFIXME
* FILENAME DOCFIXME
"
  (let ((dot (make-pathname :directory directory
                            :name filename
                            :type "dot"))
        (svg (make-pathname :directory directory
                            :name filename
                            :type "svg")))
    (with-open-file (*standard-output* dot
                                       :direction :output :if-exists :supersede)
      (multiple-value-bind (nodes edges)
          (ancestry-graph (ancestors obj))
        (graphviz nodes edges :name "Ancestry")))
    (shell "dot -Tsvg ~a > ~a" dot svg)))

(defun node-parents (x x-parent)
  "DOCFIXME
* X DOCFIXME
* X-PARENT DOCFIXME
"
  (cond
    ((plist-get :how x)
     nil)
    ((plist-get :mutant x)
     (list (list :ancestry x-parent
                 :id (plist-get :id (car x-parent))
                 :graphviz (list :label (format nil "~(~a~)"
                                                (plist-get :mutant x))))))
    ((plist-get :crossover x)
     (list (list :ancestry x-parent
                 :id (plist-get :id (car x-parent))
                 :graphviz (list :label "crossover"))
           (list :ancestry (plist-get :cross-with x)
                 :id (plist-get :id (car (plist-get :cross-with x)))
                 :graphviz (list :label "crossover" :style "dashed"))))
    (t (error "unknown ancestry"))))

(defun fitness-color (fitness)
  "DOCFIXME
* FITNES DOCFIXME
"
  (if fitness
      (format nil "~a ~a ~a"
              (clamp (+ 0.33 (* fitness -0.00556)) 0.0 1.0)
              1.0
              1.0)
      "lightgrey"))

(defun to-node-descr (x x-parent)
  "DOCFIXME
* X DOCFIXME
* X-PARENT DOCFIXME
"
  (list
   :id (plist-get :id x)
   :graphviz (let ((fitness (plist-get :fitness x)))
               (list (if fitness :fillcolor :color) (fitness-color fitness)
                     :shape (cond
                              ((not fitness) "diamond")
                              ((plist-get :how x) "note")
                              (t "ellipse"))
                     :style "filled"
                     :label (cond
                              ((not fitness) "")
                              ((plist-get :how x) (format nil "~a\\n~a"
                                                          (plist-get :how x)
                                                          fitness))
                              (t             (format nil "~a" fitness)))))
   :parents (node-parents x x-parent)))

(defun ancestry-graph (history &key (with-nodes nil) (with-edges nil))
  "Gather the ancestor graph.
* HISTORY DOCFIXME
* WITH-NODES DOCFIXME
* WITH-EDGES DOCFIXME
"
  (let ((edges (or with-edges (make-hash-table :test #'equal)))
        (nodes (or with-nodes (make-hash-table :test #'equal))))
    (labels ((add-edge (x y e)
                       (let ((x-out-edges (gethash x edges)))
                         (setf (gethash x edges) (cons (cons y e) x-out-edges)))))
      (loop :for (item . rest) :on history
            :do (let ((descr (to-node-descr item rest)))
                  (when (not (gethash (plist-get :id descr) nodes))
                    (loop :for parent :in (cdr (plist-get :parents descr))
                          :do (ancestry-graph (plist-get :ancestry parent)
                                              :with-nodes nodes
                                              :with-edges edges))
                    (assert (plist-get :id descr))
                    (setf (gethash (plist-get :id descr) nodes)
                          (plist-get :graphviz descr))
                    (loop :for parent :in (plist-get :parents descr)
                          :do (add-edge (plist-get :id parent)
                                        (plist-get :id descr)
                                        (plist-get :graphviz parent)))))))
    ;; Back at top level of recursion, return the graph data
    (when (not with-nodes) (values nodes edges))))

(defun graphviz (nodes edges &key (name "G") (stream t) (rankdir 'LR))
  "Generate graphviz code to render a graph.
Nodes are identified by the keys in the hashtables NODES and EDGES.
The values in NODES are plists of graphviz attribute/value pairs.  The
values in EDGES are pairs of the target node id and a plist of
attribute/value pair

* NODES DOCFIXME
* EDGES DOCFIXME
* NAME DOCFIXME
* STREAM DOCFIXME
* RANKDIR DOCFIXME
"
  (format stream "digraph ~a {~%    rankdir=~a;~%" name rankdir)
  (flet ((to-attr-string (attrs)
           (mapcar (lambda (pair)
                     (destructuring-bind (key . value) pair
                       (format nil "~(~a~)=\"~a\"" key value)))
                   (plist-alist attrs))))
    (loop for x being the hash-keys in nodes using (hash-value attrs)
       do (format stream "    n~a [~{~a~^,~}];~%" x (to-attr-string attrs)))
    (loop for x being the hash-keys in edges using (hash-value out-edges)
       do (loop for (y . attrs) in out-edges
             do (format stream "    n~a -> n~a [~{~a~^,~}];~%"
                        x y (to-attr-string attrs))))
    (format stream "}~%")))
