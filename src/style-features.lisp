(in-package :software-evolution)
(enable-curry-compose-reader-macros :include-utf8)


(defparameter *clang-c-ast-classes*
  '(:AddrLabelExpr :ArraySubscriptExpr :AsTypeExpr :AttributedStmt
    :BinaryOperator :BlockExpr :BreakStmt :CallExpr :CaseStmt
    :CharacterLiteral :CompoundAssignOperator :CompoundLiteralExpr
    :CompoundStmt :ConditionalOperator :ContinueStmt :CStyleCastExpr
    :DeclRefExpr :DeclStmt :DefaultStmt :DesignatedInitExpr :DoStmt :Enum
    :EnumConstant :Field :FloatingLiteral :ForStmt :Function
    :GenericSelectionExpr :GotoStmt :IfStmt :ImaginaryLiteral
    :ImplicitCastExpr :ImplicitValueInitExpr :IndirectGotoStmt :InitListExpr
    :IntegerLiteral :LabelStmt :MacroExpansion :MemberExpr :NullStmt
    :OffsetOfExpr :OpaqueValueExpr :ParenExpr :ParenListExpr :ParmVar
    :PredefinedExpr :PseudoObjectExpr :Record :ReturnStmt :StmtExpr
    :StringLiteral :SwitchStmt :Typedef :UnaryExprOrTypeTraitExpr
    :UnaryOperator :VAArgExpr :Var :WhileStmt)
  "List of clang C ast-class types.")

(defparameter *clang-c-keywords*
  '("alignof" "auto" "break" "case" "char" "const" "continue" "default" "do"
    "double" "else" "enum" "extern" "float" "for" "goto" "if" "inline" "int"
    "long" "register" "restrict" "return" "short" "signed" "sizeof" "static"
    "struct" "switch" "typedef" "union" "unsigned" "void" "volatile" "while")
  "List of clang C keywords")

(defparameter *clang-c-ast-keywords-auto-count*
  '((:SwitchStmt . ("switch"))
    (:CaseStmt . ("case"))
    (:BreakStmt . ("break"))
    (:DefaultStmt . ("default"))
    (:ContinueStmt . ("continue"))
    (:DoStmt . ("do" "while"))
    (:IfStmt . ("if"))
    (:Enum . ("enum"))
    (:ForStmt . ("for"))
    (:GotoStmt . ("goto"))
    (:IndirectGotoStmt . ("goto"))
    (:ReturnStmt . ("return"))
    (:Typedef . ("typedef"))
    (:WhileStmt . ("while")))
  "Map AST classes to C keywords whose use is implied by the AST class.
E.g., an IfStmt must include an if in its src-text, but it will not necessarily
include an else.")

(defparameter *clang-c-ast-keywords-search-count*
  '((:DeclStmt . ("auto" "char" "const" "double" "enum" "extern" "float"
                  "inline" "int" "long" "register" "restrict" "short" "signed"
                  "static" "struct" "unsigned" "void" "volatile"))
    ;; roughly same list as DeclStmt, except for extern, inline
    (:Field . ("auto" "char" "const" "double" "enum" "float"
               "int" "long" "register" "restrict" "short" "signed"
               "static" "struct" "unsigned" "void" "volatile"))
    (:IfStmt . ("else")) ;; found else if "IfStmt" has 3 children
    (:UnaryExprOrTypeTraitExpr . ("alignof" "sizeof"))
    (:Record . ("struct" "union")))
  "Map AST classes to C keywords that may occur within them.
E.g., an IfStmt may or may not include an else clause.")

(defclass style-feature ()
  ((feature-name
    :initarg :feature-name :initform nil :accessor feature-name
    :type symbol
    :documentation "Name of style feature.")
   (extractor-fn
    :initarg :extractor-fn :initform nil :accessor extractor-fn
    :type function
    :documentation "Feature extractor function, takes a software object
and returns a feature vector and metadata necessary to merge feature vectors.")
   (merge-fn
    :initarg :merge-fn :initform nil :accessor merge-fn
    :type function
    :documentation "Function to merge two feature vectors. Takes as input a
two feature vectors and their metadata (as returned by the extractor) and
returns a new feature vector and updated metadata."))
  (:documentation "Type of style feature, including a feature extractor and an
accompanying merge function for combining feature vectors."))

(define-software styleable ()
  ((features
    :initarg :features :initform nil :accessor features
    :allocation :class
    :documentation "Ordered list of style features.")
   (feature-vecs
    :initarg :feature-vecs :initform nil :accessor feature-vecs
    :documentation "Vector of feature vectors, same length as features list.")
   (feature-vec-meta
    :initarg :feature-vec-meta :initform nil
    :accessor feature-vec-meta
    :documentation "Vector of feature vector meta-information."))
  (:documentation
   "Type of software objects that can have style features extracted."))

(define-software style-project (styleable project) ())

(defmacro define-feature (feature-name feature-desc
                          (extractor-fn eargs extractor-desc &rest ebody)
                                                               (merge-fn &optional (margs nil margs-p) &rest mbody))
  `(progn
     ;; define merge-fn if args and body are provided
     ;; (otherwise, assume the function is already defined)
     (when ,margs-p
       (defun ,merge-fn ,margs ,@mbody))

     ;; define extractor-fn methods
     ;; implementation for extractor-fn must be provided
     (defgeneric ,extractor-fn (software)
       (:documentation ,extractor-desc))
     (defmethod ,extractor-fn ,eargs ,@ebody)

     ;; define feature so we can use it without warnings in project extractor-fn
     (defvar ,feature-name
       (make-instance 'style-feature
         :feature-name ',feature-name
         :extractor-fn #',extractor-fn
         :merge-fn #',merge-fn)
       ,feature-desc)

     ;; pre-defined impl for project (uses provided method impl.)
     (defmethod ,extractor-fn ((project project))
       (bind (((_ . obj1) (first (all-files project)))
              (feature-index (position ',feature-name
                                       (features obj1)
                                       :key #'feature-name))
              ((:values prev-vec prev-meta) (extract-feature obj1 ,feature-name)))
         (iter (for (file . obj) in (cdr (all-files project)))
               (declare (ignorable file))
               (bind (((:values vec meta) (extract-feature obj ,feature-name))
                      ((:values new-vec new-meta)
                       (funcall #',merge-fn prev-vec prev-meta vec meta)))
                 ;; update prev-vec and prev-meta with merged info
                 (setf prev-vec new-vec)
                 (setf prev-meta new-meta)
                 (finally (setf (elt (feature-vecs project) feature-index)
                                prev-vec)
                          (setf (elt (feature-vec-meta project) feature-index)
                                prev-meta)
                          (return (values prev-vec prev-meta)))))))))


;;; Uni-gram feature extractors
(defgeneric uni-grams (items &key key uni-grams-ht)
  (:documentation
   "Update UNI-GRAMS-HT with counts of uni-gram occurrences in ITEMS.
Use KEY to specify a function applied to each item to generate a value used as
the hash-table key. The hash-table UNI-GRAMS-HT is both updated and returned as
the result."))

(defmethod uni-grams ((items list)
                      &key (key #'identity)
                           (uni-grams-ht (make-hash-table :test #'equal)))
  (mapcar (lambda (item)
            (let ((key (funcall key item)))
              (setf (gethash key uni-grams-ht)
                    (1+ (gethash key uni-grams-ht 0)))))
          items)
  uni-grams-ht)

(defgeneric to-feature-vector (feature-values sorted-keys)
  (:documentation
   "Convert a set of FEATURE-VALUES into a feature vector.
The elements of the vector are the values corresponding to SORTED-KEYS."))

(defmethod to-feature-vector ((feature-values hash-table) (sorted-keys list))
  (let ((feature-vec (make-array (length sorted-keys) :initial-element nil)))
    (iter (for key in sorted-keys)
          (for i upfrom 0)
          (setf (elt feature-vec i)
                (gethash key feature-values 0)))
    feature-vec))

(defun normalize-vector (vec)
  (let ((sum (reduce #'+ vec)))
    (values (map 'vector {/ _ sum} vec)
            sum)))

(defgeneric ast-node-types (software)
  (:documentation
   "For a SOFTWARE object, return a list of the node types present in its ASTs."))

(defmethod ast-node-types ((clang clang))
  (mapcar #'ast-class (asts clang)))

(defgeneric merge-normalized (s1 denom1 s2 denom2)
  (:documentation
   "Merge two structures S1 and S2 which have been normalized using the
denominators DENOM1 and DENOM2, respectively."))

(defmethod merge-normalized ((vec1 vector) (denom1 number)
                             (vec2 vector) (denom2 number))
  (let ((v1 (map 'vector {* denom1} vec1))
        (v2 (map 'vector {* denom2} vec2)))
    (values (map 'vector [{/ _ (+ denom1 denom2)} #'+]
                 v1
                 v2)
            (+ denom1 denom2))))

(define-feature ast-node-type-tf-feature
    "Term frequency of AST node types."
  (ast-node-type-tf-extractor ((clang clang))
    "Return a vector with counts of occurrences of each possible AST node type."
    (-<>> (ast-node-types clang)
          (uni-grams)
          (to-feature-vector <> *clang-c-ast-classes*)
          (normalize-vector)))
  (merge-normalized))


;;; Features related to depth in AST tree
(defgeneric ast-depth (software ast)
  (:documentation "Depth of AST in SOFTWARE. The root node has a depth of 0."))

(defmethod ast-depth ((clang clang) (ast ast-ref))
  (1- (length (get-parent-asts clang ast))))


(defvar *ast-depths-cache* (make-hash-table :test #'equal))
(defmethod ast-depth :around ((clang clang) (ast ast-ref))
  (let* ((result (gethash ast *ast-depths-cache* (call-next-method))))
    (setf (gethash ast *ast-depths-cache*)
          result)))

(defgeneric max-depth-ast (software asts)
  (:documentation "Return the maximum depth of the ASTS in SOFTWARE."))

(defmethod max-depth-ast ((clang clang) asts)
  (apply #'max
         (mapcar {ast-depth clang} asts)))

(defmethod merge-max ((vec1 vector) meta1 (vec2 vector) meta2)
  (declare (ignorable meta1 meta2))
  (values (map 'vector #'max vec1 vec2)
          nil))

(define-feature max-depth-ast-feature "Maximum depth of any node in the AST."
  (max-depth-ast-extractor ((clang clang))
    "Return 1-element feature vector of the max depth of any AST in SOFTWARE."
    (values (vector (max-depth-ast clang (asts clang)))
            nil))
  (merge-max))

(define-feature avg-depth-ast-feature
    "Average depth of each type of node in the AST."
  (avg-depth-ast-extractor ((clang clang))
    "Return 1-element feature vector of the average depth of ASTs in SOFTWARE."
    (let ((asts (asts clang)))
      (values (vector (mean (mapcar {ast-depth clang} asts)))
              (length asts))))
  (merge-normalized))

(defgeneric ast-node-type-avg-depth (software node-type)
  (:documentation
   "Average depth of nodes with type NODE-TYPE in the ASTs in SOFTWARE."))

(defmethod ast-node-type-avg-depth ((clang clang) (node-type symbol))
  (bind ((node-asts (remove-if-not {eql node-type} (asts clang)
                                   :key #'ast-class))
         (depths (mapcar {ast-depth clang} node-asts)))
    (if (zerop (length depths))
        (values 0 0)
        (values (mean depths) (length depths)))))

(defgeneric all-ast-node-types (software)
  (:documentation
   "Returns a list of all possible types of AST nodes for ASTs in SOFTWARE."))

(defmethod all-ast-node-types ((clang clang))
  (declare (ignorable clang))
  *clang-c-ast-classes*)

(defmethod merge-means ((means1 vector) (lens1 vector)
                        (means2 vector) (lens2 vector))
  (values
   (map 'vector (lambda (mean1 len1 mean2 len2)
                  (/ (+ (* mean1 len1)
                        (* mean2 len2))
                     (+ len1 len2)))
        means1 lens1 means2 lens2)
   (map 'vector #'+ lens1 lens2)))

(define-feature ast-node-type-avg-depth-feature
    "Average depth of each possible node type in the AST."
  (ast-node-type-avg-depth-extractor ((clang clang))
    "Returns a feature vector of average depth of AST nodes by type.
The length is the number of possible AST node tyes (as determined by
`all-ast-node-types'). Each element is the average depth of nodes of the
corresponding type in the AST of SOFTWARE."
    (iter (for node-type in (all-ast-node-types clang))
          (multiple-value-bind (mean num-items)
              (ast-node-type-avg-depth clang node-type)
            (collect mean into means)
            (collect num-items into nums-items)
            (finally (return (values (coerce means 'vector)
                                     (coerce nums-items 'vector)))))))
  (merge-means))


;; Bi-grams feature extractors
(defgeneric bi-grams (items &key key bi-grams-ht)
  (:documentation
   "Update and return BI-GRAMS-HT of counts of bi-gram occurrences in ITEMS.
Use KEY to specify a function applied to each item to get a value which will be
paired with another such value to create a key."))


(defmethod bi-grams ((items list)
                     &key (key #'identity)
                       (bi-grams-ht (make-hash-table :test #'equal)))
  (mapcar
   ;; function applied to each item in the list (fst) and its successor (next)
   (lambda (fst next)
     ;; the key in the hash table is the ordered pair of the key function
     ;; applied to fst and next
     (let ((key (cons (funcall key fst)
                      (funcall key next))))
       (setf (gethash key bi-grams-ht)
             ;; increment value associated with key
             (1+ (gethash key bi-grams-ht 0)))))
   items
   (cdr items))
  bi-grams-ht)

(defmethod ast-full-stmt-bi-grams ((clang clang)
                                   &key (bi-grams-ht
                                         (make-hash-table :test #'equal)))
  (let ((full-stmts (remove-if-not #'ast-full-stmt (asts clang)))
        (key-fn #'ast-class))
    (bi-grams full-stmts :key key-fn :bi-grams-ht bi-grams-ht)))

(defmethod ast-bi-grams ((clang clang)
                         &key (bi-grams-ht (make-hash-table :test #'equal)))
  (bi-grams (asts clang)
            :key #'ast-class
            :bi-grams-ht bi-grams-ht))

(defmethod bi-grams-hashtable-to-feature ((clang clang) (bi-grams hash-table))
  (let ((keys (mappend (lambda (k1)
                         (mapcar {cons k1} *clang-c-ast-classes*))
                       *clang-c-ast-classes*)))
    (to-feature-vector bi-grams keys)))

(define-feature ast-full-stmt-bi-grams-feature
    "Number of occurrences of AST node type bi-grams in each full statement in
an AST."
  (ast-full-stmt-bi-grams-extractor ((clang clang))
    "Return a feature vector counting AST node type bi-grams for full statements."
                                    (->> (ast-full-stmt-bi-grams clang)
                                         (bi-grams-hashtable-to-feature clang)
                                         (normalize-vector)))
  (merge-normalized))

(define-feature ast-bi-grams-feature
    "Number of occurrences of AST node type bi-grams in an AST."
  (ast-bi-grams-extractor ((clang clang))
    "Return a feature vector counting AST node type bi-grams."
                          (->> (ast-bi-grams clang)
                               (bi-grams-hashtable-to-feature clang)
                               (normalize-vector)))
  (merge-normalized))


;;; Keyword feature extractors
(defmethod auto-count-keyword ((keyword string) (ast ast-ref))
  (if (member keyword
              (aget (ast-class ast) *clang-c-ast-keywords-auto-count*)
              :test #'string=)
      1 0))

(defmethod search-keyword ((clang clang) (keyword string) (ast ast-ref))
  (let ((ast-class (ast-class ast)))
    (if (not (member keyword
                     (aget ast-class *clang-c-ast-keywords-search-count*
                           :test #'string=)
                     :test #'string=))
        0
        (switch (ast-class :test (lambda (class ls)
                                   (some {eq class} ls)))
          ('(:DeclStmt :Field) ; Count if keyword is in src-text.
            (if (scan keyword (source-text ast))
                1 0))
          ('(:IfStmt) ; Count if keyword is "else" and ast has 3 children.
            (if (= 3 (length (get-immediate-children clang ast)))
                1 0))
          ('(:UnaryExprOrTypeTraitExpr :Record)
            ;; Only count if src-text begins with alignof, sizeof, struct, union.
            (if (starts-with-subseq keyword
                                    (string-trim (list #\Space #\Tab #\Newline)
                                                 (source-text ast)))
                1 0))
          (t 0)))))

(defmethod ast-keyword-tf ((clang clang) (keyword string) ast)
  (+ (auto-count-keyword keyword ast)
     (search-keyword clang keyword ast)))

(defgeneric all-keywords (software)
  (:documentation
   "Return the list of possible keywords that may appear in SOFTWARE."))

(defmethod all-keywords ((clang clang))
  *clang-c-keywords*)

(define-feature ast-keyword-tf-feature
    "Term frequency of keywords in an AST."
  (ast-keyword-tf-extractor ((clang clang))
    "Return a vector containing term frequencies for all possible keywords in
SOFTWARE (see also `all-keywords')."
    (iter (for keyword in (all-keywords clang))
          (collect (reduce #'+ (mapcar {ast-keyword-tf clang keyword}
                                       (asts clang)))
            into keyword-tfs)
          (finally (return (normalize-vector (coerce keyword-tfs 'vector))))))
  (merge-normalized))

(defparameter *feature-extractors*
  (list ast-node-type-tf-feature max-depth-ast-feature
        avg-depth-ast-feature ast-node-type-avg-depth-feature
        ast-full-stmt-bi-grams-feature ast-bi-grams-feature
        ast-keyword-tf-feature))


;;; Feature extraction
(defmethod diff-feature-vectors ((vec1 vector) (vec2 vector))
  (map 'vector [#'abs #'-] vec1 vec2))

(defgeneric merge-styleables (styleable1 styleable2 &key result)
  (:documentation "Merge all feature vectors from STYLEABLE1 and STYLEABLE2.
Returns a new `styleable' object containing the resulting feature vectors and
meta-information."))

(defmethod merge-styleables ((style1 styleable) (style2 styleable)
                             &key (result (make-instance 'styleable)))
  (assert (= (length (feature-vecs style1))
             (length (feature-vecs style2))
             (length (feature-vec-meta style1))
             (length (feature-vec-meta style2)))
          (style1 style2)
          "Feature vector mismatch. Unable to merge styleables ~a and ~a."
          style1 style2)
  (setf (features result)
        (features style1))
  (iter (for feature-vec1 in (feature-vecs style1))
        (for meta1 in (feature-vec-meta style1))
        (for feature-vec2 in (feature-vecs style2))
        (for meta2 in (feature-vec-meta style2))
        (for merge-fn in (mapcar #'merge-fn (features style1)))
        (multiple-value-bind (vec meta)
            (funcall merge-fn
                     feature-vec1 meta1
                     feature-vec2 meta2)
          (collect vec into fvecs)
          (collect meta into fmetas)
          (finally (setf (feature-vecs result)
                         (coerce fvecs 'vector))
                   (setf (feature-vec-meta result)
                         (coerce fmetas 'vector))
                   (return result)))))

(defmethod extract-feature ((style styleable) (feature style-feature))
  (let ((index (position feature (features style)))
        (num-features (length (features style))))
    (when index
      (unless (= num-features (length (feature-vecs style)))
        (setf (feature-vecs style)
              (coerce (repeatedly num-features nil) 'vector))
        (setf (feature-vec-meta style)
              (coerce (repeatedly num-features nil) 'vector)))
      (multiple-value-bind (vec meta) (funcall (extractor-fn feature) style)
        (setf (elt (feature-vecs style) index)
              vec)
        (setf (elt (feature-vec-meta style) index)
              meta)
        (values vec meta)))))

(defgeneric extract-features (software &key features &allow-other-keys)
  (:documentation "For a SOFTWARE object, extract a set of FEATURES.
Returns two values: a vector of feature vectors and a vector of meta information
(used by feature merge functions)."))

(defmethod extract-features ((style styleable) &key (features nil))
  (iter (for feature in (or features (features style)))
        (multiple-value-bind (vec meta) (extract-feature style feature)
          (collect vec into vecs)
          (collect meta into metas)
          (finally (return (values (coerce vecs 'vector)
                                   (coerce metas 'vector)))))))

(defun update-project-features (project &key (features nil))
  (iter (for feature in features)
        ;; assume index of feature is the same across project
        ;; and all objects in project
        (let* ((index (position feature (features project)))
               (files (all-files project))
               (obj (cdr (first files))))
          (when (< 0 (length files))
            ;; init feature vecs/meta on project to that of first obj
            (setf (elt (feature-vecs project) index)
                  (elt (feature-vecs obj) index))
            (setf (elt (feature-vec-meta project) index)
                  (elt (feature-vec-meta obj) index))
            ;; merge feature vecs of rest of objs into first
            (iter (for (file . obj) in  (cdr files))
                  (declare (ignorable file))
                  (multiple-value-bind (vec meta)
                      (funcall (merge-fn feature)
                               (elt (feature-vecs obj) index)
                               (elt (feature-vec-meta obj) index)
                               (elt (feature-vecs project) index)
                               (elt (feature-vec-meta project) index))
                    (setf (elt (feature-vecs project) index)
                          vec)
                    (setf (elt (feature-vec-meta project) index)
                          meta)))))))

(defmethod extract-features ((project style-project)
                             &key (features *feature-extractors*)
                               (files nil))
  (iter (for (file . obj) in (or files (all-files project)))
        (declare (ignorable file))
        (extract-features obj :features features))
  (update-project-features project :features features)
  (values (feature-vecs project)
          (feature-vec-meta project)))

;;; Convenience method.
(defmethod extract-baseline-features ((style styleable)
                                      &key (features *feature-extractors*))
  (when features
    (setf (features style) features))
  (extract-features style :features features))
