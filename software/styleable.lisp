;;; styleable.lisp --- Add stylability to software
(defpackage :software-evolution-library/software/styleable
  (:nicknames :sel/software/styleable :sel/sw/styleable)
  (:use :gt/full
        :metabang-bind
        :software-evolution-library
        :software-evolution-library/software/project
        :software-evolution-library/software/parseable
        :software-evolution-library/software/clang)
  (:export :style-feature
           :*clang-c-ast-classes*
           :*clang-c-keywords*
           :feature-name
           :extractor-fn
           :merge-fn
           :styleable
           :features
           :feature-vecs
           :feature-vec-meta
           :style-project
           :define-feature
           :diff-feature-vectors
           :merge-styleables
           :ast-node-type-tf-extractor
           :ast-node-type-avg-depth
           :max-depth-ast-extractor
           :avg-depth-ast-extractor
           :ast-node-type-avg-depth-extractor
           :ast-full-stmt-bi-grams-extractor
           :ast-bi-grams-extractor
           :ast-keyword-tf-extractor
           :*feature-extractors*
           :ast-node-type-tf-feature
           :max-depth-ast-feature
           :avg-depth-ast-feature
           :ast-node-type-avg-depth-feature
           :ast-full-stmt-bi-grams-feature
           :ast-bi-grams-feature
           :ast-keyword-tf-feature
           :merge-normalized
           :merge-max
           :merge-means
           :uni-grams
           :to-feature-vector
           :normalize-vector
           :ast-node-types
           :ast-depth
           :max-depth-ast
           :all-ast-node-types
           :bi-grams
           :ast-full-stmt-bi-grams
           :ast-bi-grams
           :bi-grams-hashtable-to-feature
           :auto-count-keyword
           :search-keyword
           :ast-keyword-tf
           :all-keywords
           :merge-styleables
           :extract-feature
           :extract-features
           :extract-baseline-features
           :update-project-features))
(in-package :software-evolution-library/software/styleable)
(in-readtable :curry-compose-reader-macros)

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
    :initarg :extractor-fn :initform nil :reader extractor-fn
    :type function
    :documentation "Feature extractor function, takes a software object
and returns a feature vector and metadata necessary to merge feature vectors.")
   (merge-fn
    :initarg :merge-fn :initform nil :reader merge-fn
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
    :documentation "Vector of feature vector metadata."))
  (:documentation
   "Type of software objects that can have style features extracted."))

(define-software style-project (styleable project) ()
  (:documentation "A project with style-features."))


(defmacro define-feature (feature-name feature-desc
                          (extractor-fn eargs extractor-desc &rest ebody)
                          (merge-fn &optional (margs nil margs-p) &rest mbody))
   "A macro for defining feature extractors.
* FEATURE-NAME a new variable of this name will hold the style-feature object
* FEATURE-DESC a docstring for the variable FEATURE-NAME
* EXTRACTOR-FN the name for the function to perform feature extraction
* EARGS argument to EXTRACTOR-FN (should just be one, a software object)
* EXTRACTOR-DESC a docstring for EXTRACTOR-FN
* EBODY function body of EXTRACTOR-FN
* MERGE-FN the merge function to merge two feature vectors for this feature.
If the function doesn't exist, MARGS and MBODY should be provided. Otherwise,
they should not be provided.
* MARGS arguments to the merge function (should be 4: vector1, metadata1,
vector2, metadata2). Not required if MERGE-FN is already a defined function.
* MBODY body of the merge function. Not required if MERGE-FN is already a
defined function.
"
  `(progn
     ;; define merge-fn if args and body are provided
     ;; (otherwise, assume the function is already defined)
     ,@(when margs-p
         `((defun ,merge-fn ,margs ,@mbody)))

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
a key. The data structure UNI-GRAMS-HT is both updated and returned as
the result."))

(defmethod uni-grams ((items list)
                      &key (key #'identity)
                           (uni-grams-ht (make-hash-table :test #'equal)))
  "Update UNI-GRAMS-HT with counts of uni-gram occurrences in ITEMS.
Use KEY to specify a function applied to each item to generate a value used as
the hash-table key. The hash-table UNI-GRAMS-HT is both updated and returned as
the result
* ITEMS a list of elements
* KEY a function applied to each item in ITEMS to generate a hash-table key
* UNI-GRAMS-HT a hash table mapping keys to numbers of occurrences
"
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
  "Convert a set of FEATURE-VALUES into a feature vector.
Return a vector whose elements are the values from FEATURE-VALUES that are
mapped to the keys in SORTED-KEYS.
* FEATURE-VALUES a hash-table
* SORTED-KEYS a list of keys from hash table whose values will be
  added to the resulting feature vector
"
  (let ((feature-vec (make-array (length sorted-keys) :initial-element nil)))
    (iter (for key in sorted-keys)
          (for i upfrom 0)
          (setf (elt feature-vec i)
                (gethash key feature-values 0)))
    feature-vec))

(defun normalize-vector (vec)
  "Return a copy of VEC whose elements have been normalized to sum to 1.

* VEC a vector of numeric values."
  (let ((sum (reduce #'+ vec)))
    (if (zerop sum)
        (values vec sum)
        (values (map 'vector {/ _ sum} vec) sum))))

(defgeneric ast-node-types (software)
  (:documentation
   "Return a list of the node types present in the ASTs in SOFTWARE."))

(defmethod ast-node-types ((clang clang))
  "Return a list of the ast classes that occur in CLANG"
  (mapcar #'ast-class (asts clang)))

(defun merge-normalized (vec1 denom1 vec2 denom2)
  "Merge two vectors VEC1 and VEC2 which have been normalized using the
denominators DENOM1 and DENOM2, respectively
* VEC1 normalized vector of numeric values
* DENOM1 denominator used to normalize VEC1
* VEC2 normalized vector of numeric values
* DENOM2 denominator used to normalize VEC2
"
  (without-compiler-notes
    (let ((v1 (map 'vector {* denom1} vec1))
          (v2 (map 'vector {* denom2} vec2)))
      (values (map 'vector [{/ _ (+ denom1 denom2)} #'+]
                   v1
                   v2)
              (+ denom1 denom2)))))

(define-feature ast-node-type-tf-feature
    "Term frequency of AST node types."
  (ast-node-type-tf-extractor ((clang clang))
    "Return a vector with counts of occurrences of each possible AST node type.

The returned vector will have one entry for each ast class listed in `clang-c-ast-classes'.
"
    (normalize-vector (to-feature-vector (uni-grams (ast-node-types clang))
                                         *clang-c-ast-classes*)))
  (merge-normalized))


;;; Features related to depth in AST tree
(defgeneric ast-depth (software ast)
  (:documentation "Depth of AST in SOFTWARE. The root node has a depth of 0."))

(defmethod ast-depth ((clang clang) ast)
  "Depth of AST in CLANG. The root node has a depth of 0
* CLANG software object
* AST ast pointing to the root of the AST tree in CLANG
"
  (1- (length (get-parent-asts clang ast))))

(defgeneric max-depth-ast (software asts)
  (:documentation "Return the maximum depth of the ASTS in SOFTWARE."))

(defmethod max-depth-ast ((clang clang) asts)
  "Return the maximum depth of the ASTS in CLANG.
All depths are relative to the top-level of the CLANG software object (not
relative to elements in the list of ASTs).
* CLANG software object
* ASTS list of ASTs for which to determine the depth
"
  ;;; TODO: this could be much more efficient
  (reduce #'max asts :key {ast-depth clang} :initial-value 0))

(defun merge-max (vec1 meta1 vec2 meta2)
  "Return a new vector of the pair-wise maximums for values in VEC1 and VEC2.
Metadata, META1 and META2 is ignored."
  (declare (ignorable meta1 meta2))
  (values (map 'vector #'max vec1 vec2)
          nil))

(define-feature max-depth-ast-feature "Maximum depth of any node in the AST."
  (max-depth-ast-extractor ((clang clang))
                           "Return 1-element feature vector of the max depth of any AST in SOFTWARE."
                           (if (asts clang)
        (values (vector (max-depth-ast clang (asts clang)))
                nil)
        (values (vector 0) nil)))
  (merge-max))

(define-feature avg-depth-ast-feature
    "Average depth of nodes in the AST."
  (avg-depth-ast-extractor ((clang clang))
    "Return 1-element feature vector of the average depth of ASTs in SOFTWARE."
    (if-let ((asts (asts clang)))
      (values (vector (mean (mapcar {ast-depth clang} asts)))
              (length asts))
      (values (vector) 0)))
  (merge-normalized))

(defgeneric ast-node-type-avg-depth (software node-type)
  (:documentation
   "Average depth of nodes with type NODE-TYPE in the ASTs in SOFTWARE."))

(defmethod ast-node-type-avg-depth ((clang clang) (node-type symbol))
  "Average depth of nodes with type NODE-TYPE in the ASTs in CLANG.
* CLANG a clang software object
* NODE-TYPE a value to be compared against the `ast-class' of the ASTs in CLANG
"
  (bind ((node-asts (remove-if-not {eql node-type} (asts clang)
                                   :key #'ast-class))
         (depths (mapcar {ast-depth clang} node-asts)))
    (if (zerop (length depths))
        (values 0 0)
        (values (mean depths) (length depths)))))

(defgeneric all-ast-node-types (software)
  (:documentation
   "Return a list of all possible types of AST nodes for ASTs in SOFTWARE."))

(defmethod all-ast-node-types ((clang clang))
  "Return a list of all possible types of AST nodes for ASTs in CLANG.
Uses `*clang-c-ast-classes*'."
  (declare (ignorable clang))
  *clang-c-ast-classes*)


(defun merge-means (means1 lens1 means2 lens2)
  "Return a new vector merging vectors MEANS1 and MEANS2 whose elements are means.
Each element of MEANS1 and MEANS2 represents the mean of a set whose size is the
corresponding element in vector LENS1 or LENS2, respectively. The result is two
values: a vector of the combined means, and a vector of the pair-wise sums of
LENS1 and LENS2 (i.e., the sizes of the combined sets each combined mean
represents)."
  (values
   (map 'vector (lambda (mean1 len1 mean2 len2)
                  (if (and (zerop len1) (zerop len2))
                      0
                      (/ (+ (* mean1 len1)
                            (* mean2 len2))
                         (+ len1 len2))))
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
  "Update and return hash table BI-GRAMS-HT with counts of bi-gram occurrences
in ITEMS. Use KEY to specify a function applied to each item to get a value
which will be paired with another such value to create a key.
* ITEMS a list of elements
* KEY a function applied to items in ITEMS to generate values paired up and
used as keys in the hash-table
* BI-GRAMS-HT a hash table mapping pairs of keys to numbers of occurrences
"
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

(defgeneric ast-full-stmt-bi-grams (software &key bi-grams-ht)
  (:documentation "Update and return data structure BI-GRAMS with counts of
bi-grams in the ASTs of full statements in SOFTWARE."))

(defmethod ast-full-stmt-bi-grams ((clang clang)
                                   &key (bi-grams-ht
                                         (make-hash-table :test #'equal)))
  "Update and return hash-table BI-GRAMS-HT with counts of bi-grams in the
ASTs of full statements in CLANG.
* CLANG a clang software object
* BI-GRAMS-HT a hash-table for storing counts
"
  (let ((full-stmts (remove-if-not #'ast-full-stmt (asts clang)))
        (key-fn #'ast-class))
    (bi-grams full-stmts :key key-fn :bi-grams-ht bi-grams-ht)))

(defgeneric ast-bi-grams (software &key bi-grams-ht)
  (:documentation "Update and return data structure BI-GRAMS with counts of
bi-grams in the ASTs of all statements in SOFTWARE."))

(defmethod ast-bi-grams ((clang clang)
                         &key (bi-grams-ht (make-hash-table :test #'equal)))
  "Update and return hash-table BI-GRAMS-HT with counts of bi-grams in the
ASTs of all statements in CLANG.
* CLANG a clang software object
* BI-GRAMS-HT a hash-table for storing counts
"
  (bi-grams (asts clang)
            :key #'ast-class
            :bi-grams-ht bi-grams-ht))

(defgeneric bi-grams-hashtable-to-feature (software bi-grams)
  (:documentation "Return a feature-vector containing counts of bi-grams in
SOFTWARE stored in BI-GRAMS."))

(defmethod bi-grams-hashtable-to-feature ((clang clang) (bi-grams hash-table))
  "Return a feature-vector containing counts of bi-grams in CLANG stored in
hash-table BI-GRAMS.
* CLANG a clang software object
* BI-GRAMS a hash-table containing bi-grams counts
"
  (let ((keys (mappend (lambda (k1)
                         (mapcar {cons k1} *clang-c-ast-classes*))
                       *clang-c-ast-classes*)))
    (to-feature-vector bi-grams keys)))

(define-feature ast-full-stmt-bi-grams-feature
    "Number of occurrences of AST node type bi-grams in each full
     statement in an AST."
  (ast-full-stmt-bi-grams-extractor ((clang clang))
    "Return a feature vector counting AST node type bi-grams for
     full statements."
    (nest (normalize-vector)
          (bi-grams-hashtable-to-feature clang)
          (ast-full-stmt-bi-grams clang)))
  (merge-normalized))

(define-feature ast-bi-grams-feature
    "Number of occurrences of AST node type bi-grams in an AST."
  (ast-bi-grams-extractor ((clang clang))
    "Return a feature vector counting AST node type bi-grams."
    (nest (normalize-vector)
          (bi-grams-hashtable-to-feature clang)
          (ast-bi-grams clang)))
  (merge-normalized))


;;; Keyword feature extractors
(defgeneric auto-count-keyword (keyword ast)
  (:documentation
   "Return 1 if AST is a statement matching KEYWORD, 0 otherwise."))

(defmethod auto-count-keyword ((keyword string) ast)
  "Return 1 if AST is a statement matching KEYWORD, 0 otherwise.
* KEYWORD a string, to be compared against the `ast-class' of AST. See
also `*clang-c-ast-keywords-auto-count*'.
* AST a clang ast
"
  (if (member keyword
              (aget (ast-class ast) *clang-c-ast-keywords-auto-count*)
              :test #'string=)
      1 0))

(defgeneric search-keyword (software keyword ast)
  (:documentation "Return 1 if KEYWORD occurs anywhere in the text of an AST in
SOFTWARE, 0 otherwise."))

(defmethod search-keyword ((clang clang) (keyword string) ast)
  "Return 1 if KEYWORD occurs anywhere in the source text of an AST in CLANG, 0
otherwise. Only searches ASTs whose `ast-class' is in
`*clang-c-ast-keywords-search-count*'.
* CLANG a clang software object
* KEYWORD a string to be searched for in the `source-text' of AST
* AST an ast in CLANG
"
  (let ((ast-class (ast-class ast)))
    (if (not (member keyword
                     (aget ast-class *clang-c-ast-keywords-search-count*)
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

(defgeneric ast-keyword-tf (software keyword ast)
  (:documentation "Count term frequency of KEYWORD in an AST of SOFTWARE."))

(defmethod ast-keyword-tf ((clang clang) (keyword string) ast)
  "Count term frequency of KEYWORD in an AST of CLANG. Uses `auto-count-keyword'
and `search-keyword'.
* CLANG a clang software object
* KEYWORD a string whose frequency in the AST is to be counted
* AST an AST in CLANG to search for KEYWORD
"
  (+ (auto-count-keyword keyword ast)
     (search-keyword clang keyword ast)))

(defgeneric all-keywords (software)
  (:documentation
   "Return the list of possible keywords that may appear in SOFTWARE."))

(defmethod all-keywords ((clang clang))
  "Return the list of possible keywords that may appear in CLANG.
Uses `*clang-c-keywords*'.
"
  (declare (ignorable clang))
  *clang-c-keywords*)

(define-feature ast-keyword-tf-feature
    "Term frequency of keywords in an AST."
  (ast-keyword-tf-extractor ((clang clang))
                            "Return a vector containing term frequencies for all possible
keywords in SOFTWARE (see also `all-keywords').

The returned feature vector will have one entry for each keyword
listed in `*clang-c-keywords*'.
"
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
        ast-keyword-tf-feature)
    "List of feature-extractors to use as the default set during feature
extraction.")


;;; Feature extraction
(defun diff-feature-vectors (vec1 vec2)
  "Return a vector of the absolute differences between items in VEC1 and VEC2."
  (without-compiler-notes
      (map 'vector [#'abs #'-] vec1 vec2)))

(defgeneric merge-styleables (styleable1 styleable2 &key result)
  (:documentation "Merge all feature vectors from STYLEABLE1 and STYLEABLE2.
Update and return RESULT to be a `styleable' object containing the merged
feature vectors and metadata."))

(defmethod merge-styleables ((style1 styleable) (style2 styleable)
                             &key (result (make-instance 'styleable)))
  "Merge all feature vectors from STYLE1 and STYLE2. Update and return RESULT
to be a `styleable' object containing the merged feature vectors and metadata.
* STYLE1 a styleable software object
* STYLE2 a styleable software object
* RESULT a styleable software object in which to save merged vectors. May be
`eq' to STYLE1 or STYLE2.
"
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

(defgeneric extract-feature (styleable style-feature)
  (:documentation "For a STYLEABLE object, extract a single STYLE-FEATURE.
Updates the corresponding feature-vector and feature-vec-meta data in STYLEABLE
and returns two values: the extracted feature vector and its metadata."))

(defmethod extract-feature ((style styleable) (feature style-feature))
  "For a styleable STYLE object, extract a single style-feature FEATURE.
Updates the corresponding feature-vector and feature-vec-meta data in STYLE and
returns two values: the extracted feature vector and its metadata.
* STYLE a styleable software object
* FEATURE a style-feature to extract
"
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
Returns two values: a vector of feature vectors and a vector of metadata (used
by feature merge functions)."))

(defmethod extract-features ((style styleable) &key (features nil))
  "For a styleable STYLE, extract a list of style-features FEATURES.
Returns two values: a vector of feature vectors and a vector of metadata (used
by feature merge functions).
* STYLE a styleable software object
* FEATURES a list of style-features
"
  (iter (for feature in (or features (features style)))
        (multiple-value-bind (vec meta) (extract-feature style feature)
          (collect vec into vecs)
          (collect meta into metas)
          (finally (return (values (coerce vecs 'vector)
                                   (coerce metas 'vector)))))))

(defun update-project-features (project
                                &key (features nil) (files (all-files project)))
  "Update feature values for FEATURES in PROJECT by re-merging feature-vectors
and metadata for all software objects in the project.
* PROJECT a project object
* FEATURES a list of style-features whose values are to be re-computed.
May be a subset of the features tracked in project.
* FILES a list of files on which to extract features (default: `all-files')
"
  ;; Setup project data structures
  (setf (features project) features)
  (setf (feature-vecs project)
        (coerce (repeatedly (length features) nil) 'vector))
  (setf (feature-vec-meta project)
        (coerce (repeatedly (length features) nil) 'vector))

  (iter (for feature in features)
        ;; assume index of feature is the same across project
        ;; and all objects in project
        (let* ((index (position feature (features project)))
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
                               (files (all-files project)))
  "For a PROJECT object, extract a set of FEATURES for each software object in
the project and merge the results. Return two values: a vector of feature vectors
and a vector of metadata (used by feature merge functions).
* PROJECT a project software object
* FEATURES a list of features to be extracted (default: `*feature-extractors*')
* FILES a list of files on which to extract features (default: `all-files')
"
  (iter (for (file . obj) in files)
        (declare (ignorable file))
        (setf (features obj) features)
        (extract-features obj))
  (update-project-features project :features features :files files)
  (values (feature-vecs project)
          (feature-vec-meta project)))

;;; Convenience method.
(defgeneric extract-baseline-features (styleable &key features)
  (:documentation
   "For a STYLEABLE, extract FEATURES to establish baseline feature vectors.
Returns two values: a vector of feature vectors and a vector of metadata."))

(defmethod extract-baseline-features ((style styleable)
                                      &key (features *feature-extractors*))
  "For a styleable STYLE, extract FEATURES to establish baseline feature vectors.
Return two values: a vector of feature vectors and a vector of metadata.
* STYLE a styleable software object
* FEATURES a list of style-features to extract (default: `*feature-extractors*')
"
  (when features
    (setf (features style) features))
  (extract-features style :features features))
