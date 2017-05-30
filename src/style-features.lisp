(in-package :software-evolution)


(defparameter *clang-c-ast-classes*
  '("AddrLabelExpr" "ArraySubscriptExpr" "AsTypeExpr" "AttributedStmt"
    "BinaryOperator" "BlockExpr" "BreakStmt" "CallExpr" "CaseStmt"
    "CharacterLiteral" "CompoundAssignOperator" "CompoundLiteralExpr"
    "CompoundStmt" "ConditionalOperator" "ContinueStmt" "CStyleCastExpr"
    "DeclRefExpr" "DeclStmt" "DefaultStmt" "DesignatedInitExpr" "DoStmt" "Enum"
    "EnumConstant" "Field" "FloatingLiteral" "ForStmt" "Function"
    "GenericSelectionExpr" "GotoStmt" "IfStmt" "ImaginaryLiteral"
    "ImplicitCastExpr" "ImplicitValueInitExpr" "IndirectGotoStmt" "InitListExpr"
    "IntegerLiteral" "LabelStmt" "MacroExpansion" "MemberExpr" "NullStmt"
    "OffsetOfExpr" "OpaqueValueExpr" "ParenExpr" "ParenListExpr" "ParmVar"
    "PredefinedExpr" "PseudoObjectExpr" "Record" "ReturnStmt" "StmtExpr"
    "StringLiteral" "SwitchStmt" "Typedef" "UnaryExprOrTypeTraitExpr"
    "UnaryOperator" "VAArgExpr" "Var" "WhileStmt")
  "List of clang C ast-class types.")

(defparameter *clang-c-keywords*
  '("alignof" "auto" "break" "case" "char" "const" "continue" "default" "do"
    "double" "else" "enum" "extern" "float" "for" "goto" "if" "inline" "int"
    "long" "register" "restrict" "return" "short" "signed" "sizeof" "static"
    "struct" "switch" "typedef" "union" "unsigned" "void" "volatile" "while")
  "List of clang C keywords")

(defparameter *clang-c-ast-keywords-auto-count*
  '(("SwitchStmt" . ("switch"))
    ("CaseStmt" . ("case"))
    ("BreakStmt" . ("break"))
    ("DefaultStmt" . ("default"))
    ("ContinueStmt" . ("continue"))
    ("DoStmt" . ("do" "while"))
    ("IfStmt" . ("if"))
    ("Enum" . ("enum"))
    ("ForStmt" . ("for"))
    ("GotoStmt" . ("goto"))
    ("IndirectGotoStmt" . ("goto"))
    ("ReturnStmt" . ("return"))
    ("Typedef" . ("typedef"))
    ("WhileStmt" . ("while")))
  "Map AST classes to C keywords whose use is implied by the AST class (e.g.,
an IfStmt must include an if in its src-text, but it will not necessarily
include an else).")

(defparameter *clang-c-ast-keywords-search-count*
  '(("DeclStmt" . ("auto" "char" "const" "double" "enum" "extern" "float"
                   "inline" "int" "long" "register" "restrict" "short" "signed"
                   "static" "struct" "unsigned" "void" "volatile"))
    ;; roughly same list as DeclStmt, except for extern, inline
    ("Field" . ("auto" "char" "const" "double" "enum" "float"
                   "int" "long" "register" "restrict" "short" "signed"
                   "static" "struct" "unsigned" "void" "volatile"))
    ("IfStmt" . ("else")) ;; found else if "IfStmt" has 3 children
    ("UnaryExprOrTypeTraitExpr" . ("alignof" "sizeof"))
    ("Record" . ("struct" "union")))
  "Map AST classes to C keywords that may occur within them (e.g., an IfStmt
may or may not include an else clause).")


;; Uni-gram feature extractors
(defgeneric uni-grams (items &key key test)
  (:documentation "Return a hash-table containing counts of bi-gram occurrences
in a list of ITEMS. Use KEY to specify a function applied to each item to
get a value used as the hash-table key, and use TEST to specify a test function
for hash-table key equality."))

(defmethod uni-grams ((items list) &key (key #'identity) (test #'equal))
  (let ((uni-grams (make-hash-table :test test)))
    (mapcar (lambda (item)
              (let ((key (funcall key item)))
                (setf (gethash key uni-grams)
                      (1+ (gethash key uni-grams 0)))))
            items)
    uni-grams))

(defmethod uni-grams-hashtable-to-feature ((clang clang) (uni-grams hash-table)
                                           (sorted-keys list))
  (iter (for key in (hash-table-keys uni-grams))
        (unless (member key sorted-keys :test (hash-table-test uni-grams))
          (note 1 "WARNING: Removing unrecognized uni-gram ~a." key)
          (remhash key uni-grams)))
  (let ((feature-vec (make-array (length sorted-keys) :initial-element nil)))
    (iter (for key in sorted-keys)
          (for i upfrom 0)
          (setf (elt feature-vec i)
                (gethash key uni-grams 0)))
    feature-vec))

(defmethod ast-node-types ((clang clang) (asts list))
  (uni-grams asts :key #'ast-class :test #'equal))

(defmethod ast-node-type-tf-extractor ((clang clang))
  (-<>> (ast-node-types clang (asts clang))
        (uni-grams-hashtable-to-feature clang <> *clang-c-ast-classes*)))



;; Features related to depth in AST tree
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

(defmethod max-depth-ast-extractor ((clang clang))
  (vector (max-depth-ast clang (asts clang))))

(defmethod avg-depth-asts ((clang clang) asts)
  (mean (mapcar {ast-depth clang} asts)))

(defmethod avg-depth-ast-node-type ((clang clang) (node-type string))
  (avg-depth-asts clang
                  (remove-if-not {string= node-type}
                                 (asts clang)
                                 :key #'ast-class)))

(defmethod avg-depth-ast-extractor ((clang clang))
  (vector (avg-depth-asts clang (asts clang))))


(defgeneric ast-node-type-avg-depth (software node-type asts)
  (:documentation "Average depth of nodes with type NODE-TYPE in the list
of ASTS in SOFTWARE."))

(defmethod ast-node-type-avg-depth ((clang clang) (node-type string) asts)
  (bind ((node-asts (remove-if-not {string= node-type} asts
                                   :key #'ast-class))
         (depths (mapcar {ast-depth clang} node-asts)))
    (mean depths)))


;; Bi-grams feature extractors
(defgeneric bi-grams (items &key key test)
  (:documentation "Return a hash-table containing counts of bi-gram occurrences
in a list of ITEMS. Use KEY to specify a function applied to each item to
get a value which will be paired with another such value to create a key, and
use TEST to specify a test function for hash-table key equality (operating on
cons pairs)."))

(defmethod bi-grams ((items list) &key (key #'identity) (test #'equal))
  (let ((bi-grams (make-hash-table :test test)))
    (mapcar
     ;; function applied to each item in the list (fst) and its successor (next)
     (lambda (fst next)
       ;; the key in the hash table is the ordered pair of the key function
       ;; applied to fst and next
       (let ((key (cons (funcall key fst)
                        (funcall key next))))
         (setf (gethash key bi-grams)
               ;; increment value associated with key
               (1+ (gethash key bi-grams 0)))))
            items
            (cdr items))
    bi-grams))

(defmethod ast-full-stmt-bi-grams ((clang clang))
  (let ((full-stmts (remove-if-not #'ast-full-stmt (asts clang)))
        (key-fn #'ast-class))
    (bi-grams full-stmts :key key-fn)))

(defmethod ast-bi-grams ((clang clang))
  (let ((stmts (asts clang))
        (key-fn #'ast-class))
    (bi-grams stmts :key key-fn)))

(defmethod bi-grams-hashtable-to-feature ((clang clang) (bi-grams hash-table))
  ;; Warn about ASTs missing from known AST class list
  (iter (for class-pair in (hash-table-keys bi-grams))
    (unless (member (car class-pair) *clang-c-ast-classes* :test #'equal)
      (note 1 "WARNING: Removing bi-gram containing unrecognized AST class ~a."
            (car class-pair))
      (remhash class-pair bi-grams))
    (unless (member (cdr class-pair) *clang-c-ast-classes* :test #'equal)
      (note 1 "WARNING: Removing bi-gram containing unrecognized AST class ~a."
            (cdr class-pair))
      (remhash class-pair bi-grams)))

  ;; Build feature vector from hash-table
  (let* ((num-ast-classes (length *clang-c-ast-classes*))
         (feature-vec (make-array (* num-ast-classes num-ast-classes)
                                  :initial-element nil)))
    (iter (for fst in *clang-c-ast-classes*)
          (for i upfrom 0)
          (iter (for next in *clang-c-ast-classes*)
                (for j upfrom (* i num-ast-classes))
                (setf (elt feature-vec j)
                      (gethash (cons fst next) bi-grams 0))))
    feature-vec))

(defmethod ast-full-stmt-bi-grams-extractor ((clang clang))
  (bi-grams-hashtable-to-feature clang (ast-full-stmt-bi-grams clang)))

(defmethod ast-bi-grams-extractor ((clang clang))
  (bi-grams-hashtable-to-feature clang (ast-bi-grams clang)))


;; Keyword feature extractors
(defmethod auto-count-keyword ((keyword string) (ast ast-ref))
  (if (member keyword
              (aget (ast-class ast) *clang-c-ast-keywords-auto-count*
                    :test #'string=)
             :test #'string=)
      1
      0))

(defmethod search-keyword ((clang clang) (keyword string) (ast ast-ref))
  (let ((ast-class (ast-class ast)))
    (if (not (member keyword
                     (aget ast-class *clang-c-ast-keywords-search-count*
                           :test #'string=)
                     :test #'string=))
        0
        (switch (ast-class :test (lambda (str ls)
                                   (some {string= str} ls)))
          ('("DeclStmt" "Field") ;; count if keyword is in src-text
           (if (scan keyword (source-text ast))
               1
               0))
          ('("IfStmt") ;; count if keyword is "else" and ast has 3 children
           (if (= 3 (length (get-immediate-children clang ast)))
               1
               0))
          ('("UnaryExprOrTypeTraitExpr" "Record")
           ;; only count if src-text begins with alignof, sizeof, struct, union
            (if (starts-with-subseq keyword
                                    (string-trim (list #\Space #\Tab #\Newline)
                                                 (source-text ast)))
                1
                0))
          (t 0)))))

(defmethod ast-keyword-tf ((clang clang) (keyword string) ast)
  (+ (auto-count-keyword keyword ast)
     (search-keyword clang keyword ast)))

(defmethod ast-keyword-tf-extractor ((clang clang))
  (iter (for keyword in *clang-c-keywords*)
        (collect (reduce #'+ (mapcar {ast-keyword-tf clang keyword}
                                     (asts clang)))
          into keyword-tfs)
        (finally (return (coerce keyword-tfs 'vector)))))


;; Feature extraction
(defparameter *feature-extractors*
  '(ast-node-type-tf-extractor max-depth-ast-extractor
    avg-depth-ast-extractor ast-full-stmt-bi-grams-extractor
    ast-bi-grams-extractor ast-keyword-tf-extractor))

(defgeneric extract-features (software extractors &key &allow-other-keys)
  (:documentation "For a SOFTWARE object, apply a list of feature EXTRACTORS and
return a feature vector. Each extractor should be a function from a software
object to a feature vector."))

(defmethod extract-features ((clang clang) extractors &key &allow-other-keys)
  (iter (for extractor in extractors)
        (accumulate (funcall extractor clang)
                    by {concatenate 'vector}
                    initial-value (vector)
                    into feature-vec)
        (finally (return feature-vec))))
