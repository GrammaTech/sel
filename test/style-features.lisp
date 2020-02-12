;;;; style-features.lisp --- Style features tests.
(defpackage :software-evolution-library/test/style-features
  (:nicknames :sel/test/style-features)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   :arrow-macros                        ; FIXME: Remove.
   :metabang-bind                       ; FIXME: Remove.
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/utility
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang
   :software-evolution-library/software/styleable)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :test-style-features))
(in-package :software-evolution-library/test/style-features)
(in-readtable :curry-compose-reader-macros)
(defsuite test-style-features "Style features tests." (clang-available-p))

(define-software clang-styleable-test-class (clang styleable) ())

(deftest uni-grams-ht-test ()
  (let* ((sentence (list "the" "quick" "brown" "fox"
                         "the" "lazy" "brown" "dog"
                         "the" "quick" "hare"))
         (ht (uni-grams sentence)))
    (is (= 7 (length (hash-table-keys ht))))
    (is (= 3 (gethash "the" ht 0)))
    (is (= 2 (gethash "quick" ht 0)))
    (is (= 2 (gethash "brown" ht 0)))
    (is (= 1 (gethash "fox" ht 0)))
    (is (= 1 (gethash "lazy" ht 0)))
    (is (= 1 (gethash "dog" ht 0)))
    (is (= 1 (gethash "hare" ht 0)))))

(deftest to-feature-vector-test ()
  (let* ((sentence (list "the" "quick" "brown" "fox"
                         "the" "lazy" "brown" "dog"
                         "the" "quick" "hare"))
         (ht (uni-grams sentence))
         (fv (to-feature-vector ht (list "the" "quick" "brown" "fox"
                                         "lazy" "dog" "hare"))))
    (is (= 7 (length fv)))
    (is (equalp fv (vector 3 2 2 1 1 1 1)))))

(deftest function-ast-depths-are-zero ()
  (with-fixture variety-clang
    (let ((asts (functions *variety*)))
      (is (= 4 (length asts)))
      ;; depths of all function asts are 0 (all top-level)
      (is (every #'zerop (mapcar {ast-depth *variety*} asts))))))

(deftest max-depth-ast-functions-is-0 ()
  (with-fixture variety-clang
    (is (zerop (max-depth-ast *variety* (functions *variety*))))))

(deftest max-depth-ret-stmts-is-2 ()
  (with-fixture variety-clang
    (let ((return-stmts (remove-if-not [{eql :ReturnStmt} #'ast-class]
                                       (asts *variety*))))
      (is (= 2 (max-depth-ast *variety* return-stmts))))))

(deftest (merge-max-picks-larger :long-running) ()
  (bind (((:values vec1 meta1) (with-fixture variety-clang
                                 (max-depth-ast-extractor *variety*)))
         ((:values vec2 meta2) (with-fixture gcd-clang
                                 (max-depth-ast-extractor *gcd*)))
         ((:values vecr _) (merge-max vec1 meta1 vec2 meta2)))
        (is (= 1 (length vec1)))
        (is (= 1 (length vec2)))
        (is (= 1 (length vecr)))
        (is (= (elt vecr 0)
               (max (elt vec1 0)
                    (elt vec2 0))))))

(deftest avg-depth-ast-node-type-function-is-0 ()
  (with-fixture variety-clang
    (is (zerop (ast-node-type-avg-depth *variety* :Function)))))

(deftest avg-depth-ast-node-type-return-stmts ()
  (with-fixture variety-clang
    (is (= 2 (ast-node-type-avg-depth *variety* :ReturnStmt)))))

(deftest node-type-counts ()
  (with-fixture variety-clang
    ;; list of (ast-class, occurrences)
    (bind (((:values vec denom) (ast-node-type-tf-extractor *variety*))
           (ast-counts (mapcar #'cons
                               *clang-c-ast-classes*
                               (mapcar {* denom} (coerce vec 'list)))))
          ;; for each ast-class, verify occurrence count is correct
          (iter (for (type . count) in ast-counts)
                (is (= count
                       (count-if [{equal type} #'ast-class]
                                 (asts *variety*))))
                (finally (return t))))))

(deftest (ast-keywords-auto-counts :long-running) ()
  (with-fixture variety-clang
    (let ((auto-counts
           (iter (for keyword in *clang-c-keywords*)
                 (collect
                  (cons (reduce #'+ (mapcar {auto-count-keyword keyword}
                                            (asts *variety*)))
                        keyword)
                  into counts)
                 (finally (return counts)))))
      (is (equal
           auto-counts
           '((0 . "alignof") (0 . "auto") (2 . "break") (2 . "case")
             (0 . "char") (0 . "const") (1 . "continue") (1 . "default")
             (1 . "do") (0 . "double") (0 . "else") (1 . "enum")
             (0 . "extern") (0 . "float") (1 . "for") (3 . "goto") (1 . "if")
             (0 . "inline") (0 . "int") (0 . "long") (0 . "register")
             (0 . "restrict") (4 . "return") (0 . "short") (0 . "signed")
             (0 . "sizeof") (0 . "static") (0 . "struct") (1 . "switch")
             (2 . "typedef") (0 . "union") (0 . "unsigned") (0 . "void")
             (0 . "volatile") (2 . "while")))))))

(deftest ast-keywords-search-counts ()
  (with-fixture variety-clang
    (let ((search-counts
           (iter (for keyword in *clang-c-keywords*)
                 (collect
                  (cons (reduce #'+ (mapcar {search-keyword *variety*
                                                            keyword}
                                            (asts *variety*)))
                        keyword)
                  into counts)
                 (finally (return counts)))))
      (is (equal
           search-counts
           '((0 . "alignof") (0 . "auto") (0 . "break") (0 . "case")
             (1 . "char") (1 . "const") (0 . "continue") (0 . "default")
             (0 . "do") (3 . "double") (1 . "else") (1 . "enum")
             (0 . "extern") (0 . "float") (0 . "for") (0 . "goto") (0 . "if")
             (0 . "inline") (11 . "int") (0 . "long") (0 . "register")
             (0 . "restrict") (0 . "return") (0 . "short") (0 . "signed")
             (1 . "sizeof") (0 . "static") (1 . "struct") (0 . "switch")
             (0 . "typedef") (1 . "union") (0 . "unsigned") (1 . "void")
             (0 . "volatile") (0 . "while")))))))

(deftest ast-keyword-tf-extractor-correct ()
  (with-fixture variety-clang
    (let ((ls-count (-<>> (ast-keyword-tf-extractor *variety*)
                          (coerce <> 'list)
                          (mapcar {* 44}))))
      (is
       (equal
        (mapcar #'cons
                ls-count
                *clang-c-keywords*)
        '((0 . "alignof") (0 . "auto") (2 . "break") (2 . "case") (1 . "char")
          (1 . "const") (1 . "continue") (1 . "default") (1 . "do")
          (3 . "double") (1 . "else") (2 . "enum") (0 . "extern") (0 . "float")
          (1 . "for") (3 . "goto") (1 . "if") (0 . "inline") (11 . "int")
          (0 . "long") (0 . "register") (0 . "restrict") (4 . "return")
          (0 . "short") (0 . "signed") (1 . "sizeof") (0 . "static")
          (1 . "struct") (1 . "switch") (2 . "typedef") (1 . "union")
          (0 . "unsigned") (1 . "void") (0 . "volatile") (2 . "while")))))))

(deftest small-bi-grams-count-example ()
  (let* ((ls (list "the" "tortoise" "and" "the" "hare" "and" "the" "race"))
         (bi-grams (bi-grams ls :key #'identity))
         (keys (hash-table-keys bi-grams))
         (vals (hash-table-values bi-grams))
         (sorted-keys (list (cons "and" "the")
                            (cons "hare" "and")
                            (cons "the" "hare")
                            (cons "the" "race")
                            (cons "the" "tortoise")
                            (cons "tortoise" "and"))))
    ;; correct number of keys/values
    (is (= 6 (length keys) (length vals)))
    ;; correct set of keys
    (is (equal sorted-keys
               (sort keys (lambda (k1 k2)
                            (or (string< (car k1) (car k2))
                                (and (string= (car k1) (car k2))
                                     (string< (cdr k1) (cdr k2))))))))
    ;; correct set of values (all 1 except "and the" which is 2)
    (iter (for key in sorted-keys)
          (if (equal key (cons "and" "the"))
              (is (= 2 (gethash key bi-grams 0)))
              (is (= 1 (gethash key bi-grams 0)))))))

(deftest function-bi-grams-count ()
  "For a list containing N Function ASTs, there are N-1 bi-grams, all
(Function, Function)."
  (with-fixture variety-clang
    (let* ((functions (functions *variety*))
           (bi-grams (bi-grams functions :key #'ast-class))
           (keys (hash-table-keys bi-grams))
           (vals (hash-table-values bi-grams)))
      (is (= 1 (length keys) (length vals)))
      (is (equal (car keys) (cons :Function :Function)))
      (is (= (1- (length functions)) (car vals))))))

(deftest small-ast-bigrams-count-example ()
  (with-fixture variety-clang
    (flet ((asts-by-type (type)
             (remove-if-not [{eq type} #'ast-class]
                            (asts *variety*))))
      (let* ((asts (list (first (asts-by-type :Function))
                         (first (asts-by-type :IntegerLiteral))
                         (first (asts-by-type :DeclStmt))))
             (bi-grams (bi-grams asts :key #'ast-class))
             (keys (hash-table-keys bi-grams))
             (vals (hash-table-values bi-grams))
             (sorted-keys (list (cons :Function :IntegerLiteral)
                                (cons :IntegerLiteral :DeclStmt))))
        (is (= 2 (length keys) (length vals)))
        (is (equal sorted-keys
                   (sort keys (lambda (k1 k2)
                                (or (string< (car k1) (car k2))
                                    (and (string= (car k1) (car k2))
                                         (string< (cdr k1) (cdr k2))))))))
        (is (equal '(1 1) vals))))))

(deftest extract-style-features-no-asts ()
  (is (extract-baseline-features
       (from-string (make-instance 'clang-styleable-test-class)
                    ""))
      "extract-baseline-features should not throw an error on empty software"))
