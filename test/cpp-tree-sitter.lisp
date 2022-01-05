;;;; cpp-tree-sitter.lisp --- C tree-sitter representation.
(defpackage :software-evolution-library/test/cpp-tree-sitter
  (:nicknames :sel/test/cpp-tree-sitter :sel/test/cpp-ts)
  (:use
    :gt/full
    :software-evolution-library/test/util
    :stefil+
    :software-evolution-library
    :software-evolution-library/software/parseable
    :software-evolution-library/software/tree-sitter
    :software-evolution-library/software/cpp
    :software-evolution-library/test/util-clang
    :software-evolution-library/components/file
    :software-evolution-library/components/formatting)
  (:import-from :software-evolution-library/software/tree-sitter
                :inner-declarations
                :outer-declarations
                :contextualize-ast
                :canonicalize-type)
  (:import-from :software-evolution-library/software/tree-sitter
                :explicit-namespace-qualifiers)
  (:export :test-cpp-tree-sitter))
(in-package :software-evolution-library/test/cpp-tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-cpp-tree-sitter "C++ tree-sitter representation."
  (cpp-tree-sitter-available-p))


;;; Utility

(defixture trim-front
    (:setup
     (setf *soft*
           (from-file 'cpp
                      (asdf:system-relative-pathname
                       :software-evolution-library
                       "test/etc/cpp-fragments/trim_front.cc"))))
  (:teardown (nix *soft*)))


;;; Analysis tests

(deftest test-scopes ()
  (let* ((c (sel:from-string (make 'cpp) (fmt "~
int main () {
  int x = 1;
  int z;
  y();
}")))
         (scopes (scopes c (find-if (of-type 'call-ast) c)))
         (bindings (apply #'append scopes))
         (x-binding (find "x" bindings
                          :test #'equal
                          :key {assocdr :name}))
         (z-binding (find "z" bindings
                          :test #'equal
                          :key {assocdr :name})))
    (is x-binding)
    (is z-binding)
    (is (equal "1" (source-text (rhs (assocdr :decl x-binding)))))))

(deftest test-cpp-function-name ()
  (is (equal "trim_front"
             (function-name (cpp "std::list<Point> trim_front() {}")))))

(deftest test-cpp-function-outer-declaration ()
  (is (equal "trim_front"
             (source-text
              (only-elt
               (outer-declarations
                (cpp-declarator (cpp "std::list<Point> trim_front() {}"))))))))

(deftest test-qualified-parameter-name ()
  (is (member "pts"
              (inner-declarations
               (cpp "fn(std::list<Point>& pts) {}"))
              :test #'equal
              :key #'source-text)))

(deftest test-types-not-in-parameter-names ()
  "Test that identifiers from type declarations are not included as parameter names."
  (is (equal "pts"
             (source-text
              (first
               (parameter-names
                (find-if (of-type 'parameter-ast)
                         (cpp "trim_front(std::list<Point>& pts) {}"))))))))

(deftest test-single-namespace-qualifier ()
  (let* ((cpp (from-string 'cpp "A::x;"))
         (id (find-if (of-type 'cpp-qualified-identifier) (genome cpp))))
    (is (equal '("A")
               (mapcar #'source-text
                       (explicit-namespace-qualifiers id))))))

(deftest test-multiple-explicit-namespace-qualifiers ()
  (let* ((cpp (from-string 'cpp "A::B::C::x;"))
         (id (find-if (of-type 'cpp-qualified-identifier) (genome cpp))))
    (is (equal '("A" "B" "C")
               (mapcar #'source-text
                       (explicit-namespace-qualifiers id))))))

(deftest test-multiple-explicit-namespace-qualifiers-same-name ()
  (let* ((cpp (from-string 'cpp "A::A::x;"))
         (id (find-if (of-type 'cpp-qualified-identifier) (genome cpp))))
    (is (equal '("A" "A")
               (mapcar #'source-text
                       (explicit-namespace-qualifiers id))))))

(deftest test-global-namespace-qualifier ()
  (let* ((cpp (from-string 'cpp "::x;"))
         (id (find-if (of-type 'cpp-qualified-identifier) (genome cpp)))
         (qualifiers (explicit-namespace-qualifiers id)))
    (is (eql :global (only-elt qualifiers)))))

(deftest test-namespace-qualify-1 ()
  (let* ((cpp
          (from-string 'cpp
                       "namespace A {
  int x = 0;

  int f () {
    return A::x;
  }
}"))
         (qid (find-if (of-type 'cpp-qualified-identifier) (genome cpp))))
    (is (string*= "x = 0" (source-text (get-declaration-ast cpp qid))))))

(deftest test-namespace-qualify-2 ()
  (let* ((cpp
          (from-string 'cpp
                       "namespace A {
  int x = 0;

  namespace B {
    int f () {
      return A::x;
    }
  }
}"))
         (qid (find-if (of-type 'cpp-qualified-identifier) (genome cpp))))
    (is (string*= "x = 0" (source-text (get-declaration-ast cpp qid))))))

(deftest test-namespace-qualify-3 ()
  (let* ((cpp
          (from-string 'cpp
                       "int x = 1;

  namespace A {
    int x = 2;
    int f () {
      return ::x;
    }
  }
}"))
         (qid (find-if (of-type 'cpp-qualified-identifier) (genome cpp))))
    (is (string*= "x = 1" (source-text (get-declaration-ast cpp qid))))))

(deftest test-namespace-deepest-match ()
  "Check that we return the deepest matching namespace."
  (let* ((cpp
          (from-string 'cpp
                       "namespace A {
  int x = 0;

  namespace B {
    namespace A {
      namespace B {
        int x = 1;
        int f () {
          return A::B::x;
        }
      }
    }
  }
}"))
         (qid (find-if (of-type 'cpp-qualified-identifier) (genome cpp))))
    (is (string*= "x = 1" (source-text (get-declaration-ast cpp qid))))))

(def +trim-front-types+
  '(("trim_front" . "std::list<Point>")
    ("pts" . "std::list<Point>")
    ;; TODO Should this be const float?
    ("dist" . "float")
    ("result" . "std::list<Point>")
    ("d" . "double")
    ("p1" . "auto")
    ("p2" . "auto")
    ("segdist" . "double")
    ("frac" . "double")
    ("midpoint" . "auto"))
  "The types extracted from the trim_front example.")

(deftest test-trim-front-scopes ()
  "Test that we get all and only the scopes we want."
  (with-fixture trim-front
    (let ((wanted-names
           (convert 'set (mapcar #'car +trim-front-types+)))
          (scope-names
           (convert 'set (mapcar {aget :name} (all-scopes *soft*)))))
      (is (empty? (set-difference scope-names wanted-names)))
      (is (empty? (set-difference wanted-names scope-names))))))

(labels ((last-ids ()
           "Get the last occurrence of each name."
           (remove-duplicates
            (collect-if (of-type 'cpp-identifier)
                        (genome *soft*))
            :from-end nil
            :key #'source-text
            :test #'equal))
         (accesses ()
           "Filter out everything whose name that isn't in the
         result alist."
           (filter (lambda (id)
                     (member (source-text id) +trim-front-types+
                             :key #'car
                             :test #'equal))
                   (last-ids))))

  (deftest test-trim-front-decls ()
    "Test that we get the right declaration for each identifier."
    ;; Test that we have all and only the above identifiers.
    (with-fixture trim-front
      (let* ((all-scopes (all-scopes *soft*)))
        (iter (for access in (accesses))
              (for decl = (get-declaration-ast *soft* access))
              (for string = (source-text access))
              (iter (for scope in all-scopes)
                    (when (equal string (aget :name scope))
                      (let ((scope-decl (aget :decl scope)))
                        (is (or (eql decl scope-decl)
                                (descendant-of-p *soft* decl scope-decl))))))))))

  (deftest test-trim-front-types ()
    "Test that we retrieve the correct type for each identifier."
    (with-fixture trim-front
      ;; Get the type for each access.
      (iter (for access in (accesses))
            (for access-source-text = (source-text access))
            (let ((reference-type
                   (assure string
                     (aget access-source-text
                           +trim-front-types+
                           :test #'equal)))
                  (extracted-type
                   (assure (or null ast)
                     (infer-type *soft* access))))
              (is (string= reference-type (source-text extracted-type))
                  "Mismatch for ~a: should be ~s, got ~s"
                  access-source-text
                  reference-type extracted-type))))))

(deftest test-expression-type ()
  (is (equal "double"
             (source-text
              (expression-type
               (find-if (of-type 'call-ast)
                        (cpp "static_cast<double>(x);"))))))
  (let ((cpp (from-string 'cpp "1.0 + 2.0f;")))
    (is (equal "double"
               (source-text
                (infer-type cpp
                            (find-if (of-type 'cpp-binary-expression) cpp))))))
  (let ((cpp (from-string 'cpp "1 + 2.0f;")))
    (is (equal "float"
               (source-text
                (infer-type cpp
                            (find-if (of-type 'cpp-binary-expression) cpp)))))))

(deftest test-infer-expression-type ()
  (let ((cpp (from-string 'cpp "int x = fn(b);")))
    (is (equal "int"
               (source-text
                (infer-expression-type cpp
                                       (lastcar
                                        (collect-if (of-type 'expression-ast)
                                                    (genome cpp))))))))
  (with-fixture trim-front
    (is (equal "double"
               (source-text
                (infer-expression-type *soft*
                                       (find-if
                                        (lambda (ast)
                                          (and (typep ast 'expression-ast)
                                               (equal (source-text ast)
                                                      "(dist - d) / segdist")))
                                        (genome *soft*))))))))


;;; Parsing tests

(deftest test-reference-return ()
  (is (equal "foo"
             (function-name
              (convert 'cpp-ast "int& foo() {}" :deepest t))))
  (let* ((file (asdf:system-relative-pathname
                "software-evolution-library"
                "test/etc/cpp-tree-sitter/reference.cc"))
         (software (sel:from-file 'cpp file))
         (fn (find-if (of-type 'function-ast) (genome software))))
    (is (equal "FileDescriptorTables::GetEmptyInstance"
               (function-name fn)))))

(deftest function-name-on-cpp-tree-sitter ()
  (let ((root (convert 'cpp-ast "void z::y::z() { return; }")))
    (is (equal "z::y::z"
               (function-name (find-if (of-type 'function-ast) root))))))

(deftest preprocessor-test-1 () ;; address sel issue 136
  "Ensure that #ifndef is not converted to #ifdef"
  (let ((*soft* (from-string (make-instance 'cpp) 
"#ifndef BSD4_1
#define HAVE_GETPAGESIZE
#endif
")))
    (is (typep *soft* 'cpp))
    (is (typep (genome *soft*) 'cpp-translation-unit))
    (is (typep (elt (children (genome *soft*)) 0) 'cpp-preproc-ifdef))
    (is (starts-with-subseq
         "#ifndef" (source-text (elt (children (genome *soft*)) 0))))))

(deftest preprocessor-test-2 () ;; address sel issue 136
  "Ensure that #ifdef is not converted to #ifndef"
  (let ((*soft* (from-string (make-instance 'cpp) 
"#ifdef BSD4_1
#define HAVE_GETPAGESIZE
#endif
")))
    (is (typep *soft* 'cpp))
    (is (typep (genome *soft*) 'cpp-translation-unit))
    (is (typep (elt (children (genome *soft*)) 0) 'cpp-preproc-ifdef))
    (is (starts-with-subseq
         "#ifdef" (source-text (elt (children (genome *soft*)) 0))))))

(deftest compound-operator-test-1 () ;; sel issue #137
  (let ((*soft* (from-string (make-instance 'cpp) 
"{ int x = 1; 
   x *= 4; 
   x += 2; 
   x -= 1; 
   x /= 5; }")))
    (is (typep *soft* 'cpp))
    (let ((g (genome *soft*)))
      (is (typep g 'cpp-translation-unit))
      (is (typep (@ g 0) 'cpp-compound-statement))
      (is (typep (@ g '(0 1)) 'cpp-expression-statement))
      (is (typep (@ g '(0 1 0)) 'cpp-assignment-expression))
      (is (string-equal
           (source-text (@ g '(0 1 0 1)))
           "*="))
      (is (string-equal
           (source-text (@ g '(0 2 0 1)))
           "+="))
      (is (string-equal
           (source-text (@ g '(0 3 0 1)))
           "-="))
      (is (string-equal
           (source-text (@ g '(0 4 0 1)))
           "/=")))))
      
(deftest field-expression-test-1 () ;; sel issue #142
  "Ensure that '.' and '->' are handled correctly"
  (let ((*soft* (from-string (make-instance 'cpp) 
"int main ()
{
    typedef struct { int f1; int f2; } X;
    X a[3];
    int i;
    X *p = a;
    p->f1 = 1;
    i = p->f1;
    return i;
}
")))
    (is (typep *soft* 'cpp))
    (let ((g (genome *soft*)))
      (is (typep g 'cpp-translation-unit))
      (is (typep (@ g 0) 'cpp-function-definition))
      (is (typep (@ g '(0 2)) 'cpp-compound-statement))
      (is (typep (@ g '(0 2 4)) 'cpp-expression-statement))
      (is (typep (@ g '( 0 2 4 0)) 'cpp-assignment-expression))
      (is (typep (@ g '(0 2 4 0 0)) 'cpp-field-expression))
      (is (string-equal (source-text  (@ g '(0 2 4 0 0 1)))
                        "->"))
      (is (typep (@ g '(0 2 5)) 'cpp-expression-statement))
      (is (typep (@ g '(0 2 5 0)) 'cpp-assignment-expression))
      (is (typep (@ g '(0 2 5 0 2)) 'cpp-field-expression))
      (is (string-equal (source-text  (@ g '(0 2 5 0 2 1)))
                        "->")))))


;;;; Rule Substitution tests
;;; These tests that the rule substitutions are working as intended.
;;; These help identify issues when tree-sitter libs are updated upstream.
;;; NOTE: these should be incorporated into a parsing test when one is added
;;;       for Cpp.

(deftest cpp-field-expression-rule-substitution ()
  "The field-expression substitution parses and reproduces source text."
  (let ((source "int a = (x->y < u && z >= w);"))
    (is (equal source (source-text (convert 'c-ast source))))))

(deftest cpp--constructor-specifiers-substitution ()
  "The substitution for -constructor-specifiers parses and
reproduces source text."
  (let ((source "
class MyF
    : public MapF<x::TestMapF, int32,
                      int32, X::Y::Z,
                      X::Y::Z> {
 public:
  constexpr MyF()
      : MyF::MapF(X::Y{}) {}};"))
    (is (equal source (source-text (convert 'cpp-ast source))))))

(deftest cpp-labeled-statement-rule-substitution ()
  (let ((labeled-statement (convert 'cpp-ast "label: break;" :deepest t)))
    (is (equal "label" (source-text (cpp-label labeled-statement))))
    (is (equal "break;" (source-text (cpp-statement labeled-statement))))))

(deftest cpp-for-statement-rule-substitution ()
  (let* ((source "void foo() { for (int i = 0; i<5; i++) { i=i+2; } }")
         (root (convert 'cpp-ast source))
         (for-statement (stmt-with-text root "for" :at-start t)))
    (is (typep (body for-statement) 'compound-ast))))

(deftest cpp-case-statement-rule-substitution ()
  (let* ((source "void foo(int i) { switch (i) { case 1: i++; break; } }")
         (root (convert 'cpp-ast source))
         (case-statement (stmt-with-text root "case" :at-start t)))
    (is (= 2 (length (cpp-statements case-statement))))))

(defun can-parse (lang string)
  (declare (optimize debug))
  (let* ((genome
          (finishes
           (genome (from-string lang string))))
         (new-text (source-text genome)))
    (is (equal string new-text))))

(deftest test-cpp-operator-name ()
  (can-parse 'cpp "bool operator+() {}")
  (can-parse 'cpp "bool operator<() {}")
  (can-parse 'cpp "bool operator>() {}"))

(deftest test-cpp-field-expression ()
  (can-parse 'cpp "config.put();")
  (can-parse 'cpp "config.put<int>();"))

(deftest test-cpp-virtual-destructor ()
  (can-parse 'cpp "virtual ~NoCost() {}")
  (can-parse 'cpp "class TruckCost : public DynamicCost {
public:

  virtual ~TruckCost();

};"))

(deftest test-cpp-explicit ()
  (can-parse 'cpp "struct B
{
    explicit B(int) { }
    explicit B(int, int) { }
    explicit operator bool() const { return true; }
};"))

(deftest test-cpp-virtual-const-override ()
  (can-parse 'cpp "virtual bool Allowed() const override {}")
  (can-parse 'cpp "virtual bool Allowed() const {}")
  (can-parse 'cpp "virtual bool Allowed() override {}")
  (can-parse 'cpp "virtual bool Allowed() {}"))

(deftest test-cpp-virtual-method-declaration ()
  (can-parse 'cpp "class TruckCost : public DynamicCost {
public:

  virtual bool AllowTransitions() const;
};"))

(deftest test-preserve-access-specifier-keyword ()
  "Private and protected keywords used to become public keywords."
  (can-parse 'cpp "class myclass {
private:
  const std::vector<char> memory_;
};")
  (can-parse 'cpp "class myclass {
protected:
  const std::vector<char> memory_;
};"))

(deftest test-cpp-preserve-unsigned ()
  "Unsigned used to sometimes become signed."
  (can-parse 'cpp "const std::unordered_map<unsigned, valhalla::valhalla_exception_t> error_codes{}")
  (can-parse 'cpp "s.avail_in = static_cast<unsigned int>(uncompressed.size() * sizeof(std::string::value_type));"))

(deftest test-cpp-preserve-const-in-for ()
  "Check that const doesn't disappear in the binding of a for loop."
  (can-parse 'cpp "for (const auto& location : locations) {}"))

(deftest test-cpp-preserve-const-in-optional-param ()
  ;; The first const was disappearing.
  (can-parse 'cpp "const boost::property_tree::ptree&
configure(const boost::optional<std::string>& config = boost::none) {}")
  ;; The second const was disappearing.
  (can-parse 'cpp "virtual void Log(const std::string& message, const std::string& custom_directive = \" [TRACE] \") {}"))

(deftest test-preserve-class-vs-typename ()
  "This was turning class into typename."
  (can-parse 'cpp
             "template <class T> T clamp(T val, const T low, const T high) {
  return std::min<T>(std::max<T>(val, low), high);
}"))

(deftest test-preserve-ref-ref ()
  "Two ampersands were being collapsed into one."
  (can-parse 'cpp "VectorGraphMemory(std::vector<char>&& memory) : memory_(std::move(memory)) {}"))

(deftest test-preserve-operator-cast-specifiers ()
  (can-parse 'cpp
             "struct edge_t {
  GraphId i;
  const DirectedEdge* e;
  operator const GraphId&() const {
    return i;
  }
  operator const DirectedEdge*() const {
    return e;
  }
  operator bool() const {
    return i.Is_Valid() && e;
  }
};"))

(deftest test-preserve-valueness-reference-field-declarator ()
  (can-parse 'cpp "class X {
  const Obj&
};"))

;;; TODO
#+(or)
(deftest test-cpp-stray-comma ()
  "A comma after the last element in a list gets displaced to the next
line, after a comment if there is one."
  ;; From creduce.
  (can-parse 'cpp "::{
               {
     }
      ,     };
")
  (can-parse 'cpp "for (auto* cache : std::vector<x>{
           new y,
       }) {}")
  (can-parse 'cpp "const gurka::ways ways = {
      {\"AB\", {{\"highway\", \"service\"}}},
  };")
  ;; In this one the comma ends up after the comment.
  (can-parse 'cpp "const constexpr PointLL::first_type DOUGLAS_PEUCKER_THRESHOLDS[19] = {
    2.6,      // z18
};
"))

#+(or)
(deftest test-weird-indent ()
  ;; The body gets too much indentation.
  (can-parse 'cpp "void fun() {
  for (int i = 0;
       i < 100; i++) {
    foo();
  }
}")
  ;; Only the first else-if clause gets indented.
  (can-parse 'cpp "OpenLR::LocationReferencePoint::FormOfWay get_fow(const baldr::DirectedEdge* de) {
  if (de->classification() == valhalla::baldr::RoadClass::kMotorway)
    return OpenLR::LocationReferencePoint::MOTORWAY;
  else if (de->roundabout())
    return OpenLR::LocationReferencePoint::ROUNDABOUT;
  else if (de->use() == valhalla::baldr::Use::kRamp ||
           de->use() == valhalla::baldr::Use::kTurnChannel)
    return OpenLR::LocationReferencePoint::SLIPROAD;
  else if ((de->forwardaccess() & kVehicularAccess) && (de->reverseaccess() & kVehicularAccess))
    return OpenLR::LocationReferencePoint::MULTIPLE_CARRIAGEWAY;
  else if ((de->forwardaccess() & kVehicularAccess) || (de->reverseaccess() & kVehicularAccess))
    return OpenLR::LocationReferencePoint::SINGLE_CARRIAGEWAY;

  return OpenLR::LocationReferencePoint::OTHER;
}"))


;;; Contextualize-ast Tests
(defun contextualization-check (source target-ast-type
                                &key result-type unexpected-type context-table
                                  (target-ast-position 0))
  (let* ((root (convert 'cpp-ast source))
         (software (make-instance 'cpp :genome root))
         (target-ast
           (nth
            target-ast-position
            (collect-if (of-type target-ast-type) root)))
         (result (contextualize-ast software target-ast context-table))
         (result-root (mapcar (lambda (ast)
                                (if (eq ast target-ast)
                                    result
                                    ast))
                              root)))
    (is (equal (source-text target-ast) (source-text result)))
    (is (equal source (source-text result-root)))
    (when result-type
      (is (typep result result-type)))
    (when unexpected-type
      (is (not (find-if (of-type unexpected-type) result))))))

(deftest cpp-contextualize-function-declarator-1 ()
  "Contextualize-ast turns a function-declarator into an init declarator and
doesn't contain any abstract function parameters or type identifiers."
  (contextualization-check
   "void f () {
  Obj object(a, X(b, Y<1,2>()), Y());
}"
   'cpp-function-declarator
   :target-ast-position 1
   :result-type 'cpp-init-declarator
   :unexpected-type '(or cpp-abstract-function-declarator cpp-type-identifier)))

(deftest cpp-contextualize-function-declarator-2 ()
  "Contextualize-ast doesn't alter declarations that are part of a function
definition (excluding the body)."
  (contextualization-check
   "void f () {
  Obj object(a, X(b, Y<1,2>()), Y());
}"
   'cpp-function-declarator
   :target-ast-position 0
   :result-type 'cpp-function-declarator
   :unexpected-type 'cpp-init-declarator))

(deftest cpp-contextualize-function-declarator-3 ()
  "Contextualize-ast doesn't alter declarations that have trailing specifiers."
  (contextualization-check
   "void f () {
  Obj object(a, X(b, Y<1,2>()), Y()) override;
}"
   'cpp-function-declarator
   :target-ast-position 1
   :result-type 'cpp-function-declarator
   :unexpected-type 'cpp-init-declarator))

(deftest cpp-contextualize-function-declarator-4 ()
  "Contextualize-ast doesn't alter declarations that have a parameter with a type
and an identifier."
  (contextualization-check
   "void f () {
  Obj object(int a, X(b, Y<1,2>()), Y());
}"
   'cpp-function-declarator
   :target-ast-position 1
   :result-type 'cpp-function-declarator
   :unexpected-type 'cpp-init-declarator))

(deftest cpp-contextualize-function-declarator-5 ()
  "Contextualize-ast doesn't alter declarations that have an optional parameter."
  (contextualization-check
   "void f () {
  Obj object(a, int b = 0);
}"
   'cpp-function-declarator
   :target-ast-position 1
   :result-type 'cpp-function-declarator
   :unexpected-type 'cpp-init-declarator))

(deftest cpp-contextualize-function-declarator-6 ()
  "Contextualize-ast doesn't alter declarations that have a parameter with
specializers."
  (contextualization-check
   "void f () {
  Obj object(const a);
}"
   'cpp-function-declarator
   :target-ast-position 1
   :result-type 'cpp-function-declarator
   :unexpected-type 'cpp-init-declarator))

(deftest cpp-contextualize-function-declarator-7 ()
  "Contextualize-ast doesn't alter declarations that are in a class definition."
  (contextualization-check
   "class Obj {
  Obj get_obj();
};"
   'cpp-function-declarator
   :result-type 'cpp-function-declarator
   :unexpected-type 'cpp-init-declarator))

(deftest cpp-contextualize-function-declarator-context-1 ()
  "Contextualize-ast turns a function-declarator into an init declarator when
it doesn't contain any valid types."
  (contextualization-check
   "Obj object(a, x = 10);"
   'cpp-function-declarator
   :context-table (dict "x" :type)
   :result-type 'cpp-init-declarator
   :unexpected-type '(or cpp-abstract-function-declarator
                      cpp-type-identifier cpp-optional-parameter-declaration)))

(deftest cpp-contextualize-binary-expression-1 ()
  "Contextualize-ast turns a binary expression into a cast expression when
the left hand side is a parenthesized identifier and doesn't contain
parenthesized expressions or binary expressions."
  (contextualization-check
   "(Type) * variable;"
   'cpp-binary-expression
   :result-type 'cpp-cast-expression
   :unexpected-type '(or cpp-binary-expression cpp-parenthesized-expression)))

(deftest cpp-contextualize-binary-expression-2 ()
  "Contextualize-ast maintains the source representation of a binary expression
when it is contextualized."
  (contextualization-check "(Type) * variable ;" 'cpp-binary-expression))

(deftest cpp-contextualize-binary-expression-3 ()
  "Contextualize-ast does nothing when the parent AST is a sizeof expression."
  ;; NOTE: this should be a temporary fix until precedence issues are addressed
  ;;       upstream.
  (contextualization-check
   "sizeof(Type) * variable ;"
   'cpp-binary-expression
   :result-type 'cpp-binary-expression
   :unexpected-type 'cpp-cast-expression))

(deftest cpp-contextualize-binary-expression-context-1 ()
  "Contextualize-ast turns a binary expression into a cast expression when
the left hand side is a type and the binary operator is also a valid prefix
operator."
  (contextualization-check
   "(Type) * variable;"
   'cpp-binary-expression
   :context-table (dict "Type" :type)
   :result-type 'cpp-cast-expression
   :unexpected-type '(or cpp-binary-expression cpp-parenthesized-expression)))


;;; Canonicalize-type Tests
(defmacro with-canonicalize-type-test
    ((source &key (target-ast-type 'cpp-declaration))
     &body body)
  `(let* ((root (convert 'cpp-ast ,source))
          (target-ast (find-if (of-type ',target-ast-type) root))
          (result (canonicalize-type target-ast))
          (declarator-list (aget :declarator result))
          (specifier-list (aget :specifier result))
          (bitfield-list (aget :bitfield result)))
     (declare (ignorable declarator-list specifier-list bitfield-list))
     (labels ((test-declarator-type (key type)
                "Test that the value associated with KEY is of TYPE."
                (is (equal (find-if (of-type type) root)
                           (car (aget key declarator-list)))))
              (test-bitfield-type (type)
                "Test that the AST in the bitfield list is of TYPE."
                (is (equal (find-if (of-type type) root)
                           (car bitfield-list)))))
       (declare (ignorable (function test-declarator-type)
                           (function test-bitfield-type)))
       ,@body)))

(deftest cpp-canonicalize-type-1 ()
  "Canonicalize-type returns the size in the alist of :declarator."
  (with-canonicalize-type-test ("int x [100];")
    (test-declarator-type :array 'cpp-number-literal)))

(deftest cpp-canonicalize-type-2 ()
  "Canonicalize-type returns the parameter list in the alist of :declarator."
  (with-canonicalize-type-test ("int x (int, int);")
    (test-declarator-type :function 'cpp-parameter-list)))

(deftest cpp-canonicalize-type-3 ()
  "Canonicalize-type returns the qualifiers in the alist of :declarator."
  (with-canonicalize-type-test ("const int *x;")
    (test-declarator-type :pointer 'cpp-const)))

(deftest cpp-canonicalize-type-4 ()
  "Canonicalize-type returns works on a field declaration."
  (with-canonicalize-type-test ("struct s { const int x : 4; };"
                                :target-ast-type cpp-field-declaration)
    (test-bitfield-type 'cpp-number-literal)
    (is (equal (source-text (car specifier-list))
               "const"))))

(deftest cpp-canonicalize-type-specifier-list-1 ()
  "Canonicalize-type returns the implicit 'int' in the specifier list."
  (with-canonicalize-type-test ("long long x;")
    (is (equal (source-text (find-if (of-type 'cpp-primitive-type)
                                     specifier-list))
               "int"))))

(deftest cpp-canonicalize-type-specifier-list-2 ()
  "Canonicalize-type removes the 'signed' qualifier from the specifier list."
  (with-canonicalize-type-test ("signed long long x;")
    (is (not (find-if (of-type 'cpp-signed) specifier-list)))))
