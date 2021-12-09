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
                :outer-declarations)
  (:import-from :software-evolution-library/software/tree-sitter
                :explicit-namespace-qualifiers)
  (:export :test-cpp-tree-sitter))
(in-package :software-evolution-library/test/cpp-tree-sitter)
(in-readtable :curry-compose-reader-macros)
(defsuite test-cpp-tree-sitter "C tree-sitter representation."
  (cpp-tree-sitter-available-p))

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
