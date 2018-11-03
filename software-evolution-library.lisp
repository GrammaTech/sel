;;; software-evolution-library.lisp --- Extant Software Evolution
(defpackage :software-evolution-library
  (:nicknames :sel)
  (:use
   :alexandria
   :closer-mop
   :uiop
   :bordeaux-threads
   :common-lisp
   :arrow-macros
   :cl-ppcre
   :cl-store
   :named-readtables
   :curry-compose-reader-macros
   :diff
   :elf
   :iterate
   :metabang-bind
   :split-sequence
   :software-evolution-library/utility
   :software-evolution-library/serapi-io
   :software-evolution-library/ast-diff
   :usocket
   :fast-io
   :trace-db)
  (:shadow :elf :size :type :magic-number :diff :insert :index)
  (:shadowing-import-from :software-evolution-library/utility :quit)
  (:shadowing-import-from :uiop :getenv :directory-exists-p)
  (:shadowing-import-from :iterate :iter :for :until :collecting :in)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:shadowing-import-from
   :alexandria
   :appendf :ensure-list :featurep :emptyp
   :if-let :ensure-function :ensure-gethash :copy-file :copy-stream
   :parse-body :simple-style-warning)
  (:export
   :+software-evolution-library-dir+
   :+software-evolution-library-version+
   :+software-evolution-library-branch+
   ;; software objects
   :software
   :define-software
   :edits
   :fitness
   :fitness-extra-data
   :mutation-stats
   :*mutation-improvements*
   :genome
   :phenome
   :phenome-p
   :ignore-phenome-errors
   :return-nil-for-bin
   :retry-project-build
   :evaluate
   :copy
   :size
   :lines
   :line-breaks
   :genome-string
   :tokens
   :headers
   :macros
   :includes
   :types
   :globals
   :ancestral
   :ancestors
   :pick
   :pick-good
   :pick-bad
   :pick-snippet
   :pick-guarded-compound
   :mutation-targets
   :good-mutation-targets
   :bad-mutation-targets
   :mutate
   :no-mutation-targets
   :pick-mutation-type
   :parseable-mutation
   :parseable-insert
   :parseable-swap
   :parseable-move
   :parseable-cut
   :clang-mutation
   :javascript-mutation
   :build-op
   :apply-mutation-ops
   :apply-mutation
   :apply-mutations
   :apply-all-mutations
   :apply-picked-mutations
   :text
   :obj
   :op
   :*mutation-stats*
   :*crossover-stats*
   :analyze-mutation
   :mutation-key
   :summarize-mutation-stats
   :classify
   :crossover
   :one-point-crossover
   :two-point-crossover
   :*edit-consolidation-size*
   :*consolidated-edits*
   :*edit-consolidation-function*
   :edit-distance
   :from-file
   :from-file-exactly
   :from-string
   :apply-config
   :ext
   :get-vars-in-scope
   :bind-free-vars
   :prepare-sequence-snippet
   :prepare-inward-snippet
   :create-inward-snippet
   :crossover-2pt-inward
   :crossover-2pt-outward
   :intraprocedural-2pt-crossover
   :select-crossover-points
   :function-containing-ast
   :function-body-p
   :function-decl-p
   :adjust-stmt-range
   :random-point-in-function
   :select-intraprocedural-pair
   :clang-mutate
   :update-headers-from-snippet
   :to-file
   :apply-path
   :expression
   :expression-intern
   :expression-to-c
   :mutation
   :define-mutation
   :compose-mutations
   :sequence-mutations
   :object
   :targeter
   :picker
   :targets
   :get-targets
   :at-targets
   :compiler
   :prototypes
   :functions
   :get-entry
   :asts
   :stmt-asts
   :non-stmt-asts
   :good-stmts
   :bad-stmts
   :update-asts
   :source-location
   :line
   :column
   :asts-containing-source-location
   :asts-contained-in-source-range
   :asts-intersecting-source-range
   :ast-to-source-range
   :get-ast
   :get-parent-ast
   :get-parent-asts
   :parent-ast-p
   :get-parent-full-stmt
   :wrap-ast
   :wrap-child
   :can-be-made-traceable-p
   :traceable-stmt-p
   :get-make-parent-full-stmt
   :get-immediate-children
   :extend-to-enclosing
   :get-ast-info
   :+c-numeric-types+
   :+c-relational-operators+
   :+c-arithmetic-binary-operators+
   :+c-arithmetic-assignment-operators+
   :+c-bitwise-binary-operators+
   :+c-bitwise-assignment-operators+
   :+c-arithmetic-unary-operators+
   :+c-bitwise-unary-operators+
   :+c-sign-unary-operators+
   :+c-pointer-unary-operators+
   :ast-declarations
   :declared-type
   :find-var-type
   :typedef-type
   :random-function-name
   :replace-fields-in-snippet
   ;; global variables
   :*population*
   :*generations*
   :*max-population-size*
   :*tournament-size*
   :*tournament-eviction-size*
   :*fitness-predicate*
   :fitness-better-p
   :fitness-equal-p
   :*cross-chance*
   :*mut-rate*
   :*fitness-evals*
   :*running*
   :*start-time*
   :elapsed-time
   ;; simple / asm global variables
   :*simple-mutation-types*
   :*asm-linker*
   :*asm-mutation-types*
   ;; adaptive software
   :adaptive-mutation
   :*bias-toward-dynamic-mutation*
   :*better-bias*
   :*same-bias*
   :*worse-bias*
   :*dead-bias*
   :adaptive-analyze-mutation
   :update-mutation-types
   ;; clang / clang-w-fodder global variables
   :searchable
   :fodder-database
   :in-memory-database
   :json-database
   :pliny-database
   :db
   :host
   :port
   :database-emptyp
   :source-collection
   :cache-collection
   :middle-host
   :middle-port
   :find-snippets
   :weighted-pick
   :find-type
   :find-macro
   :similar-snippets
   :*clang-max-json-size*
   :*crossover-function-probability*
   :*clang-mutation-types*
   :*clang-w-fodder-mutation-types*
   :*clang-w-fodder-new-mutation-types*
   :*free-var-decay-rate*
   :*matching-free-var-retains-name-bias*
   :*matching-free-function-retains-name-bias*
   :*allow-bindings-to-globals-bias*
   :*clang-json-required-fields*
   :*clang-json-required-aux*
   :*clang-ast-aux-fields*
   :*clang-mutate-additional-args*
   :*database*
   :*mmm-processing-seconds*
   ;; evolution functions
   :incorporate
   :evict
   :default-select-best
   :default-random-winner
   :*tournament-selector*
   :*tournament-tie-breaker*
   :*tie-breaker-predicate*
   :tournament
   :mutant
   :crossed
   :new-individual
   :mcmc
   :mcmc-step
   :*mcmc-fodder*
   :evolve
   :generational-evolve
   ;; software backends
   :simple
   :light
   :sw-range
   :diff
   :original
   :asm
   :asm-heap
   :asm-line-info
   :asm-line-info-text
   :asm-line-info-tokens
   :asm-line-info-type
   :asm-line-info-label
   :asm-line-info-opcode
   :asm-line-info-operands
   :asm-line-info-id
   :asm-line-info-orig-file
   :asm-line-info-orig-line
   :csurf-asm
   :*dynamic-linker-path*
   :*elf-copy-redirect-path*
   :*elf-edit-symtab-path*
   :elf
   :elf-cisc
   :elf-csurf
   :elf-x86
   :elf-arm
   :elf-risc
   :elf-mips
   :genome-bytes
   :pad-nops
   :nop-p
   :forth
   :lisp
   :constant-fold
   :random-subtree
   :source
   :parseable
   :clang
   :clang-w-fodder
   :clang-w-binary
   :clang-w-fodder-and-binary
   :bytes
   :diff-data
   :recontextualize
   :parse-source-snippet
   :delete-decl-stmts
   :rename-variable-near-use
   :run-cut-decl
   :run-swap-decls
   :run-rename-variable
   :common-ancestor
   :ancestor-of
   :reset-ancestry-id
   :get-fresh-ancestry-id
   :save-ancestry
   :scopes-between
   :nesting-depth
   :full-stmt-p
   :block-p
   :guard-p
   :enclosing-full-stmt
   :enclosing-block
   :nesting-relation
   :match-nesting
   :block-predeccessor
   :block-successor
   :show-full-stmt
   :full-stmt-info
   :prepare-code-snippet
   :get-children-using
   :get-declared-variables
   :cil
   :java
   :javascript
   :llvm
   :linker
   :flags
   :addr-map
   :assembler
   :asm-flags
   :redirect-file
   :linker-script
   :weak-symbols
   :linked-files
   :elf-risc-max-displacement
   :ops                      ; <- might want to fold this into `lines'
   ;; software backend specific methods
   :reference
   :base
   :disasm
   :addresses
   :instrumented-p
   :instrument
   :instrumenter
   :clang-instrumenter
   :java-instrumenter
   :javascript-instrumenter
   :uninstrument
   :var-instrument
   :+instrument-log-variable-name+
   :+instrument-log-lock-variable-name+
   :add-include
   :force-include
   :add-type
   :find-or-add-type
   :type-decl-string
   :add-macro
   :prepend-to-genome
   :nullify-asts
   :keep-partial-asts
   :retry-mutation
   :expand-stmt-pool
   :ignore-failed-mutation
   :try-another-mutation
   :fix-compilation
   :generational-evolve
   :simple-reproduce
   :simple-evaluate
   :simple-select
   :*target-fitness-p*
   :*worst-fitness-p*
   :worst-numeric-fitness
   :worst-numeric-fitness-p
   :*fitness-scalar-fn*
   :fitness-scalar
   :lexicase-select
   :lexicase-select-best
   :*lexicase-key*
   :pareto-selector
   :*pareto-comparison-set-size*
   :multi-objective-scalar
   :pick-least-crowded
   :mutation
   :targets
   :simple-cut
   :simple-insert
   :simple-swap
   :asm-replace-operand
   :asm-nth-instruction
   :asm-split-instruction
   :number-genome
   :homologous-crossover
   :clang-cut
   :clang-cut-same
   :clang-cut-full
   :clang-cut-full-same
   :clang-insert
   :clang-insert-same
   :clang-insert-full
   :clang-insert-full-same
   :clang-swap
   :clang-swap-same
   :clang-swap-full
   :clang-swap-full-same
   :clang-move
   :clang-replace
   :clang-replace-same
   :clang-replace-full
   :clang-replace-full-same
   :clang-promote-guarded
   :clang-nop
   :clang-instrument
   :explode-for-loop
   :coalesce-while-loop
   :cut-decl
   :swap-decls
   :rename-variable
   :expand-arithmatic-op
   :replace-fodder-same
   :replace-fodder-full
   :insert-fodder-decl
   :insert-fodder-decl-rep
   :insert-fodder
   :insert-fodder-full
   :pick-bad-good
   :pick-bad-bad
   :pick-bad-only
   :full-stmt-filter
   :same-class-filter
   :*lisp-mutation-types*
   :lisp-cut
   :lisp-replace
   :lisp-swap
   :change-operator
   :change-constant
   :clang-expression
   :scope
   :mult-divide
   :add-subtract
   :subtract-add
   :add-subtract-tree
   :subtract-add-tree
   :add-subtract-scope
   :evaluate-expression
   :demote-binop-left
   :demote-binop-right
   :eval-error
   :project
   :apply-to-project
   :build-command
   :artifacts
   :evolve-files
   :other-files
   :instrumentation-files
   :all-files
   :write-genome-to-files
   :with-build-dir
   :with-temp-build-dir
   :make-build-dir
   :full-path
   :*build-dir*
   :clang-project
   :project-dir
   :compilation-database
   :*process-sleep-interval*
   :*process-kill-timeout*
   :test-suite
   :test-cases
   :test-case
   :program-name
   :program-args
   :start-test
   :finish-test
   :run-test
   :instrumentation-exprs
   :synthesize-condition
   :synthesize-conditions
   :find-best-condition
   :add-condition
   :tighten-condition
   :loosen-condition
   :refine-condition
   :valid-targets
   :if-to-while
   :if-to-while-tighten-condition
   :insert-else-if
   :*abst-cond-attempts*
   :*synth-condition-attempts*
   :stmts-in-file
   :error-funcs
   :rinard
   :rinard-compare
   :rinard-incremental
   :rinard-write-out
   :rinard-read-in
   :collect-fault-loc-traces
   :to-alist
   :from-alist
   :ast
   :ast-path
   :ast-node
   :ast-children
   :make-ast
   :copy-ast
   :make-ast-node
   :copy-ast-node
   :to-ast
   :clang-ast
   :clang-ast-node
   :ast-args
   :ast-children
   :ast-class
   :ast-text
   :ast-counter
   :ast-declares
   :ast-expr-type
   :ast-full-stmt
   :ast-guard-stmt
   :ast-in-macro-expansion
   :ast-includes
   :ast-is-decl
   :ast-macros
   :ast-name
   :ast-opcode
   :ast-ret
   :ast-syn-ctx
   :ast-varargs
   :ast-void-ret
   :ast-array-length
   :ast-base-type
   :ast-bit-field-width
   :ast-aux-data
   :make-clang-ast
   :copy-clang-ast
   :make-clang-ast-node
   :copy-clang-ast-node
   :clang-type
   :type-array
   :type-col
   :type-decl
   :type-file
   :type-hash
   :type-i-col
   :type-i-file
   :type-i-line
   :type-line
   :type-pointer
   :type-const
   :type-volatile
   :type-restrict
   :type-storage-class
   :type-reqs
   :type-name
   :type-size
   :make-clang-type
   :copy-clang-type
   :clang-macro
   :macro-name
   :macro-body
   :macro-hash
   :make-clang-macro
   :copy-clang-macro
   :javascript-ast
   :javascript-ast-node
   :make-javascript-ast
   :copy-javascript-ast
   :make-javascript-ast-node
   :copy-javascript-ast-node
   :roots
   :source-text
   :function-body
   :stmt-range
   :get-ast-types
   :get-unbound-vals
   :get-unbound-funs
   :scopes
   :index-of-ast
   :ast-at-index
   :ast-later-p
   :make-statement
   :make-literal
   :make-operator
   :make-block
   :make-parens
   :make-while-stmt
   :make-for-stmt
   :make-if-stmt
   :make-var-reference
   :make-var-decl
   :make-cast-expr
   :make-call-expr
   :make-array-subscript-expr
   :make-label
   :make-switch-stmt
   :make-break-stmt
   :ast-root
   :replace-in-ast
   :prepare-fodder
   ;; traceable
   :traceable
   :binary-traceable
   :sexp-traceable
   :traces
   :collect-trace
   :collect-traces
   :read-trace-file
   :read-trace-stream
   :type-decl-string
   :type-trace-string
   :type-from-trace-string
   :trace-error
   :bin
   ;; code formatting
   :format-genome
   :astyle
   :clang-tidy
   :clang-format
   :prettier
   ;; style features
   :style-feature
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
   :extract-features
   :extract-baseline-features
   :merge-styleables
   :ast-node-type-tf-extractor
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
   :bi-grams-hashtable-to-feature
   :all-keywords
   :extract-feature
   :update-project-features
   ;; java mutation
   :java-mutation
   :java-insert
   ;; java mutation helper methods
   :java-make-literal
   :java-number
   :java-project
   :get-files-jar
   :super-mutant
   :ast-diff
   :ast-patch
   ;; Coq exports
   :coq
   :coq-project
   :ast-ids
   :project-file
   :file-source
   :imports
   :reset-and-load-imports
   :init-coq-project
   :type-safe-swap
   :tag-loc-info
   :untag-loc-info
   :lookup-source-strings
   :coq-type-checks
   ;; asm-super-mutant
   :asm-super-mutant
   :*lib-papi*
   :fitness-harness
   :load-io-file
   :target-function
   :target-function-name
   :mutants
   :create-all-simple-cut-variants
   :create-target
   :target-info
   :line-heap
   :function-index
   :evaluate-asm
   :leaf-functions))
(in-package :software-evolution-library/software-evolution-library)
(in-readtable :curry-compose-reader-macros)


;;; Software Object
(defvar +software-evolution-library-dir+
  (pathname-directory
   #.(or *compile-file-truename*
         *load-truename*
         *default-pathname-defaults*))
  "Path to directory holding SOFTWARE-EVOLUTION-LIBRARY.")

(defvar +software-evolution-library-version+
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (handler-case
        (current-git-commit +software-evolution-library-dir+)
      (git (e) (declare (ignorable e)) "UNKNOWN")))
  "Current version of the SOFTWARE-EVOLUTION-LIBRARY.")

(defvar +software-evolution-library-branch+
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (handler-case
        (current-git-branch +software-evolution-library-dir+)
      (git (e) (declare (ignorable e)) "UNKNOWN")))
  "Current branch of the SOFTWARE-EVOLUTION-LIBRARY.")

(let ((oid-counter 0))
  (defun generate-oid ()
    "Create a fresh, unique oid (object id) in range [1 ...]"
    (incf oid-counter)))
 
(defclass oid-object (standard-object)
  ((oid :initarg :oid :reader oid :initform (generate-oid)))
  (:documentation
   "Attaches a unique oid (object identifier) to each instance."))

(defclass software (oid-object)
  ((fitness :initarg :fitness :accessor fitness :initform nil))
  (:documentation "Base class for all software objects."))

(defmacro define-software
    (name direct-superclasses direct-slots &rest options)
  "Define a new `software' class NAME including a deep `copy' method.
Arguments NAME DIRECT-SUPERCLASSES and OPTIONS are passed through to
`defclass' unmodified.  Additional optional :COPIER property on each
slot in DIRECT-SLOTS may be one of the following:

* :NONE this slot is not copied and will be nil in the new object
* :DIRECT this slot is copied by direct reference to the slot value
          skipping the accessor
* otherwise if the value of :COPIER is nil (default) then the slot is
            copied through its accessor, otherwise the value is
            assumed to be a function (e.g., `copy-tree') which is used
            to copy the slot."
  ;; Ensure a child of software.
  `(prog1
       ;; Define the class
       (defclass ,name ,(if (member 'software direct-superclasses)
                            direct-superclasses
                            `(,@direct-superclasses software))
         ,(mapcar {plist-drop :copier} direct-slots)
         ,@options)
     ;; Define the copy method
     ,(unless (null direct-slots)
        `(defmethod copy :around ((obj ,name) &key)
                    (let ((copy (call-next-method)))
                      (with-slots ,(mapcar #'car direct-slots) copy
                        (setf
                         ,@(mappend
                            (lambda (accessor copier)
                              (case copier
                                (:none nil)
                                (:direct
                                 `(,accessor (with-slots (,accessor) obj
                                               ,accessor)))
                                (otherwise
                                 `(,accessor
                                   ,(if copier
                                        `(,copier (,accessor obj))
                                        `(,accessor obj))))))
                            (mapcar #'car direct-slots)
                            (mapcar {plist-get :copier} direct-slots))))
                      copy)))))

(defgeneric genome (software)
  (:documentation
   "The software genotype or ``code'', exposed as a simplified data structure.
For example an AST genome, e.g., of a `cil' or `lisp' software object
my have a tree structure while the genome of an `asm' or `llvm'
software object will be a vector."))

(defgeneric phenome (software &key bin)
  (:documentation
   "Phenotype of the software.
This method will link, compile or serialize the software object as
necessary returning an executable version of the software suitable for
testing and evaluation.  Returns multiple values holding in order; (1)
the binary path to which the executable was compiled, (2) the errno,
or a numeric indication of success, of the compilation process, (3)
STDERR of the compilation process, or a string holding error output
relevant to phenome generation, (4) STDOUT of the compilation process,
or a string holding non-error output relevant to phenome
generation, (5) the source file name used during compilation. "))

(defgeneric phenome-p (software)
  (:documentation "Return non-nil if SOFTWARE has a phenotype."))

(defmacro ignore-phenome-errors (&rest body)
  "Handle errors in `phenome' execution by returning nil as the
first value from the `phenome' method."
  `(handler-bind ((phenome
                    (lambda (c)
                      (declare (ignorable c))
                      (invoke-restart 'return-nil-for-bin))))
     (progn ,@body)))

(defmethod phenome-p ((obj software))
  (ignore-phenome-errors
    (with-temp-file (bin)
      (phenome obj :bin bin))))

(defgeneric evaluate (function software &rest extra-keys &key &allow-other-keys)
  (:documentation "Evaluate the software returning a numerical fitness."))


(defmethod evaluate ((test symbol) (obj software)
                     &rest extra-keys &key &allow-other-keys)
  (declare (ignorable extra-keys))
  (if (null test)  ;; allow NIL to be passed for the function
      (setf test 'identity))
  (evaluate (symbol-function test) obj))

(defmethod evaluate ((test function) (obj software)
                     &rest extra-keys &key &allow-other-keys)
  (declare (ignorable extra-keys))
  (if (fitness obj)
      (values (fitness obj) (fitness-extra-data obj))
      (multiple-value-bind (fit extra) (funcall test obj)
        (setf (fitness obj) fit)
        (setf (fitness-extra-data obj) extra)
        (values fit extra))))

(defgeneric fitness-extra-data (software)
  (:documentation "Hold extra data returned by the fitness function."))

(defmethod fitness-extra-data ((obj software)) nil)

(defgeneric (setf fitness-extra-data) (extra-data software)
  (:documentation "Pass extra data (optionally) returned by the fitness function
                   to the software object."))

(defmethod (setf fitness-extra-data) (extra-data (obj software))
  (declare (ignorable extra-data)))

(defgeneric copy (software &key &allow-other-keys)
  (:documentation "Copy the software.
Return a deep copy of a software object."))

(defmethod copy ((obj software) &key)
  (make-instance (class-of obj) :fitness (fitness obj)))

(defgeneric size (software)
  (:documentation "Return the size of the `genome' of SOFTWARE."))

(defmethod size ((software software)) (length (genome software)))

(defgeneric lines (software)
  (:documentation "Return the lines of code of the `genome' of SOFTWARE."))

(defgeneric genome-string (software &optional stream)
  (:documentation "Return a string of the `genome' of SOFTWARE."))

(defgeneric (setf genome-string) (text software)
  (:documentation "Set the `genome' of SOFTWARE to the string representation"))

(defgeneric pick (software key &optional func)
  (:documentation "Pick an element of GENOME based on KEY of each element.
KEY is passed to `proportional-pick' to return an index.  Optional
argument FUNC processes the index to return a result."))

(defmethod pick ((sw software) key &optional func)
  (let ((pick (proportional-pick (genome sw) key)))
    (if func (funcall func pick) pick)))

(defgeneric pick-good (software)
  (:documentation "Pick a 'good' index into a software object.
Used to target mutation."))
(defmethod pick-good ((software software)) (random (size software)))

(defgeneric pick-bad (software)
  (:documentation "Pick a 'bad' index into a software object.
Used to target mutation."))
(defmethod pick-bad  ((software software)) (random (size software)))

(defgeneric pick-bad-good (software &key &allow-other-keys)
  (:documentation "Pick a 'bad' and a 'good' indexes into a software object.
Used to target mutation."))
(defmethod pick-bad-good ((software software) &key)
  (list (pick-bad software) (pick-good software)))

(defgeneric pick-bad-bad (software &key &allow-other-keys)
  (:documentation "Pick two 'bad' indexes into a software object.
Used to target mutation."))
(defmethod pick-bad-bad ((software software) &key)
  (list (pick-bad software) (pick-bad software)))

(defgeneric pick-bad-only (software &key &allow-other-keys)
  (:documentation "Pick a single 'bad' index into a software object.
Used to target mutation."))
(defmethod pick-bad-only ((software software) &key)
  (list (pick-bad software)))

(defgeneric mutate (software)
  (:documentation "Mutate the software.  May throw a `mutate' error."))

(defgeneric pick-mutation-type (software)
  (:documentation "Select a type of mutation to apply to SOFTWARE."))

(defvar *mutation-stats* (make-hash-table
                          :test #'equal
                          #+sbcl :synchronized #+sbcl t
                          #+ccl :shared #+ccl :lock-free)
  "Variable to hold mutation statistics.")

(defgeneric create-super (variant &optional rest-variants)
  (:documentation "Create an appropriately typed super-mutant and populate
with variant . rest-variants"))

(defmethod create-super ((variant software) &optional rest-variants)
  "Creates a SUPER-MUTANT and populates variants. Returns the super-mutant."
  (let ((inst (make-instance 'super-mutant)))
    (setf (mutants inst)(cons variant rest-variants))
    inst))

;;;
;;; Note that we can't method dispatch on the types in a list, so
;;; we dispatch on the first item of the list. This is a helper
;;; function to simplify the process of pulling out a variant to
;;; dispatch on. The list of variants should all contain the same
;;; type of software object, and presumably be related to each other
;;; such that they can be assigned to a common super-mutant.
;;;
(defun create-and-populate-super (variant-list)
  "Create and populate a super-mutant with supplied list of variants."
  (create-super (first variant-list) (rest variant-list)))

(defvar *max-saved-mutation-improvements* 24
  "Maximum number of mutation improvements to hold on to.")

(defvar *mutation-improvements* nil
  "List of recent mutation improvements cons'd with *fitness-evals*.")

(defvar *crossover-stats* (make-hash-table
                           :test #'equal
                           #+sbcl :synchronized #+sbcl t
                           #+ccl :shared #+ccl :lock-free)
  "Variable to hold crossover statistics.")

(defvar *fitness-evals* 0
  "Track the total number of fitness evaluations.")

(defvar *fitness-predicate* #'>
  "Function to compare two fitness values to select which is preferred.")

(defun fitness-scalar (fitness)
  (cond ((numberp fitness) fitness)
        ((or (listp fitness) (vectorp fitness))
         (reduce #'+ fitness))
        (:otherwise (error "Can't convert fitness ~a to a scalar"
                           fitness))))

(defvar *fitness-scalar-fn* #'fitness-scalar
  "Function to convert fitness to a numeric value")

(defun worst-numeric-fitness ()
  (cond ((equal #'< *fitness-predicate*) infinity)
        ((equal #'> *fitness-predicate*) 0)
        (t (error "bad *fitness-predicate* ~a" *fitness-predicate*))))

(defvar *worst-fitness* (worst-numeric-fitness)
  "Default worst fitness TODO.")

(defun worst-numeric-fitness-p (obj)
  (= (fitness obj)
     (worst-numeric-fitness)))

(defvar *worst-fitness-p* #'worst-numeric-fitness-p
  "Predicate indicating whether an individual has the worst possible fitness.")

(defvar *target-fitness-p* nil
  "Predicate indicating whether an individual has reached the target fitness.")

(defun fitness-better-p (fitness-a fitness-b)
  "Check if FITNESS-A is strictly better than FITNESS-B."
  (funcall *fitness-predicate*
           (funcall *fitness-scalar-fn* fitness-a)
           (funcall *fitness-scalar-fn* fitness-b)))

(defun fitness-equal-p (fitness-a fitness-b)
  "Return true if FITNESS-A and FITNESS-B are equal"
  (equalp fitness-a fitness-b))

(defun analyze-mutation (obj mutation-info test &aux result)
  "Default function to collect statistics from an applied mutation.

This function will calculate the improvements to the fitness of SOFTWARE
as the result of crossover and mutation using `evaluate' and TEST.
Each crossover and mutation will be paired with one of the following tags;
:dead, :same, :worse, or :better.  Calculated stats will be added to the
*crossover-stats* and *mutation-stats* variables for analysis."
  ;; Mutation info from new-individual
  (destructuring-bind (mutation software-a cross-point-a
                                crossed software-b cross-point-b)
      mutation-info

    ;; Evaluate software objects to ensure fitness
    (when crossed    (evaluate test crossed))    ; Evaluate for fitness
    (when software-a (evaluate test software-a)) ; Safety - should have fitness
    (when software-b (evaluate test software-b)) ; Safety - should have fitness
    (when obj        (evaluate test obj))        ; Safety - should have fitness

    ;; Add information on the mutation to `*mutation-stats*`.
    (multiple-value-bind (effect fit old-fit)
        (classify obj crossed)
      (when (equal effect :better)
        (push (cons (mutation-key crossed mutation)
                    *fitness-evals*)
              *mutation-improvements*)
        (when (>= (length *mutation-improvements*)
                  *max-saved-mutation-improvements*)
          (setf *mutation-improvements*
                (butlast *mutation-improvements*))))
      (push (setf result (list effect *fitness-evals* fit old-fit))
            (gethash (mutation-key crossed mutation) *mutation-stats*)))

    ;; Add information on the crossover to `*crossover-stats*`.
    (when cross-point-a
      (let ((effect (classify crossed software-a software-b)))
        (push (list effect *fitness-evals*)
              (gethash (mutation-key crossed mutation) *crossover-stats*))))

    (values
     obj mutation
     software-a cross-point-a crossed software-b cross-point-b
     (first result))))

(defmethod classify (new &rest old)
  "Classify the fitness of NEW as :BETTER, :WORSE, :SAME, or :DEAD when
compared to OLD.  NEW and OLD must have fitness populated."
  (let ((fit (fitness new))
        (old-fit (extremum (mapcar {fitness} old)
                           #'fitness-better-p)))
    (values
     (cond
       ((funcall *worst-fitness-p* new) :dead)
       ((and (not (fitness-better-p fit old-fit))
             (not (fitness-better-p old-fit fit)))
        :same)
       ((funcall (complement #'fitness-better-p) fit old-fit)
        :worse)
       ((fitness-better-p fit old-fit)
        :better))
     fit old-fit)))

(defgeneric mutation-key (software mutation)
  (:documentation "Key used to organize mutations in *mutation-stats*."))

(defmethod mutation-key ((obj software) mutation)
  "DOCFIXME
* OBJ DOCFIXME
* MUTATION DOCFIXME
"
  ;; Default to using the mutation op.
  (declare (ignorable obj)) (car mutation))

(defun summarize-mutation-stats (&aux results)
  "DOCFIXME
* RESULTS DOCFIXME
"
  (maphash (lambda (key vals)
             (mapc (lambda (result)
                     (if (aget result (aget (car key) results))
                         (incf (aget result (aget (car key) results)))
                         (setf (aget result (aget (car key) results)) 1)))
                   (mapcar #'first vals)))
           *mutation-stats*)
  results)

(defgeneric mcmc-step (software)
  (:documentation "Change software in a way amenable to MCMC.
Specifically every step should be reversible, and the resulting walk
should be ergodic."))

(defvar *mcmc-fodder* nil
  "Holds the genome elements which may be used by `mcmc-step'.
Should be initialized to a list of the unique possible genome
elements.")

(define-condition phenome (error)
  ((text :initarg :text :initform nil :reader text)
   (obj  :initarg :obj  :initform nil :reader obj)
   (loc  :initarg :loc  :initform nil :reader loc))
  (:report (lambda (condition stream)
             (if (loc condition)
                 (format stream "Phenome error ~S on ~S in ~a."
                         (text condition) (obj condition) (loc condition))
                 (format stream "Phenome error ~S on ~S."
                         (text condition) (obj condition)))))
  (:documentation "DOCFIXME"))

(define-condition mutate (error)
  ((text :initarg :text :initform nil :reader text)
   (obj  :initarg :obj  :initform nil :reader obj)
   (op   :initarg :op   :initform nil :reader op))
  (:report (lambda (condition stream)
             (if (op condition)
                 (format stream "Mutation error, ~a, applying ~S to ~S"
                         (text condition) (op condition) (obj condition))
                 (format stream "Mutation error, ~a, on ~S"
                         (text condition) (obj condition)))))
  (:documentation
   "Mutation errors are thrown when a mutation fails.
These may often be safely ignored.  A common restart is
`ignore-failed-mutation'."))

(define-condition no-mutation-targets (mutate)
  ((text :initarg :text :initform nil :reader text)
   (obj  :initarg :obj  :initform nil :reader obj)
   (op   :initarg :op   :initform nil :reader op))
  (:report (lambda (condition stream)
             (if (op condition)
                 (format stream "No targets error ~a applying ~S to ~S"
                         (text condition) (op condition) (obj condition))
                 (format stream "No targets error ~a on ~S"
                         (text condition) (obj condition)))))
  (:documentation
   "This is a particularly benign form of mutation error.
A common restart is `ignore-failed-mutation'."))

(defgeneric apply-mutation (software mutation)
  (:documentation "Apply MUTATION to SOFTWARE, return the resulting software object.
Mutation application may destructively modify the software object, or it may return a
new instance with the mutation applied, and leave the original untouched. Any client
which calls apply-mutation should ensure that the result returned by apply-mutation is 
captured, and should not make assumptions about the state of the original.

Example:  (let ((mutated-software (apply-mutation (copy software) mutation)))
              ...

Define an :around method on this function to record mutations."))

(defgeneric apply-all-mutations (software mutation)
  (:documentation "Apply MUTATION to every target in SOFTWARE.
Returns the resulting software objects.  Returns a list of the applied
mutations as an optional second value."))

(defgeneric apply-mutations (software mutation n)
  (:documentation "Apply MUTATION to the first N targets in SOFTWARE.
Returns the resulting software objects.  Returns a list of the applied
mutations as an optional second value."))

(defgeneric apply-picked-mutations (software mutation n)
  (:documentation "Apply MUTATION to N randomly selected targets in SOFTWARE.
Returns the resulting software objects.  Returns a list of the applied
mutations as an optional second value."))

(defgeneric crossover (software-a software-b)
  (:documentation "Crossover two software objects.
Define an :around method on this function to record crossovers."))

(defmethod crossover :around ((software-a software) (software-b software))
  ;; Mutation removes previously calculated fitness values.
  (multiple-value-call (lambda (child &rest rest)
                         (setf (fitness child) nil)
                         (apply #'values child rest))
    (call-next-method)))

(defgeneric one-point-crossover (software-a software-b)
  (:documentation "Crossover at a single point."))

(defgeneric two-point-crossover (software-a software-b)
  (:documentation "Crossover between two points."))

(defgeneric from-string (software string)
  (:documentation "Initialize SOFTWARE with contents of STRING."))

(defgeneric from-file (software file)
  (:documentation "Initialize SOFTWARE with contents of FILE."))

(defgeneric apply-config (software config-file)
  (:documentation "Parse CONFIG-FILE and use to configure SOFTWARE."))

(defgeneric to-file (software file)
  (:documentation "Write SOFTWARE to FILE"))

(defmethod to-file ((software software) file)
  (string-to-file (genome software) file))

(defgeneric apply-path (software key PATH) ; TODO: is this used?
  (:documentation "Apply the execution trace PATH behind KEY in SOFTWARE."))

(defgeneric expression (software what)
  (:documentation "Return WHAT in SOFTWARE as a lisp expression."))


;;; Mutation object
(defmacro define-mutation (class-name superclasses slots &rest options)
  "Like `defclass' but inherits TARGETER slot-options from MUTATION.
Also, ensures MUTATION is a member of superclasses"
  `(defclass ,class-name ,(if (member 'mutation (cons class-name superclasses))
                              superclasses
                              (append superclasses (list 'mutation)))
     ((targeter
       ,@(plist-merge
          (cdr (assoc 'targeter slots))
          (list :initarg :targeter :reader 'targeter
                :initform '(function pick-bad) :type 'function
                :documentation "A function from software -> targets.")))
      (picker
       ,@(plist-merge
          (cdr (assoc 'picker slots))
          (list :initarg :picker :reader 'picker
                :initform '(compose #'random-elt #'pick-bad) :type 'function
                :documentation "A function from software -> random target.")))
      ,@(remove-if {member _ '(targeter picker)} slots :key #'car))
     ,@options))

(defmacro compose-mutations (class-name mutations &rest options)
  "Define a new mutation named CLASS-NAME composing MUTATIONS.
MUTATIONS is a list of the names of mutation classes."
  (with-gensyms ((args args)
                 (mut mut)
                 (software software)
                 (mutation mutation)
                 (target target))
    (flet ((slot-initform (slot-name class)
             (finalize-inheritance (find-class class))
             (slot-definition-initform
              (find-if [{eql slot-name} #'slot-definition-name]
                       (class-slots (find-class class))))))
      `(prog1
           (define-mutation ,class-name
               ,(remove-duplicates
                 (mappend
                  (lambda (obj)
                    (remove-if «or {eql 'standard-object} {eql 'mutation}»
                               (mapcar #'class-name
                                       (class-direct-superclasses
                                        (find-class obj)))))
                  mutations))
             ((targeter
               :initform
               (lambda (&rest ,args)
                 (list ,@(mapcar
                          (lambda (fun) `(apply ,fun ,args))
                          (mapcar {slot-initform 'targeter} mutations))))
               :type 'function
               :documentation
               ,(format nil "Targeters from ~a." mutations))
              (picker
               :initform
               (lambda (targets)
                 (mapcar
                  (lambda (target picker) (funcall picker target))
                  targets
                  (list ,@(mapcar {slot-initform 'picker} mutations))))
               :type 'function
               :documentation
               ,(format nil "Pickers from ~a." mutations)))
             ;; NOTE: Should compose other slots as well.
             ,@options)
         (defmethod build-op ((,mut ,class-name) ,software)
           (mappend
            (lambda (,mutation ,target)
              (build-op (make-instance ,mutation :targets ,target)
                        ,software))
            ',mutations
            (targets ,mut)))))))

(defmacro sequence-mutations  (class-name mut-a mut-b &rest options)
  "Define a new mutation named CLASS-NAME sequencing MUT-A and MUT-B.
MUT-A and MUT-B are instances of mutations.  Instead of collecting
targets for A and then targets for B and then applying A and B as done
by `compose-mutations', `sequence-mutations' first targets and applies A and then targets and applied B."
  (declare (ignorable class-name mut-a mut-b options))
  (error "TODO: Implement `sequence-mutations'."))

(defclass mutation (oid-object)
  ((object :initarg :object :accessor object :initform nil
           :type (or software null)
           :documentation "The software object to be mutated.")
   (targets :initarg :targets :reader get-targets :initform nil
            :type (or list fixnum ast)
            :documentation "A calculated target set."))
  (:documentation "The base class of all software mutations."))

(defmethod print-object ((mut mutation) stream)
  "DOCFIXME"
  (print-unreadable-object (mut stream :type t)
    (prin1 (object mut) stream)
    (when (or (get-targets mut) (targeter mut))
      (format stream " ")
      (prin1 (or (get-targets mut)
                 (multiple-value-call [#'third #'list]
                   (function-lambda-expression (targeter mut)))) stream))))

(defmethod targets ((mut mutation))
  "DOCFIXME
* MUT DOCFIXME
"
  (or (get-targets mut)
      (when (object mut)
        (setf (slot-value mut 'targets)
              (funcall (targeter mut) (object mut))))))

(defgeneric at-targets (mutation targets &key &allow-other-keys)
  (:documentation "Return a copy of MUTATION with `targets' set to TARGETS."))

(defmethod at-targets ((mut mutation) targets &key (object (object mut)))
  "DOCFIXME
* MUT DOCFIXME
* TARGETS DOCFIXME
* OBJECT DOCFIXME
"
  (make-instance (type-of mut) :object object :targets targets))

(defmethod mutation-key ((obj software) (mutation mutation))
  "DOCFIXME
* OBJ DOCFIXME
* MUTATION DOCFIXME
"
  (declare (ignorable obj)) (type-of mutation))

(defmethod apply-mutation :before ((obj software) (mut mutation))
  "DOCFIXME
* OBJ DOCFIXME
* MUT DOCFIXME
"
  ;; Mutation removes previously calculated fitness values.
  (declare (ignorable mut))
  (setf (fitness obj) nil))

(defmethod apply-all-mutations ((obj software) (mut mutation))
  "DOCFIXME
* OBJ DOCFIXME
* MUT DOCFIXME
"
  (apply-mutations obj mut infinity))

(defmethod apply-mutations ((obj software) (mut mutation) n)
  "DOCFIXME
* OBJ DOCFIXME
* MUT DOCFIXME
* N DOCFIXME
"
  (setf (object mut) obj)
  (iter (for targeted in (mapcar {at-targets mut} (targets mut)))
        (for i below n)
        (collect targeted into mutations)
        (collect (apply-mutation (copy obj) targeted) into results)
        (finally (return (values results mutations)))))

(defmethod apply-picked-mutations ((obj software) (mut mutation) n)
  "DOCFIXME
* OBJ DOCFIXME
* MUT DOCFIXME
* N DOCFIXME
"
  (setf (object mut) obj)
  (iter (for i below n)
        (for picked = (funcall (picker mut) obj))
        (while picked)
        (let ((targeted (at-targets mut picked)))
          (collect targeted into mutations)
          (collect (apply-mutation (copy obj) targeted) into results))
        (finally (return (values results mutations)))))


;;; Evolution
(defvar *population* nil
  "Holds the variant programs to be evolved.

This variable may be read to inspect a running search process, or
written to as part of a running search process.
")

(defvar *generations* nil
  "Holds the running generation count.")

(defvar *max-population-size* nil
  "Maximum allowable population size.")

(defvar *tournament-size* 2
  "Number of individuals to participate in tournament selection.")

(defvar *tournament-eviction-size* 2
  "Number of individuals to participate in eviction tournaments.")

(defvar *cross-chance* 2/3
  "Fraction of new individuals generated using crossover rather than mutation.")

(defvar *mut-rate* 1
  "Chance to mutate a new individual.
* If <1, new individuals will be mutated once with change *MUT-RATE*.
* If =1, then every new individual will be mutated exactly once.
* If >1, then new individuals will be mutated from 1 to *MUT-RATE* times.")

(defvar *running* nil
  "True when a search process is running, set to nil to stop evolution.")

(defvar *start-time* nil
  "Holds the start time of evolutionary processes.")

(declaim (inline elapsed-time))
(defun elapsed-time () (/ (- (get-internal-real-time) *start-time*)
                          internal-time-units-per-second))

(defun incorporate (software)
  "Incorporate SOFTWARE into POPULATION, keeping POPULATION size constant."
  (push software *population*)
  (loop :while (and *max-population-size*
                    (> (length *population*) *max-population-size*))
     :do (evict)))

(defvar *tie-breaker-predicate* #'>
  "Function to compare two tie breaker values to select which is preferred.")

(defun evict ()
  (let ((loser (tournament :predicate (complement *fitness-predicate*)
                           :size *tournament-eviction-size*
                           :tie-breaker-predicate
                              (complement *tie-breaker-predicate*))))
    (setf *population* (remove loser *population* :count 1))
    loser))

(defun default-select-best (group &key (predicate *fitness-predicate*))
  "Return the members of GROUP with most PREDICATE fitness.
Default selection function for `tournament'."
  (remove-if-not [{= (fitness (extremum group predicate :key #'fitness))}
                  #'fitness]
                 group))

(defun default-random-winner (group &key predicate)
  "Choose a random winner from GROUP."
  (declare (ignorable predicate))
  (random-elt group))

(defvar *tournament-selector* #'default-select-best
  "Function used to select winners of a tournament. Returns a list of
  winners.")

(defvar *tournament-tie-breaker* #'default-random-winner
  "Function used to break ties in a tournament. Returns a single winner.")

(defun tournament
    (&key (predicate *fitness-predicate*)
       (tie-breaker-predicate *tie-breaker-predicate*)
       (size *tournament-size*))
  "Select an individual from *POPULATION* with a tournament."
  (flet ((verify (it)
           (assert (typep it 'software) (it)
                   "Population member is not software object")
           (assert (fitness it) (it)
                   "Population member with no fitness")
           it))
    (assert *population* (*population*) "Empty population.")
    (funcall *tournament-tie-breaker*
             (funcall *tournament-selector*
                      (iter (for i below size)
                            (collect (verify (random-elt *population*))))
                      :predicate predicate)
             :predicate tie-breaker-predicate)))

(defun mutant (&optional (new (copy (tournament))))
  "Generate a new mutant from a *POPULATION*."
  (cond ((< *mut-rate* 1) (if (< (random 1.0) *mut-rate*) (mutate new) new))
        ((= *mut-rate* 1) (mutate new))
        ((> *mut-rate* 1) (dotimes (n (1+ (floor (random *mut-rate*))) new)
                            (mutate new)))))

(defun crossed (&optional (a (tournament)) (b (tournament)))
  "Generate a new individual from *POPULATION* using crossover."
  (if (< (random 1.0) *cross-chance*)
      (crossover a b)
      (values (copy a) nil nil)))

(defmethod new-individual (&optional (a (tournament)) (b (tournament)))
  "Generate a new individual from *POPULATION*."
  (multiple-value-bind (crossed a-point b-point) (crossed a b)
    ;; NOTE: This `copy' call is only needed for `analyze-mutation'.
    ;; If it appears to be adding significant overhead, consider two
    ;; alternate implementations of `new-individual' instead of the
    ;; current approach in which `analyze-mutate' "wraps"
    ;; `new-individual'.
    (multiple-value-bind (mutant mutation) (mutant (copy crossed))
      (values mutant
              ;; Mutation info for analyze-mutation
              (list mutation a a-point crossed b b-point)))))

(defun new-individuals (count)
  "Generate COUNT new individuals from *POPULATION*."
  (iter (with new-count = 0)
        (restart-case
            (multiple-value-bind (variant mutation-info)
                (new-individual)
              (collect variant into variants)
              (collect mutation-info into infos)
              (incf new-count))

          (ignore-failed-mutation ()
            :report
            "Ignore failed mutation and continue evolution"))
        (while (< new-count count))
        (finally (return (values variants infos)))))

(defmacro -search (specs step &rest body)
  "Perform a search loop with early termination.

SPECS should be a list of the following elements.

  (VARIANT MUT-INFO) -- Symbols for variant and mut-info resulting from STEP
  TEST ---------------- Test function used to `evaluate' every VARIANT.
  MAX-EVALS ----------- Maximum number of evaluations to perform.
  MAX-TIME ------------ Maximum time to run.
  PERIOD -------------- Period (in evals) at which to call PERIOD-FN.
  PERIOD-FN ----------- Function to call every Period evals.
  EVERY-PRE-FN -------- Function to call before every evaluation.
  EVERY-POST-FN ------- Function to call after every evaluation.
  FILTER -------------- Function to filter variants from BODY.
  ANALYZE-MUTATION-FN - Function to call to analyze mutation results

The following global variables are implicitly updated by this function
and should be dynamically bound to perform multiple different
simultaneous searches, `*running*', `*start-time*', `*fitness-evals*'.
The global variable `*target-fitness-p*' implicitly defines a stopping
criteria for this search."
  (destructuring-bind (variant mutation-info) (car specs)
    ;; Outside the returned code let-bind unevaluated elements of SPECS.
    (with-gensyms ((test test)
                   (max-evals max-evals)
                   (max-time max-time)
                   (period period)
                   (period-fn period-fn)
                   (every-pre-fn every-pre-fn)
                   (every-post-fn every-post-fn)
                   (filter filter)
                   (analyze-mutation-fn analyze-mutation-fn)
                   (variants variants)
                   (mutation-infos mutation-infos))
      ;; Inside the returned code let-bind evaluated elements of SPECS.
      `(let ((,test ,(nth 1 specs))
             (,max-evals ,(nth 2 specs))
             (,max-time ,(nth 3 specs))
             (,period ,(nth 4 specs))
             (,period-fn ,(nth 5 specs))
             (,every-pre-fn ,(nth 6 specs))
             (,every-post-fn ,(nth 7 specs))
             (,filter ,(nth 8 specs))
             (,analyze-mutation-fn ,(nth 9 specs)))
         (block search-target-reached
           (unless *start-time* (setq *start-time* (get-internal-real-time)))
           (setq *running* t)
           (loop :until (or (not *running*)
                            (and ,max-evals
                                 (> *fitness-evals* ,max-evals))
                            (and ,max-time
                                 (> (/ (- (get-internal-real-time) *start-time*)
                                       internal-time-units-per-second)
                                    ,max-time))) :do
              (restart-case
                  (multiple-value-bind (,variants ,mutation-infos)
                      (funcall ,step)
                    (when ,every-pre-fn
                      (mapc ,every-pre-fn ,variants))
                    (if (cdr ,variants)
                        ;; Multiple variants. Combine into super-mutant.
                        ;;
                        ;; FIXME: We should split the use of
                        ;;        super-mutants into a separate
                        ;;        variable instead of using the number
                        ;;        of individuals returned by
                        ;;        new-individual.
                        (let ((super (create-and-populate-super ,variants)))
                          (genome super)
                          (evaluate ,test super))
                        ;; Single variant. Evaluate directly.
                        (evaluate ,test (car ,variants)))
                    (when ,analyze-mutation-fn
                      (mapc (lambda (variant info)
                              (funcall ,analyze-mutation-fn variant info ,test))
                            ,variants
                            ,mutation-infos))
                    (when ,every-post-fn
                      (mapc ,every-post-fn ,variants))
                    (dotimes (_ (length ,variants))
                      (incf *fitness-evals*)
                      (when (and ,period ,period-fn
                                 (zerop (mod *fitness-evals* ,period)))
                        (funcall ,period-fn)))
                    (mapc (lambda (,variant ,mutation-info)
                            (declare (ignorable ,mutation-info))
                            (assert (fitness ,variant) (,variant)
                                    "Variant with no fitness")
                            (when (or (not ,filter) (funcall ,filter ,variant))
                              ,@body)
                            (when (and *target-fitness-p*
                                       (funcall *target-fitness-p* ,variant))
                              (return-from search-target-reached ,variant)))
                          ,variants
                          ,mutation-infos))
                (ignore-failed-mutation ()
                  :report
                  "Ignore failed mutation and continue evolution"))))
         (setq *running* nil)))))

(defmacro mcmc (original test
                &key
                  accept-fn max-evals max-time period period-fn
                  every-pre-fn every-post-fn filter analyze-mutation-fn)
  "MCMC search from ORIGINAL using `mcmc-step' and TEST.
If keyword argument ACCEPT-FN is given it is used to determine when a
newly found candidate replaces the current candidate.  If ACCEPT-FN is
not supplied MCMC defaults to using Metropolis Hastings.

Other keyword arguments are used as defined in the `-search' function."
  (let* ((curr (gensym))
         (body
          `(let ((,curr ,original))
             (-search ((new mut-info)
                       ,test ,max-evals ,max-time ,period ,period-fn
                       ,every-pre-fn ,every-post-fn
                       ,filter ,analyze-mutation-fn)
                      (mcmc-step ,curr)
                      (when (funcall accept-fn (fitness ,curr) (fitness new))
                        (setf ,curr new))))))
    (if accept-fn
        body
        `(let ((accept-fn
                (lambda (curr new) ;; Default to Metropolis Hastings.
                  (or (funcall *fitness-predicate* new curr)
                      (< (random 1.0) ;; Assume a numeric fitness.
                         (if (> new curr) (/ curr new) (/ new curr)))))))
           ,body))))

(defmacro evolve (test
                  &key
                    max-evals max-time period period-fn
                    every-pre-fn every-post-fn filter analyze-mutation-fn
                    (super-mutant-count 1))
  "Evolves `*population*' using `new-individual' and TEST.

* SUPER-MUTANT-COUNT evaluate this number of mutants at once in a
  combined genome.

Other keyword arguments are used as defined in the `-search' function.
"
  `(-search ((new mut-info)
             ,test ,max-evals ,max-time ,period ,period-fn
             ,every-pre-fn ,every-post-fn ,filter ,analyze-mutation-fn)
            {new-individuals ,super-mutant-count}
            (incorporate new)))

(defun generational-evolve
    (reproduce evaluate-pop select
     &key
       every-pre-fn every-post-fn analyze-mutation-fn test period period-fn
       max-generations max-evals max-time filter)
  "Evolves `*population*' using REPRODUCE EVALUATE-POP and SELECT.

Required arguments are as follows:
  REPRODUCE ----------- create new individuals from the current population
  EVALUATE-POP -------- evaluate-pop the entire population
  SELECT -------------- select best individuals from the population
Keyword arguments are as follows:
  MAX-GENERATIONS ----- stop after this many generations
  MAX-EVALS ----------- stop after this many fitness evaluations
  MAX-TIME ------------ stop after this many seconds
  PERIOD -------------- interval of generations evaluations to run PERIOD-FN
  PERIOD-FN ----------- function to run every PERIOD generations
  EVERY-PRE-FN -------- function to run on each new individual before evaluation
  EVERY-POST-FN ------- function to run on each new individual after evaluation
  ANALYZE-MUTATION-FN - function to call to analyze mutation results
  TEST ---------------- fitness test function for mutation statistics
  FILTER -------------- remove individuals for which FILTER returns false"

  (setq *running* t)
  (setq *generations* 0)
  (setq *start-time* (get-internal-real-time))
  (when *target-fitness-p*
    (assert (functionp *target-fitness-p*) (*target-fitness-p*)
            "`*target-fitness-p*' must be a function"))
  (flet
      ((check-max (current max)
         (or (not max) (not current) (< current max))))
    (prog1
        (loop :while (and *running*
                          (check-max *generations* max-generations)
                          (check-max *fitness-evals* max-evals)
                          (check-max (elapsed-time) max-time)) :do
           (incf *generations*)
           (multiple-value-bind (children mutation-info)
               (funcall reproduce *population*)
             (if every-pre-fn (mapc every-pre-fn children))
             (funcall evaluate-pop children)
             (if analyze-mutation-fn
                 (mapcar (lambda (c info)
                           (funcall analyze-mutation-fn c info test))
                         children mutation-info))
             (if every-post-fn (mapc {funcall every-post-fn} children))
             (if filter (setq children (delete-if-not filter children)))
             (setq *population* (append children *population*))
             (loop :for child :in children
                :when (funcall *target-fitness-p* child) :do
                (setf *running* nil)
                (return-from generational-evolve child)))
           (format t "Selecting~%")
           (setq *population*
                 (funcall select *population* *max-population-size*))
           (assert (<= (length *population*) *max-population-size*))
           (if (and period period-fn (zerop (mod *generations* period)))
               (funcall period-fn)))
      (setq *running* nil))))

(defun simple-reproduce (population &aux children mutations)
  "Reproduce using every individual in POPULATION.
Return a list of the resulting children and as optional extra value a
list of the mutations applied to produce those children."
  (mapcar (lambda (parent)
            (restart-case
                (multiple-value-bind (child info)
                    (new-individual parent (random-elt population))
                  (push child children)
                  (push info mutations))
              (ignore-failed-mutation ()
                :report "Ignore failed mutation and continue evolution")))
          population)
  (values children mutations))

(defun simple-evaluate (test new-children)
  "Evaluate NEW-CHILDREN using TEST assigning each a fitness."
  (mapc (lambda (child)
          (incf *fitness-evals*)
          (restart-case
              (evaluate test child)
            (worse-for-failed-fitness-evaluation ()
              :report
              "Assign `worst-numeric-fitness' for failed fitness evaluation."
              *worst-fitness*)))
        new-children))

(defun simple-select (population max-size &aux new-pop)
  (declare (ignorable population)) ; tournament uses global *population*
  (iter (until (= max-size (length new-pop)))
        (restart-case (push (tournament) new-pop)
          (ignore-failed-selection ()
            :report "Ignore failed `tournament' selection."))))
