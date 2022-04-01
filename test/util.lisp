(defpackage :software-evolution-library/test/util
  (:nicknames :sel/test/util)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/utility/debug
        :software-evolution-library/utility/range
        :stefil+
        :software-evolution-library/software/elf-cisc
        :software-evolution-library/software/parseable
        :software-evolution-library/software/tree-sitter
        :functional-trees/attrs)
  (:export :test
           :+etc-dir+
           :+gcd-dir+
           :+grep-prj-dir+
           :+cpp-sample-dir+
           :+multi-file-dir+
           :+asm-test-dir+
           :+multiple-artifact-dir+
           :+lisp-scopes-dir+
           :+javascript-dir+
           :+typescript-dir+
           :+java-dir+
           :+python-dir+
           :+python-utility-dir+
           :+include-processing-dir+
           ;; Other functions
           :c-tree-sitter-available-p
           :cpp-tree-sitter-available-p
           :cl-tree-sitter-available-p
           :go-tree-sitter-available-p
           :java-tree-sitter-available-p
           :python-tree-sitter-available-p
           :javascript-tree-sitter-available-p
           :typescript-tree-sitter-available-p
           :json-tree-sitter-available-p
           :rust-tree-sitter-available-p
           :stmt-with-text
           :fully-every
           :different-asts
           :range-to-list
           :scope-contains-string-p
           :scopes-contains-string-p
           ;; Directory functions
           :gcd-dir
           :fib-dir
           :asm-test-dir
           :javascript-dir
           :java-dir
           :python-dir
           ;; Variables referenced in tests
           :*tfos*
           :*soft*
           :*gcd*
           :*binary-search*
           :*fib*
           :*test*
           :*project*
           :*soft-mutate-errors*
           ;; Fixtures.
           :soft
           :range
           :double-range
           :gcd-elf
           :test-ast-source-ranges-for-files
           :test-single-ast-source-ranges
           :expand-wildcard
           :with-fixture/attrs))
(in-package :software-evolution-library/test/util)
(in-readtable :curry-compose-reader-macros)

(defvar *tfos* nil "Another software used in tests.")
(defvar *soft* nil "Software used in tests.")
(defvar *gcd* nil "Holds the gcd software object.")
(defvar *binary-search* nil "Holds the binary_search software object.")
(defvar *fib* nil "Holds the fibonacci software object.")
(defvar *test* nil "Variable to hold evaluation function for tests.")
(defvar *project* nil "Software used in project fixtures.")

(defroot test)

(define-constant +etc-dir+
    (append (pathname-directory
             #.(or *compile-file-truename*
                   *load-truename*
                   *default-pathname-defaults*))
            (list "etc"))
  :test #'equalp
  :documentation "Path to directory holding testing artifacts.")

(define-constant +gcd-dir+ (append +etc-dir+ (list "gcd"))
  :test #'equalp
  :documentation "Path to directory holding gcd.")

(define-constant +grep-prj-dir+ (append +etc-dir+ (list "grep-prj"))
  :test #'equalp
  :documentation "Path to directory holding the grep project.")

(define-constant +include-processing-dir+
    (append +etc-dir+ (list "include-processing"))
  :test #'equalp
  :documentation "Path to directory holding include processing test project.")

(define-constant +cpp-sample-dir+ (append +etc-dir+ (list "cpp-sample"))
  :test #'equalp
  :documentation "Path to directory holding the c++ sample project.")

(define-constant +multi-file-dir+ (append +etc-dir+ (list "multi-file"))
  :test #'equalp
  :documentation "Path to directory holding the multi-file example.")

(define-constant +clang-format-dir+ (append +etc-dir+ (list "clang-format"))
  :test #'equalp
  :documentation "Location of the clang-format example directory")

(define-constant +fib-dir+ (append +etc-dir+ (list "fib"))
  :test #'equalp
  :documentation "Location of the fib example dir")

(define-constant +javascript-dir+ (append +etc-dir+ (list "javascript"))
  :test #'equalp
  :documentation "Path to directory holding Javascript test programs.")

(define-constant +typescript-dir+ (append +etc-dir+ (list "typescript"))
  :test #'equalp
  :documentation "Path to directory holding Javascript test programs.")

(define-constant +java-dir+ (append +etc-dir+ (list "java"))
  :test #'equalp
  :documentation "Path to directory holding Java test programs.")

(define-constant +python-dir+ (append +etc-dir+ (list "python"))
  :test #'equalp
  :documentation "Path to directory holding python test programs.")

(define-constant +asm-test-dir+ (append +etc-dir+ (list "asm-test"))
  :test #'equalp
  :documentation "Path to asm-test examples.")

(define-constant +multiple-artifact-dir+
    (append +etc-dir+ (list "multiple-artifacts"))
  :test #'equalp
  :documentation
  "Path to directory holding a project which produces multiple artifacts.")

(define-constant +lisp-scopes-dir+ (append +etc-dir+ (list "lisp-scopes"))
  :test #'equalp
  :documentation
  "Path to directory holding lisp code to test scopes.")

(define-constant +python-utility-dir+
    (append +etc-dir+ (list "python-utility"))
  :test #'equalp
  :documentation
  "Path to directory holding python code to test utility functions.")

(defun asm-test-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +asm-test-dir+))


;;; Functions on constants.
(defun gcd-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +gcd-dir+))

(defun clang-format-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +clang-format-dir+))

(defun fib-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +fib-dir+))

(defun javascript-dir (path)
  (merge-pathnames-as-file (make-pathname :directory +javascript-dir+)
                           path))

(defun java-dir (path)
  (merge-pathnames-as-file (make-pathname :directory +java-dir+)
                           path))

(defun python-dir (path)
  (merge-pathnames-as-file (make-pathname :directory +python-dir+)
                           path))


;;; Helper macros.
(defmacro with-fixture/attrs (fixture &body body)
  `(with-fixture ,fixture
     (with-attr-table *soft*
       ,@body)))


;;;; Helper functions.
(defun c-tree-sitter-available-p ()
  (handler-case (progn (make-instance 'sel/sw/tree-sitter::c))
    (error (e) (declare (ignorable e)) nil)))

(defun cpp-tree-sitter-available-p ()
  (handler-case (progn (make-instance 'sel/sw/tree-sitter::cpp))
    (error (e) (declare (ignorable e)) nil)))

(defun cl-tree-sitter-available-p ()
  (handler-case (progn (make-instance 'sel/sw/tree-sitter::cl))
    (error (e) (declare (ignorable e)) nil)))

(defun go-tree-sitter-available-p ()
  (handler-case (progn (make-instance 'sel/sw/tree-sitter::golang))
    (error (e) (declare (ignorable e)) nil)))

(defun java-tree-sitter-available-p ()
  (handler-case (progn (make-instance 'sel/sw/tree-sitter::java))
                (error (e) (declare (ignorable e)) nil)))

(defun python-tree-sitter-available-p ()
  (handler-case (progn (make-instance 'sel/sw/tree-sitter::python))
                (error (e) (declare (ignorable e)) nil)))

(defun javascript-tree-sitter-available-p ()
  (handler-case (progn (make-instance 'sel/sw/tree-sitter::javascript))
                (error (e) (declare (ignorable e)) nil)))

(defun typescript-tree-sitter-available-p ()
  (handler-case (progn
                  (make-instance 'sel/sw/tree-sitter::typescript-ts)
                  (make-instance 'sel/sw/tree-sitter::typescript-tsx))
    (error (e) (declare (ignorable e)) nil)))

(defun json-tree-sitter-available-p ()
  (handler-case (progn (make-instance 'sel/sw/tree-sitter::json))
                (error (e) (declare (ignorable e)) nil)))

(defun rust-tree-sitter-available-p ()
  (handler-case (progn (make-instance 'sel/sw/tree-sitter::rust))
                (error (e) (declare (ignorable e)) nil)))

(defun stmt-with-text- (asts text trim at-start)
  (find-if [(if at-start [{equal 0} {search text}] {string= text})
            (if trim #'trim-whitespace #'identity)
            #'source-text]
           asts))

(defgeneric stmt-with-text (obj text &key no-error trim at-start &allow-other-keys)
  (:documentation "Return the AST in OBJ holding TEXT.
Unless optional argument NO-ERROR is non-nil an error is raised if no
AST holding STMT is found.")
  (:method :around (obj (text string) &key no-error trim &allow-other-keys)
           (when trim (setf text (trim-whitespace text)))
           (or (call-next-method)
               (if no-error
                   nil
                   (error "`stmt-with-text' failed to find ~S in ~S"
                          text
                          (etypecase obj
                            (software (genome-string obj))
                            (ast (source-text obj)))))))
  (:method ((obj software) (text string) &rest rest)
    (apply #'stmt-with-text (asts obj) text :no-error t rest))
  (:method ((asts sequence) (text string) &key trim at-start &allow-other-keys)
    (stmt-with-text- asts text trim at-start))
  (:method ((asts ast) (text string) &key trim at-start &allow-other-keys)
    (stmt-with-text- asts text trim at-start)))

(defun scope-contains-string-p (scope string)
  "Return the variable alist associated with STRING if it exists in SCOPE."
  (find-if [{equalp string} {aget :name}] scope))

(defun scopes-contains-string-p (scopes string)
  "Return the variable alist associated with STRING if it exists in SCOPES."
  (mappend {scope-contains-string-p _ string} scopes))


;;; Software.
(define-software soft (software)
  ((genome :initarg :genome :accessor genome :initform nil)))

(defvar *soft-mutate-errors* nil
  "Control when mutations on soft objects throw errors.")


;;; Methods.
(defmethod crossover ((a soft) (b soft))
  (values (copy a)(list :fake-a) (list :fake-b)))
(defmethod mutate ((a soft))
  (setf (fitness a) nil)
  (if *soft-mutate-errors*
      (error (make-condition 'mutate
                             :text "FAKE"
                             :obj a
                             :operation '(:fake)))
      (values (copy a) (list :fake))))


;;; Fixtures
(defixture soft
  (:setup (setf *soft* (make-instance 'soft
                         :genome (coerce (loop :for i :from 0 :to 9 :collect i)
                                         'vector))))
  (:teardown (setf *soft* nil)))

(defixture gcd-elf
  (:setup (setf *gcd* (from-file (make-instance 'elf-x86) (gcd-dir "gcd"))))
  (:teardown (setf *gcd* nil)))

(defun fully-every (fn seq &rest other-seqs)
  "A version of EVERY that also checks that all sequences are
of the same length"
  (let ((len (length seq)))
    (and (every (lambda (s) (= (length s) len)) other-seqs)
         (apply #'every fn seq other-seqs))))

;; Check if the two AST lists differ. Do a smoke test with
;; the list lengths; if they match, use the src-text
;; field as a proxy for equality. Strict equality isn't
;; useful because of nondeterministic fields like :src-file.
(defun different-asts (this that)
  (or (not (equal (length this) (length that)))
      (not (every (lambda (x y)
                    (string= (source-text x) (source-text y)))
                  this that))))

(defun range-to-list (range)
  "Convert a range object to a pair of (begin-line . begin-col)
(end-line . end-col) conses."
  (list (cons (line (begin range)) (column (begin range)))
        (cons (line (end range)) (column (end range)))))

(defun test-ast-source-ranges-for-files (class files
                                         &key (limit 1000)
                                           ignore-indentation)
  (iter (for file in-vector (take limit (reshuffle files)))
        (ignore-some-conditions
            (simple-error
             error)  ;Ignore unparseable files.
         (test-single-ast-source-ranges
          class file
          :ignore-indentation ignore-indentation))))

(defun test-single-ast-source-ranges (class file &key ignore-indentation)
  "Test that AST source ranges round-trip.
That is, test that the result of calling `source-text' on an AST is the same as calling `ast-source-ranges' on its containing software and extracting the specified range from the software's serialization."
  (ignore-some-conditions (mutate)
    (let* ((sw (from-file (make class) file))
           (ranges (ast-source-ranges sw))
           (text (source-text (genome sw) :trim nil))
           (newline-offsets (precompute-newline-offsets text)))
      (is (not (emptyp ranges)))
      (iter (for (ast . range) in ranges)
            (let ((reference-text1 (source-range-subseq text range))
                  (reference-text2 (source-range-subseq text range
                                                        newline-offsets))
                  (output-text (source-text ast :trim nil)))
              ;; Also test that source-range-subseq works properly
              ;; with newline offsets.
              (is (equal reference-text1 reference-text2))
              (if ignore-indentation
                  (let ((output-lines (lines output-text))
                        (reference-lines (lines reference-text1)))
                    (is (length= output-lines reference-lines))
                    (is (every #'string$= output-lines reference-lines)))
                  (is (equal reference-text1 output-text))))))))

(defun expand-wildcard (wildcard)
  "Get test files matching WILDCARD relative to test/etc/."
  (is (wild-pathname-p wildcard))
  (let* ((path
          (path-join (asdf:system-relative-pathname
                      :software-evolution-library
                      #p"test/etc/")
                     wildcard))
         (files (directory path)))
    (is (not (emptyp files)))
    files))
