;;; tests.lisp --- tests for the `software-evolution' package

;; Copyright (C) 2011-2013  Eric Schulte

;; Licensed under the Gnu Public License Version 3 or later

;;; Code:
(in-package :software-evolution-test)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

;; Disable clang-format and any other helpers
(defmacro every-is (function &rest lists)
  (let ((args-sym (gensym "args")))
    `(mapc (lambda (&rest ,args-sym)
             (is (apply ,function ,args-sym)))
           ,@lists)))

;;; Run tests is "batch" mode printing results as a string.
(defun batch-test (&optional args)
  (declare (ignorable args))

  (let* ((*test-progress-print-right-margin* (expt 2 20))
         (failures (coerce (stefil::failure-descriptions-of
                            (without-debugging (test)))
                           'list)))
    (if failures
        (progn (format *error-output* "FAILURES~%")
               (mapc [{format *error-output* "  ~a~%"}
                      #'stefil::name-of
                      #'stefil::test-of
                      #'car #'stefil::test-context-backtrace-of]
                     failures))
        (format *error-output* "SUCCESS~%"))))

;;; Run all tests, printing test name and either pass or fail to error output.
;;; Intended to be used for sending results to datamanager
(defun testbot-test (&optional args)
  (declare (ignorable args))
  (let ((*print-test-run-progress* nil))
    (->> (hash-table-alist (stefil::run-tests-of (without-debugging (test))))
         (mapcar
          (lambda-bind ((test . run))
            (list
             (if (zerop (stefil::number-of-added-failure-descriptions-of run))
                 :pass
                 :fail)
             (stefil::name-of test))))
         (format *error-output* "~{~{~a~^ ~}~%~}"))))

(defsuite test)
(in-suite test)

(defvar *genome*      nil "Genome used in tests.")
(defvar *soft*        nil "Software used in tests.")
(defvar *tfos*        nil "Another software used in tests.")
(defvar *gcd*         nil "Holds the gcd software object.")
(defvar *binary-search* nil "Holds the binary_search software object.")
(defvar *headers*     nil "Holds the headers software object.")
(defvar *hello-world* nil "Holds the hello world software object.")
(defvar *huf*         nil "Holds the huf software object.")
(defvar *scopes*      nil "Holds the scopes software object.")
(defvar *range-ref* #("one" "two" "three" "four" "five" "six")
  "Example range software object.")

(handler-bind ((error (lambda (e) (declare (ignorable e)) (invoke-restart 'ignore))))
  (progn
    (define-constant +etc-dir+
        (append (butlast (pathname-directory
                          #.(or *compile-file-truename*
                                *load-truename*
                                *default-pathname-defaults*)))
                (list "etc"))
      :test #'equalp
      :documentation "Path to directory holding testing artifacts.")

    (define-constant +gcd-dir+ (append +etc-dir+ (list "gcd"))
      :test #'equalp
      :documentation "Path to directory holding gcd.")

    (define-constant +headers-dir+ (append +etc-dir+ (list "headers"))
      :test #'equalp
      :documentation "Path to directory holding headers.")

    (define-constant +hello-world-dir+ (append +etc-dir+ (list "hello-world"))
      :test #'equalp
      :documentation "Location of the hello world example directory")

    (define-constant +clang-format-dir+ (append +etc-dir+ (list "clang-format"))
      :test #'equalp
      :documentation "Location of the clang-format example directory")

    (define-constant +huf-dir+ (append +etc-dir+ (list "huf"))
      :test #'equalp
      :documentation "Location of the huf example directory")

    (define-constant +scopes-dir+ (append +etc-dir+ (list "scopes"))
      :test #'equalp
      :documentation "Location of the scopes example directory")))

(defun gcd-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +gcd-dir+))

(defun headers-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +headers-dir+))

(defun hello-world-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +hello-world-dir+))

(defun clang-format-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +clang-format-dir+))

(defun huf-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +huf-dir+))

(defun scopes-dir (filename)
  (make-pathname :name (pathname-name filename)
                 :type (pathname-type filename)
                 :directory +scopes-dir+))

(define-software soft (software)
  ((genome :initarg :genome :accessor genome :initform nil)))

(defvar *soft-mutate-errors* nil
  "Control when mutations on soft objects throw errors.")

(defmethod crossover ((a soft) (b soft))
  (values (copy a)(list :fake-a) (list :fake-b)))
(defmethod mutate ((a soft))
  (setf (fitness a) nil)
  (if *soft-mutate-errors*
      (error (make-condition 'mutate
               :text "FAKE"
               :obj a
               :op '(:fake)))
      (values (copy a) (list :fake))))

(defixture soft
  (:setup (setf *soft* (make-instance 'soft
                         :genome (coerce (loop :for i :from 0 :to 9 :collect i)
                                         'vector))))
  (:teardown (setf *soft* nil)))

(defixture range
  (:setup (setf *soft* (make-instance 'sw-range
                         :genome '((0 . 2) (1 . 1) (1 . 2))
                         :reference #("one" "two" "three"))))
  (:teardown (setf *soft* nil)))

(defixture double-range
  (:setup
     (setf *soft* (make-instance 'sw-range
                    :genome '((0 . 2) (1 . 1) (1 . 2))
                    :reference *range-ref*)
           *tfos* (make-instance 'sw-range
                    :genome '((2 . 5) (4 . 4) (4 . 5))
                    :reference *range-ref*)))
  (:teardown (setf *soft* nil *tfos* nil)))

(defixture diff
  (:setup
   (setf *soft* (make-instance 'diff)
         (genome *soft*) '(((:code 1)) ((:code 2)) ((:code 3)) ((:code 4)))))
  (:teardown (setf *soft* nil)))

(defixture double-diff
  (:setup
   (setf *soft* (make-instance 'diff)
         (genome *soft*) '(((:code 1)) ((:code 2)) ((:code 3)) ((:code 4)))
         *tfos* (make-instance 'diff)
         (genome *tfos*) '(((:code 1)) ((:code 2)) ((:code 3)) ((:code 4)))))
  (:teardown (setf *soft* nil *tfos* nil)))

(defixture diff-array
  (:setup
   (setf *soft* (make-instance 'diff)
         (genome *soft*) #(((:code 1)) ((:code 2)) ((:code 3)) ((:code 4)))))
  (:teardown (setf *soft* nil)))

(defixture gcd-asm
  (:setup (setf *gcd* (from-file (make-instance 'asm) (gcd-dir "gcd.s"))))
  (:teardown (setf *gcd* nil)))

(defixture gcd-elf
  (:setup
   (let ((arch (intern (string-upcase (subseq (shell "uname -m") 0 3)))))
     (setf *gcd* (from-file (make-instance (case arch
                                             (x86 'elf-x86)
                                             (mips 'elf-mips)))
                            (gcd-dir "gcd")))))
  (:teardown (setf *gcd* nil)))

(defixture binary-search-clang
  (:setup
   (setf *binary-search*
         (from-file
          (make-instance 'clang
            :flags (list
                    "-I"
                    (namestring (make-pathname :directory +etc-dir+))))
          (make-pathname
           :name "binary_search"
           :type "c"
           :directory +etc-dir+))))
  (:teardown
   (setf *binary-search* nil)))

(defixture gcd-clang
  (:setup
   (setf *gcd*
         (from-file (make-instance 'clang :compiler "clang-3.7")
                    (gcd-dir "gcd.c"))))
  (:teardown
   (setf *gcd* nil)))

(defixture gcd-wo-curlies-clang
  (:setup
   (setf *gcd*
         (from-file (make-instance 'clang :compiler "clang-3.7")
                    (gcd-dir "gcd-wo-curlies.c"))))
  (:teardown
   (setf *gcd* nil)))

(defixture headers-clang
  (:setup
   (setf *headers*
         (from-file (make-instance 'clang
                      :compiler "clang-3.7"
                      :flags (list "-I" (namestring
                                         (make-pathname
                                          :directory +headers-dir+))))
                    (headers-dir "main.c"))))
  (:teardown
   (setf *hello-world* nil)))

(defixture hello-world-clang
  (:setup
    (setf *hello-world*
      (from-file (make-instance 'clang :compiler "clang-3.7"
                                       :flags '("-g -m32 -O0"))
                 (hello-world-dir "hello_world.c"))))
  (:teardown
    (setf *hello-world* nil)))

(defun inject-missing-swap-macro (obj)
  ;; Inject a macro that clang-mutate currently misses, then force the ASTs to
  ;; be recalculated by setting the genome-string.
  (add-macro obj
             "swap_" "swap_(I,J) do { int t_; t_ = a[(I)]; a[(I)] = a[(J)]; a[(J)] = t_; } while (0)")
  (setf (genome-string obj) (genome-string obj)))

(defixture hello-world-clang-w-fodder
  (:setup
   (setf *database*
         (with-open-file (in (hello-world-dir "hello_world_ast.json"))
           (make-instance 'json-database :json-stream in)))
   (setf *hello-world*
     (from-file (make-instance 'clang-w-fodder :compiler "clang-3.7"
                                               :flags '("-g -m32 -O0"))
                (hello-world-dir "hello_world.c"))))
  (:teardown
    (setf *database* nil)
    (setf *hello-world* nil)))

(defixture huf-clang
  (:setup
    (setf *huf*
      (from-file (make-instance 'clang :compiler "gcc" :flags '("-g -m32 -O0"))
                 (huf-dir "huf.c")))
    (inject-missing-swap-macro *huf*))
  (:teardown
    (setf *huf* nil)))

(defixture scopes-clang
  (:setup
    (setf *scopes*
          (from-file (make-instance 'clang-control-picks
                                    :compiler "clang" :flags '("-g -m32 -O0"))
                 (scopes-dir "scopes.c"))))
  (:teardown
    (setf *scopes* nil)))

(defixture population
  (:setup (setf *population* (loop :for i :from 1 :to 9
                                collect (make-instance 'soft
                                          :genome (loop :for j :from 0 :to i
                                                     :collect j)
                                          :fitness i))
                *fitness-evals* 0
                *mutation-stats* (make-hash-table)
                *crossover-stats* (make-hash-table)))
  (:teardown (setf *population* nil
                   *fitness-evals* 0
                   *mutation-stats* nil
                   *crossover-stats* nil)))


;;; ASM representation
(deftest simple-read ()
  (with-fixture gcd-asm
    (is (equal 'asm (type-of *gcd*)))))

(deftest idempotent-read-write ()
  (let ((a (software-evolution::temp-file-name)))
    (unwind-protect
         (with-fixture gcd-asm
           (to-file *gcd* a)
           (multiple-value-bind (out err ret)
               (software-evolution::shell "diff ~s ~a"
                                          (namestring (gcd-dir "gcd.s")) a)
             (declare (ignorable out err))
             (is (= 0 ret))))
      (delete-file a))))

(deftest idempotent-copy ()
  (with-fixture gcd-asm
   (is (software-evolution::equal-it *gcd* (copy *gcd*)))))

(deftest idempotent-read-copy-write ()
  (let ((a (software-evolution::temp-file-name)))
    (unwind-protect
         (with-fixture gcd-asm
           (to-file (copy *gcd*) a)
           (multiple-value-bind (out err ret)
               (software-evolution::shell "diff ~s ~a"
                                          (namestring (gcd-dir "gcd.s")) a)
             (declare (ignorable out err))
             (is (= 0 ret))))
      (delete-file a))))

(deftest edit-of-copy-does-not-change-original ()
  (with-fixture gcd-asm
    (let ((orig-hash (sxhash (genome *gcd*)))
          (ant (copy *gcd*)))
      (mutate ant)
      (is (not (software-evolution::equal-it (genome ant) (genome *gcd*))))
      (is (equal orig-hash (sxhash (genome *gcd*)))))))

(deftest asm-cut-actually-shortens ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*)))
      (apply-mutation variant '(:cut 4))
      (is (< (length (genome variant)) (length (genome *gcd*)))))))

(deftest asm-insertion-actually-lengthens ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*)))
      (apply-mutation variant '(:insert 4 8))
      (is (> (length (genome variant)) (length (genome *gcd*)))))))

(deftest asm-swap-maintains-length ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*)))
      (apply-mutation variant '(:swap 4 8))
      (is (not (tree-equal (genome variant) (genome *gcd*))))
      (is (= (length (genome variant)) (length (genome *gcd*)))))))

(deftest simple-crossover-test ()
  (with-fixture gcd-asm
    (let ((variant (copy *gcd*)))
      (apply-mutation variant '(:cut 0))
      ;; (push '(:cut 0) (edits variant))
      (let ((new (crossover variant *gcd*)))
        (is (not (tree-equal (genome new) (genome *gcd*))))
        ;; (is (some [{equal :crossover} #'car] (edits new)))
        ;; (is (some [{equal :cut} #'caar] (second (edits new))))
        ))))


;;; ELF representation
#| ;; TODO: Currently failing because we're not populating the .text section.

(defun bytes (elf) (mappend [#'cdr {assoc :code}] (genome elf)))

(deftest elf-read ()
  (with-fixture gcd-elf
    (is (or (equal 'elf-x86 (type-of *gcd*))
            (equal 'elf-mips (type-of *gcd*))))))

(deftest elf-idempotent-read-write ()
  (with-temp-file (a)
    (with-fixture gcd-elf
      (phenome *gcd* :bin a)
      (multiple-value-bind (out err ret)
          (software-evolution::shell "diff ~s ~a"
                                     (namestring (gcd-dir "gcd")) a)
        (declare (ignorable out err))
        (is (= 0 ret))))))

(deftest elf-copy-same-genome ()
  (with-fixture gcd-elf
   (is (software-evolution::equal-it (genome *gcd*)
                                     (genome (copy *gcd*))))))

(deftest elf-idempotent-read-copy-write ()
  (with-temp-file (a)
    (with-fixture gcd-elf
      (phenome (copy *gcd*) :bin a)
      (multiple-value-bind (out err ret)
          (software-evolution::shell "diff ~s ~a"
                                     (namestring (gcd-dir "gcd")) a)
        (declare (ignorable out err))
        (is (= 0 ret))))))

(deftest elf-edit-of-copy-does-not-change-original ()
  (with-fixture gcd-elf
    (let ((orig-hash (sxhash (genome *gcd*)))
          (ant (copy *gcd*)))
      (handler-case (mutate ant)
        (mutate (obj) (declare (ignorable obj)) nil))
      (is (not (software-evolution::equal-it (genome ant) (genome *gcd*))))
      (is (equal orig-hash (sxhash (genome *gcd*)))))))

(deftest elf-cut-changes-but-maintains-byte-length ()
  (with-fixture gcd-elf
    (let ((variant (copy *gcd*)))
      (apply-mutation variant '(:cut 4))
      (is (= (length (bytes *gcd*)) (length (bytes variant))))
      (is (not (equal-it (bytes *gcd*) (bytes variant)))))))

(deftest elf-insertion-changes-but-maintains-lengthens ()
  (with-fixture gcd-elf
    (let ((variant (copy *gcd*))
          ;; FIND-SMALL: Pick a single-byte instruction so that it is
          ;; more likely that there are sufficient no-ops to delete.
          ;; This works with the local compiled version of gcd, but
          ;; may fail in the future or on other systems.
          (to-copy (position-if [{= 1} #'length {aget :code}] (genome *gcd*))))
      (apply-mutation variant (list :insert 0 to-copy))
      (is (= (length (bytes *gcd*)) (length (bytes variant))))
      (is (not (equal-it (bytes *gcd*) (bytes variant)))))))

(deftest elf-replace-changes-but-maintains-length ()
  (with-fixture gcd-elf
    (let* ((variant (copy *gcd*))
           ;; See FIND-SMALL in `elf-insertion-changes-but-maintains-lengthens'
           (to-copy (position-if [{= 1} #'length {aget :code}] (genome *gcd*)))
           (new-genome (software-evolution::elf-replace
                        variant 0 (copy-tree (nth to-copy (genome *gcd*))))))
      (is (= (length (mappend {aget :code} (genome *gcd*)))
             (length (mappend {aget :code} new-genome))))
      (is (not (equal-it (mappend {aget :code} (genome *gcd*))
                         (mappend {aget :code} new-genome)))))))

(deftest elf-swap-changes-but-maintains-length ()
  (with-fixture gcd-elf
    (let* ((variant (copy *gcd*))
           ;; Find two instructions of differing content so the genome
           ;; isn't the same after our swap.
           (prev nil)
           (place (position-if (lambda (el)
                                 (prog1 (and prev (not (tree-equal prev el)))
                                   (setf prev el)))
                               (genome *gcd*))))
      (apply-mutation variant (list :swap place (1- place)))
      (is (= (length (bytes *gcd*)) (length (bytes variant))))
      (is (not (equal-it (bytes *gcd*) (bytes variant)))))))

(deftest elf-swap-touching-last-element ()
  (with-fixture gcd-elf
    (let* ((variant (copy *gcd*))
           (place1 (1- (length (genome *gcd*))))
           (value1 (nth place1 (genome *gcd*)))
           (place2 (random (1- (length (genome *gcd*)))))
           (value2 (nth place2 (genome *gcd*))))
      (apply-mutation variant (list :swap place1 place2))
      (is (= (length (bytes *gcd*)) (length (bytes variant))))
      (is (or (tree-equal value1 value2)
              (not (equal-it (bytes *gcd*) (bytes variant))))))))

(deftest elf-swap-with-self ()
  (with-fixture gcd-elf
    (let* ((place (random (length (genome *gcd*))))
           (variant (copy *gcd*)))
      (apply-mutation variant (list :swap place place))
      (is (equal-it (genome variant) (genome *gcd*))))))

(deftest elf-crossover-test ()
  (with-fixture gcd-elf
    (let ((variant (copy *gcd*)))
      (apply-mutation variant '(:cut 0))
      (let ((new (crossover variant *gcd*)))
        (is (not (equal-it (genome new) (genome *gcd*))))
        (is (= (length (bytes *gcd*)) (length (bytes variant))))))))
|#


;;; Clang representation
(deftest simply-able-to-load-a-clang-software-object()
  (with-fixture hello-world-clang
    (is (not (null *hello-world*)))))

;; Check our temporary hack to split multi-variable declarations.
(deftest split-multi-variable-declarations ()
  (is (= 2 (count-if {scan "double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double a,b;
}")))))
      "Splits a simple single-line declaration.")
  (is (= 2 (count-if {scan "double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double a,
         b;
}")))))
      "Splits a multi-line declaration.")
  (is (= 2 (count-if {scan "double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double a = 4,
         b = 68;
}")))))
      "Splits a multi-line declaration with initialization.")
  (is (= 2 (count-if {scan "double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double a = 4,b = 68;
}")))))
      "Splits a single-line declaration with initialization.")
  (is (= 2 (count-if {scan "double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double a=4,b=68;
}")))))
      "Splits another single-line declaration with initialization.")
  (is (= 2 (count-if {scan "double\\*"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double* a=4,b=68;
}")))))
      "Handles pointer type.")
  (is (= 2 (count-if {scan "double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double *a=4,b=68;
}")))))
      "Handles first variable is pointer.")
  (is (= 2 (count-if {scan "double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double a=4,*b=68;
}")))))
      "Handles second variable is pointer.")
  (is (= 2 (count-if {scan "double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  double a=fopen(one, two), b;
}")))))
      "Handles initialization with commas.")
  (is (= 2 (count-if {scan "const double"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  const double a,b;
}")))))
      "Retains a modifier declaration.")
  (is (= 2 (count-if {scan "register int"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  register int a,b;
}")))))
      "Retains another modifier declaration.")
  (is (= 2 (count-if {scan "static unsigned int"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  static unsigned int a,b;
}")))))
      "Retains two modifiers in declaration.")
  (is (= 2 (count-if {scan "struct dfamust"}
                     (split-sequence #\Newline
                       (genome-string
                        (from-string-exactly (make-instance 'clang)
                                             "void int main(void) {
  struct dfamust *dm, *ndm;
}")))))
      "Another with a modifier."))

;; Check if the two AST lists differ. Do a smoke test with
;; the list lengths; if they match, use the :src-text
;; field as a proxy for equality. Strict equality isn't
;; useful because of nondeterministic fields like :src-file.
(defun different-asts (this that)
  (or (not (equal (length this) (length that)))
      (not (every (lambda (x y)
                    (string= (aget :src-text x) (aget :src-text y)))
                  this that))))

(deftest can-apply-mutation-w-value1 ()
  (with-fixture hello-world-clang
    (let* ((variant (copy *hello-world*))
           (stmt1 (stmt-with-text variant
                                  "printf(\"Hello, World!\\n\")")))
      (apply-mutation variant
        `(clang-replace
          (:stmt1 . ,stmt1)
          (:value1 . ((:src-text . "/* FOO */")))))
      (is (different-asts (asts variant) (asts *hello-world*)))
      (is (not (equal (genome variant) (genome *hello-world*)))))))

(deftest cut-shortens-a-clang-software-object()
  (with-fixture hello-world-clang
    (let* ((variant (copy *hello-world*))
           (stmt1 (stmt-with-text variant
                                  "printf(\"Hello, World!\\n\")")))
      (apply-mutation variant `(clang-cut (:stmt1 . ,stmt1)))
      (is (different-asts (asts variant)
                          (asts *hello-world*)))
      (is (not (equal (genome variant)
                      (genome *hello-world*))))
      (is (< (size variant)
             (size *hello-world*))))))

(deftest insert-lengthens-a-clang-software-object()
  (with-fixture hello-world-clang
    (let ((variant (copy *hello-world*))
          (stmt1 (stmt-with-text *hello-world*
                                 "printf(\"Hello, World!\\n\")"))
          (stmt2 (stmt-with-text *hello-world*
                                 "return 0")))
      (apply-mutation variant
        `(clang-insert (:stmt1 . ,stmt1) (:stmt2 . ,stmt2)))
      (is (different-asts (asts variant)
                          (asts *hello-world*)))
      (is (not (equal (genome variant)
                      (genome *hello-world*))))
      (is (> (size variant)
             (size *hello-world*))))))

(deftest swap-changes-a-clang-software-object()
  (with-fixture hello-world-clang
    (let ((variant (copy *hello-world*))
          (stmt1 (stmt-with-text *hello-world*
                                 "printf(\"Hello, World!\\n\")"))
          (stmt2 (stmt-with-text *hello-world*
                                 "return 0")))
      (apply-mutation variant
        `(clang-swap (:stmt1 . ,stmt1) (:stmt2 . ,stmt2)))
      (is (different-asts (asts variant)
                          (asts *hello-world*)))
      (is (not (equal (genome variant)
                      (genome *hello-world*))))
      (is (= (size variant)
             (size *hello-world*))))))

(deftest crossover-clang-software-object-do-not-crash()
  (with-fixture hello-world-clang
    (let* ((variant (crossover (copy *hello-world*) (copy *hello-world*))))
      (is (string/= (genome variant)
                    "")))))

(deftest able-to-wrap-statements-in-blocks ()
  (with-fixture gcd-wo-curlies-clang
    (let ((var (copy *gcd*)))
      ;; Setup, ensure everything is what we thing it should be.
      (is (string= "BinaryOperator"     ; Guard
                   (aget :ast-class (ast-with-text var "a > b"))))
      (is (string= "BinaryOperator"     ; Then
                   (aget :ast-class (ast-with-text var "a = a - b"))))
      (is (string= "BinaryOperator"     ; Else
                   (aget :ast-class (ast-with-text var "b = b - a"))))
      ;; Wrap children and ensure changes are made.
      (let ((if-counter
             (aget :counter
                   (second (remove-if-not [{string= "IfStmt"} {aget :ast-class}]
                                          (asts var))))))
        (setf var (wrap-child var if-counter 1))
        (setf var (wrap-child var if-counter 2)))
      (is (string= "BinaryOperator"     ; Guard
                   (aget :ast-class (ast-with-text var "a > b"))))
      (is (string= "CompoundStmt"       ; Then
                   (aget :ast-class (get-parent-ast var
                                      (ast-with-text var "a = a - b")))))
      (is (string= "CompoundStmt"       ; Then
                   (aget :ast-class (get-parent-ast var
                                      (ast-with-text var "b = b - a")))))
      ;; Ensure gcd remains unchanged.
      (is (string= "BinaryOperator"     ; Guard
                   (aget :ast-class (ast-with-text *gcd* "a > b"))))
      (is (string= "BinaryOperator"     ; Then
                   (aget :ast-class (ast-with-text *gcd* "a = a - b"))))
      (is (string= "BinaryOperator"     ; Else
                   (aget :ast-class (ast-with-text *gcd* "b = b - a")))))))

(deftest split-multi-var-decls-on-clang ()
  (with-fixture gcd-clang
    (is (not (scan "double a,b,c;" (genome-string *gcd*))))))

(deftest clang-headers-parsed-in-order ()
  (with-fixture headers-clang
    ;; TODO: Include "first.c" before include "third.c".

    ;; TODO: Ensure "MAIN" is present.  Presently MAIN is not present
    ;; because it is not used in the immediate source.

    ;; TODO: Ensure "ANOTHER" is not present.  It is defined in
    ;; another file.
    ))

(deftest clang-expression-test ()
  (flet ((test-conversion (obj pair)
           (destructuring-bind (text expected-expression) pair
             (let ((result (expression obj (ast-with-text obj text))))
               (is (equalp result expected-expression)
                   "Statement ~S yields ~S not ~S."
                   text result expected-expression)))))
    (append
     (with-fixture gcd-clang
       (mapc {test-conversion *gcd*}
             '(("b = b - a" (:= :b (:- :b :a)))
               ("a = a - b" (:= :a (:- :a :b)))
               ("b != 0"    (:!= :b 0))
               ("a > b"     (:> :a :b))
               ("a == 0"    (:== :a 0)))))
     (with-fixture binary-search-clang
       (mapc {test-conversion *binary-search*}
             '(("mid = (start + end) / 2"
                (:= :mid (:/ (:+ :start :end) 2)))
               ("haystack[i] = malloc(256 * sizeof(*haystack[i]))"
                (:= (:|[]| :haystack :i)
                 (:malloc (:* 256
                              (:sizeof (:unary-* (:|[]| :haystack :i))))))))))
     (with-fixture huf-clang
       (mapc {test-conversion *huf*}
             '(("h->h = malloc(sizeof(int)*s)"
                (:= (:-> :h :h) (:malloc (:* (:sizeof :int) :s))))
               ("heap->h = realloc(heap->h, heap->s + heap->cs)"
                (:= (:-> :heap :h)
                 (:realloc (:-> :heap :h) (:+ (:-> :heap :s)
                                              (:-> :heap :cs)))))))))))


;;; Detailed clang mutation tests
;;;
;;; These all run the entire mutate method, rather that just
;;; apply-mutation, adjusting the good and bad picks to get
;;; predictable results. And they check the results of each mutation
;;; in as much detail as possible.

(defvar *good-asts* nil "Control pick-good")
(defvar *bad-asts* nil "Control pick-bad")
(define-software clang-control-picks (clang) ())
(defmethod good-stmts ((obj clang-control-picks))
  (or *good-asts* (remove-if {aget :is-decl} (asts obj))))
(defmethod bad-stmts ((obj clang-control-picks))
  (or *bad-asts* (remove-if {aget :is-decl} (asts obj))))

(defixture hello-world-clang-control-picks
  (:setup
    (setf *hello-world*
      (from-file (make-instance 'clang-control-picks :compiler "clang-3.7"
                                :flags '("-g -m32 -O0"))
                 (hello-world-dir "hello_world.c"))))
  (:teardown
   (setf *hello-world* nil)))

(defun asts-with-text (obj &rest texts)
  (mapcar [{get-ast obj} {stmt-with-text obj}] texts))

(deftest cut-full-removes-full-stmt ()
  (with-fixture hello-world-clang
    (let ((software-evolution::*clang-mutation-types*
           '(clang-cut clang-cut-full clang-cut-same clang-cut-full-same))
          (*decl-mutation-bias* 0.0)
          (*clang-full-stmt-bias* 1.0)
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (< (count-if {aget :full-stmt} (asts variant))
             (count-if {aget :full-stmt} (asts *hello-world*)))))))

(deftest cut-removes-non-full-stmt ()
  (with-fixture hello-world-clang
    (let ((software-evolution::*clang-mutation-types*
           '(clang-cut clang-cut-full clang-cut-same clang-cut-full-same))
          (*decl-mutation-bias* 0.0)
          (*clang-full-stmt-bias* 0.0)
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (< (count-if-not {aget :full-stmt} (asts variant))
             (count-if-not {aget :full-stmt} (asts *hello-world*)))))))

(deftest insert-full-adds-full-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((software-evolution::*clang-mutation-types*
           '(clang-insert clang-insert-full
             clang-insert-same clang-insert-full-same))
          (*bad-asts* (asts-with-text *hello-world* "return 0"))
          (*decl-mutation-bias* 0.0)
          (*clang-full-stmt-bias* 1.0)
          (*clang-same-class-bias* 0.0)
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (> (count-if {aget :full-stmt} (asts variant))
             (count-if {aget :full-stmt} (asts *hello-world*)))))))

(deftest insert-adds-non-full-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "printf"))
          (*good-asts* (asts-with-text *hello-world* "printf"))
          (software-evolution::*clang-mutation-types*
           '(clang-insert clang-insert-full
             clang-insert-same clang-insert-full-same))
          (*decl-mutation-bias* 0.0)
          (*clang-full-stmt-bias* 0.0)
          (*clang-same-class-bias* 0.0)
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-with-text variant "printfprintf")))))

(deftest insert-same-adds-same-class ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "0"))
          (software-evolution::*clang-mutation-types*
           '(clang-insert clang-insert-full
             clang-insert-same clang-insert-full-same))
          (*decl-mutation-bias* 0.0)
          (*clang-full-stmt-bias* 0.0)
          (*clang-same-class-bias* 1.0)
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-with-text variant "00")))))

(deftest insert-full-same-adds-same-class-full-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "printf" "return 0"))
          (software-evolution::*clang-mutation-types*
           '(clang-insert clang-insert-full
             clang-insert-same clang-insert-full-same))
          (*decl-mutation-bias* 0.0)
          (*clang-full-stmt-bias* 1.0)
          (*clang-same-class-bias* 1.0)
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (> (count-if [{equal "ReturnStmt"} {aget :ast-class}]
                       (asts variant))
             (count-if [{equal "ReturnStmt"} {aget :ast-class}]
                       (asts *hello-world*)))))))

(deftest replace-changes-non-full-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "0"))
          (*good-asts* (asts-with-text *hello-world* "printf"))
          (software-evolution::*clang-mutation-types*
           '(clang-replace clang-replace-full
             clang-replace-same clang-replace-full-same))
          (*decl-mutation-bias* 0.0)
          (*clang-full-stmt-bias* 0.0)
          (*clang-same-class-bias* 0.0)
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-with-text variant "return printf")))))

(deftest replace-full-changes-full-stmt ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "printf" "return 0"))
          (*good-asts* (asts-with-text *hello-world*
                                       "0" "printf(\"Hello, World!\\n\")"))
          (software-evolution::*clang-mutation-types*
           '(clang-replace clang-replace-full
             clang-replace-same clang-replace-full-same))
          (*decl-mutation-bias* 0.0)
          (*clang-full-stmt-bias* 1.0)
          (*clang-same-class-bias* 0.0)
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (> (count-if [{equal "CallExpr"} {aget :ast-class}]
                       (asts variant))
             (count-if [{equal "CallExpr"} {aget :ast-class}]
                       (asts *hello-world*)))))))

(deftest replace-same-changes-same-class ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world* "\"Hello, World!\\n\""))
          (*good-asts* (asts-with-text *hello-world*
                                       "0" "printf"))
          (software-evolution::*clang-mutation-types*
           '(clang-replace clang-replace-full
             clang-replace-same clang-replace-full-same))
          (*decl-mutation-bias* 0.0)
          (*clang-full-stmt-bias* 0.0)
          (*clang-same-class-bias* 1.0)
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-with-text variant "printf(printf)")))))

(deftest replace-full-same-changes-same-class-full-stmt ()
  (with-fixture hello-world-clang
    (let ((software-evolution::*clang-mutation-types*
           '(clang-replace clang-replace-full
             clang-replace-same clang-replace-full-same))
          (*decl-mutation-bias* 0.0)
          (*clang-full-stmt-bias* 1.0)
          (*clang-same-class-bias* 1.0)
          (variant (copy *hello-world*)))
      (multiple-value-bind  (variant mutation) (mutate variant)
        (is (aget :full-stmt
                  (get-ast *hello-world* (aget :stmt1 (targets mutation)))))
        (is (aget :full-stmt
                  (get-ast *hello-world* (aget :stmt2 (targets mutation)))))

        ;; Not a very interesting test: this can only replace a
        ;; statement with itself, but sometimes there are whitespace
        ;; changes. Just compare AST classes to avoid spurious
        ;; failures.
        (is (equal (mapcar {aget :ast-class} (asts variant))
                   (mapcar {aget :ast-class} (asts *hello-world*))))))))

(deftest swap-changes-non-full-stmts ()
  (with-fixture hello-world-clang-control-picks
    (let ((*bad-asts* (asts-with-text *hello-world*
                                      "\"Hello, World!\\n\"" "0"))
          (software-evolution::*clang-mutation-types*
           '(clang-swap clang-swap-full
             clang-swap-same clang-swap-full-same))
          (*decl-mutation-bias* 0.0)
          (*clang-full-stmt-bias* 0.0)
          (*clang-same-class-bias* 0.0)
          (variant (copy *hello-world*)))
      (mutate variant)
      (is (stmt-with-text variant "\"Hello, World!\\n\""))
      (is (stmt-with-text variant "0")))))

(deftest swap-full-changes-full-stmts ()
  (with-fixture hello-world-clang-control-picks
    (let ((software-evolution::*clang-mutation-types*
           '(clang-swap clang-swap-full clang-swap-same
             clang-swap-full-same))
          ;; Avoid swapping the function body
          (*bad-asts* (remove-if [{member _  '("CompoundStmt" "Function")
                                          :test #'string=}
                                  {aget :ast-class}]
                                 (asts *hello-world*)))
          (*decl-mutation-bias* 0.0)
          (*clang-full-stmt-bias* 1.0)
          (*clang-same-class-bias* 0.0)
          (variant (copy *hello-world*)))

      (multiple-value-bind  (variant mutation) (mutate variant)
        ;; We can't predict exactly what will be swapped. Just
        ;; sanity check.
        (is (aget :full-stmt
                  (get-ast *hello-world* (aget :stmt1 (targets mutation)))))
        (is (aget :full-stmt
                  (get-ast *hello-world* (aget :stmt2 (targets mutation)))))
        (is (stmt-with-text variant "printf"))
        (is (stmt-with-text variant "return 0"))))))

(deftest swap-full-same-changes-same-class-full-stmt ()
  (with-fixture hello-world-clang
    (let ((software-evolution::*clang-mutation-types*
           '(clang-swap clang-swap-full clang-swap-same
             clang-swap-full-same))
          (*decl-mutation-bias* 0.0)
          (*clang-full-stmt-bias* 1.0)
          (*clang-same-class-bias* 1.0)
          (variant (copy *hello-world*)))
      (multiple-value-bind  (variant mutation) (mutate variant)
        (is (aget :full-stmt
                  (get-ast *hello-world* (aget :stmt1 (targets mutation)))))
        (is (aget :full-stmt
                  (get-ast *hello-world* (aget :stmt2 (targets mutation)))))

        ;; Not a very interesting test: this can only swap a
        ;; statement with itself, but sometimes there are whitespace
        ;; changes. Just compare AST classes to avoid spurious
        ;; failures.
        (is (equal (mapcar {aget :ast-class} (asts variant))
                   (mapcar {aget :ast-class} (asts *hello-world*))))))))


;;; Clang w/ mutation fodder representation
(deftest simply-able-to-load-a-clang-w-fodder-software-object()
  (with-fixture hello-world-clang-w-fodder
    (is (not (null *hello-world*)))))

(deftest pick-snippet-json-db-returns-a-json-snippet ()
  (with-fixture hello-world-clang-w-fodder
    (let ((json (pick-snippet *hello-world*)))
      (is (numberp (aget :counter json)))
      (is (stringp (aget :src-text json)))
      (is (assoc :full-stmt json)))))

(deftest pick-snippet-json-db-respects-full-argument ()
  (with-fixture hello-world-clang-w-fodder
    (is (aget :full-stmt (pick-snippet *hello-world* :full t)))))

(deftest pick-snippet-json-db-respects-class-argument ()
  (with-fixture hello-world-clang-w-fodder
    (dolist (class '("StringLiteral" "ReturnStmt" "CompoundStmt"))
      (is (string= class
                   (aget :ast-class
                         (pick-snippet *hello-world* :class class)))))))

(deftest insert-value-lengthens-a-clang-w-fodder-software-object()
  (with-fixture hello-world-clang-w-fodder
    (let ((variant (copy *hello-world*)))
      (apply-mutation variant '(clang-insert (:stmt1 . 3)
                                (:literal1 . "int i = 0;")))
      (is (> (size variant)
             (size *hello-world*)))
      (is (string/= (genome variant)
                    (genome *hello-world*))))))

(deftest set-value-changes-a-clang-w-fodder-software-object()
  (with-fixture hello-world-clang-w-fodder
    (let ((variant (copy *hello-world*)))
      (apply-mutation variant
        `(clang-replace
          (:stmt1 . ,(stmt-with-text variant "\"Hello, World!\\n\""))
          (:literal1 . "\"Hello, mutate!\"")))
      (is (= (size variant)
             (size *hello-world*)))
      (is (string/= (genome variant)
                    (genome *hello-world*))))))

;;; Clang utility methods
(deftest asts-populated-on-creation ()
  (with-fixture hello-world-clang
    (is (= 10 (length (asts *hello-world*))))))

(deftest parent-ast-p-true-test()
  (with-fixture hello-world-clang
    (is (parent-ast-p *hello-world*
          (get-ast *hello-world* (stmt-with-text *hello-world*
                                                  "return 0"))
          (get-ast *hello-world* (stmt-with-text *hello-world*
                                                  "0"))))))

(deftest parent-ast-p-false-test()
  (with-fixture hello-world-clang
    (is (not (parent-ast-p *hello-world*
               (get-ast *hello-world* (stmt-with-text *hello-world*
                                                       "0"))
               (get-ast *hello-world* (stmt-with-text *hello-world*
                                                       "return 0")))))))

(deftest tidy-a-clang-w-fodder-software-object()
  (with-fixture hello-world-clang-w-fodder
    (let ((variant (copy *hello-world*)))
      (clang-tidy variant)
      (is (= (size variant)
             (size *hello-world*))))))

(deftest format-a-clang-software-object ()
  (flet ((run (obj)
           (with-temp-file (bin)
             (phenome obj :bin bin)
             (shell bin))))
    (with-fixture hello-world-clang
      (multiple-value-bind (obj errno) (clang-format (copy *hello-world*))
        (is (zerop errno))
        (is (string= (run *hello-world*) (run obj)))))))


;;; Range representation
(deftest range-size ()
  (with-fixture range (is (= 6 (size *soft*)))))

(deftest range-lines ()
  (with-fixture range
    (is (tree-equal (lines *soft*)
                    '("one" "two" "three" "two" "two" "three")
                    :test #'string=))))

(deftest range-nth-test ()
  (with-fixture range
    (is (equal (mapcar {software-evolution::range-nth _ (genome *soft*)}
                       (loop :for i :below (size *soft*) :collect i))
               '(0 1 2 1 1 2)))))

(deftest range-subseq-test ()
  (with-fixture range
    ;; to
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 0 1)
                    '((0 . 0))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 0 2)
                    '((0 . 1))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 0 3)
                    '((0 . 2))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 0 4)
                    '((0 . 2) (1 . 1))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 0 5)
                    '((0 . 2) (1 . 1) (1 . 1))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 0 6)
                    '((0 . 2) (1 . 1) (1 . 2))))
    ;; from
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 1 7)
                    '((1 . 2) (1 . 1) (1 . 2))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 2 7)
                    '((2 . 2) (1 . 1) (1 . 2))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 3 7)
                    '((1 . 1) (1 . 2))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 4 7)
                    '((1 . 2))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 5 7)
                    '((2 . 2))))
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 6 7)
                    'NIL))
    ;; both
    (is (tree-equal (software-evolution::range-subseq (genome *soft*) 2 5)
                    '((2 . 2) (1 . 1) (1 . 1))))))

(deftest some-range-cut-mutations ()
  (with-fixture range
    (is (tree-equal (genome (apply-mutation *soft* '(:cut 2)))
                    '((0 . 1) (1 . 1) (1 . 2))))
    (is (tree-equal (genome (apply-mutation *soft* '(:cut 2)))
                    '((0 . 1) (2 . 2))))
    (is (tree-equal (genome (apply-mutation *soft* '(:cut 1)))
                    '((0 . 0) (2 . 2))))
    (is (tree-equal (genome (apply-mutation *soft* '(:cut 1)))
                    '((0 . 0))))
    (is (null (genome (apply-mutation *soft* '(:cut 0)))))))

(deftest some-range-insert-mutations ()
  (with-fixture range
    (is (tree-equal (genome (apply-mutation *soft* '(:insert 0 2)))
                    '((2 . 2) (0 . 2) (1 . 1) (1 . 2))))
    (is (tree-equal (genome (apply-mutation *soft* '(:insert 5 1)))
                    '((2 . 2) (0 . 2) (1 . 1) (0 . 0) (1 . 2))))
    (is (tree-equal (genome (apply-mutation *soft* '(:insert 5 2)))
                    '((2 . 2) (0 . 2) (1 . 1) (1 . 1) (0 . 0) (1 . 2))))
    (is (tree-equal
         (genome (apply-mutation *soft* '(:insert 2 1)))
         '((2 . 2) (0 . 0) (0 . 0) (1 . 2) (1 . 1) (1 . 1) (0 . 0) (1 . 2))))))

(deftest some-range-swap-mutations ()
  (with-fixture range
    (apply-mutation *soft* '(:swap 0 2))
    (is (tree-equal (lines *soft*)
                    '("three" "two" "one" "two" "two" "three")
                    :test #'string=))))

(deftest range-copy ()
  (with-fixture range (is (typep (copy *soft*) 'sw-range))))

(deftest range-single-point-crossover ()
  (with-fixture double-range
    (is (eq (reference *soft*) (reference *tfos*)))
    (let ((child (one-point-crossover *soft* *tfos*)))
      (is (typep child 'sw-range))
      (is (listp (genome child))))))

(deftest range-crossover ()
  (with-fixture double-range
    (let ((before-a (copy-tree (genome *soft*)))
          (before-b (copy-tree (genome *tfos*)))
          (child (crossover *soft* *tfos*)))
      (is (typep child 'sw-range))
      (is (listp (genome child)))
      ;; (is (not (null (edits child))))
      (is (eq (reference *soft*) (reference child)))
      (is (tree-equal before-a (genome *soft*)))
      (is (tree-equal before-b (genome *tfos*))))))


;;; Mutation analysis and statistics collection tests
(defvar *test* nil "Variable to hold evaluation function for tests.")

(defixture hello-world-clang-w-fitness
  (:setup
   (setf *hello-world*
         (from-file (make-instance 'clang :compiler "clang-3.7"
                                   :flags '("-g -m32 -O0"))
                    (hello-world-dir "hello_world.c"))
         *test* [#'length #'genome]
         *fitness-predicate* #'>
         *mutation-stats* (make-hash-table)
         *population* (list *hello-world*)))
  (:teardown
   (setf *hello-world* nil *test* nil *mutation-stats* (make-hash-table))))

(deftest mutation-stats-notices-fitness-improvement ()
  (with-fixture hello-world-clang-w-fitness
    (evaluate *test* *hello-world*)
    (is (numberp (fitness *hello-world*)))
    (let ((variant (copy *hello-world*))
          (op (make-instance 'clang-insert
                :targets '((:stmt1 . 1)
                           (:literal1 . "/* nothing */")))))
      (apply-mutation variant op)
      (is (null (fitness variant))
          "Fitness is null after `apply-mutation'")
      (analyze-mutation variant (list op nil nil *hello-world* nil nil) *test*)
      (is (not (null (fitness variant)))
          "`analyze-mutation' calculates fitness when missing")
      (let ((stats-alist (hash-table-alist *mutation-stats*)))
        (is (= (length stats-alist) 1) "Single element in stats")
        (is (equal :better (second (second (first stats-alist))))
            "`analyze-mutation' notices fitness improvement")))))

(deftest mutation-stats-notices-worsening ()
  (with-fixture hello-world-clang-w-fitness
    (evaluate *test* *hello-world*)
    (is (numberp (fitness *hello-world*)))
    (let ((variant (copy *hello-world*))
          (op (make-instance 'clang-cut :targets '((:stmt1 . 2)))))
      (apply-mutation variant op)
      (analyze-mutation variant (list op nil nil *hello-world* nil nil) *test*)
      (is (equal :worse (second (second (first (hash-table-alist
                                                *mutation-stats*)))))
          "`analyze-mutation' notices worse improvement"))))

(deftest mutation-stats-notices-same ()
  (with-fixture hello-world-clang-w-fitness
    (evaluate *test* *hello-world*)
    (is (numberp (fitness *hello-world*)))
    (let ((variant (copy *hello-world*))
          (op (make-instance 'clang-swap
                :targets '((:stmt1 . 2) (:stmt2 . 2)))))
      (setf (fitness variant) nil)
      (analyze-mutation variant (list op nil nil *hello-world* nil nil) *test*)
      (is (equal :same (second (second (first (hash-table-alist
                                               *mutation-stats*)))))
          "`analyze-mutation' notices no change: ~S"
          (hash-table-alist *mutation-stats*)))))

;; Ancestry tests
(defclass clang-w-ancestry (clang ancestral) ())

(defixture hello-world-clang-w-ancestry
  (:setup
   (setf software-evolution::*next-ancestry-id* 0
         *hello-world*
         (from-file (make-instance 'clang-w-ancestry :compiler "clang-3.7"
                                   :flags '("-g -m32 -O0"))
                    (hello-world-dir "hello_world.c"))
         *test* [#'length #'genome])
   (evaluate *test* *hello-world*))
  (:teardown
   (setf *hello-world* nil
         *test* nil
         software-evolution::*next-ancestry-id* 0)))

(deftest apply-mutation-logs-ancestry ()
  (with-fixture hello-world-clang-w-ancestry
    (let ((op (make-instance 'clang-cut
                :object *hello-world*
                :targets `((:stmt1 . ,(stmt-with-text *hello-world*
                                                      "return 0"))))))
      (apply-mutation *hello-world* op)
      (evaluate *test* *hello-world*)

      (is (< 1 (length (ancestors *hello-world*))))

      (is (= 1 (plist-get :id (first (ancestors *hello-world*)))))
      (is (not (null (plist-get :fitness (first (ancestors *hello-world*))))))
      (is (equal (type-of op)
                 (plist-get :mutant (first (ancestors *hello-world*)))))

      (is (= 0 (plist-get :id (second (ancestors *hello-world*)))))
      (is (equal 'from-file
                 (plist-get :how (second (ancestors *hello-world*))))))))

(deftest crossover-logs-ancestry ()
  (with-fixture hello-world-clang-w-ancestry
    (let ((crossed (crossover *hello-world* *hello-world*)))
      (is (< 1 (length (ancestors crossed))))

      (is (not (null (plist-get :crossover (first (ancestors crossed))))))
      (is (= 0 (plist-get :id
                          (first (plist-get :cross-with
                                            (first (ancestors crossed)))))))
      (is (equal
           'from-file
           (plist-get :how
                      (first (plist-get :cross-with
                                        (first (ancestors crossed))))))))))

(deftest graphing-ancestry ()
  (with-fixture hello-world-clang-w-ancestry
    (let ((crossed (crossover *hello-world* *hello-world*))
          (op (make-instance 'clang-cut
                :object *hello-world*
                :targets `((:stmt1 . ,(stmt-with-text *hello-world*
                                                      "return 0"))))))
      (apply-mutation crossed op)
      (with-temp-file (save-base)
        (multiple-value-bind (stdout stderr errno)
            (save-ancestry crossed
                           (pathname-directory save-base)
                           (pathname-name save-base))
          (declare (ignorable stdout stderr))
          (let ((svg (make-pathname :directory (pathname-directory save-base)
                                    :name (pathname-name save-base)
                                    :type "svg")))
            (when (probe-file svg) (delete-file svg)))
          (is (zerop errno)))))))


;;; Diff tests
(defmacro with-static-reference (software &rest body)
  (let ((ref-sym (gensym)))
    `(let ((,ref-sym (copy-tree (reference ,software))))
       ,@body
       (is (tree-equal ,ref-sym (reference ,software))))))

(deftest diff-size ()
  (with-fixture diff (is (= 4 (size *soft*)))))

(deftest diff-protects-reference ()
  (with-fixture diff
    (with-static-reference *soft*
      (setf (genome *soft*) nil)
      (is (tree-equal (reference *soft*)
                      '(((:CODE 1)) ((:CODE 2)) ((:CODE 3)) ((:CODE 4))))))))

(deftest diff-lines ()
  (with-fixture diff
    (with-static-reference *soft*
      (is (tree-equal (lines *soft*) '((1) (2) (3) (4)))))))

(deftest some-diff-cut-mutations ()
  (with-fixture diff
    (with-static-reference *soft*
      (is (tree-equal (genome (apply-mutation *soft* '(:cut 2)))
                      '(((:CODE 1)) ((:CODE 2)) ((:CODE 4)))))
      (is (tree-equal (genome (apply-mutation *soft* '(:cut 1)))
                      '(((:CODE 1)) ((:CODE 4)))))
      (is (tree-equal (genome (apply-mutation *soft* '(:cut 1)))
                      '(((:CODE 1))))))))

(deftest some-diff-insert-mutations ()
  (with-fixture diff
    (with-static-reference *soft*
      (is (tree-equal (genome (apply-mutation *soft* '(:insert 0 2)))
                      '(((:CODE 3)) ((:CODE 1)) ((:CODE 2))
                        ((:CODE 3)) ((:CODE 4))))))))

(deftest some-diff-swap-mutations ()
  (with-fixture diff
    (with-static-reference *soft*
      (is (tree-equal (genome (apply-mutation *soft* '(:swap 0 2)))
                      '(((:CODE 3)) ((:CODE 2)) ((:CODE 1)) ((:CODE 4))))))))

(deftest diff-copy ()
  (with-fixture diff (is (typep (copy *soft*) 'diff))))

(deftest diff-single-point-crossover ()
  (with-fixture double-diff
    (with-static-reference *soft*
      (is (tree-equal (reference *soft*) (reference *tfos*)))
      (let ((child (one-point-crossover *soft* *tfos*)))
        (is (typep child 'diff))
        (is (tree-equal (genome child) (genome *soft*)))))))

(deftest diff-array-protects-reference ()
  (with-fixture diff-array
    (with-static-reference *soft*
      (setf (genome *soft*) nil)
      (is (tree-equal (reference *soft*)
                      '(((:CODE 1)) ((:CODE 2)) ((:CODE 3)) ((:CODE 4))))))))

(deftest diff-array-lines ()
  (with-fixture diff-array
    (with-static-reference *soft*
      (is (tree-equal (lines *soft*) '((1) (2) (3) (4)))))))

(deftest some-diff-array-cut-mutations ()
  (with-fixture diff-array
    (with-static-reference *soft*
      (is (equalp (genome (apply-mutation *soft* '(:cut 2)))
                  #(((:CODE 1)) ((:CODE 2)) ((:CODE 4)))))
      (is (equalp (genome (apply-mutation *soft* '(:cut 1)))
                  #(((:CODE 1)) ((:CODE 4)))))
      (is (equalp (genome (apply-mutation *soft* '(:cut 1)))
                  #(((:CODE 1))))))))

(deftest some-diff-array-insert-mutations ()
  (with-fixture diff-array
    (with-static-reference *soft*
      (is (equalp (genome (apply-mutation *soft* '(:insert 0 2)))
                  #(((:CODE 3)) ((:CODE 1)) ((:CODE 2))
                    ((:CODE 3)) ((:CODE 4))))))))

(deftest some-diff-array-swap-mutations ()
  (with-fixture diff-array
    (with-static-reference *soft*
      (is (equalp (genome (apply-mutation *soft* '(:swap 0 2)))
                  #(((:CODE 3)) ((:CODE 2)) ((:CODE 1)) ((:CODE 4))))))))


;;; Population tests
(deftest evict-population ()
  (with-fixture population
    (let ((before (length *population*)))
      (is (> before (length (progn (evict) *population*)))))))

(deftest incorporate-population ()
  (with-fixture population
    (let* ((before (length *population*))
           (*max-population-size* (+ 1 before)))
      (is (< before (length (progn (incorporate (make-instance 'software))
                                   *population*)))))))

(deftest evolution-collects-no-statistics-by-default ()
  (let ((counter 0)
        (*fitness-predicate* #'>))
    (flet ((test (candidate)
             (declare (ignorable candidate))
             (incf counter)
             (if (= counter 5) 2 1)))
      (with-fixture population
        (evolve #'test :max-evals 10)
        (is (zerop (length (hash-table-alist *mutation-stats*))))))))

(deftest evolution-collects-statistics-when-asked ()
  (let ((counter 0)
        (*fitness-predicate* #'>)
        (*mutation-stats* (make-hash-table)))
    (flet ((test (candidate)
             (declare (ignorable candidate))
             (incf counter)
             (let ((out (if (> counter 10)
                            (- 0 counter)
                            counter)))
               out)))
      (with-fixture population
        ;; Should still signal errors.
        (let ((*soft-mutate-errors* t))
          (signals mutate (evolve #'test :max-evals 20 :mutation-stats t)))
        (evolve #'test :max-evals 20 :mutation-stats t)
        (is (equal '(:fake) (hash-table-keys *mutation-stats*)))
        ;; Error, and 1 more than :max-evals.
        (is (= 22 (length (gethash :fake *mutation-stats*))))
        (let ((statuses (mapcar #'cadr (gethash :fake *mutation-stats*))))
          (is (member :better statuses))
          (is (member :worse statuses))
          (is (member :error statuses)))))))

(deftest terminate-evolution-on-success ()
  (let ((counter 0))
    (flet ((test (candidate)
             (declare (ignorable candidate))
             (incf counter)
             (if (= counter 5) 2 1)))
      (with-fixture population
        (evolve #'test :target 2)
        (is (= *fitness-evals* 5))))))


;;; Helper functions to avoid hard-coded statement numbers.
(defun stmt-with-text (obj text)
  (->> (asts obj)
       (find-if [{string= text} #'peel-bananas {aget :src-text}])
       (aget :counter)))

(defun ast-with-text (obj text)
  (when-let ((id (stmt-with-text obj text)))
    (get-ast obj id)))

(defun stmt-starting-with-text (obj text)
  (let ((the-snippet
         (find-if
          (lambda (snippet)
            (and snippet
                 (equal 0
                        (search text
                                (peel-bananas (aget :src-text snippet))))))
          (asts obj))))
    (aget :counter the-snippet)))

(deftest swap-can-recontextualize ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*)))
      (apply-mutation variant
        (cons 'clang-swap
              (list (cons :stmt1 (stmt-with-text variant "n > 0"))
                    (cons :stmt2 (stmt-with-text variant "bc=0")))))
      (is (zerop (second (multiple-value-list (phenome variant))))))))

(defun diff-strings (original modified diff-region)
  "Convert a diff-region to a list of contents in ORIGINAL and MODIFIED."
  (flet ((diff-subseq (seq start length)
           (subseq seq start (+ start length))))
    (list (diff-subseq original
                       (diff::original-start diff-region)
                       (diff::original-length diff-region))
          (diff-subseq modified
                       (diff::modified-start diff-region)
                       (diff::modified-length diff-region)))))

(defun show-diff (original modified &optional (stream t))
  "Return a string diff of two software objects.
Useful for printing or returning differences in the REPL."
  (diff:render-diff (diff::generate-seq-diff 'DIFF:UNIFIED-DIFF
                                             (lines original)
                                             (lines modified))
                    stream))

(deftest swap-makes-expected-change ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*))
          (text-1 "n > 0")
          (text-2 "bc=0"))
      ;; Apply the swap mutation.
      (apply-mutation variant
        (cons 'clang-swap
              (list (cons :stmt1
                          (stmt-with-text variant text-1))
                    (cons :stmt2
                          (stmt-with-text variant text-2)))))
      ;; Each element should contain the text of one of the swapped
      ;; pieces with possibly different variable names.
      (every-is {scan (create-scanner (list :alternation text-1 text-2))}
                (remove-if
                 {string= ""}
                 (mapcar [{apply #'concatenate 'string}
                          {mapcar {apply #'concatenate 'string}}]
                         ;; Collect the differences between the
                         ;; original and the variant.
                         (mapcar {diff-strings (lines *huf*) (lines variant)}
                                 (remove-if-not
                                  [{equal 'diff:modified-diff-region} #'type-of]
                                  (diff::compute-raw-seq-diff
                                   (lines *huf*)
                                   (lines variant))))))))))

(deftest swap-at-different-levels-can-recontextualize ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*))
          (text-1 "n > 0")
          (text-2 "bn++"))
      ;; Apply the swap mutation.
      (apply-mutation variant
        (cons 'clang-swap
              (list (cons :stmt1
                          (stmt-with-text variant text-1))
                    (cons :stmt2
                          (stmt-with-text variant text-2)))))
      (let ((string-diffs
             (remove-if
              {string= ""}
              (mapcar [{apply #'concatenate 'string}
                       {mapcar {apply #'concatenate 'string}}]
                      ;; Collect the differences between the
                      ;; original and the variant.
                      (mapcar {diff-strings (lines *huf*) (lines variant)}
                              (remove-if-not
                               [{equal 'diff:modified-diff-region} #'type-of]
                               (diff::compute-raw-seq-diff
                                (lines *huf*)
                                (lines variant))))))))
        ;; Each element should contain the text of one of the swapped
        ;; pieces with possibly different variable names.
        (every-is {scan (create-scanner (list :alternation text-1 text-2))}
                  string-diffs)
        ;; Variables should not fail to be rebound due to no bound vars in
        ;; scope
        (every-is [{not} {scan "\/\* no bound vars in scope \/\*"}]
                  string-diffs)))))

(deftest insert-can-recontextualize ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*)))
      (apply-mutation variant
        (cons 'clang-insert
              (list (cons :stmt1
                          (stmt-with-text variant "bc=0"))
                    (cons :stmt2
                          (stmt-with-text variant "n > 0")))))
      (is (zerop (second (multiple-value-list (phenome variant))))))))

(deftest insert-makes-expected-change ()
  (with-fixture huf-clang
    (let ((variant (copy *huf*)))
      (apply-mutation variant
        (cons 'clang-insert
              (list (cons :stmt1 (stmt-with-text variant "bc=0"))
                    (cons :stmt2 (stmt-with-text variant "n > 0")))))
      ;; Original and modified strings of the difference.
      (destructuring-bind (original modified)
          (mapcar {apply #'concatenate 'string}
                  (first (mapcar {diff-strings (lines *huf*) (lines variant)}
                                 (remove-if-not
                                  [{equal 'diff:modified-diff-region}
                                   #'type-of]
                                  (diff::compute-raw-seq-diff
                                   (lines *huf*)
                                   (lines variant))))))
        (let ((size-o (length original))
              (size-m (length modified))
              (non-whitespace-orig
               (multiple-value-bind (match-p matches)
                   (scan-to-strings "^(\\s*)(\\S.*)" original)
                 (declare (ignorable match-p))
                 (aref matches 1))))
          ;; Modified should be longer.
          (is (> size-m size-o))
          ;; End of modified should be the original.
          (is (string= non-whitespace-orig
                       (subseq modified
                               (- size-m (length non-whitespace-orig))))))))))

;; When recontextualizing, function should be considered defined even
;; if its body is not present.
(deftest bodyless-function-is-not-recontextualized ()
  (let* ((obj (make-instance 'clang
                  :genome "void test(int x);
                          int main(int argc, char **argv) {
                            test(0); return 0;
                           }"))
         (stmt (stmt-with-text obj "test(0)"))
        (*matching-free-function-retains-name-bias* 1.0))
    (apply-mutation obj
                    `(clang-replace (:stmt1 . ,stmt) (:stmt2 . ,stmt)))
    (is (eq stmt (stmt-with-text obj "test(0)")))))

;; Check that the ASTLister traversal and the ASTMutate traversal see
;; the same number of ASTs.
(deftest ast-lister-finds-same-number-of-ids ()
  (with-fixture huf-clang
    (is (= (size *huf*)
           (count '#\Newline (clang-mutate *huf* '(:list)))))))

;; huf.c only contains one user function with 3 parameters,
;; check that random-function-name can find it.
(deftest finds-function-binding ()
  (with-fixture huf-clang
    (is (string= "inttobits"
                 (random-function-name (functions *huf*)
                                       :original-name "foo"
                                       :arity 3)))))


;;; Fix compilation tests.
(defvar *broken-clang* nil "")
(defvar *broken-gcc* nil "")

(defixture broken-compilation
  (:setup (setf *broken-clang*
                (make-instance 'clang-w-fodder
                  :genome "int main(int argc, char **argv) {
	printf(\"Hello, World!\\n\");
	return missing_variable;}"))
          (setf *database*
                (with-open-file (in (make-pathname :name "euler-example.json"
                                                   :directory +etc-dir+))
                  (make-instance 'json-database :json-stream in))))
  (:teardown (setf *database* nil)))

(defixture broken-compilation-gcc
  (:setup (setf *broken-gcc*
                (make-instance 'clang-w-fodder
                  :compiler "gcc"
                  :flags '("-m32" "-O0" "-g")
                  :genome "int main(int argc, char **argv) {
	printf(\"Hello, World!\\n\");
	return missing_variable;}"))
          (setf *database*
                (with-open-file (in (make-pathname :name "euler-example.json"
                                                   :directory +etc-dir+))
                  (make-instance 'json-database :json-stream in))))
  (:teardown (setf *database* nil)))

(deftest fix-compilation-inserts-missing-include ()
  (with-fixture broken-compilation
    (is (scan (format nil "\#include <~a>" "stdio.h")
              (genome-string (fix-compilation *broken-clang* 1)))))
  (with-fixture broken-compilation-gcc
    (is (scan (format nil "\#include <~a>" "stdio.h")
              (genome-string (fix-compilation *broken-gcc* 1))))))

(deftest fix-compilation-inserts-declaration-and-initializes ()
  (with-fixture broken-compilation
    (is (scan (quote-meta-chars "missing_variable =")
              (genome (fix-compilation *broken-clang* 4)))))
  (with-fixture broken-compilation-gcc
    (is (scan (quote-meta-chars "missing_variable =")
              (genome (fix-compilation *broken-gcc* 4))))))

(deftest fix-compilation-declare-var-as-pointer ()
  (with-temp-file (genome ".c")
    (string-to-file "int main(int argc, char **argv) {
                      int y = 0;
                      return *y;
                    }"
                    genome)
    (let ((broken-clang (from-file (make-instance 'clang
                                     :compiler "clang"
                                     :flags '("-m32" "-O0" "-g"))
                                   genome))
          (broken-gcc   (from-file (make-instance 'clang
                                     :compiler "clang"
                                     :flags '("-m32" "-O0" "-g"))
                                   genome)))
      (is (compile-p (fix-compilation broken-clang 1)))
      (is (compile-p (fix-compilation broken-gcc 1))))))

(deftest basic-2pt-crossover-works ()
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int d"))
           (a-stmt2 (stmt-with-text *scopes* "d = 5"))
           (b-stmt1 (stmt-with-text *scopes* "int e"))
           (b-stmt2 (stmt-with-text *scopes* "c = 10"))
           (target-a-pts (cons a-stmt1 a-stmt2)))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts))
        (is ok)
        (is (compile-p variant))
        (is (equal effective-a-pts target-a-pts))))))

(deftest crossover-can-match-nesting ()
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "--d"))
           (a-stmt2 (stmt-with-text *scopes* "int e"))
           (b-stmt1 (stmt-with-text *scopes* "c = 6"))
           (b-stmt2 (stmt-with-text *scopes* "e = 8"))
           (target-a-pts
            (cons (stmt-starting-with-text *scopes* "while (d > 0)")
                  a-stmt2)))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable variant a-pts b-pts))
        (is ok)
        (is (equal effective-a-pts target-a-pts)))))
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "--d"))
           (a-stmt2 (stmt-with-text *scopes* "int e"))
           (b-stmt1 (stmt-with-text *scopes* "a = 13"))
           (b-stmt2 (stmt-with-text *scopes* "c = 15"))
           (target-a-pts
            (cons (stmt-starting-with-text *scopes* "for (b = 2;")
                  (stmt-starting-with-text *scopes* "if (b == 7)"))))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable variant a-pts b-pts))
        (is ok)
        (is (equal effective-a-pts target-a-pts))))))

(deftest crossover-can-rebind-text ()
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int b"))
           (a-stmt2 (stmt-with-text *scopes* "int c"))
           (b-stmt1 (stmt-with-text *scopes* "a = 13"))
           (b-stmt2 (stmt-with-text *scopes* "a = 13")))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant)))))
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int b"))
           (a-stmt2 (stmt-with-text *scopes* "int c"))
           (b-stmt1 (stmt-with-text *scopes* "d = 5"))
           (b-stmt2 (stmt-with-text *scopes* "--d")))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant))))))

(deftest crossover-the-world ()
  ;; Entire text of a function
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int a"))
           (a-stmt2 (stmt-with-text *scopes* "return a + b + c"))
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant)))))))
  ;; A single statement (the first one)
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "int a"))
           (a-stmt2 a-stmt1)
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant)))))))
  ;; A single statement (the last one)
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "return a + b + c"))
           (a-stmt2 a-stmt1)
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant)))))))
  ;; A single complex statement
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-starting-with-text *scopes*
                                             "for (b = 2;"))
           (a-stmt2 a-stmt1)
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant)))))))
  ;; A statement and one of its descendants
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-starting-with-text *scopes*
                                             "for (b = 2;"))
           (a-stmt2 (stmt-with-text *scopes* "c = 4"))
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant)))))))
  ;; A statement and one of its ancestors
  (with-fixture scopes-clang
    (let* ((a-stmt1 (stmt-with-text *scopes* "c = 4"))
           (a-stmt2 (stmt-starting-with-text *scopes*
                                             "for (b = 2;"))
           (b-stmt1 a-stmt1)
           (b-stmt2 a-stmt2))
      (multiple-value-bind (variant a-pts b-pts ok effective-a-pts)
          (intraprocedural-2pt-crossover *scopes* *scopes*
                                         a-stmt1 a-stmt2
                                         b-stmt1 b-stmt2)
        (declare (ignorable a-pts b-pts effective-a-pts))
        (is ok)
        (is (compile-p variant))
        (is (= (length (asts *scopes*))
               (length (asts variant))))))))

(deftest single-decl-works ()
  (with-fixture scopes-clang
    (let ((ast (get-ast *scopes*
                        (stmt-with-text *scopes* "int a"))))
      (is (= 1 (length (aget :declares ast)))))))

(deftest multiple-decl-works ()
  (with-fixture scopes-clang
    (when-let* ((it (stmt-with-text *scopes* "int f, g"))
                (ast (get-ast *scopes* it)))
      (is (= 2 (length (aget :declares ast)))))))

(deftest delete-decl-stmts-works ()
  (with-fixture scopes-clang
    (let ((variant (copy *scopes*)))
      (apply-mutation
          variant
        `(cut-decl (:stmt1 . ,(stmt-with-text *scopes* "int a"))))
      (is (compile-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))
    (let ((variant (copy *scopes*)))
      (apply-mutation
          variant
        `(cut-decl (:stmt1 . ,(stmt-with-text *scopes* "int d"))))
      (is (compile-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))
    (when-let* ((variant (copy *scopes*))
                (id (stmt-with-text *scopes* "int f, g")))
      (apply-mutation variant `(cut-decl (:stmt1 . ,id)))
      (is (compile-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))))

(deftest swap-decls-works ()
  (with-fixture scopes-clang
    ;; Check if the basic swap-decls mutation works.
    (let ((variant (copy *scopes*))
          (*bad-asts*
           (list (get-ast *scopes*
                          (stmt-with-text *scopes* "int a")))))
      (apply-mutation variant
                      (make-instance 'swap-decls :object variant))
      (is (compile-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))
    ;; Check if swap-decls works when only one decl is in the
    ;; selected scope. The expected behavior is to crawl up to
    ;; the first enclosing scope with at least two decls.
    (let ((variant (copy *scopes*))
          (*bad-asts*
           (list (get-ast *scopes*
                          (stmt-with-text *scopes* "int d")))))
      (apply-mutation variant
                      (make-instance 'swap-decls :object variant))
      (is (compile-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))))

(deftest rename-variable-works ()
  (with-fixture scopes-clang
    (let ((variant (copy *scopes*))
          (*bad-asts*
           (list (get-ast *scopes*
                          (stmt-with-text *scopes* "b = 1")))))
      (apply-mutation variant
                      (make-instance 'rename-variable :object variant))
      (is (compile-p variant))
      (is (not (equal (genome-string *scopes*)
                      (genome-string variant)))))))


;;; Instrumentation tests
(defun count-fullable (obj)
  "Return a count of full statements parented by compound statements"
  (count-if {can-be-made-full-p obj} (asts obj)))

(defun read-trace (string)
  "Read a trace into a lisp objects."
  (let ((start 0))
    (iter (for (values piece end) =
               (read-from-string string nil :eof :start start))
          (until (eql piece :eof))
          (setf start end)
          (collect piece))))

(deftest instrumentation-insertion-test ()
  (with-fixture gcd-clang
    (let ((instrumented (instrument (copy *gcd*))))
      ;; Do we insert the right number of printf statements?
      (is (<= (* 2 (count-fullable *gcd*))
              (count-fullable instrumented)))
      ;; Instrumented compiles and runs.
      (with-temp-file (bin)
        (is (zerop (second (multiple-value-list
                            (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
          (declare (ignorable stdout))
          (is (zerop errno))
          (let ((trace (read-trace stderr)))
            (is (listp trace))
            (is (= (length trace)
                   (length (split-sequence
                               #\Newline stderr
                               :remove-empty-subseqs t))))))))))

(deftest instrumentation-insertion-w-points-test ()
  (with-fixture gcd-clang
    (let ((instrumented
           (handler-bind ((warning #'muffle-warning))
             (instrument (copy *gcd*)
               :points
               (iter (for i below (size *gcd*))
                     (if (evenp i)
                         (collect (list i :even))
                         (collect (list i :odd))))))))
      ;; Do we insert the right number of printf statements?
      (is (<= (* 3 (count-fullable *gcd*))
              (count-fullable instrumented)))
      ;; Instrumented compiles and runs.
      (with-temp-file (bin)
        (is (zerop (second (multiple-value-list
                            (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
          (declare (ignorable stdout))
          (is (zerop errno))
          (let ((trace (read-trace stderr)))
            (is (listp trace))
            (is (= (length trace)
                   (length (split-sequence
                               #\Newline stderr
                               :remove-empty-subseqs t))))))))))

(deftest instrumentation-insertion-w-trace-file-test ()
  (with-fixture gcd-clang
    (with-temp-file (trace)
      (with-temp-file (bin)
        (let ((instrumented
               (instrument (copy *gcd*) :trace-file trace)))
          (is (scan (quote-meta-chars trace) (genome-string instrumented)))
          (is (zerop (second (multiple-value-list
                              (phenome instrumented :bin bin)))))
          (is (probe-file bin))
          (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
            (declare (ignorable stdout stderr))
            (is (zerop errno))
            (is (probe-file trace))))))))

(deftest instrumentation-handles-missing-curlies-test ()
  (with-fixture gcd-wo-curlies-clang
    (let ((instrumented (instrument (copy *gcd*))))
      ;; Ensure we were able to instrument an else branch w/o curlies.
      (let* ((else-counter (stmt-with-text *gcd* "b = b - a"))
             (matcher (quote-meta-chars (format nil "(:C . ~d)" else-counter))))
        (is (scan matcher (genome instrumented)))
        ;; The next line (after flushing) should be the else branch.
        (let ((location (position-if {scan matcher} (lines instrumented))))
          (is (scan (quote-meta-chars "b = b - a")
                    (nth (+ 3 location) (lines instrumented))))))
      ;; Finally, lets be sure we still compile.
      (with-temp-file (bin)
        (is (zerop (second (multiple-value-list
                            (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
          (declare (ignorable stdout))
          (is (zerop errno))
          (is (listp (read-trace stderr))))))))

(deftest instrumentation-insertion-w-points-and-added-blocks-test ()
  (with-fixture gcd-wo-curlies-clang
    (let* ((cookie :test-cookie)
           (instrumented
            (instrument (copy *gcd*)
              :points
              `((,(aget :counter (ast-with-text *gcd* "b - a")) ,cookie)))))
      ;; Instrumented program holds the TEST-COOKIE line.
      (is (scan (quote-meta-chars (string cookie))
                (genome-string instrumented))
          "The point trace string ~S appears in the instrumented program text."
          (string cookie))
      ;; Instrumented compiles and runs.
      (with-temp-file (bin)
        (is (zerop (second (multiple-value-list
                            (phenome instrumented :bin bin)))))
        (is (probe-file bin))
        (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
          (declare (ignorable stdout))
          (is (zerop errno))
          (is (scan (quote-meta-chars (string cookie)) stderr)
              "The point trace string ~S appears in the program output."
              (string cookie)))))))

(defparameter unbound-vals-fn
  [{mapcar [#'peel-bananas #'car]} {aget :unbound-vals}]
  "Function to pull unbound variables from an AST for use in `var-instrument'.")

(deftest instrumentation-print-unbound-vars ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *gcd* :functions
                  (list {var-instrument *gcd* :unbound-vals unbound-vals-fn})))
    (is (scan (quote-meta-chars "fprintf(stderr, \"(:UNBOUND-VALS")
              (genome-string *gcd*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (second (multiple-value-list (phenome *gcd* :bin bin))))
          "Successfully compiled instrumented GCD.")
      (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
        (declare (ignorable stdout))
        (is (zerop errno))
        (let ((trace (read-trace stderr)))
          (is (listp trace) "We got a trace.")
          (is (= (length trace) (count-if {assoc :c} trace))
              "Counter in every trace element.")
          (is (= (length trace) (count-if {assoc :UNBOUND-VALS} trace))
              "Variable list in every trace element.")
          (is (> (length trace) (count-if {aget :UNBOUND-VALS} trace))
              "Variable list not populated in every trace element."))))))

(deftest instrumentation-print-in-scope-vars ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *gcd* :functions
                  (list {var-instrument *gcd* :scopes
                                        [{apply #'append} {aget :scopes}]})))
    (is (scan (quote-meta-chars "fprintf(stderr, \"(:SCOPES")
              (genome-string *gcd*))
        "We find code to print unbound variables in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (second (multiple-value-list (phenome *gcd* :bin bin))))
          "Successfully compiled instrumented GCD.")
      (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
        (declare (ignorable stdout))
        (is (zerop errno))
        (let ((trace (read-trace stderr)))
          (is (listp trace) "We got a trace.")
          (is (= (length trace) (count-if {assoc :c} trace))
              "Counter in every trace element.")
          (is (= (length trace) (count-if {assoc :SCOPES} trace))
              "Variable list in every trace element.")
          (is (> (length trace) (count-if {aget :SCOPES} trace))
              "Variable list not populated in every trace element.")
          (is (not (null (mappend {aget :SCOPES} trace)))
              "Variable list not always empty."))))))

(deftest instrumentation-print-argv ()
  (with-fixture gcd-clang
    (handler-bind ((warning #'muffle-warning))
      (genome (instrument *gcd* :print-argv t)))
    (is (scan (quote-meta-chars "fprintf(stderr, \"((:INPUT")
              (genome-string *gcd*))
        "We find code to print input in the instrumented source.")
    (with-temp-file (bin)
      (is (zerop (second (multiple-value-list (phenome *gcd* :bin bin))))
          "Successfully compiled instrumented GCD.")
      (multiple-value-bind (stdout stderr errno) (shell "~a 4 8" bin)
        (declare (ignorable stdout))
        (is (zerop errno))
        (let ((trace (read-trace stderr)))
          (is (listp trace) "We got a trace.")
          (is (= 1 (count-if {assoc :input} trace))
              "Input shown once in trace.")
          (is (car (mapcar [{= 3} {length} {aget :input}]
                           (remove-if-not {aget :input} trace)))
              "GCD has 3 inputs."))))))

(deftest instrumentation-handles-binary-search ()
  (with-fixture binary-search-clang
    (handler-bind ((warning #'muffle-warning))
      (instrument *binary-search*
        :functions
        (list
         {var-instrument *binary-search* :unbound-vals unbound-vals-fn})))))


;;; Tests of declaration and type databases on clang objects
(deftest binary-search-collects-types ()
  (with-fixture binary-search-clang
    (let ((collected-vars (mapcar #'car (hash-table-alist
                                         (declarations *binary-search*))))
          ;; TODO: Add "argv" to this list once the outstanding issue
          ;;       with char* types in clang-mutate is resolved.
          (variables (list "haystack" "i" "argc")))
      (mapc (lambda (var)
              (is (member var collected-vars :test #'equal)
                  "Includes ~s variable in the `declarations' hash." var))
            variables)
      (mapc (lambda (var)
              (is (type-of-var *binary-search* var)
                  "Includes ~s variable in the `declarations' hash." var))
            variables))))

(deftest huf-knows-types ()
  (with-fixture huf-clang
    (is (and (listp (types *huf*)) (not (null (types *huf*))))
        "Huf software objects has a type database.")
    (is (= 7 (count-if {aget :pointer} (types *huf*)))
        "Huf has seven pointer types.")
    (is (= 3 (count-if [#'not #'emptyp {aget :array}] (types *huf*)))
        "Huf has three array types.")
    (is (= 3 (count-if [{string= "int"} {aget :type}] (types *huf*)))
        "Huf has three different \"int\" types (some are array and pointer).")))

(deftest huf-finds-type-info-for-variables ()
  (with-fixture huf-clang
    (let ((type (type-of-var *huf* "strbit")))
      (is type "Found type for \"strbit\" in huf.")
      (is (string= "[*]" (aget :array type))
          "Variable \"strbit\" in huf is a dynamically sized array.")
      (is (not (aget :pointer type))
          "Variable \"strbit\" in huf is not a pointer."))))


;; Lisp representation
(defvar *clang-expr*  nil "The clang expression (lisp) software object.")
(defixture clang-expr
  (:setup
    (setf *clang-expr*
          (make-instance 'clang-expression
            :genome (copy-tree '(:+ 1 (:* 2 (:- 3 :y)))))))
  (:teardown
   (setf *clang-expr* nil)))

(deftest lisp-cut-first ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-cut :targets 0))
    (is (equal (genome *clang-expr*) '(1 (:* 2 (:- 3 :y)))))))

(deftest lisp-cut-leaf ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-cut :targets 1))
    (is (equal (genome *clang-expr*) '(:+ (:* 2 (:- 3 :y)))))))

(deftest lisp-cut-subtree ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-cut :targets 2))
    (is (equal (genome *clang-expr*) '(:+ 1)))))

#+(or ) ; TODO: Fix this (unused) function before turning on this test.
(deftest lisp-cut-function ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-cut :targets 3))
    (is (equal (genome *clang-expr*) '(:+ 1 (2 (:- 3 :y)))))))

(deftest lisp-swap-leaves ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-swap :targets '(1 4)))
    (is (equal (genome *clang-expr*) '(:+ 2 (:* 1 (:- 3 :y)))))))

(deftest lisp-swap-leaf-subtree ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-swap :targets '(1 5)))
    (is (equal (genome *clang-expr*) '(:+ (:- 3 :y) (:* 2 1))))))

(deftest lisp-swap-functions ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-swap :targets '(3 6)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:- 2 (:* 3 :y)))))))

;; FIXME: what is the correct behavior here?
(deftest lisp-swap-parent-child ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-swap :targets '(2 5)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:- 3 :y))))))

(deftest lisp-replace-leaves ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-replace :targets '(1 4)))
    (is (equal (genome *clang-expr*) '(:+ 2 (:* 2 (:- 3 :y)))))))

(deftest lisp-replace-leaf-subtree ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-replace :targets '(1 5)))
    (is (equal (genome *clang-expr*) '(:+ (:- 3 :y) (:* 2 (:- 3 :y)))))))

(deftest lisp-replace-parent-child ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-replace :targets '(2 5)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:- 3 :y))))))

(deftest lisp-replace-function ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr* (make-instance 'lisp-replace :targets '(3 6)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:- 2 (:- 3 :y)))))))


;; Mutations of clang expressions in Lisp form
(deftest change-operator-first ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr*
      (make-instance 'change-operator :targets '(0 :-)))
    (is (equal (genome *clang-expr*) '(:- 1 (:* 2 (:- 3 :y)))))))

(deftest change-operator-subtree ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr*
                    (make-instance 'change-operator :targets '(3 :+)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:+ 2 (:- 3 :y)))))))

(deftest double-constant ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr*
                    (make-instance 'change-constant :targets '(7 :double)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:* 2 (:- 6 :y)))))))

(deftest halve-constant ()
  (with-fixture clang-expr
    (apply-mutation *clang-expr*
                    (make-instance 'change-constant :targets '(7 :halve)))
    (is (equal (genome *clang-expr*) '(:+ 1 (:* 2 (:- 1 :y)))))))

(deftest mult-divide-leaf ()
        (with-fixture clang-expr
    (apply-mutation *clang-expr*
                    (make-instance 'mult-divide :targets 4))
    (is (equal (genome *clang-expr*) '(:+ 1 (:* (:/ (:* 2 2) 2) (:- 3 :y)))))))

(deftest mult-divide-subtree ()
        (with-fixture clang-expr
    (apply-mutation *clang-expr*
                    (make-instance 'mult-divide :targets 2))
    (is (equal (genome *clang-expr*) '(:+ 1 (:/ (:* (:* 2 (:- 3 :y)) 2) 2))))))

(deftest add-subtract-subtree ()
        (with-fixture clang-expr
    (apply-mutation *clang-expr*
                    (make-instance 'add-subtract :targets 2))
    (is (equal (genome *clang-expr*) '(:+ 1 (:- (:+ (:* 2 (:- 3 :y)) 1) 1))))))

(deftest subtract-add-subtree ()
        (with-fixture clang-expr
    (apply-mutation *clang-expr*
                    (make-instance 'subtract-add :targets 2))
    (is (equal (genome *clang-expr*) '(:+ 1 (:+ (:- (:* 2 (:- 3 :y)) 1) 1))))))


;; Evaluation of clang expressions in Lisp form
(deftest eval-number ()
  (is (equal (evaluate-expression 1 nil) '(1 "int"))))

(deftest eval-var ()
  (is (equal (evaluate-expression :a '((:a 1 "int"))) '(1 "int"))))

(deftest eval-function ()
  (is (equal (evaluate-expression '(:+ 1 :a) '((:a 2 "int"))) '(3 "int"))))

(deftest eval-division-truncates ()
  (is (equal (evaluate-expression '(:/ 3 2) nil) '(1 "int")))
  (is (equal (evaluate-expression '(:/ -3 2) nil) '(-1 "int"))))

(deftest eval-interior-max ()
  (multiple-value-bind (result interior-max)
      (evaluate-expression '(:- (:* 2 :a) 2) '((:a 3 "int")))
    (is (equal result '(4 "int")))
    (is (equal interior-max 6))))

(deftest eval-signals-on-undefined-variable ()
  (signals eval-error
    (evaluate-expression :a nil)))

(deftest eval-signals-on-unknown-type ()
  (signals eval-error
    (evaluate-expression "test" nil)))

(deftest eval-signals-on-unknown-function ()
  (signals eval-error
    (evaluate-expression '(:test 1 2) nil)))

(deftest eval-signals-on-wrong-arity ()
  (signals eval-error
    (evaluate-expression '(:+ 1 2 3) nil)))

(deftest eval-signals-on-illegal-pointer-ops ()
  (signals eval-error
    (evaluate-expression '(:* 2 (:+ :ptr 1)) '((:ptr 1234 "*char"))))
  (signals eval-error
    (evaluate-expression '(:/ 2 (:+ :ptr 1)) '((:ptr 1234 "*char")))))
