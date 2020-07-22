(defpackage :software-evolution-library/test/lisp
  (:nicknames :sel/test/lisp)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/software/parseable
   :software-evolution-library/software/lisp)
  (:import-from :asdf
                :system-relative-pathname)
  (:import-from :software-evolution-library/software/lisp
                :*string*)
  (:import-from :cl-interpol)
  (:export :test-lisp))
(in-package :software-evolution-library/test/lisp)

;;; Set up a readtable using cl-interpol (with interpolation
;;; disabled).. This gives us a nice way of quoting Lisp code as
;;; strings, usind #?(...), so the editor sees it as Lisp but the
;;; reader sees it as a string.

(eval-always
 (defun interpol-reader-no-interpol (s c arg)
   (let ((cl-interpol:*inner-delimiters* '()))
     (cl-interpol:interpol-reader s c arg)))

 (defreadtable lisp-test
     (:fuse :standard :curry-compose-reader-macros)
   (:dispatch-macro-char #\# #\? 'interpol-reader-no-interpol)))

(in-readtable lisp-test)

(defsuite test-lisp "Lisp representation")


;;; Utility
(defgeneric is-convert-round-trip-p (form)
  (:documentation "Checks whether FORM is the same after a
round-trip through convert.")
  (:method ((form string))
    (is (equal form (source-text (convert 'lisp-ast form)))))
  (:method ((form sequence))
    (is (equal (string-downcase (format nil "~s" form))
               (source-text (convert 'lisp-ast form))))))


;;; Tests
(deftest expression-equality ()
  "Take the expression slot into account when comparing Lisp ASTs."
  (is (equal? (convert 'lisp-ast "(- (+ 2 1) (- 2 3))")
              (convert 'lisp-ast "(- (+ 2 1) (- 2 3))")))
  (is (equal? (convert 'lisp-ast "(+ (- 2 1) (+ 2 3))")
              (convert 'lisp-ast "(+ (- 2 1) (+ 2 3))")))
  (is (not (equal? (convert 'lisp-ast "(+ (- 2 1) (+ 2 3))")
                   (convert 'lisp-ast "(- (+ 2 1) (- 2 3))"))))
  (is (equal? (convert 'lisp-ast "#+(ccl) 1")
              (convert 'lisp-ast "#+(ccl) 1")))
  (is (not (equal? (convert 'lisp-ast "#+(ccl) 1")
                   (convert 'lisp-ast "#-(ccl) 1")))))

(deftest read-eval-preserved ()
  (let ((found?
         (block found
           (mapc (lambda (node)
                   (if (and (typep node 'expression-result)
                            (equal (expression node) '(* 24 60 60)))
                       (return-from found t)))
                 (convert 'lisp-ast "(defvar *day-seconds* (* 24 60 60))"))
           nil)))
    (is found?)))

(deftest read-conditional-preserved ()
  (let (found-sbcl? found-not-sbcl?)
    (mapc (lambda (node)
            (when (typep node 'expression-result)
              (case (expression node)
                (:sbcl (setf found-sbcl? t))
                (:not-sbcl (setf found-not-sbcl? t)))))
          (convert 'lisp-ast "(list #+sbcl :sbcl #-sbcl :not-sbcl)"))
    (is (and found-sbcl? found-not-sbcl?))))

(deftest read-conditional-preserves-whitespace ()
  (is (equal "#+sbcl
t"
             (source-text
              (convert 'lisp-ast "#+sbcl
t")))))

(defun gather-features (ast)
  "Report all the features referenced in AST."
  (nest
   (remove-if {member _ '(:or :and :not)})
   remove-duplicates
   (serapeum:collecting)
   (walk-feature-expressions (lambda (featurex)
                               (dolist (feature (flatten (ensure-list featurex)))
                                 (collect feature)))
                             ast)))

(deftest test-gather-features ()
  (let ((example
         ;; A real bit of code from ASDF.
         #?(let* ((i (first (input-files o c)))
                  (f (compile-file-pathname
                      i #+clasp :output-type #+ecl :type #+(or clasp ecl) :fasl
                      #+mkcl :fasl-p #+mkcl t)))
             `(,f ;; the fasl is the primary output, in first position
               #+clasp
               ,@(unless nil ;; was (use-ecl-byte-compiler-p)
                   `(,(compile-file-pathname i :output-type :object)))
               #+clisp
               ,@`(,(make-pathname :type "lib" :defaults f))
               #+ecl
               ,@(unless (use-ecl-byte-compiler-p)
                   `(,(compile-file-pathname i :type :object)))
               #+mkcl
               ,(compile-file-pathname i :fasl-p nil) ;; object file
               ,@(when (and *warnings-file-type* (not (builtin-system-p (component-system c))))
                   `(,(make-pathname :type *warnings-file-type* :defaults f)))))))
    (is (set-equal
         '(:clasp :ecl :mkcl :clisp)
         (gather-features (convert 'lisp-ast example))))

    (is (set-equal
         '(:sbcl :unix :windows)
         (gather-features (convert 'lisp-ast "#+sbcl #+unix t #+sbcl #+windows nil"))))))

(defun rewrite-empty (ast)
  (map-reader-conditionals
   (lambda (sign featurex ex)
     (values sign
             (if (null featurex) '(:or) featurex)
             ex))
   ast))

(deftest test-rewrite-empty-feature-expression ()
  (is (equal "#+(or) (coda-non-grata)"
             (let* ((ast (convert 'lisp-ast "#+() (coda-non-grata)")))
               (source-text (rewrite-empty ast)))))
  (is (equal "#+sbcl #+(or) (coda-non-grata)"
             (let* ((ast (convert 'lisp-ast "#+sbcl #+() (coda-non-grata)")))
               (source-text (rewrite-empty ast))))))

(defun flip-conditions (ast)
  (map-reader-conditionals (lambda (sign featurex ex)
                             (values
                              (ecase sign
                                (#\+ #\-)
                                (#\- #\+))
                              featurex
                              ex))
                           ast))

(deftest test-flip-conditions ()
  (is (equal "(list #-sbcl :sbcl #+sbcl :not-sbcl)"
             (source-text
              (flip-conditions
               (convert 'lisp-ast "(list #+sbcl :sbcl #-sbcl :not-sbcl)"))))))

(deftest test-remove-feature-expressions ()
  (let* ((*features* (cons :foo *features*))
         (string
          "(list #+foo :foo #-foo :bar)")
         (ast (convert 'lisp-ast string))
         (ast (map-reader-conditionals
               (lambda (sign featurex ex)
                 (let ((test
                        (ecase sign
                          (#\+ featurex)
                          (#\- `(:not ,featurex)))))
                   (if (featurep test)
                       ;; Trivial, only the expression is kept.
                       (values #\- '(:or) ex)
                       ;; Impossible, the whole guard is removed.
                       (values #\+ '(:or) ex))))
               ast
               :remove-newly-empty t)))
    (is (equal "(list :foo)"
               (source-text ast)))))

(deftest test-remove-features ()
  (is (equal :mcl (remove-expression-features :mcl '(:genera))))
  (is (equal '() (remove-expression-features :genera '(:genera))))
  (is (equal '() (remove-expression-features '(:or :genera) '(:genera))))
  (is (equal '() (remove-expression-features '(:and :genera) '(:genera))))
  (is (equal '() (remove-expression-features '(:and :genera) '(:genera))))
  (is (equal :mcl (remove-expression-features '(:or :genera :mcl) '(:genera))))
  (is (equal :mcl (remove-expression-features '(:and :genera :mcl) '(:genera)))))

(deftest test-strip-feature ()
  (let ((start
         #?(uiop/package:define-package :uiop/common-lisp
               (:nicknames :uoip/cl)
             (:use :uiop/package)
             (:use-reexport #-genera :common-lisp #+genera :future-common-lisp)
             #+allegro (:intern #:*acl-warn-save*)
             #+cormanlisp (:shadow #:user-homedir-pathname)
             #+cormanlisp
             (:export
              #:logical-pathname #:translate-logical-pathname
              #:make-broadcast-stream #:file-namestring)
             #+genera (:shadowing-import-from :scl #:boolean)
             #+genera (:export #:boolean #:ensure-directories-exist #:read-sequence #:write-sequence)
             #+(or mcl cmucl) (:shadow #:user-homedir-pathname)))
        (goal
         #?(uiop/package:define-package :uiop/common-lisp
               (:nicknames :uoip/cl)
             (:use :uiop/package)
             (:use-reexport :common-lisp)
             #+allegro (:intern #:*acl-warn-save*)
             #+cormanlisp (:shadow #:user-homedir-pathname)
             #+cormanlisp
             (:export
              #:logical-pathname #:translate-logical-pathname
              #:make-broadcast-stream #:file-namestring)
             #+(or mcl cmucl) (:shadow #:user-homedir-pathname))))
    (is (equal (collapse-whitespace goal)
               (collapse-whitespace
                (source-text (remove-feature-support (convert 'lisp-ast start)
                                                     '(:genera))))))))

(deftest test-strip-feature-nested ()
  (let ((string
         #?(list
            1
            #+(or genera mcl)
            (list #+genera 2 #+mcl 3)
            4))
        (goal
         #?(list 1
                 #+mcl
                 (list #+mcl 3)
                 4)))
    (is (equal (collapse-whitespace goal)
               (collapse-whitespace
                (source-text
                 (remove-feature-support
                  (convert 'lisp-ast string)
                  '(:genera))))))))

(deftest test-rename-feature ()
  (let ((string
         #?(#+openmcl t #+clozure t #+ccl t #-(or openmcl clozure ccl) t)))
    (is (equal #?(#+ccl t #+ccl t #+ccl t #-ccl t)
               (source-text
                (map-feature-expressions
                 (lambda (featurex)
                   (transform-feature-expression
                    featurex
                    (lambda (feature)
                      (case feature
                        (:openmcl :ccl)
                        (:clozure :ccl)
                        (t feature)))))
                 (convert 'lisp-ast string)))))))

(deftest test-feature-expansion ()
  (let ((fn (lambda (featurex)
              (if (featurep-with featurex '(:x))
                  `(:or ,featurex :z)
                  featurex))))
    (is (equal '(:or :x :z :y)
               (transform-feature-expression '(:or :x :y) fn)))))

(deftest can-parse-a-lisp-software-object ()
  (let ((obj (from-string (make-instance 'lisp)
                          "(- (+ 1 2) 5)")))
    (is (= 16 (size (genome obj))))))

(deftest test-round-trip-sharpsign-dot ()
  ;; Strings with `#.' converted to 'lisp-ast and back are the same.
  (is-convert-round-trip-p #?(+ #.1 #.2)))

(deftest test-round-trip-sharpsign-quote ()
  ;; Strings with "#'" converted to 'lisp-ast and back are the same.
  (is-convert-round-trip-p #?(mapcar #'- '(1 2 3 4 5))))

(deftest test-sharpsign-quote-is-present ()
  ;; Strings with "#'" converted to 'lisp-ast have a sharpsign-quote node.
  (is (find-if {typep _ 'sharpsign-quote}
               (convert 'lisp-ast #?(mapc #'- '(1))))))

(deftest test-round-trip-uninterned-symbol ()
  ;; Forms with `#:' converted to 'lisp-ast and back are the same.
  ;; Test with string.
  (is-convert-round-trip-p #?(test #:test))
  ;; Test with sequence.
  (is-convert-round-trip-p '(test #:test)))

(deftest test-uninterned-symbol-is-present ()
  ;; Uninterned symbols converted to 'lisp-ast stay uninterned.
  ;; Test with string.
  (is (find-if [{string= "#:TEST"} {format nil "~s"} #'expression]
               (convert 'lisp-ast #?(#:test))))
  ;; Test with sequence.
  (is (find-if [{string= "#:TEST"} {format nil "~s"} #'expression]
               (convert 'lisp-ast '#:test))))
