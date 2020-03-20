(defpackage :software-evolution-library/test/lisp
  (:nicknames :sel/test/lisp)
  (:use
   :gt/full
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/software/lisp)
  (:import-from :asdf
                :system-relative-pathname)
  (:import-from :software-evolution-library/software/parseable
                :source-text)
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

(deftest read-eval-preserved ()
  (let ((ast (convert 'lisp-ast "(defvar *day-seconds* (* 24 60 60))"))
        (quit nil)
        found?)
    (traverse-nodes ast
                    (lambda (node)
                      (if (typep node 'expression-result)
                          (if (equal (expression node) '(* 24 60 60))
                              (progn
                                (setf found? t)
                                quit)
                              :keep-going)
                          :keep-going)))
    (is found?)))

(deftest read-conditional-preserved ()
  (let* ((ast (convert 'lisp-ast "(list #+sbcl :sbcl #-sbcl :not-sbcl)"))
         (quit nil)
         found-sbcl? found-not-sbcl?)
    (traverse-nodes ast
                    (lambda (node)
                      (if (typep node 'expression-result)
                          (case (expression node)
                            (:sbcl (setf found-sbcl? t)
                                   quit)
                            (:not-sbcl (setf found-not-sbcl? t)
                                       quit)
                            (t :keep-going))
                          :keep-going)))
    (is (and found-sbcl? found-not-sbcl?))))

(deftest read-conditional-preserves-whitespace ()
  (is (equal "#+sbcl
t"
             (source-text
              (convert 'lisp-ast "#+sbcl
t")))))

(defun gather-features (ast)
  "Report all the features referenced in AST."
  (let ((features '()))
    (traverse-nodes ast
                    (lambda (node)
                      (if (typep node 'feature-expression-result)
                          (let ((feature-expression (flatten (ensure-list (feature-expression node)))))
                            (dolist (feature feature-expression)
                              (pushnew feature features))
                            :keep-going)
                          :keep-going)))
    (remove-if {member _ '(:or :and :not)}
               (flatten (mapcar #'expression features)))))

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
         (gather-features (convert 'lisp-ast example))))))

(deftest rewrite-empty-feature-expression ()
  (is (equal "#+(or) (coda-non-grata)"
             (let* ((ast (map-tree (lambda (node)
                                     (if (typep node 'feature-expression-result)
                                         (values
                                          (transform-feature-expression
                                           node
                                           (lambda (sign test expr)
                                             (if (null test)
                                                 (values sign
                                                         '(:or)
                                                         expr)
                                                 (values sign test expr))))
                                          t)
                                         node))
                                   (convert 'lisp-ast "#+() (coda-non-grata)"))))
               (source-text ast)))))

(defun flip-conditions (ast)
  (map-tree (lambda (node)
              (if (not (typep node 'feature-expression-result))
                  node
                  (values (transform-feature-expression
                           node
                           (lambda (sign test expr)
                             (values
                              (ecase sign
                                (#\+ #\-)
                                (#\- #\+))
                              test
                              expr)))
                          t)))
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
         (to-remove '())
         (ast
          (map-tree (lambda (n)
                      (if (typep n 'feature-expression-result)
                          (let* ((test (expression (feature-expression n)))
                                 (sign (feature-expression-sign n))
                                 (test
                                  (ecase sign
                                    (#\+ test)
                                    (#\- `(:not ,test)))))
                            (if (featurep test)
                                (values (expression n) t)
                                (progn
                                  (push n to-remove)
                                  (values n t))))
                          n))
                    ast))
         (ast
          (reduce (lambda (ast node)
                    (remove node ast :test #'node-equalp))
                  to-remove
                  :initial-value ast)))
    (is (equal "(list :foo)"
               (source-text ast)))))
