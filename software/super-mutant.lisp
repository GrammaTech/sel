;;; super-mutant.lisp --- evaluate multiple mutants in a single
;;; phenome
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(define-software super-mutant (software)
  ((mutants :accessor mutants :initarg :mutants :initform nil)))

(defmethod phenome ((obj super-mutant) &key (bin (temp-file-name)))
  (declare (ignorable bin))
  "/tmp/phenome")

(defmethod evaluate ((test function) (super super-mutant)
                     &rest extra-keys &key &allow-other-keys)
  (declare (ignorable extra-keys))
  (flet
      ((make-phenome-proxy (phenome-results obj)
         (let ((proxy (copy obj)))
           ;; TODO: how much memory is used up by these temporary functions?
           ;; Will they be freed along with the associated object or do we
           ;; need to use `remove-method' or `fmakunbound' on them?
           (eval `(defmethod phenome ((proxy (eql ,proxy)) &key bin)
                    (values-list
                     (cons (if bin
                               (progn (shell "cp -a ~a ~a"
                                             ,(car phenome-results) bin)
                                      bin)
                               ,(car phenome-results))
                           ',(cdr phenome-results)))))
           proxy)))

    (with-temp-file (bin)
     ;; Compile the super-mutant
     (destructuring-bind (bin . rest)
         (multiple-value-list (phenome super :bin bin))
       (mapc (lambda-bind ((i obj))
               ;; Create a wrapper script to ensure the correct mutant
               ;; is evaluated
               (with-temp-file-of (wrapper)
                 (format nil "#!/bin/sh~%SUPER_MUTANT=~d ~a" i bin)
                 (shell "chmod +x ~s" wrapper)

                 ;; Perform normal evaluation on the proxy, which will
                 ;; return the wrapper script as its phenome
                 (multiple-value-bind (fit extra)
                     (evaluate test
                               (make-phenome-proxy (if bin
                                                       (cons wrapper rest)
                                                       (cons nil rest))
                                                   obj))

                   (setf (fitness obj) fit)
                   (setf (fitness-extra-data obj) extra))))
             (indexed (mutants super)))))))
