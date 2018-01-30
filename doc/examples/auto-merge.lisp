(ql:quickload :software-evolution-library)
(ql:quickload :software-evolution-library/test)
(in-package :software-evolution-library)


;;; Three parts of the merge.
(defparameter *orig*
  (setf *sqrt*
        (from-file (make-instance 'clang)
                   (make-pathname :name "gcd-wo-curlies"
                                  :type "c"
                                  :directory sel/test::+gcd-dir+))))

(defparameter *fix*
  (setf *sqrt*
        (from-file (make-instance 'clang)
                   (make-pathname :name "gcd-wo-curlies-fix"
                                  :type "c"
                                  :directory sel/test::+gcd-dir+))))

(defparameter *prose*
  (setf *sqrt*
        (from-file (make-instance 'clang)
                   (make-pathname :name "gcd-wo-curlies-fix"
                                  :type "c"
                                  :directory sel/test::+gcd-dir+))))


;;; Three test suites.

