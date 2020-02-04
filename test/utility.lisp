;;;; utility.lisp --- Utility tests.
(defpackage :software-evolution-library/test/utility
  (:nicknames :sel/test/utility)
  (:use
   :common-lisp
   :alexandria
   :closer-mop
   :software-evolution-library/test/util
   :software-evolution-library/stefil-plus
   :named-readtables
   :curry-compose-reader-macros
   :iterate
   :split-sequence
   :cl-ppcre
   :cl-store
   #+gt :testbot
   :software-evolution-library
   :software-evolution-library/utility)
  (:import-from :uiop :nest)
  (:shadowing-import-from
   :closer-mop
   :standard-method :standard-class :standard-generic-function
   :defmethod :defgeneric)
  (:export :test-utility))
(in-package :software-evolution-library/test/utility)
(in-readtable :curry-compose-reader-macros)
(defsuite test-utility "Utility tests.")

(deftest intersects-does-not-include-endpoints ()
  (is (not (intersects (make-instance 'range :begin 0 :end 1)
                       (make-instance 'range :begin 1 :end 2))))
  (is (not (intersects (make-instance 'source-range
                         :begin (make-instance 'source-location :line 1
                                               :column 0)
                         :end   (make-instance 'source-location :line 2
                                               :column 0))
                       (make-instance 'source-range
                         :begin  (make-instance 'source-location :line 2
                                                :column 0)
                         :end    (make-instance 'source-location :line 3
                                                :column 0))))))

(deftest pad-list-expand-to-requisite-length ()
  (is (equal '(1 2 3 3) (pad '(1 2) 4 3))))

(deftest pad-list-already-of-requisite-length ()
  (is (equal '(1 2 3) (pad '(1 2 3) 3))))

(deftest file-to-string-non-utf8-encoding ()
  #-ccl       ; CCL silently reads w/o warning despite bad encoding...
  (let ((path (make-pathname :directory +etc-dir+ :defaults "latin-1.c")))
    (is (string= (file-to-string path)
                 "/* Here is a non-ASCII character: § */
"))))

(deftest in-directory-with-trailing-slash ()
  (is (equal (in-directory #P"/tmp/build/" #P"src/test.c")
             #P"/tmp/build/src/test.c")))

(deftest in-directory-no-trailing-slash ()
  (is (equal (in-directory #P"/tmp/build" #P"src/test.c")
             #P"/tmp/build/src/test.c")))

(deftest which-test ()
  (is (null (which "dsjafpoarue")))
  (is (not (null (which #-windows "ls" #+windows "cmd.exe")))))

#-windows
(deftest shell-directory-test ()
  (multiple-value-bind (stdout stderr errno)
      (shell "pwd" :directory "/tmp/")
    (is (equal "/tmp" (trim-whitespace stdout)))
    (is (equal "" stderr))
    (is (zerop errno))))

#+windows
(deftest shell-directory-test ()
  (let* ((temp-file (pathname (sel/utility:temp-file-name)))
         (temp-dir (make-pathname :directory (pathname-directory temp-file)
                                  :device (pathname-device temp-file))))
    (multiple-value-bind (stdout stderr errno)
        (shell "chdir" :directory (namestring temp-dir))
      (let ((dir (sel/utility:trim-whitespace stdout)))
        ;; append slash at end if necessary to make it a directory
        (unless (or (char= (char dir (1- (length dir))) #\\)
                    (char= (char dir (1- (length dir))) #\/))
          (setf dir (concatenate 'string dir "/")))
        (is (equal temp-dir (pathname dir))) ; compare pathnames
        (is (equal "" stderr))
        (is (zerop errno))))))

#-windows ; IO-SHELL not yet supported on Windows
(deftest (read-and-write-shell-files :long-running) ()
  (let ((test-string "Hello world. Hello world. Hello world."))
    (is (string= test-string
                 (handler-case
                     (with-temp-file (temp.xz)
                       (write-shell-file (out temp.xz "xz")
                                         (write-line test-string out))
                       ;; NOTE: sleep one second to account for rare stochastic
                       ;; end-of-file errors in which I think we're going from
                       ;; writing to reading too quickly for the file system.
                       (sleep 1)
                       (read-shell-file (in temp.xz "xzcat")
                                        (read-line in)))
                   (error (c) (declare (ignorable c)) nil))))))

#-windows
(deftest (read-and-write-bytes-shell-files :long-running) ()
  (let ((byte #x25))
    (is (equal byte
               (handler-case
                   (with-temp-file (temp.xz)
                     (write-shell-file (out temp.xz "xz")
                                       (write-byte byte out))
                     (sleep 1) ; see note in `read-and-write-shell-files'
                     (read-shell-file (in temp.xz "xzcat")
                                      (read-byte in)))
                 (error (c) (declare (ignorable c)) nil))))))

#-windows
(deftest (cl-store-read-and-write-shell-files :long-running) ()
  (let ((it (make-instance 'software :fitness 37)))
    (is (= (fitness it)
           (handler-case
               (with-temp-file (temp.xz)
                 (write-shell-file (out temp.xz "xz")
                                   (store it out))
                 (sleep 1) ; see note in `read-and-write-shell-files'
                 (read-shell-file (in temp.xz "xzcat")
                                  (fitness (restore in))))
             (error (c) (declare (ignorable c)) nil))))))

(deftest cartesian-test ()
  (is (equal '(()) (cartesian nil)))
  (is (equal '((1)) (cartesian '((1)))))
  (is (equal '((1 2) (2 2) (1 3) (2 3))
             (cartesian '((1 2) (2 3))))))

(deftest cartesian-without-duplicates-test ()
  (is (equal '(()) (cartesian-without-duplicates nil)))
  (is (equal '((1)) (cartesian-without-duplicates '((1)))))
  (is (equal '((1 2) (1 3) (2 3))
             (cartesian-without-duplicates '((1 2) (2 3))))))

