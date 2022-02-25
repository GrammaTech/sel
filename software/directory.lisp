;;; directory.lisp --- Functional Tree representation of a directory of software
(defpackage :software-evolution-library/software/directory
  (:nicknames :sel/software/directory :sel/sw/directory)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/command-line
        :software-evolution-library/software/simple
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/project
        :software-evolution-library/software/parseable)
  (:export :directory
           :evolve-files
           :other-files
           :ignore-paths
           :only-paths
           :ignore-other-paths
           :only-other-paths
           :all-files
           :pick-file))
(in-package :software-evolution-library/software/directory)
(in-readtable :curry-compose-reader-macros)

(define-software directory-project (project parseable) ()
  (:documentation "A directory of files and software.
- Genome (from parseable) holds the directory structure.
  By inheriting from parseable we get support for the many common
  lisp sequence functions which map over the genomes of parseable
  objects.

- evolve-files and other-files (from project) hold software objects"))

(defclass directory-or-file-ast (node ast)
  ((name :accessor name :initarg :name :type string
         :documentation "Name of the directory"))
  (:documentation "Node to hold a directory or a file."))

(define-node-class directory-ast (directory-or-file-ast)
  ((entries :accessor entries :initarg :entries :initform nil :type list
            :documentation "Entries (children) of the directory.")
   (child-slots :initform '(entries) :allocation :class))
  (:documentation "FT Node to hold a directory entry."))

(define-node-class file-ast (directory-or-file-ast)
  ((contents :accessor contents :initarg :contents :initform nil :type (or null node)
             :documentation "Contents of the file.")
   (child-slots :initform '((contents . 0)) :allocation :class))
  (:documentation "FT Node to hold a directory entry."))

(defgeneric mkdir-p (directory path)
  (:documentation "Ensure that PATH exists under DIRECTORY.")
  (:method ((dir directory-ast) (path list))
    (when (emptyp path) (return-from mkdir-p dir))
    (if-let ((subdir (find-if (op (string= (car path) (name _1))) (entries dir))))
      (mkdir-p subdir (cdr path))
      (progn (push (make-instance 'directory-ast :name (car path)) (entries dir))
             (mkdir-p dir path))))
  (:method (dir (path pathname))
    (mkdir-p dir (cdr (pathname-directory path))))
  (:method (dir (path string))
    (mkdir-p dir (pathname path))))

(defgeneric subdir (directory path)
  (:documentation "Return the contents of PATH under DIRECTORY.")
  (:method ((dir directory-ast) (path list))
    (when (emptyp path) (return-from subdir dir))
    (if-let ((subdir (find-if (op (string= (car path) (name _1))) dir)))
      (subdir subdir (cdr path))
      (error "subdirectory ~a not found in directory ~a" path (name dir))))
  (:method (dir (path pathname))
    (subdir dir (cdr (pathname-directory path))))
  (:method (dir (path string))
    (subdir dir (pathname path))))

(defgeneric filepath-to-treepath (directory filepath)
  (:documentation "Ensure that PATH exists under DIRECTORY.")
  (:method ((dir directory-ast) (path list))
    (when (emptyp path) (return-from filepath-to-treepath nil))
    (if-let ((index (position-if (op (string= (car path) (name _1))) (entries dir))))
      (cons index (filepath-to-treepath (subdir dir (list (car path))) (cdr path)))
      (error "subdirectory ~a not found in directory ~a" path (name dir))))
  (:method (dir (path pathname))
    (filepath-to-treepath dir (cdr (pathname-directory path))))
  (:method (dir (path string))
    (filepath-to-treepath dir (pathname path))))

(defgeneric (setf subdir) (new directory path)
  (:documentation "Save NEW into PATH under DIRECTORY.")
  (:method ((new ast) (obj directory-ast) (path list))
    (econd
      ((every #'stringp path)
       (setf (@ obj (filepath-to-treepath obj path)) new))
      ((every #'integerp path)
       (setf (@ obj path) new))))
  (:method ((new ast) (obj directory-ast) (path list))))

(defmethod collect-evolve-files :around ((obj directory-project))
  (let ((evolve-files (call-next-method)))
    (dolist (pair evolve-files evolve-files)
      (destructuring-bind (path software-object) pair
        ;; Ensure the directory path down to software exists.
        ;; Splice in the software's AST at this path.
        (mkdir-p obj path)
        (setf (subdir obj path) (genome software-object))))))

(defmethod collect-evolve-files ((obj directory-project) &aux result)
  (walk-directory (project-dir obj)
                  (op (let ((language (guess-language _1)))
                        (push (cons (pathname-relativize (project-dir obj) _1)
                                    (from-file (make-instance (if (find-class-safe language)
                                                                  language
                                                                  'simple))
                                               _1))
                              result)))
                  :test (op (and (text-file-p _1)
                                 (not (nest
                                       (ignored-evolve-path-p obj)
                                       (pathname-relativize (project-dir obj) _1))))))
  result)
