;;; javascript-project.lisp --- Projects composed of JavaScript objects
;;;
;;; Implements the core functionality of the software-evolution-library
;;; for nodejs projects.  The JavaScript project software object
;;; utilizes the nodejs project's @code{package.json} file to identify
;;; the files to parse and utilize.  This class is supported by the
;;; @code{javascript} software object class.
;;;
;;; For proper operation, installation of nodejs and the npm package
;;; manager is required.  Additionally, as this class is dependent
;;; on the @code{javascript} class, dependencies for this class
;;; are also required.
;;;
;;; @texi{javascript-project}
(defpackage :software-evolution-library/software/javascript-project
  (:nicknames :sel/software/javascript-project
              :sel/sw/javascript-project)
  (:use :common-lisp
        :cl-json
        :named-readtables
        :curry-compose-reader-macros
        :uiop/pathname
        :software-evolution-library
        :software-evolution-library/software/json
        :software-evolution-library/software/javascript
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/project
        :software-evolution-library/utility)
  (:export :javascript-project
           :package-spec))
(in-package :software-evolution-library/software/javascript-project)
(in-readtable :curry-compose-reader-macros)

(define-software javascript-project (parseable-project)
  ((package-spec
     :initarg :package-spec
     :accessor package-spec
     :initform nil
     :documentation "Javascript project specification from package.json file.")
   (ignore-paths
    :initarg :ignore-paths
    :reader ignore-paths
    :initform (list "test/*" "tests/*" "node_modules/*")
    :documentation
    "List of paths to ignore when collecting evolve-files.
This default value is particular to node.js JavaScript projects."))
  (:documentation "Project specialization for javascript software objects."))

(defmethod initialize-instance :after ((javascript-project javascript-project)
                                       &key)
  (setf (slot-value javascript-project 'component-class)
        (or (component-class javascript-project) 'javascript)))

(defmethod from-file :around ((obj javascript-project) path)
  ;; Sanity check that a package.json file exists
  (assert (probe-file (merge-pathnames-as-file path "package.json")) (path)
          "JavaScript project requires a package.json file in ~a." path)
  (with-cwd (path)
    ;; Load the package.json file
    (setf (package-spec obj)
          (decode-json-from-string (file-to-string "package.json"))))
  (call-next-method))

(defmethod collect-evolve-files ((obj javascript-project) &aux result)
  (with-cwd ((project-dir obj))
    (walk-directory
        (project-dir obj)
      (lambda (file &aux (rel-path (pathname-relativize (project-dir obj) file))
                 (pkg (package-spec obj)))
        (push (cons (namestring rel-path)
                    (from-file
                     (make-instance (component-class obj)
                       :parsing-mode
                       (cond ((find rel-path (aget :bin pkg)
                                    :key [#'canonical-pathname #'cdr]
                                    :test #'equal)
                              :script)
                             ((equal rel-path
                                     (canonical-pathname
                                      (or (aget :main pkg)
                                          "index.js")))
                              :script)
                             (t :module)))
                     file))
              result))
      :test (lambda (file &aux
                       (rel-path (pathname-relativize (project-dir obj) file))
                       (pkg (package-spec obj)))
              ;; Heuristics for identifying files in the project:
              ;; 1) The file is not in an ignored directory.
              ;; 2) The file has a "js" extension.
              ;; 3) The file is listed as a "bin" in package.json.
              ;; 4) The file is listed as "main" in package.json.
              (or (and (not (ignored-evolve-path-p obj rel-path))
                       (equal "js" (pathname-type rel-path)))
                  (find rel-path (aget :bin pkg)
                        :key [#'canonical-pathname #'cdr]
                        :test #'equal)
                  (equal rel-path (aget :main pkg))))))
  result)

(defmethod phenome ((obj javascript-project) &key (bin (temp-file-name)))
  "Create a phenotype of the JAVASCRIPT-PROJECT.  In this case,
override the phenome method to output the genome of OBJ to BIN as JavaScript
is not a compiled language.
OBJ object to create a phenome for
BIN location where the phenome will be created on the filesystem"
  (to-file obj bin)
  (values bin 0 nil nil nil))
