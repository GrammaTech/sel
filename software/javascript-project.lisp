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
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/software/json
        :software-evolution-library/software/javascript
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/project)
  (:export :javascript-project
           :package-spec))
(in-package :software-evolution-library/software/javascript-project)
(in-readtable :curry-compose-reader-macros)

(define-software javascript-project (parseable-project)
  ((package-spec
     :initarg :package-spec
     :accessor package-spec
     :initform nil
     :documentation "Javascript project specification from package.json file."))
  (:documentation "Project specialization for javascript software objects."))

(defmethod initialize-instance :after ((javascript-project javascript-project)
                                       &key)
  (setf (slot-value javascript-project 'component-class)
        (or (component-class javascript-project) 'javascript)
        (slot-value javascript-project 'ignore-other-paths)
        (append (ignore-other-paths javascript-project)
                (list "node_modules/**/*"))
        (slot-value javascript-project 'ignore-paths)
        (append (ignore-paths javascript-project)
                (list "node_modules/**/*"))))

(defmethod from-file :around ((obj javascript-project) path)
  ;; Ensure PATH is a directory
  (setf path (ensure-directory-pathname path))
  ;; Sanity check that a package.json file exists
  (assert (probe-file (merge-pathnames-as-file path "package.json")) (path)
          "JavaScript project requires a package.json file in ~a." path)
  (with-current-directory (path)
    ;; Load the package.json file
    (setf (package-spec obj)
          (decode-json-from-string (file-to-string "package.json"))))
  (call-next-method))

(defmethod collect-evolve-files ((project javascript-project) &aux result)
  (with-current-directory ((project-dir project))
    (walk-directory
      (project-dir project)
      (lambda (file)
        (let ((rel-path (pathname-relativize (project-dir project) file))
              (pkg (package-spec project)))
          (push (cons (namestring rel-path)
                      (from-file
                       (make-instance (component-class project)
                         :parsing-mode
                         (cond ((find rel-path (aget :bin pkg)
                                      :key [#'canonical-pathname #'cdr]
                                      :test #'equal)
                                :script)
                               ((equal rel-path
                                       (nest (canonical-pathname)
                                             (or (aget :main pkg)
                                                 "index.js")))
                                :script)
                               (t :module)))
                       file))
                result)))
      :test (lambda (file)
              ;; Heuristics for identifying files in the project:
              ;; 1) The file is not in an ignored directory.
              ;; 2) The file has a "js" extension.
              ;; 3) The file is listed as a "bin" in package.json.
              ;; 4) The file is listed as "main" in package.json.
              (let ((rel-path (pathname-relativize (project-dir project) file))
                    (pkg (package-spec project)))
                (or (and (not (ignored-evolve-path-p project rel-path))
                         (equal "js" (pathname-type rel-path)))
                    (find rel-path (aget :bin pkg)
                          :key [#'canonical-pathname #'cdr]
                          :test #'equal)
                    (equal rel-path (aget :main pkg)))))))
  result)

(defmethod phenome ((obj javascript-project) &key (bin (temp-file-name)))
  "Create a phenotype of the JAVASCRIPT-PROJECT.  In this case,
override the phenome method to output the genome of OBJ to BIN as JavaScript
is not a compiled language.
OBJ object to create a phenome for
BIN location where the phenome will be created on the filesystem"
  (to-file obj bin)
  (values bin 0 nil nil nil))
