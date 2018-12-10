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
   (ignore-directories
     :initarg :ignore-directories
     :reader ignore-directories
     :initform (list "test" "tests" "node_modules")
     :documentation "List of directories to ignore when building the project."))
  (:documentation "Project specialization for javascript software objects."))

(defmethod initialize-instance :after ((javascript-project javascript-project)
                                       &key)
  (setf (component-class javascript-project)
        (or (component-class javascript-project) 'javascript)))

(defmethod from-file ((javascript-project javascript-project) project-dir)
  "Populate JAVASCRIPT-PROJECT from the source code in PROJECT-DIR.
* JAVASCRIPT-PROJECT to be populated from source in PROJECT-DIR
* PROJECT-DIR source code to populate JAVASCRIPT-PROJECT with
"
  (labels ((relativize-path (path)
             "Return a new PATH relative to PROJECT-DIR."
             (make-pathname
               :host (pathname-host path)
               :device (pathname-device path)
               :directory (cons :relative
                                (subseq
                                  (pathname-directory path)
                                  (length (pathname-directory project-dir))))
               :name (pathname-name path)
               :type (pathname-type path)
               :version (pathname-version path))))
    ;; Sanity check that the project directory exists
    (assert (probe-file project-dir)
            (javascript-project)
            "~a does not exist." project-dir)
    (setf (project-dir javascript-project) project-dir)

    (with-cwd (project-dir)
      ;; Sanity check that a package.json file exists
      (assert (probe-file "package.json")
              (javascript-project)
              "JavaScript projects require a package.json file.")

      ;; Load the package.json file
      (setf (package-spec javascript-project)
            (decode-json-from-string (file-to-string "package.json")))

      ;; Populate the project's file fields
      (setf (other-files javascript-project) nil)
      (walk-directory
        project-dir
        (lambda (file &aux (rel-path (relativize-path file))
                           (pkg (package-spec javascript-project)))
          (push (cons (namestring rel-path)
                      (from-file
                        (make-instance (component-class javascript-project)
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
                (evolve-files javascript-project)))
        :test (lambda (file &aux (rel-path (relativize-path file))
                                 (pkg (package-spec javascript-project)))
                ;; Heuristics for identifying files in the project:
                ;; 1) The file is not in an ignored directory.
                ;; 2) The file has a "js" extension.
                ;; 3) The file is listed as a "bin" in package.json.
                ;; 4) The file is listed as "main" in package.json.
                (or (and (not (find-if {search _ (pathname-directory rel-path)
                                               :test #'equalp}
                                       (ignore-directories javascript-project)
                                       :key [#'pathname-directory
                                             #'ensure-directory-pathname]))
                         (equal "js" (pathname-type rel-path)))
                    (find rel-path (aget :bin pkg)
                          :key [#'canonical-pathname #'cdr]
                          :test #'equal)
                    (equal rel-path (aget :main pkg)))))))

  javascript-project)

(defmethod phenome ((obj javascript-project) &key (bin (temp-file-name)))
  "Create a phenotype of the JAVASCRIPT-PROJECT.  In this case,
override the phenome method to output the genome of OBJ to BIN as JavaScript
is not a compiled language.
OBJ object to create a phenome for
BIN location where the phenome will be created on the filesystem"
  (to-file obj bin)
  (values bin 0 nil nil nil))
