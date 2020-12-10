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
        :software-evolution-library/software/tree-sitter
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/project)
  (:export :javascript-project))
(in-package :software-evolution-library/software/javascript-project)
(in-readtable :curry-compose-reader-macros)

(define-software javascript-project (parseable-project) ()
  (:documentation "Project specialization for javascript software objects."))

;;; TODO Remove these once Resolve works with tree-sitter.
(defun find-javascript ()
  (let ((package
         (some #'find-package
               '(:software-evolution-library/software/javascript
                 :software-evolution-library/software/tree-sitter))))
    (or (and package
             (find-external-symbol (string 'javascript) package))
        (error "No available representation for Javascript ASTs."))))

(defun find-json ()
  (let ((package
         (some #'find-package
               '(:software-evolution-library/software/json
                 :software-evolution-library/software/tree-sitter))))
    (or (and package
             (find-external-symbol (string 'json) package))
        (error "No available representation for JSON ASTs."))))

(defmethod initialize-instance :after ((javascript-project javascript-project)
                                       &key)
  (setf (slot-value javascript-project 'component-class)
        (or (component-class javascript-project)
            (find-javascript))
        (slot-value javascript-project 'ignore-other-paths)
        (adjoin "node_modules/**/*" (ignore-other-paths javascript-project)
                :test #'equal)
        (slot-value javascript-project 'ignore-paths)
        (adjoin "node_modules/**/*" (ignore-paths javascript-project)
                :test #'equal)))

(defmethod collect-evolve-files ((project javascript-project) &aux result)
  (with-current-directory ((project-dir project))
    (assert (probe-file "package.json") ((project-dir project))
            "JavaScript project requires a package.json file in ~a."
            (project-dir project))
    (let ((package-spec (nest (decode-json-from-string)
                              (file-to-string "package.json"))))
      (walk-directory
        (project-dir project)
        (lambda (file)
          (push (cons (pathname-relativize (project-dir project) file)
                      (from-file (make-instance (component-class project))
                                 file))
                result))
        :test (lambda (file)
                ;; Heuristics for identifying files in the project:
                ;; 1) The file is not in an ignored directory.
                ;;    and the file has a "js" extension.
                ;; 2) The file is listed as a "bin" in package.json.
                ;; 3) The file is listed as "main" in package.json.
                (let ((rel-path (pathname-relativize (project-dir project)
                                                     file)))
                  (or (and (not (ignored-evolve-path-p project rel-path))
                           (equal "js" (pathname-type rel-path)))
                      (find rel-path (aget :bin package-spec)
                            :key [#'canonical-pathname #'cdr]
                            :test #'equal)
                      (equal rel-path (aget :main package-spec))))))))
  result)

(defmethod collect-other-files :around ((project javascript-project))
  "Wrapper to represent JSON files as JSON software objects instead of
simple text software objects."
  (mapcar (lambda (pair &aux (file (car pair)))
            (if (equal "json" (pathname-type file))
                (cons file (nest (from-file (make-instance (find-json)))
                                 (merge-pathnames-as-file (project-dir project)
                                                          file)))
                pair))
          (call-next-method)))

(defmethod phenome ((obj javascript-project) &key (bin (temp-file-name)))
  "Create a phenotype of the JAVASCRIPT-PROJECT.  In this case,
override the phenome method to return BIN where the genome of OBJ
is output.  JavaScript is not a compiled language, so we return the
genome instead of a binary.

OBJ object to create a phenome for
BIN location where the phenome will be created on the filesystem"
  (values bin 0 nil nil nil))

(defmethod phenome :around ((obj javascript-project) &key (bin (temp-file-name)))
  "Bind *build-dir* to BIN ensuring the genome of OBJ in written to BIN."
  (ensure-directories-exist (ensure-directory-pathname bin))
  (let ((*build-dir* bin))
    (call-next-method)))
