;;; javascript-project.lisp --- Projects composed of JavaScript objects
(defpackage :software-evolution-library/software/javascript-project
  (:nicknames :sel/software/javascript-project
              :sel/sw/javascript-project)
  (:use :common-lisp
        :cl-json
        :software-evolution-library
        :software-evolution-library/software/javascript
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/project
        :software-evolution-library/utility)
  (:export :javascript-project
           :package-spec
           :javascript-class))
(in-package :software-evolution-library/software/javascript-project)

(define-software javascript-project (parseable-project)
  ((package-spec
     :initarg :package-spec
     :accessor package-spec
     :initform nil
     :documentation "Javascript project specification from package.json file.")
   (javascript-class
     :initarg :javascript-class
     :accessor javascript-class
     :initform 'javascript
     :documentation "JavaScript subclass to utilize in the project."))
  (:documentation "Project specialization for javascript software objects."))

(defmethod from-file ((javascript-project javascript-project) project-dir)
  "Populate JAVASCRIPT-PROJECT from the source code in PROJECT-DIR.
* JAVASCRIPT-PROJECT to be populated from source in PROJECT-DIR
* PROJECT-DIR source code to populate JAVASCRIPT-PROJECT with
"
  ;; Sanity check that the project directory exists
  (assert (probe-file project-dir)
          (javascript-project)
          "~a does not exist" project-dir)
  (setf (project-dir javascript-project) project-dir)

  (with-cwd (project-dir)
    ;; Sanity check that a package.json file exists
    (assert (probe-file "package.json")
            (javascript-project)
            "Calling `from-file` on a javascript-project requires a project ~
             with a package.json file")

    ;; Load the package.json file
    (setf (package-spec javascript-project)
          (decode-json-from-string (file-to-string "package.json")))

    ;; Sanity check the package.json file contents
    (assert (or (probe-file (aget :main (package-spec javascript-project)))
                (probe-file "index.js"))
            (javascript-project)
            "JavaScript project entry point is not present.")
    (mapcar (lambda (file)
               (assert (probe-file file)
                       (javascript-project)
                       "~a is not present in the JavaScript project." file))
            (aget :files (package-spec javascript-project)))

    ;; Populate the project's file fields
    (setf (other-files javascript-project) nil)
    (setf (evolve-files javascript-project)
          (mapcar
            (lambda (file)
              (cons file
                    (from-file (make-instance
                                 (javascript-class javascript-project))
                               file)))
            (cons (if (probe-file (aget :main (package-spec javascript-project)))
                      (aget :main (package-spec javascript-project))
                      "index.js")
                  (aget :files (package-spec javascript-project))))))

  javascript-project)

(defmethod phenome ((obj javascript-project) &key (bin (temp-file-name)))
  "Create a phenotype of the JAVASCRIPT-PROJECT.  In this case,
override the phenome method to output the genome of OBJ to BIN as JavaScript
is not a compiled language.
OBJ object to create a phenome for
BIN location where the phenome will be created on the filesystem"
  (to-file obj bin)
  (values bin 0 nil nil nil))
