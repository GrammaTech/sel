(defpackage :software-evolution-library/software/compilation-database-project
  (:nicknames
   :sel/software/compilation-database-project
   :sel/sw/compilation-database-project
   :sel/sw/compdb-project)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/components/compilation-database
        :software-evolution-library/components/file
        :software-evolution-library/software/simple
        :software-evolution-library/software/parseable
        :software-evolution-library/software/project
        :software-evolution-library/software/parseable-project)
  (:local-nicknames (:json :cl-json))
  (:export
    :command-object
    :compilation-database
    :compilation-database-path
    :compilation-database-project
    :ensure-compilation-database))
(in-package :software-evolution-library/software/compilation-database-project)

(define-software compilation-database-project (project)
  ((compilation-database :initarg :compilation-database
                         :accessor compilation-database
                         :initform nil
                         :documentation "Compilation database for the project.
See https://clang.llvm.org/docs/JSONCompilationDatabase.html for
information on the format of compilation databases.")
   (compilation-database-path
    :initarg :compilation-database-path
    :reader compilation-database-path
    :type (or null string pathname)
    :documentation "Location of the compilation database."))
  (:default-initargs
   :compilation-database-path nil))

(defmethod collect-evolve-files :before ((obj compilation-database-project))
  "Ensure OBJ has a compilation-database populated."
  (unless (compilation-database obj)
    (let* ((supplied-path (compilation-database-path obj))
           (default-compdb-paths
            (mapcar (op (project-relative-pathname obj _))
                    '("compile_commands.json"
                      "build/compile_commands.json")))
           (comp-db-paths
            (ensure-list
             (econd ((no supplied-path)
                     default-compdb-paths)
                    ((relative-pathname-p supplied-path)
                     (project-relative-pathname obj supplied-path))
                    ((absolute-pathname-p supplied-path)
                     supplied-path))))
           (compilation-database
            (progn
              (unless (find-if #'file-exists-p comp-db-paths)
                (ensure-compilation-database obj))
              (when-let (comp-db-path (find-if #'file-exists-p comp-db-paths))
                (with-open-file (in comp-db-path)
                  (parse-compilation-database in))))))
      (when compilation-database
        (setf (compilation-database obj) compilation-database)))))

(defgeneric ensure-compilation-database (obj)
  (:method ((obj compilation-database-project))
    nil))

(defgeneric command-object (obj file)
  (:documentation "Get FILE's command object in OBJ's compilation database.")
  (:method ((obj compilation-database-project)
            (file string))
    (command-object obj (pathname file)))
  (:method ((obj compilation-database-project)
            (file software))
    (command-object obj (original-path file)))
  (:method ((obj compilation-database-project)
            (path pathname))
    (if-let (db (compilation-database obj))
      (let* ((key (if (absolute-pathname-p path)
                      path
                      (project-relative-pathname obj path))))
        (lookup db (namestring key)))
      (values nil nil))))
