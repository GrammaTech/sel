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
  (:local-nicknames
   (:dbg :software-evolution-library/utility/debug)
   (:json :cl-json))
  (:export
    :command-object
    :compilation-database
    :compilation-database-path
    :compilation-database-project
    :ensure-compilation-database))
(in-package :software-evolution-library/software/compilation-database-project)

(eval-always
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
     :compilation-database-path nil)))

;;; NOTE: overwrite define-software's copy method.
(define-default-copy compilation-database-project (:around-method t)
  (when (and compilation-database
             (not (original-project-p copy)))
    (setf (compilation-database copy)
          (relativize-command-objects
           compilation-database (original-path copy) (project-dir copy)))))

(defmethod collect-evolve-files :before ((obj compilation-database-project))
  "Ensure OBJ has a compilation-database populated."
  (unless (compilation-database obj)
    (populate-compilation-database obj)))

(defun populate-compilation-database (obj)
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
         (comp-db-path
           (find-if #'file-exists-p comp-db-paths))
         (compilation-database
           (progn
             (if comp-db-path
                 (progn
                   (dbg:note :debug "Found compilation database ~a" comp-db-path)
                   (with-open-file (in comp-db-path)
                     (parse-compilation-database in)))
                 (progn
                   (dbg:note
                    :debug
                    "No compilation database: checked ~{~a~^, ~}"
                    comp-db-paths)
                   (ensure-compilation-database obj))))))
    (when compilation-database
      (let ((project-dir (truename (project-dir obj)))
            (build-path (maybe-build-path compilation-database)))
        ;; NOTE: original-path can be nil.
        ;;       assume build-path always exists.
        (if (and build-path
                 (let ((build-path (pathname build-path)))
                   (nor (pathname-equal build-path project-dir)
                        (subpathp build-path project-dir))))
            (setf (compilation-database obj)
                  (relativize-command-objects
                   compilation-database
                   build-path
                   project-dir))
            (setf (compilation-database obj) compilation-database))))))

(defun original-project-p (obj)
  "Return T if the project-dir and original-path of OBJ are equal."
  (with-slots (original-path project-dir) obj
    (and original-path
         project-dir
         (pathname-equal (truename original-path)
                         (truename project-dir)))))

(defun maybe-build-path (compilation-database)
  "Return what might be the project's path based on the directories used in
COMPILATION-DATABASE. Returns NIL if one isn't found."
  (gcp (mapcar #'command-directory (command-objects compilation-database))))

;;; TODO: only relativizes the directory and file name.
(defun relativized-path-p (original-path command-object)
  "Return the relativized path of COMMAND-OBJECT to PROJECT's path
             if it exists."
  (let* ((directory (ensure-directory-pathname
                     (command-directory command-object)))
         (file (command-file command-object))
         (path (canonical-pathname (path-join directory file)))
         (relativized-to-original
          (enough-pathname path original-path)))
    (unless (pathname-equal relativized-to-original path)
      relativized-to-original)))

(defun relative-command-object (command-object original-path new-path)
  "Copy COMMAND object with ORIGINAL-PATH relative to NEW-PATH."
  (if-let* ((relative-path
             (relativized-path-p original-path command-object))
            (new-path (fmt "~a/~a" new-path relative-path)))
    (copy command-object
          :file (file-namestring new-path)
          :directory (directory-namestring new-path))
    command-object))

(defun relativize-command-objects (compilation-database original-path new-path)
  "Relativize the command objects in COMPILATION-DATABASE such that they're
relative to NEW-PATH."
  (assert (and compilation-database original-path new-path))
  (let ((original-path
         (ensure-directory-pathname
          (canonical-pathname original-path)))
        (new-path
         (ensure-directory-pathname
          (canonical-pathname new-path))))
    (copy compilation-database
          :command-objects
          (mapcar (op (relative-command-object _ original-path new-path))
                  (command-objects compilation-database)))))

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
        (multiple-value-bind (found found?)
            (lookup db (namestring key))
          ;; If there are multiple command objects, just use the
          ;; first. This is the behavior of the LLVM compilation
          ;; database tooling (e.g. clangd).
          (multiple-value-prog1 (values (car found) found?)
            (when (rest found)
              (dbg:lazy-note
               :debug
               "~a compilation database entr~:@p for ~a"
               (length found)
               path)))))
      (values nil nil))))
