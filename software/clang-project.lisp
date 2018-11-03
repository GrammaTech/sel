;;; clang-project.lisp --- Projects with a clang compilation database
(defpackage :software-evolution-library/software/clang-project
  (:nicknames :sel/software/clang-project :sel/sw/clang-project)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :software-evolution-library
        :software-evolution-library/utility)
  (:export :clang-project
           :project-dir
           :compilation-database))
(in-package :software-evolution-library/software/clang-project)
(in-readtable :curry-compose-reader-macros)

(define-software clang-project (project)
  ((project-dir :initarg :project-dir
                :accessor project-dir
                :initform nil
                :documentation "Source directory containing the project.")
   (compilation-database :initarg :compilation-database
                         :accessor compilation-database
                         :initform nil
                         :documentation "Compilation database for the project.
See https://clang.llvm.org/docs/JSONCompilationDatabase.html for
information on the format of compilation databases.")
   (clang-class :initarg :clang-class
                :accessor clang-class
                :initform 'clang
                :documentation "Clang subclass to utilize in the project."))
  (:documentation "Project specialization for clang software objects."))

(defgeneric project-path (project path)
  (:documentation "Expand PATH relative to PROJECT.")
  (:method ((obj project) path)
    (replace-all (namestring (canonical-pathname path))
                 (-> (if (and *build-dir*
                              (search (namestring *build-dir*)
                                      (namestring
                                       (ensure-directory-pathname
                                        (canonical-pathname path)))))
                         *build-dir*
                         (project-dir obj))
                   (canonical-pathname)
                   (ensure-directory-pathname)
                   (namestring))
                 (-> (project-dir obj)
                   (canonical-pathname)
                   (ensure-directory-pathname)
                   (namestring)))))

(defgeneric create-evolve-files (clang-project)
  (:documentation
   "Create the evolve files for CLANG-PROJECT from the `compilation-database'.")
  (:method ((clang-project clang-project))
    (flet ((get-compiler (entry)
             (->> (aget :command entry)
               (split-sequence #\Space)
               (first)))
           (relativize (clang-project path)
             (replace-all (-> (canonical-pathname path)
                            (namestring))
                          (-> (project-dir clang-project)
                            (ensure-directory-pathname)
                            (canonical-pathname)
                            (namestring))
                          "")))
      (iter (for entry in (compilation-database clang-project))
            (collect
                (let ((file-path
                       (-<>> (aget :directory entry)
                         (ensure-directory-pathname)
                         (merge-pathnames-as-file <>
                                                  (aget :file entry))
                         (project-path clang-project))))
                  (cons (relativize clang-project file-path)
                        (-> (make-instance (clang-class clang-project)
                              :compiler (get-compiler entry)
                              :flags (compilation-db-flags clang-project entry))
                          (from-file file-path)))))))))

(defun compilation-db-flags (clang-project entry)
  "Return the flags for ENTRY in a compilation database."
  ;; Get flags from :arguments or :command field.  These fields handle quote
  ;; escaping differently; see
  ;; https://clang.llvm.org/docs/JSONCompilationDatabase.html.
  (->> (iter (for f in (->> (or (mapcar (lambda (arg) ; Wrap quotes for the shell.
                                          (regex-replace
                                           "\"([^\"]*)\"" arg "'\"\\1\"'"))
                                        (aget :arguments entry))
                                (cdr (split-sequence
                                         #\Space (or (aget :command entry) "")
                                         :remove-empty-subseqs t)))
                         (mappend (lambda (arg) ; Split leading "-I".
                                    (split-sequence #\Space
                                      (replace-all arg "-I" "-I ")
                                      :remove-empty-subseqs t)))))
             (for p previous f)
             (collect
                 (if (string= p "-I")
                     (if (starts-with-subseq "/" f)
                         (->> (ensure-directory-pathname f)
                           (project-path clang-project))
                         (->> (merge-pathnames-as-directory
                               (make-pathname :directory
                                              (aget :directory
                                                    entry))
                               (make-pathname :directory
                                              (list :relative f)))
                           (ensure-directory-pathname)
                           (project-path clang-project)))
                     f)))
    (remove-if {string= (aget :file entry)})
    (append (list "-I"
                  (namestring (project-dir clang-project))))
    (append (list "-I"
                  (->> (merge-pathnames-as-directory
                        (ensure-directory-pathname
                         (aget :directory entry))
                        (aget :file entry))
                    (ensure-directory-pathname)
                    (project-path clang-project))))))

(defmethod from-file ((clang-project clang-project) project-dir)
  "Populate CLANG-PROJECT from the source code in PROJECT-DIR.
* CLANG-PROJECT to be populated from source in PROJECT-DIR
* PROJECT-DIR source code to populate CLANG-PROJECT with
"
  (assert (or (compilation-database clang-project) (which "bear"))
          (clang-project)
          "Calling `from-file` on a clang-project requires a compilation ~
           database or 'bear' installation on your PATH")
  (labels ((create-compilation-database (clang-project)
             (multiple-value-bind (stdout stderr errno)
                 (shell "cd ~a && bear ~a"
                        *build-dir* (build-command clang-project))
               (declare (ignorable errno))
               (or (and (probe-file (make-pathname
                                      :directory (directory-namestring
                                                  *build-dir*)
                                      :name "compile_commands.json"))
                        (with-open-file (in (make-pathname
                                              :directory (directory-namestring
                                                          *build-dir*)
                                              :name "compile_commands.json"))
                          (remove-duplicates (json:decode-json-from-source in)
                            :test #'equalp
                            :key (lambda (entry)
                                   (merge-pathnames-as-file
                                     (ensure-directory-pathname
                                       (aget :directory entry))
                                     (aget :file entry)))
                            :from-end t)))
                   (error "Failed to create compilation database for project.~%~
                           build command: ~a~%~
                           stdout: ~a~%~
                           stderr: ~a~%"
                           (build-command clang-project)
                           stdout stderr)))))
    (setf (project-dir clang-project)
          (canonical-pathname (truename project-dir)))
    (if (compilation-database clang-project)
        ;; Load from compilation database.
        (setf (evolve-files clang-project)
              (create-evolve-files clang-project))
        (with-temp-build-dir (project-dir)
          (setf (compilation-database clang-project)
                (create-compilation-database clang-project))
          (setf (evolve-files clang-project)
                (create-evolve-files clang-project))))
    clang-project))

(defmethod to-file ((clang-project clang-project) path)
  "Write CLANG-PROJECT to the path directory.
* CLANG-PROJECT project to output
* PATH directory to write the project to
"
  (let ((*build-dir* (make-build-dir (project-dir clang-project) :path path)))
    (write-genome-to-files clang-project)))

(defmethod asts ((obj clang-project))
  "Return a list of all ASTs in the project OBJ."
  (apply #'append (mapcar [#'asts #'cdr] (evolve-files obj))))

(defmethod update-asts ((obj clang-project))
  "Call `update-asts' on all `evolve-files' of OBJ."
  (mapc [#'update-asts #'cdr] (evolve-files obj)))

(defmethod update-caches ((obj clang-project))
  "Call `update-caches' on all `evolve-files' of OBJ."
  (mapc [#'update-caches #'cdr] (evolve-files obj)))
