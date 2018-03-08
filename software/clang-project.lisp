;; Specialization for building a project from a clang compilation database
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(define-software clang-project (project)
  ((project-dir :initarg :project-dir
                :accessor project-dir
                :initform nil
                :documentation "Source directory containing the project")
   (compilation-database :initarg :compilation-database
                         :accessor compilation-database
                         :initform nil
                         :documentation "Compilation database for the project")
   (clang-class :initarg :clang-class
                :accessor clang-class
                :initform 'clang
                :documentation "Clang subclass to utilize in the project"))
  (:documentation "Project specialization for clang software objects."))

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
                 (shell "cd ~a && bear ~a ~a"
                        *build-dir*
                        (build-command clang-project)
                        (build-target clang-project))
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
                                     (pathname-as-directory
                                       (aget :directory entry))
                                     (pathname-as-file
                                       (aget :file entry))))
                            :from-end t)))
                   (error "Failed to create compilation database for project.~%~
                           build command: ~a ~a~%~
                           stdout: ~a~%~
                           stderr: ~a~%"
                           (build-command clang-project)
                           (build-target clang-project)
                           stdout stderr))))
           (relativize (clang-project path)
             (replace-all (-> (if (-> (pathname-as-directory path)
                                      (directory-exists-p))
                                  (pathname-as-directory path)
                                  path)
                              (canonical-pathname)
                              (namestring))
                          (-> (project-dir clang-project)
                              (pathname-as-directory)
                              (canonical-pathname)
                              (namestring))
                          ""))
           (get-project-path (clang-project path)
             (replace-all (-> (canonical-pathname path)
                              (namestring))
                          (-> (if (and *build-dir*
                                       (search (-> *build-dir*
                                                   (namestring))
                                               (-> path
                                                   (canonical-pathname)
                                                   (pathname-as-directory)
                                                   (namestring))))
                                  *build-dir*
                                  (project-dir clang-project))
                              (canonical-pathname)
                              (pathname-as-directory)
                              (namestring))
                          (-> (project-dir clang-project)
                              (canonical-pathname)
                              (pathname-as-directory)
                              (namestring))))
           (create-evolve-files (clang-project)
             (iter (for entry in (compilation-database clang-project))
                   (collect
                     (let ((file-path
                             (->> (merge-pathnames-as-file
                                    (pathname-as-directory
                                      (aget :directory entry))
                                    (pathname-as-file
                                      (aget :file entry)))
                                  (get-project-path clang-project))))
                       (cons (relativize clang-project file-path)
                             (-> (make-instance (clang-class clang-project)
                                                :compiler (get-compiler entry)
                                                :flags (get-flags entry))
                                 (from-file file-path)))))))
           (get-compiler (entry)
             (->> (aget :command entry)
                  (split-sequence #\Space)
                  (first)))
           (get-flags (entry)
             (let ((flags (-<>> (or (aget :command entry) "")
                                (replace-all <> "-I" "-I ")
                                (split-sequence #\Space)
                                (remove-if {string= ""})
                                (cdr))))
               (->> (iter (for f in flags)
                          (for p previous f)
                          (collect
                            (if (string= p "-I")
                                (if (starts-with-subseq "/" f)
                                    (->> (pathname-as-directory f)
                                         (get-project-path clang-project))
                                    (->> (merge-pathnames-as-directory
                                           (->> (aget :directory entry)
                                                (make-pathname :directory))
                                           (make-pathname :directory f))
                                         (pathname-as-directory)
                                         (get-project-path clang-project)))
                                f)))
                    (remove-if {string= (aget :file entry)})
                    (append (list "-I"
                                  (namestring (project-dir clang-project))))
                    (append (list "-I"
                                  (->> (merge-pathnames-as-directory
                                         (pathname-as-directory
                                           (aget :directory entry))
                                         (pathname-as-file
                                           (aget :file entry)))
                                       (pathname-as-directory)
                                       (get-project-path clang-project))))))))
    (setf (project-dir clang-project)
          (-> (truename project-dir)
              (canonical-pathname)))
    (cond ((all-files clang-project)
           ;; Reload existing files.
           (iter (for (src-file . obj) in (all-files clang-project))
                 (from-file obj (in-directory (project-dir clang-project)
                                              src-file))))
          ((compilation-database clang-project)
           ;; Load from compilation database.
           (setf (evolve-files clang-project)
                 (create-evolve-files clang-project)))
          (t ;; Create compilation database.
           (with-temp-build-dir (project-dir)
             (setf (compilation-database clang-project)
                   (create-compilation-database clang-project))
             (setf (evolve-files clang-project)
                   (create-evolve-files clang-project)))))
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
  (if (current-file obj)
      (asts (current-file obj))
      (apply #'append (mapcar [#'asts #'cdr] (evolve-files obj)))))

(defmethod clang-format ((clang-project clang-project) &optional style)
  "Apply `clang-format' to CLANG-PROJECT.
* CLANG-PROJECT project to format and return
* STYLE `clang-format' style to utilize
"
  (apply-to-project clang-project {clang-format _ style})
  clang-project)

(defmethod clang-tidy ((clang-project clang-project))
  "Apply `clang-tidy' to CLANG-PROJECT.
* CLANG-PROJECT project to tidy and return
"
  (apply-to-project clang-project #'clang-tidy)
  clang-project)

(defmethod indent ((clang-project clang-project) &optional style)
  "Apply GNU `indent' to CLANG-PROJECT.
* CLANG-PROJECT project to format and return
* STYLE GNU `indent' style to utilize
"
  (apply-to-project clang-project {indent _ style}))
