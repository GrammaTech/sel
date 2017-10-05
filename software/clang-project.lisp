;; Specialization for building a project from a clang compilation database
(in-package :software-evolution)

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
                :documentation "Clang subclass to utilize in the project")))


(defmethod from-file ((clang-project clang-project) project-dir)
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
                          (json:decode-json-from-source in)))
                   (error "Failed to create compilation database for project.~%~
                           build command: ~a ~a~%~
                           stdout: ~a~%~
                           stderr: ~a~%"
                           (build-command clang-project)
                           (build-target clang-project)
                           stdout stderr))))
           (relativize (clang-project path)
             (replace-all (-> (if (-> (cl-fad:pathname-as-directory path)
                                      (cl-fad:directory-exists-p))
                                  (cl-fad:pathname-as-directory path)
                                  path)
                              (cl-fad:canonical-pathname)
                              (namestring))
                          (-> (project-dir clang-project)
                              (cl-fad:pathname-as-directory)
                              (cl-fad:canonical-pathname)
                              (namestring))
                          ""))
           (get-project-path (clang-project path)
             (replace-all (-> (cl-fad:canonical-pathname path)
                              (namestring))
                          (-> (if (and *build-dir*
                                       (search (-> *build-dir*
                                                   (namestring))
                                               (-> path
                                                   (cl-fad:canonical-pathname)
                                                   (cl-fad:pathname-as-directory)
                                                   (namestring))))
                                  *build-dir*
                                  (project-dir clang-project))
                              (cl-fad:canonical-pathname)
                              (cl-fad:pathname-as-directory)
                              (namestring))
                          (-> (project-dir clang-project)
                              (cl-fad:canonical-pathname)
                              (cl-fad:pathname-as-directory)
                              (namestring))))
           (create-evolve-files (clang-project)
             (iter (for entry in (compilation-database clang-project))
                   (collect
                     (cons (->> (get-project-path clang-project
                                                  (aget :file entry))
                                (relativize clang-project))
                           (->  (make-instance (clang-class clang-project)
                                               :compiler (get-compiler entry)
                                               :flags (get-flags entry))
                                (from-file (aget :file entry)))))))
           (get-compiler (compilation-database-entry)
             (->> (aget :command compilation-database-entry)
                  (split-sequence #\Space)
                  (first)))
           (get-flags (compilation-database-entry)
             (let ((flags (-<>> (aget :command compilation-database-entry)
                                (replace-all <> "-I" "-I ")
                                (split-sequence #\Space)
                                (remove-if {string= ""})
                                (cdr))))
               (->> (iter (for f in flags)
                          (for p previous f)
                          (collect
                            (if (string= p "-I")
                                (if (starts-with-subseq "/" f)
                                    (->> (cl-fad:pathname-as-directory f)
                                         (get-project-path clang-project))
                                    (->> (cl-fad:merge-pathnames-as-directory
                                           (->> (aget :directory
                                                      compilation-database-entry)
                                                (make-pathname :directory))
                                           (make-pathname :directory f))
                                         (cl-fad:pathname-as-directory)
                                         (get-project-path clang-project)))
                                f)))
                    (remove-if {string= (aget :file compilation-database-entry)})
                    (append (list "-I" (-> (project-dir clang-project)
                                           (namestring))))))))
    (setf (project-dir clang-project)
          (-> (truename project-dir)
              (cl-fad:canonical-pathname)))
    (cond ((all-files clang-project)
           ;; reload existing files
           (iter (for (src-file . obj) in (all-files clang-project))
                 (from-file obj (in-directory (project-dir clang-project)
                                              src-file))))
          ((compilation-database clang-project)
           ;; load from compilation database
           (setf (evolve-files clang-project)
                 (create-evolve-files clang-project)))
          (t ;;create compilation database
           (with-temp-build-dir (project-dir)
             (setf (compilation-database clang-project)
                   (create-compilation-database clang-project))
             (setf (evolve-files clang-project)
                   (create-evolve-files clang-project)))))
    clang-project))

(defmethod to-file ((clang-project clang-project) path)
  (let ((*build-dir* (make-build-dir (project-dir clang-project) :path path)))
    (write-genome-to-files clang-project)))


(defmethod asts ((obj clang-project))
  (if (current-file obj)
      (asts (current-file obj))
      (apply #'append (mapcar [#'asts #'cdr] (evolve-files obj)))))

(defmethod clang-format ((clang-project clang-project) &optional style)
  (apply-to-project clang-project {clang-format _ style})
  clang-project)

(defmethod clang-tidy ((clang-project clang-project))
  (apply-to-project clang-project #'clang-tidy)
  clang-project)

(defmethod indent ((clang-project clang-project) &optional style)
  (apply-to-project clang-project {indent _ style}))

(defmethod flags ((obj clang-project))
  nil)
