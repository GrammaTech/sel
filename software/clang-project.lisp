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
  (:documentation "DOCFIXME"))


(defmethod from-file ((clang-project clang-project) project-dir)
  "DOCFIXME
* CLANG-PROJECT DOCFIXME
* PROJECT-DIR DOCFIXME
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
                                             :key {aget :file}
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
             (let ((flags (-<>> (or (aget :command compilation-database-entry)
                                    "")
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
                                           (->> (aget :directory
                                                      compilation-database-entry)
                                                (make-pathname :directory))
                                           (make-pathname :directory f))
                                         (pathname-as-directory)
                                         (get-project-path clang-project)))
                                f)))
                    (remove-if {string= (aget :file compilation-database-entry)})
                    (append (list "-I"
                                  (namestring (project-dir clang-project))))
                    (append (list "-I"
                                  (->> (aget :file compilation-database-entry)
                                       (directory-namestring)
                                       (get-project-path clang-project))))))))
    (setf (project-dir clang-project)
          (-> (truename project-dir)
              (canonical-pathname)))
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
  "DOCFIXME
* CLANG-PROJECT DOCFIXME
* PATH DOCFIXME
"
  (let ((*build-dir* (make-build-dir (project-dir clang-project) :path path)))
    (write-genome-to-files clang-project)))


(defmethod asts ((obj clang-project))
  "DOCFIXME
* OBJ DOCFIXME
"
  (if (current-file obj)
      (asts (current-file obj))
      (apply #'append (mapcar [#'asts #'cdr] (evolve-files obj)))))

(defmethod clang-format ((clang-project clang-project) &optional style)
  "DOCFIXME
* CLANG-PROJECT DOCFIXME
"
  (apply-to-project clang-project {clang-format _ style})
  clang-project)

(defmethod clang-tidy ((clang-project clang-project))
  "DOCFIXME
* CLANG-PROJECT DOCFIXME
"
  (apply-to-project clang-project #'clang-tidy)
  clang-project)

(defmethod indent ((clang-project clang-project) &optional style)
  "DOCFIXME
* CLANG-PROJECT DOCFIXME
* STYLE DOCFIXME
"
  (apply-to-project clang-project {indent _ style}))

(defmethod flags ((obj clang-project))
  "DOCFIXME
* OBJ DOCFIXME
"
  nil)
