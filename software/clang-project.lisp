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
           (create-evolve-files (clang-project)
             (iter (for entry in (compilation-database clang-project))
                   (collect
                     (cons (replace-all (aget :file entry)
                                        (-> (if *build-dir*
                                                *build-dir*
                                                (project-dir clang-project))
                                            (cl-fad:pathname-as-directory)
                                            (namestring))
                                        "")
                           (from-file (make-instance
                                        (clang-class clang-project)
                                        :compiler (get-compiler entry)
                                        :flags (get-flags entry))
                                      (aget :file entry))))))
           (get-compiler (compilation-database-entry)
             (->> (aget :command compilation-database-entry)
                  (split-sequence #\Space)
                  (first)))
           (get-flags (compilation-database-entry)
             (let ((flags (-<>> (aget :command compilation-database-entry)
                                (replace-all <> "-I" "-I ")
                                (split-sequence #\Space)
                                (remove-if {string= ""}))))
               (->> (iter (for (prev . f) in (mapcar #'cons
                                                     flags
                                                     (cdr flags)))
                          (collect
                            (if (string= prev "-I")
                                (if (starts-with-subseq "/" f)
                                    f
                                    (-> (cl-fad:merge-pathnames-as-directory
                                          (make-pathname :directory f)
                                          (->> (aget :directory
                                                     compilation-database-entry)
                                               (make-pathname :directory)))
                                        (directory-namestring)))
                                f)))
                    (remove-if {string= (aget :file compilation-database-entry)})
                    (append (list "-I" (-> (project-dir clang-project)
                                           (namestring))))))))
    (setf (project-dir clang-project)
          (truename project-dir))
    (cond ((all-files clang-project)
           ;; reload existing files
           (iter (for (src-file . obj) in (all-files obj))
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


(defmethod instrument ((clang-project clang-project) &rest args)
  "Instrument a project. Arguments are passed through to instrument on
the underlying software objects."
  (let ((files (if (current-file clang-project)
                   (list (current-file clang-project))
                   (mapcar #'cdr (evolve-files clang-project))))
        (functions (plist-get :functions args))
        (other-args (plist-drop :functions args)))
    ;; Fully instrument evolve-files
    (loop
       for f in files
       for i upfrom 0
       do (apply #'instrument f
                 ;; Print file index at each AST
                 :functions (cons (lambda (obj ast)
                                    (declare (ignorable obj ast))
                                    (list (format nil "(:F . ~a)" i)))
                                  functions)
                 other-args))

    ;; Insert log setup code in other-files
    (loop for obj in (mapcar #'cdr (other-files clang-project))
       do (log-to-filename obj
                           (plist-get :trace-file args)
                           (plist-get :trace-env args))))

  clang-project)

(defmethod collect-traces ((clang-project clang-project) inputs
                           &key predicate max (bin (temp-file-name))
                           &aux (args (list :bin bin)) (delete-bin-p t))
  (when predicate (setf args (append args (list :predicate predicate))))
  (when max (setf args (append args (list :max max))))
  (if (probe-file bin)
      (setf delete-bin-p nil)
      (assert (phenome clang-project :bin bin) (clang-project)
              "Unable to compile software ~a" clang-project))
  (unwind-protect
      (labels
        ((collect-and-process-trace (clang-project input args)
           (let ((trace (apply #'collect-trace clang-project input args))
                 (file-to-points (make-hash-table :test #'equal)))
             ;; Partition trace points for each file in the project
             (iter (for point in (aget :trace trace))
                   (push point (gethash (aget :f point) file-to-points)))
             ;; Set trace for each file
             (iter (for (src-file . obj) in (evolve-files clang-project))
                   (for f upfrom 0)
                   (appendf (traces obj)
                            (list (list (cons :input
                                              (aget :input trace))
                                        (cons :trace
                                              (-> (gethash f file-to-points)
                                                  (reverse))))))))))
        (map nil
             (lambda (input)
               (collect-and-process-trace clang-project input args))
             inputs))
    (when (and delete-bin-p (probe-file bin)) (delete-file bin))))


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
