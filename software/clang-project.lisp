;;; clang-project.lisp --- Projects with a clang compilation database
(defpackage :software-evolution-library/software/clang-project
  (:nicknames :sel/software/clang-project :sel/sw/clang-project)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :split-sequence
        :cl-ppcre
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/simple
        :software-evolution-library/software/parseable
        :software-evolution-library/software/clang
        :software-evolution-library/software/project
        :software-evolution-library/software/parseable-project)
  (:shadowing-import-from :uiop
                          :ensure-directory-pathname
                          :directory-exists-p
                          :run-program
			  :resolve-symlinks
			  :directory*)
  (:export :clang-project
           :compilation-database))
(in-package :software-evolution-library/software/clang-project)
(in-readtable :curry-compose-reader-macros)

(define-software clang-project (parseable-project)
  ((compilation-database :initarg :compilation-database
                         :accessor compilation-database
                         :initform nil
                         :documentation "Compilation database for the project.
See https://clang.llvm.org/docs/JSONCompilationDatabase.html for
information on the format of compilation databases."))
  (:documentation "Project specialization for clang software objects."))

(defmethod initialize-instance :after ((clang-project clang-project) &key)
  (setf (component-class clang-project)
        (or (component-class clang-project) 'clang)))

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

(defun clang-project-relativize (clang-project path)
  "Clip off the leading directories of PATH, so it is relative
to the base directory of CLANG-PROJECT.  Returns the clipped
namestring."
  (pathname-relativize (project-dir clang-project) path))

(defun pathname-relativize (root-path path)
   (replace-all (-> (canonical-pathname path)
		 (namestring))
	       (-> root-path
		 (ensure-directory-pathname)
		 (canonical-pathname)
		 (namestring))
	       ""))

(defgeneric create-evolve-files (clang-project)
  (:documentation
   "Create the evolve files for CLANG-PROJECT from the `compilation-database'.")
  (:method ((clang-project clang-project))
    (flet ((get-compiler (entry)
             (->> (aget :command entry)
               (split-sequence #\Space)
               (first))))
      (iter (for entry in (compilation-database clang-project))
            (collect
                (let ((file-path
                       (-<>> (aget :directory entry)
                         (ensure-directory-pathname)
                         (merge-pathnames-as-file <>
                                                  (aget :file entry))
                         (project-path clang-project))))
                  (cons (clang-project-relativize clang-project file-path)
                        (-> (make-instance (component-class clang-project)
                              :compiler (get-compiler entry)
                              :flags (compilation-db-flags clang-project entry))
                          (from-file file-path)))))))))

(defgeneric create-other-files (project &key excluded-dirs &allow-other-keys)
  (:documentation
   "Finds those files in PROJECT that were not included in evolve-files.
Assumes evolve-files has been initialized.")
  (:method (obj &key &allow-other-keys) nil))

(defun pathname-has-symlink (p)
  (not (equal p (uiop:resolve-symlinks p))))

(defmethod create-other-files ((project clang-project) &key (excluded-dirs '(".git"))
							 &allow-other-keys)
  (let* ((all-file-paths
	  (-<>> (project-dir project)
	    (merge-pathnames-as-file <> #p"**/*.*")
	    (uiop:directory*)
	    (remove-if
	     (lambda (p) (or (null (pathname-name p))
			     (ends-in-tilde (pathname-name p))
			     (ends-in-tilde (pathname-type p))
			     ;; For now do not include symlinks
			     ;; In the future, make links be special
			     ;; objects
			     (pathname-has-symlink p)
			     (intersection (pathname-directory p)
					   excluded-dirs
					   :test #'equal))))))
	 (all-relativized-file-paths
	  (iter (for p in all-file-paths)
		(collect (clang-project-relativize project p))))
	 (evolve-file-paths (mapcar #'car (evolve-files project)))
	 (other-relativized-file-paths
	  ;; evolve files may have come from the build dir, but that
	  ;; doesn't matter here as we are comparing the relativized paths
	  (remove-if (lambda (p) (member p evolve-file-paths :test #'equal))
		     all-relativized-file-paths)))
    ;; Create software objects for these other files
    (iter (for p in other-relativized-file-paths)
	  (collect (cons p
			 ;; These are represented as simple text files,
			 ;; for line-oriented diffs
			 (-> (make-instance 'simple)
			   (from-file (merge-pathnames-as-file
				       (project-dir project)
				       p))))))))

(defun ends-in-tilde (s)
  (let ((len (length s)))
    (and (> len 0) (eql (elt s (1- len)) #\~))))

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
    (setf (other-files clang-project)
	  (create-other-files clang-project))
    clang-project))
