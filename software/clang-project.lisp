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
                          :nest
                          :ensure-directory-pathname
                          :directory-exists-p
                          :run-program
                          :absolute-pathname-p
                          :getcwd)
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

(defun create-compilation-database (clang-project)
  "Build CLANG-PROJECT under bear and return a compilation database
for the project."
  (multiple-value-bind (stdout stderr errno)
      (shell "bear ~a" (build-command clang-project))
    (declare (ignorable errno))
    (or (and (probe-file "compile_commands.json")
             (with-open-file (in "compile_commands.json")
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
                stdout stderr))))

(defmethod collect-evolve-files :around ((clang-project clang-project))
  "Wrapper to create a compilation database for CLANG-PROJECT if necessary."
  (assert (or (compilation-database clang-project) (which "bear"))
          (clang-project)
          "Creation of a clang-project requires a compilation ~
           database or 'bear' installation on your PATH.")

  (if (compilation-database clang-project)
      (call-next-method)
      (with-temp-cwd-of (tempdir) (project-dir clang-project)
        (setf (compilation-database clang-project)
              (create-compilation-database clang-project))
        (call-next-method))))

(defmethod collect-evolve-files ((clang-project clang-project))
  (labels ((project-path (path)
             "Ensure that PATH is a subdirectory of CLANG-PROJECT."
             ;; We may have built the project in a temporary sandbox
             ;; when creating the compilation database. Replace the
             ;; temporary sandbox (cwd) with the project directory.
             (if (or (not (search (namestring (getcwd))
                                  (namestring path)))
                     (search (namestring (getcwd))
                             (namestring (project-dir clang-project))))
                 ;; We did not build in a temporary sandbox; return PATH.
                 (namestring path)
                 ;; We did build in a temporary sandbox; replace the
                 ;; temporary directory (cwd) with the project directory.
                 (replace-all (namestring path)
                              (->> (getcwd)
                                   (ensure-directory-pathname)
                                   (namestring))
                              (->> (project-dir clang-project)
                                   (ensure-directory-pathname)
                                   (namestring)))))
           (pathname-project-relativize (path)
             "Return PATH relative to the base of CLANG-PROJECT."
             (pathname-relativize (project-dir clang-project) path))
           (get-compiler (entry)
             "Return the compiler in the compilation database ENTRY."
             (or (first (split-sequence #\Space (aget :command entry)))
                 (first (aget :arguments entry))))
           (flag-list-helper (entry)
             "Get a massaged list of compiler flags from the ENTRY."
             (->> (or (mapcar (lambda (arg) ; Wrap quotes for the shell.
                                (regex-replace
                                 "\"([^\"]*)\"" arg "'\"\\1\"'"))
                              ;; Drop the first element of
                              ;; arguments which is the compiler.
                              (cdr (aget :arguments entry)))
                      (cdr (split-sequence
                               #\Space (or (aget :command entry) "")
                               :remove-empty-subseqs t)))
                  (mappend (lambda (arg) ; Split leading "-I".
                             (split-sequence #\Space
                               (replace-all arg "-I" "-I ")
                               :remove-empty-subseqs t)))
                  (mappend (lambda (arg) ; Split leading "-L".
                             (split-sequence #\Space
                               (replace-all arg "-L" "-L ")
                               :remove-empty-subseqs t)))))
           (get-flags (entry)
             "Return the compiler flags in the compilation database ENTRY."
             (->> (iter (for f in (flag-list-helper entry))
                        (for p previous f)
                        (collect (if (or (string= p "-I") (string= p "-L"))
                                     ;; Ensure include/library paths
                                     ;; point to the correct location
                                     ;; and not a temporary build directory.
                                     (if (absolute-pathname-p f)
                                         (project-path f)
                                         (->> (merge-pathnames-as-directory
                                               (make-pathname :directory
                                                (aget :directory entry))
                                               (make-pathname :directory
                                                (list :relative f)))
                                              (project-path)))
                                     ;; Pass the flag thru.
                                     f)))
                  ;; Remove the file being built from the flags.
                  (remove-if {string= (aget :file entry)})
                  ;; Add the base project directory as an explicit include
                  ;; directory to allow for building in a temporary sandbox.
                  (append (list "-I" (namestring (project-dir clang-project))))
                  ;; Add the parent directory of the file being compiled
                  ;; as an explicit include directory to allow for building
                  ;; in a temporary sandbox.
                  (append (list "-I" (->> (merge-pathnames-as-directory
                                           (ensure-directory-pathname
                                            (aget :directory entry))
                                           (aget :file entry))
                                          (project-path))))))
           (get-project-relative-path (entry)
             "Return the path to ENTRY relative to the base of CLANG-PROJECT."
             (-<> (aget :directory entry)
                  (ensure-directory-pathname)
                  (merge-pathnames-as-file <> (aget :file entry))
                  (project-path)
                  (pathname-project-relativize))))
    (mapcar (lambda (entry)
              (unless (ignored-evolve-path-p clang-project
                                             (get-project-relative-path entry))
                (cons (get-project-relative-path entry)
                      (from-file
                       (make-instance (component-class clang-project)
                         :compiler (get-compiler entry)
                         :flags (get-flags entry))
                       (merge-pathnames-as-file
                        (project-dir clang-project)
                        (get-project-relative-path entry))))))
            (compilation-database clang-project))))
