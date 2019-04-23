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
        :uiop/filesystem
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
                          :absolute-pathname-p
                          :directory-exists-p
                          :run-program
			  :resolve-symlinks
			  :directory*)
  (:export :clang-project
           :compilation-database
           :compilation-db-entry-compiler
           :compilation-db-entry-flags))
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

(defmethod collect-evolve-files :before ((obj clang-project))
  "Ensure CLANG-PROJECT has a compilation-database populated."
  (assert (or (compilation-database obj)
              (and (build-command obj) (which "bear")))
          (obj)
          "Clang-project requires a compilation-database ~
           or a build-command and 'bear' in your PATH")
  (nest
   (unless (compilation-database obj))
   (let ((comp-db-path (make-pathname
                        :directory (directory-namestring
                                    (ensure-directory-pathname
                                     (project-dir obj)))
                        :name "compile_commands"
                        :type "json"))))
   (setf (compilation-database obj))
   (progn
     (unless (probe-file comp-db-path)
       (multiple-value-bind (stdout stderr errno)
           (shell "cd ~a && bear ~a" (project-dir obj) (build-command obj))
         (declare (ignorable errno))
         (unless (probe-file comp-db-path)
           (error "Failed to create compilation database for project.~%~
                   build command: ~a~%~
                   stdout: ~a~%~
                   stderr: ~a~%"
                  (build-command obj)
                  stdout stderr))))
     (with-open-file (in comp-db-path)
       (or (remove-duplicates (json:decode-json-from-source in)
                              :test #'equalp
                              :key (lambda (entry)
                                     (merge-pathnames-as-file
                                      (ensure-directory-pathname
                                       (aget :directory entry))
                                      (aget :file entry)))
                              :from-end t)
           (warn "Empty compilation-database for ~a" (project-dir obj)))))))

(defun compilation-db-entry-compiler (entry)
  "Return the compiler in the compilation database ENTRY."
  (or (first (split-sequence #\Space (aget :command entry)))
      (first (aget :arguments entry))))

(defun compilation-db-entry-flags (entry)
  "Return the compiler flags in the compilation database ENTRY."
  (flet ((flag-list-helper (entry)
           "Get a massaged list of compiler flags from the ENTRY."
           (nest
            (mappend (lambda (arg) ; Split leading "-L".
                       (split-sequence #\Space
                         (replace-all arg "-L" "-L ")
                         :remove-empty-subseqs t)))
            (mappend (lambda (arg) ; Split leading "-I".
                       (split-sequence #\Space
                         (replace-all arg "-I" "-I ")
                         :remove-empty-subseqs t)))
            (or (mapcar (lambda (arg) ; Wrap quotes for the shell.
                          (regex-replace
                           "\"([^\"]*)\"" arg "'\"\\1\"'"))
                        ;; Drop the first element of
                        ;; arguments which is the compiler.
                        (cdr (aget :arguments entry)))
                (cdr (split-sequence
                         #\Space (or (aget :command entry) "")
                         :remove-empty-subseqs t))))))
    (nest
     ;; Remove the file being built from the flags.
     (remove-if {equalp (aget :file entry)})
     (iter (for f in (flag-list-helper entry))
           (for p previous f)
           (collect (if (or (string= p "-I") (string= p "-L"))
                        ;; Ensure include/library paths
                        ;; point to the correct location
                        ;; and not a temporary build directory.
                        (if (absolute-pathname-p f)
                            f
                            (merge-pathnames-as-directory
                             (make-pathname :directory
                                            (aget :directory entry))
                             (make-pathname :directory
                                            (list :relative f))))
                        ;; Pass the flag thru.
                        f))))))

(defmethod collect-evolve-files ((clang-project clang-project))
  (labels ((get-file-path (entry)
             (-<>> (aget :directory entry)
                   (ensure-directory-pathname)
                   (merge-pathnames-as-file <> (aget :file entry)))))
    (mapcar (lambda (entry)
              (cons (pathname-relativize (project-dir clang-project)
                                         (get-file-path entry))
                    (from-file
                     (make-instance (component-class clang-project)
                       :compiler (compilation-db-entry-compiler entry)
                       :flags (compilation-db-entry-flags entry))
                     (get-file-path entry))))
            (remove-if «or [{ignored-evolve-path-p clang-project} {aget :file}]
                           [#'not #'file-exists-p #'get-file-path]»
                       (compilation-database clang-project)))))
