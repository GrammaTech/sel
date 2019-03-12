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

(defmethod collect-evolve-files :before ((obj clang-project))
  "Ensure CLANG-PROJECT has a compilation-database populated."
  (assert (or (compilation-database obj) (which "bear")) (obj)
          "Calling `from-file' on a clang-project requires a compilation ~
           database or 'bear' installation on your PATH")
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

(defun compilation-db-flags (clang-project entry)
  "Return the flags for ENTRY in a compilation database."
  ;; Get flags from :arguments or :command field.  These fields handle quote
  ;; escaping differently; see
  ;; https://clang.llvm.org/docs/JSONCompilationDatabase.html.
  (->> (iter (for f in (->> (or (mapcar (lambda (arg) ; Wrap quotes for the shell.
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

(defmethod collect-evolve-files ((clang-project clang-project))
  (mapcar (lambda (entry)
            (let ((file-path (-<>> (aget :directory entry)
                               (ensure-directory-pathname)
                               (merge-pathnames-as-file <>
                                                        (aget :file entry))
                               (project-path clang-project))))
              (cons (pathname-relativize (project-dir clang-project) file-path)
                    (from-file
                     (make-instance (component-class clang-project)
                       :compiler (or (first (split-sequence
                                                #\Space (aget :command entry)))
                                     (first (aget :arguments entry)))
                       :flags (compilation-db-flags clang-project entry))
                     file-path))))
          (remove-if [{ignored-evolve-path-p clang-project} {aget :file}]
                     (compilation-database clang-project))))
