;;; c-project.lisp --- C/C++ Projects which use a database
;;; These are not dependent on clang (build using specified build commands).
;;;
(defpackage :software-evolution-library/software/c-project
  (:nicknames :sel/software/c-project :sel/sw/c-project)
  (:use :gt/full
        :cl-json
        :software-evolution-library
        :software-evolution-library/software/simple
        :software-evolution-library/software/parseable
        :software-evolution-library/software/project
        :software-evolution-library/software/parseable-project
        :software-evolution-library/software/tree-sitter)
  (:export :c-project))
(in-package :software-evolution-library/software/c-project)
(in-readtable :curry-compose-reader-macros)

(define-software c-project (parseable-project)
  ((compilation-database :initarg :compilation-database
                         :accessor compilation-database
                         :initform nil
                         :documentation "Compilation database for the project.
See https://clang.llvm.org/docs/JSONCompilationDatabase.html for
information on the format of compilation databases."))
  (:documentation "Project specialization for c/c++ software objects."))

(defmethod initialize-instance :after ((c-project c-project) &key)
  (setf (component-class c-project)
        (or (component-class c-project) 'c)))

(defmethod mutation-key ((obj project) op)
  "Return key used to organize mutations in *mutation-stats* hashtable.
* OBJ object mutation is to be applied to
* OP operation to be performed
"
  (list (nth 1 op))) ; default to mutation type, as per default
                     ; mutation-key impl

(defmethod collect-evolve-files :before ((obj c-project))
  "Ensure C-PROJECT has a compilation-database populated."
  ;; (assert (or (compilation-database obj)
  ;;             (and (build-command obj) (which "bear")))
  ;;         (obj)
  ;;         "C-project requires a compilation-database ~
  ;;          or a build-command and 'bear' in your PATH")
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
           (if (aget :arguments entry)
               (mapcar (lambda (arg) ; Wrap quotes for the shell.
                         (regex-replace
                          "\"([^\"]*)\"" arg "'\"\\1\"'"))
                       (cdr (aget :arguments entry)))
               (nest (cdr)
                     (split-quoted)
                     (unescape-string)
                     (regex-replace-all "\\\\\\\"(.*?)\\\\\\\""
                                        (or (aget :command entry) "")
                                        "'\"\\1\"'")))))
    (nest
     ;; Normalize the list of compiler flags
     ;; (normalize-flags (aget :directory entry))
     ;; Remove the file being built from the flags.
     (remove-if {equalp (aget :file entry)})
     ;; Get list of compiler flags from the ENTRY
     (flag-list-helper entry))))

(defmethod collect-evolve-files ((c-project c-project))
  (labels ((get-file-path (entry)
             (merge-pathnames-as-file (nest (ensure-directory-pathname)
                                            (aget :directory entry))
                                      (aget :file entry))))
    (mapcar (lambda (entry)
              (cons (pathname-relativize (project-dir c-project)
                                         (get-file-path entry))
                    (from-file
                     (make-instance (component-class c-project)
                       :compiler (compilation-db-entry-compiler entry)
                       :flags (compilation-db-entry-flags entry))
                     (get-file-path entry))))
            (remove-if «or [{ignored-evolve-path-p c-project} {aget :file}]
                           [#'not #'file-exists-p #'get-file-path]»
                       (compilation-database c-project)))))
