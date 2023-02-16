(defpackage :software-evolution-library/software/compilation-database-project
  (:nicknames :sel/software/compilation-database-project
   :sel/sw/compilation-database-project)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/simple
        :software-evolution-library/software/parseable
        :software-evolution-library/software/project
        :software-evolution-library/software/parseable-project)
  (:local-nicknames (:json :cl-json))
  (:export
    :compilation-database-project
    :compilation-database
    :ensure-compilation-database
    :normalize-flags
    :compilation-db-entry-flags
    :compilation-db-entry-compiler))
(in-package :software-evolution-library/software/compilation-database-project)

(define-software compilation-database-project (project)
  ((compilation-database :initarg :compilation-database
                         :accessor compilation-database
                         :initform nil
                         :documentation "Compilation database for the project.
See https://clang.llvm.org/docs/JSONCompilationDatabase.html for
information on the format of compilation databases.")))

(defmethod collect-evolve-files :before ((obj compilation-database-project))
  "Ensure OBJ has a compilation-database populated."
  (unless (compilation-database obj)
    (let* ((comp-db-paths
            (mapcar (op (project-relative-pathname obj _))
                    '("compile_commands.json"
                      "build/compile_commands.json")))
           (compilation-database
            (progn
              (unless (find-if #'file-exists-p comp-db-paths)
                (ensure-compilation-database obj))
              (when-let (comp-db-path (find-if #'file-exists-p comp-db-paths))
                (with-open-file (in comp-db-path)
                  (parse-compilation-database in))))))
      (when compilation-database
        (setf (compilation-database obj) compilation-database)))))

(defgeneric parse-compilation-database (source)
  (:method ((source string))
    (with-input-from-string (in source)
      (parse-compilation-database in)))
  (:method ((source stream))
    (remove-duplicates (json:decode-json-from-source source)
                       :test #'equalp
                       :key (lambda (entry)
                              (base-path-join
                               (ensure-directory-pathname
                                (aget :directory entry))
                               (aget :file entry)))
                       :from-end t)))

(defgeneric ensure-compilation-database (obj)
  (:method ((obj compilation-database-project))
    nil))

(defun compilation-db-entry-compiler (entry)
  "Return the compiler in the compilation database ENTRY."
  (or (first (split-sequence #\Space (aget :command entry)))
      (first (aget :arguments entry))))

(defun normalize-flags (dir flags)
  "Normalize the list of compiler FLAGS so all search paths are fully
expanded relative to DIR.

* DIR base directory for all relative paths
* FLAGS list of compiler flags
"
  (labels ((split-flags (flags)
             (nest (remove-if #'emptyp)
                   (mapcar #'trim-whitespace)
                   (mappend (lambda (flag) ; Split leading "L".
                              (split-quoted
                               (replace-all flag "-L" "-L "))))
                   (mappend (lambda (flag) ; Split leading "-I".
                              (split-quoted
                               (replace-all flag "-I" "-I ")))
                            flags))))
    (iter (for f in (split-flags flags))
          (for p previous f)
          (collect (if (and dir (or (string= p "-I") (string= p "-L")))
                       ;; Ensure include/library paths
                       ;; point to the correct location
                       ;; and not a temporary build directory.
                       (if (absolute-pathname-p f)
                           (nest (namestring)
                                 (ensure-directory-pathname)
                                 (canonical-pathname f))
                           (nest (namestring)
                                 (canonical-pathname)
                                 (merge-pathnames-as-directory
                                  (ensure-directory-pathname dir)
                                  (make-pathname :directory
                                                 (list :relative f)))))
                       ;; Pass the flag thru.
                       f)))))

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
     (normalize-flags (aget :directory entry))
     ;; Remove the file being built from the flags.
     (remove-if (op (equalp (aget :file entry) _)))
     ;; Get list of compiler flags from the ENTRY
     (flag-list-helper entry))))
