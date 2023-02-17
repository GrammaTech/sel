;;; compilation-database.lisp --- Compilation database data structures

(defpackage :software-evolution-library/components/compilation-database
  (:use :gt/full)
  (:local-nicknames (:json :cl-json))
  (:export :parse-compilation-database
           :compilation-db-entry-compiler
           :compilation-db-entry-flags
           :normalize-flags))
(in-package :software-evolution-library/components/compilation-database)

(defgeneric parse-compilation-database (source)
  (:method ((source string))
    (with-input-from-string (in source)
      (parse-compilation-database in)))
  (:method ((source pathname))
    (with-input-from-file (in source)
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
