;;; compilation-database.lisp --- Compilation database data structures

(defpackage :software-evolution-library/components/compilation-database
  (:use :gt/full)
  (:local-nicknames
   (:json :cl-json))
  (:shadow :path)
  (:import-from :shlex)
  (:export :parse-compilation-database
           :normalize-flags
           :compilation-database
           :compilation-database.command-objects
           :compilation-database.path
           :compilation-database.file-command-objects
           :command-object
           :command-object.directory
           :command-object.file
           :command-object.arguments
           :command-object.command
           :command-object.output
           :command-object.flags
           :command-object.compiler
           :normalize-flag-string))
(in-package :software-evolution-library/components/compilation-database)

;;; Define classes for compile_commands.json and its command objects.
;;; This lets us pretend they are uniform by lazily computing any
;;; missing values.

(defclass compilation-database ()
  ((command-objects
    :type list
    :initarg :command-objects
    :reader compilation-database.command-objects
    :documentation "List of command objects.")
   (path
    :type (or pathname null)
    :initarg :path
    :reader compilation-database.path
    :documentation "On-disk path of compilation database (optional).")
   (size
    :type (integer 0 *)
    :initarg :size
    :reader size
    :documentation "Length of entry list.")
   (file-command-objects
    :type hash-table
    :documentation "Map from each file to its command objects."
    :reader compilation-database.file-command-objects))
  (:documentation "A JSON compilation database.
See <https://clang.llvm.org/docs/JSONCompilationDatabase.html>.")
  (:default-initargs
   :command-objects nil
   :path nil))

(defmethod slot-unbound ((class t)
                         (self compilation-database)
                         (slot-name (eql 'size)))
  "Lazily compute the entry count."
  (with-slots (command-objects) self
    (setf (slot-value self 'size)
          (length command-objects))))

(defmethod slot-unbound ((class t)
                         (self compilation-database)
                         (slot-name (eql 'file-command-objects)))
  "Lazily compute the file->command-objects mapping."
  (setf (slot-value self 'file-command-objects)
        (lret ((dict (dict)))
          (do-each (entry (compilation-database.command-objects self))
            (with-slots (file) entry
              (push (href dict file) entry))))))

(defclass command-object ()
  ((directory
    :type string
    :initarg :directory
    :reader command-object.directory
    :documentation "The working directory.")
   (file
    :type string :initarg :file
    :reader file
    :reader command-object.file
    :documentation "The source file.")
   (arguments
    :type (soft-list-of string)
    :initarg :arguments
    :reader command-object.arguments
    :documentation "The compiler command, as an argv.")
   (command
    :type string
    :initarg :command
    :reader command-object.command
    :documentation "The compile command, as one string.")
   (output
    :type string
    :initarg output
    :reader command-object.output
    :documentation "The output file.")
   (flags
    :type (soft-list-of string)
    :reader command-object.flags
    :documentation "Compiler flags.")
   (compiler
    :type string
    :reader command-object.compiler
    :documentation "compiler"))
  (:documentation "Entry in a compiler database")
  (:default-initargs
   :directory (required-argument :directory)
   :file (required-argument :file)))

(defmethod slot-unbound ((class t)
                         (self command-object)
                         (slot-name (eql 'arguments)))
  ;; Lazily compute the arguments from the command.
  (assert (slot-boundp self 'command))
  (with-slots (command arguments) self
    (setf arguments (shlex:split command))))

(defmethod slot-unbound ((class t)
                         (self command-object)
                         (slot-name (eql 'command)))
  (assert (slot-boundp self 'arguments))
  ;; Lazily compute the command from the arguments.
  (with-slots (command arguments) self
    (setf command
          (mapconcat #'shlex:quote arguments " "))))

(defmethod slot-unbound ((class t)
                         (self command-object)
                         (slot-name (eql 'compiler)))
  ;; Lazily parse out the compiler.
  (setf (slot-value self 'compiler)
        (first (command-object.arguments self))))

(defmethod slot-unbound ((class t)
                         (self command-object)
                         (slot-name (eql 'flags)))
  ;; Lazily parse out the flags.
  (setf (slot-value self 'flags)
        (compilation-db-entry-flags self)))

(defmethod initialize-instance :after ((self command-object) &key command arguments)
  (unless (or command arguments)
    (error "Either ~s or ~s is required for a command object."
           :arguments
           :command)))

(defgeneric parse-compilation-database (source &key path)
  (:method ((source compilation-database) &key path)
    (declare (ignore path))
    source)
  (:method ((source string) &key path)
    (with-input-from-string (in source)
      (parse-compilation-database in :path path)))
  (:method ((source pathname) &key (path source))
    (with-input-from-file (in source)
      (parse-compilation-database in :path path)))
  (:method ((source file-stream) &key path)
    (call-next-method source :path (or path (pathname source))))
  (:method ((source stream) &key path)
    (parse-compilation-database
     (json:decode-json-from-source source)
     :path path))
  (:method ((source list) &key path)
    "Parse a cl-json style alist."
    (make 'compilation-database
          :path path
          :command-objects
          (mapcar (lambda (args)
                    (apply #'make 'command-object
                           (alist-plist args)))
                  source))))

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
                   (mappend (lambda (flag)   ; Split leading "L".
                              (cond ((member flag '("-L" "-I") :test #'equal)
                                     (list flag))
                                    ((string^= "-L" flag)
                                     (list "-L" (drop-prefix "-L" flag)))
                                    ((string^= "-I" flag)
                                     (list "-I" (drop-prefix "-I" flag)))
                                    (t (list flag))))
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

(defun normalize-flag-string (dir flag-string)
  (normalize-flags dir (shlex:split flag-string)))

(defun compilation-db-entry-flags (entry)
  "Return the compiler flags in the compilation database ENTRY."
  (nest
   ;; Normalize the list of compiler flags
   (normalize-flags (command-object.directory entry))
   ;; Remove the file being built from the flags.
   (remove-if (op (equalp (command-object.file entry) _)))
   ;; Get list of compiler flags from the ENTRY
   (rest (command-object.arguments entry))))
