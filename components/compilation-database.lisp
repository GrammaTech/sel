;;; compilation-database.lisp --- Compilation database data structures

(defpackage :software-evolution-library/components/compilation-database
  (:use :gt/full)
  (:local-nicknames
   (:json :cl-json))
  (:shadow :path)
  (:import-from :shlex)
  (:shadowing-import-from :serapeum :~>)
  (:export :parse-compilation-database
           :normalize-flags
           :normalize-flags-string
           :compilation-database
           :command-objects
           :disk-path
           :file-command-objects
           :command-object
           :command-directory
           :command-file
           :command-arguments
           :command-string
           :command-output
           :command-flags
           :command-compiler
           :command-preproc-defs
           :parse-macro-definition))
(in-package :software-evolution-library/components/compilation-database)

(deftype macro-def ()
  ;; Null for a canceled definition.
  '(or null string))

(defparameter *normalizable-flags*
  '#.(stable-sort (list
                   "-L" "-l"
                   "-I" "-isystem" "iquote"
                   "-D" "-U")
                  #'length>)
  "Flags to normalize, in descending order of length.")

(defparameter *path-flags*
  '("-L" "-I" "-isystem" "-iquote")
  "Flags whose values are paths to be resolved.")

;;; Define classes for compile_commands.json and its command objects.
;;; This lets us pretend they are uniform by lazily computing any
;;; missing values.

(defclass compilation-database ()
  ((command-objects
    :type list
    :initarg :command-objects
    :reader command-objects
    :documentation "List of command objects.")
   (path
    :type (or pathname null)
    :initarg :path
    :reader disk-path
    :documentation "On-disk path of compilation database (optional).")
   (size
    :type (integer 0 *)
    :initarg :size
    :reader size
    :documentation "Length of entry list.")
   (file-command-objects
    :type hash-table
    :documentation "Map from each file to its command objects."
    :reader file-command-objects))
  (:documentation "A JSON compilation database.
See <https://clang.llvm.org/docs/JSONCompilationDatabase.html>.")
  (:default-initargs
   :command-objects nil
   :path nil))

(defmethod print-object ((self compilation-database) stream)
  (print-unreadable-object (self stream :type t)
    (princ (disk-path self) stream)))

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
          (do-each (entry (command-objects self))
            (with-slots (file) entry
              (push entry (href dict file)))))))

(defclass command-object ()
  ((directory
    :type string
    :initarg :directory
    :reader command-directory
    :documentation "The working directory.")
   (file
    :type string :initarg :file
    :reader file
    :reader command-file
    :documentation "The source file.")
   (arguments
    :type (soft-list-of string)
    :initarg :arguments
    :reader command-arguments
    :documentation "The compiler command, as an argv.")
   (command
    :type string
    :initarg :command
    :reader command-string
    :documentation "The compile command, as one string.")
   (output
    :type string
    :initarg output
    :reader command-output
    :documentation "The output file.")
   (flags
    :type (soft-list-of string)
    :reader command-flags
    :documentation "Compiler flags.")
   (compiler
    :type string
    :reader command-compiler
    :documentation "compiler")
   (preprocessor-definitions
    :type (soft-alist-of string macro-def)
    :reader command-preproc-defs
    :documentation "Preprocessor definition alist."))
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
        (first (command-arguments self))))

(defmethod slot-unbound ((class t)
                         (self command-object)
                         (slot-name (eql 'flags)))
  ;; Lazily parse out the flags.
  (setf (slot-value self 'flags)
        (compilation-db-entry-flags self)))

(defmethod slot-unbound ((class t)
                         (self command-object)
                         (slot-name (eql 'preprocessor-definitions)))
  (setf (slot-value self 'preprocessor-definitions)
        (preprocessor-definition-alist (command-flags self))))

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

(-> parse-macro-definition (string)
    (values string macro-def))
(defun parse-macro-definition (string)
  (if-let (pos (position #\= string))
    (let ((name (take pos string))
          (definition
           (~> string
               (drop (1+ pos) _)
               ;; Drop everything after the first newline.
               (split-sequence #\Newline _ :count 1)
               car)))
      ;; TODO
      (when (find #\( name)
        (warn "Function-like macro not parsed"))
      (values name definition))
    ;; A definition without a value has an implicit value of 1.
    (values string "1")))

(defun preprocessor-definition-alist (flags)
  (nreverse                             ;Later should override.
   (iter (for f in flags)
         (for p previous f)
         (cond ((equal p "-D")
                (collecting
                 (multiple-value-call #'cons
                   (parse-macro-definition f))))
               ((equal p "-U")
                (collect (cons f nil)))))))

(defun normalize-flags (dir flags)
  "Normalize the list of compiler FLAGS so all search paths are fully
expanded relative to DIR.

* DIR base directory for all relative paths
* FLAGS list of compiler flags
"
  (labels ((split-flags (flags &aux (normalizable-flags *normalizable-flags*))
             (nest (remove-if #'emptyp)
                   (mapcar #'trim-whitespace)
                   (mappend (lambda (flag)   ; Split leading -I, -L, etc.
                              (or (some (lambda (nflag)
                                          (and (string^= nflag flag)
                                               (if (length= nflag flag)
                                                   (list flag)
                                                   (list nflag
                                                         (drop-prefix nflag flag)))))
                                        normalizable-flags)
                                  (list flag)))
                            flags))))
    (iter (for f in (split-flags flags))
          (for p previous f)
          (collect (if (and dir
                            (member p *path-flags* :test #'equal))
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

(defun normalize-flags-string (dir flag-string)
  (normalize-flags dir (shlex:split flag-string)))

(defun compilation-db-entry-flags (entry)
  "Return the compiler flags in the compilation database ENTRY."
  (nest
   ;; Normalize the list of compiler flags
   (normalize-flags (command-directory entry))
   ;; Remove the file being built from the flags.
   (remove-if (op (equalp (command-file entry) _)))
   ;; Get list of compiler flags from the ENTRY
   (rest (command-arguments entry))))
