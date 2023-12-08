;;; compilation-database.lisp --- Compilation database data structures

(defpackage :software-evolution-library/components/compilation-database
  (:use :gt/full)
  (:local-nicknames
   (:json :cl-json))
  (:shadow :path :macro-name)
  (:import-from :shlex)
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
           :command-header-dirs
           :command-isysroot
           :parse-macro-def
           :compute-header-dirs
           :header-dir
           :header-dirs
           :*default-header-dirs*
           :preproc-macro-source)
  (:nicknames
   :sel/components/compilation-database
   :sel/components/compdb
   :sel/cp/compdb))
(in-package :software-evolution-library/components/compilation-database)

(deftype macro-def ()
  "Macro definition: either a string or nil, for a macro that has been
undefined."
  '(or null string))

(deftype macro-name ()
  "The name of a macro: a string, or, for a function-like macro, a list
of strings."
  '(or string (soft-list-of string)))

(deftype macro-alist ()
  "Alist from macro names to macro definitions."
  '(soft-alist-of macro-name macro-def))

(deftype header-dir ()
  "Specification of a directory to search for headers.
This can be a string for a concrete directory, or:
* `:current', to replace with the current directory.
* `:always', which precedes directories to always search.
* `:system', which precedes system include directories.
* `:stdinc', which should be replaced by standard includes.

Note that directories before `:always' are only searched for program
headers."
  '(or (member :current :always :system :stdinc)
    string))

(deftype header-dirs ()
  '(and (not null) (soft-list-of header-dir)))

(declaim (type header-dirs *default-header-dirs*))
(defparameter *default-header-dirs*
  '(:current :always :system :stdinc))

(progn
  (defparameter *path-flags*
    '("-L"
      "-I" "--include-directory"
      "-isystem" "-cxx-isystem"
      "-iquote"
      "-idirafter" "--include-directory-after"
      "-iprefix"
      "-isysroot" "--sysroot"))
  "Flags whose values are paths to be resolved."

  (defparameter *path-flags-table*
    (set-hash-table *path-flags* :test #'equal))

  (defparameter *normalizable-flags*
    (stable-sort (append *path-flags* '("-D" "-U")) #'length>)
    "Flags to normalize, in descending order of length.

To normalize a flag is first to split it, if it is a single (shell)
token with its argument:
    \"-Idir\" -> '\(\"-I\" \"-dir\")
Then, if the argument is a directory, it is made absolute.")

  (defparameter *normalizable-flags-table*
    (set-hash-table *normalizable-flags* :test #'equal)))

;;; Define classes for compile_commands.json and its command objects.
;;; This lets us pretend they are uniform by lazily computing any
;;; missing values.

(eval-always
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
     :path nil)))

(define-default-copy compilation-database (:around-method t))

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
        (file-command-table self)))

(defun file-command-table (compdb)
  (lret ((dict (dict)))
    (do-each (entry (command-objects compdb))
      (with-slots (directory file) entry
        (let* ((directory (pathname-as-directory (pathname directory)))
               (file (pathname file))
               (key
                ;; "All paths specified in the command or file
                ;; fields must be either absolute or relative to
                ;; this directory."
                (if (absolute-pathname-p file) file
                    (namestring
                     (canonical-pathname
                      (base-path-join directory file))))))
          (assert (absolute-pathname-p key))
          (push entry (href dict key)))))))

(declaim (notinline lookup-in-compdb))
(defun lookup-in-compdb (compdb key)
  (gethash key (file-command-objects compdb)))

(defmethod lookup ((self compilation-database) (key string))
  (lookup-in-compdb self key))

(defmethod lookup ((self compilation-database) (key pathname))
  (lookup self (namestring key)))

(eval-always
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
      :initarg :output
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
      :type macro-alist
      :reader command-preproc-defs
      :documentation "Preprocessor definition alist.")
     (header-dirs
      :type header-dirs
      :reader command-header-dirs)
     (isysroot
      :type (or null string)
      :reader command-isysroot))
    (:documentation "Entry in a compiler database")
    (:default-initargs
     :directory (required-argument :directory)
     :file (required-argument :file))))

(define-default-copy command-object ())

(defmethod print-object ((self command-object) stream)
  (print-unreadable-object (self stream :type t)
    (format stream (command-file self))))

(defmethod slot-unbound ((class t)
                         (self command-object)
                         (slot-name (eql 'arguments)))
  "Lazily compute the arguments from the command."
  (assert (slot-boundp self 'command))
  (with-slots (command arguments) self
    (setf arguments (shlex:split command))))

(defmethod slot-unbound ((class t)
                         (self command-object)
                         (slot-name (eql 'command)))
  "Lazily compute the command from the arguments."
  (assert (slot-boundp self 'arguments))
  (with-slots (command arguments) self
    (setf command
          (mapconcat #'shlex:quote arguments " "))))

(defmethod slot-unbound ((class t)
                         (self command-object)
                         (slot-name (eql 'compiler)))
  "Lazily parse out the compiler."
  (setf (slot-value self 'compiler)
        (first (command-arguments self))))

(defmethod slot-unbound ((class t)
                         (self command-object)
                         (slot-name (eql 'flags)))
  "Lazily parse out the flags."
  (setf (slot-value self 'flags)
        (compilation-db-entry-flags self)))

(defmethod slot-unbound ((class t)
                         (self command-object)
                         (slot-name (eql 'preprocessor-definitions)))
  "Lazily compute preprocessor definitions."
  (setf (slot-value self 'preprocessor-definitions)
        (preprocessor-definition-alist (command-flags self))))

(defmethod slot-unbound ((class t)
                         (self command-object)
                         (slot-name (eql 'header-dirs)))
  "Lazily compute header search path."
  (setf (slot-value self 'header-dirs)
        (compute-header-dirs (command-flags self))))

(defmethod slot-unbound ((class t)
                         (self command-object)
                         (slot-name (eql 'isysroot)))
  "Lazily compute sysroot for standard includes."
  (setf (slot-value self 'isysroot)
        (extract-isysroot (command-flags self))))

(defmethod initialize-instance :after ((self command-object) &key command arguments)
  "Require that at least one of command or arguments is provided."
  (unless (or command arguments)
    (error "Either ~s or ~s is required for a command object."
           :arguments
           :command)))

(defgeneric parse-compilation-database (source &key path)
  (:documentation "Parse SOURCE into a compilation database.
PATH is a fallback to use for the path slot.")
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

(-> parse-macro-def (string)
    (values macro-name macro-def))
(defun parse-macro-def (string)
  "Parse STRING, a macro definition.

Return the macro name and macro definition as two values."
  (if-let (pos (position #\= string))
    (let ((name (take pos string))
          (definition
           (car
            ;; Drop everything after the first newline.
            (split-sequence #\Newline
                            (drop (1+ pos) string)
                            :count 1))))
      (if-let (lparen (position #\( name))
        (if (whitespacep (aref name (1- lparen)))
            ;; A space between the name and the arg means this is not
            ;; actually a function-like macro.
            (values
             (take (position-if #'whitespacep name) name)
             (string+ (subseq name lparen)
                      #\Space
                      definition))
            ;; Parse a function-like macro.
            (let ((rparen (position #\) name)))
              (assert rparen)
              (let* ((name (take lparen string))
                     (arg-string (subseq string (1+ lparen) rparen))
                     (args (mapcar #'trim-whitespace
                                   (split-sequence #\, arg-string
                                                   :remove-empty-subseqs t))))
                (values (cons name args) definition))))
        (values name definition)))
    ;; A definition without a value has an implicit value of 1.
    (values string "1")))

(defun preproc-macro-source (mname mdef)
  "Return the source code for a macro."
  (assert (not (position #\Newline mdef)))
  (if (listp mname)
      (fmt "#define ~a(~{~a~^, ~}) ~a~%"
           (first mname)
           (rest mname)
           mdef)
      (if (null mdef)
          (fmt "#undef ~a~%" mname)
          (fmt "#define ~a ~a~%" mname mdef))))

(-> preprocessor-definition-alist ((soft-list-of string))
    (values macro-alist &optional))
(defun preprocessor-definition-alist (flags)
  "Extract an alist of macro definitions from FLAGS.
The alist maps names to definitions.

If the macro name has been undefined (with `-U'), the definition is
nil.

If the macro is a function-like macro, the \"name\" is a list of the
actual name and the arguments."
  (nreverse                             ;Later should override.
   (iter (for f in flags)
         (for p previous f)
         (cond ((equal p "-D")
                (collecting
                 (multiple-value-call #'cons
                   (parse-macro-def f))))
               ((equal p "-U")
                (collect (cons f nil)))))))

(defun extract-isysroot (split-flags)
  "Extract the sysroot for standard includes if there is one.
This is the value of `-isysroot', or the value of `--sysroot' if
`-isysroot' is not provided."
  (let (sysroot)
    (nlet rec ((split-flags split-flags))
      (match split-flags
        ((list) sysroot)
        ;; Sysroot uses two dashes, isysroot uses one.
        ((list* "--sysroot" s rest)
         (setf sysroot s)
         (rec rest))
        ((list* "-isysroot" s _)
         ;; Return the isysroot! If both are supplied
         ;; isysroot is preferred.
         s)
        ((list* _ rest)
         (rec rest))))))

(defun normalize-flags (dir flags
                        &aux (nflags *normalizable-flags*)
                          (nflags-table *normalizable-flags-table*))
  "Normalize the list of compiler FLAGS so all search paths are fully
expanded relative to DIR.

* DIR base directory for all relative paths
* FLAGS list of compiler flags

This function also expands = and $SYSROOT prefixes when --sysroot or
-isysroot is specified."
  (labels ((nflag-arg (nflag flag)
             "Normalize FLAG according to NFLAG, the flag it starts with."
             (if (string^= "--" nflag)
                 (drop-prefix "=" (drop-prefix nflag flag))
                 ;; We can't always remove =; it has meaning if
                 ;; --sysroot is specified.
                 (drop-prefix nflag flag)))
           (split-flag (flag)
             "Split leading -I, -L, etc."
             (or (some
                  (lambda (nflag)
                    (or (and (gethash flag nflags-table)
                             (list flag))
                        (and (string^= nflag flag)
                             (list nflag (nflag-arg nflag flag)))))
                  nflags)
                 (list flag)))
           (split-flags (flags)
             "Split FLAGS using `split-flag'."
             (nest (remove-if #'emptyp)
                   (mapcar #'trim-whitespace)
                   (mappend #'split-flag flags)))
           (maybe-prepend-sysroot (sysroot path)
             "Handle a PATH starting with = or $SYSROOT."
             (cond ((no sysroot) nil)
                   ((string^= "=" path)
                    (string+ sysroot (drop-prefix "=" path)))
                   ((string^= "$SYSROOT" path)
                    (string+ sysroot (drop-prefix "$SYSROOT" path)))
                   (t nil)))
           (normalize-dir (f sysroot)
             "Normalize dir F, maybe prepending SYSROOT."
             (or (maybe-prepend-sysroot sysroot f)
                 (namestring
                  (if (absolute-pathname-p f)
                      (nest (ensure-directory-pathname)
                            (canonical-pathname f))
                      (nest (canonical-pathname)
                            (merge-pathnames-as-directory
                             (ensure-directory-pathname dir)
                             (make-pathname :directory
                                            (list :relative f))))))))
           (normalize-flags (split-flags sysroot)
             (iter (for f in split-flags)
                   (for p previous f)
                   (collecting
                    (if (and dir (gethash p *path-flags-table*))
                        ;; Ensure include/library paths
                        ;; point to the correct location
                        ;; and not a temporary build directory.
                        (normalize-dir f sysroot)
                        ;; Pass the flag thru.
                        f)))))
    (declare (ftype (-> (string) list) split-flag))
    (let* ((flags (split-flags flags))
           (sysroot (extract-isysroot flags)))
      (normalize-flags flags sysroot))))

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

(-> compute-header-dirs ((soft-list-of string))
    (values header-dirs &optional))
(defun compute-header-dirs (flags)
  "Compute the header search path from FLAGS."
  (let ((current? t)
        (stdinc? t)
        iquote-dirs
        i-dirs
        isystem-dirs
        idirafter-dirs
        (iprefix ""))
    (nlet rec ((flags flags))
      (match flags
        ((list))
        ((list* (or "-nostdinc" "-nostdinc++") rest)
         (setf stdinc? nil)
         (rec rest))
        ;; "Split the include path".
        ((list* (or "-I-" "--include-barrier") rest)
         (setf current? nil
               iquote-dirs
               (nconc (shiftf i-dirs nil)
                      iquote-dirs))
         (rec rest))
        ((list* (and p (type string))
                (and f (type string))
                rest)
         ;; NB Long options seem to be Clang-only. TODO: Clang allows
         ;; = with long options.
         (string-case p
           ("-iquote"
            (push f iquote-dirs)
            (rec rest))
           (("-I" "--include-directory")
            (push f i-dirs)
            (rec rest))
           ;; TODO Is this Clang-specific.
           (("-isystem" "-cxx-isystem")
            (push f isystem-dirs)
            (rec rest))
           (("-idirafter" "--include-directory-after")
            (push f idirafter-dirs)
            (rec rest))
           ("-iprefix"
            (setf iprefix f)
            (rec rest))
           ("-iwithprefixbefore"
            (push (string+ iprefix f) i-dirs)
            (rec rest))
           ("-iwithprefix"
            (push (string+ iprefix f) idirafter-dirs)
            (rec rest))
           (otherwise (rec (rest flags)))))
        ((list* _ rest)
         (rec rest))))
    (nconc (and current? (list :current))
           (nreverse iquote-dirs)
           (list :always)
           (nreverse i-dirs)
           (list :system)
           (nreverse isystem-dirs)
           (and stdinc? (list :stdinc))
           (nreverse idirafter-dirs))))
