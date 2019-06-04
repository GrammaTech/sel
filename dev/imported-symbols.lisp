(in-package :cl-user)

;;; This code is for use in pruning down the exported symbols
;;; of SEL packages.  It is not part of SEL itself and should
;;; not be loaded as part of the SEL build.

(defparameter *default-excluded*
  '("CL" "FAST-IO" "USOCKET"  "SPLIT-SEQUENCE" "CL-STORE"
    "CL-PPCRE" "BORDEAUX-THREADS" "UIOP/COMMON-LISP" "UIOP/LAUNCH-PROGRAM"
    "UIOP/VERSION" "UIOP/PACKAGE" "UIOP/RUN-PROGRAM" "UIOP/CONFIGURATION"
    "UIOP/LISP-BUILD" "UIOP/UTILITY" "UIOP/PATHNAME" "UIOP/BACKWARD-DRIVER"
    "UIOP/STREAM" "SB-PCL" "SB-MOP" "CURRY-COMPOSE-READER-MACROS"
    "EDITOR-HINTS.NAMED-READTABLES" "ARROW-MACROS" "UIOP/IMAGE"
    "ITERATE" "UIOP/FILESYSTEM" "CLOSER-MOP"
    "ALEXANDRIA.0.DEV" "UIOP/OS" "KEYWORD" "SB-INT"
    "SB-ALIEN" "SB-IMPL" "ASDF-ENCODINGS" "OSICAT-POSIX" "OSICAT"
    "FLEXI-STREAMS" "SB-POSIX" "CFFI-SYS" "CFFI" "SB-INTROSPECT"
    "METABANG.BIND" "SB-DEBUG" "SB-EXT"
    "SB-KERNEL" "DIFF" "CL-DOT" "SB-SPROF" "STEFIL" "ELF"
    "ASDF/SYSTEM" "SWANK" "FARE-QUASIQUOTE" "OPTIMA" "SB-SYS"
    ;; "COMMAND-LINE-ARGUMENTS"
    "SNOOZE-TYPES" "COMMON-LISP-USER" "BABEL" "ECLECTOR.PARSE-RESULT"
    "ECLECTOR.READER" "CLACK" "CLACK.HANDLER" "JSON"
    "SNOOZE" "DRAKMA" ;; "TRACE-DB"
    ))

(defun imported-symbols (pd &key (exclude *default-excluded*))
  (let ((excluded-packages (mapcar #'find-package exclude))
        (pkg (find-package pd)))
    ;; Find any symbols of PKG whose home package is elsewhere
    ;; Exclude symbols in EXCLUDE
    (setf excluded-packages (cons pkg excluded-packages))
    (let ((syms nil))
      (do-symbols (s pkg)
        (unless (member (symbol-package s) excluded-packages)
          (push s syms)))
      syms)))

(defun imported-symbols-of-file (fp &key (exclude *default-excluded*))
  "Find the imported symbols of a file.  The file is assumed
to contain a single IN-PACKAGE form, from which the package
is taken.  Consider only those symbols actually referenced in
the source text."
  (let ((excluded-packages (mapcar #'find-package exclude)))
    (with-open-file (s fp :direction :input)
      (let ((the-package nil)
            (*package* *package*)
            (*readtable* (copy-readtable)))
        (let ((forms nil) (eof (list nil)))
          (loop for x = (read s nil eof)
             until (eql x eof)
             do (push x forms)
             do (labels ((%ev (x)
                           (when (consp x)
                             (case (car x)
                               (eval-when (mapc #'%ev (cddr x)))
                               (progn (mapc #'%ev (cdr x)))
                               (in-package
                                (when the-package
                                  (warn "File has more than one IN-PACKAGE form: ~a" fp)
                                  (return-from imported-symbols-of-file nil))
                                (eval x)
                                (setf the-package *package*))
                               ((named-readtables:in-readtable defpackage)
                                (eval x))))))
                  (%ev x)))
          (unless the-package
            (warn "File has no top level IN-PACKAGE form")
            (return-from imported-symbols-of-file nil))
          (pushnew the-package excluded-packages)
          (pushnew nil excluded-packages)
          (labels ((%rflatten (x) (%rflatten1 x nil))
                   (%rflatten1 (x result)
                     (loop while (consp x)
                        do (setf result (%rflatten1 (pop x) result)))
                     (typecase x
                       (null nil)
                       ;; Descend into backquote
                       #+sbcl (sb-impl::comma (setf result (%rflatten1 (sb-impl::comma-expr x) result)))
                       (t (setf result (cons x result))))
                     result))
            (let ((syms (remove-if-not #'symbolp (%rflatten forms))))
              (remove-duplicates
               (remove-if (lambda (s) (member (symbol-package s)
                                              excluded-packages))
                          syms)))))))))

(defun count-imported-symbols-of-files (files)
  "For each file in a list FILES, produce a pair (file count),
where COUNT is the number of distinct imported symbols used
in FILE, or :ERROR if this could not be determined."
  (loop for (f l) in (imported-symbols-of-files files)
       collect (list f (if (listp l) (length l) l))))

(defun imported-symbols-of-files (files)
  "For each file in a list FILES, produce a pair (file syms),
where SYMS is a list of the distinct imported symbols used
in FILE, or :ERROR if this could not be determined."
  (loop for f in files
     collect (list f (handler-case (imported-symbols-of-file f)
                       (error () :error)))))
