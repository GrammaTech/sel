;;; utility.lisp --- utility functions

;;; Code:
(in-package :software-evolution-library/utility)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-preserving-case (stream char n)
    (declare (ignorable char) (ignorable n))
    (let ((*readtable* (copy-readtable nil)))
      (setf (readtable-case *readtable*) :preserve)
      (cl-user::read stream t nil t)))

  (defreadtable :sel-readtable
    ;; Define an SEL readtable which combines CCRM with a #! reader
    ;; macro for preserving case reads.  Both can be useful generally.
    (:merge :curry-compose-reader-macros)
    (:dispatch-macro-char #\# #\! #'read-preserving-case))

  (in-readtable :sel-readtable))

(defvar infinity
  #+sbcl
  sb-ext:double-float-positive-infinity
  #+ccl
  ccl::double-float-positive-infinity
  #+allegro
  excl:*infinity-double*
  #+ecl
  ext:long-float-positive-infinity
  #-(or ecl sbcl ccl allegro)
  (error "must specify a positive infinity value"))

(define-condition git (error)
  ((description :initarg :description :initform nil :reader description))
  (:report (lambda (condition stream)
             (format stream "Git failed: ~a" (description condition)))))

(defmacro with-git-directory ((directory git-dir) &rest body)
  (with-gensyms (recur dir)
    `(labels
         ((,recur (,dir)
            (when (< (length ,dir) 2)
              (error (make-condition 'git
                       :description
                       (format nil "~a is not in a git repository." ,dir))))
            (handler-case
                (let ((,git-dir (make-pathname
                                 :directory (append ,dir (list ".git")))))
                  (if (probe-file git-dir)
                      ,@body
                      (,recur (butlast ,dir))))
              (error (e)
                (error (make-condition 'git
                         :description
                         (format nil "~a finding git information." e)))))))
       (,recur ,directory))))

(defun current-git-commit (directory)
  (with-git-directory (directory git-dir)
    (with-open-file (git-head-in (merge-pathnames "HEAD" git-dir))
      (let ((git-head (read-line git-head-in)))
        (if (scan "ref:" git-head)
            (with-open-file (ref-in (merge-pathnames
                                     (second (split-sequence #\Space git-head))
                                     git-dir))
              (subseq (read-line ref-in) 0 7)) ; attached head
            (subseq git-head 0 7))))))         ; detached head

(defun current-git-branch (directory)
  (with-git-directory (directory git-dir)
    (with-open-file (git-head-in (merge-pathnames "HEAD" git-dir))
      (lastcar (split-sequence #\/ (read-line git-head-in))))))

#+sbcl
(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (sb-alien:define-alien-routine (#-win32 "tempnam" #+win32 "_tempnam" tempnam)
      sb-alien:c-string
    (dir sb-alien:c-string)
    (prefix sb-alien:c-string)))

#+ccl
(defun tempnam (dir prefix)
  (ccl:with-filename-cstrs ((base dir) (prefix (or prefix "")))
    (ccl:get-foreign-namestring
     (ccl:external-call "tempnam" :address base :address prefix :address))))

#+ecl
(defun tempnam (dir &optional prefix)
  (let ((dir-name (uiop:ensure-directory-pathname
                   (or dir *temporary-directory*))))
    (ext:mkstemp (if prefix
                     (uiop::merge-pathnames dir-name prefix)
                     dir-name))))

(defun file-to-string
    (pathname &key (external-format
                    (encoding-external-format (detect-encoding pathname))))
  #+ccl (declare (ignorable external-format))
  (restart-case
      (let (#+sbcl (sb-impl::*default-external-format* external-format)
                   #+ecl (ext:*default-external-format* external-format))
        (with-open-file (in pathname)
          (let* ((file-bytes (file-length in))
                 (seq (make-string file-bytes))
                 (file-chars (read-sequence seq in)))
            (if (= file-bytes file-chars)
                seq
                ;; Truncate the unused tail of seq.  It is possible
                ;; for read-sequence to read less than file-length
                ;; when the file has multi-byte UTF-8 characters.
                (subseq seq 0 file-chars)))))
    ;; Try a different encoding
    (use-encoding (encoding)
      :report "Specify another encoding"
      (file-to-string pathname :external-format encoding))))

(defun file-to-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((seq (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence seq in)
      seq)))

(defun stream-to-string (stream)
  (when stream
    (with-open-stream (in stream)
      (iter (for char = (read-char in nil nil))
            (while char)
            (collect char into chars)
            (finally (return (coerce chars 'string)))))))

(defun string-to-file (string path &key (if-exists :supersede))
  (with-open-file (out path :direction :output :if-exists if-exists)
    (format out "~a" string))
  path)

(defun bytes-to-file (bytes path &key (if-exists :supersede))
  (with-open-file (out path :element-type '(unsigned-byte 8)
                       :direction :output :if-exists if-exists)
    (write-sequence bytes out)))

(defvar *temp-dir* nil
  "Set to non-nil for a custom temporary directory.")

(defun temp-file-name (&optional ext)
  (let ((base #+clisp
          (let ((stream (gensym)))
            (eval `(with-open-stream
                       (,stream (ext:mkstemp
                                 (if *temp-dir*
                                     (namestring (make-pathname
                                                  :directory *temp-dir*
                                                  :name "XXXXXX"))
                                     nil)))
                     (pathname ,stream))))
          #+(or sbcl ccl ecl)
          (tempnam *temp-dir* nil)
          #+allegro
          (system:make-temp-file-name nil *temp-dir*)
          #-(or sbcl clisp ccl allegro ecl)
          (error "no temporary file backend for this lisp.")))
    (if ext
        (if (pathnamep base)
            (namestring (make-pathname :directory (pathname-directory base)
                                       :name (pathname-name base)
                                       :type ext))
            (concatenate 'string base "." ext))
        (if (pathname base)
            (namestring base)
            base))))

(defun ensure-temp-file-free (path)
  "Delete anything at PATH."
  (let ((probe (probe-file path)))
    (when probe
      (if (equal (directory-namestring probe)
                 (namestring probe))
          (progn
            #+sbcl (sb-ext:delete-directory probe :recursive t)
            #+ccl (ccl:delete-directory probe)
            #-(or sbcl ccl)
            (uiop/filesystem:delete-directory-tree
             (uiop:ensure-directory-pathname probe)
             :validate t))
          (delete-file path)))))

(defmacro with-temp-file (spec &rest body)
  "SPEC holds the variable used to reference the file w/optional extension.
After BODY is executed the temporary file is removed."
  `(let ((,(car spec) (temp-file-name ,(second spec))))
     (unwind-protect (progn ,@body) (ensure-temp-file-free ,(car spec)))))

(defmacro with-temp-fifo (spec &rest body)
  `(with-temp-file ,spec
     (ensure-temp-file-free ,(car spec))
     (mkfifo ,(car spec) (logior osicat-posix:s-iwusr osicat-posix:s-irusr))
     ,@body))

(defmacro with-temp-dir (spec &rest body)
  `(with-temp-file ,spec
     (ensure-directories-exist (ensure-directory-pathname ,(car spec)))
     ,@body))

(defmacro with-cwd (dir &rest body)
  "Change the current working directory to dir and execute body.
WARNING: This function is not thread safe.  Execution in a threaded
environment may causes execution outside of the intended directory or
may lose the original working directory."
  (with-gensyms (orig)
    `(let ((,orig (getcwd)))
       (unwind-protect
         (progn (cd ,(car dir)) ,@body)
         (cd ,orig)))))

(defun pwd ()
  (getcwd))

(defun cd (directory)
  (let ((pathname (probe-file directory)))
    (unless pathname
      (error "Directory ~S does not exist." directory))
    (unless (directory-exists-p (ensure-directory-pathname pathname))
      (error "Directory ~S is not a directory." directory))
    (setf *default-pathname-defaults* pathname)
    (chdir pathname)))

(defmacro with-temp-file-of (spec str &rest body)
  "SPEC should be a list of the variable used to reference the file
and an optional extension."
  `(let ((,(car spec) (temp-file-name ,(second spec))))
     (unwind-protect (progn (string-to-file ,str ,(car spec)) ,@body)
       (when (probe-file ,(car spec)) (delete-file ,(car spec))))))

(defmacro with-temp-file-of-bytes (spec bytes &rest body)
  "SPEC should be a list of the variable used to reference the file
and an optional extension."
  `(let ((,(car spec) (temp-file-name ,(second spec))))
     (unwind-protect (progn (bytes-to-file ,bytes ,(car spec)) ,@body)
       (when (probe-file ,(car spec)) (delete-file ,(car spec))))))

(defmacro with-temp-dir-of (spec dir &rest body)
  "Populate SPEC with the path to a temporary directory with the contents
of DIR and execute BODY"
  `(with-temp-dir ,spec
     (shell "cp -pr ~a/* ~a" (namestring ,dir) (namestring ,(car spec)))
     ,@body))

(defmacro with-temp-files (specs &rest body)
  (labels ((expander (specs body)
             (let ((s (car specs)))
               `(let ((,(car s) (temp-file-name ,(second s))))
                  (unwind-protect
                       ,(if (cdr specs)
                            (expander (cdr specs) body)
                            `(progn ,@body))
                    (when (probe-file ,(car s)) (delete-file ,(car s))))))))
    (expander (mapcar (lambda (s)
                        (if (listp s) s (list s)))
                      specs) body)))

(defun ensure-path-is-string (path)
  (cond
    ((stringp path) path)
    ((pathnamep path) (namestring path))
    (:otherwise (error "Path not string ~S." path))))

(defun in-directory (directory path)
  "Return PATH based in DIRECTORY.
Uses `ensure-directory-pathname' to force DIRECTORY to be a directory
pathname (i.e., ending in a \"/\")."
  (let ((directory (ensure-directory-pathname directory)))
    (make-pathname
     :host (pathname-host directory)
     :device (pathname-device directory)
     :directory (append (pathname-directory directory)
                        (cdr (pathname-directory path)))
     :name (pathname-name path)
     :type (pathname-type path)
     :version (pathname-version path))))

(defun directory-p (pathname)
  "Return a directory version of PATHNAME if it indicates a directory."
  (if (directory-pathname-p pathname)
      pathname
      ;; When T `directory-exists-p' (like this function) returns the pathname.
      (directory-exists-p (pathname-as-directory pathname))))


;;;; Utilities from cl-fad.
;;;
;;; The following three functions (`merge-pathnames-as-file',
;;; `merge-pathnames-as-directory', and `canonical-pathname') were
;;; adapted from cl-fad.  The CL-FAD license applies to the source for
;;; these functions and is included below:
;;;
;;; Copyright (c) 2004, Peter Seibel.  All rights reserved.
;;; Copyright (c) 2004-2010, Dr. Edmund Weitz.  All rights reserved.
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;
(defun canonical-pathname (path)
  "Remove redundant information from PATH."
  (iter (with full-dir = (or (pathname-directory (pathname path))
                             (list :relative)))
        (with canon-dir = (if (member (first full-dir) '(:relative :absolute))
                              (list (pop full-dir))
                              (list :relative)))
        (while full-dir)
        (cond ((string= "." (first full-dir))
               (pop full-dir))
              ((eql :back (second full-dir))
               (pop full-dir)
               (pop full-dir))
              (t (push (pop full-dir) canon-dir)))
        (finally (return (make-pathname :defaults (pathname path)
                                        :directory (nreverse canon-dir))))))

(defun merge-pathnames-as-directory (&rest pathnames)
  "Given a list of pathnames, this returns a single
directory pathname containing the logical concatenation of them all."
  (if (null pathnames)
      (make-pathname)
      (let* ((pathnames (mapcar #'pathname pathnames))
             (defaults (first pathnames))
             (dir (pathname-directory defaults)))
        (make-pathname
          :defaults defaults
          :directory (iter (for pathname in (rest pathnames))
                           (for directory = (pathname-directory pathname))
                           (cond ((eq :absolute (first directory))
                                  (setf dir directory))
                                 ((eq :relative (first directory))
                                  (setf dir (append dir (rest directory)))))
                           (finally (return dir)))
          :name nil :type nil))))

(defun merge-pathnames-as-file (&rest pathnames)
  "Given a list of pathnames returns a single filename pathname
containing the logical concatenation of them all."
  (if (null pathnames)
      (make-pathname)
      (let ((file-name (first (last pathnames))))
        (make-pathname :defaults (apply #'merge-pathnames-as-directory pathnames)
                       :name (pathname-name file-name)
                       :type (pathname-type file-name)
                       :version (pathname-version file-name)))))


;;;; Process wrapping
;;;
;;; TODO: What is the benefit of this wrapper layer?  Just interop
;;;       between lisps implementations?  Does nothing else already
;;;       provide this?
;;;
(defclass process ()
  ((os-process
    :initarg :os-process :initform nil :reader os-process
    :documentation "The underlying process object (compiler-specific).
This field will not usually need to be accessed directly: use methods
`process-input-stream', `process-output-stream',
`process-error-stream', `process-error-code', `process-status',
`signal-process' to interact with processes."))
  (:documentation "Object representing an external process.
Wraps around SBCL- or CCL-specific representations of external processes."))

(defgeneric process-id (process)
  (:documentation "Return the process id for PROCESS"))

(defmethod process-id ((process process))
  "Return the process id for PROCESS."
  (process-info-pid (os-process process)))

(defgeneric process-input-stream (process)
  (:documentation "Return the input stream for PROCESS."))

(defmethod process-input-stream ((process process))
  "Return the input stream for PROCESS."
  (process-info-input (os-process process)))

(defgeneric process-output-stream (process)
  (:documentation "Return the output stream for PROCESS."))

(defmethod process-output-stream ((process process))
  "Return the output stream for PROCESS."
  (process-info-output (os-process process)))

(defgeneric process-error-stream (process)
  (:documentation "Return the error stream for PROCESS."))

(defmethod process-error-stream ((process process))
  "Return the error stream for PROCESS."
  (process-info-error-output (os-process process)))

(defgeneric process-exit-code (process)
  (:documentation
   "Return the exit code for PROCESS, or nil if PROCESS has not exited."))

(defmethod process-exit-code ((process process))
  "Return the exit code for PROCESS, or nil if PROCESS has not exited."
  (and (not (process-running-p process))
       (wait-process (os-process process))))

(defgeneric process-running-p (process)
  (:documentation "Return T if PROCESS is running, NIL otherwise."))

(defmethod process-running-p ((process process))
  "Return T if PROCESS is running, NIL otherwise."
  (process-alive-p (os-process process)))

(defgeneric kill-process (process &key urgent)
  (:documentation
   "Send a kill signal to PROCESS. If URGENT is T, send SIGKILL."))

(defmethod kill-process (process &key urgent)
  (uiop/launch-program::terminate-process (os-process process) :urgent urgent))


;;;; Shell and system command helpers
(defvar *shell-debug* nil
  "Set to true to print shell invocations.")

(defvar *shell-error-codes* '(126 127)
  "Raise a condition on these exit codes.")

(defvar *shell-non-error-codes* nil
  "Raise a condition on any but these exit codes.")

(define-condition shell-command-failed (error)
  ((commmand :initarg :command :initform nil :reader command)
   (exit-code :initarg :exit-code :initform nil :reader exit-code))
  (:report (lambda (condition stream)
             (format stream "Shell command failed with status ~a: \"~a\""
                     (exit-code condition) (command condition)))))

(defun shell (control-string &rest format-arguments &aux input)
  "Apply CONTROL-STRING to FORMAT-ARGUMENTS and execute the result with a shell.
Return (values stdout stderr errno).  FORMAT-ARGUMENTS up to the first
keyword are passed to `format' with CONTROL-STRING to construct the
shell command.  All subsequent elements of FORMAT-ARGUMENTS are passed
through as keyword arguments to `uiop:run-program'.

Raise a `shell-command-failed' exception depending on the combination
of errno with `*shell-error-codes*' and `*shell-non-error-codes*'.

Optionally print debug information if `*shell-debug*' is non-nil."
  (let ((format-arguments (take-until #'keywordp format-arguments))
        (run-program-arguments (drop-until #'keywordp format-arguments)))
    ;; Manual handling of an :input keyword argument.
    (when-let ((input-arg (plist-get :input run-program-arguments)))
      (setq input
            (if (stringp input-arg)
                (make-string-input-stream input-arg)
                input-arg))
      (setq run-program-arguments (plist-drop :input run-program-arguments)))
    (let ((cmd (apply #'format (list* nil control-string format-arguments)))
          (stdout-str nil)
          (stderr-str nil)
          (errno nil))
      (when *shell-debug*
        (format t "  cmd: ~a~%" cmd)
        (when input
          (format t "  input: ~a~%" input)))

      ;; Direct shell execution with `uiop/run-program:run-program'.
      #-ccl
      (progn
        (setf stdout-str (make-array '(0)
                                     :element-type
                                     #+sbcl 'extended-char
                                     #-sbcl 'character
                                     :fill-pointer 0 :adjustable t))
        (setf stderr-str (make-array '(0)
                                     :element-type
                                     #+sbcl 'extended-char
                                     #-sbcl 'character
                                     :fill-pointer 0 :adjustable t))
        (with-output-to-string (stderr stderr-str)
          (with-output-to-string (stdout stdout-str)
            (setf errno (nth-value 2 (apply #'run-program
                                            cmd
                                            :force-shell t
                                            :ignore-error-status t
                                            :input input
                                            :output stdout
                                            :error-output stderr
                                            run-program-arguments))))))
      #+ccl
      (progn
        (with-temp-file (stdout-file)
          (with-temp-file (stderr-file)
            (setf errno (nth-value 2 (apply #'run-program
                                            (format nil "~a 1>~a 2>~a"
                                                    cmd stdout-file stderr-file)
                                            :force-shell t
                                            :ignore-error-status t
                                            :input input
                                            run-program-arguments)))
            (setf stdout-str (if (probe-file stdout-file)
                                 (file-to-string stdout-file)
                                 ""))
            (setf stderr-str (if (probe-file stderr-file)
                                 (file-to-string stderr-file)
                                 "")))))
      (when *shell-debug*
        (format t "~&stdout:~a~%stderr:~a~%errno:~a"
                stdout-str stderr-str errno))
      (when (or (and *shell-non-error-codes*
                     (not (find errno *shell-non-error-codes*)))
                (find errno *shell-error-codes*))
        (restart-case (error (make-condition 'shell-command-failed
                               :exit-code errno
                               :command cmd))
          (ignore-shell-error () "Ignore error and continue")))
      (values stdout-str stderr-str errno))))

(defmacro write-shell-file
    ((stream-var file shell &optional args) &rest body)
  "Executes BODY with STREAM-VAR passing through SHELL to FILE."
  (let ((proc-sym (gensym)))
    `(let* ((,proc-sym
             #+sbcl (sb-ext:run-program ,shell ,args :search t
                                        :output ,file
                                        :input :stream
                                        :wait nil)
             #+ccl (ccl:run-program ,shell ,args :output ,file
                                    :input :stream
                                    :wait nil)
             #+ecl (ext:run-program ,shell ,args :output ,file
                                    :input :stream
                                    :wait nil)
             #-(or sbcl ccl ecl)
             (error
              "`WRITE-SHELL-FILE' only implemented for SBCL, CCL, and ECL.")))
       (unwind-protect
            (with-open-stream
                (,stream-var #+sbcl (sb-ext:process-input ,proc-sym)
                             #+ccl (ccl:external-process-input-stream ,proc-sym)
                             #+ecl (ext:external-process-input ,proc-sym)
                             #-(or sbcl ccl ecl)
                             (error "Only SBCL, CCL or ECL."))
              ,@body)))))

(defmacro read-shell-file
    ((stream-var file shell &optional args) &rest body)
  "Executes BODY with STREAM-VAR passing through SHELL from FILE."
  #+(or sbcl ccl ecl)
  (let ((proc-sym (gensym)))
    `(let* ((,proc-sym
             #+sbcl (sb-ext:run-program ,shell ,args :search t
                                        :output :stream
                                        :input ,file
                                        :wait nil)
             #+ccl (ccl:run-program ,shell ,args :output :stream
                                    :input ,file
                                    :wait nil)
             #+ecl (ext:run-program ,shell ,args :output :stream
                                    :input ,file
                                    :wait nil)
             #-(or sbcl ccl ecl)
             (error
              "`READ-SHELL-FILE' only implemented for SBCL, CCL or ECL.")))
       (unwind-protect
            (with-open-stream
                (,stream-var
                 #+sbcl (sb-ext:process-output ,proc-sym)
                 #+ccl (ccl:external-process-output-stream ,proc-sym)
                 #+ecl (ext:external-process-output ,proc-sym)
                 #-(or sbcl ccl ecl) (error "Only SBCL or CCL."))
              ,@body)))))

(defvar *bash-shell* "/bin/bash"
  "Bash shell for use in `read-shell'.")

#+sbcl
(defmacro read-shell ((stream-var shell) &rest body)
  "Executes BODY with STREAM-VAR holding the output of SHELL.
The SHELL command is executed with `*bash-shell*'."
  (let ((proc-sym (gensym)))
    `(let* ((,proc-sym (sb-ext:run-program *bash-shell*
                                           (list "-c" ,shell) :search t
                                           :output :stream
                                           :wait nil)))
       (with-open-stream (,stream-var (sb-ext:process-output ,proc-sym))
         ,@body))))

#-sbcl
(defmacro read-shell (&rest args)
  (declare (ignorable args))
  (error "`READ-SHELL' unimplemented for non-SBCL lisps."))

(defmacro xz-pipe ((in-stream in-file) (out-stream out-file) &rest body)
  "Executes BODY with IN-STREAM and OUT-STREAM read/writing data from xz files."
  `(read-shell-file (,in-stream ,in-file "unxz")
     (write-shell-file (,out-stream ,out-file "xz")
       ,@body)))

(define-condition parse-number (error)
  ((text :initarg :text :initform nil :reader text))
  (:report (lambda (condition stream)
             (format stream "Can't parse ~a as a number" (text condition)))))

(defun parse-number (string)
  "Parse the number located at the front of STRING or return an error."
  (let ((number-str
         (or (multiple-value-bind (whole matches)
                 (scan-to-strings
                  "^(-?.?[0-9]+(/[-e0-9]+|\\.[-e0-9]+)?)([^\\./A-Xa-x_-]$|$)"
                  string)
               (declare (ignorable whole))
               (when matches (aref matches 0)))
             (multiple-value-bind (whole matches)
                 (scan-to-strings "0([xX][0-9A-Fa-f]+)([^./]|$)"
                                  string)
               (declare (ignorable whole))
               (when matches (concatenate 'string "#" (aref matches 0)))))))
    (unless number-str
      (make-condition 'parse-number :text string))
    (read-from-string number-str)))

(defun parse-numbers (string &key (radix 10) (delim #\Space))
  (mapcar #'(lambda (num) (parse-integer num :radix radix))
          (split-sequence delim string :remove-empty-subseqs t)))

(defun trim-whitespace (str)
  (string-trim '(#\Space #\Tab #\Newline #\Linefeed)
               str))

(defun make-terminal-raw ()
  "Place the terminal into 'raw' mode, no echo non canonical.
This allows characters to be read directly without waiting for a newline.
See 'man 3 termios' for more information."
  #+win32 (error "`make-terminal-raw' not implemented for windows.")
  #-sbcl (error "`make-terminal-raw' not implemented for non-SBCL.")
  #+sbcl
  (let ((options (sb-posix:tcgetattr 0)))
    (setf (sb-posix:termios-lflag options)
          (logand (sb-posix:termios-lflag options)
                  (lognot (logior sb-posix:icanon
                                  sb-posix:echo
                                  sb-posix:echoe
                                  sb-posix:echok
                                  sb-posix:echonl
                                  sb-posix:echo))))
    (sb-posix:tcsetattr 0 sb-posix:TCSANOW options)))

(defun which (file &key (path (getenv "PATH")))
  (iterate (for dir in (split-sequence #\: path))
           (let ((fullpath (merge-pathnames file
                                            (make-pathname :directory dir))))
             (when (probe-file fullpath)
               (return fullpath)))))

(defmacro getopts (args-and-opts &body forms)
  "Collect command-line options from ARGS in an executable.

For usage see the definition of `clang-instrument'.  E.g.,

    (getopts
      (\"-c\" \"--compiler\" (setf (compiler original) (pop args)))
      (\"-e\" \"--exit\" (setf instrument-exit t))
      (\"-F\" \"--flags\" (setf (flags original) (split-sequence #\, (pop args))))
      #| ... |#)
"
  (let ((arg (gensym))
        (getopts-block (gensym))
        (unknown (or (plist-get :unknown (cdr args-and-opts)) :error)))
    `(block ,getopts-block
       (loop :for ,arg = (pop ,(car args-and-opts)) :while ,arg :do
          (cond
            ,@(mapcar (lambda-bind ((short long . body))
                        `((or (string= ,arg ,short) (string= ,arg ,long))
                          ,@body))
                      forms)
            (:otherwise
             ,(case unknown
               (:error `(error "Unrecognized argument:~a" ,arg))
               (:return `(progn (push ,arg ,(car args-and-opts))
                                (return-from ,getopts-block))))))))))


;;;; generic forensic functions over arbitrary objects
(defun arglist (fname)
  "Return the argument list of FNAME."
  ;; Taken from swank/backend:arglist.
  #+sbcl
  (sb-introspect:function-lambda-list fname)
  ;; NOTE: The following is similar, but may return 0 for nil args.
  ;; (sb-kernel:%simple-fun-arglist fname)
  #+ecl
  (multiple-value-bind (arglist foundp)
      (ext:function-lambda-list name)
    (if foundp arglist :not-available))
  #+ccl
  (multiple-value-bind (arglist binding) (let ((*break-on-signals* nil))
                                           (ccl:arglist fname))
    (if binding
        arglist
        :not-available))
  #-(or ecl sbcl ccl)
  (error "Only ECL, SBCL, and CCL."))

(defun show-it (hd &optional out)
  "Print the fields of a elf, section or program header.
Optional argument OUT specifies an output stream."
  (format (or out t) "~&")
  (mapcar
   (lambda (slot)
     (let ((val (slot-value hd slot)))
       (format (or out t) "~s:~a " slot val)
       (list slot val)))
   (mapcar #'slot-definition-name (class-slots (class-of hd)))))

(defun equal-it (obj1 obj2 &optional trace)
  "Equal over objects and lists."
  (let ((trace1 (concatenate 'list (list obj1 obj2) trace)))
    (cond
      ((or (member obj1 trace) (member obj2 trace)) t)
      ((and (listp obj1) (not (listp (cdr obj1)))
            (listp obj2) (not (listp (cdr obj2))))
       (and (equal-it (car obj1) (car obj2))
            (equal-it (cdr obj1) (cdr obj2))))
      ((or (and (listp obj1) (listp obj2)) (and (vectorp obj1) (vectorp obj2)))
       (and (equal (length obj1) (length obj2))
            (reduce (lambda (acc pair)
                      (and acc (equal-it (car pair) (cdr pair) trace1)))
                    (if (vectorp obj1)
                        (mapcar #'cons (coerce obj1 'list) (coerce obj2 'list))
                        (mapcar #'cons obj1 obj2))
                    :initial-value t)))
      ((class-slots (class-of obj1))
       (reduce (lambda (acc slot)
                 (and acc (equal-it (slot-value obj1 slot) (slot-value obj2 slot)
                                    trace1)))
               (mapcar #'slot-definition-name
                       (class-slots (class-of obj1)))
               :initial-value t))
      (t (equal obj1 obj2)))))

(defmacro with-quiet-compilation (&body body)
  `(let ((*load-verbose* nil)
         (*compile-verbose* nil)
         (*load-print* nil)
         (*compile-print* nil)
         (uiop/lisp-build:*uninteresting-conditions*
          uiop/lisp-build:*usual-uninteresting-conditions*))
     ,@body))

(defmacro repeatedly (times &rest body)
  (let ((ignored (gensym)))
    `(loop :for ,ignored :below ,times :collect ,@body)))

(defun indexed (list)
  (loop :for element :in list :as i :from 0 :collect (list i element)))

(defun different-it (obj1 obj2 &optional trace)
  (let ((trace1 (concatenate 'list (list obj1 obj2) trace)))
    (cond
      ((or (member obj1 trace) (member obj2 trace)) t)
      ((or (and (vectorp obj1) (vectorp obj2))
           (and (proper-list-p obj1) (proper-list-p obj2)))
       (and (or (equal (length obj1) (length obj2))
                (format t "~&different lengths ~a!=~a"
                        (length obj1) (length obj2)))
            (reduce (lambda-bind (acc (i (a b)))
                      (and acc (or (different-it a b trace1)
                                   (format t "~& at ~d ~a!=~a" i a b))))
                    (indexed
                     (if (vectorp obj1)
                         (mapcar #'list (coerce obj1 'list) (coerce obj2 'list))
                         (mapcar #'list obj1 obj2)))
                    :initial-value t)))
      ((and (consp obj1) (consp obj2))
       (and (different-it (car obj1) (car obj2))
            (different-it (cdr obj1) (cdr obj2))))
      ((class-slots (class-of obj1))
       (reduce (lambda (acc slot)
                 (and acc (or (different-it
                               (slot-value obj1 slot) (slot-value obj2 slot)
                               trace1)
                              (format t "~&  ~a" slot))))
               (mapcar #'slot-definition-name
                       (class-slots (class-of obj1)))
               :initial-value t))
      (t (or (equal obj1 obj2) (format t "~&~a!=~a" obj1 obj2))))))

(defun count-cons (cons-cell)
  "Count and return the number of cons cells used in CONS-CELL."
  ;; TODO: extend to map over the fields in an object.
  (the fixnum (if (consp cons-cell)
                  (+ (count-cons (car cons-cell))
                     (count-cons (cdr cons-cell)))
                  1)))

(defun tree-right-length (tree &aux (size 1))
  "Return the length of the right spine of TREE."
  (declare (optimize speed))
  (iter (while (consp tree))
        (setf tree (cdr tree))
        (incf (the fixnum size)))
  (the fixnum size))

(defun tree-right-walk (tree)
  "Return the right spine of TREE as a list."
  (declare (optimize speed))
  (if tree
      (if (consp tree)
          (cons (car tree) (tree-right-walk (cdr tree)))
          (list tree))
      nil))


;;;; Generic utility functions
(defun mapt (function tree)
  "Like `mapcar' but TREE is a cons tree instead of a proper list."
  (if (consp tree)
      (cons (mapt function (car tree))
            (mapt function (cdr tree)))
      (funcall function tree)))

(defun plist-get (item list &key (test #'eql) &aux last)
  (loop :for element :in list :do
     (cond
       (last (return element))
       ((funcall test item element) (setf last t)))))

(defun plist-keys (plist)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (iter (for (key value) on plist by #'cddr)
        (declare (ignorable value))
        (collect key)))

(defun plist-drop-if (predicate list &aux last)
  (nreverse (reduce (lambda (acc element)
                      (cond
                        (last (setf last nil) acc)
                        ((funcall predicate element) (setf last t) acc)
                        (t (cons element acc))))
                    list :initial-value '())))

(defun plist-drop (item list &key (test #'eql))
  (plist-drop-if {funcall test item} list))

(defun plist-merge (plist-1 plist-2)
  "Merge arguments into a single plist with unique keys, prefer PLIST-1 items."
  (append plist-1 (plist-drop-if {member _ (plist-keys plist-1)} plist-2)))

(defun counts (list &key (test #'eql) key frac &aux totals)
  "Return an alist keyed by the unique elements of list holding their counts.
Keyword argument FRAC will return fractions instead of raw counts."
  (mapc (lambda (el)
          (if-let (place (assoc el totals :key key :test test))
            (incf (cdr place))
            (push (cons el 1) totals)))
        list)
  (if frac
      (let ((total (reduce #'+ (mapcar #'cdr totals))))
        (mapcar (lambda-bind ((obj . cnt)) (cons obj (/ cnt total))) totals))
      totals))

(defun proportional-pick (list key)
  (let ((raw (reduce (lambda (acc el) (cons (+ el (car acc)) acc))
                     (mapcar key list) :initial-value '(0))))
    (position-if {<= (random (first raw))} (cdr (reverse raw)))))

(defun position-extremum (list predicate key)
  "Returns the position in LIST of the element maximizing KEY."
  (car (extremum (indexed list) predicate :key [key #'second])))

(defun position-extremum-rand (list predicate key)
  "Randomly returns one of position in LIST maximizing KEY."
  (declare (ignorable predicate))
  (warn "`position-extremum-rand' not finished: doesn't use all parameters")
  (let ((scores (mapcar key list)))
    (random-elt (mapcar #'car (remove-if-not [{= (apply #'max scores)} #'second]
                                             (indexed scores))))))

(defun partition (test list)
  "Return a list of lists of elements of LIST which do and do not satisfy TEST.
The first list holds elements of LIST which satisfy TEST, the second
holds those which do not."
  (loop :for x :in list
     :if (funcall test x) :collect x :into yes
     :else :collect x :into no
     :finally (return (list yes no))))

(defun random-bool (&optional bias)
  (> (or bias 0.5) (random 1.0)))

(defun uniform-probability (list)
  (mapcar {cons _ (/ 1.0 (length list))} list))

(defun normalize-probabilities (alist)
  "Normalize ALIST so sum of second elements is equal to 1."
  (let ((total-prob (reduce #'+ (mapcar #'cdr alist))))
    (mapcar (lambda-bind ((key . prob)) (cons key (/ prob total-prob))) alist)))

(defun cumulative-distribution (alist)
  "Cumulative distribution function.
Return an updated version of ALIST in which the cdr of each element is
transformed from an instant to a cumulative probability."
  (nreverse
   (reduce (lambda-bind (acc (value . prob)) (acons value (+ (cdar acc) prob) acc))
           (cdr alist) :initial-value (list (car alist)))))

(defun un-cumulative-distribution (alist)
  "Undo the `cumulative-distribution' function."
  (let ((last 0))
    (mapcar (lambda-bind ((value . prob))
              (prog1 (cons value (- prob last)) (setf last prob)))
            alist)))

(defun random-pick (cdf)
  (car (find-if {<= (random 1.0)} cdf :key #'cdr)))

(defun random-elt-with-decay (orig-list decay-rate)
  (if (null orig-list)
      nil
      (labels ((pick-from (list)
                 (if (null list)
                     (pick-from orig-list)
                     (if (< (random 1.0) decay-rate)
                         (car list)
                         (pick-from (cdr list))))))
        (pick-from orig-list))))

(defun random-subseq (list &optional (size (1+ (if (null list) 0
                                                   (random (length list))))))
  (if (null list)
      nil
      (subseq (shuffle list) 0 size)))

(declaim (inline random-sample-with-replacement))
(defun random-sample-with-replacement
    (range size &aux (result (make-array size :element-type 'fixnum)))
  "Return a random sample of SIZE numbers in RANGE with replacement."
  (declare (optimize speed))
  (declare (type fixnum size))
  (declare (type fixnum range))
  (dotimes (n size (coerce result 'list))
    (setf (aref result n) (random range))))

(declaim (inline random-sample-without-replacement))
(defun random-sample-without-replacement (range size)
  (declare (optimize speed))
  (declare (type fixnum size))
  (declare (type fixnum range))
  "Return a random sample of SIZE numbers in RANGE without replacement."
  (cond
    ((> size range)
     (error "Can't sample ~a numbers from [0,~a] without replacement"
            size range))
    ((= size range)
     (let ((result (make-array size :element-type 'fixnum)))
       (dotimes (n range (coerce result 'list))
         (setf (aref result n) n))))
    (t
     ;; TODO: For faster collection implement a skip-list which
     ;;       increments the value being stored as it passes might be
     ;;       a better data structure.
     (labels ((sorted-insert (list value)
                (declare (type fixnum value))
                (cond
                  ((null list) (cons value nil))
                  ((< value (the fixnum (car list))) (cons value list))
                  (t (cons (car list) (sorted-insert (cdr list) (1+ value)))))))
       (let (sorted)
         (dotimes (n size sorted)
           (setf sorted (sorted-insert sorted (random (- range n))))))))))

(defun find-hashtable-element (hash-tbl n)
  (maphash
   (lambda (k v)
     (declare (ignore v))
     (when (= n 0) (return-from find-hashtable-element k))
     (decf n))
   hash-tbl))

(defun random-hash-table-key (hash-tbl)
  "Return a random key in a hash table"
  (let ((size (hash-table-count hash-tbl)))
    (unless (zerop size)
      (find-hashtable-element hash-tbl (random size)))))

;; From the Common Lisp Cookbook
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (assert (and (stringp string)
               (stringp part)
               (stringp replacement))
          (string part replacement)
          "Arguments to `replace-all' must be strings.")
  (with-output-to-string (out)
    (loop :with part-length := (length part)
       :for old-pos := 0 :then (+ pos part-length)
       :for pos := (search part string
                           :start2 old-pos
                           :test test)
       :do (write-string string out
                         :start old-pos
                         :end (or pos (length string)))
       :when pos :do (write-string replacement out)
       :while pos)))

(defun apply-replacements (list str)
  (if (null list)
      str
      (let ((new-str
             ;; If (caar list) is null then `replace-all' can fall
             ;; into an infinite loop.
             (if (and (caar list) (cdar list))
                 (replace-all str (caar list) (cdar list))
                 str)))
        (apply-replacements (cdr list) new-str))))

;;  Helper function for removing tags identifying DeclRefs
;;  from a code snippet.
(defun peel-bananas (text)
  (apply-replacements '(("(|" . "") ("|)" . "")) text))

(defun unpeel-bananas (text)
  (concatenate 'string "(|" text "|)"))

(defun aget (item list &key (test #'eql))
  "Get KEY from association list LIST."
  (cdr (assoc item list :test test)))

(define-setf-expander aget (item list &key (test ''eql) &environment env)
  (multiple-value-bind (dummies vals stores store-form access-form)
      (get-setf-expansion list env)
    (declare (ignorable stores store-form))
    (let ((store (gensym))
          (cons-sym (gensym)))
      (values dummies
              vals
              `(,store)
              `(let ((,cons-sym (assoc ,item ,access-form :test ,test)))
                 (if ,cons-sym
                     (setf (cdr ,cons-sym) ,store)
                     (prog1 ,store
                       (setf ,access-form (acons ,item ,store ,access-form)))))
              `(aget ,item ,access-form :test ,test)))))

(defun alist-filter (keep-keys alist)
  "Remove all keys from ALIST except those in KEEP-KEYS."
  (remove-if-not [{member _ keep-keys} #'car] alist))

(defun getter (key)
  "Return a function which gets KEY from an association list."
  (lambda (it) (aget key it)))

(defun transpose (matrix)
  "Simple matrix transposition."
  (apply #'map 'list #'list matrix))

(defun interleave (list sep &optional rest)
  (cond
    ((cdr list) (interleave (cdr list) sep (cons sep (cons (car list) rest))))
    (list (reverse (cons (car list) rest)))
    (t nil)))

(defun mapconcat (func list sep)
  (apply #'concatenate 'string (interleave (mapcar func list) sep)))

(defun drop (n seq)
  "Return SEQ less the first N items."
  (if (> n (length seq))
      nil
      (subseq seq (min n (length seq)))))

(defun drop-while (pred seq)
  (if (and (not (null seq)) (funcall pred (car seq)))
      (drop-while pred (cdr seq))
      seq))

(defun drop-until (pred seq)
  (drop-while (complement pred) seq))

(defun take (n seq)
  "Return the first N items of SEQ."
  (subseq seq 0 (min n (length seq))))

(defun take-while (pred seq)
  (if (and (not (null seq)) (funcall pred (car seq)))
      (cons (car seq) (take-while pred (cdr seq)))
      '()))

(defun take-until (pred seq)
  (take-while (complement pred) seq))

(defun pad (list n &optional (elem nil))
  "Pad LIST to a length of N with ELEM"
  (if (>= (length list) n)
      list
      (append list (make-list (- n (length list))
                              :initial-element elem))))

(defun chunks (list size &optional include-remainder-p)
  "Return subsequent chunks of LIST of size SIZE."
  (loop :for i :to (if include-remainder-p
                       (length list)
                       (- (length list) size))
     :by size :collect (subseq list i (min (+ i size) (length list)))))

(defun binary-search (value array &key (low 0)
                                       (high (1- (length array)))
                                       (test (lambda (v)
                                                (cond ((< v value) -1)
                                                      ((> v value) 1)
                                                      (t 0)))))
  "Perform a binary search for VALUE on a sorted ARRAY.
Optional keyword parameters:
LOW:  Lower bound
HIGH: Higher bound
TEST: Test for the binary search algorithm taking on arg.
Return -1 if arg is less than value, 1 if arg is greater than value,
and 0 otherwise."
  (if (< high low)
      nil
      (let ((middle (floor (/ (+ low high) 2))))

        (cond ((< 0 (funcall test (aref array middle)))
               (binary-search value array :low low
                                          :high (1- middle)
                                          :test test))

              ((> 0 (funcall test (aref array middle)))
               (binary-search value array :low (1+ middle)
                                          :high high
                                          :test test))

              (t middle)))))

(defun tails (lst)
  "Return all final segments of the LST, longest first.

For example (tails '(a b c)) => ('(a b c) '(b c) '(c))
"
  (when lst (cons lst (tails (cdr lst)))))

(defun pairs (lst)
  "Return all pairs of elements in LST.

For example (pairs '(a b c)) => ('(a . b) '(a . c) '(b . c))
"
  (iter (for (a . rest) in (tails lst))
        (appending (iter (for b in rest)
                         (collecting (cons a b))))))

(defun make-thread-safe-hash-table (&rest args)
  "Create a thread safe hash table with the given ARGS"
  #+(or sbcl ecl)
  (apply #'make-hash-table :synchronized t args)
  #+ccl
  (apply #'make-hash-table :shared :lock-free args)
  #-(or ccl sbcl ecl)
  (error "unsupported implementation for thread-safe hashtables"))


;;;; Source and binary locations and ranges.
(defclass source-location ()
  ((line :initarg :line :accessor line :type 'fixnum)
   (column :initarg :column :accessor column :type 'fixnum)))

(defclass source-range ()
  ((begin :initarg :begin :accessor begin :type 'source-location)
   (end   :initarg :end   :accessor end   :type 'source-location)))

(defclass range ()
  ((begin :initarg :begin :accessor begin :type 'fixnum)
   (end   :initarg :end   :accessor end   :type 'fixnum)))

(defmethod print-object ((obj source-location) stream)
  (print-unreadable-object (obj stream :type t)
    (prin1 (line obj) stream)
    (format stream ":")
    (prin1 (column obj) stream)))

(defmethod print-object ((obj source-range) stream)
  (flet ((p1-range (range)
           (prin1 (line range) stream)
           (format stream ":")
           (prin1 (column range) stream)))
    (print-unreadable-object (obj stream :type t)
      (p1-range (begin obj))
      (format stream " to ")
      (p1-range (end obj)))))

(defmethod print-object ((obj range) stream)
  (print-unreadable-object (obj stream :type t)
    (prin1 (begin obj) stream)
    (format stream " to ")
    (prin1 (end obj) stream)))

(defmethod source-< ((a source-location) (b source-location))
  (or (< (line a) (line b))
      (and (= (line a) (line b))
           (< (column a) (column b)))))

(defmethod source-<= ((a source-location) (b source-location))
  (or (< (line a) (line b))
      (and (= (line a) (line b))
           (<= (column a) (column b)))))

(defmethod source-> ((a source-location) (b source-location))
  (or (> (line a) (line b))
      (and (= (line a) (line b))
           (> (column a) (column b)))))

(defmethod source->= ((a source-location) (b source-location))
  (or (> (line a) (line b))
      (and (= (line a) (line b))
           (>= (column a) (column b)))))

(defmethod contains ((range source-range) (location source-location))
  (and (source-<= (begin range) location)
       (source->= (end range) location)))

(defmethod contains ((a-range source-range) (b-range source-range))
  (and (source-<= (begin a-range) (begin b-range))
       (source->= (end a-range) (end b-range))))

(defmethod contains ((range range) point)
  (and (<= (begin range) point) (>= (end range) point)))

(defmethod contains((a-range range) (b-range range))
  (and (<= (begin a-range) (begin b-range))
       (>= (end a-range) (end b-range))))

(defmethod intersects ((a-range source-range) (b-range source-range))
  (and (source-< (begin a-range) (end b-range))
       (source-> (end a-range) (begin b-range))))

(defmethod intersects ((a-range range) (b-range range))
  (and (< (begin a-range) (end b-range))
       (> (end a-range) (begin b-range))))


;;;; debugging helpers
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *compile-w/tracing* nil
    "Controls compilation of tracing information with the `traced' macro."))

(defmacro traced ((fn &rest args))
  "Trace wrapped function call when `*compile-w/tracing*' is non-nil.
This is useful for `flet' and `labels' functions which can't be traced
with `cl-user:trace'."
  (if *compile-w/tracing*
      (let ((result-sym (gensym)))
        `(progn (format t "  X: ~S ~S~%" ',fn (list ,@args))
                (let ((,result-sym (,fn ,@args)))
                  (format t ,(format nil "  X: ~a returned~~%      ~~S~~%" fn)
                          ,result-sym)
                  ,result-sym)))
      `(,fn ,@args)))

(defvar *note-level* 0 "Enables execution notes.")
(defvar *note-out* '(t) "Targets of notation.")

(defun replace-stdout-in-note-targets (&optional (targets *note-out*))
  "Replace `t' which is a place holder for `*standard-output*'.
Ideally we would like to set the value of `*note-out*' to a list
holding `*standard-output*', however in compiled binaries the value of
`*standard-output*' changes each time the binary is launched.  So
instead we use `t' as a place-holder, and provide this function for
performing the replacement on the fly when `note' is called.  To
specify a particular value for `*standard-output*' the user may
replace `t' in the `*note-out*' list."
  (mapcar (lambda (s) (if (eq s t) *standard-output* s)) targets))

(defun print-time (&optional (out t))
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignorable day-of-week dst-p tz))
    (format out "~d.~2,'0d.~2,'0d.~2,'0d.~2,'0d.~2,'0d"
            year month date hour minute second)))

(defun note (level &rest format-args)
  (when (>= *note-level* level)
    (let ((*print-pretty* nil))
      (mapcar
       #'finish-output
       (mapc
        {write-sequence
         (concatenate 'string ";;" (print-time nil) ": "
                      (apply #'format nil format-args)
                      (list #\Newline))}
        (replace-stdout-in-note-targets)))))
  ;; Always return nil.
  nil)

#+sbcl
(defun trace-memory ()
  (when (>= *note-level* 2)
    (let ((percentage-used (/ (sb-vm::dynamic-usage)
                              (sb-ext::dynamic-space-size))))
      (if (>= *note-level* 4)
        (note 4 "~a ~,2f~%" (second (sb-debug:list-backtrace))
                            percentage-used)
        (when (>= percentage-used 0.5)
          (note 2 "~a ~,2f~%" (second (sb-debug:list-backtrace))
                              percentage-used))))))

;; adopted from a public domain lisp implementation copied from the

;; scheme implementation given at
;; http://en.wikipedia.org/wiki/Levenshtein_distance
(defun levenshtein-distance (s1 s2 &key (test #'char=) (key #'identity))
  (let* ((width (1+ (length s1)))
         (height (1+ (length s2)))
         (d (make-array (list height width))))
    (dotimes (x width)
      (setf (aref d 0 x) x))
    (dotimes (y height)
      (setf (aref d y 0) y))
    (dotimes (x (length s1))
      (dotimes (y (length s2))
        (setf (aref d (1+ y) (1+ x))
              (min (1+ (aref d y (1+ x)))
                   (1+ (aref d (1+ y) x))
                   (+ (aref d y x)
                      (if (funcall test
                                   (funcall key (aref s1 x))
                                   (funcall key (aref s2 y)))
                          0
                          1))))))
    (aref d (1- height) (1- width))))

;;; Diff computing
(defun diff-scalar (original-seq modified-seq)
  "Return an integer representing the diff size of two sequences
Sum O + |O - M| over each diff region.  O is the length of the
original diff region and M is the length of the modified diff
region."
  (reduce (lambda (acc region)
            (+ acc
               (ecase (type-of region)
                 (common-diff-region 0)
                 (modified-diff-region
                   (+ (original-length region)
                      (abs (- (original-length region)
                              (modified-length region))))))))
          (diff:compute-raw-seq-diff original-seq modified-seq)
          :initial-value 0))

;;; memory mapping, address -> LOC
(defun gdb-disassemble (phenome function)
  "Return the raw gdb disassembled code of FUNCTION in PHENOME."
  (shell "gdb --batch --eval-command=\"disassemble ~s\" ~s 2>/dev/null"
         function phenome))

(defun addrs (phenome function)
  "Return the numerical addresses of the lines (in order) of FUNCTION."
  (remove nil
    (mapcar
     (lambda (line)
       (multiple-value-bind (matchp strings)
           (scan-to-strings "[\\s]*0x([\\S]+)[\\s]*<([\\S]+)>:.*" line)
         (when matchp (parse-integer (aref strings 0) :radix 16))))
     (split-sequence #\Newline (gdb-disassemble phenome function)))))

(defun function-lines (lines)
  "Return the line numbers of the lines (in order) of FUNCTION.
LINES should be the output of the `lines' function on an ASM object."
  (loop :for line :in lines :as counter :from 0
     :for function = (register-groups-bind
                         (line-function) ("^\\$*([^\\.][\\S]+):" line)
                       line-function)
     :collect (or function counter)))

(defun calculate-addr-map (lines phenome genome)
  "Calculate a map of memory address to offsets in LINES.
LINES should be the output of the `lines' function on an ASM object,
PHENOME should be the phenome of an ASM object and GENOME should be
the genome of an ASM object."
  (let ((flines (function-lines lines))
        (genome (coerce genome 'vector))
        (map (make-hash-table)))
    (loop
       :for addrs :in (mapcar (lambda (func) (addrs phenome func))
                              (remove-if-not #'stringp flines))
       :for lines :in (cdr (mapcar
                            {remove-if
                             [{scan "^[\\s]*\\."} {aget :code} {aref genome}]}
                            (split-sequence-if #'stringp flines)))
       :do (mapc (lambda (addr line) (setf (gethash addr map) line))
                 addrs lines))
    map))


;;;; Oprofile functions
(defun samples-from-oprofile-file (path)
  (with-open-file (in path)
    (remove nil
      (iter (for line = (read-line in nil :eof))
            (until (eq line :eof))
            (collect (register-groups-bind (c a)
                         ("^ *(\\d+).+: +([\\dabcdef]+):" line)
                       (cons (parse-integer (or a "") :radix 16)
                             (parse-integer (or c "")))))))))

(defun samples-from-tracer-file (path &aux samples)
  (with-open-file (in path)
    (loop :for line := (read-line in nil)
       :while line
       :do (let ((addr (parse-integer line)))
             (if (assoc addr samples)
                 (setf (cdr (assoc addr samples))
                       (1+ (cdr (assoc addr samples))))
                 (setf samples (cons (cons addr 0) samples)))))
    samples))

(defvar *resolved-header-files* (make-hash-table :test 'equal)
  "A map from function name to a list of headers where
that function may be declared.")

(defun headers-in-manpage (section name)
  (multiple-value-bind (stdout stderr errno)
      (shell
       "man -P cat ~a ~a | sed -n \"/DESCRIPTION/q;p\" | grep \"#include\" | cut -d'<' -f 2 | cut -d'>' -f 1"
       section name)
    (declare (ignorable stderr errno))
    (split-sequence #\Newline stdout :remove-empty-subseqs t)))

(defun resolve-function-includes (func)
  (let ((headers (gethash func *resolved-header-files* 'not-found)))
    (mapcar {format nil "<~a>"}
            (if (eq headers 'not-found)
                (setf (gethash func *resolved-header-files*)
                      (or (headers-in-manpage 3 func)
                          (headers-in-manpage 2 func)))
                headers))))

(defun unlines (lines)
  (format nil "~{~a~^~%~}" lines))

;; Just a little sed-ish thing: find the first line that
;; contains the substring needle, and return the lines
;; after the one that matched.
(defun keep-lines-after-matching (needle haystack)
  (labels ((keep-after (lines)
             (if (null lines)
                 '()
                 (if (search needle (car lines))
                     (cdr lines)
                     (keep-after (cdr lines))))))
    (unlines (keep-after (split-sequence '#\Newline haystack)))))



;;;; Enhanced COPY-SEQ functionality
;;;

(defun sel-copy-array (array)
  (let* ((element-type (array-element-type array))
	 (fill-pointer (and (array-has-fill-pointer-p array)(fill-pointer array)))
	 (adjustable (adjustable-array-p array))
	 (new (make-array (array-dimensions array)
		:element-type element-type
		:adjustable adjustable
		:fill-pointer fill-pointer)))
    (dotimes (i (array-total-size array) new)
      (setf (row-major-aref new i)(row-major-aref array i)))))

(defun enhanced-copy-seq (sequence)
  "Copies any type of array (except :displaced-to) and lists. Otherwise returns NIL."
  (if (arrayp sequence)
      (sel-copy-array sequence)
      (if (listp sequence)
	  (copy-list sequence))))


;;;; Iteration helpers
(defmacro-clause (CONCATENATING expr &optional INTO var INITIAL-VALUE (val ""))
  `(reducing ,expr by {concatenate 'string} into ,var initial-value ,val))


;;;; Profiling

;; Dot implementation from
;; https://techfak.uni-bielefeld.de/~jmoringe/call-graph.html.
(defvar *profile-dot-min-ratio* 1/200
  "Minimum percentage ratio to include a node in the profile dot graph.")

#+sbcl
(defmethod cl-dot:graph-object-node
    ((graph sb-sprof::call-graph) (object sb-sprof::node))
  (flet ((ratio->color (ratio)
           (let ((red   (floor 255))
                 (green (floor (alexandria:lerp ratio 255 0)))
                 (blue  (floor (alexandria:lerp ratio 255 0))))
             (logior (ash red 16) (ash green 8) (ash blue 0)))))
    (let ((ratio (/ (sb-sprof::node-count object)
                    (sb-sprof::call-graph-nsamples graph))))
      (make-instance 'cl-dot:node
        :attributes `(:label ,(format nil "~A\\n~,2,2F %"
                                      (sb-sprof::node-name object) ratio)
                             :shape     :box
                             :style     :filled
                             :fillcolor ,(format nil "#~6,'0X"
                                                 (ratio->color ratio)))))))

#+sbcl
(defmethod cl-dot:graph-object-pointed-to-by
    ((graph sb-sprof::call-graph) (object sb-sprof::node))
  (sb-sprof::node-callers object))

#+sbcl
(defun profile-to-dot-graph (stream)
  "Write profile to STREAM."
  (progn
    (unless sb-sprof::*samples*
      (warn "; `profile-to-dot-graph': No samples to report.")
      (return-from profile-to-dot-graph))
    (let ((call-graph (sb-sprof::make-call-graph most-positive-fixnum)))
      (cl-dot:print-graph
       (cl-dot:generate-graph-from-roots
        call-graph
        (remove-if [{> *profile-dot-min-ratio*}
                    {/ _ (sb-sprof::call-graph-nsamples call-graph)}
                    #'sb-sprof::node-count]
                   (sb-sprof::call-graph-vertices call-graph)))
       :stream stream))))

#-sbcl
(defun profile-to-dot-graph (&rest args)
  (declare (ignorable args))
  (error "`PROFILE-TO-DOT-GRAPH' unimplemented for non-SBCL lisps."))

;; FlameGraph implementation from
;; http://paste.lisp.org/display/326901.
#+sbcl
(defun profile-to-flame-graph (stream)
  "Write FlameGraph profile data to STREAM.
The resulting file may be fed directly to the flamegraph tool as follows.

    REPL> (sb-sprof:start-profiling)

       ...do some work...

    REPL> (with-open-file (out \"profile.data\"
                               :direction :output
                               :if-exists :supersede)
            (profile-to-flame-graph out))

    shell$ cat profile.data|flamegraph > profile.svg

See http://www.brendangregg.com/FlameGraphs/cpuflamegraphs.html."
  (progn
    (unless sb-sprof::*samples*
      (warn "; `profile-to-flame-graph': No samples to report.")
      (return-from profile-to-flame-graph))
    (let ((samples (sb-sprof::samples-vector sb-sprof::*samples*))
          (counts (make-hash-table :test #'equal)))

      (sb-sprof::with-lookup-tables ()
        (loop :for start = 0 :then end
           :while (< start (length samples))
           :for end = (or (position 'sb-sprof::trace-start samples
                                    :start (1+ start))
                          (return))
           :do (let ((key
                      (sb-sprof::with-output-to-string (stream)
                        (loop :for i :from (- end 2) :downto (+ start 2) :by 2
                           :for node = (sb-sprof::lookup-node
                                        (aref samples i))
                           :when node
                           :do (let ((*print-pretty* nil))
                                 (format stream "~A;"
                                         (sb-sprof::node-name node)))))))
                 (incf (gethash key counts 0)))))

      (maphash (lambda (trace count)
                 (format stream "~A ~D~%" trace count))
               counts))))

#-sbcl
(defun profile-to-flame-graph (&rest args)
  (declare (ignorable args))
  (error "`PROFILE-TO-FLAME-GRAPH' unimplemented for non-SBCL lisps."))

(defun starts-with-p (str1 str2)
  "Determine whether `str1` starts with `str2`"
  (let ((p (search str2 str1)))
    (and p (= 0 p))))
 
(defun ends-with-p (str1 str2)
  "Determine whether `str1` ends with `str2`"
  (let ((p (mismatch str2 str1 :from-end T)))
    (or (not p) (= 0 p))))
