;;; java-project.lisp --- Java
;;;
;;; Specialization for building a software object from a java project
;;;
;;; Implements the core functionality of the software-evolution-library
;;; for class files written in java. The java versions that are supported
;;; relies heavily on an input's success of parsing individual files with
;;; @uref{http://javaparser.org/, Java Parser}.
;;;
;;; The project will use jar files to extract which files need to be
;;; initialized in this context. A java-project will contain relative
;;; path names and the corresponding software object, stored in
;;; evolve-files. Naturally, the java software object is used in this
;;; implementation.
;;;
;;; The core functionality is supported by the command line tool
;;; @uref{https://github.com/GrammaTech/java-mutator, java-mutator}.
;;;
;;; @texi{java-project}
(defpackage :software-evolution-library/software/java-project
  (:nicknames :sel/software/java-project :sel/sw/java-project)
  (:use :gt/full
        :software-evolution-library
        :software-evolution-library/software/java
        :software-evolution-library/software/project)
  (:import-from :uiop/run-program :run-program)
  (:export :java-project
           :get-files-jar))
(in-package :software-evolution-library/software/java-project)
(in-readtable :curry-compose-reader-macros)

(define-software java-project (project) ()
  (:documentation "Project specialization for java software objects."))

(defmethod initialize-instance :after ((java-project java-project) &key)
  (setf (component-class java-project)
        (or (component-class java-project) 'java)))

(defmethod from-file :before ((obj java-project) project-dir)
  (setf (project-dir obj) project-dir)
  (multiple-value-bind (bin exit-code stdout stderr files)
      (phenome obj)
    (declare (ignorable bin files))
    (if (not (zerop exit-code))
        (error "Failed to build java project for project.~%~
                  build-command: ~a~%~
                  stdout: ~a~%~
                  stderr: ~a~%"
               (build-command obj)
               stdout stderr))))

(defmethod collect-evolve-files ((obj java-project))
  (iter (for entry in
             (nest (mapcar
                    (lambda (file)
                      (replace-all
                       file
                       (namestring
                        (ensure-directory-pathname (project-dir obj)))
                       "")))
                   (get-applicable-project-files (project-dir obj))
                   (merge-pathnames-as-file (project-dir obj))
                   ;; FIXME: The following artificially
                   ;; limits Java projects to a single
                   ;; build artifact and should be
                   ;; generalized as in
                   ;; clang-project.lisp.
                   (first (artifacts obj))))
        (unless (ignored-evolve-path-p obj entry)
          (handler-case
              (let ((java-obj (from-file
                               (make-instance (component-class obj))
                               (merge-pathnames-as-file
                                (project-dir obj)
                                entry))))
                (if (not (zerop (size java-obj)))
                    (collect (cons entry java-obj))
                    (warn "Ignoring file ~a with 0 statements" entry)))
            (mutate (e)
              (declare (ignorable e))
              (warn "Ignoring file ~a, failed to initialize" entry))))))

(defun get-filename (path)
  "Return filename of a path"
  (pathname-name (pathname path)))

(defun extract-filename-list (pathList)
  "Return a list of filenames for a list of full paths"
  (mapcar #'get-filename pathList))

(defun extract-jars-in-jar (folder jar-name)
  "Extracts jars within the jar passed to the function
and returns the list of jars extracted for the next
extraction iteration"
  (let ((jar-files (run-command-split-newline
                     (format nil "jar tf ~a/~a | grep -o '[^/]*.jar$'"
                             folder
                             jar-name))))
    (when jar-files
      (shell "unzip -jo ~a/~a '*\.jar' -d ~a" folder jar-name folder))
    jar-files))

(defun get-files-project-folder (project-path)
  "Returns a list of all files with ext java"
  (multiple-value-bind (stdout stderr errno)
      (shell "find ~a -type f -name '*\.java'" project-path)
    (declare (ignorable stderr errno))
    (unless (emptyp (trim-whitespace stdout))
      (split-sequence #\Newline
        (string-trim '(#\Newline) stdout)))))

(defun get-files-with-ext-in-dir (folder ext)
  "Returns a list of file paths with the ext file extension"
  (run-command-split-newline
    (format nil "find ~a -type f -name '*\.~a'" folder ext)))

(defun run-command-split-newline (command)
  "Executes a command and splits output by newline"
  (let ((stdout-str (make-array '(0)
                                :element-type
                                #+sbcl 'extended-char
                                #-sbcl 'character
                                :fill-pointer 0 :adjustable t)))
    (with-output-to-string (stdout stdout-str)
        (run-program command
                     :force-shell t
                     :ignore-error-status t
                     :output stdout)
      (unless (emptyp (trim-whitespace stdout-str))
        (split-sequence #\Newline (string-trim '(#\Newline) stdout-str))))))

(defun get-files-jar (jar-path)
  "Returns a list of class files in a jar or directory.
Jars within jars are recursivly extracted
to the depth of 3 within the built jar"
  (with-temp-dir (sandbox)
    (let* ((jar-paths
            (if (directory-exists-p jar-path)
                (get-files-with-ext-in-dir jar-path "jar")
                (list jar-path)))
           (paths (iter (for path in jar-paths)
                        (collect
                            (format nil "~a.~a"
                                    (pathname-name (pathname path))
                                    (pathname-type (pathname path)))))))
      (shell "cp ~{~a~^ ~} ~a" jar-paths sandbox)
      (iter (for i from 1 to 4)
            (setf paths
                  (iter (for path in paths)
                        (appending (extract-jars-in-jar sandbox path)))))

      (iter (for jar-file in (get-files-with-ext-in-dir sandbox "jar"))
            (appending
             (extract-filename-list
              (run-command-split-newline
               (format nil
                       "jar tf ~a | grep -o '[^/]*.class$'"
                       jar-file))))))))

(defun compare-file-lists (jar-files project-files)
  "Compare the lists, returns full path list if filename is in both lists"
  (iter (for path in project-files)
        (when (find (get-filename path) jar-files :test #'equal)
          (collect path))))

(defun get-applicable-project-files (project-path jar-path)
  "Get list of files that are both in project folder and in jar"
  (compare-file-lists (get-files-jar jar-path)
                      (get-files-project-folder project-path)))
