;;; Specialization for building a software object from a java project
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(define-software java-project (project)
  ((project-dir :initarg :project-dir
                :accessor project-dir
                :initform nil
                :documentation "Source directory containing the project")
   (java-class  :initarg :java-class
                :accessor java-class
                :initform 'java
                :documentation "Java subclass to utilize in the project")))

(defmethod from-file ((obj java-project) project-dir)
  "Java-project from-file builds the project, extracts relevant
java files in project and creates java software object for
each and stores them in a list."
  (setf (slot-value obj 'project-dir) project-dir)
  (with-temp-build-dir (project-dir)
    (multiple-value-bind (stdout stderr exit-code)
        (shell "cd ~a && ~a ~a"
               *build-dir* (build-command obj) (build-target obj))
      (if (not (zerop exit-code))
          (error "Failed to build java project for project.~%~
                  build-command: ~a ~a~%~
                  stdout: ~a~%~
                  stderr: ~a~%"
                  (build-command obj)
                  (build-target obj)
                  stdout stderr)

          (let* ((jar-path (merge-pathnames-as-file *build-dir*
                                                    (build-target obj)))
                 (files (get-applicable-project-files jar-path *build-dir*)))
            (setf (slot-value obj 'evolve-files)
                  (iter (for entry in files)
                        (collect
                          (cons (replace-all entry
                                  (-> (if *build-dir*
                                          *build-dir*
                                          (project-dir obj))
                                      (cl-fad:pathname-as-directory)
                                      (namestring))
                                  "")
                                (from-file (make-instance (java-class obj))
                                           entry)))))))))
  obj)

(defmethod to-file ((java-project java-project) path)
  (let ((*build-dir* (make-build-dir (project-dir java-project) :path path)))
    (write-genome-to-files java-project)))

(defun get-filename (path)
  "Return filename of a path"
  (pathname-name (pathname path)))

(defun extract-filename-list (pathList)
  "Return a list of filenames for a list of full paths"
  (mapcar #'get-filename pathList))

(defun get-files-project-folder (project-path)
  "Returns a list of all files with ext java"
  (let ((stdout-str (make-array '(0)
                                 :element-type
                                 #+sbcl 'extended-char
                                 #-sbcl 'character
                                 :fill-pointer 0 :adjustable t)))
    (with-output-to-string (stdout stdout-str)
      (run-program (format nil "find ~a -type f -name '*\.java'" project-path)
                   :force-shell t
                   :ignore-error-status t
                   :output stdout)
      (split-sequence #\Newline (string-trim '(#\Newline) stdout-str)))))

(defun get-files-jar (jar-path)
  "Returns a list of class files in a jar"
  (let ((stdout-str (make-array '(0)
                                 :element-type
                                 #+sbcl 'extended-char
                                 #-sbcl 'character
                                 :fill-pointer 0 :adjustable t)))
    (with-output-to-string (stdout stdout-str)
      (run-program (format nil "jar tf ~a | grep '.*\.class$'" jar-path)
                   :force-shell t
                   :ignore-error-status t
                   :output stdout)
      (split-sequence #\Newline (string-trim '(#\Newline) stdout-str)))))

(defun compare-file-lists (jar-files project-files)
  "Compare the lists, returns full path list if filename is in both lists"
  ;; project-files much smaller than jar-files, looping over project-files
  (loop for path in project-files
    when (find (get-filename path) jar-files :test #'equal)
      collect path))

(defun get-applicable-project-files (jar-path project-path)
  "Get list of files that are both in project folder and in jar"
  (compare-file-lists (extract-filename-list (get-files-jar jar-path))
                      (get-files-project-folder project-path)))

(defvar *java-project-execution-script-template*
  "#!/bin/bash
echo \"$@\"
java -jar ~a $@"
  "Template for the java execution script to run class files")

(defmethod phenome ((obj java-project) &key (bin (temp-file-name)))
  (write-genome-to-files obj)

  ;; Build the object
  (multiple-value-bind (stdout stderr exit)
      (shell "cd ~a && ~a ~a" *build-dir*
             (build-command obj) (build-target obj))
    (restart-case
        (if (zerop exit)
            (with-temp-file-of (script-file) *java-execution-script-template*
              (shell "cat ~a ~a > ~a && chmod +x ~a"
                     script-file
                     (merge-pathnames-as-file *build-dir*
                                              (build-target obj))
                     bin bin))
            (error (make-condition 'phenome
                     :text stderr :obj obj :loc *build-dir*)))
      (retry-project-build ()
        :report "Retry `phenome' on OBJ."
        (phenome obj :bin bin))
      (return-nil-for-bin ()
        :report "Allow failure returning NIL for bin."
        (setf bin nil)))
    (values bin exit stderr stdout
            (mapcar [#'full-path #'first] (evolve-files obj)))))

(defmethod flags ((obj java-project))
  (declare (ignorable obj))
  nil)

(defmethod clang-format ((java-project java-project) &optional style)
  (declare (ignorable style))
  java-project)

(defmethod indent ((java-project java-project) &optional style)
  (declare (ignorable style))
  java-project)

(defmethod clang-tidy ((java-project java-project))
  java-project)

