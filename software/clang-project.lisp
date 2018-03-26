#||
*******************************************************************************
*
* This software is furnished under a license and/or other restrictive terms and may
* be used and copied only in accordance with such terms and the inclusion of the
* below copyright notice. This software or any other copies thereof may not be
* provided or otherwise made available to any other person without the express
* written consent of an authorized reprsentative of GrammaTech, Inc. Title to,
* ownership of, and all rights in the software is retained by GrammaTech, Inc.
*
* Any reproduction of the software or portions thereof marked with this legend
* must also reproduce the following markings.
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* Distribution Statement B: Distribution authorized to U.S. Government agencies only;
* Export Controlled and Proprietary Information: DFARS SBIR Data Rights, 09/15/2017.
* Other requests for this document shall be referred to the DoD Controlling
* Office or the DoD SBIR Program Office.
*
* SBIR Data Rights
*
* Contract Number: D17PC00096
* Contractor Name: GrammaTech, Inc.
* Contractor Address: 531 Esty Street, Ithaca, NY  14850
* Expiration Date: April 30, 2024
*
* The Governmentís rights to use, modify, reproduce, release, perform, display, or disclose
* technical data or computer software contained in this report are restricted by paragraph
* (b)(4) of the Rights in Noncommercial Technical Data and Computer Software - Small Business
* Innovation Research (SBIR) Program clause (DFARS 252.227-7018 (FEB 2014)) contained in the
* above identified contract. No restrictions apply after the expiration date shown above. Any
* reproduction of technical data, computer software, or portions thereof marked with this
* legend must also reproduce the markings.
*
* (c) GrammaTech, Inc. 2018.  All other rights reserved.
*
*******************************************************************************
||#
;; Specialization for building a project from a clang compilation database
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(define-software clang-project (project)
  ((project-dir :initarg :project-dir
                :accessor project-dir
                :initform nil
                :documentation "Source directory containing the project")
   (compilation-database :initarg :compilation-database
                         :accessor compilation-database
                         :initform nil
                         :documentation "Compilation database for the project")
   (clang-class :initarg :clang-class
                :accessor clang-class
                :initform 'clang
                :documentation "Clang subclass to utilize in the project"))
  (:documentation "Project specialization for clang software objects."))

(defmethod from-file ((clang-project clang-project) project-dir)
  "Populate CLANG-PROJECT from the source code in PROJECT-DIR.
* CLANG-PROJECT to be populated from source in PROJECT-DIR
* PROJECT-DIR source code to populate CLANG-PROJECT with
"
  (assert (or (compilation-database clang-project) (which "bear"))
          (clang-project)
          "Calling `from-file` on a clang-project requires a compilation ~
           database or 'bear' installation on your PATH")
  (labels ((create-compilation-database (clang-project)
             (multiple-value-bind (stdout stderr errno)
                 (shell "cd ~a && bear ~a ~a"
                        *build-dir*
                        (build-command clang-project)
                        (build-target clang-project))
               (declare (ignorable errno))
               (or (and (probe-file (make-pathname
                                      :directory (directory-namestring
                                                  *build-dir*)
                                      :name "compile_commands.json"))
                        (with-open-file (in (make-pathname
                                              :directory (directory-namestring
                                                          *build-dir*)
                                              :name "compile_commands.json"))
                          (remove-duplicates (json:decode-json-from-source in)
                            :test #'equalp
                            :key (lambda (entry)
                                   (merge-pathnames-as-file
                                     (pathname-as-directory
                                       (aget :directory entry))
                                     (pathname-as-file
                                       (aget :file entry))))
                            :from-end t)))
                   (error "Failed to create compilation database for project.~%~
                           build command: ~a ~a~%~
                           stdout: ~a~%~
                           stderr: ~a~%"
                           (build-command clang-project)
                           (build-target clang-project)
                           stdout stderr))))
           (relativize (clang-project path)
             (replace-all (-> (if (-> (pathname-as-directory path)
                                      (directory-exists-p))
                                  (pathname-as-directory path)
                                  path)
                              (canonical-pathname)
                              (namestring))
                          (-> (project-dir clang-project)
                              (pathname-as-directory)
                              (canonical-pathname)
                              (namestring))
                          ""))
           (get-project-path (clang-project path)
             (replace-all (-> (canonical-pathname path)
                              (namestring))
                          (-> (if (and *build-dir*
                                       (search (-> *build-dir*
                                                   (namestring))
                                               (-> path
                                                   (canonical-pathname)
                                                   (pathname-as-directory)
                                                   (namestring))))
                                  *build-dir*
                                  (project-dir clang-project))
                              (canonical-pathname)
                              (pathname-as-directory)
                              (namestring))
                          (-> (project-dir clang-project)
                              (canonical-pathname)
                              (pathname-as-directory)
                              (namestring))))
           (create-evolve-files (clang-project)
             (iter (for entry in (compilation-database clang-project))
                   (collect
                     (let ((file-path
                             (->> (merge-pathnames-as-file
                                    (pathname-as-directory
                                      (aget :directory entry))
                                    (pathname-as-file
                                      (aget :file entry)))
                                  (get-project-path clang-project))))
                       (cons (relativize clang-project file-path)
                             (-> (make-instance (clang-class clang-project)
                                                :compiler (get-compiler entry)
                                                :flags (get-flags entry))
                                 (from-file file-path)))))))
           (get-compiler (entry)
             (->> (aget :command entry)
                  (split-sequence #\Space)
                  (first)))
           (get-flags (entry)
             (let ((flags (-<>> (or (aget :command entry) "")
                                (replace-all <> "-I" "-I ")
                                (split-sequence #\Space)
                                (remove-if {string= ""})
                                (cdr))))
               (->> (iter (for f in flags)
                          (for p previous f)
                          (collect
                            (if (string= p "-I")
                                (if (starts-with-subseq "/" f)
                                    (->> (pathname-as-directory f)
                                         (get-project-path clang-project))
                                    (->> (merge-pathnames-as-directory
                                           (->> (aget :directory entry)
                                                (make-pathname :directory))
                                           (make-pathname :directory f))
                                         (pathname-as-directory)
                                         (get-project-path clang-project)))
                                f)))
                    (remove-if {string= (aget :file entry)})
                    (append (list "-I"
                                  (namestring (project-dir clang-project))))
                    (append (list "-I"
                                  (->> (merge-pathnames-as-directory
                                         (pathname-as-directory
                                           (aget :directory entry))
                                         (pathname-as-file
                                           (aget :file entry)))
                                       (pathname-as-directory)
                                       (get-project-path clang-project))))))))
    (setf (project-dir clang-project)
          (-> (truename project-dir)
              (canonical-pathname)))
    (cond ((all-files clang-project)
           ;; Reload existing files.
           (iter (for (src-file . obj) in (all-files clang-project))
                 (from-file obj (in-directory (project-dir clang-project)
                                              src-file))))
          ((compilation-database clang-project)
           ;; Load from compilation database.
           (setf (evolve-files clang-project)
                 (create-evolve-files clang-project)))
          (t ;; Create compilation database.
           (with-temp-build-dir (project-dir)
             (setf (compilation-database clang-project)
                   (create-compilation-database clang-project))
             (setf (evolve-files clang-project)
                   (create-evolve-files clang-project)))))
    clang-project))

(defmethod to-file ((clang-project clang-project) path)
  "Write CLANG-PROJECT to the path directory.
* CLANG-PROJECT project to output
* PATH directory to write the project to
"
  (let ((*build-dir* (make-build-dir (project-dir clang-project) :path path)))
    (write-genome-to-files clang-project)))

(defmethod asts ((obj clang-project))
  "Return a list of all ASTs in the project OBJ."
  (if (current-file obj)
      (asts (current-file obj))
      (apply #'append (mapcar [#'asts #'cdr] (evolve-files obj)))))

(defmethod clang-format ((clang-project clang-project) &optional style)
  "Apply `clang-format' to CLANG-PROJECT.
* CLANG-PROJECT project to format and return
* STYLE `clang-format' style to utilize
"
  (apply-to-project clang-project {clang-format _ style})
  clang-project)

(defmethod clang-tidy ((clang-project clang-project))
  "Apply `clang-tidy' to CLANG-PROJECT.
* CLANG-PROJECT project to tidy and return
"
  (apply-to-project clang-project #'clang-tidy)
  clang-project)

(defmethod astyle ((clang-project clang-project) &optional style)
  "Apply Artistic Style to CLANG-PROJECT.
* CLANG-PROJECT project to format and return
* STYLE style to utilize
"
  (apply-to-project clang-project {astyle _ style}))

#||
*******************************************************************************
*
* This software is furnished under a license and/or other restrictive terms and may
* be used and copied only in accordance with such terms and the inclusion of the
* below copyright notice. This software or any other copies thereof may not be
* provided or otherwise made available to any other person without the express
* written consent of an authorized reprsentative of GrammaTech, Inc. Title to,
* ownership of, and all rights in the software is retained by GrammaTech, Inc.
*
* Any reproduction of the software or portions thereof marked with this legend
* must also reproduce the following markings.
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* Distribution Statement B: Distribution authorized to U.S. Government agencies only;
* Export Controlled and Proprietary Information: DFARS SBIR Data Rights, 09/15/2017.
* Other requests for this document shall be referred to the DoD Controlling
* Office or the DoD SBIR Program Office.
*
* SBIR Data Rights
*
* Contract Number: D17PC00096
* Contractor Name: GrammaTech, Inc.
* Contractor Address: 531 Esty Street, Ithaca, NY  14850
* Expiration Date: April 30, 2024
*
* The Governmentís rights to use, modify, reproduce, release, perform, display, or disclose
* technical data or computer software contained in this report are restricted by paragraph
* (b)(4) of the Rights in Noncommercial Technical Data and Computer Software - Small Business
* Innovation Research (SBIR) Program clause (DFARS 252.227-7018 (FEB 2014)) contained in the
* above identified contract. No restrictions apply after the expiration date shown above. Any
* reproduction of technical data, computer software, or portions thereof marked with this
* legend must also reproduce the markings.
*
* (c) GrammaTech, Inc. 2018.  All other rights reserved.
*
*******************************************************************************
||#
