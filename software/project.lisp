;;; project.lisp --- evolve multiple source files
(defpackage :software-evolution-library/software/project
  (:nicknames :sel/software/project :sel/sw/project)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :metabang-bind
        :iterate
        :uiop
        :software-evolution-library
        :software-evolution-library/software/simple
        :software-evolution-library/software/source
        :software-evolution-library/components/formatting
        :software-evolution-library/utility)
  (:shadowing-import-from :uiop/run-program :run-program)
  (:shadowing-import-from :uiop :quit)
  (:shadowing-import-from
   :alexandria
   :appendf :ensure-list :featurep :emptyp
   :if-let :ensure-function :ensure-gethash :copy-file
   :parse-body :simple-style-warning)
  (:export :project
           :*build-dir*
           :build-command
           :artifacts
           :evolve-files
           :other-files
           :ignore-paths
           :only-paths
           :ignore-other-paths
           :only-other-paths
           :component-class
           :project-dir
           :ignored-evolve-path-p
           :ignored-other-path-p
           :apply-to-project
           :collect-evolve-files
           :collect-other-files
           :instrumentation-files
           :all-files))
(in-package :software-evolution-library/software/project)
(in-readtable :curry-compose-reader-macros)

(define-software project (software)
    ((build-command :initarg :build-command :accessor build-command
                    :initform nil
                    :documentation "Shell command to build the project.")
     (artifacts :initarg :artifacts :accessor artifacts :initform nil
                :documentation
                "Artifacts (e.g., executables) of the project build.")
     (evolve-files :initarg :evolve-files :accessor evolve-files
                   :initform nil
                   :documentation
                   "Files within the project to mutate.
This holds a list of cons cells of the form (path . software-object-for-path)."
                   :copier copy-files)
     (other-files
      :initarg :other-files :accessor other-files :initform nil
      :documentation
      "Source files which may be used (e.g., instrumented) but not evolved.
This holds a list of cons cells of the form (path . software-object-for-path)."
      :copier copy-files)
     (ignore-paths
      :initarg :ignore-paths
      :reader ignore-paths
      :initform nil
      :documentation
      "List of paths to ignore when collecting evolve-files.
Paths may contain wildcards.")
     (only-paths
      :initarg :only-paths
      :reader only-paths
      :initform nil
      :documentation
      "List of paths to only consider when collecting evolve-files.
Paths may contain wildcards.")
     (ignore-other-paths
      :initarg :ignore-other-paths
      :reader ignore-other-paths
      :initform nil
      :documentation
      "List of paths to ignore when collecting other-files.
Paths may contain wildcards.")
     (only-other-paths
      :initarg :only-other-paths
      :reader only-other-paths
      :initform nil
      :documentation
      "List of paths to only consider when collecting other-files.
Paths may contain wildcards.")
     (component-class
      :initarg :component-class :accessor component-class :initform nil
      :documentation "Software object class to utilize in component objects.")
     (project-dir :initarg :project-dir
                  :accessor project-dir
                  :initform nil
                  :documentation "Source directory containing the project."))
  (:documentation
   "A project is composed of multiple component software objects.
E.g., a multi-file C software project may include multiple clang
software objects in it's `evolve-files'."))



(defvar *build-dir* nil
  "Directory in which to build projects with `phenome'.
When non-nil `phenome' builds projects in this directory instead of
the `project-dir' field of the software object.  Calling `phenome' on
software objects sharing the same project-dir in multiple threads will
lead to conflicts.  This may be avoided by giving each thread its own
build directory.  To do this set *BUILD-DIR* to a different location
in each thread and then initialize *BUILD-DIR* in each thread by
calling `{to-file _ *BUILD_DIR*}' against a base software
object (e.g., the original program).")

(defun ignored-path-p (path &key ignore-paths only-paths
                       &aux (canonical-path (canonical-pathname path)))
  (flet ((included (files)
           (find-if {pathname-match-p canonical-path} files
                    ;; Wildcard paths must also be canonical to match for CCL.
                    :key #'canonical-pathname)))
    (or (and only-paths (not (included only-paths)))
        (included ignore-paths))))

(defgeneric ignored-evolve-path-p (software path)
  (:documentation "Check if PATH is an ignored evolve path in SOFTWARE.")
  (:method ((obj project) path &aux (canonical-path
                                     (if (project-dir obj)
                                         (canonical-pathname
                                          (pathname-relativize
                                           (project-dir obj) path))
                                         (canonical-pathname path))))
    (ignored-path-p canonical-path
                    :ignore-paths (ignore-paths obj)
                    :only-paths (only-paths obj))))

(defgeneric ignored-other-path-p (software path)
  (:documentation "Check if PATH is an ignored other path in SOFTWARE.")
  (:method ((obj project) path &aux (canonical-path
                                     (if (project-dir obj)
                                         (canonical-pathname
                                          (pathname-relativize
                                           (project-dir obj) path))
                                         (canonical-pathname path))))
    (ignored-path-p canonical-path
                    :ignore-paths (ignore-other-paths obj)
                    :only-paths (only-other-paths obj))))

(defun copy-files (files)
  "Copier for `evolve-files' and `other-files' on `project' software objects."
  (loop for (p . c) in files
     collecting (cons p (copy c))))

(defmethod all-files ((obj project))
  "Returns concatenation of `evolve-files' and `other-files'."
  (append (evolve-files obj) (other-files obj)))

(defmethod genome ((obj project))
  "Returns all genomes joined with separators."
  (format nil "~{~a~%~}"
          (loop for (f . c) in (all-files obj)
             collect "=============================="
             collect f
             collect "=============================="
             collect (genome c))))

(defmethod (setf genome) (text (project project))
  (declare (ignore text project))
  (error "Can only set the genome of component files of a project."))

(defgeneric collect-evolve-files (project)
  (:documentation "Create the evolve files for PROJECT."))

(defgeneric collect-other-files (project)
  (:documentation
   "Find parseable files in PROJECT that were not included in `evolve-files'.
Assumes `evolve-files' has been initialized.  Only applies to
non-symlink text files that don't end in \"~\" and are not ignored by
`ignore-other-paths', or `only-other-paths'.")
  (:method (obj) (declare (ignorable obj)) nil)
  (:method ((project project))
    ;; Create software objects for these other files.
    (nest
     (flet ((text-file-p (p) (eql 'text (car (file-mime-type p))))
            (ends-in-tilde (s)
              (let ((len (length s)))
                (and (> len 0) (eql (elt s (1- len)) #\~))))
            (pathname-has-symlink (p)
              (not (equal p (resolve-symlinks p))))))
     ;; These are represented as simple text files, for line-oriented diffs.
     (mapcar «cons #'identity
                   [{from-file (make-instance 'simple)}
                    {merge-pathnames-as-file (project-dir project)}]»)
     ;; Evolve files may have come from the build dir, but that
     ;; doesn't matter here as we are comparing the relativized paths.
     (remove-if {member _ (mapcar #'car (evolve-files project)) :test #'equal})
     (mapcar {pathname-relativize (project-dir project)})
     (remove-if
      (lambda (p) (or (null (pathname-name p))
		 (not (file-exists-p p))
		 (ends-in-tilde (pathname-name p))
		 (ends-in-tilde (pathname-type p))
		 (ignored-other-path-p project p)
		 ;; For now do not include symlinks.  In the future,
		 ;; make links be special objects.
		 (pathname-has-symlink p)
                 (not (text-file-p p)))))
     (uiop:directory*)
     (merge-pathnames-as-file (project-dir project) #p"**/*.*"))))

(defmethod from-file ((obj project) path)
  (assert (probe-file path) (path) "~a does not exist." path)
  (setf (project-dir obj) (canonical-pathname (truename path))
        (evolve-files obj) (collect-evolve-files obj)
        (other-files obj) (collect-other-files obj))
  obj)

(defmethod to-file ((project project) path)
  (assert path (path) "Destination path may not be nil")

  ;; Ensure path is a canonical directory path.
  (setf path (canonical-pathname (ensure-directory-pathname path)))

  ;; Verify parent directory exists, otherwise the copy will fail.
  (ensure-directories-exist (pathname-parent-directory-pathname path))

  ;; Copy the project directory to the output path.
  (when (and (project-dir project)
             (probe-file (project-dir project))
             (not (equalp path (canonical-pathname (project-dir project)))))
    (multiple-value-bind (stdout stderr errno)
        (if (probe-file path) ; Different copy if directory already exists.
            (shell "cp -pr ~a/* ~a/" (project-dir project) path)
            (shell "cp -pr ~a ~a" (project-dir project) path))
      (declare (ignorable stdout))
      (assert (zerop errno) (path)
              "Creation of output directory failed with: ~a" stderr)))

  ;; Write the software objects.
  (handler-bind ((file-access
                  (lambda (c)
                    (warn "Changing permission from ~a to ~a"
                          (file-access-operation c) (file-access-path c))
                    (invoke-restart 'set-file-writable))))
    (loop for (file . obj) in (all-files project)
       do (to-file obj (in-directory path file)))))

(defmethod size ((obj project))
  "Return summed size across all `evolve-files'."
  (reduce #'+ (mapcar [#'size #'cdr] (evolve-files obj))))

(defun pick-file (obj)
  "Randomly pick one evolved file. Return its index in the alist."
  (proportional-pick (evolve-files obj) (lambda (x) (max 1 (size (cdr x))))))

(defmethod mutate ((obj project))
  "Randomly pick one file to mutate."

  ;; Note: this is normally done in apply-mutation, but that will
  ;; never be called for project objects.
  (setf (fitness obj) nil)

  (bind ((which (pick-file obj))
         ((file . sub-obj) (nth which (evolve-files obj)))
         ((:values _ mutation) (mutate sub-obj))
         ;; Add filename to mutation for better stats
         (result (cons file (cons (class-name (class-of mutation))
                                  (targets mutation)))))
    (values obj result)))

;; This isn't used in normal operation (because mutate just dispatches
;; to the individual files), but it's handy for debugging.
(defmethod apply-mutation ((obj project) op)
  (destructuring-bind (file . mutation) op
    (apply-mutation (aget file (evolve-files obj) :test #'equal)
                    mutation)))

(defmethod apply-mutations ((project project) (mut mutation) n)
  (labels ((apply-mutations-single-file (evolve-file mut n)
             (setf (slot-value mut 'object) (cdr evolve-file))
             (setf (slot-value mut 'targets) nil)
             (iter (for targeted in (mapcar {at-targets mut}
                                            (targets mut)))
                   (for i below n)
                   (collect targeted into mutations)
                   (collect (->> (apply-mutation (copy (cdr evolve-file))
                                                 targeted)
                                 (incorporate (copy project)
                                              (car evolve-file)))
                            into results)
                   (finally (return (values results mutations)))))
           (incorporate (project src-file obj)
             (setf (aget src-file (evolve-files project) :test #'equal) obj)
             project))
    (iter (for evolve-file in (evolve-files project))
          (while (< (length results) n))
          (multiple-value-bind (single-file-results single-file-mutations)
              (apply-mutations-single-file evolve-file
                                           mut
                                           (- n (length results)))
            (appending single-file-results into results)
            (appending single-file-mutations into mutations))
          (finally (return (values results mutations))))))

(defmethod apply-picked-mutations ((project project) (mut mutation) n)
  (labels ((apply-mutation-single-file (evolve-file mut)
             (setf (slot-value mut 'object) (cdr evolve-file))
             (setf (slot-value mut 'targets) nil)
             (when-let* ((picked (funcall (picker mut) (cdr evolve-file)))
                         (targeted (at-targets mut picked)))
               (values (->> (apply-mutation (copy (cdr evolve-file))
                                            targeted)
                            (incorporate (copy project)
                                         (car evolve-file)))
                       targeted)))
           (incorporate (project src-file obj)
             (setf (aget src-file (evolve-files project) :test #'equal) obj)
             project))
    (iter (for i upfrom 0)
          (while (and (< (length results) n)
                      (< i (* n (ceiling (/ (size project) 1000))))))
          (bind ((evolve-file (nth (pick-file project) (evolve-files project)))
                 ((:values result mutation)
                  (apply-mutation-single-file evolve-file mut)))
            (while (and result mutation))
            (collect result into results)
            (collect mutation into mutations))
          (finally (return (values results mutations))))))

(defmethod crossover ((a project) (b project))
  "Randomly pick a file in A and crossover with the corresponding file in B."
  (bind ((which (pick-file a))
         (file (car (nth which (evolve-files a))))
         ((:values crossed point-a point-b)
          (crossover (cdr (nth which (evolve-files a)))
                     (cdr (nth which (evolve-files b)))))
         (new (copy a)))
    (setf (cdr (nth which (evolve-files new))) crossed)
    ;; Add filenames to crossover points for better stats
    (values new (cons point-a file) (cons point-b file))))

(defmethod format-genome ((project project) &key)
  "Apply a code formatting tool to each file in PROJECT."
  (apply-to-project project #'format-genome))

(defmethod apply-to-project ((project project) function)
  "Mapcar FUNCTION over `all-files' of PROJECT."
  (values project (mapcar [function #'cdr] (all-files project))))

(defmethod phenome :around
    ((obj project) &key
                     (bin (temp-file-name))
                     (build-dir (or *build-dir* (project-dir obj))))
  (let ((keep-file-p (and build-dir (probe-file build-dir))))
    (unless keep-file-p
      (warn "No valid build-dir or project-dir specified for project ~S. Using temp directory."
            obj)
      (setf build-dir (temp-file-name)))
    ;; Ensure source and required artifacts are present in build-dir.
    (to-file obj build-dir)
    ;; Using `call-next-method' with arguments to ensure build-dir has
    ;; the same value in the main `phenome' method.
    (unwind-protect (call-next-method obj :bin bin :build-dir build-dir)
      (unless keep-file-p (sel/utility::ensure-temp-file-free build-dir)))))

(defmethod phenome
    ((obj project) &key
                     (bin (temp-file-name))
                     (build-dir (or *build-dir* (project-dir obj))))
  "Build the software project OBJ and copy build artifact(s) to BIN."
  (multiple-value-bind (stdout stderr exit)
      (shell "cd ~a && ~a" build-dir (build-command obj))
    (restart-case
        (if (zerop exit)
            (progn
              (when (> (length (artifacts obj)) 1)
                (shell "mkdir ~a" bin))
              ;; Copy artifacts to BIN.
              (iter (for artifact in (artifacts obj))
                    (shell "cp -r ~a ~a"
                           (namestring (in-directory build-dir artifact)) bin)))
            (error (make-condition 'phenome
                     :text stderr :obj obj :loc build-dir)))
      (retry-project-build ()
        :report "Retry `phenome' on OBJ."
        (phenome obj :bin bin))
      (return-nil-for-bin ()
        :report "Allow failure returning NIL for bin."
        (setf bin nil)))
    (when bin (assert (probe-file bin) (bin) "BIN not created!"))
    (values bin exit stderr stdout
            (mapcar [{in-directory build-dir} #'first] (evolve-files obj)))))
