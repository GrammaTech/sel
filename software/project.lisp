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
        :software-evolution-library/components/formatting
	:software-evolution-library/ast-diff/alist
        :software-evolution-library/utility)
  (:shadowing-import-from :uiop/run-program :run-program)
  (:shadowing-import-from :uiop :quit)
  (:shadowing-import-from
   :alexandria
   :appendf :ensure-list :featurep :emptyp
   :if-let :ensure-function :ensure-gethash :copy-file
   :parse-body :simple-style-warning)
  (:export :project
           :apply-to-project
           :build-command
           :artifacts
           :evolve-files
           :other-files
           :project-dir
           :instrumentation-files
           :all-files
           :write-genome-to-files
           :with-build-dir
           :with-temp-build-dir
           :make-build-dir
           :full-path
           :*build-dir*))
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
     (project-dir :initarg :project-dir
                  :accessor project-dir
                  :initform nil
                  :documentation "Source directory containing the project."))
  (:documentation
   "A project is composed of multiple component software objects.
E.g., a multi-file C software project may include multiple clang
software objects in it's `evolve-files'."))

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

(defgeneric write-genome-to-files (obj)
  (:documentation "Overwrite evolved files with current genome."))

(defmethod write-genome-to-files ((obj project))
  (handler-bind ((file-access
                  (lambda (c)
                    (warn "Changing permission to ~a to ~a"
                          (file-access-operation c) (file-access-path c))
                    (invoke-restart 'set-file-writable))))
    (loop for (path . c) in (all-files obj)
       do (string-to-file (genome c) (full-path path)))))

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
  "DOCFIXME"
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
  "DOCFIXME"
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
  "Randomly pick one file in a and perform crossover with the corresponding file in b."

  (bind ((which (pick-file a))
         (file (car (nth which (evolve-files a))))
         ((:values crossed point-a point-b) (crossover (cdr (nth which (evolve-files a)))
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


;;;; Diffs on projects

(defun confirm-files-are-same (alist1 alist2)
  (let ((files1 (mapcar #'car alist1))
	(files2 (mapcar #'car alist2)))
    (unless (equal files1 files2)
      (error "Two file alists do not reference the same files: ~A, ~A" files1 files2))))

(defun sort-file-alist (alist)
  (sort (copy-list alist) #'string< :key (lambda (x) (string (car x)))))

#|
(defmethod ast-diff ((project1 project) (project2 project))
  (let ((files1 (all-files project1))
	(files2 (all-files project2))
	(ntab (make-hash-table :test #'equal)))
    (setf files1 (sort-file-alist files1))
    (setf files2 (sort-file-alist files2))
    (setf files1 (remove-files-not-in files1 files2))
    (setf files2 (remove-files-not-in files2 files1))
    (confirm-files-are-same files1 files2)
    (iter (for (name . file1) in files1)
	  (for (nil . file2) in files2)
	  (collect (cons name (ast-diff file1 file2))))))
|#

(defmethod ast-diff ((project1 project) (project2 project))
  (flet ((%obj (proj) (make-instance 'alist-for-diff :alist (all-files proj))))
    (list (ast-diff (%obj project1) (%obj project2)))))

(defun remove-files-not-in (files1 files2)
  (let ((ntab (make-hash-table :test #'equal)))
    (iter (for (n) in files2)
	  (setf (gethash n ntab) t))
    (remove-if-not (lambda (p) (gethash (car p) ntab)) files1)))

(defmethod ast-patch ((project project) (diff t) &rest args &key &allow-other-keys)
  (let* ((files-obj (make-instance 'alist-for-diff
				  :alist (all-files project)))
	 (new-files-obj (apply #'ast-patch files-obj diff args))
	 (new-project (copy project)))
    (setf (evolve-files new-project) (alist-of-alist-for-diff new-files-obj))
    new-project))


;;;; Build directory handling.

(defvar *build-dir* nil
  "Directory in which to build projects.
Each project needs a build directory which contains copies of the
build scripts and other dependencies. Paths within a project are
relative to *build-dir*, which allows us to do evolution in multiple
threads by creating separate build directory per thread.")

(defun make-build-dir (src-dir &key (path (temp-file-name)))
  "Create a temporary copy of a build directory for use during evolution."
  (restart-case (make-build-dir-aux src-dir path)
    (retry-make-build-dir ()
      :report "Retry `make-build-dir' with new temp dir."
      (make-build-dir src-dir))
    (new-path (new-path)
      :report "Retry `make-build-dir' to a new interactively specified path."
      :interactive (lambda ()
                     (princ "Path: " *query-io*)
                     (list (read-line  *query-io*)))
      (make-build-dir src-dir :path new-path))))

(defun make-build-dir-aux (src-dir path)
  "DOCFIXME"
  (let ((dir (ensure-directory-pathname path)))
    ;; Verify parent directory exists, otherwise the copy will fail.
    (ensure-directories-exist (pathname-parent-directory-pathname dir))
    ;; Copy from src-dir into path.
    (multiple-value-bind (stdout stderr errno)
        (shell "cp -pr ~a ~a" (namestring src-dir) (namestring dir))
      (declare (ignorable stdout))
      (assert (zerop errno) (src-dir path)
              "Creation of build directory failed with: ~a" stderr))
    dir))

(defun full-path (rel-path)
  "Prepend current build directory to a relative path."
  (assert *build-dir*)
  (in-directory *build-dir* rel-path))

(defmacro with-build-dir ((build-dir) &body body)
  "Rebind *build-dir* within BODY"
  `(let ((*build-dir* ,build-dir))
     ,@body))

(defmacro with-temp-build-dir ((src-dir) &body body)
  "Create a temporary copy of src-dir, and rebind *build-dir* to that
path within BODY."
  (let ((build-dir (gensym)))
    `(let ((,build-dir (when ,src-dir (make-build-dir ,src-dir))))
       (unwind-protect (with-build-dir (,build-dir) ,@body)
         (delete-directory-tree ,build-dir :validate t)))))

(defmethod phenome ((obj project) &key (bin (temp-file-name)))
  "Build the software project OBJ and copy build artifact(s) to BIN."
  (write-genome-to-files obj)
  ;; Build the object and copy it to desired location.
  (multiple-value-bind (stdout stderr exit)
      (shell "cd ~a && ~a" *build-dir* (build-command obj))
    (restart-case
        (if (zerop exit)
            (progn
              (when (> (length (artifacts obj)) 1)
                (shell "mkdir ~a" bin))
              (iter (for artifact in (artifacts obj))
                    (shell "cp -r ~a ~a"
                           (namestring (full-path artifact)) bin)))
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
