;;; project.lisp --- evolve multiple source files
(defpackage :software-evolution-library/software/project
  (:nicknames :sel/software/project :sel/sw/project)
  (:use :gt/full
        :metabang-bind
        :software-evolution-library
        :software-evolution-library/software/simple
        :software-evolution-library/components/formatting
        :software-evolution-library/components/configuration)
  (:import-from :software-evolution-library/software/parseable
                :ast-path)
  (:export :project
           :*build-dir*
           :build-command
           :artifacts
           :evolve-files
           :evolve-files-ref
           :walk-evolve-files
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
           :all-files
           :pick-file
           :include-paths
           :include-paths-mixin
           :text-file-p
           :find-include-files
           :directories-of-header-files
           :project-relative-pathname))
(in-package :software-evolution-library/software/project)
(in-readtable :curry-compose-reader-macros)

(defparameter *always-ignore-paths*
  '(".git/**/*"
    ".vscode/**/*"
    ".ccls-cache/**/*"
    ".DS_Store/**/*"))

(defparameter *default-ignore-paths*
  '())

(define-software project (software)
  ((build-command :initarg :build-command :reader build-command
                  :initform nil
                  :documentation "Shell command to build the project.")
   (artifacts :initarg :artifacts :reader artifacts :initform nil
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
    "Source files which may be used but not evolved.
This holds a list of cons cells of the form (path . software-object-for-path)."
    :copier copy-files)
   (ignore-paths
    :initarg :ignore-paths
    :reader ignore-paths
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
  (:default-initargs
   :ignore-paths *default-ignore-paths*
   :ignore-other-paths *default-ignore-paths*)
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

(defmethod initialize-instance :after ((project project) &key)
  "Wrapper to ensure software objects are not created from git artifacts."
  (setf (slot-value project 'ignore-other-paths)
        (union *always-ignore-paths* (ignore-other-paths project) :test #'equal)
        (slot-value project 'ignore-paths)
        (union *always-ignore-paths* (ignore-paths project) :test #'equal)))

(defun ignored-path-p (path &key ignore-paths only-paths
                       &aux (canonical-path (canonical-pathname path)))
  (flet ((included (files)
           (find-if {pathname-match-p canonical-path} files)))
    (or (and only-paths (not (included only-paths)))
        (included ignore-paths))))

(defgeneric evolve-files-ref (software path)
  (:documentation "Lookup PATH in the evolve-files of SOFTWARE.")
  (:method (software (path string))
    (aget path (evolve-files software) :test #'equal))
  (:method (software (path pathname))
    (evolve-files-ref software (namestring path))))

(defun walk-evolve-files (project fn)
  "Call FN on the path and software of each evolve-file in PROJECT."
  (fbind (fn)
    (iter (for (k . v) in (evolve-files project))
          (fn k v))))

(defun (setf evolve-files-ref) (value software path)
  "Make PATH point to VALUE in SOFTWARE."
  (check-type value software)
  ;; Update without mutating in case of shared structure with other
  ;; projects.
  (setf (evolve-files software)
        (acons path value
               (remove path (evolve-files software)
                       :key #'car
                       :test #'equal))))

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

(defgeneric project-relative-pathname (project path)
  (:documentation "Reinterpret PATH as relative to PROJECT.
Like `asdf:system-relative-pathname' for projects.

If PATH is a string, it is parsed using Unix syntax.")
  (:method ((p project) (f string))
    (project-relative-pathname p (parse-unix-namestring f)))
  (:method ((project project) (pathname pathname))
    (base-path-join (ensure-directory-pathname (project-dir project))
                    pathname)))

(defun copy-files (files)
  "Copier for `evolve-files' on `project' software objects."
  (loop for (p . c) in files
     collecting (cons p (copy c))))

(defun shallow-copy-files (files)
  "Copier for `other-files' on `project' software objects. Since these are not
 modified during evolve, no need to fully copy them--just copy the wrappers."
  (loop for (p . c) in files
        collecting (cons p c)))

(defmethod all-files ((obj project))
  "Returns concatenation of `evolve-files' and `other-files'."
  (append (evolve-files obj) (other-files obj)))

(defmethod genome-string ((obj project) &optional stream)
  "Returns all genomes joined with separators, optionally writing to STREAM."
  (let ((genome-string (format nil "~{~a~%~}"
                               (loop for (f . c) in (all-files obj)
                                  collect "=============================="
                                  collect f
                                  collect "=============================="
                                  collect (genome-string c)))))
    (if stream (write-string genome-string stream) genome-string)))

(defmethod (setf genome) (text (project project))
  (declare (ignore text project))
  (error "Can only set the genome of component files of a project."))

(defgeneric collect-evolve-files (project)
  (:documentation "Create the evolve files for PROJECT."))

(defun text-file-p (p)
  "Given a path, returns T if the file appears to be a text file (not binary).
 This is determined by a heuristic: if no 0 bytes are found in the first
 256 bytes, it presumes it is text (and not binary)."
  (with-open-file (is p :direction :input :element-type 'unsigned-byte)
    (dotimes (i 256 t)
      (let ((b (read-byte is nil nil)))
        (cond ((null b) (return t))
              ((zerop b) (return nil)))))))

(defgeneric collect-other-files (project)
  (:documentation
   "Find parseable files in PROJECT that were not included in `evolve-files'.
Assumes `evolve-files' has been initialized.  Only applies to
non-symlink text files that don't end in \"~\" and are not ignored by
`ignore-other-paths', or `only-other-paths'.")
  (:method ((project project))
    ;; Create software objects for these other files.
    (nest
     (flet ((ends-in-tilde (s)
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
     (remove-if (hash-table-function
                 (alist-hash-table (evolve-files project)
                                   :test #'equal)))
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

  ;; Verify directory exists, otherwise the copy will fail.
  (ensure-directories-exist path)

  ;; Write the software objects.
  (loop for (file . obj) in (all-files project)
        do (to-file obj (ensure-directories-exist
                         (path-join path file))))

  ;; Exit early if the project path is identical to path.
  (when (pathname-equal (project-dir project) path)
    (return-from to-file))

  ;; Copy the remainder of the project directory to the output path.
  (when (and (project-dir project)
             (probe-file (project-dir project))
             (not (equalp path (canonical-pathname (project-dir project)))))
    (flet ((ensure/ (path)
             (when (pathnamep path)
               (setf path (namestring path)))
             (if (string$= #\/ path)
                 path
                 (string+ path #\/))))
      (multiple-value-bind (stdout stderr errno)
          (shell "cp -pnRf ~a. ~a" (ensure/ (project-dir project)) (ensure/ path))
        (declare (ignorable stdout))
        (assert (zerop errno) (path)
                "Population of output directory failed with: ~a" stderr)))))

(defmethod size ((obj project))
  "Return summed size across all `evolve-files'."
  (reduce #'+ (mapcar [#'size #'cdr] (evolve-files obj))))

(defgeneric pick-file (obj)
  (:documentation "Randomly pick one evolve file. Return its index in
the alist.")
  (:method ((obj project))
    (proportional-pick (evolve-files obj)
                       (lambda (x) (max 1 (size (cdr x)))))))

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
                   (collect (nest (incorporate (copy project)
                                               (car evolve-file))
                                  (apply-mutation (copy (cdr evolve-file))
                                                  targeted))
                            into results)
                   (finally (return (values results mutations)))))
           (incorporate (project src-file obj)
             (setf (aget src-file (evolve-files project) :test #'equal) obj)
             project))
    (iter (for evolve-file in (evolve-files project))
          (while (length< results n))
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
               (values (nest (incorporate (copy project)
                                          (car evolve-file))
                             (apply-mutation (copy (cdr evolve-file))
                                             targeted))
                       targeted)))
           (incorporate (project src-file obj)
             (setf (aget src-file (evolve-files project) :test #'equal) obj)
             project))
    (iter (for i upfrom 0)
          (while (and (length< results n)
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
  (if-let ((pool (union (evolve-files a) (evolve-files b)
                        :key #'car :test #'equal)))
    (bind ((file (car (random-elt pool)))
           ((:values crossed point-a point-b)
            (crossover (cdr (find file (evolve-files a)
                                  :key #'car :test #'equal))
                       (cdr (find file (evolve-files b)
                                  :key #'car :test #'equal)))))
          ;; Add filenames to crossover points for better stats
          (values (nest (copy a :evolve-files)
                        (cons (cons file crossed)
                              (remove file (evolve-files a)
                                      :key #'car :test #'equal)))
                  (cons point-a file)
                  (cons point-b file)))
    (values (copy a) nil nil)))

(defmethod format-genome ((project project) &key)
  "Apply a code formatting tool to each file in PROJECT."
  (apply-to-project project #'format-genome))

(defmethod apply-to-project ((project project) function)
  "Mapcar FUNCTION over `all-files' of PROJECT."
  (values project (mapcar [function #'cdr] (all-files project))))

(defun copy-artifacts (project build-dir bin)
  "Copies ARTIFACTS from BUILD-DIR to BIN following symbolic links.
If there's one artifact, it will be copied directly to BIN without
making a directory."
  (let ((artifacts (artifacts project)))
    (cond
      ((length> artifacts 1)
       ;; Copy artifacts to BIN.
       (iter (for artifact in artifacts)
             (for target-dir = (namestring (merge-pathnames-as-directory
                                            (ensure-directory-pathname bin)
                                            artifact)))
             (multiple-value-bind (stdout stderr exit)
                 (shell "mkdir -p ~a && cp -RL ~a ~a"
                        target-dir
                        (namestring (merge-pathnames-as-file
                                     (ensure-directory-pathname build-dir)
                                     artifact))
                        target-dir)
               (declare (ignore stdout))
               (when (not (zerop exit))
                 (error (make-condition 'phenome :text stderr
                                        :loc build-dir))))))
      ((car artifacts)
       ;; Copy artifact to BIN.
       (multiple-value-bind (stdout stderr exit)
           (shell "cp -RL ~a ~a"
                  (namestring (merge-pathnames-as-file
                               (ensure-directory-pathname build-dir)
                               (car artifacts)))
                  bin)
         (declare (ignore stdout))
         (when (not (zerop exit))
           (error (make-condition 'phenome :text stderr
                                  :loc build-dir))))))))

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
      (unless keep-file-p (delete-path build-dir)))))

(defmethod phenome
    ((obj project) &key
                     (bin (temp-file-name))
                     (build-dir (or *build-dir* (project-dir obj))))
  "Build the software project OBJ and copy build artifact(s) to BIN."
  (multiple-value-bind (stdout stderr exit)
      (shell "cd ~a && ~a" build-dir (build-command obj))
    (restart-case
        (if (zerop exit)
            (copy-artifacts obj build-dir bin)
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
            (mapcar [{merge-pathnames-as-file
                      (ensure-directory-pathname build-dir)}
                     #'first]
                    (evolve-files obj)))))

(defmethod lookup ((obj project) (key list))
  ;; Enables the use of the `@' macro directly against projects.
  (let ((next (lookup obj (car key))))
    (if (cdr key)
        (lookup next (cdr key))
        next)))

(defmethod lookup ((obj project) (key number))
  ;; Enables the use of the `@' macro directly against projects.
  (cdr (nth key (evolve-files obj))))

(defmethod lookup ((obj project) (key string))
  ;; Enables the use of the `@' macro directly against projects.
  (cdr (assoc key (evolve-files obj) :test #'string=)))

(defmethod ast-path ((obj project) (key software))
  (car (rassoc key (evolve-files obj))))

(defmethod mapc (function (obj project) &rest more)
  (declare (ignorable more))
  (mapc (op (mapc function (cdr _1))) (evolve-files obj))
  obj)

(defmethod mapcar (function (obj project) &rest more)
  (declare (ignorable more))
  (copy obj :evolve-files (mapcar (op (cons (car _1) (mapcar function (cdr _1))))
                                  (evolve-files obj))))

(defmethod convert ((to-type (eql 'list)) (project project) &key &allow-other-keys)
  (mappend [{convert 'list} #'cdr] (evolve-files project)))


;;; Include file processing
;;;
;;; In order to associate identifiers with definitions in C and C++,
;;; we need to be able to find include files.  This code is a simple
;;; stab at that.
;;;
;;; The method here does not require a FILE argument, but if it is
;;; present it looks for the include file in that file's directory
;;; before searching the include paths.
;;;
;;; We do not yet have machinery for handling system includes,
;;; those specified with <...> in C and C++.
;;;
;;; One option would be for library include files to not be handled
;;; directly, but instead use some sort of standard-conforming stub.
;;; This method does not yet search for such include files.  Stubs
;;; will contain abbreviated definitions that still parse (in
;;; tree-sitter) but that do not commit to some implementation.
;;; Exactly what these look like will need to be defined explicitly.
;;; Some possible examples:
;;;
;;;    typedef foo_t foo_t;    /* self referential def */
;;;    typedef struct bar_tag bar_t;
;;;    #define XYZ XYZ        /* Self expanding macro */
;;;
;;; Alternately, we could have a separate syntax for stub files that
;;; that allows extra information to be added, and not overload the
;;; language like that.
;;;
;;; Alternately, we could just read the system header files.  This
;;; last approach would require adding the system include paths as
;;; another slot in the mixin.  Instead of searching ALL-FILES, the
;;; actual directories there would be searched, and if an include file
;;; were found it would be read and parsed and a component created
;;; (although not put into ALL-FILES).
;;;
;;; TBD.
;;;
;;; #ifdef processing for symbol tables:
;;;
;;;    The approach will be to simply leave ifdefs unexpanded.
;;;    Internal ifdefs will be part of the AST.   Ifdefs that affect
;;;    the names of objects will leave the names unfindable in
;;;    a symbol table.   If more than one definition is found
;;;    then store them all.  We may want to record the preprocessor
;;;    conditions in force while traversing an AST so we can record
;;;    that for each definition (why not).
;;;
(defclass include-paths-mixin ()
  ;; May want a second set of paths for system include files
  ((include-paths :initarg :include-paths
                  :initform (list (make-pathname :directory nil))
                  :accessor include-paths
                  :documentation "List of pathnames (either absolute
or relative to the root of the project) in which include files are
to be searched"))
  (:documentation "Mixin for finding include files.  This can be combined
with a project class or a class for software objects for files of specific
languages, such as C or C++."))

(defgeneric find-include-files (project file include-name)
  (:documentation "Locate the include file(s) for inclusion of include-name."))

(defmethod find-include-files ((proj project) (file t) (include-name string))
  (find-include-files proj file (merge-pathnames (pathname include-name)
                                                 (make-pathname :type "h"))))

(defmethod find-include-files ((proj project) (file t) (include-pathname pathname))
  (include-files-in-files proj (include-paths proj) include-pathname))

;;; I find myself traversing the entire all-files list here.
;;; This isn't scalable.  There should (also?) be a hash table from file namestrings
;;; to component objects.

(defmethod find-include-files ((proj project) (file include-paths-mixin) (include-path pathname))
  (let ((all-files (all-files proj)))
    (block nil
      (let ((file-name (rassocar file all-files)))
        (if file-name
            (let ((include-file-pathname (merge-pathnames* include-path (pathname file-name))))
              (if-let ((p (assoc include-file-pathname all-files
                                 :test (lambda (x y) (equal x (pathname y))))))
                (return (list (cdr p)))))
          (progn (warn "File object ~a not found in project ~a" file proj)
                 nil)))
      (if-let ((include-paths (include-paths file)))
        (include-files-in-files proj include-path include-path)
        (call-next-method)))))

(defgeneric include-files-in-files (proj include-paths include-pathname)
  (:documentation "Given a list of include paths, and a pathname for a
specific include, returns a list of sw objects for the matching include
files.   For example, if PROJ has include paths (\"include/\" \"other-include/\"),
and there is a sw object for the file include/lib/foo.h, then when
include-pathname is #p\"lib/foo.h\" this will return the list containing
the sw object for include/lib/foo.h")
  ;; This has been broken out into a separate function because it was
  ;; needed for two methods of find-include-files: one where include-paths
  ;; comes from the file,  and the other where it comes from the project.
  ;;
  ;;  TODO:  add separate consideration of "system include files" and
  ;;   "user include files"
  (:method ((proj project) (include-paths list) (include-pathname pathname))
    (let ((all-files (all-files proj)))
      (iter (for path in include-paths)
            (assert (typep path 'pathname))
            (let ((full-path (merge-pathnames* include-pathname path)))
              (when-let ((p (find full-path all-files :key [#'pathname #'car]
                                                      :test #'equal)))
                        (collecting (cdr p))))))))

(defgeneric directories-of-header-files (project &key types)
  (:documentation "Return a list of the pathnames of directories
that contain header files.   TYPES is a list of strings of the
types of files to loeok for (defaults to (\"h\")).  Names are
sorted into shortlex order by namestring.")
  (:method ((project include-paths-mixin) &key (types '("h")))
    (let ((pathnames nil))
      (iter
       (for (p . c) in (all-files project))
       (declare (ignorable c))
       (let ((pn (pathname p)))
         (when (member (pathname-type pn) types :test #'equal)
           (pushnew (make-pathname :name nil :type nil :defaults pn)
                    pathnames
                    :test #'equal))))
      (sort pathnames
            (lambda (p1 p2)
              (let* ((s1 (namestring p1))
                     (s2 (namestring p2))
                     (l1 (length s1))
                     (l2 (length s2)))
                (or (< l1 l2)
                    (and (= l1 l2)
                         (string< s1 s2)))))))))

;;; Also needed: compute closure of includes


;;; Configuration File

(defmethod configuration-path ((project project) &key path)
  "Return the configuration file path of PROJECT."
  ;; TODO: project-dir doesn't appear to be populated for base project objects?
  (make-pathname :directory (pathname-directory (or path (project-dir project)))
                 :name ".sel" :type "conf"))

;;; NOTE: this should probably be a :before, but 'project-dir isn't populated
;;;       at that point.
(defmethod from-file :before ((project project) path)
  (handle-configuration project path))

(defun add-ignore-paths (config paths)
  "Add FILES to the configuration file of PROJECT such that they are ignored.
Stores them as comma separated values."
  (setf (option config "Ignore" "ignore-paths")
        (reduce #'string+ (interleave (mapcar #'namestring paths) ","))))

(defmethod handle-configuration ((project project) path &key)
  (let ((config (get-configuration project :path path)))
    (when-let ((ignore-paths (option config "Ignore" "ignore-paths")))
      (with-slots ((previous-value ignore-paths)) project
        (setf previous-value (append previous-value (split "," ignore-paths)))))))
