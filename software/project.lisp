;;; project.lisp --- evolve multiple source files

#|
*************************************************************************************
*
* Unless otherwise specified, software artifacts in this directory and its
* subdirectories are subject to:
*
* UNLIMITED RIGHTS
*
* The Government's rights to use, modify, reproduce, release, perform, display, or
* disclose this software are governed by DFARS 252.227-7013, RIGHTS IN TECHNICAL DATA
* --NONCOMMERCIAL ITEMS, and DFARS 252.227-7014 RIGHTS IN NONCOMMERCIAL SOFTWARE AND
* NONCOMMERCIAL COMPUTER SOFTWARE DOCUMENTATION.
*
*************************************************************************************
*
* All GrammaTech IP (sole or co-developed) needs to include the GrammaTech copyright.
*
* (c) 2016 GrammaTech, Inc.  All rights reserved.
*
* Such IP is also subject to the terms of the Prioprietary Information Agreement (PIA)
* executed between BAE Systems Information and Electronics Systems Integration Inc.
* and GrammaTech, Inc. dated April 21, 2015
*
*************************************************************************************
|#

(in-package :software-evolution)
(enable-curry-compose-reader-macros :include-utf8)

(define-software project (software)
  ((build-command :initarg :build-command :accessor build-command :initform nil
                  :documentation "Shell command to build the project.")
   (build-target :initarg :build-target :accessor build-target :initform nil
                 :documentation "The executable to build.
Passed as an argument to BUILD-COMMAND.")
   (evolve-files :initarg :evolve-files :accessor evolve-files :initform nil
                 :documentation "Files within the project to mutate"
                 :copier copy-files)
   (other-files :initarg :other-files :accessor other-files :initform nil
                :documentation "Files which are not mutated."
                :copier copy-files)
   ;; Implementation of this is tricky: use with-current-file rather
   ;; than setting it directly.
   (current-file-name :initform nil :accessor current-file-name
                      :documentation "Delegate method calls to this file.")))

(defun copy-files (files)
  (loop for (p . c) in files
     collecting (cons p (copy c))))

(defmethod current-file ((project project))
  "The software object representing the currently selected file within
PROJECT. Or NIL if current file is unset."
  (aget (car (current-file-name project))
        (all-files project)
        :test #'string=))

(defmacro with-current-file ((project file) &body body)
  "Bind the current file of PROJECT to FILE in BODY.

FILE can be either a software object or the filename, but it must
designate one of the component files of PROJECT.

Within BODY, all unrecognized methods will be forwarded to the
current-file. Copies of PROJECT will have their current file set to a
copy of the original current file.
"
  (let ((orig-file (gensym))
        (new-name (gensym)))
    `(let ((,orig-file (car (current-file-name ,project)))
           (,new-name ,(if (stringp file)
                           file
                           `(car (find ,file (all-files ,project)
                                       :key #'cdr)))))
       ,@(unless (stringp file)
           `((assert ,new-name
                     nil "~s is not a file of project ~s." ,file ,project)))
       (unwind-protect
            ;; Set current-file-name to a single-element list
            ;; containing the file name. Storing the name rather than
            ;; the file ensures that copies of PROJECT will have their
            ;; current-file bound to the corresponding object.
            (progn (setf (slot-value ,project 'current-file-name)
                         (list ,new-name))
                   ,@body)
         (progn
           ;; Set the first element of the list back to the original value.
           ;; Copies of PROJECT will have a reference to the same
           ;; list, so destructively modifying it will reset the
           ;; current file of copies as well.
           (setf (car (current-file-name ,project)) ,orig-file))))))

(defmethod no-applicable-method :around (method &rest args)
  "Forward method calls to current-file of projects."
  (let ((receiver (car args)))
    (if (and (typep receiver 'project)
             (current-file receiver))
        (apply method (current-file receiver) (cdr args))
        (call-next-method))))

(defmethod all-files ((obj project))
  (append (evolve-files obj) (other-files obj)))

(defmethod genome ((obj project))
  (if (current-file obj)
      (genome (current-file obj))
      ;; If no current file, join all genomes with separators.
      (format nil "~{~a~%~}"
              (loop for (f . c) in (all-files obj)
                 collect "=============================="
                 collect f
                 collect "=============================="
                 collect (genome c)))))

(defmethod (setf genome) (text (project project))
  (assert (current-file project) nil
          "Genome setting is only allowed when current-file is set.")
  (setf (genome (current-file project)) text))

(defgeneric write-genome-to-files (obj)
  (:documentation "Overwrite evolved files with current genome."))

(defmethod write-genome-to-files ((obj project))
  (loop for (path . c) in (all-files obj)
     do (string-to-file (genome c) (full-path path))))

(defmethod size ((obj project))
  (if (current-file obj)
      (size (current-file obj))
      (loop for (p . c) in (evolve-files obj)
         summing (size c))))

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
  (if (current-file obj)
      (apply-mutation (current-file obj) op)
      (destructuring-bind (file . mutation) op
        (apply-mutation (aget file (evolve-files obj) :test #'equal)
          mutation))))

(defmethod apply-mutations ((project project) (mut mutation) n)
  (labels ((apply-mutations-single-file (src-file obj n)
             (setf (object mut) obj)
             (iter (for targeted in (mapcar {at-targets mut}
                                            (targets mut)))
                   (for i below n)
                   (collect targeted into mutations)
                   (collect (->> (apply-mutation (copy obj) targeted)
                                 (incorporate (copy project) src-file))
                            into results)
                   (finally (return (values results mutations)))))
           (incorporate (project src-file obj)
             (setf (aget src-file (evolve-files project) :test #'equal) obj)
             project))
    (iter (for (src-file . obj) in (if (current-file project)
                                       (list (current-file project))
                                       (evolve-files project)))
          (while (< (length results) n))
          (multiple-value-bind (single-file-results single-file-mutations)
              (apply-mutations-single-file src-file obj (- n (length results)))
            (appending single-file-results into results)
            (appending single-file-mutations into mutations))
          (finally (return (values results mutations))))))

(defmethod apply-picked-mutations ((project project) (mut mutation) n)
  (labels ((apply-mutation-single-file (src-file obj)
             (setf (object mut) obj)
             (when-let* ((picked (funcall (picker mut) obj))
                         (targeted (at-targets mut picked)))
               (values (->> (apply-mutation (copy obj) targeted)
                            (incorporate (copy project) src-file))
                       targeted)))
           (incorporate (project src-file obj)
             (setf (aget src-file (evolve-files project) :test #'equal) obj)
             project))
    (iter (for i below n)
          (bind (((src-file . obj) (or (current-file project)
                                       (nth (pick-file project)
                                            (evolve-files project))))
                 ((:values result mutation)
                  (apply-mutation-single-file src-file obj)))
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

(defmethod apply-to-project ((project project) f)
  (values project
          (iterate (for file in (if (current-file project)
                                    (list (current-file project))
                                    (mapcar #'cdr (all-files project))))
                   (collect (funcall f file)))))


;;; Build directory handling.

;;; Each project needs a build directory which contains copies of the
;;; build scripts and other dependencies. Paths within a project are
;;; relative to *build-dir*, which allows us to do evolution in
;;; multiple threads by creating separate build directory per thread.

(defvar *build-dir* nil "Directory in which to build projects")

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
  (let ((dir (ensure-directory-pathname path)))
    ;; Verify parent directory exists, otherwise the copy will fail.
    (ensure-directories-exist (pathname-parent-directory-pathname dir))
    ;; Copy from src-dir into path.
    (multiple-value-bind (stdout stderr errno)
        (shell "cp -pr ~a ~a" (namestring src-dir) (namestring dir))
      (declare (ignorable stdout stderr))
      (assert (zerop errno)))
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
  (write-genome-to-files obj)

  ;; Build the object for fitness testing and copy it to desired location
  (multiple-value-bind (stdout stderr exit)
      (shell "cd ~a && ~a ~a" *build-dir*
             (build-command obj) (build-target obj))
    (values
     (when (zerop exit)
       (prog1 bin
         (shell "cp -r ~a ~a" (namestring (full-path (build-target obj))) bin)))
     exit
     stderr
     stdout
     (mapcar [#'full-path #'first] (evolve-files obj)))))
