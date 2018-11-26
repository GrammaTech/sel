;;;; clang-merge.lsip -- command line interface for three way merging
(defpackage :software-evolution-library/ast-diff/clang-merge
  (:nicknames :sel/ast-diff/clang-merge)
  (:documentation "Command line interface for three way merging of software")
  (:use :common-lisp
	:alexandria
        :command-line-arguments
        :metabang-bind
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
	:split-sequence
	:uiop
        :bordeaux-threads
        :uiop/filesystem
        :software-evolution-library/utility
	:software-evolution-library/ast-diff/ast-diff
        :software-evolution-library
        :software-evolution-library/software/project)
  (:shadowing-import-from
   :uiop :getenv :directory-exists-p :copy-file :appendf :parse-body
   :ensure-list :simple-style-warning :ensure-gethash :ensure-function
   :if-let :emptyp :featurep)
  (:export :clang-merge :run-clang-merge))
(in-package :sel/ast-diff/clang-merge)
(in-readtable :curry-compose-reader-macros)

(defun run-clang-merge (&aux (self (argv0)) (args *command-line-arguments*))
  (run-clang-merge* self args))

(defun run-clang-merge* (self args
			 &aux
			   original version1 version2
			   original-soft version1-soft version2-soft
			   project-name ;; software-store ;; flags
			   comp-db (build-command "make")
			   ;; (no-store-software t) ;; includes
			   out-dir
			   ;; (language "c") artifacts (compiler "clang")
			   )
  "Merge two branches original => version1, original => version2 into
a merged version"
  (flet ((report (fmt &rest args)
           (apply #'format *error-output* (concatenate 'string "~a: " fmt)
		  self args))
	 (usage (n)
	   (format t
                   "Usage: ~a [OPTION] ~
                    original-version version1 version2 output-directory~%" self)
	   (quit n)))
    (when (or (< (length args) 4)
	      (if-let ((d (string/= "-h" (car args)))) (>= d 2))
	      (if-let ((d (string/= "--h" (car args)))) (>= d 3)))
      (usage 0))

    (getopts
     (args :unknown :return)
     ("-o" "--out-dir" (setf out-dir (pop args)))
     ("-c" "--comp-db" (setf comp-db (pop args)))
     ;; ("--compiler" "--compiler" (setf compiler (pop args)))
     ;; ("-I" "-I" (setf includes (split-sequence #\, (pop args)
     ;;                                           :remove-empty-subseqs t)))
     ("--build-command" "-b" (setf build-command (pop args)))
     ;; ("-l" "--language" (setf language (pop args)))
     ;; ("--no-store-software" "--no-store-software" (setf no-store-software t))
     )

    (when (/= (length args) 4) (usage 2))
    (flet ((check-file (file)
	     (unless (probe-file file)
	       (format *error-output*
		        "~a: ~a: No such file or directory~%"
			self file)
	       (quit 3))
	     file))
      (setf original (check-file (pop args)))
      (setf version1 (check-file (pop args)))
      (setf version2 (check-file (pop args)))
      (setf out-dir (pop args))
      (setf out-dir (pathname (concatenate 'string out-dir "/")))
      (ensure-directories-exist out-dir)
      (setf out-dir (pathname-directory out-dir))))

  ;; (setf flags (mappend [{list "-I"} {concatenate 'string (pwd) "/"}]
  ;;                      includes))
   (when comp-db
      (push (cons :build-path comp-db)
            *clang-mutate-additional-args*))

   ;; These functions were defined in BI
   ;; They should get moved to SEL as global
   ;; utility functions.  For now, use FLET
   (flet ((resolve-out-dir-from-source (source)
	     (if-let ((as-dir (directory-p source)))
	       (butlast (pathname-directory as-dir))
	       (pathname-directory source)))
	  (resolve-name-from-source (source)
	    (if-let ((as-dir (directory-p source)))
	      (lastcar (pathname-directory as-dir))
	      (pathname-name source)))
	  (resolve-store-path-from-out-dir-and-name (out-dir name)
	    (namestring (make-pathname :directory out-dir
				       :name name
				       :type "store"))))
     (setf original (truename original)
	   version1 (truename version1)
	   version2 (truename version2)
	   out-dir (or out-dir (resolve-out-dir-from-source original))
	   project-name (resolve-name-from-source original)
	   ;; software-store nil
	   ;; (unless no-store-software
           ;;   (resolve-store-path-from-out-dir-and-name out-dir project-name))
	   ))

  (note 1  "Parameters:~%~S~%"
        `((original . ,original)
          (version1 . ,version1)
          (version2 . ,version2)
          (out-dir . ,out-dir)
          (*note-level* . ,*note-level*)))

  (note 1 "Creating software objects after processing options.")
  (flet ((%create (s)
	   (let ((project
		  (make-instance 'clang-project :project-dir s
				 :compilation-database comp-db
				 :build-command build-command
				 ;; :flags flags
				 ;; :store-path software-store
				 )))
	     (from-file project s))))
    (setf original-soft (%create original))
    (setf version1-soft (%create version1))
    (setf version2-soft (%create version2))

    ;; Create styled versions of the input files
    (save-styled-to original-soft out-dir "original")
    (save-styled-to version1-soft out-dir "v1")
    (save-styled-to version2-soft out-dir "v2")

    ;; Perform merge

    (multiple-value-bind (new-merged unstable)
	(converge original-soft version1-soft version2-soft)
      (save-styled-to new-merged out-dir "merged")
      (if (not unstable)
	  (format t "No merge conflicts~%")
	  (format t "Merge conflicts:~%~a~%" unstable)))))

(defgeneric save-styled-to (soft out-dir sub))

(defmethod save-styled-to ((soft project) out-dir sub)
  (let ((dest (make-pathname :directory (append out-dir (list sub)))))
    (unless (probe-file dest)
      (to-file (astyle (copy soft)) dest))))
