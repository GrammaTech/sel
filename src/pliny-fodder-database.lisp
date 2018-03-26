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
;;; Concrete implementation of the database interface
;;; for an external Pliny fodder database.

(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

;; Helpers
(defclass json-false ()
  ()
  (:documentation "DOCFIXME"))

(defmethod cl-json:encode-json ((object json-false) &optional stream)
  "DOCFIXME

* OBJECT DOCFIXME
* STREAM DOCFIXME
"
  (princ "false" stream)
  nil)

(defvar *json-false* (make-instance 'json-false)
  "DOCFIXME")

(defclass json-true ()
  ()
  (:documentation "DOCFIXME"))

(defmethod cl-json:encode-json ((object json-true) &optional stream)
  "DOCFIXME

* OBJECT DOCFIXME
* STREAM DOCFIXME
"
  (princ "true" stream)
  nil)

(defvar *json-true* (make-instance 'json-true)
  "DOCFIXME")

(define-condition pliny-query-failed (error)
  ((command :initarg :command :initform nil :reader command)
   (stdout :initarg :stdout :initform nil :reader stdout)
   (stderr :initarg :stderr :initform nil :reader stderr)
   (exit-code :initarg :exit-code :initform nil :reader exit-code))
  (:report (lambda (condition stream)
             (format stream "Shell command failed with status ~a: \"~a\"~%~
                             stdout: ~a~%~
                             stderr: ~a~%"
                     (exit-code condition) (command condition)
                     (stdout condition) (stderr condition))))
  (:documentation "DOCFIXME"))

(defmethod features-to-weights (features)
  "DOCFIXME"
  (mapcar (lambda (feature) (cons (car feature) (/ 1 (length features))))
          features))

;; Pliny Database
(defclass pliny-database (fodder-database)
  ((host :initarg :host
         :reader host
         :initform "localhost"
         :type simple-string
         :documentation "Pliny database host")
   (port :initarg :port
         :reader port
         :initform 10005
         :type integer
         :documentation "Pliny database port")
   (storage :initform (temp-file-name)
            :reader storage
            :type simple-string
            :documentation "Database storage file")
   (catalog :initform (temp-file-name)
            :reader catalog
            :type simple-string
            :documentation "File with database storage locations")
   (num-threads :initform 5
                :reader num-threads
                :type integer
                :documentation "Number of database front-end threads")
   (memory-per-thread :initform 436207616
                      :reader memory-per-thread
                      :type integer
                      :documentation "Memory per database thread")
   (server-log :initform (temp-file-name)
               :reader server-log
               :type simple-string
               :documentation "Database server log")
   (server-bin   :initform "GTServer"
                 :reader server-bin
                 :type simple-string
                 :documentation "Database server creation binary")
   (shutdown-bin :initform "GTServerShutdown"
                 :reader shutdown-bin
                 :type simple-string
                 :documentation "Database server shutdown binary")
   (loader-bin   :initform "GTLoader"
                 :reader loader-bin
                 :type simple-string
                 :documentation "Database loader binary")
   (query-bin    :initform "GTQuery"
                 :reader query-bin
                 :type simple-string
                 :documentation "Database query binary")
   (server-thread :reader server-thread
                  :documentation "Thread running the GTServer"))
  (:documentation "DOCFIXME"))

(defmethod from-file ((obj pliny-database) db)
  "Create a Pliny database using the contents of DB"
  (note 1 "Starting Pliny Database")
  (start-server obj)
  (sleep 2.5)

  (note 1 "Loading Pliny Database from ~a. ~
           This could take several minutes." db)
  (load-server obj db)

  #+sbcl
  ;; sb-ext:finalize will not be called during a normal exit of SBCL.
  ;; Further, it does not appear to be reliably called during a full
  ;; garbage collection.  Use exit hooks to cleanup GTServer instances
  ;; created during processing instead of 'destructors'.
  (push {shutdown-server obj} sb-ext:*exit-hooks*)
  #+ccl
  (push {shutdown-server obj} ccl:*lisp-cleanup-functions*)
  #-(or sbcl ccl)
  (warning "Unsupported lisp; please cleanup Pliny ~
            server instance manually upon exit")

  obj)

(defmethod from-string ((obj pliny-database) arg)
  "Parse a database argument in the form \"<HOST|FILE>:PORT\""
  (register-groups-bind (file-or-host-arg port-arg)
    ("^(.*):(\\d+)" arg)
    (when (and file-or-host-arg port-arg)
      (with-slots (host port) obj
        (setf host file-or-host-arg
              port (parse-integer port-arg)))
      (when (probe-file file-or-host-arg)
        (setf (slot-value obj 'host) "localhost")
        (from-file obj file-or-host-arg))))
  obj)

(defmethod database-emptyp ((obj pliny-database))
  "DOCFIXME"
  (handler-case
      (null (find-snippets obj :limit 1))
    (pliny-query-failed (e)
      (declare (ignorable e))
      t)))

(defmethod start-server ((obj pliny-database))
  "DOCFIXME"
  (setf (slot-value obj 'host) "localhost")
  (setf (slot-value obj 'server-thread)
        (make-thread (lambda()
                      (shell "~a ~a ~d ~d ~d ~a"
                             (server-bin obj)
                             (catalog obj)
                             (port obj)
                             (num-threads obj)
                             (memory-per-thread obj)
                             (server-log obj)))
                     :name (format nil "GTServerThread:~a" (port obj)))))

(defmethod load-server ((obj pliny-database) db)
  "DOCFIXME

* OBJ DOCFIXME
* DB DOCFIXME
"
  (with-temp-file (logfile)
    (shell "~a ~a ~d ~a --storage ~a --logfile ~a"
           (loader-bin obj) (host obj) (port obj) db (storage obj) logfile)))

(defmethod shutdown-server ((obj pliny-database))
  "DOCFIXME"
  (or (or (null (host obj)) (null (port obj)))
      (with-temp-file (logfile)
        (shell "~a ~a ~d --logfile ~a"
               (shutdown-bin obj) (host obj) (port obj) logfile)))
  (or (null (server-thread obj)) (join-thread (server-thread obj)))
  (or (null (catalog obj)) (delete-file (catalog obj)))
  (or (null (storage obj)) (delete-file (storage obj)))
  (or (null (server-log obj)) (delete-file (server-log obj)))

  (with-slots (host port server-thread catalog storage
               server-log) obj
    (setf host nil
          port nil
          server-thread nil
          catalog nil
          storage nil
          server-log nil))

  nil)

(defmethod print-object ((obj pliny-database) stream)
  "DOCFIXME

* OBJ DOCFIXME
* STREAM DOCFIXME
"
  (print-unreadable-object (obj stream :type t)
    (format stream "~a:~d" (host obj) (port obj))))

(defmethod find-snippets ((obj pliny-database)
                          &key ast-class full-stmt decls
			  (limit (- (expt 2 32) 1)))
  "DOCFIXME

* OBJ DOCFIXME
* AST-CLASS DOCFIXME
* FULL-STMT DOCFIXME
* DECLS DOCFIXME
* LIMIT DOCFIXME
"
  (let ((features (cond (ast-class
                         `((:ast--class  . ,ast-class)
                           (:random . ,(random 1.0))))
                        ((and full-stmt (eql decls :only))
                         `((:full--stmt . t)
                           (:is--decl . ,*json-true*)))
                        ((and full-stmt decls)
                         `((:full--stmt . t)
                           (:random . ,(random 1.0))))
                        (full-stmt
                         `((:full--stmt . t)
                           (:is--decl . ,*json-false*)
                           (:random . ,(random 1.0))))
                        ((eql decls :only)
                         `((:is--decl . ,*json-true*)
                           (:random . ,(random 1.0))))
                        ((not decls)
                         `((:is--decl . ,*json-false*)
                           (:random . ,(random 1.0))))
                        (t `((:random . ,(random 1.0)))))))
    (execute-query obj
                   `((:*features . ,features)
                     (:*weights  . ,(features-to-weights features)))
                   limit)))

(defmethod similar-snippets ((obj pliny-database) target
                            &key predicate metric
                              key ast-class limit-considered
                              (limit (- (expt 2 32) 1))
                              (filter #'null))
  "DOCFIXME

* OBJ DOCFIXME
* PREDICATE DOCFIXME
* METRIC DOCFIXME
* KEY DOCFIXME
* AST-CLASS DOCFIXME
* LIMIT-CONSIDERED DOCFIXME
* LIMIT DOCFIXME
* FILTER DOCFIXME
"
  (declare (ignorable predicate metric key limit-considered))
  (labels ((add-target-feature ()
             (if (every 'integerp target)
                 `((:binary--contents . ,(format nil "~{~2,'0x~^ ~}" target)))
                 `((:disasm . ,(format nil "~S" target)))))
           (add-ast-class-feature (features)
             (if ast-class
                 (append features `((:ast--class . ,ast-class)))
                 features)))
    (let ((features (-> (add-target-feature)
                        (add-ast-class-feature))))
      (remove-if filter
                 (execute-query obj
                                `((:*features . ,features)
                                  (:*weights . ,(features-to-weights features)))
                                limit)))))

(defmethod find-type ((obj pliny-database) hash)
  "DOCFIXME

* OBJ DOCFIXME
* HASH DOCFIXME
"
  (pliny-find-hash obj hash #'snippet->clang-type))

(defmethod find-macro ((obj pliny-database) hash)
  "DOCFIXME

* OBJ DOCFIXME
* HASH DOCFIXME
"
  ;; FIXME: Update pliny database to support new macro format
  (pliny-find-hash obj hash #'snippet->clang-macro))

(defun pliny-find-hash (pliny-db hash to-obj-fn)
  "DOCFIXME

* PLINY-DB DOCFIXME
* HASH DOCFIXME
* TO-OBJ-FN DOCFIXME
"
  (->> (execute-query pliny-db
                      `((:*features (:hash . ,hash))
                        (:*weights (:hash . 1)))
                      1)
       (first)
       (funcall to-obj-fn)))

(defgeneric execute-query (pliny-database query limit)
  (:documentation
   "Execute QUERY against PLINY-DATABASE with GTQuery."))

(defmethod execute-query ((obj pliny-database) query limit)
  "DOCFIXME

* OBJ DOCFIXME
* QUERY DOCFIXME
* LIMIT DOCFIXME
"
  (with-temp-file-of (query-file "json")
    (cl-json:encode-json-to-string query)
    (with-temp-file (log-file)
      (let ((query-command (format nil "~a ~a ~D ~D ~a --logfile ~a"
                                   (query-bin obj) (host obj) (port obj) limit
                                   query-file log-file))
            (cl-json:*identifier-name-to-key* 'se-json-identifier-name-to-key))
        (multiple-value-bind (stdout stderr errno)
            (shell query-command)
          (if (zerop errno)
              (reverse (cl-json:decode-json-from-string stdout))
              (error (make-condition 'pliny-query-failed
                       :exit-code errno
                       :command query-command
                       :stdout stdout
                       :stderr stderr))))))))

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
