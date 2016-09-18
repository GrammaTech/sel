;;; Concrete implementation of the database interface
;;; for an external Pliny fodder database.

(in-package :software-evolution)

(defvar *pliny-default-host* "localhost"
  "Default Pliny database host")

(defvar *pliny-default-port* 10005
  "Default Pliny database port")

(defclass pliny-database (fodder-database)
  ((host :initarg :host :accessor host
         :initform *pliny-default-host* :type simple-string)
   (port :initarg :port :accessor port
         :initform *pliny-default-port* :type integer)))

(define-condition pliny-query-failed (error)
  ((command :initarg :command :initform nil :reader command)
   (exit-code :initarg :exit-code :initform nil :reader exit-code))
  (:report (lambda (condition stream)
             (format stream "Shell command failed with status ~a: \"~a\""
                     (exit-code condition) (command condition)))))

(defmethod initialize-instance :after ((obj pliny-database) &key)
  (when (not (listp (find-snippets obj :limit 1)))
    (error "Pliny database ~a does not contain fodder snippets." obj)))

(defmethod print-object ((obj pliny-database) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~a:~d" (host obj) (port obj))))

(defmethod find-snippets ((obj pliny-database)
                          &key ast-class full-stmt decls
                            (limit (- (expt 2 32) 1)))
  (let ((features (cond (ast-class
                         `((:ast--class  . ,ast-class)
                           (:random . ,(random 1.0))))
                        ((and full-stmt decls)
                         `((:full--stmt . t)
                           (:random . ,(random 1.0))))
                        (full-stmt
                         `((:full--stmt . t)
                           (:is--decl . ,*json-false*)
                           (:random . ,(random 1.0))))
                        ((not decls)
                         `((:is--decl . ,*json-false*)
                           (:random . ,(random 1.0))))
                        (t `((:random . ,(random 1.0)))))))
    (execute-query obj
                   `((:*features . ,features)
                     (:*weights  . ,(features-to-weights features)))
                   limit)))

(defmethod sorted-snippets ((obj pliny-database) predicate
                            &key target key ast-class limit-considered
                              (limit (- (expt 2 32) 1))
                              (filter #'null))
  (declare (ignorable predicate key limit-considered))
  (labels ((format-op (op)
             (if (stringp op) (cl-json:decode-json-from-string op) op))
           (add-target-feature ()
             (if (every 'integerp target)
                 `((:binary--contents . ,(format nil "~{~2,'0x~^ ~}" target)))
                 `((:disasm . ,(format nil "~S" (mapcar #'format-op target))))))
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
  (first (execute-query obj
                        `((:*features (:hash . ,hash))
                          (:*weights (:hash . 1)))
                        1)))

(defgeneric execute-query (pliny-database query limit)
  (:documentation
   "Execute QUERY against PLINY-DATABASE with GTQuery."))

(defmethod execute-query ((obj pliny-database) query limit)
  (with-temp-file-of (query-file "json")
      (cl-json:encode-json-to-string query)
    (let ((query-command (format nil "GTQuery ~a ~D ~D ~a"
                                 (host obj) (port obj) limit query-file))
          (cl-json:*identifier-name-to-key* 'se-json-identifier-name-to-key))
      (multiple-value-bind (stdout stderr errno)
          (shell query-command)
        (declare (ignorable stderr))
        (if (zerop errno)
            (reverse (cl-json:decode-json-from-string stdout))
            (make-condition 'shell-command-failed
              :exit-code errno
              :command query-command))))))

(defmethod features-to-weights (features)
  (mapcar (lambda (feature) (cons (car feature) (/ 1 (length features))))
          features))

(defclass json-false ()
  ())

(defmethod cl-json:encode-json ((object json-false) &optional stream)
  (princ "false" stream)
  nil)

(defvar *json-false* (make-instance 'json-false))
