;;; rest-sessions.lisp --- Interface for managing RESTful sessions.
;;;
;;; This defines abilites to create new sessions and lookup existing sessions.
;;;
;;; @texi{rest}
(defpackage :software-evolution-library/rest-sessions
  (:nicknames :sel/rest-sessions)
  (:use
   :alexandria
   :arrow-macros
   :named-readtables
   :curry-compose-reader-macros
   :common-lisp
   :snooze
   :split-sequence
   :cl-json
   :iterate
   :trace-db
   :software-evolution-library/software-evolution-library
   :software-evolution-library/utility
   :software-evolution-library/components/test-suite
   :software-evolution-library/components/formatting
   :software-evolution-library/components/instrument
   :software-evolution-library/components/traceable
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang
   :software-evolution-library/command-line)
  (:shadowing-import-from :clack :clackup :stop)
  (:export :lookup-session
           :lookup-session-value
           :set-session-value
           :session-store-value
           :session-property
           :push-session-store-value
           :create-new-session))
(in-package :software-evolution-library/rest-sessions)

(defvar *session-id-generator* 1000
  "Start session ids at 1001 and increment")

(defvar *session-pool* (make-hash-table :test 'equal)
  "Keep a collection of live session objects")

(defun lookup-session (cid)
  (if (symbolp cid)
      (setf cid (string-downcase (symbol-name cid))))
  (gethash cid *session-pool*))

(defun lookup-session-value (cid key)
  (if-let ((client (lookup-session cid)))
    (session-store-value client (string-downcase (string key)))
    NIL))

(defun set-session-value (cid key value)
  (if-let ((client (lookup-session cid)))
    (setf (session-store-value
           client
           (string-downcase (string key)))
          value)
    NIL))

(defclass session ()
  ((id
    :initarg :id
    :accessor session-id
    :initform (format nil "client-~D" (incf *session-id-generator*))
    :documentation "Unique id for the client session")
   (settings
    :initarg :settings
    :accessor session-settings
    :initform '()
    :documentation "Global variable bindings specified by the client")
   (value-store ; generic storage for information to maintain session state
    :initarg :value
    :accessor session-value-store
    :initform (make-hash-table :test 'equalp))
   (properties  ; allow other modules (which use sel) to add properties
    :initarg :properties
    :accessor session-properties
    :initform (make-hash-table :test 'equalp)))
  (:documentation "Client session object"))

(defun session-store-value (session key)
  (gethash (string-downcase (string key))
           (session-value-store session)))

(defun push-session-store-value (session key value)
  (push value
        (gethash (string-downcase (string key))
                 (session-value-store session))))

(defun (setf session-store-value) (value session key)
  (setf (gethash (string-downcase (string key))
                 (session-value-store session))
        value))

(defun session-property (session name)
  (gethash name (session-properties session)))

(defun (setf session-property) (value session name)
  (setf (gethash name (session-properties session)) value))

;; Creates a new session, returning the appropriate session ID.
(defun create-new-session
    (&optional max-population-size cross-chance mutation-rate)
  (let ((session-obj
         (make-instance
             'session
           :settings
           (append
            (if max-population-size
                (list :max-population-size max-population-size))
            (if cross-chance
                (list :cross-chance
                      (or (numberp cross-chance)
                          (coerce (read-from-string cross-chance) 'real))))
            (if mutation-rate
                (list :mutation-rate
                      (or (numberp mutation-rate)
                          (coerce
                           (read-from-string mutation-rate)
                           'real))))))))
    (setf (gethash (session-id session-obj) *session-pool*) session-obj)
    (session-id session-obj)))

(defun lookup-resource (session str)
  "See if the json value is a string which can be parsed as a resource
   lookup. Resource lookups are of the form \"<resource>:<oid>\""
  (if (and (stringp str)
           (find #\: str))
      (let ((spl (split-sequence #\: str)))
        (if spl
            (let* ((res (first spl))
                   (id (ignore-errors (parse-integer (second spl)))))
              (and (stringp res) (integerp id)
                   (car
                    (member id (session-property session res)
                            :key 'sel::oid :test 'eql))))))))

(defroute
    client (:post "application/json")
  (let* ((json (handler-case
                   (json:decode-json-from-string (payload-as-string))
                 (error (e)
                   (http-condition 400 "Malformed JSON (~a)!" e))))
         (max-population-size (aget :max-population-size json))
         (cross-chance (aget :cross-chance json))
         (mutation-rate (aget :mut-rate json)))
    (create-new-session max-population-size cross-chance mutation-rate)))

