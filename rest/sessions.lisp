;;; sessions.lisp --- Interface for managing RESTful sessions.
;;;
;;; This defines abilites to create new sessions and lookup existing sessions,
;;; managed by session client ID.
;;;
;;; @subsubsection Dependencies
;;;
;;; The Rest API leverages a number of Common Lisp components, which
;;; are open-source and generally available via Quicklisp.  See the core REST
;;; file for a full description and how to start a rest server.
;;;
;;; @subsubsection Resources and Operations
;;;
;;; A standard SEL Rest API supports logical resources, all driven by an
;;; underlying client session system. This file defines that client session
;;; system, including the internal data structures and the client session
;;; endpoint.
;;;
;;; @subsubheading Resources
;;;
;;; The following resources are define by this file:
;;;
;;;  Client sessions
;;;     establish ownership of a jobs, software objects, populations, etc.
;;;
;;; @subsubheading Operations on Resources
;;;
;;; Client:
;;;
;;;  See the `sessions` section for more details.
;;;
;;;  POST
;;;     @code{<service-base>/client} Create a new client session. The
;;;     request should include a JSON body for special variable bindings
;;;     including:
;;;
;;;     - @code{max-population-size (integer)} : Max Population Size (see
;;;       @code{*max-population-size*})
;;;     - @code{mutation-rate (float)} : Mutation Rate (see
;;;       @code{*mut-rate*})
;;;     - @code{cross-chance (float)} : Cross Chance (see
;;;       @code{*cross-chance*})
;;;
;;;     Returns the Client-ID.
;;;  DELETE
;;;     @code{<service-base>/client&cid=<cid>} Delete
;;;     the client session, and cause all the software objects
;;;     owned by the session to become garbage.
;;;
;;;
;;; @texi{rest-sessions}
(defpackage :software-evolution-library/rest/sessions
  (:nicknames :sel/rest/sessions)
  (:use
   :gt/full
   :snooze
   :software-evolution-library/software-evolution-library
   :software-evolution-library/components/test-suite
   :software-evolution-library/components/formatting
   :software-evolution-library/rest/utility
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
(in-package :software-evolution-library/rest/sessions)

(defvar *session-id-generator* 1000
  "Start session ids at 1001 and increment")

(defvar *session-pool* (make-hash-table :test 'equal)
  "Keep a collection of live session objects")

(defun lookup-session (cid)
  "Look up a session by Client ID."
  (if (symbolp cid)
      (setf cid (string-downcase (symbol-name cid))))
  (gethash cid *session-pool*))

(defun lookup-session-value (cid key)
  "Look up a session value by Client ID and key."
  (if-let ((client (lookup-session cid)))
    (session-store-value client (string-downcase (string key)))
    NIL))

(defun set-session-value (cid key value)
  "Set a session value by Client ID and key."
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

(defun create-new-session
    (&optional max-population-size cross-chance mutation-rate)
  "Creates a new session, returning the appropriate session ID."
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

(defroute
    client (:post "application/json")
  (let* ((json (handler-case
                   (decode-json-payload)
                 (error (e)
                   (http-condition 400 "Malformed JSON (~a)!" e))))
         (max-population-size (aget :max-population-size json))
         (cross-chance (aget :cross-chance json))
         (mutation-rate (aget :mut-rate json)))
    (create-new-session max-population-size cross-chance mutation-rate)))

