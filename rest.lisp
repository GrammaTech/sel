;;; rest.lisp --- RESTful interface over SEL.
;;;
;;; Rest API for Software Evolution Library
;;;
;;; The Rest API for Software Evolution Library is implemented as a web
;;; service which may be accessed via HTTP operations.
;;;
;;; It attempts to conform to principals described here:
;;; @uref{https://en.wikipedia.org/wiki/Representational_state_transfer,
;;; Representational State Transfer}
;;;
;;; @subsection Dependencies
;;;
;;; The Rest API leverages a number of Common Lisp components, which
;;; are open-source and generally available via Quicklisp.  These
;;; packages support JSON <-> Common Lisp translations, JSON
;;; streaming, HTTP web server functions, client HTTP support and
;;; RESTful interface utilities.
;;;
;;;  CL-JSON
;;;      Parse and generate JSON format
;;;  ST-JSON
;;;      Stream support for JSON format
;;;  CLACK
;;;      utility to easily launch web services
;;;  DRAKMA
;;;      http client utilities for Common Lisp (for calling Rest
;;;      APIs/testing
;;;  HUNCHENTOOT
;;;      Web server, written in Common Lisp, hosts Rest APIs
;;;  SNOOZE
;;;      Rest API framework
;;;
;;; @subsection Running the Rest API Web Service
;;;
;;; Starting the server:
;;;
;;;     (start-server)
;;;
;;; Stopping the server:
;;;
;;;     (stop-server)
;;;
;;; Restart the server:
;;;
;;;     (start-server)            ;; will stop, if running, then start
;;;
;;; @subsection Resources and Operations
;;;
;;; A Rest API supports logical resources. This section lists the logical
;;; resources supported by the SEL Rest APIs. In some cases (e.g. Software)
;;; there is a class hierarchy in the SEL package which directly models the
;;; resource. Or rather, the resource can be thought to be a distillation
;;; of the methods of that class. In other cases (e.g. Populations) there
;;; isn't a specific class in SEL. The Rest API resource in this course
;;; provides a way to name and manage lists of Software objects.
;;;
;;; @subsubsection Resources
;;;
;;; The following types of resources are supported.
;;;
;;;  Software
;;;      Any non-abstract class that inherits from SOFTWARE
;;;
;;;  Mutations
;;;      SEL mutation objects.
;;;
;;;  Populations
;;;      Collections of software objects, which may change rapidly (evolution).
;;;
;;;  Jobs
;;;     Any long running operation which starts a task and returns to
;;;     the client immediately.  Examples of jobs include running
;;;     evolutions, fitness tests across large populations, and
;;;     searches.
;;;
;;;  Client sessions
;;;     establish ownership of a jobs, software objects, populations, etc.
;;;
;;;  Test Suites
;;;     Lists of Test Cases, which each include references to an
;;;     executable file and some command-line argumants. These are
;;;     used to evaluate Software objects.
;;;
;;;  Traces
;;;     Models and manages the data collected from running test suites against
;;;     instrumented Software objects.
;;;
;;;  Instrumented Software
;;;     Software objects which have been instrumented.
;;;
;;;  Traced Software
;;;     Instrumented Software objects which have also been traced.
;;;
;;; @subsubsection Operations on Resources
;;;
;;; Note: all operations (other than session create) require a client-ID
;;; parameter. Although only specified in the first ones below, all others
;;; require it as well.
;;;
;;; Client:
;;;
;;;  POST
;;;     @code{<service-base>/client} Create a new client.  Body
;;;     (JSON) contains initial values for special variable settings.
;;;     Returns the Client-ID.
;;;  DELETE
;;;     @code{<service-base>/client&cid=<client-ID>} Delete
;;;     the client session, and cause all the software objects
;;;     owned by the client to become garbage.
;;;
;;; Software:
;;;
;;;  POST
;;;     @code{<service-base>/soft?cid=<client-ID>&type=<software-type>}
;;;     Body, JSON format, contains path (to C file, AST, Asm file,
;;;     etc.), or a URL to a file, or the actual source code which
;;;     would comprise the file.  If the body simply contains a
;;;     software ID (sid), makes a copy of the software object.
;;;     All JSON fields which are not [path, url, code,
;;;     software-id] are passed as keyword parameters to
;;;     (MAKE-INSTANCE '<software-type> &key).  Returns a software
;;;     object ID of newly created software.
;;;  GET
;;;     @code{<service-base>/soft?cid=<client-ID>&sid=<software
;;;     ID>} Returns JSON describing software object (differs
;;;     depending on type of software).
;;;  GET
;;;     @code{<service-base>/soft?type=<software type>} Returns
;;;     IDs of live software objects of the passed type, owned by
;;;     the client.
;;;  GET
;;;     @code{<service-base>/soft} Return IDs of all live
;;;     software objects owned by the client.
;;;  PUT
;;;     @code{<service-base>/soft?sid=<software ID>} Update a
;;;     software object.  Body (JSON) contains slots to update, new
;;;     values.
;;;  DELETE
;;;     @code{<service-base>/soft?sid=<software ID>} Delete a
;;;     software object.  (work in progress)
;;;
;;; Population:
;;;
;;;  POST
;;;     @code{<service-base>/population?name=<population-name>}
;;;     Creates a Population resource, owned by the client,
;;;     containing software objects of the specified type.  Body
;;;     should contain, JSON format, list of Software-ID (sid) to
;;;     include in population, and software type.  Returns the ID
;;;     of newly created Population (pid).
;;;  PUT
;;;     @code{<service-base>/pop?pid=<Population-ID>} Adds the
;;;     specified sid(s) to the population.  Body (JSON) contains
;;;     sid field, which is a list of sids to add.
;;;  GET
;;;     @code{<service-base>/pop?pid=<Population-ID>} Retrieves
;;;     information about the population, including list of
;;;     Software-IDs and software type.
;;;  DELETE
;;;     @code{<service-base>/pop?pid=<Population-ID>} Delete
;;;     the population.  (work in progress)
;;;
;;; Mutation:
;;;  POST
;;;     @{<service-base>/mut?type=<mutation-type>&sid=<software-id>}
;;;     Body (JSON) contains targets field (integer, list, or ast).
;;;     Returns mutation-id (mid).
;;;  GET
;;;     @code{<service-base>/mut?mid=<mutation-id>} Returns
;;;     mutation details.
;;;  PUT
;;;     @code{<service-base>/mut?mid=<mutation-id>} Apply
;;;     mutation to its target software object, returns the sid of
;;;     the new (mutated) software object.
;;;  DELETE
;;;     @code{service-base>/mut?mid=<mutation-id>}
;;;     Delete the specified mutation. (work in progress)
;;;
;;; Test Suites:
;;;
;;;  POST
;;;     @code{<service-base>/tests} Creates a TEST-SUITE
;;;     instance, owned by the client, containing a collection of
;;;     TEST-SUITE objects.  Body should contain "test" field, JSON
;;;     format, array of structures, each containing program-name
;;;     and program-args.  Returns oid of newly created tests
;;;     suite.
;;;  GET
;;;     @code{<service-base>/tests?oid=<test-suite-oid>}
;;;     Retrieves collection of test cases in the test-suite.
;;;  DELETE
;;;     @code{<service-base>/tests?pid=<test-suite-oid>} Delete
;;;     the test suite.  (work in progress)
;;;
;;; Instrumented:
;;;
;;;  POST
;;;     @code{<service-base>/instrumented?sid=<software-oid>}
;;;     Creates an Instrumented Software object instance,
;;;     owned by the client
;;;  GET
;;;     @code{<service-base>/instrumented?sid=<software-oid>}
;;;     If sid is unspecified, retrieves the instrumented software
;;;     objects owned by the client. If sid is supplied,
;;;     returns the details of the specified instrumented software object.
;;;
;;; Traced Software:
;;;
;;;  POST
;;;     @code{<service-base>/tracesoft?sid=<software-oid>&tests-oid=<tests oid>}
;;;     Traces the specified software object, using the specified tests.
;;;     Returns the oid specified (does not create a new, distinct software
;;;     object).
;;;
;;; Write Software:
;;;
;;;  POST
;;;     @code{<service-base>/writesoft?sid=<software-oid>}
;;;     Writes the specified software object to a file (TO-FILE).
;;;     Returns success or error code.
;;;     Body contains path specification.
;;;
;;; Async-Job:
;;;
;;;  POST
;;;     @code{<service-base>/async?type=<job-type>} Body
;;;     contains (JSON) parameters for EVOLVE task, FITNESS-TEST or
;;;     other defined type of task.  Returns immediately with a
;;;     Job-ID (jid).  Creating a Job starts a task (on a new
;;;     thread) which will execute until stopped or completed.
;;;  GET
;;;     @code{<service-base>/async?jid=<Job-ID>} Returns JSON
;;;     containing job status and results.
;;;  PUT
;;;     @code{<service-base>/async?jid=<Job-ID>&<update-vars>}
;;;     Allows some control of the task, such as stopping the task.
;;;
;;; @texi{rest}
(defpackage :software-evolution-library/rest
  (:nicknames :sel/rest)
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
           :session-property
           :start-server
           :stop-server
           :define-async-job
           :apply-async-job-func))
(in-package :software-evolution-library/rest)
(in-readtable :curry-compose-reader-macros)

(setf snooze::*catch-http-conditions* nil)
(setf snooze::*catch-errors* :verbose)

(defvar *session-id-generator* 1000
  "Start session ids at 1001 and increment")

(defvar *session-pool* (make-hash-table :test 'equal)
  "Keep a collection of live session objects")

(defvar *server* nil)
(defvar *default-rest-port* 9003)

(defun start-server (&optional (port *default-rest-port*))
  (if *server*
      (stop-server))
  (setf *server* (clack:clackup (snooze:make-clack-app) :port port)))

(defun stop-server ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)))

(defun lookup-session (cid)
  (if (symbolp cid)
      (setf cid (string-downcase (symbol-name cid))))
  (gethash cid *session-pool*))

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
   (software
    :initarg :software
    :accessor session-software
    :initform '())
   (mutations
    :initarg :mutations
    :accessor session-mutations
    :initform '())
   (populations
    :initarg :populations
    :accessor session-populations
    :initform nil)
   (test-suites
    :initarg :test-suites
    :accessor session-test-suites
    :initform nil)
   (properties  ; allow other modules (which use sel) to add properties
    :initarg :properties
    :accessor session-properties
    :initform (make-hash-table :test 'equalp))
   (trace-results
    :initarg :trace-results
    :accessor session-trace-results
    :initform nil)
   (jobs
    :initarg :jobs
    :accessor session-jobs
    :initform nil))

  (:documentation "Client session object"))

(defun session-property (session name)
  (gethash name (session-properties session)))

(defun (setf session-property) (value session name)
  (setf (gethash name (session-properties session)) value))

(defroute
    client (:post "application/json")
  (let* ((json (handler-case
                   (json:decode-json-from-string (payload-as-string))
                 (error (e)
                   (http-condition 400 "Malformed JSON (~a)!" e))))
         (max-population-size (cdr (assoc :max-population-size json)))
         (cross-chance (cdr (assoc :cross-chance json)))
         (mutation-rate (cdr (assoc :mut-rate json)))
         (session-obj
          (make-instance 'session
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

(defun convert-symbol (string)
  "If a string contains '::' then convert it to a symbol if possible."
  (if (and (stringp string)(search "::" string))
      (let ((sym (read-from-string string)))
        (if (symbolp sym)
            sym
            string))
      string))

;;; Main REST server Command

(define-command rest-server
    (port &spec +common-command-line-options+ &aux handler)
  "Run the SEL rest server."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose load eval out-dir read-seed save-seed))
  (flet ((shutdown (&optional (message "quit") (errno 0))
           (format t "Stopping server on ~a~%" message)
           (stop handler)
           (exit-command rest-server errno)))
    (when help (show-help-for-rest-server) (exit-command rest-server 0))
    (setf handler (clackup (make-clack-app) :port (parse-integer port)))
    ;; From https://github.com/LispCookbook/cl-cookbook/blob/master/scripting.md
    (handler-case
        (iter (for char = (read-char))
              (when (or (eql char #\q) (eql char #\Q))
                (shutdown)))
      ;; Catch a user's C-c
      (#+sbcl sb-sys:interactive-interrupt
        #+ccl  ccl:interrupt-signal-condition
        #+clisp system::simple-interrupt-condition
        #+ecl ext:interactive-interrupt
        #+allegro excl:interrupt-signal
        () (shutdown "abort" 0))
      (error (e)
        (shutdown (format nil "unexpected error ~S" e) 1)))))

;;; Catch-all explainer for any error that gets sent back
(defmethod explain-condition ((error error) (resource t)
                              (ct snooze-types:text/plain))
  (with-output-to-string (s)
    (format s
            "Error occurred processing ~A resource object via REST API: ~A"
            resource (princ-to-string error))))

;;; Catch-all explainer for any http-condition that gets sent back
(defmethod explain-condition ((condition http-condition) (resource t)
                              (ct snooze-types:text/plain))
  (with-output-to-string (s)
    (format s
            "Error occurred processing ~A resource object via REST API: ~A"
            resource (princ-to-string condition))))

;;; Specific explainer for soft resource (software object)
(defmethod explain-condition ((error error) (resource (eql #'soft))
                              (ct snooze-types:text/plain))
  (with-output-to-string (s)
    (format s
            "Error occurred processing SOFT (Software) object via REST API: ~A"
            (princ-to-string error))))


;;;; REST Routes

;;;; Software Routes
(defroute
    soft (:post "application/json" &key cid (sid nil) (type nil))
  (declare (ignore sid))
  (let ((json (handler-case
                  (json:decode-json-from-string (payload-as-string))
                (error (e)
                  (http-condition 400 "Malformed JSON (~a)!" e)))))
    (handler-case
        (let* ((client (lookup-session cid))
               (path (cdr (assoc :path json)))
               (url (cdr (assoc :url json)))
               (code (cdr (assoc :code json)))
               (software-type (convert-symbol type))) ;conv. string to symbol
          (declare (ignore url code)) ; not implemented yet
          (when path
            (let ((software
                   (from-file
                    (apply 'make-instance
                           software-type
                           (iter (for x in json)
                                 (unless
                                     (member (car x)
                                             '(:path :project-dir
                                               :url :code :software-id))
                                   (collect (car x))
                                   (collect (convert-symbol (cdr x))))))
                    path)))
              ;; store the software obj with the session
              (push (format-genome software) (session-software client))
              (format nil "~D" (sel::oid software)))))
      (error (e)
        (http-condition 400 "Error in software POST method (~a)!" e)))))

(defun find-software (client sid)
  "Return the population from the client record (if found)."
  (car (member sid (session-software client)
               :key 'sel::oid :test 'eql)))

(defun get-software (cid sid type)
  (let* ((result "{ \"error\": \"Nothing\"}")
         (client (lookup-session cid)))
    (if client
        (let ((software (find-software client sid)))
          (if software
              (setf result
                    (json:encode-json-plist-to-string
                     (list
                      :oid (sel::oid software)
                      :class (format nil "~A"
                                     (class-name (class-of software)))
                      :size (format nil "~D" (size software))
                      :fitness (fitness software)
                      :instrumented (instrumented-p software))))
              (let ((ids (iter (for x in (session-software client))
                               (if (or (null type)
                                       (eq (class-name (class-of x)) type))
                                   (collect (sel::oid x))))))
                (setf result
                      (json:encode-json-to-string ids))))))

    result))

(defroute
    soft (:get :text/* &key cid sid (type nil))
  (get-software cid sid type))

(defroute
    soft (:get "application/json" &key cid sid (type nil))
  (get-software cid sid type))

;;;; Population Routes
(defclass population ()
  ((name
    :initarg :name
    :accessor population-name
    :initform (symbol-name (gensym "POP-"))
    :documentation "Unique name/id for the population")
   (individuals
    :initarg :individuals
    :accessor population-individuals
    :initform nil)
   (type
    :initarg :type
    :accessor population-type
    :initform nil))
  (:documentation "Collection of Software"))

(defun find-population (client pop-name)
  "Return the population from the client record (if found)."
  (car (member pop-name (session-populations client)
               :key 'population-name :test 'equal)))
(defun population-size (population)
  "Return the number of software objects in the population."
  (length (population-individuals population)))

(defroute
    population (:post "application/json" &key cid name)
  (let* ((json (handler-case
                   (json:decode-json-from-string (payload-as-string))
                 (error (e)
                   (http-condition 400 "Malformed JSON (~a)!" e))))
         (client (lookup-session cid))
         (type (intern (string-upcase (cdr (assoc :type json))) :sel))
         (sids (cdr (assoc :sids json)))
         (population
          (apply 'make-instance
                 'population
                 :type type
                 :individuals (mapcar
                               (lambda (sid) (find-software client sid))
                               sids)
                 (if name (list ':name name)))))
    ;; store the software obj with the session
    (push population (session-populations client))
    (population-name population)))

(defun format-population-as-json (population)
  (json:encode-json-plist-to-string
   (list
    :name (population-name population)
    :type (symbol-name (population-type population))
    :size (population-size population)
    :sids (mapcar 'sel::oid (population-individuals population)))))

(defun get-population (cid name)
  (let* ((client (lookup-session cid)))
    (if client
        (let ((population (and name (find-population client name))))
          (if (null population)
              (let ((pop-names (iter (for x in (session-populations client))
                                     (collect (population-name x)))))
                (json:encode-json-to-string pop-names))
              (format-population-as-json population))))))

(defun add-population (client population sids)
  (iter (for sid in sids)
        (let ((ind (find-software client sid)))
          (if ind
              (push ind (population-individuals population))))))

(defroute
    population (:get :text/* &key cid name)
  (get-population cid name))

(defroute
    population (:get "application/json" &key cid name)
  (get-population cid name))

(defroute
    population (:put "application/json" &key cid name)
  (let* ((json (handler-case
                   (json:decode-json-from-string (payload-as-string))
                 (error (e)
                   (http-condition 400 "Malformed JSON (~a)!" e))))
         (sids (cdr (assoc :sids json)))
         (client (lookup-session cid))
         (population (if name (find-population client name))))
    (when population
      (add-population client population sids)
      (format-population-as-json population))))

;;;; Mutation Routes

(defun lookup-resource (client str)
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
                    (member id (session-property client res)
                            :key 'sel::oid :test 'eql))))))))

(defroute
    mut (:post "application/json" &key cid mid)
  (declare (ignore mid))
  (let* ((json (handler-case
                   (json:decode-json-from-string (payload-as-string))
                 (error (e)
                   (http-condition 400 "Malformed JSON (~a)!" e))))
         (client (lookup-session cid))
         (type (cdr (assoc :type json)))
         (type-sym (and type (convert-symbol type)))
         (sid (cdr (assoc :sid json)))
         (software (find-software client sid))
         (properties (iter (for x in json)
                           (unless
                               (member (car x)
                                       '(:type :sid :targets))
                             (collect (car x))
                             (collect
                              (or (lookup-resource  client (cdr x))
                                  (convert-symbol (cdr x)))))))
         (targets (cdr (assoc :targets json))))
    (if type
        (let ((mutation
               (apply 'make-instance
                      type-sym
                      :object software
                      :targets targets
                      properties)))

          ;; store the software obj with the session
          (push mutation (session-mutations client))
          (format nil "~D" (sel::oid mutation))))))

(defun format-mutation-as-json (mutation)
  (json:encode-json-plist-to-string
   (apply 'list
          :id (sel::oid mutation)
          :type (symbol-name (class-name (class-of mutation)))
          :sid (and (object mutation) (sel::oid (object mutation)))
          :software-type (symbol-name (class-name (class-of (object mutation))))
          :targets (targets mutation))))

(defun find-mutation (client mid)
  "Return the mutation from the client record (if found)."
  (car (member mid (session-mutations client)
               :key 'sel::oid :test 'eql)))

(defun get-mutation (cid mid)
  (let* ((client (lookup-session cid)))
    (if client
        (let ((mutation (and mid (find-mutation client mid))))
          (if (null mutation)
              (let ((mids (iter (for x in (session-mutations client))
                                (collect (sel::oid x)))))
                (json:encode-json-to-string mids))
              (format-mutation-as-json mutation))))))

(defroute
    mut (:get :text/* &key cid mid)
  (get-mutation cid mid))

(defroute
    mut (:get "application/json" &key cid mid)
  (get-mutation cid mid))

;;;; Test and Test Suite Routes

(defun find-test-suite (client oid)
  "Return the test-suite from the client record with specified oid (if found)."
  (car (member oid (session-test-suites client)
               :key 'sel::oid :test 'eql)))

(defroute
    tests (:post "application/json" &key cid oid)
  (declare (ignore oid))
  (let* ((json (handler-case
                   (json:decode-json-from-string (payload-as-string))
                 (error (e)
                   (http-condition 400 "Malformed JSON (~a)!" e))))
         (client (lookup-session cid))
         (tests (cdr (assoc :tests json)))
         (test-suite
          (funcall 'make-instance
                   'test-suite
                   :test-cases
                   (mapcar
                    (lambda (test)
                      (let ((program-name (cdr (assoc :program-name test)))
                            (program-args (cdr (assoc :program-args test))))
                        (make-instance 'test-case :program-name program-name
                                       :program-args
                                       (mapcar (lambda (x)
                                                 (if (or (equal x ":BIN")
                                                         (equal x "BIN"))
                                                     :bin
                                                     x))
                                               program-args))))
                    tests))))
    ;; store the test-suite obj with the session
    (push test-suite (session-test-suites client))
    (format nil "~D" (sel::oid test-suite)))) ;; return the oid

(defun format-test-as-json (test)
  (json:encode-json-plist-to-string
   (list
    :program-name (program-name test)
    :program-args (program-args test))))

(defun format-test-suite-as-json (test-suite)
  (json:encode-json-plist-to-string
   (list
    :oid (sel::oid test-suite)
    :test-cases (mapcar 'format-test-as-json (test-cases test-suite)))))

(defun get-test-suite (cid oid)
  (let* ((client (lookup-session cid)))
    (if client
        (let ((test-suite (and oid (find-test-suite client oid))))
          (if (null test-suite)
              (let ((test-suite-oids
                     (iter (for x in (session-test-suites client))
                           (collect (sel::oid x)))))
                (json:encode-json-to-string test-suite-oids))
              (format-test-suite-as-json test-suite))))))

(defroute
    tests (:get :text/* &key cid oid)
  (get-test-suite cid oid))

(defroute
    tests (:get "application/json" &key cid oid)
  (get-test-suite cid oid))

;;;; Instrumentated Program Routes

(defroute instrumented (:post "application/json" &key cid sid)
  (handler-case
      (let* ((client (lookup-session cid))
             (soft (find-software client sid)))
        (let ((inst-soft
               (instrument
                (copy soft)
                :functions
                (list
                 (lambda (instrumenter ast)
                   (var-instrument
                    {get-vars-in-scope (software instrumenter)}
                    instrumenter
                    ast)))
                :filter #'sel/sw/parseable::traceable-stmt-p)))
          ;; store the software obj with the session
          (push inst-soft (session-software client))
          (format nil "~D" (sel::oid inst-soft))))
    (error (e)
      (http-condition 400 "Error in INSTRUMENTED POST method (~a)!" e))))


(defun get-instrumented (cid sid type)
  (let* ((result "{ \"error\": \"Nothing\"}")
         (client (lookup-session cid)))
    (if client
        (let ((software (find-software client sid)))
          (if software
              (setf result
                    (json:encode-json-plist-to-string
                     (list
                      :oid (sel::oid software)
                      :class (format nil "~A"
                                     (class-name (class-of software)))
                      :size (format nil "~D" (size software))
                      :fitness (fitness software)
                      :instrumented (instrumented-p software))))
              (let ((ids (iter (for x in (session-software client))
                               (if (and
                                    (or (null type)
                                        (eq (class-name (class-of x)) type))
                                    (ignore-errors (instrumented-p x)))
                                   (collect (sel::oid x))))))
                (setf result
                      (json:encode-json-to-string ids))))))

    result))

(defroute
    instrumented (:get :text/* &key cid sid (type nil))
  (get-instrumented cid sid type))

(defroute
    instrumented (:get "application/json" &key cid sid (type nil))
  (get-instrumented cid sid type))

;;;
;;; Pulled this from bi/utility
;;;
(defmacro with-trace-error-handling (&rest body)
  "FIXME

* BODY FIXME
"
  `(handler-bind ((end-of-file
                   (lambda (c)
                     (declare (ignorable c))
                     (invoke-restart 'sel::ignore-rest-of-stream)))
                  (trace-error (lambda (c)
                                 (declare (ignorable c))
                                 (cond
                                   ((find-restart 'sel::ignore-empty-trace)
                                    (invoke-restart 'sel::ignore-empty-trace))
                                   ((find-restart 'sel::nil-traces)
                                    (invoke-restart 'sel::nil-traces))
                                   ((find-restart 'sel::skip-test-case)
                                    (invoke-restart 'sel::skip-test-case))))))
     (progn ,@body)))

(defroute
    tracesoft (:post "application/json" &key cid sid tests-oid)
  (handler-case
      (let* ((json (handler-case
                       (json:decode-json-from-string (payload-as-string))
                     (error (e)
                       (http-condition 400 "Malformed JSON (~a)!" e))))
             (client (lookup-session cid))
             (soft (find-software client sid))
             (test-suite (find-test-suite client tests-oid))
             (inst-bin (cdr (assoc :inst-bin json))))

        (if (and (typep soft 'sel/sw/clang::clang)
                 (null (sel/software/source::compiler soft)))
            (setf (sel/software/source::compiler soft) "clang"))

        ;; create the binary executable
        (if inst-bin
            (phenome soft :bin inst-bin))

        (with-trace-error-handling
            (with-temp-dir-of (temp)
              (make-pathname :directory (pathname-directory inst-bin))
              (with-cwd (temp)
                (apply 'collect-traces soft test-suite
                       :max nil
                       (if inst-bin (list :bin inst-bin))))))
        (format nil "~D" (sel::oid soft)))
    (error (e)
      (http-condition 400 "Error in TRACESOFT POST method (~a)!" e))))

;;;
;;; write the contents of a software object
;;;
(defroute
    writesoft (:post "application/json" &key cid sid)
  (handler-case
      (let* ((json (handler-case
                       (json:decode-json-from-string (payload-as-string))
                     (error (e)
                       (http-condition 400 "Malformed JSON (~a)!" e))))
             (client (lookup-session cid))
             (soft (find-software client sid))
             (path (cdr (assoc :path json))))

        (to-file (format-genome (uninstrument (copy soft)))
                 path)
        #(format nil "Software ID ~D successfully written to ~A" sid path)
        (format nil "~D" sid))

    (error (e)
      (http-condition 400
                      "Error in WRITESOFT POST method (~a)!" e))))

;;; Asynchronous Jobs

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct async-job-type
    "Entry in async-job-type table."
    job-name        ; Symbol names the job type.
    arg-names       ; Lambda-list for user-defined function.
    arg-types func) ; Type of each argument
                                        ; :LAMBDA-LIST-KEYWORD pseudo-type is used
                                        ; for lambda list keywords.

  (defvar *async-job-types* (make-hash-table :test 'equalp)
    "Registry of async-job-types.")

  (defun register-async-job (job-name arg-names arg-types func)
    "Register an async-job-type."
    (setf (gethash job-name *async-job-types*)
          (make-async-job-type :job-name job-name :arg-names arg-names
                               :arg-types arg-types :func func))))

;;; The lambda-list should use (<name> <type>) for each variable name.
;;; The type can be any of: integer, float, string, or boolean.
;;;
;;; If body consists of special case (:function <function>) then
;;; <function> is assumed to be a function (or symbol bound to
;;; a function) with a lambda-list congruent with the specification
;;; of DEFINE-ASYNC-JOB. In this case that function will be called
;;; rather than a new function created.

(defmacro define-async-job
    (name lambda-list &rest body)
  "Define an async-job type."
  (let* ((arg-names (mapcar
                     (lambda (x)
                       (if (member x lambda-list-keywords)
                           x
                           (first x)))
                     lambda-list))
         (arg-types (mapcar
                     (lambda (x)
                       (if (member x lambda-list-keywords)
                           :lambda-list-keyword
                           (second x)))
                     lambda-list))
         (func (if (and (listp body)
                        (= (length body) 2)
                        (eq (first body) ':function))
                   `',(second body)
                   `(lambda (,@arg-names) ,@body))))

    `(progn
       (register-async-job
        ',name
        ',arg-names
        ',arg-types
        (lambda (arguments)
          (apply ,func arguments)))
       ',name)))

(defun lookup-async-job-type (name)
  "Given a name, lookup the async-job-type registry entry."
  (values (gethash name *async-job-types*)))

(defun apply-async-job-func (name &rest args)
  "Given a name and arguments, call the async-job-type function."
  (funcall (async-job-type-func (gethash name *async-job-types*)) args))

(defclass async-job ()
  ((name
    :initarg :name
    :accessor async-job-name
    :initform (symbol-name (gensym "JOB-"))
    :documentation "Unique name/id for the job")
   (population
    :initarg :population
    :accessor async-job-population
    :initform nil)
   (func   ;; function to run on each software object in population
    ;; e.g. EVALUATE, TEST-FITNESS, etc.
    :initarg :func
    :accessor async-job-func
    :initform nil)
   (task-runner
    :initarg :task-runner
    :accessor async-job-task-runner
    :initform nil))
  (:documentation "Task to perform asynchronously, against every item
in a population"))

(defun find-job (client job-name)
  "Return the named job from the client record (if found)."
  (car (member job-name (session-jobs client)
               :key 'async-job-name :test 'equal)))

(defun format-job-as-json (async-job)
  (let ((task-runner (async-job-task-runner async-job)))
    (json:encode-json-plist-to-string
     (list
      :name (async-job-name async-job)
      :threads-running
      (task-runner-workers-count (async-job-task-runner async-job))
      :remaining-jobs
      (task-runner-remaining-jobs (async-job-task-runner async-job))
      :population (async-job-population async-job)
      :completed-tasks (task-runner-completed-tasks task-runner)
      :results (task-runner-results task-runner)))))

(defun get-job (cid name)
  (let* ((client (lookup-session cid)))
    (if client
        (let ((job (and name (find-job client name))))
          (if (null job)
              (let ((job-names (iter (for x in (session-jobs client))
                                     (collect (async-job-name x)))))
                (json:encode-json-to-string job-names))
              (format-job-as-json job))))))

(defun lookup-job-type-entry (name)
  "Allow some special-case names, otherwise fall through to symbol
 in SEL package by the specified name."
  (let* ((sym (convert-symbol name)))
    (lookup-async-job-type sym)))

(defun lookup-job-func (name)
  "Allow some special-case names, otherwise fall through to symbol
 in SEL package by the specified name."
  (let* ((entry (lookup-job-type-entry name)))
    (if entry (async-job-type-func entry))))

(defun type-check (population job-type-entry)
  (declare (ignore job-type-entry)) ; use this later
  (mapcar (lambda (x)
            (mapcar (lambda (y) (convert-symbol y)) x))
          population))

(defroute
    async (:post "application/json" &key cid name)
  (let* ((json (handler-case
                   (json:decode-json-from-string (payload-as-string))
                 (error (e)
                   (http-condition 400 "Malformed JSON (~a)!" e))))
         (client (lookup-session cid))
         (pid (cdr (assoc :pid json)))  ; name/id of population
         (population
          (if pid
              (find-population client pid) ; pid specifies a population
              (cdr (assoc :population json)))) ; else assume a list
         (func-name (cdr (assoc :func json)))
         (func (lookup-job-func func-name))
                                        ; name of function to run
         (threads (cdr (assoc :threads json))) ; max number of threads to use
                                        ; must be at least 1 thread!
         (job (apply 'make-instance 'async-job
                     :population population
                     :func func
                     :task-runner
                     (sel/utility::task-map-async
                      threads
                      func
                      (if (typep population 'population)
                          (population-individuals population)
                          (type-check population
                                      (lookup-job-type-entry func-name))))
                     (if name (list :name name)))))
    ;; store the software obj with the session
    (push job (session-jobs client))
    (async-job-name job)))

(defroute
    async (:get :text/* &key cid name)
  (get-job cid name))

(defroute
    async (:get "application/json" &key cid name)
  (get-job cid name))


;;; WIP
;;; WIP
;;; WIP
;;;
;;; Endpoint definitions macros for `define-command-rest`.
;;;
;;; The required arguments are of the form (<name> type>), as:
;;;
;;; ((source string) (num-tests integer))
;;;
;;; The type can be any of integer, float, string, or boolean.
;;;
;;; Optional arguments are expected to be of the form of arguments in
;;; command-line.lisp, e.g.:
;;;
;;; (("help" #\h #\?)
;;;  :TYPE BOOLEAN
;;;  :OPTIONAL T
;;;  :DOCUMENTATION "display help output")
;;;
;;; This should work:
#|
> (define-endpoint-route addfive (lambda (value) (+ value 5)) ((value integer)) ())
. . .
~> curl -X POST -H "Accept: application/json" \
-H "Content-Type: application/json" \
http://127.0.0.1:9003/getfive?cid='client-1001' \
-d "{}"

TODO
- Rename `population` to `args` or `arguments` in `async-job`.
- Ideally the thing should work without an empty json field, but
payload-as-string gets angry in that case. Maybe we can find a workaroud...
- Ensure the requires arguments work correctly
- Write a parser / handler for optional arguments
- Incorporate into define-command-rest
- Write test cases
|#
(defun lookup-main-args
    (args json)
  (mapcar  (lambda (argument)
             (if-let ((result (aget (car argument) json :test #'string=)))
               (if (typep result (cdr argument))
                   result
                   (error "Invalid type"))
               (error "Did not find required argument ~a" (car argument))))
           args))

                                        ; (defmacro def-task-async ((&rest lambda-list) (&rest args) &rest body)
                                        ;   `(task-map-async
                                        ;     1
                                        ;     (lambda (arg-list) (apply (lambda ,lambda-list ,@body) arg-list))
                                        ;     (list (list ,@args))))

                                        ; (def-task-async (x y) (10 20) (format t "~a + ~a: ~a~%" x y (+ x y)))

(defmacro define-endpoint-route
    (name func required-args optional-args &rest body)
  `(progn
     (let* ((main-args (mapcar (lambda (arg) (cons (string (car arg)) (cadr arg)))
                               ',required-args))
            ;; (optional-args (create-optional-json-bindings optional-args))
            )
       (defroute
           ,name (:post "application/json" &key cid)
         (let* ((json (handler-case
                          (if-let ((payload (payload-as-string)))
                            (mapcar (lambda (entry) (cons (string (car entry)) (cdr entry)))
                                    (json:decode-json-from-string payload))
                            '())
                        (error (e)
                          (http-condition 400 "Malformed JSON (~a)!" e))))
                (client (lookup-session cid))
                (_ (note 0 "~a" json))
                (first-args (lookup-main-args main-args json))
                (_ (note 0 "~a" first-args))
                ;; (rest-args (lookup-optional-args optional-args json))
                (args first-args)
                (lookup-fn (lookup-job-func ,func))
                (threads 1)
                (job (apply 'make-instance 'async-job
                            :func ,func
                            :population args
                            :task-runner
                            (sel/utility::task-map-async
                             threads
                             (if lookup-fn lookup-fn ,func)
                             args)
                            (list :name (symbol-name (gensym (string ',name)))))))
           ;; store the job with the session
           (push job (session-jobs client))
           ;; return the job name
           (async-job-name job))))
     (defroute
         ,name (:get :text/* &key cid name)
       (get-job cid name))
     (defroute
         ,name (:get "application/json" &key cid name)
       (progn
         (format t "~a ~a" cid name)
         (get-job cid name)))))

