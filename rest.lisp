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
;;;     (setf *handler* (clack:clackup (snooze:make-clack-app) :port 9003))
;;;
;;; Stopping the server:
;;;
;;;     (clack:stop *handler*)
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
;;;  Directories
;;;     Lists (dictionaries) of sel-supported options software types,
;;;     with meta-data for each type to support create/update mutation
;;;     types.
;;;
;;;  Client sessions
;;;     establish ownership of a jobs, software objects, populations, etc.
;;;
;;;  Test Suites
;;;     Lists of Test Cases, which each include references to an
;;;     executable file and some command-line argumants. These are
;;;     used to evaluate Software objects.
;;;
;;;  Scions
;;;     Patterns defined by Bug-Injector to describe software bugs
;;;     (this is a work still in progress)
;;;
;;;  Traces
;;;     Models and manages the data collected from running test suites against
;;;     instrumented Software objects. (work in progress)
;;;
;;;
;;; @subsubsection Operations on resources
;;;
;;; Note: all operations (other than session create) require a client-ID
;;; parameter. Although only specified in the first ones below, all others
;;; require it as well.
;;;
;;; Client:
;;;
;;;  POST
;;;     @code{<service-base>/sel/client} Create a new client.  Body
;;;     (JSON) contains initial values for special variable settings.
;;;     Returns the Client-ID.
;;;  DELETE
;;;     @code{<service-base>/sel/client&cid=<client-ID>} Delete
;;;     the client session, and cause all the software objects
;;;     owned by the client to become garbage.
;;;
;;; Software:
;;;
;;;  POST
;;;     @code{<service-base>/sel/soft?cid=<client-ID>&type=<software-type>}
;;;     Body, JSON format, contains path (to C file, AST, Asm file,
;;;     etc.), or a URL to a file, or the actual source code which
;;;     would comprise the file.  If the body simply contains a
;;;     software ID (sid), makes a copy of the software object.
;;;     All JSON fields which are not [path, url, code,
;;;     software-id] are passed as keyword parameters to
;;;     (MAKE-INSTANCE '<software-type> &key).  Returns a software
;;;     object ID of newly created software.
;;;  GET
;;;     @code{<service-base>/sel/soft?cid=<client-ID>&sid=<software
;;;     ID>} Returns JSON describing software object (differs
;;;     depending on type of software).
;;;  GET
;;;     @code{<service-base>/sel/soft?type=<software type>} Returns
;;;     IDs of live software objects of the passed type, owned by
;;;     the client.
;;;  GET
;;;     @code{<service-base>/sel/soft} Return IDs of all live
;;;     software objects owned by the client.
;;;  PUT
;;;     @code{<service-base>/sel/soft?sid=<software ID>} Update a
;;;     software object.  Body (JSON) contains slots to update, new
;;;     values.
;;;  DELETE
;;;     @code{<service-base>/sel/soft?sid=<software ID>} Delete a
;;;     software object.  (work in progress)
;;;
;;; Population:
;;;
;;;  POST
;;;     @code{<service-base>/sel/population?name=<population-name>}
;;;     Creates a Population resource, owned by the client,
;;;     containing software objects of the specified type.  Body
;;;     should contain, JSON format, list of Software-ID (sid) to
;;;     include in population, and software type.  Returns the ID
;;;     of newly created Population (pid).
;;;  PUT
;;;     @code{<service-base>/sel/pop?pid=<Population-ID>} Adds the
;;;     specified sid(s) to the population.  Body (JSON) contains
;;;     sid field, which is a list of sids to add.
;;;  GET
;;;     @code{<service-base>/sel/pop?pid=<Population-ID>} Retrieves
;;;     information about the population, including list of
;;;     Software-IDs and software type.
;;;  DELETE
;;;     @code{<service-base>/sel/pop?pid=<Population-ID>} Delete
;;;     the population.  (work in progress)
;;;
;;; Async-Job:
;;;
;;;  POST
;;;     @code{<service-base>/sel/async?type=<job-type>} Body
;;;     contains (JSON) parameters for EVOLVE task, FITNESS-TEST or
;;;     other defined type of task.  Returns immediately with a
;;;     Job-ID (jid).  Creating a Job starts a task (on a new
;;;     thread) which will execute until stopped or completed.
;;;  GET
;;;     @code{<service-base>/sel/async?jid=<Job-ID>} Returns JSON
;;;     containing job status and results.
;;;  PUT
;;;     @code{<service-base>/sel/async?jid=<Job-ID>&<update-vars>}
;;;     Allows some control of the task, such as stopping the task.
;;;
;;; Mutation:
;;;
;;;  POST
;;;     @{<service-base>/sel/mut?type=<mutation-type>&sid=<software-id>}
;;;     Body (JSON) contains targets field (integer, list, or ast).
;;;     Returns mutation-id (mid).
;;;  GET
;;;     @code{<service-base>/sel/mut?mid=<mutation-id>} Returns
;;;     mutation details.
;;;  PUT
;;;     @code{<service-base>/sel/mut?mid=<mutation-id>} Apply
;;;     mutation to its target software object, returns the sid of
;;;     the new (mutated) software object.
;;;  DELETE
;;;     @code{service-base>/sel/mut?mid=<mutation-id>}
;;;     Delete the specified mutation. (work in progress)
;;;
;;; Directories:
;;;
;;;  GET
;;;     @code{<service-base>/sel/dir/software} Returns list of types
;;;     of software which may be intantiated.  (work in progress)
;;;
;;; Test Suites:
;;;
;;;  POST
;;;     @code{<service-base>/sel/tests} Creates a TEST-SUITE
;;;     instance, owned by the client, containing a collection of
;;;     TEST-SUITE objects.  Body should contain "test" field, JSON
;;;     format, array of structures, each containing program-name
;;;     and program-args.  Returns oid of newly created tests
;;;     suite.
;;;  GET
;;;     @code{<service-base>/sel/tests?oid=<test-suite-oid>}
;;;     Retrieves collection of test cases in the test-suite.
;;;  DELETE
;;;     @code{<service-base>/tests?pid=<test-suite-oid>} Delete
;;;     the test suite.  (work in progress)
;;;
;;; @texi{rest}
(defpackage :software-evolution-library/rest
  (:nicknames :sel/rest)
  (:use
   :common-lisp
   :snooze
   :cl-json
   :iterate
   :software-evolution-library/software-evolution-library
   :software-evolution-library/utility
   :software-evolution-library/components/test-suite
   ;; TODO: Maybe remove this dependency.
   :software-evolution-library/software/asm-super-mutant
   :software-evolution-library/software/clang)
  (:export :lookup-session))
(in-package :software-evolution-library/rest)

(defvar *session-id-generator* 1000
  "Start session ids at 1001 and increment")

(defvar *session-pool* (make-hash-table :test 'equal)
  "Keep a collection of live session objects")

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
     (jobs
      :initarg :jobs
      :accessor session-jobs
      :initform nil))

  (:documentation "Client session object"))

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

(defroute
    soft (:post "application/json" &key cid (sid nil) (type nil))
    (declare (ignore sid))
    (let* ((json (handler-case
		     (json:decode-json-from-string (payload-as-string))
		   (error (e)
		     (http-condition 400 "Malformed JSON (~a)!" e))))
	   (client (lookup-session cid))
	   (path (cdr (assoc :path json)))
	   (url (cdr (assoc :url json)))
	   (code (cdr (assoc :code json)))
	   (software-type (convert-symbol type))) ; convert string to symbol
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
	    (format nil "~D" (sel::oid software))))))

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

(defroute
    mut (:post "application/json" &key cid mid)
    (declare (ignore mid))
    (let* ((json (handler-case
		     (json:decode-json-from-string (payload-as-string))
		   (error (e)
		     (http-condition 400 "Malformed JSON (~a)!" e))))
	   (client (lookup-session cid))
	   (type (cdr (assoc :type json)))
	   (type-sym (intern (string-upcase type) :sel))
	   (sid (cdr (assoc :sid json)))
	   (software (find-software client sid))
	   (targets (cdr (assoc :targets json))))
      (if type
	  (let ((mutation
		  (funcall 'make-instance
			 type-sym
			 :object software
			 :targets targets)))

	    ;; store the software obj with the session
	    (push mutation (session-mutations client))
	    (format nil "~D" (sel::oid mutation))))))

(defun format-mutation-as-json (mutation)
  (json:encode-json-plist-to-string
   (list
    :id (sel::oid mutation)
    :type (symbol-name (class-name (class-of mutation)))
    :sid (sel::oid (object mutation))
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
      :population-name (population-name (async-job-population async-job))
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

(defun lookup-job-func (population name)
  "Allow some special-case names, otherwise fall through to symbol
 in SEL package by the specificed name."
  (let* ((sym (intern (string-upcase name) :sel))
	 (func (symbol-function sym)))
    (if (and (eq 'sel::evaluate sym)
             ;; TODO: Maybe remove this special case and dependency.
	     (subtypep (population-type population) 'asm-super-mutant))
	    (setf func (lambda (soft) (evaluate nil soft))))
    func))

(defroute
    async (:post "application/json" &key cid name)
    (let* ((json (handler-case
		     (json:decode-json-from-string (payload-as-string))
		   (error (e)
		     (http-condition 400 "Malformed JSON (~a)!" e))))
	   (client (lookup-session cid))
	   (pid (cdr (assoc :pid json)))  ; name/id of population
	   (population (find-population client pid))
	   (func (lookup-job-func population (cdr (assoc :func json))))
					; name of function to run
	   (threads (cdr (assoc :threads json))) ; max number of threads to use
	   (job (apply 'make-instance 'async-job
		       :population population
		       :func func
		       :task-runner (sel/utility::task-map-async
				     threads
				     func
				     (population-individuals population))
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
				       :program-args program-args)))
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

(defroute
    instrumented (:post "application/json" &key cid sid)
    (let* ((client (lookup-session cid))
	   (soft (find-software client sid)))

      (let ((inst-soft (instrument (copy soft))))
	;; store the software obj with the session
	(push inst-soft (session-software client))
	(format nil "~D" (sel::oid inst-soft)))))

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

