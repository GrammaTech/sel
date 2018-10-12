;;;
;;; rest-api.lisp --- RESTful interface over SEL
;;;
;;; @subsection REST API overview
;;;

;; you'll need to quickload :cl-json, :snooze
;;;
;;; To run the server:
;;; (setf *handler* (clack:clackup (snooze:make-clack-app) :port 9003))
;;; to stop server: (clack:stop *handler*)
;;;

(require :snooze)

(defpackage :software-evolution-library/rest
  (:nicknames :sel/rest)
  (:use
   :common-lisp
   :snooze
   :sel
   :sel/utility
   :iterate))

(in-package :sel/rest)

#|
Client
    POST <service-base>/sel/client
         Create a new client.
	 Body (JSON) contains initial values for special variable settings.
         Returns the Client-ID.
|#

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

#|
Software
    POST <service-base>/sel/soft?cid=<client-ID>&type=<software-type>
         Body, JSON format, contains path (to C file, AST, Asm file, 
         etc.), or a URL to a file, or the actual source code which would
         comprise the file.
	 If the body simply contains a software ID, makes a copy
	 of the software object.
         All JSON fields which are not [path, url, code, software-id]
         are passed as keyword parameters to 
         (MAKE-INSTANCE '<software-type> &key).
	 Returns a software object ID of newly created software.
    GET  <service-base>/sel/software/<Software-ID>?cid=<client-ID>
         Returns JSON describing software object 
         (differs depending on type of software)
    GET  <service-base>/sel/software/<type>
         Returns IDs of live software objects of the passed type, 
         owned by the client
    GET  <service-base>/sel/software/
         Return IDs of all live software objects owned by the client
    PUT  <service-base>/sel/software/<Software-ID>?<slots-to-update+new-values>
         Update a software object
    DELETE <service-base>/sel/software/<Software-ID>
         Delete a software object
|#

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
	   (code (cdr (assoc :code json))))
      (declare (ignore url code)) ; not implemented yet
      (if path
	  (let ((software
		 (from-file
		  (apply 'make-instance
			 type
			 (iter (for x in json)
			       (unless
				   (member (car x)
					   '(:path :url :code :software-id))
				 (collect (car x))
				 (collect (cdr x)))))
		  path)))
	    ;; store the software obj with the session
	    (push software (session-software client))
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
		      :fitness (fitness software))))
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

#|
Population
    POST <service-base>/sel/population?name=<population-name>
         Creates a Population resource, owned by the client, 
         containing software objects of the specified type.
         Name may be supplied, or else one will be generated.
	 Body should contain, JSON format, list of Software-ID (sid) to 
         include in population, and software type.
         Returns the name of newly created Population (pid).
    PUT  <service-base>/sel/population?pid=<Population-ID>
         Adds the specified sid(s) to the population.
	 Body (JSON) contains sid field, which is a list of sids to add.
    GET  <service-base>/sel/population?pid=<Population-ID>
         Retrieves information about the population, including list of 
         Software-IDs.
    DELETE <service-base>/sel/population?pid=<Population-ID>
         Delete the population
|#

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

#|
Mutation
    POST service-base>/sel/mut?sid=<software-id>
         Body (JSON) contains targets field (integer, list, or ast),
         mutation-type and sid (software id).
	 Returns mutation-id (mid).
    GET  service-base>/sel/mut?mid=<mutation-id>
    PUT  service-base>/sel/mut?mid=<mutation-id>
         Apply mutation to its target software object, returns the sid
	 of the new (mutated) software object.
    DELETE service-base>/sel/mut?mid=<mutation-id>
         Delete the specified mutation.
|#
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

#|
Async-Job
    POST <service-base>/sel/async?type=<job-type>
         Body contains (JSON) parameters for EVOLVE task, FITNESS-TEST or other
	 defined type of task.
	 Returns immediately with a Job-ID (jid).
	 Creating a Job starts a task (on a new thread) which will execute until
	 stopped or completed.
    GET  <service-base>/sel/async?jid=<Job-ID>
         Returns JSON containing job status and results.
    PUT  <service-base>/sel/async/<Job-ID>&<update-vars>
         Allows some control of the task, such as stopping the task. 
|#

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
	     (subtypep (population-type population) 'sel::asm-super-mutant))
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

#|
TESTS
    POST <service-base>/sel/tests
         Creates a TEST-SUITE instance, owned by the client, 
         containing a collection of TEST-SUITE objects.
	 Body should contain "test" field, JSON format, array of structures, 
         each containing program-name and program-args.
         Returns oid of newly created tests suite.
    GET  <service-base>/sel/tests?oid=<test-suite-oid>
         Retrieves collection of test cases in the test-suite.
    DELETE <service-base>/tests?pid=<test-suite-oid>
         Delete the test suite
|#
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

