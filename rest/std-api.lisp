;;; std-api.lisp --- Standard RESTful interface over SEL.
;;;
;;; Standard Rest API for Software Evolution Library
;;;
;;; The Standard Rest API for Software Evolution Library is implemented as a web
;;; service which may be accessed through HTTP requests.
;;;
;;; It attempts to conform to principals described here:
;;; @uref{https://en.wikipedia.org/wiki/Representational_state_transfer,
;;; Representational State Transfer}
;;;
;;; @subsubsection Dependencies
;;;
;;; The Rest API leverages a number of Common Lisp components, which
;;; are open-source and generally available via Quicklisp.  See the core REST
;;; file for a full description.
;;; @subsubsection Dependencies
;;;
;;; The Rest API leverages a number of Common Lisp components, which
;;; are open-source and generally available via Quicklisp.  See the core REST
;;; file for a full description and how to start a rest server. In addition,
;;; this file is built from the session definitions for REST provided as part
;;; of SEL.
;;;
;;; @subsubsection Resources and Operations
;;;
;;; A standard SEL Rest API supports logical resources. This section lists the
;;; basic logical resources supported by this standard API. In some cases (e.g.
;;; Software), the resource can be thought to be a distillation of the methods
;;; of the matching SEL class. In other cases (e.g. Populations) there isn't a
;;; specific class in SEL, and the REST API resource provides ways to manage and
;;; name these lists of software objects.
;;;
;;; @subsubheading Resources
;;;
;;; The following types of resources are defined by this standard API.
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
;;; @subsubheading Operations on Resources
;;;
;;; Note: all operations (other than session create) require a @code{cid}
;;; (Client ID) integer value representing the Client ID for session lookup.
;;; See the client sessions file for endpoint usage to acquire one.
;;;
;;; Software:
;;;
;;;  POST
;;;     @code{<service-base>/soft?cid=<cid>&type=<software-type>}
;;;     Body, JSON format, contains path (to C file, AST, Asm file,
;;;     etc.), or a URL to a file, or the actual source code which
;;;     would comprise the file.  If the body simply contains a
;;;     software ID (sid), makes a copy of the software object.
;;;     All JSON fields which are not [path, url, code,
;;;     software-id] are passed as keyword parameters to
;;;     (MAKE-INSTANCE '<software-type> &key).  Returns a software
;;;     object ID of newly created software.
;;;  GET
;;;     @code{<service-base>/soft?cid=<cid>&sid=<software
;;;     ID>} Returns JSON describing software object (differs
;;;     depending on type of software).
;;;  GET
;;;     @code{<service-base>/soft?cid=<cid>&type=<software type>} Returns
;;;     IDs of live software objects of the passed type, owned by the client.
;;;  GET
;;;     @code{<service-base>/soft?cid=<cid>} Return IDs of all live
;;;     software objects owned by the client.
;;;  PUT
;;;     @code{<service-base>/soft?cid=<cid>&sid=<software ID>} Update a
;;;     software object.  Body (JSON) contains slots to update, new
;;;     values.
;;;  DELETE
;;;     @code{<service-base>/soft?cid=<cid>&sid=<software ID>} Delete a
;;;     software object. (Currently not supported in full -- work in progress.)
;;;
;;; Population:
;;;
;;;  POST
;;;     @code{<service-base>/cid=<cid>&population?name=<population-name>}
;;;     Creates a Population resource, storing it in the assocaited client
;;;     session, containing software objects of the specified type. The request
;;;     should also contain JSON including:
;;;
;;;     - @code{sid (list)} : list of Software IDs (@code{sid}) to include in
;;;       population
;;;     - @code{type (string)} : software type
;;;
;;;     Returns the ID of newly created Population (pid).
;;;  PUT
;;;     @code{<service-base>/cid=<cid>&pop?pid=<Population-ID>} Adds software to
;;;     the population.
;;;     Takes a JSON request containing:
;;;
;;;     - @code{sid (list)} : list of Software IDs (@code{sid}) to include in
;;;       population
;;;
;;;     It adds the specified sid(s) to the population.
;;;  GET
;;;     @code{<service-base>/cid=<cid>&pop?pid=<Population-ID>} Retrieves
;;;     information about the population, including list of
;;;     Software IDs and software type.
;;;  DELETE
;;;     @code{<service-base>/cid=<cid>&pop?pid=<Population-ID>} Delete
;;;     the population.  (work in progress)
;;;
;;; Mutation:
;;;  POST
;;;     @{<service-base>/cid=<cid>&mut?type=<mutation-type>&sid=<software-id>}
;;;     Body (JSON) contains targets field (integer, list, or ast).
;;;     Returns mutation-id (mid).
;;;  GET
;;;     @code{<service-base>/cid=<cid>&mut?mid=<mutation-id>} Returns
;;;     mutation details.
;;;  PUT
;;;     @code{<service-base>/cid=<cid>&mut?mid=<mutation-id>} Apply
;;;     mutation to its target software object, returns the sid of
;;;     the new (mutated) software object.
;;;  DELETE
;;;     @code{service-base>/cid=<cid>&mut?mid=<mutation-id>}
;;;     Delete the specified mutation. (work in progress)
;;;
;;; Test Suites:
;;;
;;;  POST
;;;     @code{<service-base>/tests?cid=<cid>} Creates a TEST-SUITE
;;;     instance, owned by the client, containing a collection of
;;;     TEST-SUITE objects.  Body should contain "test" field, JSON
;;;     format, array of structures, each containing program-name
;;;     and program-args.  Returns oid of newly created tests
;;;     suite.
;;;  GET
;;;     @code{<service-base>/tests?cid=<cid>&oid=<test-suite-oid>}
;;;     Retrieves collection of test cases in the test-suite.
;;;  DELETE
;;;     @code{<service-base>/tests?cid=<cid>&pid=<test-suite-oid>} Delete
;;;     the test suite.  (work in progress)
;;;
;;; Instrumented:
;;;
;;;  POST
;;;     @code{<service-base>/instrumented?cid=<cid>&sid=<software-oid>}
;;;     Creates an Instrumented Software object instance,
;;;     owned by the client
;;;  GET
;;;     @code{<service-base>/instrumented?cid=<cid>&sid=<software-oid>}
;;;     If sid is unspecified, retrieves the instrumented software
;;;     objects owned by the client. If sid is supplied,
;;;     returns the details of the specified instrumented software object.
;;;
;;; Traced Software:
;;;
;;;  POST
;;;     @code{<service-base>/tracesoft?cid=<cid>&sid=<software-oid>&tests-oid=<tests oid>}
;;;     Traces the specified software object, using the specified tests.
;;;     Returns the oid specified (does not create a new, distinct software
;;;     object).
;;;
;;; Write Software:
;;;
;;;  POST
;;;     @code{<service-base>/writesoft?cid=<cid>&sid=<software-oid>}
;;;     Writes the specified software object to a file (TO-FILE).
;;;     Returns success or error code.
;;;     Body contains path specification.
;;;
;;; Async-Job:
;;;
;;;  POST
;;;     @code{<service-base>/async?cid=<cid>&type=<job-type>} Body
;;;     contains (JSON) parameters for EVOLVE task, FITNESS-TEST or
;;;     other defined type of task.  Returns immediately with a
;;;     Job-ID (jid).  Creating a Job starts a task (on a new
;;;     thread) which will execute until stopped or completed.
;;;  GET
;;;     @code{<service-base>/cid=<cid>&async?jid=<Job-ID>} Returns JSON
;;;     containing job status and results.
;;;  PUT
;;;     @code{<service-base>/cid=<cid>&async?jid=<Job-ID>&<update-vars>}
;;;     Allows some control of the task, such as stopping the task.
;;;
;;; @texi{rest-std-api}
(defpackage :software-evolution-library/rest/std-api
  (:nicknames :sel/rest/std-api)
  (:use
   :gt/full
   :snooze
   :cl-json
   :trace-db
   :software-evolution-library/software-evolution-library
   :software-evolution-library/components/test-suite
   :software-evolution-library/components/formatting
   :software-evolution-library/components/instrument
   :software-evolution-library/components/traceable
   :software-evolution-library/rest/sessions
   :software-evolution-library/rest/utility
   :software-evolution-library/software/parseable
   :software-evolution-library/software/clang
   :software-evolution-library/command-line)
  (:shadowing-import-from :clack :clackup :stop)
  (:export :session-software
           :session-populations
           :session-mutations
           :session-test-suites
           :find-software
           :find-population
           :find-mutation
           :find-test-suite
           :population
           :population-individuals))
(in-package :software-evolution-library/rest/std-api)
(in-readtable :curry-compose-reader-macros)


;;;; REST Routes

;;;; Software Routes
(defun session-software (session)
  (session-store-value session "software"))

(defun (setf session-software) (value session)
  (setf (session-store-value session "software") value))

(defroute
    soft (:post "application/json" &key cid (sid nil) (type nil))
  (declare (ignore sid))
  (let ((json (handler-case
                  (decode-json-payload)
                (error (e)
                  (http-condition 400 "Malformed JSON (~a)!" e)))))
    (handler-case
        (let* ((client (lookup-session cid))
               (path (aget :path json))
               (url (aget :url json))
               (code (aget :code json))
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

(defun find-software (session sid)
  "Return the population from the session record (if found)."
  (if-let ((software (session-software session)))
    (car (member sid software
                 :key 'sel::oid :test 'eql))
    NIL))

(defun get-software (cid sid type)
  (let ((result "{ \"error\": \"Nothing\"}"))
    (if-let ((client (lookup-session cid)))
      (if-let ((software (find-software client sid)))
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
                (json:encode-json-to-string ids))))
      result)))

(defroute
    soft (:get :text/* &key cid sid (type nil))
  (get-software cid sid type))

(defroute
    soft (:get "application/json" &key cid sid (type nil))
  (get-software cid sid type))

;;; Specific explainer for soft resource (software object)
(defmethod explain-condition ((error error) (resource (eql #'soft))
                              (ct snooze-types:text/plain))
  (with-output-to-string (s)
    (format s
            "Error occurred processing SOFT (Software) object via REST API: ~A"
            (princ-to-string error))))

;;;; Population Routes
(defclass population ()
  ((name
    :initarg :name
    :accessor population-name
    :initform (make-gensym-string "pop-")
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

(defun session-populations (session)
  (session-store-value session "populations"))

(defun (setf session-populations) (value session)
  (setf (session-store-value session "populations") value))

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
                   (decode-json-payload)
                 (error (e)
                   (http-condition 400 "Malformed JSON (~a)!" e))))
         (client (lookup-session cid))
         (type (intern (string-upcase (aget :type json)) :sel))
         (sids (aget :sids json))
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
                   (decode-json-payload)
                 (error (e)
                   (http-condition 400 "Malformed JSON (~a)!" e))))
         (sids (aget :sids json))
         (client (lookup-session cid))
         (population (if name (find-population client name))))
    (when population
      (add-population client population sids)
      (format-population-as-json population))))

;;;; Mutation Routes

(defun session-mutations (session)
  (session-store-value session "mutations"))

(defun (setf session-mutations) (value session)
  (setf (session-store-value session "mutations") value))

(defun find-mutation (session mid)
  "Return the mutation from the session record (if found)."
  (car (member mid (session-mutations session)
               :key 'sel::oid :test 'eql)))

(defun lookup-resource (session str)
  "See if the json value is a string which can be parsed as a resource lookup.
Resource lookups are of the form \"<resource>:<oid>\""
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
    mut (:post "application/json" &key cid mid)
  (declare (ignore mid))
  (let* ((json (handler-case
                   (decode-json-payload)
                 (error (e)
                   (http-condition 400 "Malformed JSON (~a)!" e))))
         (session (lookup-session cid))
         (type (aget :type json))
         (type-sym (and type (convert-symbol type)))
         (sid (aget :sid json))
         (software (find-software session sid))
         (properties (iter (for x in json)
                           (unless
                               (member (car x)
                                       '(:type :sid :targets))
                             (collect (car x))
                             (collect
                              (or (lookup-resource session (cdr x))
                                  (convert-symbol (cdr x)))))))
         (targets (aget :targets json)))
    (if type
        (let ((mutation
               (apply 'make-instance
                      type-sym
                      :object software
                      :targets targets
                      properties)))
          ;; store the software obj with the session
          (push mutation (session-mutations session))
          (format nil "~D" (sel::oid mutation))))))

(defun format-mutation-as-json (mutation)
  (json:encode-json-plist-to-string
   (apply 'list
          :id (sel::oid mutation)
          :type (symbol-name (class-name (class-of mutation)))
          :sid (and (object mutation) (sel::oid (object mutation)))
          :software-type (symbol-name (class-name (class-of (object mutation))))
          :targets (targets mutation))))

(defun get-mutation (cid mid)
  (let* ((session (lookup-session cid)))
    (if session
        (let ((mutation (and mid (find-mutation session mid))))
          (if (null mutation)
              (let ((mids (iter (for x in (session-mutations session))
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

(defun session-test-suites (session)
  (session-store-value session "test-suites"))

(defun (setf session-test-suites) (value session)
  (setf (session-store-value session "test-suites") value))

(defun find-test-suite (client oid)
  "Return the test-suite from the client record with specified oid (if found)."
  (car (member oid (session-test-suites client)
               :key 'sel::oid :test 'eql)))

(defroute
    tests (:post "application/json" &key cid oid)
  (declare (ignore oid))
  (let* ((json (handler-case
                   (decode-json-payload)
                 (error (e)
                   (http-condition 400 "Malformed JSON (~a)!" e))))
         (session (lookup-session cid))
         (tests (aget :tests json))
         (test-suite
          (funcall 'make-instance
                   'test-suite
                   :test-cases
                   (mapcar
                    (lambda (test)
                      (let ((program-name (aget :program-name test))
                            (program-args (aget :program-args test)))
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
    (push test-suite (session-test-suites session))
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
  (if-let ((session (lookup-session cid)))
    (if-let ((test-suite (and oid (find-test-suite session oid))))
      (format-test-suite-as-json test-suite)
      (let ((test-suite-oids
             (iter (for x in (session-test-suites session))
                   (collect (sel::oid x)))))
        (json:encode-json-to-string test-suite-oids)))))

(defroute
    tests (:get :text/* &key cid oid)
  (get-test-suite cid oid))

(defroute
    tests (:get "application/json" &key cid oid)
  (get-test-suite cid oid))

;;;; Instrumentated Program Routes

(defroute instrumented (:post "application/json" &key cid sid)
  (handler-case
      (let* ((session (lookup-session cid))
             (soft (find-software session sid)))
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
          (push inst-soft (session-software session))
          (format nil "~D" (sel::oid inst-soft))))
    (error (e)
      (http-condition 400 "Error in INSTRUMENTED POST method (~a)!" e))))


(defun get-instrumented (cid sid type)
  (let* ((result "{ \"error\": \"Nothing\"}")
         (session (lookup-session cid)))
    (if session
        (let ((software (find-software session sid)))
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
              (let ((ids (iter (for x in (session-software session))
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
                       (decode-json-payload)
                     (error (e)
                       (http-condition 400 "Malformed JSON (~a)!" e))))
             (session (lookup-session cid))
             (soft (find-software session sid))
             (test-suite (find-test-suite session tests-oid))
             (inst-bin (aget :inst-bin json)))

        (if (and (typep soft 'sel/sw/clang::clang)
                 (null (sel/software/source::compiler soft)))
            (setf (sel/software/source::compiler soft) "clang"))

        ;; create the binary executable
        (if inst-bin
            (phenome soft :bin inst-bin))

        (with-trace-error-handling
            (with-temp-dir-of (temp)
              (make-pathname :directory (pathname-directory inst-bin))
              (with-current-directory (temp)
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
                       (decode-json-payload)
                     (error (e)
                       (http-condition 400 "Malformed JSON (~a)!" e))))
             (session (lookup-session cid))
             (soft (find-software session sid))
             (path (aget :path json)))

        (to-file (format-genome (uninstrument (copy soft)))
                 path)
        #(format nil "Software ID ~D successfully written to ~A" sid path)
        (format nil "~D" sid))
    (error (e)
      (http-condition 400
                      "Error in WRITESOFT POST method (~a)!" e))))
