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
(defpackage :software-evolution-library/rest-utility
  (:nicknames :sel/rest-util :sel/rest-utility)
  (:use
   :alexandria
   :named-readtables
   :curry-compose-reader-macros
   :common-lisp)
  (:shadowing-import-from :clack :clackup :stop)
  (:export :convert-symbol
           :make-gensym-string))
(in-package :software-evolution-library/rest-utility)
(in-readtable :curry-compose-reader-macros)

(defun convert-symbol (string)
  "If a string contains '::' then convert it to a symbol if possible."
  (if-let ((sym (and (stringp string) (search "::" string)
                     (read-from-string string))))
    (if (symbolp sym)
        sym
        string)
    string))

(defun make-gensym-string (input)
  "Converts the input to a string, appending a random (gensym'd) number."
  (symbol-name (gensym (string input))))
