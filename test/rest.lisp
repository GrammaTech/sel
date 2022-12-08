;;;; rest.lisp --- REST API tests.
(defpackage :software-evolution-library/test/rest
  (:nicknames :sel/test/rest)
  (:use
   :gt/full
   :clack
   :snooze
   :drakma
   #+gt :testbot
   :software-evolution-library/test/util
   :software-evolution-library/test/util-clang
   :stefil+
   :software-evolution-library
   :software-evolution-library/command-line
   :software-evolution-library/command-line-rest
   :software-evolution-library/rest/async-jobs
   :software-evolution-library/software/clang)
  #-windows (:import-from :hunchentoot)
  (:shadowing-import-from :clack :stop)
  (:export :test-rest))
(in-package :software-evolution-library/test/rest)
(in-readtable :curry-compose-reader-macros)
(defsuite test-rest "REST API tests.")

(defvar *rest-client* nil "Client-id (cid) for REST API test client.")
(defvar *clack-port*  9003 "Default port for clack web server instance.")
(defvar *clack-delay* 0.5 "Seconds to delay after starting server")
(defvar *clack* nil)

#-(or windows ccl)
(defixture rest-server
  (:setup (unless *clack* (setf *clack* (initialize-clack))))
  (:teardown (clack:stop *clack*)(setf *clack* nil)(setf *rest-client* nil)))

#-(or windows ccl)
(let (old-standard-out old-error-out)
  (defixture fact-rest-server
    (:setup
     (setf old-standard-out *standard-output*
           old-error-out *error-output*
           *standard-output* (make-broadcast-stream)
           *error-output* (make-broadcast-stream))
     (define-command-async-rest (fact-entry :environment (*population*))
         ((n integer) &spec +common-command-line-options+)
       "Test that canonical REST endpoints work. Computes factorial."
       #.(format nil
                 "~%Built from SEL ~a, and ~a ~a.~%"
                 +software-evolution-library-version+
                 (lisp-implementation-type) (lisp-implementation-version))
       (declare (ignorable quiet verbose language))
       (if help
           (let ((*standard-output* (make-broadcast-stream)))
             (show-help-for-fact-entry))
           (factorial n)))
     (unless *clack* (setf *clack* (initialize-clack))))
    (:teardown
     (clack:stop *clack*)
     (setf *clack* nil
           *rest-client* nil
           *standard-output* old-standard-out
           *error-output* old-error-out))))

#-(or windows ccl)
(defun initialize-clack ()
  (let ((tries 0))
    (handler-bind
        ((usocket:socket-error
          (lambda (e)
            (warn "Starting web server on ~a failed with ~a" *clack-port* e)
            (if (< tries 20)
                (progn (incf tries)
                       (invoke-restart 'try-a-new-port))
                (error e)))))
      (restart-case
          (prog1
              ;; Inhibit the clack "Hunchentoot server is started." messages.
              (let ((*standard-output* (make-broadcast-stream)))
                (clack:clackup (snooze:make-clack-app) :port *clack-port*))
            ;; Wait for a second before continuing, to ensure the server is up.
            (sleep *clack-delay*))
        (try-a-new-port ()
          :report "Try using a new port"
          (incf *clack-port*)
          (prog1 (clack:clackup (snooze:make-clack-app)
                                :port *clack-port*)
            (sleep *clack-delay*)))))))

#-windows
(defun rest-test-create-client ()
  "Returns 2 values: new client id or nil, and status.
 Assumes service is running."
  (let* ((params '(("max-population-size" . "1024")))
         (result nil))
    (multiple-value-bind (stream status)
        (drakma:http-request
         (format nil "http://127.0.0.1:~D/client" *clack-port*)
         :method :post
         :content-type "application/json"
         :content (json:encode-json-to-string params)
         :want-stream t)
      (if (= status 200)
          (setf result (read stream)))
      (if (symbolp result)
          (setf result (symbol-name result)))
      (values result status))))

#-(or windows ccl)
(defun rest-test-create-software (type cid)
  "Given type of Software object and client-id, returns 2
 values: new software oid or nil, and status.
 Assumes service is running."
  (let* ((path (namestring (hello-world-dir "hello_world.c")))
         (params `(("path" . ,path)
                   ("compiler" . "clang")
                   ("flags" . ,(list "-I" (namestring
                                           (make-pathname
                                            :directory +headers-dir+))))))
         (result nil))
    (multiple-value-bind (stream status)
        (drakma:http-request
         (format nil "http://127.0.0.1:~D/soft?cid=~A&type=~A"
                 *clack-port* cid type)
         :method :post
         :content-type "application/json"
         :content (json:encode-json-to-string params)
         :want-stream t)
      (if (= status 200)
          (setf result (read stream)))
      (if (symbolp result)
          (setf result (symbol-name result)))

      (values result status))))

#-(or windows ccl)
(defun rest-test-get-new-client ()
  "Always creates a new REST client and returns (1) new client id,
 (2) http status code. The new client id is stored in *rest-client*."
  (multiple-value-bind (cid status)
      (rest-test-create-client)
    (setf *rest-client* cid) ; store the new client for further tests
    (values cid status)))

#-(or windows ccl)
(defun rest-test-get-client ()
  "If REST client already has been created, return it.
 Else, create one and return the client id (cid)."
  (or *rest-client* (rest-test-get-new-client)))

#-(or windows ccl)
(deftest (rest-create-client :long-running) ()
  ;; test ensures the web service is running and it can create a new client,
  ;; tests Create Client (HTTP POST) method.
  (with-fixture rest-server
    (multiple-value-bind (cid status) (rest-test-get-new-client)
      (is (eql status 200))
      (is (stringp cid))
      (is (string-equal (subseq cid 0 7) "client-")))))

#-(or windows ccl)
(deftest (rest-create-software :long-running) ()
  ;; test ensures the web service is running and it can create a new software
  ;; object. Tests Create Software (HTTP POST) method.
  (with-fixture rest-server
    (let ((cid (rest-test-get-client)))
      (multiple-value-bind (oid status)
          (rest-test-create-software
           "SOFTWARE-EVOLUTION-LIBRARY/SOFTWARE/CLANG:CLANG" cid)
        (is (eql status 200))
        (is (integerp oid))))))

#-(or windows ccl)
(define-async-job four-types-1
    ((a integer) (b string) (c float) (d boolean))
  "Test that the four supported types can be passed via REST."
  (format nil "~A: ~D, ~A: ~S, ~A: ~F, ~A: ~A"
          (type-of a) a
          (type-of b) b
          (type-of c) c
          (type-of d) d))

#-(or windows ccl)
(deftest run-async-job-func ()
  (let ((result
         (sel/rest:apply-async-job-func 'four-types-1 10 "twenty" 30.1 t)))
    (is (search "10" result))
    (is (search "\"twenty\"" result))
    (is (search "30.1" result))
    (is (search " T" result))))

#-(or windows ccl)
(define-command-async-rest (four-types-2)
    ((a integer) (b string) (c float) (d boolean)
     &spec +common-command-line-options+)
  "Test that the four supported types can be passed to an endpoint via REST."
  ""
  (declare (ignorable help quiet verbose load eval out-dir read-seed language))
  (format nil "~A: ~D, ~A: ~S, ~A: ~F, ~A: ~A"
          (type-of a) a
          (type-of b) b
          (type-of c) c
          (type-of d) d))

#-(or windows ccl)
(deftest run-rest-command-line-func ()
  (let ((result
         (four-types-2 10 "twenty" 30.1 t)))
    (is (search "10" result))
    (is (search "\"twenty\"" result))
    (is (search "30.1" result))
    (is (search " T" result))))

#-(or windows ccl)
(define-command-async-rest (fact-entry-cl)
    ((n integer) &spec +common-command-line-options+)
  "Test that canonical REST endpoints work. Computes factorial."
  #.(format nil
            "~%Built from SEL ~a, and ~a ~a.~%"
            +software-evolution-library-version+
            (lisp-implementation-type) (lisp-implementation-version))
  (declare (ignorable quiet verbose language))
  (if help
      (show-help-for-fact-entry-cl)
      (factorial n)))

#-(or windows ccl)
(deftest (run-rest-factorial-cl-func :long-running) ()
  (let ((*standard-output* (make-broadcast-stream)))
    (is (eql (fact-entry-cl 5 :verbose 3) 120))
    (is (eql (fact-entry-cl 52235215 :help T) nil))))

#-(or windows ccl)
(deftest (run-rest-factorial-cl-func-2 :long-running) ()
  (with-fixture fact-rest-server
    (let ((*standard-output* (make-broadcast-stream)))
      ;; This produces a warning that FACT-ENTRY is undefined,
      ;; but the function is defined by the fixture
      (is (eql (funcall (symbol-function 'fact-entry) 5 :verbose 3) 120))
      (is (eql (funcall (symbol-function 'fact-entry) 52235215 :help T) nil)))))

#-(or windows ccl)
(defun rest-endpoint-test-create-fact-job (client-id json-input)
  "Returns new job name or nil.
 Assumes service is running."
  (multiple-value-bind (stream status)
      (drakma:http-request
       (format nil "http://127.0.0.1:~D/rest-fact-entry?cid=~A"
               *clack-port* client-id)
       :method :post
       :content-type "application/json"
       :content (json:encode-json-to-string json-input)
       :want-stream t)
    (if (= status 200)
        (read stream)
        (format nil "Status code ~A" status))))

#-(or windows ccl)
(deftest (run-rest-factorial-remote-1 :long-running) ()
  (with-fixture fact-rest-server
    (multiple-value-bind (cid status) (rest-test-get-new-client)
      (declare (ignore status))
      (let ((*standard-output* (make-broadcast-stream))
            (result
             (symbol-name
              (rest-endpoint-test-create-fact-job cid '(("n" . 5))))))
        (is (stringp result))
        (is (starts-with-subseq "REST-FACT-ENTRY" result))))))

#-(or windows ccl)
(deftest (run-rest-factorial-remote-2 :long-running) ()
  (with-fixture fact-rest-server
    (multiple-value-bind (cid status) (rest-test-get-new-client)
      (declare (ignore status))
      (let* ((*standard-output* (make-broadcast-stream))
             (result
              (symbol-name
               (rest-endpoint-test-create-fact-job
                cid
                '(("n" . 5) ("help" . T))))))
        (is (stringp result))
        (is (starts-with-subseq "REST-FACT-ENTRY" result))))))
