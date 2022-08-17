;;; configuration.lisp --- primitives for specifying options, variable values,
;;;                        etc.
;;;
;;;
;;; @texi{configuration}
(defpackage :software-evolution-library/components/configuration
  (:nicknames :sel/components/configuration :sel/cp/configuration
              :sel/cp/cfg)
  (:use :gt/full
        :software-evolution-library
        :py-configparser)
  (:export :configuration-path
           :get-configuration
           :write-configuration-file
           :ensure-section
           :option
           :add-ignore-paths
           :handle-configuration))
(in-package :software-evolution-library/components/configuration)


;;; Configuration

(defgeneric configuration-path (project &key)
  (:documentation "Return the path to the configuration file for PROJECT."))

(defun write-configuration-file (project config)
  "Write CONFIG to the configuration file of PROJECT."
  (with-open-file (stream (configuration-path project)
                          :if-does-not-exist :create
                          :if-exists :supersede
                          :direction :output)
    (write-stream config stream)))

(defun get-configuration (project &rest rest &key &allow-other-keys)
  (let ((configuration-path (apply #'configuration-path project rest))
        (config (make-config)))
    (if (probe-file configuration-path)
        (read-files config (list configuration-path))
        config)))

(defun ensure-section (config section-name)
  "Ensure that SECTION-NAME exists in CONFIG."
  (unless (has-section-p config section-name)
    (add-section config section-name)))

(defun option (config section-name option-name)
  "Get OPTION-NAME that is in SECTION-NAME in CONFIG."
  (handler-bind (((or py-configparser:no-section-error
                      py-configparser:no-option-error)
                   (lambda (condition)
                     (declare (ignore condition))
                     (return-from option nil))))
    (get-option config section-name option-name)))

(defun (setf option) (value config section-name option-name)
  "Set OPTION-NAME that is in SECTION-NAME to VALUE in CONFIG."
  (ensure-section config section-name)
  (set-option config section-name option-name value))

(defgeneric handle-configuration (object path &key)
  (:documentation "Handle the config associated with OBJECT at PATH."))
