;;; External replacement for GT-specific test submission helpers
(defun batch-test (test project branch &optional args)
  "Run tests in 'batch' mode, printing results as a string."
  (declare (ignorable project branch args))

  (let* ((*test-progress-print-right-margin* (expt 2 20))
         (failures (coerce (stefil::failure-descriptions-of
                            (without-debugging (funcall test)))
                           'list)))
    (if failures
        (prog1 nil
          (format *error-output* "FAILURES~%")
          (mapc [{format *error-output* "  ~a~%"}
                 #'stefil::name-of
                 #'stefil::test-of
                 #'car #'stefil::test-context-backtrace-of]
                failures))
        (prog1 t
          (format *error-output* "SUCCESS~%")))))
