;;; External replacement for GT-specific test submission helpers
(defun batch-test (test &optional args)
  "Run tests in 'batch' mode, printing results as a string."
  (declare (ignorable args))

  (let* ((*test-progress-print-right-margin* (expt 2 20))
         (failures (coerce (stefil::failure-descriptions-of
                            (without-debugging (funcall test)))
                           'list)))
    (if failures
        (progn (format *error-output* "FAILURES~%")
               (mapc [{format *error-output* "  ~a~%"}
                      #'stefil::name-of
                      #'stefil::test-of
                      #'car #'stefil::test-context-backtrace-of]
                     failures)
               (quit 1))
        (progn (format *error-output* "SUCCESS~%")
               (quit 0)))))

(defun testbot-test (test project branch &optional args)
  (declare (ignorable test project branch args))
  (error "Function `testbot-test' only makes sense inside GrammaTech."))
