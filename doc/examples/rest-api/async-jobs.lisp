(in-package :sel/rest)

;;;
;;; examples of async-jobs:
;;;
(define-async-job four-types
    ((a integer) (b string) (c float) &optional ((d t) boolean))
  "Test that the four supported types can be passed via REST."
  (format nil "~A: ~D, ~A: ~S, ~A: ~F, ~A: ~A"
          (type-of a) a
          (type-of b) b
          (type-of c) c
          (type-of d) d))

#|
Python:
sel.create_async_job('my-job2', None, [[10, "twenty", 30.1, "CL::T"]],
sel.create_async_job('my-job2', None, [[10, "twenty", 30.1, "CL::T"]],
job = sel.create_async_job('my-job2', None, [[10, "twenty", 30.1, "CL::T"]],
"SEL/REST::FOUR-TYPES", 1)
|#

;;;
;;; This is just an example of calling an external command (in this case "eso").
;;;
(define-async-job eso
    ((command string)
     (src string)
     (io-file string)
     (func-name string)
     (fitness-harness string)
     (output-file string)
     &optional
     (verbose integer)
     (evals integer))
  (shell "~S ~A ~A ~S ~S ~S ~S ~S"
         command
         (if verbose (format nil "--verbose ~D" verbose) "")
         (if evals (format nil "--evals ~D" evals) "")
         src
         io-file
         func-name
         fitness-harness
         output-file))

#|
Python:
job = sel.create_async_job(None, None,
[["../eso/bin/eso",
"test/etc/asm-test/zeroset.s.att",
"test/etc/asm-test/zeroset-io",
"zeroset",
"~/quicklisp/local-projects/sel/software/asm-super-mutant-fitness.c",
"test/etc/asm-test/zeroset-improved.s.att",
0,
10000]],
"SEL/REST::ESO",
1)
|#

(sel/command-line-rest:define-command-rest (four-types)
    ((a integer) (b string) (c float) &optional ((d t) boolean))
  nil nil
  "Test that the four supported types can be passed via REST."
  (format nil "~A: ~D, ~A: ~S, ~A: ~F, ~A: ~A"
          (type-of a) a
          (type-of b) b
          (type-of c) c
          (type-of d) d))

#|
Python (same as first example):
job = sel.create_async_job('my-job2', None, [[10, "twenty", 30.1, "CL::T"]],
"SEL/REST::FOUR-TYPES", 1)
sel.get_async_job(job)

|#
