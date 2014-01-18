;;; neutral.lisp --- Evolve neutral variants of an original program

;; Copyright (C) 2011-2013  Eric Schulte

;;; License: GNU General Public License, Version 3 or later

;;; Commentary:

;;; Code:
(defpackage :neutral
  (:use :common-lisp
        :alexandria
        :split-sequence
        :metabang-bind
        :curry-compose-reader-macros
        :software-evolution
        :software-evolution-utility
        :software-evolution-command-line)
  (:export :main))
(in-package :neutral)

(defvar help "Usage: ~a SCRIPT ORIGINAL [OPTIONS...]

Collect mutated variants of ORIGINAL which are neutral with
respect to the test suite supplied in SCRIPT.

SCRIPT:
  Command line used to evaluate variant executables.  If SCRIPT
  contains the substring \"~~a\" it will be replaced with the name of
  the executable, otherwise the executable will be appended to the end
  of SCRIPT.

ORIGINAL:
  A file holding the original program.  The software representation
  used to generate neutral variants will be guessed based on the type
  of the ORIGINAL file.  Supported file types include ELF, assembler
  and C source code.

Options:
 -h,--help ------------- print this help message and exit
 -l,--linker LINKER ---- linker to use when linking C or assembler
 -L,--lflags FLAGS ----- flags to use when linking C or assembler
 -r,--res-dir DIR ------ store collected variants in DIR
 -n,--num NUM ---------- collect NUM variants at each step
                         default: 256
 -s,--steps ------------ total steps from the original
                         default: 16
 -k,--keep ------------- keep non-neutral variants
 -v,--verbose ---------- verbose debugging output
 -t,--type TYPE -------- force software representation to TYPE
~%")

(defun main (args)
  (in-package :neutral)
  (let ((self (pop args)))
    (when (or (not args)
              (string= (subseq (car args) 0 2) "-h")
              (string= (subseq (car args) 0 3) "--h"))
      (format t help self) (quit)))

  (let* ((script (pop args)) (path (pop args))
         ;; guess type from path
         (type (case (intern (string-upcase (pathname-type path)))
                 (s 'asm)
                 (c 'cil)
                 (t 'elf-sw)))
         (num 256) (steps 16) keep
         (res-dir "neutral-variants")
         orig previous collected linker flags)

    (getopts
     ("-l" "--linker"  (setf linker (pop args)))
     ("-L" "--lflags"  (setf flags
                             (split-sequence #\Space (pop args)
                                             :remove-empty-subseqs t)))
     ("-r" "--res-dir" (setf res-dir (pop args)))
     ("-n" "--num"     (setf num (parse-number (pop args))))
     ("-s" "--steps"   (setf steps (parse-number (pop args))))
     ("-k" "--keep"    (setf keep t))
     ("-v" "--verbose" (setf *shell-debug* t)))

    (setf orig (from-file (make-instance type) path))
    (when linker (setf (linker orig) linker))
    (when flags (setf (flags orig) flags))
    (setf previous (list orig))

    (ensure-directories-exist (make-pathname :directory res-dir))
    (flet ((test (asm)
             ;; Return numerical fitness printed to STDOUT by SCRIPT or zero.
             (with-temp-file (bin)
               (or (and (multiple-value-bind (info exit) (phenome asm :bin bin)
                          (declare (ignorable info)) (zerop exit))
                        (multiple-value-bind (out err errno) (shell script bin)
                          (declare (ignorable err))
                          (and (zerop errno)
                               (ignore-errors (parse-number out)))))
                   0))))
      (setf (fitness orig) (test orig))
      (format t "~&original fitness ~d~%" (fitness orig))
      (loop :for step :below steps :do
         (format t "~&step ~d~%" step)
         (loop :until (>= (length collected) num) :as i :from 0 :do
            (let ((new (mutate (random-elt previous))))
              (setf (fitness new) (test new))
              (when (= (fitness new) (fitness orig))
                (push new collected))
              (when (or keep (= (fitness new) (fitness orig)))
                (let ((filename
                       (make-pathname
                        :directory res-dir :type (pathname-type path)
                        :name (format nil "~d-~d-~d" num i (fitness new)))))
                  (with-open-file (out filename :direction :output)
                    (genome-string new out))))))
         (setf previous collected collected nil)))))
