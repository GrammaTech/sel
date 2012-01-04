;; Copyright (C) 2011  Eric Schulte
(defpackage #:software-evolution
  (:use
   :common-lisp
   :alexandria
   :split-sequence
   :metabang-bind
   :cl-store
   :trivial-shell
   :trivial-timeout
   :cl-ppcre)
  (:shadow :type))
