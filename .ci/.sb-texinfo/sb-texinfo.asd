;;; -*- lisp -*-

;;;; A docstring extractor for the SBCL. Creates @include-ready documentation
;;;; from the docstrings of exported symbols of specified packages.

;;;; This software is part of the SBCL software system. SBCL is in the public
;;;; domain and is provided with absolutely no warranty. See the COPYING file
;;;; for more information.
;;;;
;;;; Written by Rudi Schlatte <rudi@constantly.at>, mangled
;;;; by Nikodemus Siivola <nikodemus@random-state.net>.

(defsystem :sb-texinfo
    :depends-on (:sb-introspect)
  :components ((:file "docstrings")))

