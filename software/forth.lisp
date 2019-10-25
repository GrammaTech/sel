;;; forth.lisp --- software representation of Forth code
(defpackage :software-evolution-library/software/forth
  (:nicknames :sel/software/forth :sel/sw/forth)
  (:use :common-lisp
        :alexandria
        :arrow-macros
        :named-readtables
        :curry-compose-reader-macros
        :iterate
        :cl-ppcre
        :software-evolution-library
        :software-evolution-library/utility
        :software-evolution-library/software/simple)
  (:export :forth))
(in-package :software-evolution-library/software/forth)
(in-readtable :curry-compose-reader-macros)


;;; asm software objects
(define-software forth (simple)
  ((shebang :initarg :shebang :accessor shebang :initform nil))
  (:documentation "Forth program represented as a list of commands."))

(defmethod from-file ((forth forth) path &aux strings)
  "Read forth script from PATH setting `genome' and `shebang'."
  (setf (genome forth)
        (flet ((rm-eol-comments (line)
                 (subseq line 0 (search "\\ " line)))
               (rm-inline-comments (line)
                 (cl-ppcre:regex-replace-all "[\\s]\\([\\s][^\\)]*\\)" line ""))
               (rm-strings (line)
                 (let ((start 0) out)
                   (loop :while
                      (multiple-value-bind (match-start match-end)
                          (scan "[\.sS]\"[^\"]+\"" line :start start)
                        (when match-start
                          (push (subseq line start match-start) out)
                          (push :string out)
                          (push (subseq line match-start match-end) strings)
                          (setq start match-end))))
                   (push (subseq line start) out)
                   (nreverse out))))
          (with-open-file (in path)
            (loop :for line = (read-line in nil) :while line :append
               (if (and (> (length line) 2) (string= "#! " (subseq line 0 3)))
                   (prog1 nil (setf (shebang forth) line))
                   (mapcar [#'list {cons :code}]
                           (mapcan (lambda (el)
                                     (if (equal :string el)
                                         (list (pop strings))
                                         (remove-if #'emptyp
                                                    (split "[\\s]+"
                                                           (rm-inline-comments
                                                            el)))))
                                   (rm-strings (rm-eol-comments line)))))))))
  forth)

(defmethod phenome ((forth forth) &key (bin (temp-file-name)))
  "Write FORTH to an executable script suitable for evaluation."
  #-ccl (declare (values t fixnum string string string))
  (setf bin (namestring bin))
  (string-to-file (format nil "~a~%~a" (shebang forth) (genome-string forth))
                  bin)
  (multiple-value-bind (stdout stderr errno) (shell "chmod +x ~s" bin)
    (values bin errno stderr stdout bin)))

(defmethod genome-string ((forth forth) &optional stream)
  "Return genome of FORTH as a string.
Like `(genome-string simple)' but lines are delimited by spaces
instead of Newlines."
  (format stream "~{~a ~}~%" (lines forth)))
