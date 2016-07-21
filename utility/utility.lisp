;;; utility.lisp --- utility functions

;; Copyright (C) 2011  Eric Schulte

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:
(in-package :software-evolution-utility)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros))

(defvar infinity
  #+sbcl
  SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY
  #+ccl
  CCL::DOUBLE-FLOAT-POSITIVE-INFINITY
  #+allegro
  excl:*infinity-double*
  #+ecl
  (/ (coerce 1 'double-float) (coerce 0.0 'double-float))
  #-(or ecl sbcl ccl allegro)
  (error "must specify a positive infinity value"))

(defun getenv (name &optional default)
  #-(or allegro clisp ecl lispworks sbcl ccl)
  (error "getenv not implemented for ~a"
         (lisp-implementation-type))
  (or #+allegro (sys:getenv name)
      #+clisp (ext:getenv name)
      #+ecl (si:getenv name)
      #+lispworks (lispworks:environment-variable name)
      #+sbcl (sb-ext:posix-getenv name)
      #+ccl  (uiop/os:getenv name)
      default))

#+sbcl
(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (sb-alien:define-alien-routine (#-win32 "tempnam" #+win32 "_tempnam" tempnam)
      sb-alien:c-string
    (dir sb-alien:c-string)
    (prefix sb-alien:c-string)))

#+ccl
(defun tempnam (dir prefix)
  (ccl:with-filename-cstrs ((base dir) (prefix prefix))
    (ccl:get-foreign-namestring
     (ccl:external-call "tempnam" :address base :address prefix :address))))

(defun file-to-string (path)
  (with-open-file (in path)
    (let ((seq (make-string (file-length in))))
      (read-sequence seq in)
      seq)))

(defun file-to-bytes (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((seq (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence seq in)
      seq)))

(defun string-to-file (string path &key (if-exists :supersede))
  (with-open-file (out path :direction :output :if-exists if-exists)
    (format out "~a" string))
  path)

(defun bytes-to-file (bytes path &key (if-exists :supersede))
  (with-open-file (out path :element-type '(unsigned-byte 8)
                       :direction :output :if-exists if-exists)
    (write-sequence bytes out)))

(defun temp-file-name (&optional ext)
  (let ((base #+clisp
          (let ((stream (gensym)))
            (eval `(with-open-stream (,stream (ext:mkstemp nil))
                     (pathname ,stream))))
          #+(or sbcl ccl)
          (tempnam nil nil)
          #+allegro
          (system:make-temp-file-name)
          #-(or sbcl clisp ccl allegro)
          (error "no temporary file backend for this lisp.")))
    (if ext
        (concatenate 'string base "." ext)
        base)))

(defmacro with-temp-file (spec &rest body)
  "SPEC holds the variable used to reference the file w/optional extension.
After BODY is executed the temporary file is removed."
  `(let ((,(car spec) (temp-file-name ,(second spec))))
     (unwind-protect (progn ,@body)
       (when (probe-file ,(car spec)) (delete-file ,(car spec))))))

(defmacro with-temp-file-of (spec str &rest body)
  "SPEC should be a list of the variable used to reference the file
and an optional extension."
  `(let ((,(car spec) (temp-file-name ,(second spec))))
     (unwind-protect (progn (string-to-file ,str ,(car spec)) ,@body)
       (when (probe-file ,(car spec)) (delete-file ,(car spec))))))

(defmacro with-temp-file-of-bytes (spec bytes &rest body)
  "SPEC should be a list of the variable used to reference the file
and an optional extension."
  `(let ((,(car spec) (temp-file-name ,(second spec))))
     (unwind-protect (progn (bytes-to-file ,bytes ,(car spec)) ,@body)
       (when (probe-file ,(car spec)) (delete-file ,(car spec))))))

(defun from-bytes (bytes)
  (with-temp-file (tmp) (bytes-to-file bytes tmp) (restore tmp)))

(defun to-bytes (software)
  (with-temp-file (tmp) (store software tmp) (file-to-bytes tmp)))

(defun ensure-path-is-string (path)
  (cond
    ((stringp path) path)
    ((pathnamep path) (namestring path))
    (:otherwise (error "Path not string ~S." path))))


;;; Shell and system command helpers
(defvar *work-dir* nil)

(defvar *shell-debug* nil
  "Set to true to print shell invocations.")

(defvar *shell-error-codes* '(126 127)
  "Raise a condition on these exit codes.")

(define-condition shell-command-failed (error)
  ((commmand :initarg :command :initform nil :reader command)
   (exit-code :initarg :exit-code :initform nil :reader exit-code))
  (:report (lambda (condition stream)
             (format stream "Shell command failed with status ~a: \"~a\""
                     (exit-code condition) (command condition)))))

(defun shell (&rest rst)
  (apply {shell-with-input nil} rst))

(defun shell-with-input (input &rest rst)
  (let ((cmd (apply #'format (cons nil rst))))
    (when *shell-debug*
      (format t "  cmd: ~a~%" cmd)
      (when input
        (format t "  input: ~a~%" input)))
    (if *work-dir*
        ;; more robust shell execution using foreman
        (let* ((name
                #+(or sbcl ccl) (tempnam *work-dir* "lisp-")
                #-(or sbcl ccl) (error "work-dir not supported for this lisp"))
               (run-file (format nil "~a.run" name))
               (done-file (format nil "~a.done" name)))
          (string-to-file cmd run-file)
          (do () (())
            (when (probe-file done-file)
              (let ((lines (split-sequence #\Newline (file-to-string done-file)
                                           :remove-empty-subseqs t)))
                (delete-file done-file)
                (let ((stdout (or (mapconcat (curry #'format nil "~a~%")
                                             (butlast lines) "")
                                  ""))
                      (errno (if (lastcar lines)
                                 (parse-integer (lastcar lines))
                                 2)))
                  (when *shell-debug*
                    (format t "~&stdout:~a~%errno:~a~%" stdout errno))
                  (return (values stdout "" errno)))))
            (sleep 0.1)))
        ;; native shell execution
        (multiple-value-bind (stdout stderr errno)
            #+sbcl (shell-command cmd :input input)
            #+ccl (progn (warn "shell-command may hang if output is large")
                         (shell-command cmd :input ""))
            #+allegro
            (multiple-value-bind (out-lines err-lines errno)
                (excl.osi:command-output cmd)
              (let ((out (apply #'concatenate 'string out-lines))
                    (err (apply #'concatenate 'string err-lines)))
                (values out err errno)))
            #-(or sbcl ccl allegro) (error "not implemented")
            (when *shell-debug*
              (format t "~&stdout:~a~%stderr:~a~%errno:~a" stdout stderr errno))
            (if (find errno *shell-error-codes*)
                (restart-case (error (make-condition 'shell-command-failed
                                                     :exit-code errno
                                                     :command cmd))
                  (ignore-shell-error () "Ignore error and continue")))
            (values stdout stderr errno)))))

(defmacro write-shell-file
    ((stream-var file shell &optional args) &rest body)
  "Executes BODY with STREAM-VAR passing through SHELL to FILE."
  #-sbcl (error "`WRITE-SHELL-FILE' unimplemented for non-SBCL lisps.")
  (let ((proc-sym (gensym))
        (thread-sym (gensym))
        (byte-sym (gensym))
        (file-var (gensym)))
    `(let* ((,proc-sym (sb-ext:run-program ,shell ,args :search t
                                           :output :stream
                                           :input :stream
                                           :wait nil))
            (,thread-sym
             (sb-thread:make-thread ; Thread connecting shell output to file.
              (lambda ()
                (with-open-file (,file-var ,file :direction :output
                                           :if-exists :supersede
                                           :element-type 'unsigned-byte)
                  (loop :for ,byte-sym = (read-byte
                                          (sb-ext:process-output ,proc-sym)
                                          nil :eof)
                     :until (eql ,byte-sym :eof)
                     :do (write-byte ,byte-sym ,file-var))
                  (close (sb-ext:process-output ,proc-sym)))))))
       (unwind-protect
            (with-open-stream (,stream-var (sb-ext:process-input ,proc-sym))
              ,@body)
         (sb-thread:join-thread ,thread-sym)))))

(defmacro read-shell-file
    ((stream-var file shell &optional args) &rest body)
  "Executes BODY with STREAM-VAR passing through SHELL from FILE."
  #-sbcl (error "`READ-SHELL-FILE' unimplemented for non-SBCL lisps.")
  (let ((proc-sym (gensym))
        (thread-sym (gensym))
        (byte-sym (gensym))
        (file-var (gensym)))
    `(let* ((,proc-sym (sb-ext:run-program ,shell ,args :search t
                                           :output :stream
                                           :input :stream
                                           :wait nil))
            (,thread-sym
             (sb-thread:make-thread ; Thread connecting file to shell input.
              (lambda ()
                (with-open-file (,file-var ,file :direction :input
                                           :if-exists :supersede
                                           :element-type 'unsigned-byte)
                  (loop :for ,byte-sym = (read-byte ,file-var nil :eof)
                     :until (eql ,byte-sym :eof) :do
                     (write-byte ,byte-sym (sb-ext:process-input ,proc-sym)))
                  (close (sb-ext:process-input ,proc-sym)))))))
       (unwind-protect
            (with-open-stream (,stream-var (sb-ext:process-output ,proc-sym))
              ,@body)
         (sb-thread:join-thread ,thread-sym)))))

(defvar *bash-shell* "/bin/bash"
  "Bash shell for use in `read-shell'.")

(defmacro read-shell ((stream-var shell) &rest body)
  "Executes BODY with STREAM-VAR holding the output of SHELL.
The SHELL command is executed with `*bash-shell*'."
  #-sbcl (error "`READ-SHELL-FILE' unimplemented for non-SBCL lisps.")
  (let ((proc-sym (gensym)))
    `(let* ((,proc-sym (sb-ext:run-program *bash-shell*
                                           (list "-c" ,shell) :search t
                                           :output :stream
                                           :wait nil)))
       (with-open-stream (,stream-var (sb-ext:process-output ,proc-sym))
         ,@body))))

(defmacro xz-pipe ((in-stream in-file) (out-stream out-file) &rest body)
  "Executes BODY with IN-STREAM and OUT-STREAM read/writing data from xz files."
  `(read-shell-file (,in-stream ,in-file "unxz")
     (write-shell-file (,out-stream ,out-file "xz")
       ,@body)))

(defun parse-number (string)
  "Parse the number located at the front of STRING or return an error."
  (let ((number-str
         (or (multiple-value-bind (whole matches)
                 (scan-to-strings
                  "^(-?.?[0-9]+(/[-e0-9]+|\\.[-e0-9]+)?)([^\\./A-Xa-x_-]$|$)"
                  string)
               (declare (ignorable whole))
               (when matches (aref matches 0)))
             (multiple-value-bind (whole matches)
                 (scan-to-strings "0([xX][0-9A-Fa-f]+)([^./]|$)"
                                  string)
               (declare (ignorable whole))
               (when matches (concatenate 'string "#" (aref matches 0)))))))
    (assert number-str (string) "String ~S doesn't specify a number." string)
    (read-from-string number-str)))

(defun parse-numbers (string &key (radix 10) (delim #\Space))
  (mapcar #'(lambda (num) (parse-integer num :radix radix))
          (split-sequence delim string :remove-empty-subseqs t)))


;;; generic forensic functions over arbitrary objects
(defun my-slot-definition-name (el)
  #+sbcl
  (sb-mop::slot-definition-name el)
  #+ccl
  (ccl:slot-definition-name el)
  #-(or sbcl ccl)
  (clos::slot-definition-name el))

(defun my-class-slots (el)
  #+sbcl
  (sb-mop::class-slots el)
  #+ccl
  (ccl:class-slots el)
  #-(or sbcl ccl)
  (clos::class-slots el))

(defun show-it (hd &optional out)
  "Print the fields of a elf, section or program header.
Optional argument OUT specifies an output stream."
  (format (or out t) "~&")
  (mapcar
   (lambda (slot)
     (let ((val (slot-value hd slot)))
       (format (or out t) "~s:~a " slot val)
       (list slot val)))
   (mapcar #'my-slot-definition-name (my-class-slots (class-of hd)))))

(defun equal-it (obj1 obj2 &optional trace)
  "Equal over objects and lists."
  (let ((trace1 (concatenate 'list (list obj1 obj2) trace)))
    (cond
      ((or (member obj1 trace) (member obj2 trace)) t)
      ((and (listp obj1) (not (listp (cdr obj1)))
            (listp obj2) (not (listp (cdr obj2))))
       (and (equal-it (car obj1) (car obj2))
            (equal-it (cdr obj1) (cdr obj2))))
      ((or (and (listp obj1) (listp obj2)) (and (vectorp obj1) (vectorp obj2)))
       (and (equal (length obj1) (length obj2))
            (reduce (lambda (acc pair)
                      (and acc (equal-it (car pair) (cdr pair) trace1)))
                    (if (vectorp obj1)
                        (mapcar #'cons (coerce obj1 'list) (coerce obj2 'list))
                        (mapcar #'cons obj1 obj2))
                    :initial-value t)))
      ((my-class-slots (class-of obj1))
       (reduce (lambda (acc slot)
                 (and acc (equal-it (slot-value obj1 slot) (slot-value obj2 slot)
                                    trace1)))
               (mapcar #'my-slot-definition-name
                       (my-class-slots (class-of obj1)))
               :initial-value t))
      (t (equal obj1 obj2)))))

(defmacro repeatedly (times &rest body)
  (let ((ignored (gensym)))
    `(loop :for ,ignored :below ,times :collect ,@body)))

(defun indexed (list)
  (loop :for element :in list :as i :from 0 :collect (list i element)))

(defun different-it (obj1 obj2 &optional trace)
  (let ((trace1 (concatenate 'list (list obj1 obj2) trace)))
    (cond
      ((or (member obj1 trace) (member obj2 trace)) t)
      ((or (and (vectorp obj1) (vectorp obj2))
           (and (proper-list-p obj1) (proper-list-p obj2)))
       (and (or (equal (length obj1) (length obj2))
                (format t "~&different lengths ~a!=~a"
                        (length obj1) (length obj2)))
            (reduce (lambda-bind (acc (i (a b)))
                      (and acc (or (different-it a b trace1)
                                   (format t "~& at ~d ~a!=~a" i a b))))
                    (indexed
                     (if (vectorp obj1)
                         (mapcar #'list (coerce obj1 'list) (coerce obj2 'list))
                         (mapcar #'list obj1 obj2)))
                    :initial-value t)))
      ((and (consp obj1) (consp obj2))
       (and (different-it (car obj1) (car obj2))
            (different-it (cdr obj1) (cdr obj2))))
      ((my-class-slots (class-of obj1))
       (reduce (lambda (acc slot)
                 (and acc (or (different-it
                               (slot-value obj1 slot) (slot-value obj2 slot)
                               trace1)
                              (format t "~&  ~a" slot))))
               (mapcar #'my-slot-definition-name
                       (my-class-slots (class-of obj1)))
               :initial-value t))
      (t (or (equal obj1 obj2) (format t "~&~a!=~a" obj1 obj2))))))

(defun count-cons (cons-cell)
  "Count and return the number of cons cells used in CONS-CELL."
  ;; TODO: extend to map over the fields in an object.
  (if (consp cons-cell)
      (+ (count-cons (car cons-cell))
         (count-cons (cdr cons-cell)))
      1))


;;; Generic utility functions
(defun plist-get (item list &key (test #'eql) &aux last)
  (loop :for element :in list :do
     (cond
       (last (return element))
       ((funcall test item element) (setf last t)))))

(defun plist-keys (plist)
  (loop :for (key value) :on plist :by #'cddr :collect key))

(defun plist-drop-if (predicate list &aux last)
  (nreverse (reduce (lambda (acc element)
                      (cond
                        (last (setf last nil) acc)
                        ((funcall predicate element) (setf last t) acc)
                        (t (cons element acc))))
                    list :initial-value '())))

(defun plist-drop (item list &key (test #'eql))
  (plist-drop-if {funcall test item} list))

(defun plist-merge (plist-1 plist-2)
  "Merge arguments into a single plist with unique keys, prefer PLIST-1 items."
  (append plist-1 (plist-drop-if {member _ (plist-keys plist-1)} plist-2)))

(defun counts (list &key (test #'eql) key frac &aux totals)
  "Return an alist keyed by the unique elements of list holding their counts.
Keyword argument FRAC will return fractions instead of raw counts."
  (mapc (lambda (el)
          (if-let (place (assoc el totals :key key :test test))
            (incf (cdr place))
            (push (cons el 1) totals)))
        list)
  (if frac
      (let ((total (reduce #'+ (mapcar #'cdr totals))))
        (mapcar (lambda-bind ((obj . cnt)) (cons obj (/ cnt total))) totals))
      totals))

(defun proportional-pick (list key)
  (let ((raw (reduce (lambda (acc el) (cons (+ el (car acc)) acc))
                     (mapcar key list) :initial-value '(0))))
    (position-if {<= (random (first raw))} (cdr (reverse raw)))))

(defun position-extremum (list predicate key)
  "Returns the position in LIST of the element maximizing KEY."
  (car (extremum (indexed list) predicate :key [key #'second])))

(defun position-extremum-rand (list predicate key)
  "Randomly returns one of position in LIST maximizing KEY."
  (declare (ignorable predicate))
  (warn "`position-extremum-rand' not finished: doesn't use all parameters")
  (let ((scores (mapcar key list)))
    (random-elt (mapcar #'car (remove-if-not [{= (apply #'max scores)} #'second]
                                             (indexed scores))))))

(defun random-bool (&key bias)
  (> (or bias 0.5) (random 1.0)))

(defun uniform-probability (list)
  (mapcar {cons _ (/ 1.0 (length list))} list))

(defun cdf (alist)
  "Cumulative distribution function.
Return an updated version of ALIST in which the cdr of each element is
transformed from an instant to a cumulative probability."
  (nreverse
   (reduce (lambda-bind (acc (value . prob)) (acons value (+ (cdar acc) prob) acc))
           (cdr alist) :initial-value (list (car alist)))))

(defun random-pick (cdf)
  (car (find-if {<= (random 1.0)} cdf :key #'cdr)))

(defun random-elt-with-decay (orig-list decay-rate)
  (if (null orig-list)
      nil
      (labels ((pick-from (list)
                 (if (null list)
                     (pick-from orig-list)
                     (if (< (random 1.0) decay-rate)
                         (car list)
                         (pick-from (cdr list))))))
        (pick-from orig-list))))

(defun random-subseq (list &optional (size (random (1+ (length list)))))
  (subseq (shuffle list) 0 size))

(defun find-hashtable-element (hash-tbl n)
  (maphash
   (lambda (k v)
     (declare (ignore v))
     (when (= n 0) (return-from find-hashtable-element k))
     (decf n))
   hash-tbl))

(defun random-hash-table-key (hash-tbl)
  "Return a random key in a hash table"
  (let ((size (hash-table-count hash-tbl)))
    (unless (zerop size)
      (find-hashtable-element hash-tbl (random size)))))

;; From the Common Lisp Cookbook
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part
is replaced with replacement."
  (with-output-to-string (out)
    (loop :with part-length := (length part)
       :for old-pos := 0 :then (+ pos part-length)
       :for pos := (search part string
                           :start2 old-pos
                           :test test)
       :do (write-string string out
                         :start old-pos
                         :end (or pos (length string)))
       :when pos :do (write-string replacement out)
       :while pos)))

(defun apply-replacements (list str)
  (if (null list)
      str
      (let ((new-str (if (cdar list)
                         (replace-all str (caar list) (cdar list))
                         str)))
        (apply-replacements (cdr list) new-str))))

;;  Helper function for removing tags identifying DeclRefs
;;  from a code snippet.
(defun peel-bananas (text)
  (apply-replacements '(("(|" . "") ("|)" . "")) text))

(defun aget (item list &key (test #'eql))
  "Get KEY from association list LIST."
  (cdr (assoc item list :test test)))

(define-setf-expander aget (item list &key (test ''eql) &environment env)
  (multiple-value-bind (dummies vals stores store-form access-form)
      (get-setf-expansion list env)
    (declare (ignorable stores store-form))
    (let ((store (gensym))
          (cons-sym (gensym)))
      (values dummies
              vals
              `(,store)
              `(let ((,cons-sym (assoc ,item ,access-form :test ,test)))
                 (if ,cons-sym
                     (setf (cdr ,cons-sym) ,store)
                     (prog1 ,store
                       (setf ,access-form (acons ,item ,store ,access-form)))))
              `(aget ,item ,access-form :test ,test)))))

(defun alist (key value &rest rest)
  "Create an association list from the alternating keys and values."
  (acons key value (if (null rest) nil (apply #'alist rest))))

(defun alist-merge (alist-1 alist-2)
  "Merge arguments into a single alist with unique keys, prefer ALIST-1 items."
  (mapcar (lambda-bind ((key . value)) (cons key (or (aget key alist-1) value))) alist-2))

(defun getter (key)
  "Return a function which gets KEY from an association list."
  (lambda (it) (aget key it)))

(defun transpose (matrix)
  "Simple matrix transposition."
  (apply #'map 'list #'list matrix))

(defun interleave (list sep &optional rest)
  (cond
    ((cdr list) (interleave (cdr list) sep (cons sep (cons (car list) rest))))
    (list (reverse (cons (car list) rest)))
    (t nil)))

(defun mapconcat (func list sep)
  (apply #'concatenate 'string (interleave (mapcar func list) sep)))

(defun drop (n seq)
  "Return SEQ less the first N items."
  (if (> n (length seq))
      nil
      (subseq seq (min n (length seq)))))

(defun drop-while (pred seq)
  (if (and (not (null seq)) (funcall pred (car seq)))
      (drop-while pred (cdr seq))
      seq))

(defun drop-until (pred seq)
  (drop-while (complement pred) seq))

(defun take (n seq)
  "Return the first N items of SEQ."
  (subseq seq 0 (min n (length seq))))

(defun take-while (pred seq)
  (if (and (not (null seq)) (funcall pred (car seq)))
      (cons (car seq) (take-while pred (cdr seq)))
      '()))

(defun take-until (pred seq)
  (take-while (complement pred) seq))

(defun chunks (list size)
  "Return subsequent chunks of LIST of size SIZE."
  (loop :for i :below (1+ (- (length list) size)) :by size :collect
     (subseq list i (+ i size))))

(defun binary-search (value array &key (low 0)
                                       (high (1- (length array)))
                                       (test (lambda (v)
                                                (cond ((< v value) -1)
                                                      ((> v value) 1)
                                                      (t 0)))))
  "Perform a binary search for VALUE on a sorted ARRAY.
Optional keyword parameters:
LOW:  Lower bound
HIGH: Higher bound
TEST: Test for the binary search algorithm taking on arg.
Return -1 if arg is less than value, 1 if arg is greater than value,
and 0 otherwise."
  (if (< high low)
      nil
      (let ((middle (floor (/ (+ low high) 2))))

        (cond ((< 0 (funcall test (aref array middle)))
               (binary-search value array :low low
                                          :high (1- middle)
                                          :test test))

              ((> 0 (funcall test (aref array middle)))
               (binary-search value array :low (1+ middle)
                                          :high high
                                          :test test))

              (t middle)))))

;;; Source and binary locations and ranges.
(defclass source-location ()
  ((line :initarg :line :accessor line :type 'fixnum)
   (column :initarg :column :accessor column :type 'fixnum)))

(defclass source-range ()
  ((begin :initarg :begin :accessor begin :type 'source-location)
   (end   :initarg :end   :accessor end   :type 'source-location)))

(defclass range ()
  ((begin :initarg :begin :accessor begin :type 'fixnum)
   (end   :initarg :end   :accessor end   :type 'fixnum)))

(defmethod print-object ((obj source-location) stream)
  (print-unreadable-object (obj stream :type t)
    (prin1 (line obj) stream)
    (format stream ":")
    (prin1 (column obj) stream)))

(defmethod print-object ((obj source-range) stream)
  (flet ((p1-range (range)
           (prin1 (line range) stream)
           (format stream ":")
           (prin1 (column range) stream)))
    (print-unreadable-object (obj stream :type t)
      (p1-range (begin obj))
      (format stream " to ")
      (p1-range (end obj)))))

(defmethod print-object ((obj range) stream)
  (print-unreadable-object (obj stream :type t)
    (prin1 (begin obj) stream)
    (format stream " to ")
    (prin1 (end obj) stream)))

(defmethod source-< ((a source-location) (b source-location))
  (or (< (line a) (line b))
      (and (= (line a) (line b))
           (< (column a) (column b)))))

(defmethod source-<= ((a source-location) (b source-location))
  (or (< (line a) (line b))
      (and (= (line a) (line b))
           (<= (column a) (column b)))))

(defmethod source-> ((a source-location) (b source-location))
  (or (> (line a) (line b))
      (and (= (line a) (line b))
           (> (column a) (column b)))))

(defmethod source->= ((a source-location) (b source-location))
  (or (> (line a) (line b))
      (and (= (line a) (line b))
           (>= (column a) (column b)))))

(defmethod contains ((range source-range) (location source-location))
  (and (source-<= (begin range) location)
       (source->= (end range) location)))

(defmethod contains ((a-range source-range) (b-range source-range))
  (and (source-<= (begin a-range) (begin b-range))
       (source->= (end a-range) (end b-range))))

(defmethod contains ((range range) point)
  (and (<= (begin range) point) (>= (end range) point)))

(defmethod contains((a-range range) (b-range range))
  (and (<= (begin a-range) (begin b-range))
       (>= (end a-range) (end b-range))))

(defmethod intersects ((a-range source-range) (b-range source-range))
  (or (and (source-<= (begin a-range) (begin b-range))
           (source->  (end a-range) (begin b-range)))
      (and (source->= (end a-range) (end b-range))
           (source-<  (begin a-range) (end b-range)))))

(defmethod intersects ((a-range range) (b-range range))
  (or (and (<= (begin a-range) (begin b-range))
           (>  (end a-range) (begin b-range)))
      (and (>= (end a-range) (end b-range))
           (<  (begin a-range) (end b-range)))))


;;; debugging helpers
(defvar *note-level* 0 "Enables execution notes.")
(defvar *note-out* '(t) "Targets of notation.")

(defun print-time (&optional (out t))
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignorable day-of-week dst-p tz))
    (format out "~d.~2,'0d.~2,'0d.~2,'0d.~2,'0d.~2,'0d"
            year month date hour minute second)))

(defun note (level &rest format-args)
  (when (>= *note-level* level)
    (let ((*print-pretty* nil))
      (mapcar
       #'finish-output
       (mapc
        {format _ "~&;; ~a: ~a~%"
                (print-time nil) (apply #'format nil format-args)}
        *note-out*))))
  ;; Always return nil.
  nil)

#+sbcl
(defun trace-memory ()
  (when (>= *note-level* 2)
    (let ((percentage-used (/ (sb-vm::dynamic-usage)
                              (sb-ext::dynamic-space-size))))
      (if (>= *note-level* 4)
        (note 4 "~a ~,2f~%" (second (sb-debug:list-backtrace))
                            percentage-used)
        (when (>= percentage-used 0.5)
          (note 2 "~a ~,2f~%" (second (sb-debug:list-backtrace))
                              percentage-used))))))

;; adopted from a public domain lisp implementation copied from the

;; scheme implementation given at
;; http://en.wikipedia.org/wiki/Levenshtein_distance
(defun levenshtein-distance (s1 s2 &key (test #'char=) (key #'identity))
  (let* ((width (1+ (length s1)))
         (height (1+ (length s2)))
         (d (make-array (list height width))))
    (dotimes (x width)
      (setf (aref d 0 x) x))
    (dotimes (y height)
      (setf (aref d y 0) y))
    (dotimes (x (length s1))
      (dotimes (y (length s2))
        (setf (aref d (1+ y) (1+ x))
              (min (1+ (aref d y (1+ x)))
                   (1+ (aref d (1+ y) x))
                   (+ (aref d y x)
                      (if (funcall test
                                   (funcall key (aref s1 x))
                                   (funcall key (aref s2 y)))
                          0
                          1))))))
    (aref d (1- height) (1- width))))

;;; Diff computing
(defun diff-scalar (original-seq modified-seq)
  "Return an integer representing the diff size of two sequences
Sum O + |O - M| over each diff region.  O is the length of the
original diff region and M is the length of the modified diff
region."
  (reduce (lambda (acc region)
            (+ acc
               (ecase (type-of region)
                 (common-diff-region 0)
                 (modified-diff-region
                   (+ (original-length region)
                      (abs (- (original-length region)
                              (modified-length region))))))))
          (diff:compute-raw-seq-diff original-seq modified-seq)
          :initial-value 0))

;;; memory mapping, address -> LOC
(defun gdb-disassemble (phenome function)
  "Return the raw gdb disassembled code of FUNCTION in PHENOME."
  (shell "gdb --batch --eval-command=\"disassemble ~s\" ~s 2>/dev/null"
         function phenome))

(defun addrs (phenome function)
  "Return the numerical addresses of the lines (in order) of FUNCTION."
  (remove nil
    (mapcar
     (lambda (line)
       (declare (string line))
       (register-groups-bind (addr offset)
           ("[\\s]*0x([\\S]+)[\\s]*<([\\S]+)>:.*" line)
         (declare (ignorable offset) (string addr))
         (parse-integer addr :radix 16)))
     (split-sequence #\Newline (gdb-disassemble phenome function)))))

(defun function-lines (lines)
  "Return the line numbers of the lines (in order) of FUNCTION.
LINES should be the output of the `lines' function on an ASM object."
  (loop :for line :in lines :as counter :from 0
     :for function = (register-groups-bind
                         (line-function) ("^([^\\.][\\S]+):" line)
                       line-function)
     :collect (or function counter)))

(defun calculate-addr-map (lines phenome genome)
  "Calculate a map of memory address to offsets in LINES.
LINES should be the output of the `lines' function on an ASM object,
PHENOME should be the phenome of an ASM object and GENOME should be
the genome of an ASM object."
  (let ((flines (function-lines lines))
        (genome (coerce genome 'vector))
        (map (make-hash-table)))
    (loop
       :for addrs :in (mapcar (lambda (func) (addrs phenome func))
                              (remove-if-not #'stringp flines))
       :for lines :in (cdr (mapcar
                            {remove-if
                             [{scan "^[\\s]*\\."} {aget :code} {aref genome}]}
                             (split-sequence-if #'stringp flines)))
       :do (mapc (lambda (addr line) (setf (gethash addr map) line))
                 addrs lines))
    map))


;;; Oprofile functions
(defun samples-from-oprofile-file (path)
  (with-open-file (in path)
    (remove nil
      (loop :for line := (read-line in nil)
         :while line
         :collect
         (register-groups-bind (c a) ("^ *(\\d+).+: +([\\dabcdef]+):" line)
           (declare (string c) (string a))
           (cons (parse-integer a :radix 16) (parse-integer c)))))))

(defun samples-from-tracer-file (path &aux samples)
  (with-open-file (in path)
    (loop :for line := (read-line in nil)
       :while line
       :do (let ((addr (parse-integer line)))
             (if (assoc addr samples)
                 (setf (cdr (assoc addr samples))
                       (1+ (cdr (assoc addr samples))))
                 (setf samples (cons (cons addr 0) samples)))))
    samples))

(defvar *resolved-header-files* (make-hash-table :test 'equal)
  "A map from function name to a list of headers where
that function may be declared.")

(defun headers-in-manpage (section name)
  (multiple-value-bind (stdout stderr errno)
      (shell "man -P cat ~a ~a | sed -n \"/DESCRIPTION/q;p\" | grep \"#include\" | cut -d'<' -f 2 | cut -d'>' -f 1"
             section name)
    (declare (ignorable stderr errno))
    (split-sequence #\Newline stdout :remove-empty-subseqs t)))

(defun resolve-function-includes (func)
  (let ((headers (gethash func *resolved-header-files* 'not-found)))
    (mapcar {format nil "<~a>"}
            (if (eq headers 'not-found)
                (setf (gethash func *resolved-header-files*)
                      (or (headers-in-manpage 3 func)
                          (headers-in-manpage 2 func)))
                headers))))

(defun unlines (lines)
  (format nil "~{~a~^~%~}" lines))

;; Just a little sed-ish thing: find the first line that
;; contains the substring needle, and return the lines
;; after the one that matched.
(defun keep-lines-after-matching (needle haystack)
  (labels ((keep-after (lines)
             (if (null lines)
                 '()
                 (if (search needle (car lines))
                     (cdr lines)
                     (keep-after (cdr lines))))))
    (unlines (keep-after (split-sequence '#\Newline haystack)))))

(defun <not> (f)
  (lambda (x) (not (funcall f x))))

(defmacro <or> (&rest fs)
  (let ((args (gensym "args")))
    `(lambda (&rest ,args)
       (or ,@(mapcar (lambda (f) `(apply ,f ,args)) fs)))))

(defmacro <and> (&rest fs)
  (let ((args (gensym "args")))
    `(lambda (&rest ,args)
       (and ,@(mapcar (lambda (f) `(apply ,f ,args)) fs)))))


;;; Iteration helpers
(defmacro-clause (CONCATENATING expr &optional INTO var INITIAL-VALUE (val ""))
  `(reducing ,expr by {concatenate 'string} into ,var initial-value ,val))
