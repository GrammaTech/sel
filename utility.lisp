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
(in-package :software-evolution)

#+sbcl
(locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
  (sb-alien:define-alien-routine (#-win32 "tempnam" #+win32 "_tempnam" tempnam)
      sb-alien:c-string
    (dir sb-alien:c-string)
    (prefix sb-alien:c-string)))

(defun file-to-string (path) ;; TODO: unbearably slow in some instances
  (apply #'concatenate 'string
         (let (res)
           (with-open-file (in path)
             (do ((p (multiple-value-bind (a b) (read-line in nil) (cons a b))
                     (multiple-value-bind (a b) (read-line in nil) (cons a b))))
                 ((cdr p))
               (push (car p) res)
               (push (vector #\Newline) res)))
           (reverse res))))

(defun string-to-file (string path &key if-exists)
  (with-open-file (out path :direction :output :if-exists if-exists)
    (format out "~a" string)))

(defun temp-file-name (&optional ext)
  (let ((base #+clisp
          (let ((stream (gensym)))
            (eval `(with-open-stream (,stream (ext:mkstemp nil))
                     (pathname ,stream))))
          #+sbcl
          (tempnam nil nil)
          #+ccl
          (ccl:temp-pathname)
          #-(or sbcl clisp ccl)
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
  "SPEC should be a list of the variable used to reference the file and an optional extension."
  `(let ((,(car spec) (temp-file-name ,(second spec))))
     (unwind-protect
          (progn ;; (format t "~&-->~a<--~%" ,(car spec))
                 (string-to-file ,str ,(car spec))
                 ,@body)
       (when (probe-file ,(car spec)) (delete-file ,(car spec))))))

;; (defun shell (&rest rst)
;;   (multiple-value-bind (output err-output exit)
;;       (shell-command (apply #'format (cons nil rst)) :input nil)
;;     (values output err-output exit)))

(defvar *work-dir* nil)

(defun shell (&rest rst)
  (if *work-dir*
      ;; more robust shell execution using foreman
      (let* ((cmd (apply #'format (cons nil rst)))
             (name (tempnam *work-dir* "lisp-job"))
             (run-file (format nil "~a.run" name))
             (done-file (format nil "~a.done" name)))
        (string-to-file cmd run-file)
        (do () (())
          (when (probe-file done-file)
            (let ((lines (split-sequence #\Newline (file-to-string done-file)
                                         :remove-empty-subseqs t)))
              (delete-file done-file)
              (return (values (or (mapconcat (curry #'format nil "~a~%")
                                             (butlast lines) "")
                                  "")
                              ""
                              (parse-integer (car (last lines)))))))
          (sleep 0.1)))
      ;; native shell execution
      (multiple-value-bind (output err-output exit)
          #+sbcl (shell-command (apply #'format (cons nil rst)) :input nil)
          #-(or sbcl) (error "not implemented")
        (values output err-output exit))))

(defun parse-number (string)
  "Parse the number located at the front of STRING or return an error."
  (let ((number-str
         (or (multiple-value-bind (whole matches)
                 (scan-to-strings "^([-0-9/]+|[-0-9.]+)([^./A-Xa-x_-]|$)"
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


;;; Generic utility functions
(defmacro comp (&rest funcs)
  "Return a function equal to the composition of FUNCS."
  (let ((arg-sym (gensym)))
    `(lambda (,arg-sym)
       ,(reduce (lambda (f a) `(funcall ,f ,a))
                funcs :initial-value arg-sym :from-end t))))

#+eager
(defun pmapcar (f list)
  "Parallel map (from http://marijnhaverbeke.nl/pcall/)."
  (let ((result (mapcar (lambda (n) (pexec (funcall f n))) list)))
    (map-into result #'yield result)))

(defmacro mapcomb (func &rest lists)
  "Parallel map FUNC over all combinations of the elements of LISTS.
The arity of FUNC must match the number of elements of LISTS."
  (let* ((args (mapcar (lambda (n) (declare (ignorable n)) (gensym)) lists))
         (paired (map 'list #'cons args lists)))
    (reduce (lambda (prev pair)
              `(mapcar (lambda (,(car pair)) ,prev) ,(cdr pair)))
            paired
            :initial-value `(funcall ,func ,@args))))

#+eager
(defmacro pmapcomb (func &rest lists)
  "Parallel map FUNC over all combinations of the elements of LISTS.
The arity of FUNC must match the number of elements of LISTS."
  (let* ((args (mapcar (lambda (n) (declare (ignorable n)) (gensym)) lists))
         (paired (map 'list #'cons args lists)))
    (reduce (lambda (prev pair)
              `(pmapcar (lambda (,(car pair)) ,prev) ,(cdr pair)))
            paired
            :initial-value `(funcall ,func ,@args))))

(defmacro repeatedly (n &body body)
  (let ((var (gensym)))
    `(loop :for ,var :upto ,n :collect ,@body)))

#+eager
(defmacro prepeatedly (n &body body)
  "Return the result of running BODY N times in parallel."
  (let ((loop-sym (gensym))
        (result-sym (gensym)))
    `(let ((,result-sym (loop :for ,loop-sym :upto ,n :collect (pexec ,@body))))
       (map-into ,result-sym #'yield ,result-sym))))

(defun aget (key list)
  "Get KEY from association list LIST."
  (cdr (assoc key list)))

(defun getter (key)
  "Return a function which gets KEY from an association list."
  (lambda (it) (aget key it)))

(defun transpose (matrix)
  "Simple matrix transposition."
  (apply #'map 'list #'list matrix))

(defun interleave (list sep &optional rest)
  (if (cdr list)
      (interleave (cdr list) sep (cons sep (cons (car list) rest)))
      (reverse (cons (car list) rest))))

(defun mapconcat (func list sep)
  (apply #'concatenate 'string (interleave (mapcar func list) sep)))

(defun drop (n seq)
  "Return SEQ less the first N items."
  (subseq seq n))

(defun take (n seq)
  "Return the first N items of SEQ."
  (subseq seq 0 n))


;;; debugging helpers
(defvar *note-level* 0 "Enables execution notes.")
(defvar *note-out* t "Target of notation.")

(defun print-time (&optional (out t))
  (multiple-value-bind
        (second minute hour date month year day-of-week dst-p tz)
      (get-decoded-time)
    (declare (ignorable day-of-week dst-p tz))
    (format out "~d.~2,'0d.~2,'0d.~2,'0d.~2,'0d.~2,'0d"
            year month date hour minute second)))

(defun note (level &rest format-args)
  (when (>= *note-level* level)
    (format *note-out* "~&~a: ~a~%"
            (print-time nil)
            (apply #'format nil format-args))))

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
