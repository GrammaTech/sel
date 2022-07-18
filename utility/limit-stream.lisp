(defpackage :software-evolution-library/utility/limit-stream
  (:use :gt/full)
  (:local-nicknames (:gray :trivial-gray-streams))
  (:export :make-limit-stream))
(in-package :software-evolution-library/utility/limit-stream)

(defclass limit-stream (gray:fundamental-character-output-stream)
  ((newlines :initform 0)
   (limit :initarg :limit)
   (callback :initarg :callback :type function))
  (:default-initargs :limit 1)
  (:documentation "A stream that invokes a callback after seeing a
  certain number of newlines."))

(defun make-limit-stream (stream callback &key (limit 1))
  "Return a stream that wraps STREAM and invokes CALLBACK after
writing LIMIT newlines.

Note that LIMIT is a lower-bound; the actual number of newlines may be
greater."
  (make-broadcast-stream
   stream
   (make 'limit-stream
         :callback callback
         :limit limit)))

(defun increment-newlines (stream n)
  (with-slots (newlines limit callback) stream
    (incf newlines n)
    (when (= newlines limit)
      (funcall callback))))

(defmethod gray:stream-write-char ((stream limit-stream)
                                   (char (eql #\Newline)))
  (increment-newlines stream 1))

(defmethod gray:stream-terpri ((stream limit-stream))
  (increment-newlines stream 1))

(defmethod gray:stream-write-string ((stream limit-stream)
                                     (string string)
                                     &optional (start 0)
                                       (end (length string)))
  (increment-newlines stream (count #\Newline string :start start :end end)))
