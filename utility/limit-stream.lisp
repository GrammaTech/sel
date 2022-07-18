(defpackage :software-evolution-library/utility/limit-stream
  (:use :gt/full)
  (:local-nicknames (:gray :trivial-gray-streams))
  (:export :make-limit-stream))
(in-package :software-evolution-library/utility/limit-stream)

(defclass limit-stream (gray:fundamental-character-output-stream)
  ((newlines :initform 0)
   (chars :initform 0)
   (newline-limit :initarg :newline-limit)
   (char-limit :initarg :char-limit)
   (callback :initarg :callback :type function))
  (:default-initargs
   :newline-limit 0
   :char-limit 0)
  (:documentation "A stream that invokes a callback after seeing a
  certain number of newlines."))

(defun make-limit-stream (stream callback &key
                                            (newline-limit 0)
                                            (char-limit 0))
  "Return a stream that wraps STREAM and invokes CALLBACK after
writing LIMIT newlines.

Note that LIMIT is a lower-bound; the actual number of newlines may be
greater."
  (make-broadcast-stream
   stream
   (make 'limit-stream
         :callback callback
         :newline-limit newline-limit
         :char-limit char-limit)))

(defun increment-newlines (stream n)
  (with-slots (newlines newline-limit callback) stream
    (unless (zerop newline-limit)
      (incf newlines n)
      (when (>= newlines newline-limit)
        (funcall callback)))))

(defun increment-chars (stream n)
  (with-slots (chars char-limit callback) stream
    (unless (zerop char-limit)
      (incf chars n)
      (when (>= chars char-limit)
        (funcall callback)))))

(defmethod gray:stream-write-char ((stream limit-stream)
                                   (char (eql #\Newline)))
  (increment-newlines stream 1)
  (increment-chars stream 1))

(defmethod gray:stream-terpri ((stream limit-stream))
  (increment-newlines stream 1)
  (increment-chars stream 1))

(defmethod gray:stream-write-string ((stream limit-stream)
                                     (string string)
                                     &optional start end)
  ;; Workaround for CCL, to ensure end is not nil.
  (let ((start (or start 0))
        (end (or end (length string))))
    (increment-chars stream (- end start))
    (increment-newlines stream (count #\Newline string :start start :end end))))
