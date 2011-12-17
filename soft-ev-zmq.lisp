;;; soft-ev-zmq.lisp --- sharing individuals between soft-ev instances

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
(in-package :soft-ev-zmq)

(defun accept (address)
  "Accept and `incorporate' any incoming individuals on ADDRESS.
The address should look like \"tcp://*:1234\"."
  (flet ((raw-bytes (msg)
           (let ((data (zmq:msg-data-as-is msg))
                 (bytes (make-array '(0)
                                    :element-type '(unsigned-byte 8)
                                    :fill-pointer 0 :adjustable t)))
             (loop :for i :from 0 :to (1- (zmq:msg-size msg))
                :do (vector-push-extend (cffi:mem-ref data :char i) bytes))
             bytes)))
    (zmq:with-context (context 1)
      (zmq:with-socket (socket context zmq:upstream)
        (zmq:bind socket address)
        (loop
           (handler-case
               (let ((msg (make-instance 'zmq:msg)))
                 ;; Wait for next msg from client
                 (zmq:recv socket msg)
                 (let ((raw (raw-bytes msg)))
                   (when (> (length raw) 0)
                     (format t "~&individual received[~a]~%" (length raw))
                     (incorporate (from-bytes raw)))))
             (error (e) "~&zmq error ~a~%" e)))))))

(defun share (individual address)
  "Accept and `incorporate' any incoming individuals on ADDRESS.
The address should look like \"tcp://localhost:1234\"."
  (zmq:with-context (context 1)
    (zmq:with-socket (socket context zmq:req)
      (zmq:connect socket address)
      (zmq:send socket (make-instance 'zmq:msg :data (to-bytes individual))))))
