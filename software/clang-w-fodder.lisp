;;; clang-w-fodder.lisp 

;;; clang software representation with a 
;;; JSON 'database' containing AST entries
;;; as fodder for the evolution process.

;;; Each AST entry in the JSON 'database'
;;; contains source text and, where applicable,
;;; the corresponding binary source code.
(in-package :software-evolution)

(defclass clang-w-fodder (clang)
  ((json-db :initarg :json-db :accessor json-db :initform '())))

(defun clang-w-fodder-from-file (path &key flags json-db-path)
  (assert (listp flags) (flags) "flags must be a list")

  (from-file 
    (make-instance 'clang-w-fodder 
      :flags flags
      :json-db 
        (with-open-file (json-stream json-db-path)
          (json:decode-json-from-source json-stream)))
    path))

(defmethod pick-json ((clang-w-fodder clang-w-fodder))
  (let ((ast-entry (random-elt (json-db clang-w-fodder))))
    (dolist (json-value ast-entry)
      (if (eq :SRC--TEXT (car json-value))
        (return (cdr json-value))))))

(defmethod mutate ((clang-w-fodder clang-w-fodder))
  (unless (> (size clang-w-fodder) 0)
    (error 'mutate :text "No valid IDS" :obj clang-w-fodder))
  (unless (> (length (json-db clang-w-fodder)))
    (error 'mutate :text "No valid JSON 'database' for fodder" :obj clang-w-fodder))
  (setf (fitness clang-w-fodder) nil)

  (let ((op (case (random-elt '(cut insert swap set-value insert-value))
              (cut          `(:cut ,(pick-bad clang-w-fodder)))
              (insert       `(:insert ,(pick-bad clang-w-fodder) ,(pick-good clang-w-fodder)))
              (swap         `(:swap ,(pick-bad clang-w-fodder) ,(pick-good clang-w-fodder)))
              (set-value    `(:set-value ,(pick-good clang-w-fodder) ,(pick-json clang-w-fodder)))
              (insert-value `(:insert-value ,(pick-good clang-w-fodder) ,(pick-json clang-w-fodder))))))
    (apply-mutation clang-w-fodder op)
    (values clang-w-fodder op)))

(defmethod apply-mutation :around ((clang-w-fodder clang-w-fodder) mut)
   (call-next-method clang-w-fodder mut))

(defmethod apply-mutation ((clang-w-fodder clang-w-fodder) op)
  (with-temp-file-of (src (ext clang-w-fodder)) (genome clang-w-fodder)
    (multiple-value-bind (stdout stderr exit)
      (shell "clang-mutate ~a ~a ~a -- ~{~a~^ ~}|tail -n +2"
             (ecase (car op)
               (:cut          "-cut")
               (:insert       "-insert")
               (:swap         "-swap")
               (:set-value    "-set")
               (:insert-value "-insert-value")
               (:ids          "-ids"))
             (mapconcat (lambda (pair)
                          (if (stringp (cdr pair))
                              (format nil "-value='~a'" (cdr pair))
                              (format nil "-stmt~d=~d" (car pair) (cdr pair))))
                        (loop :for id :in (cdr op) :as i :from 1
                           :collect (cons i id)) " ")
             src (flags clang-w-fodder))
      stdout)))
