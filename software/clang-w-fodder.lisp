;;; clang-w-fodder.lisp

;;; clang software representation with a
;;; JSON 'database' containing AST entries
;;; as fodder for the evolution process.

;;; Each AST entry in the JSON 'database'
;;; contains source text and, where applicable,
;;; the corresponding binary source code.
(in-package :software-evolution)

(defvar *json-database* (make-hash-table :test 'equal)
  "A database of source code snippets, grouped by AST class name.")

(defvar *json-database-bins* (lambda (x) "undefined-class")
  "The inverse cumulative distribution function for the AST class name of
a uniformly selected element of the JSON database.")

(defun random-snippet ()
  (assert (not (null *json-database-bins*)) (*json-database-bins*)
          "*json-database-bins* must be precomputed.")
  (let* ((binprob (random 1.0))
         (bin (cdr (find-if (lambda (datum)
                              (<= binprob (car datum))) *json-database-bins*))))
    (random-elt (gethash bin *json-database* '("/* bad snippet */")))))

(defun populate-*json-database-bins* ()
  (let ((total 0)
        (totalp 0))

    (setq *json-database-bins* '())
    (maphash (lambda (k v)
               (setf total (+ total (length v))))
             *json-database*)

    (maphash (lambda (k v)
               (setq totalp (+ totalp (/ (length v) total)))
               (setq *json-database-bins*
                     (cons (cons totalp k) *json-database-bins*)))
             *json-database*)

    (reverse *json-database-bins*)))

(defclass clang-w-fodder (clang) ())

(defmethod from-file :before ((obj clang-w-fodder) path)
  (assert (not (null *json-database-bins*))))

(defun clang-w-fodder-setup-db (json-db-path)
  ;; Clobber the existing database
  (setq *json-database* (make-hash-table :test 'equal))
  (setq *json-database-bins* '())

  ;; Load the snippet database and classify by AST class.
  (dolist (snippet (with-open-file (json-stream json-db-path)
                     (json:decode-json-from-source json-stream)))
    (let* ((ast-class (aget :AST--CLASS snippet))
           (cur (gethash ast-class *json-database*)))
      (setf (gethash ast-class *json-database*) (cons snippet cur))))

  ;; Compute the bin sizes so that (random-snippet) becomes useful.
  (populate-*json-database-bins*))

(defun nonempty-lines (text)
  (remove-if (lambda (x) (string= x ""))
             (split-sequence #\Newline text)))

(defmethod get-vars-in-scope ((clang-w-fodder clang-w-fodder) pt)
  (with-temp-file-of (src (ext clang-w-fodder)) (genome clang-w-fodder)
    (multiple-value-bind (stdout stderr exit)
        (shell "clang-mutate -get-scope=~a -stmt1=~a ~a -- ~{~a~^ ~}"
             20
             pt
             src (flags clang-w-fodder))
      (nonempty-lines stdout))))

(defun random-elt-with-decay (orig-list decay-rate)
  (if (null orig-list)
      "/* no bound vars */"
      (labels ((pick-from (list)
                 (if (null list)
                     (pick-from orig-list)
                     (if (< (random 1.0) decay-rate)
                         (car list)
                         (pick-from (cdr list))))))
        (pick-from orig-list))))

;; From the Common Lisp Cookbook
(defun replace-all (string part replacement &key (test #'char=))
  "Returns a new string in which all the occurences of the part 
is replaced with replacement."
  (with-output-to-string (out)
    (loop with part-length = (length part)
          for old-pos = 0 then (+ pos part-length)
          for pos = (search part string
                            :start2 old-pos
                            :test test)
          do (write-string string out
                           :start old-pos
                           :end (or pos (length string)))
          when pos do (write-string replacement out)
       while pos)))

(defun apply-replacements (list str)
  (if (null list)
      str
      (let ((new-str (replace-all str (caar list) (cdar list))))
        (apply-replacements (cdr list) new-str))))

;; Returns multiple values: (stmt-class-string has-semicolon)
(defmethod get-stmt-info ((clang-w-fodder clang-w-fodder) pt)
  (with-temp-file-of (src (ext clang-w-fodder)) (genome clang-w-fodder)
     (multiple-value-bind (stdout stderr exit)
         (shell "clang-mutate -get-info -stmt1=~a ~a -- ~{~a~^ ~}"
                pt
                src
                (flags clang-w-fodder))
       (apply #'values
        (let ((result (nonempty-lines stdout)))
          (if (not (equal (length result) 2))
              '("[unknown-class]" nil)
              (list (car result)
                    (equal (cadr result) "has-semi"))))))))

(defmethod pick-any-json ((clang-w-fodder clang-w-fodder) pt)
  (let ((stmt-info (get-stmt-info clang-w-fodder pt)))
    (multiple-value-bind (ast-class has-semi) stmt-info
      (prepare-code-snippet clang-w-fodder
                            pt
                            (random-snippet)
                            has-semi))))

(defmethod pick-json-by-class ((clang-w-fodder clang-w-fodder) pt)
  (let ((stmt-info (get-stmt-info clang-w-fodder pt)))
    (multiple-value-bind (ast-class has-semi) stmt-info
      (let ((asts (gethash ast-class *json-database* '())))
        (prepare-code-snippet clang-w-fodder
                              pt
                              (if (null asts)
                                  (random-snippet)
                                  (random-elt asts)) 
                              has-semi)))))

(defmethod prepare-code-snippet ((clang-w-fodder clang-w-fodder)
                                 pt
                                 snippet
                                 has-semi)
  (let* ((raw-code   (aget :SRC--TEXT snippet))
         (free-vars  (aget :UNBOUND--VALS snippet))
         (scope-vars (get-vars-in-scope clang-w-fodder pt))
         (bindings (map 'list
                        (lambda (x)
                          (cons x (random-elt-with-decay scope-vars 0.5)))
                        free-vars))
         (replaced (apply-replacements bindings raw-code)))
    (if has-semi
        (string-append replaced ";")
        replaced)))

(defmethod mutate ((clang-w-fodder clang-w-fodder))
  (unless (> (size clang-w-fodder) 0)
    (error 'mutate :text "No valid IDS" :obj clang-w-fodder))
  (unless (> (hash-table-size *json-database*) 0)
    (error 'mutate :text "No valid JSON 'database' for fodder"
           :obj clang-w-fodder))

  (setf (fitness clang-w-fodder) nil)

  (let ((op (case (random-elt '(cut insert swap set-value insert-value))
              (cut          `(:cut ,(pick-bad clang-w-fodder)))
              (insert       `(:insert ,(pick-bad clang-w-fodder)
                                      ,(pick-good clang-w-fodder)))
              (swap         `(:swap ,(pick-bad clang-w-fodder)
                                    ,(pick-good clang-w-fodder)))
              (set-value    (let ((good (pick-good clang-w-fodder)))
                              `(:set-value ,good
                                           ,(pick-json-by-class clang-w-fodder
                                                                good))))
              (insert-value (let ((good (pick-good clang-w-fodder)))
                              `(:insert-value ,good
                                              ,(pick-any-json clang-w-fodder
                                                              good)))))))
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
