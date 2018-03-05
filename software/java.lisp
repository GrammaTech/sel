;;; java.lisp --- java software representation
;;;
;;; DOCFIXME_JAVA Need a page or so introduction to java software objects.
;;;
;;; @texi{java}
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(defvar *java-execution-script-template*
  "#!/bin/sh
MYSELF=`which \"$0\" 2>/dev/null`
[ $? -gt 0 -a -f \"$0\" ] && MYSELF=\"./$0\"
java=java
if test -n \"$JAVA_HOME\"; then
    java=\"$JAVA_HOME/bin/java\"
fi
exec \"$java\" $java_args -jar $MYSELF \"$@\"
exit 0")

(defvar *java-mutator-execution-string* "java-mutator ~a"
   "The java-jar command to execute the java-mutator.jar")

(defvar *java-jar-exit-code* 242
 "Error exit code from the java-mutator.jar")


(define-software java (ast)
  ((genome    :initarg :genome :initform nil
              :copier :direct)
   (compiler  :initarg :compiler
              :accessor compiler :initform "java")
   (file-name :initarg :file-name
              :accessor file-name :initform nil))
  (:documentation "DOCFIXME_JAVA"))

(defmethod from-file ((obj java) path)
  "Populate software object upon initialization"
  (with-slots (genome ext file-name raw-size) obj
    (setf genome (file-to-string path)
          ext (pathname-type (pathname path))
          file-name (pathname-name (pathname path))
          raw-size (java-ids-aux obj)))
  obj)

(defmethod java-jar-exec (format-string)
  "Error code handler for error codes from the java-mutator.jar"
  (multiple-value-bind (stdout stderr exit-code)
      (shell *java-mutator-execution-string* format-string)
    (if (eq exit-code *java-jar-exit-code*)
        (error (make-condition 'mutate
                 :text (format nil "java-mutator exited with an error on ~s."
                               format-string)))
        (values stdout stderr exit-code))))

(defmethod java-ids-aux ((java java))
  (with-temp-file-of (src-file (ext java)) (genome java)
    (multiple-value-bind (stdout stderr errno)
        (java-jar-exec (format nil "-ids ~a" src-file))
      (declare (ignorable stderr))
      (if (not (zerop errno))
          (restart-case
              (error (make-condition 'mutate
                       :text (format nil "java-mutator exited with an error ~
                                          parsing the following genome: ~%~s"
                                     (genome java))))
            (keep-partial-asts ()
              :report "Ignore error retaining partial ASTs for software object."
              nil))
          (parse-number stdout)))))

(defmethod (setf genome) :before (new (obj java))
  (declare (ignorable new))
  (setf (slot-value obj 'raw-size) nil))


;;;; Core Mutations.
(defmethod java-insert-aux ((java java) stmt1 value1)
  (with-temp-file-of (src-file (ext java)) (genome java)
    (java-jar-exec (format nil "-insert ~a -out=~a -stmt1=~a -value=~a"
                           src-file
                           (directory-namestring src-file)
                           stmt1 value1))
    (setf (genome java) (file-to-string src-file)))
  java)

;;;; Mutation class definitions.
(defclass java-mutation (mutation) ()
  (:documentation "Base class for all Java mutations."))

;;; Insert
(define-mutation java-insert (java-mutation) ())

(defmethod build-op ((mutation java-insert) software)
  (declare (ignorable software))
  `((:insert . ,(targets mutation))))

;;; Helper method
(defmethod apply-mutation ((java java) (op list))
  "Helper method for initializing mutation objects when a list is provided."
  (apply-mutation java (make-instance (car op) :targets (cdr op))))

;;; TODO: add sort on statements, reference clang.lisp.
(defmethod apply-mutation ((software java) (mutation java-mutation))
  "Driver function to mutate java source.
Build-op function prepends the mutation function (cut, replace, swap,
insert)."
  (java-apply-mutation-ops software (build-op mutation software)))

(defun java-apply-mutation-ops (software ops)
  "Execute one of the operations and use the op-list to extract operands"
  (iter (for (op . properties) in ops)
        ;; Retrieve mutation properties.
        (let ((stmt1 (aget :stmt1 properties))
              (value1 (aget :value1 properties)))
          ;; Perform operation.
          (ecase op ; NOTE: We only support insertion not cut, replace, or swap?
            (:insert (java-insert-aux software stmt1 value1)))))
  software)


;;;; Non-mutation functions.
(defun java-make-literal (kind value)
  "Returns simple representations of the input literal."
  (ecase kind
    (:integer (values (format nil "~a" (round value))))
    (:float (values (format nil "~a" value)))
    (:string (values (format nil "~s" value)))))

(defmethod phenome ((obj java) &key (bin (temp-file-name)))
  (with-temp-dir (sandbox)
    (with-cwd (sandbox)
      (let ((bin (ensure-path-is-string bin))
            (file-path (namestring (make-pathname :directory sandbox
                                                  :name (file-name obj)
                                                  :type (ext obj)))))

        ;; Write genome to file in temporary directory.
        (with-open-file (out file-path :direction :output
                             :if-exists :supersede
                             :if-does-not-exist :create)
          (format out
                  (flet ((remove-package (x)
                           (let ((*allow-named-registers* t))
                             (regex-replace
                              (create-scanner "package [^;]+;") x ""))))
                    (remove-package (genome obj)))))

        ;; Create class file, then Manifest file, and finally the jar.
        (multiple-value-bind (stdout stderr errno)
            (shell "javac ~a.java && \
                  echo Main-Class: ~a > MANIFEST.MF && \
                  jar -cvmf MANIFEST.MF ~a.jar ~a.class"
                   (file-name obj) (file-name obj)
                   (file-name obj) (file-name obj))

          ;; Create the bin using the jar file and the input.
          (with-temp-file-of (script-file) *java-execution-script-template*
            (shell "cat ~a ~a.jar > ~a && chmod +x ~a"
                   script-file (file-name obj) bin bin))

          (values bin errno stderr stdout))))))

(defmethod size ((obj java))
  (or (raw-size obj)
      (setf (raw-size obj) (java-ids-aux obj))))

;;; TODO: Rename `clang-format' to `format' (or something that won't
;;;       conflict with a core CL function name.
(defmethod clang-format ((obj java) &optional style)
  (declare (ignorable style))
  obj)

;;; TODO: Rename to `indent' to `indent', or think of another way to
;;;       control if tidy or indent is run (e.g., a clang-specific
;;;       global configuration variable)..
(defmethod indent ((obj java) &optional style)
  (declare (ignorable style))
  obj)

;;; TODO: Rename to `clang-tidy' to `tidy'.
(defmethod clang-tidy ((obj java))
  "This method has no effect on Java objects."
  obj)
