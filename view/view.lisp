;;; view.lisp --- view functions

;;; Commentary:

;; The SOFTWARE-EVOLUTION-VIEW (SE-VIEW) library provides a live
;; status view which is continually updated during the course of an
;; evolutionary run.  Currently the interface is maintained on STDOUT
;; of the process running the SE library.  This view includes
;; information on the progress and timing of the evolutionary run, the
;; mutations used over the course of the run and their efficacy, and a
;; summary of the `note' output printed during the run.
;;
;; To use SE-VIEW simply include it into your package, and then call
;; `start-view' to begin maintain a status view on STDOUT.  The
;; following global variables of the SE-VIEW package may be used to
;; customize the appearance of the view output.
;;
;; *view-run-name* ----------- controls the title of the run
;; *view-length* ------------- controls the maximum width of the printed view
;; *view-delay* -------------- control the refresh rate of the view in seconds
;; *view-mutation-header-p* -- show headers of mutation view columns
;; *view-max-mutations* ------ number of top mutations to show
;; *view-max-note-lines* ----- number of notes lines to display

;;; Code:
(in-package :software-evolution-view)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros)
  (enable-interpol-syntax))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *view-stream* t
    "Dynamically bind to use modify."))

(defvar *view-length* 65
  "Dynamically bind to use modify.")

(defvar *view-delay* 2
  "Seconds to wait between updating the view.")

(defvar *view-running* nil
  "Set to nil to terminate the view thread.")

(defvar *view-run-name* nil
  "Set the name of the current run.
For example a description of the evolution target.")

(defvar *view-mutation-header-p* t
  "Show headers of mutation stats.")

(defvar *view-max-mutations* 0
  "Maximum number of mutations to show.")

(defvar *view-max-note-lines* 12
  "Maximum number of lines of notes to show.")

(defvar *view-cached-note-lines* nil
  "Holds cached note lines for continued display.")

(define-constant +golden-ratio+ 21/34)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; AFL, forgive me this.
  (mapc (lambda-bind ((name value documentation))
          (eval `(define-constant ,name ,value :test 'equalp
                                  :documentation ,documentation)))
        '((+set-G1+      #?"\x1b)0"   "Set G1 for box drawing")
          (+reset-G1+    #?"\x1b)B"   "Reset G1 to ASCII")
          (+b-start+     #?"\x0e"     "Enter G1 drawing mode")
          (+b-stop+      #?"\x0f"     "Leave G1 drawing mode")
          (+b-h+         #\q        "Horizontal line")
          (+b-v+         #\x        "Vertical line")
          (+b-lt+        #\l        "Left top corner")
          (+b-rt+        #\k        "Right top corner")
          (+b-lb+        #\m        "Left bottom corner")
          (+b-rb+        #\j        "Right bottom corner")
          (+b-x+         #\n        "Cross")
          (+b-vr+        #\t        "Vertical, branch right")
          (+b-vl+        #\u        "Vertical, branch left")
          (+b-ht+        #\v        "Horizontal, branch top")
          (+b-hb+        #\w        "Horizontal, branch bottom")
          (+term-home+   #?"\x1b[H"     "Set terminal back to home (top left).")
          (+term-clear+  #?"\x1b[H[2J" "Clear terminal.")
          (+ceol+        #?"\x1b[0K"    "Clear to end of line.")
          (+cursor-hide+ #?"\x1b[?25l"  "Hide the cursor.")
          (+cursor-show+ #?"\x1b[?25h"  "Show the cursor.")
          ;; Colors
          (+color-BLK+   #?"\x1b[0;30m" "Color BLK.")
          (+color-RED+   #?"\x1b[0;31m" "Color RED.")
          (+color-GRN+   #?"\x1b[0;32m" "Color GRN.")
          (+color-BRN+   #?"\x1b[0;33m" "Color BRN.")
          (+color-BLU+   #?"\x1b[0;34m" "Color BLU.")
          (+color-MGN+   #?"\x1b[0;35m" "Color MGN.")
          (+color-CYA+   #?"\x1b[0;36m" "Color CYA.")
          (+color-NOR+   #?"\x1b[0;37m" "Color NOR.")
          (+color-GRA+   #?"\x1b[1;30m" "Color GRA.")
          (+color-LRD+   #?"\x1b[1;31m" "Color LRD.")
          (+color-LGN+   #?"\x1b[1;32m" "Color LGN.")
          (+color-YEL+   #?"\x1b[1;33m" "Color YEL.")
          (+color-LBL+   #?"\x1b[1;34m" "Color LBL.")
          (+color-PIN+   #?"\x1b[1;35m" "Color PIN.")
          (+color-LCY+   #?"\x1b[1;36m" "Color LCY.")
          (+color-BRI+   #?"\x1b[1;37m" "Color BRI.")
          (+color-RST+   #?"\x1b[0m"    "Color RST."))))

(defun clear-terminal ()
  (format *view-stream* "~a" +term-clear+))

(defun hide-cursor ()
  (format *view-stream* "~a" +cursor-hide+))

(defun show-cursor ()
  (format *view-stream* "~a" +cursor-show+))

(defmacro with-line-printing (&rest body)
  `(unwind-protect
        (progn (format ,*view-stream* "~a" +set-G1+)
               (format ,*view-stream* "~a" +b-start+)
               ,@body)
     (format ,*view-stream* "~a" +b-stop+)
     (format ,*view-stream* "~a" +reset-G1+)))

(defmacro with-color-printing (color &rest body)
  `(unwind-protect
        (progn (format ,*view-stream* "~a" ,color) ,@body)
     (format ,*view-stream* "~a" +color-RST+)))

(defun label-line-print (&key (value "") (values)
                           (color +color-RST+) (colors)
                           (balance (- 1 +golden-ratio+))
                           (filler-color +color-GRA+) (filler +b-h+)
                           (left +b-lt+) (right +b-rt+)
                           (inhibit-newline))
  (let* ((values (or values (list value)))
         (colors (or colors (list color)))
         (remainder-length
          (- *view-length* (+ 2 (reduce #'+ (mapcar #'length values)))))
         (left-l (floor (* remainder-length balance)))
         (right-l (ceiling (* remainder-length (- 1 balance)))))
    (assert (and (>= left-l 0) (>= right-l 0))
            (left-l right-l)
            "Padding on one side is negative (~a,~a)" left-l right-l)
    (unless inhibit-newline (format *view-stream* "~&"))
    (with-color-printing filler-color
      (with-line-printing
          (format *view-stream* "~a" (concatenate 'string
                                       (string left)
                                       (make-string left-l
                                                    :initial-element filler)))))
    (mapc (lambda (value color)
            (with-color-printing color (format *view-stream* value)))
          values colors)
    (with-color-printing filler-color
      (with-line-printing
          (format *view-stream* "~a" (concatenate 'string
                                       (make-string right-l
                                                    :initial-element filler)
                                       (string right)))))))

(defun runtime-print ()
  (label-line-print
   :balance 0
   :colors (list +color-GRA+ +color-RST+
                 +color-GRA+ +color-RST+
                 +color-GRA+ +color-RST+)
   :values (list
            " runtime: "
            (multiple-value-bind
                  (hours remainder) (floor (/ (elapsed-time) 3600))
              (multiple-value-bind
                    (minutes remainder) (floor (/ (* remainder 3600) 60))
                (format nil "~dh ~2,'0dm ~2,'0ds" hours minutes
                        (floor (* remainder 60)))))
            " evals: " (format nil "~f" *fitness-evals*)
            " last-improv: "
            (format nil "~d" (- *fitness-evals*
                                (cdadr *mutation-improvements*))))
   :filler #\Space :left +b-v+ :right +b-v+))

(defun fitness-data-print (best med &optional uniq union)
  (label-line-print
   :balance 0
   :colors (append
            (list +color-PIN+ +color-GRA+ +color-RST+ +color-GRA+ +color-RST+)
            (list +color-GRA+ +color-RST+ +color-GRA+ +color-RST+))
   :values (append
            (list
             " fitness"
             " best: " best
             " med: " med)
            (when (and uniq union)
              (list
               " uniq: " uniq
               " union: " union)))
   :filler #\Space :left +b-v+ :right +b-v+))

(defun genome-data-print (max med min)
  (label-line-print
   :balance 0
   :colors (list +color-PIN+
                 +color-GRA+ +color-RST+
                 +color-GRA+ +color-RST+
                 +color-GRA+ +color-RST+)
   :values (list "  length" " min: " max " med: " med " max: " min)
   :filler #\Space :left +b-v+ :right +b-v+))

(defun mutation-stats-print (stats)
  (let ((longest (extremum (mapcar [#'length #'symbol-name #'car] stats) #'>))
        (keys '(:better :same :worse :dead)))
    (when *view-mutation-header-p*
      (label-line-print
       :balance 0
       :colors (mapcar (constantly +color-PIN+) (append (list 1 2 3) keys))
       :values (append (cons (format nil " ~V@a " longest "MUTATION")
                             (mapcar (lambda (key) (format nil " ~6@a" key)) keys))
                       (list " /" " total"))
       :filler #\Space :left +b-v+ :right +b-v+))
    (mapc (lambda (mut)
            (label-line-print
             :balance 0
             :colors
             (append (cons +color-GRA+
                           (mapcar (constantly +color-RST+) keys))
                     (list +color-GRA+ +color-RST+))
             :values
             (append (list (format nil " ~V@a:" longest (car mut)))
                     (mapcar (lambda (key)
                               (format nil " ~6d"
                                       (or (aget key (cdr mut)) 0)))
                             keys)
                     (list " / "
                           (format nil "~d"
                                   (reduce #'+ (mapcar #'cdr (cdr mut))))))
             :filler #\Space :left +b-v+ :right +b-v+))
          stats)))

(defun notes-print (lines)
  (mapc (lambda (line)
          (label-line-print :value line :color +color-GRA+ :balance 0
                            :filler #\Space :left +b-v+ :right +b-v+))
        lines))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun subtree-starting-with (token tree &key (test #'equalp))
    (if (funcall test token (car tree)) tree
        (car (remove nil (mapcar {subtree-starting-with token}
                                 (remove-if-not #'listp tree)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun replace-subtree-starting-with (token replace tree &key (test #'equalp))
    (mapcar (lambda (subtree)
              (if (and (listp subtree) (car subtree))
                  (if (funcall test token (car subtree))
                      replace
                      (replace-subtree-starting-with token replace subtree))
                  subtree))
            tree)))

(defmacro with-delayed-invocation (function &rest body)
  "Take a form with one function marked as DELAYED.
Rewrite into a form which calculates all arguments and returns a
lambda calling the delayed function on the arguments."
  (let ((syms-and-args (mapcar (lambda (arg) (list (gensym) arg))
                               (cdr (subtree-starting-with function body)))))
    (assert syms-and-args (function body)
            "Couldn't find instance of ~a in ~a" function body)
    (replace-subtree-starting-with
     function
     `(let ,syms-and-args
        (lambda () (,function ,@(mapcar #'car syms-and-args))))
     `(progn ,@body))))

(defun view-status (&rest args)
  (clear-terminal)
  (hide-cursor)
  (label-line-print
   :values (append (list " SEL " +software-evolution-version+)
                   (when *view-run-name*
                     (list (format nil " (~a)" *view-run-name*))))
   :colors (list +color-YEL+ +color-CYA+ +color-LGN+)
   :balance 1/2
   :inhibit-newline t
   :filler #\Space :left #\Space :right #\Space)
  ;; funcs
  (mapcar #'funcall args)
  (label-line-print :left +b-lb+ :right +b-rb+)
  (force-output *view-stream*))

(defun string-output-stream-p (stream)
  (typep stream
         #+sbcl 'sb-impl::string-output-stream
         #-sbcl (error "`string-output-stream-p' only supported for SBCL.")))

(defun view-truncate (line &optional (less 6))
  (if (> (length line) (- *view-length* less))
      (subseq line 0 (- *view-length* less))
      line))

(defun view-start ()
  "Start a viewing thread regularly updating `view-status'.
Optional argument DELAY controls the rate at which the view refreshes."
  (setf *view-running* t)
  (unless
      (or (zerop *view-max-note-lines*) ; If we want to show notes,
          (some #'string-output-stream-p *note-out*)) ; and need string stream.
    (push (make-string-output-stream) *note-out*))
  (make-thread
   (lambda ()
     (let ((*view-stream* *standard-output*))
       (iter
         (while *view-running*)
         (view-status
          (lambda ()
            (label-line-print :value " timing " :color +color-CYA+
                              :balance (/ (- 1 +golden-ratio+) 2)
                              :left +b-lt+ :right +b-rt+)
            (runtime-print))
          (lambda ()
            (label-line-print :value " population " :color +color-CYA+
                              :balance (/ (- 1 +golden-ratio+) 2)
                              :left +b-vr+ :right +b-vl+))
          ;; Fitness data informaion (pre-calculated).
          (lambda ()
            (when *population*
              (funcall
               (with-delayed-invocation fitness-data-print
                 (let* ((vectorp (not (numberp (car *population*))))
                        (fits (mapcar (if vectorp
                                          [{reduce #'+} #'fitness]
                                          #'fitness)
                                      *population*)))
                   (fitness-data-print
                    (format nil "~f" (extremum fits *fitness-predicate*))
                    (format nil "~f" (median fits))
                    (when vectorp
                      (format nil "0~4f"
                              (/ (length (remove-duplicates
                                          (mapcar #'fitness *population*)
                                          :test #'equalp))
                                 (length *population*))))
                    (when vectorp
                      (format nil "~f"
                              (->> *population*
                                   (mapcar [{coerce _ 'list} #'fitness])
                                   (apply #'mapcar [{apply #'min} #'list])
                                   (reduce #'+))))))))))
          ;; Genomic data informaion (pre-calculated).
          (lambda ()
            (when *population*
              (funcall
               (with-delayed-invocation genome-data-print
                 (let ((lengths (mapcar [#'length #'lines] *population*)))
                   (genome-data-print (format nil "~d" (apply #'min lengths))
                                      (format nil "~d" (median lengths))
                                      (format nil "~d" (apply #'max lengths))))))))
          ;; Mutations
          (lambda ()
            (unless (or (zerop *view-max-mutations*)
                        (zerop (hash-table-count *mutation-stats*)))
              (label-line-print :value " mutations " :color +color-CYA+
                                :balance (/ (- 1 +golden-ratio+) 2)
                                :left +b-vr+ :right +b-vl+)))
          (lambda ()
            (unless (or (zerop *view-max-mutations*)
                        (zerop (hash-table-count *mutation-stats*)))
              (funcall
               (with-delayed-invocation mutation-stats-print
                 (mutation-stats-print
                  (take *view-max-mutations*
                        (sort (summarize-mutation-stats) #'>
                              :key (lambda (mut)
                                     (/ (or (aget :better (cdr mut)) 0)
                                        (reduce #'+ (mapcar
                                                     #'cdr (cdr mut))))))))))))
          ;; Notes.
          (lambda ()
            (unless (zerop *view-max-note-lines*)
              (label-line-print :value " notes " :color +color-CYA+
                                :balance (/ (- 1 +golden-ratio+) 2)
                                :left +b-vr+ :right +b-vl+)))
          (lambda ()
            (unless (zerop *view-max-note-lines*)
              (funcall
               (with-delayed-invocation notes-print
                 (notes-print
                  (setf *view-cached-note-lines*
                        (-<>> *note-out*
                              (find-if #'string-output-stream-p)
                              (get-output-stream-string)
                              (split-sequence #\Newline <>
                                              :remove-empty-subseqs t)
                              (mapcar {view-truncate _ 6})
                              (reverse)
                              (append <> *view-cached-note-lines*)
                              (take *view-max-note-lines*)))))))))
         (sleep *view-delay*))))
   :name "view"))
