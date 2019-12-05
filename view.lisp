;;;; view.lisp --- view functions
;;;
;;; The SOFTWARE-EVOLUTION-VIEW (SE-VIEW) library provides a live
;;; status view which is continually updated during the course of an
;;; evolutionary run.  Currently the interface is maintained on STDOUT
;;; of the process running the SE library.  This view includes
;;; information on the progress and timing of the evolutionary run, the
;;; mutations used over the course of the run and their efficacy, and a
;;; summary of the `note' output printed during the run.
;;;
;;; To use SE-VIEW simply include it into your package, and then call
;;; `start-view' to begin maintain a status view on STDOUT.  The
;;; following global variables of the SE-VIEW package may be used to
;;; customize the appearance of the view output.
;;;
;;; * *VIEW-RUN-NAME* controls the title of the run
;;; * *VIEW-LENGTH* controls the maximum width of the printed view
;;; * *VIEW-DELAY* control the refresh rate of the view in seconds
;;; * *VIEW-MUTATION-HEADER-P* show headers of mutation view columns
;;; * *VIEW-MAX-MUTATIONS* number of top mutations to show
;;; * *VIEW-MAX-NOTE-LINES* number of notes lines to display
;;; * *VIEW-BEST-GENOME-LINES* show lines of the best genome
;;;
;;; @texi{view}
(defpackage :software-evolution-library/view
  (:nicknames :sel/view)
  (:use
   :common-lisp
   :alexandria
   :metabang-bind
   :named-readtables
   :curry-compose-reader-macros
   :arrow-macros
   :iterate
   :split-sequence
   :cl-ppcre
   :cl-store
   :cl-dot
   :diff
   :bordeaux-threads
   :software-evolution-library
   :software-evolution-library/utility
   :cl-interpol)
  (:shadow :diff)
  (:export :*view-stream*
           :*view-length*
           :*view-delay*
           :*view-running*
           :*view-application-name*
           :*view-application-version*
           :*view-run-name*
           :*view-mutation-header-p*
           :*view-max-mutations*
           :*view-max-note-lines*
           :*view-max-best-lines*
           :*view-max-best-offset*
           :*view-functions*
           ;; Colors.
           :+set-G1+
           :+reset-G1+
           :+b-start+
           :+b-stop+
           :+b-h+
           :+b-v+
           :+b-lt+
           :+b-rt+
           :+b-lb+
           :+b-rb+
           :+b-x+
           :+b-vr+
           :+b-vl+
           :+b-ht+
           :+b-hb+
           :+term-home+
           :+term-clear+
           :+ceol+
           :+cursor-hide+
           :+cursor-show+
           :+color-BLK+
           :+color-RED+
           :+color-GRN+
           :+color-BRN+
           :+color-BLU+
           :+color-MGN+
           :+color-CYA+
           :+color-NOR+
           :+color-GRA+
           :+color-LRD+
           :+color-LGN+
           :+color-YEL+
           :+color-LBL+
           :+color-PIN+
           :+color-LCY+
           :+color-BRI+
           :+color-RST+
           :+golden-ratio+
           :label-line-print
           ;; Utility functions.
           :best-print
           ;; View functions.
           :timing-view-function
           :population-label-view-function
           :fitness-view-function
           :genome-view-function
           :mutation-label-view-function
           :mutation-view-function
           :best-label-view-function
           :best-view-function
           :notes-label-view-function
           :notes-view-function
           ;; Interface functions.
           :with-delayed-invocation
           :view-truncate
           :view-start
           :view-controller-start))
(in-package :software-evolution-library/view)
(in-readtable :curry-compose-reader-macros)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-interpol-syntax)
  (defvar *view-stream* t
    "Dynamically bind to use modify."))

(defvar *view-length* 65
  "Dynamically bind to use modify.")

(defvar *view-delay* 2
  "Seconds to wait between updating the view.")

(defvar *view-running* nil
  "Set to nil to terminate the view thread.")

(defvar *view-application-name* "SEL"
  "Name of the application using the view.")

(defvar *view-application-version* +software-evolution-library-version+
  "Version string of the application using the view.")

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

(defvar *view-max-best-lines* 0
  "Number of lines of the best candidate to show.")

(defvar *view-max-best-offset* 0
  "Offset into the lines of the best candidate to show.")

(define-constant +golden-ratio+ 21/34)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; AFL, forgive me this.
  (mapc (lambda (triple)
          (destructuring-bind (name value documentation) triple
            (eval `(define-constant ,name ,value :test 'equalp
                                    :documentation ,documentation))))
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


;;; Utility functions

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

(defun string-output-stream-p (stream)
  (typep stream
         #+sbcl 'sb-impl::string-output-stream
         #+ccl  'ccl:string-output-stream
         #- (or sbcl ccl)
         (error "`string-output-stream-p' only supported for SBCL and CCL")))

(defun view-truncate (line &optional (less 2))
  (if (> (length line) (- *view-length* less))
      (subseq line 0 (- *view-length* less))
      line))


;;; View functions.

(defun runtime-print ()
  (if *start-time*
      (label-line-print
       :balance 0
       :colors (list +color-GRA+ +color-RST+
                     +color-GRA+ +color-RST+
                     +color-GRA+ +color-RST+)
       :values
       (remove nil
         (list
          " runtime: "
          (multiple-value-bind
                (days remainder) (floor (/ (elapsed-time) 86400))
            (multiple-value-bind
                  (hours remainder) (floor (/ (* remainder 86400) 3600))
              (multiple-value-bind
                    (minutes remainder) (floor (/ (* remainder 3600) 60))
                (format nil "~dd ~dh ~2,'0dm ~2,'0ds" days hours minutes
                        (floor (* remainder 60))))))
          " evals: "
          (format nil "~f" *fitness-evals*)
          (when (and *fitness-evals* (cdadr *mutation-improvements*))
            " last-improv: ")
          (when (and *fitness-evals* (cdadr *mutation-improvements*))
            (format nil "~d"
                    (- *fitness-evals*
                       (cdadr *mutation-improvements*))))))
       :filler #\Space :left +b-v+ :right +b-v+)
      (label-line-print
       :balance 0
       :value "No run started."
       :color +color-GRA+
       :filler #\Space :left +b-v+ :right +b-v+)))

(defun fitness-data-print (best med &optional union uniq)
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
               " union: " union
               " uniq: " uniq)))
   :filler #\Space :left +b-v+ :right +b-v+))

(defun genome-data-print (min med max)
  (label-line-print
   :balance 0
   :colors (list +color-PIN+
                 +color-GRA+ +color-RST+
                 +color-GRA+ +color-RST+
                 +color-GRA+ +color-RST+)
   :values (list "  length" " min: " min " med: " med " max: " max)
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
          (append stats
                  (list
                   (cons :total
                         (mapcar (lambda (key)
                                   (cons key
                                         (->> stats
                                              (mapcar [{aget key} #'cdr])
                                              (remove nil)
                                              (reduce #'+))))
                                 (list :better :dead :same :worse))))))))

(defun notes-print (lines)
  (mapc (lambda (line)
          (label-line-print :value line :color +color-GRA+ :balance 0
                            :filler #\Space :left +b-v+ :right +b-v+))
        lines))

(defun best-print (lines)
  (mapc (lambda (line)
          (label-line-print :value line :color +color-RST+ :balance 0
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

(defmacro with-delayed-invocation (spec &rest body)
  "Take a form with a function marked as DELAYED.
Argument SPEC should be a list holding a function name, and optionally
a form to evaluate to determine if the invocation should be
run (otherwise an empty function is returned).  Rewrite into a form
which calculates all arguments and returns a lambda calling the
delayed function on the arguments."
  (let* ((function (first spec))
         (conditional (second spec))
         (syms-and-args (mapcar (lambda (arg) (list (gensym) arg))
                                (cdr (subtree-starting-with function body)))))
    (assert syms-and-args (function body)
            "Couldn't find instance of ~a in ~a" function body)
    (let ((body (replace-subtree-starting-with
                 function
                 `(let ,syms-and-args
                    (lambda () (,function ,@(mapcar #'car syms-and-args))))
                 `(progn ,@body))))
      (if conditional
          `(if ,conditional ,body (lambda ()))
          body))))


;;; View functions
;;;
;;; TODO: Determine how to setup this list with functions s.t. they
;;;       are evaluated *first* to populate arguments (previously done
;;;       as arguments to view were evaluated) and then second to
;;;       actually do the display.
;;;
;;;       Each of these functions should be a function which is
;;;       evaluated to return a function to do the printing.

(defun timing-view-function ()
  (lambda ()
    (label-line-print :value " timing " :color +color-CYA+
                      :balance (/ (- 1 +golden-ratio+) 2)
                      :left +b-lt+ :right +b-rt+)
    (runtime-print)))

(defun population-label-view-function ()
  (lambda ()
    (when (and *population* (every #'fitness *population*))
      (label-line-print :value " population " :color +color-CYA+
                        :balance (/ (- 1 +golden-ratio+) 2)
                        :left +b-vr+ :right +b-vl+))))

(defun filter-population-by-fitness-type (population
                                          &key
                                            (type (if (numberp
                                                       (fitness
                                                        (first population)))
                                                      :scalar
                                                      :vector)))
  "Filter POPULATION so that all members have TYPE (:VECTOR or :SCALAR) fitness.
If TYPE is not specified, use the fitness type of the first member of
the population."
  (remove-if-not (ecase type
                   (:scalar #'numberp)
                   (:vector #'vectorp))
                 population :key #'fitness))

(defun fitness-view-function ()
  ;; Fitness data information (pre-calculated).
  (with-delayed-invocation (fitness-data-print
                            (and *population* (every #'fitness *population*)))
    (let* ((vectorp (not (numberp (fitness (car *population*)))))
           ;; only include variants that have the right type (numeric or vector)
           (fit-population (filter-population-by-fitness-type
                            *population*
                            :type (if vectorp :vector :scalar)))
           (fits (mapcar (if vectorp
                             [{reduce #'+} #'fitness]
                             #'fitness)
                         fit-population)))
      (fitness-data-print
       (format nil "~6f" (extremum fits *fitness-predicate*))
       (format nil "~6f" (median fits))
       (when vectorp
         (format nil "~6f"
                 (->> fit-population
                      (mapcar [{coerce _ 'list} #'fitness])
                      (apply #'mapcar [{apply #'min} #'list])
                      (reduce #'+))))
       (when vectorp
         (format nil "~d"
                 (/ (length (remove-duplicates
                             (mapcar #'fitness fit-population)
                             :test #'equalp))
                    (length fit-population))))))))

(defun genome-view-function ()
  ;; Genomic data informaion (pre-calculated).
  (with-delayed-invocation (genome-data-print *population*)
    (let ((lengths
           (without-compiler-notes
               (mapcar [#'length #'lines] *population*))))
      (genome-data-print (format nil "~d" (apply #'min lengths))
                         (format nil "~d" (median lengths))
                         (format nil "~d" (apply #'max lengths))))))

(defun mutation-label-view-function ()
  (lambda ()
    (when (and (> *view-max-mutations* 0)
               (> (hash-table-count *mutation-stats*) 0))
      (label-line-print :value " mutations " :color +color-CYA+
                        :balance (/ (- 1 +golden-ratio+) 2)
                        :left +b-vr+ :right +b-vl+))))

(defun mutation-view-function ()
  (with-delayed-invocation
      (mutation-stats-print
       (and (> *view-max-mutations* 0)
            (> (hash-table-count *mutation-stats*) 0)))
    (mutation-stats-print
     (take *view-max-mutations*
           (sort (summarize-mutation-stats) #'>
                 :key (lambda (mut)
                        (/ (or (aget :better (cdr mut)) 0)
                           (reduce #'+ (mapcar
                                        #'cdr (cdr mut))))))))))

(defun best-label-view-function ()
  (lambda ()
    (when (and (> *view-max-best-lines* 0)
               *population*
               (every #'fitness *population*))
      (label-line-print :value " best " :color +color-CYA+
                        :balance (/ (- 1 +golden-ratio+) 2)
                        :left +b-vr+ :right +b-vl+))))

(defun best-view-function ()
  (with-delayed-invocation (best-print (and (> *view-max-best-lines* 0)
                                            *population*
                                            (every #'fitness *population*)))
    (best-print (-<>> (filter-population-by-fitness-type *population*)
                      (extremum <> #'fitness-better-p :key #'fitness)
                      (lines)
                      (mapcar #'view-truncate)
                      ((lambda (lines) ; Allow scrolling through the best lines.
                         (drop (max (min *view-max-best-offset*
                                         (1- (- (length lines)
                                                *view-max-best-lines*)))
                                    0)
                               lines)))
                      (append <> (make-list *view-max-best-lines*
                                            :initial-element ""))
                      (take *view-max-best-lines*)))))

(defun notes-label-view-function ()
  (lambda ()
    (when (> *view-max-note-lines* 0)
      (label-line-print :value " notes " :color +color-CYA+
                        :balance (/ (- 1 +golden-ratio+) 2)
                        :left +b-vr+ :right +b-vl+))))

(defun notes-view-function ()
  (with-delayed-invocation (notes-print (> *view-max-note-lines* 0))
    (notes-print
     (setf *view-cached-note-lines*
           (-<>> *note-out*
                 (find-if #'string-output-stream-p)
                 (get-output-stream-string)
                 (split-sequence #\Newline <>
                                 :remove-empty-subseqs t)
                 (mapcar
                  (lambda (line)
                    (multiple-value-bind (start end)
                        (scan "^;;\\d\\d\\d\\d.\\d\\d.\\d\\d." line)
                      (if start
                          (subseq line end)
                          line))))
                 (mapcar #'view-truncate)
                 (reverse)
                 (append <> *view-cached-note-lines*)
                 (take *view-max-note-lines*))))))

(defvar *view-functions*
  '(timing-view-function
    population-label-view-function
    fitness-view-function
    genome-view-function
    mutation-label-view-function
    mutation-view-function
    best-label-view-function
    best-view-function
    notes-label-view-function
    notes-view-function)
  "List of the functions called to populate the view.
View functions return functions which print view information.  All
functions in this list should return functions to do the printing.
The reason for this two stage call-return-call process is to allow
expensive operations to be performed *before* the view is being
updated so that the returned printing functions execute quickly
avoiding flickering in the terminal display.  The
`with-delayed-invocation' macro helps with the caching of computation
and returning of quickly-executing printing functions.")


;;; Interface functions
(defun view-status ()
  (clear-terminal)
  (hide-cursor)
  (label-line-print
   :values (append (list (format nil " ~a " *view-application-name*)
                         *view-application-version*)
                   (when *view-run-name*
                     (list (format nil " (~a)" *view-run-name*))))
   :colors (list +color-YEL+ +color-CYA+ +color-LGN+)
   :balance 1/2
   :inhibit-newline t
   :filler #\Space :left #\Space :right #\Space)
  ;; Double-evaluate the view functions first to return the printing
  ;; functions, then to quickly call the printing functions.
  (->> *view-functions*
       (mapcar #'funcall)
       (mapcar #'funcall))
  (label-line-print :left +b-lb+ :right +b-rb+)
  (force-output *view-stream*))

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
         (view-status)
         (sleep *view-delay*))))
   :name "view"))

(defun view-help ()
  (clear-terminal)
  (label-line-print :value "TODO: HELP" :left +b-lb+ :right +b-rb+))


;;; Control functions
(defun handle-user-input (command-char)
  ;; TODO: limits on changes to offset
  ;; TODO: horizontal scrolling
  ;; TODO: help on ?
  ;; TODO: signals for refresh, for more responsive commands
  (case command-char
    (#\k (decf *view-max-best-offset*))
    (#\j (incf *view-max-best-offset*))
    (#\u (decf *view-max-best-offset* *view-max-best-lines*))
    ((#\Space #\n #\d) (incf *view-max-best-offset* *view-max-best-lines*))
    (#\g (setf *view-max-best-offset* 0))
    (#\G (let ((best (extremum *population* #'fitness-better-p :key #'fitness)))
           (setf *view-max-best-offset* (max 0 (- (length (lines best))
                                                  *view-max-best-lines*)))))
    (#\? (view-help))
    (otherwise (note 3 "Unknown command char ~S" command-char)))
  ;; Return nil on q or Q to terminate.
  (not (or (equal command-char #\q)
           (equal command-char #\Q))))

(defun view-controller-start ()
  (make-terminal-raw)
  (make-thread
   (lambda ()
     (iter (for command-char = (read-char))
           (while (handle-user-input command-char)))
     :name "view-controller")))
