;;; view.lisp --- view functions

;;; Commentary:

;;; Code:
(in-package :software-evolution-view)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (enable-curry-compose-reader-macros)
  (enable-interpol-syntax))

(defvar *view-stream* t
  "Dynamically bind to use modify.")

(defvar *view-length* 65
  "Dynamically bind to use modify.")

(defvar *view-running* nil
  "Set to nil to terminate the view thread.")

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
                           (left +b-lt+) (right +b-rt+))
  (let* ((values (or values (list value)))
         (colors (or colors (list color)))
         (remainder-length
          (- *view-length* (+ 2 (reduce #'+ (mapcar #'length values)))))
         (left-l (floor (* remainder-length balance)))
         (right-l (ceiling (* remainder-length (- 1 balance)))))
    (assert (and (>= left-l 0) (>= right-l 0))
            (left-l right-l)
            "Padding on one side is negative (~a,~a)" left-l right-l)
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
                                       (string right)))))
    (format *view-stream* "~%")))

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
                (format nil "~dh ~dm ~ds" hours minutes
                        (floor (* remainder 60)))))
            " evals: " (format nil "~f" *fitness-evals*)
            " last improv: " "????") 
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

(defun subtree-starting-with (token tree &key (test #'equalp))
  (if (funcall test token (car tree)) tree
      (car (remove nil (mapcar {subtree-starting-with token}
                               (remove-if-not #'listp tree))))))

(defun replace-subtree-starting-with (token replace tree &key (test #'equalp))
  (mapcar (lambda (subtree)
            (if (and (listp subtree) (car subtree))
                (if (funcall test token (car subtree))
                    replace
                    (replace-subtree-starting-with token replace subtree))
                subtree))
          tree))

(defmacro with-delayed-invocation (function &rest body)
  "Take a form with one function marked as DELAYED.
Rewrite into a form which calculates all arguments and returns a
lambda calling the delayed function on the arguments."
  (let ((syms-and-args (mapcar (lambda (arg) (list (gensym) arg))
                               (cdr (subtree-starting-with function body)))))
    (replace-subtree-starting-with
     function
     `(let ,syms-and-args
        (lambda () (,function ,@(mapcar #'car syms-and-args))))
     `(progn ,@body))))

(defun view-status (&rest args)
  (clear-terminal)
  (hide-cursor)
  (label-line-print
   :values (list " BED " +software-evolution-version+)
   :colors (list +color-YEL+ +color-CYA+)
   :balance 1/2
   :filler #\Space :left #\Space :right #\Space)
  (label-line-print :value " timing " :color +color-CYA+
                    :balance (/ (- 1 +golden-ratio+) 2)
                    :left +b-lt+ :right +b-rt+)
  (runtime-print)
  (label-line-print :value " population " :color +color-CYA+
                    :balance (/ (- 1 +golden-ratio+) 2)
                    :left +b-vr+ :right +b-vl+)
  ;; funcs
  (mapcar #'funcall args)
  (label-line-print :left +b-lb+ :right +b-rb+)
  (force-output *view-stream*))

(defun view-start (&key (delay 2))
  "Start a viewing thread regularly updating `view-status'."
  (setf *view-running* t)
  (make-thread
   (lambda ()
     (let ((*view-stream* *standard-output*))
       (iter
         (while *view-running*)
         (view-status
          ;; Fitness data informaion (pre-calculated).
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
                 (format nil "~d"
                         (/ (length (remove-duplicates
                                     (mapcar #'fitness *population*)
                                     :test #'equalp))
                            (length *population*))))
               (when vectorp
                 (format nil "~f"
                         (->> *population*
                              (mapcar [{coerce _ 'list} #'fitness])
                              (apply #'mapcar [{apply #'min} #'list])
                              (reduce #'+)))))))
          ;; Genomic data informaion (pre-calculated).
          (with-delayed-invocation genome-data-print
            (let ((lengths (mapcar [#'length #'lines] *population*)))
              (genome-data-print (format nil "~f" (apply #'min lengths))
                                 (format nil "~f" (median lengths))
                                 (format nil "~f" (apply #'max lengths))))))
         (sleep delay))))
   :name "view"))
