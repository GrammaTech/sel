;;; asm-parse.lisp --- parse assembly code into asm-line-info structs
(in-package :software-evolution-library)
(in-readtable :curry-compose-reader-macros)

(defstruct asm-line-info
  text    ;; original text
  tokens  ;; list of tokens after parsing
  type    ;; empty (white space/comments), decl, data, label-decl, op)
  label   ;; for operations which refer to labels
  opcode  ;; for operations
  )

(defvar *assembler-x86-readtable* (copy-readtable))

;; 
;; treat some characters such as : and , as special tokens
;;
(set-macro-character #\: (lambda (stream ch)(declare (ignore stream ch)) :colon)
		     nil
		     *assembler-x86-readtable*)

(set-macro-character #\, (lambda (stream ch)(declare (ignore stream ch)) :comma)
		     nil
		     *assembler-x86-readtable*)
;;;
;;; takes a line of text from a .asm file, and, and converts it to tokens
;;;
(defun tokenize-asm-line (line)
  (with-input-from-string (s line)
    (do* ((*readtable* *assembler-x86-readtable*)
	  (result '())
	  (eof (cons 0 0))
	  (token (read s nil eof)(read s nil eof)))
	 ((eq token eof)(nreverse result))
	(push token result))))

;;;
;;; Given a list of tokens representing the line, returns either of:
;;;     :nothing
;;;     :declaration
;;;     :data
;;;     :label
;;;     :operation
(defun parse-line-type (tokens)
  (cond ((null tokens) ':empty)
	((and (symbolp (first tokens))  ; is first token a symbol beginning with '$'?
	      (char= (char (symbol-name (first tokens)) 0) #\$)
	      (> (length tokens) 1)
	      (eq (second tokens) :colon)) ; followed by a ':'?
	 ':label-decl)
	((or (member 'db tokens)(member 'dq tokens)(member 'dd tokens))
	 ':data)
	((member (first tokens) '(push pop lea sub mov sar call test jz zor nop
				     add cmp mul div jnz ja jb jl jg jlt jgt je jne
				     movzx movq movsd movupd jmp leave pxor ucomisd jp
				     pxor ret hlt mulsd subsd))
	 ':op)
	(t ':decl)))     ;; use this as catch-all for anything else

;;;
;;; takes a line of text from a .asm file, and, and returns 1 or 2 asm-line-info structs.
;;; If the line begins with a label, the line is split into two lines: the label,
;;; and the remaining text/tokens. In this case 2 asm-line-info structs are returned.
;;; Otherwise, as single asm-line-info is returned.
;;;
(defun parse-asm-line (line)
  (let ((tokens (tokenize-asm-line line))
	(info (make-asm-line-info)))
    (setf (asm-line-info-text info) line)
    (setf (asm-line-info-tokens info) tokens)

    ;; determine type of line
    (let ((line-type (parse-line-type tokens)))
      (setf (asm-line-info-type info) line-type)
      
      (case line-type
	(:label-decl (let* ((label (first tokens))
		       (label-end (position #\: line))
		       (line1 (subseq line 0 (+ label-end 1)))
		       (line2 (subseq line (+ label-end 1)))
		       (next-info (parse-asm-line line2))) ;; recurse!
		  (setf (asm-line-info-text info) line1)
		  (setf (asm-line-info-tokens info) (list label ':colon))
		  (setf (asm-line-info-label info) label)
		  (list info next-info)))
	(:empty (list info))
	(:op (setf (asm-line-info-opcode info) (first tokens))(list info))
	(:data (list info))
	(:decl (list info))))))

