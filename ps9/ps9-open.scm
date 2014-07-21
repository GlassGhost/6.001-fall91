;;; -*- Mode:Scheme; Base:10 -*-  PS9-OPEN.SCM

;;		     MASSACHUSETTS INSTITUTE OF TECHNOLOGY
;;	   Department of Electrical Engineering and Computer Science
;;	   6.001---Structure and Interpretation of Computer Programs
;;			     Fall Semester, 1991
;;
;;				 Problem Set 9

;;;; Compiler modifications for open-coded primitives with one and two operands

;;; This flag permits you to turn open coding on and off
(define open-code? false)

(define ( open-code!) (set! open-code?  true)  'open-coding)
(define (close-code!) (set! open-code? false) 'close-coding)

;;; Redefine this procedure from the standard compiler
(define (compile-application app c-t-env target cont)
  (if (and open-code? (primitive-application? app))
      (open-code-primitive-application app c-t-env target cont)
      (compile-unknown-application app c-t-env target cont)))

;;; This is the default compilation method, the same as
;;; COMPILE-APPLICATION in the unmodified compiler.
(define (compile-unknown-application app c-t-env target cont)
  (preserving
   'env
   (compile-expression (operator app) c-t-env 'fun 'next)
   (preserving 'fun
               (compile-operands (operands app) c-t-env)
               (compile-call target cont))))

;;; Test for recognizing primitive applications
(define (primitive-application? app)
  (memq  (operator app) (primitive-procedure-names)))

;;; We'll open code primitive applications only when there are
;;; one or two arguments.
(define (open-code-primitive-application app c-t-env target cont)
  (let ((l (length (operands app))))
    (cond ((= l 1) (open-code-unary-primitive-application app c-t-env target
							  cont))
	  ((= l 2) (open-code-binary-primitive-application app c-t-env target
							   cont))
	  (else (compile-unknown-application app c-t-env target cont)))))

;;; For one argument, we compile code that evaluates the operand and places it
;;; into the ARG1 register.  Then we append the instruction:
;;;      (ASSIGN <target> (<op> (FETCH ARG1)))
(define (open-code-unary-primitive-application app c-t-env target cont)
  (let ((operand-code
	 (compile-expression (first-operand (operands app))
			     c-t-env
			     'arg1
			     'next)))
    (append-instruction-sequences
     operand-code
     (make-register-assignment target
			       (make-operation (operator app)
					       (make-fetch 'arg1)))
     (compile-continuation cont))))

;;; A selector that gets the second operand.
(define (second-operand app) (cadr app))

;;; For two operands we compile code that evaluates the first operand and
;;; places the result in ARG1, and code that evaluates the second operand
;;; and places the result in ARG2.  We then paste together both instruction
;;; sequences, along with the instruction:
;;;      (ASSIGN <target> (<op> (FETCH ARG1) (FETCH ARG2)))

;;; We'll leave this procedure for you to complete.  The tricky part is
;;; to realize that certain registers have to be preserved, when the
;;; instruction sequences are glued together.  You should use the
;;; the PRESERVING operation (see text, page 447--448) to acomplish this.

(define (open-code-binary-primitive-application app c-t-env target cont)
  "<this is left for you to complete>"
  )
