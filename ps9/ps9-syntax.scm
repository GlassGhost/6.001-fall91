;;; -*- Mode:Scheme; Base:10 -*- PS9-SYNTAX.SCM

;;		     MASSACHUSETTS INSTITUTE OF TECHNOLOGY
;;	   Department of Electrical Engineering and Computer Science
;;	   6.001---Structure and Interpretation of Computer Programs
;;			     Fall Semester, 1991
;;
;;				 Problem Set 9

;;;; Syntax for simulation of explicit-control evaluator -- from section 4.1.2
;;;
;;; Slightly modified to abstract out common structure of special forms
;;;  and to support explicit tail recursion (as per section 5.2.1).

;; Self-evaluating entities

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)))	; Our prompt (viz., "EC-EVAL==> ") is a string.

;; Variables

(define (variable? exp) (symbol? exp))

(define (same-variable? var1 var2) (eq? var1 var2))  ;; Nice abstraction

;; Special forms (in general)

(define (form-with-tag? tag exp)		;; Exploit the fact that Scheme
  (cond ((not (atom? exp))			;; special forms are in essence
	 (eq? (car exp) tag))			;; type-tagged by the special
	(else false)))				;; form name.

;; Quotations

(define (quoted? exp) (form-with-tag? 'quote exp))

(define (text-of-quotation quot) (cadr quot))

;; Assignment--- SET!

(define (assignment? exp) (form-with-tag? 'set! exp))

(define (assignment-variable assn) (cadr  assn))
(define (assignment-value    assn) (caddr assn))

;; Definitions

(define (definition? exp) (form-with-tag? 'define exp))

(define (definition-variable defn)
  (if (variable? (cadr defn))			;;   (DEFINE  foo      ...)
      (cadr  defn)
      (caadr defn)))				;;   (DEFINE (foo ...) ...)

(define (definition-value defn)
  (if (variable? (cadr defn))			;;   (DEFINE  foo        ...)
      (caddr defn)
      (cons 'lambda				;;   (DEFINE (foo p...) b...)
            (cons (cdadr defn)			;; = (DEFINE  foo
                  (cddr  defn)))))		;;     (LAMBDA (p...) b...))


;; LAMBDA expressions

(define (lambda? exp) (form-with-tag? 'lambda exp))
(define (lambda-parameters lambda-exp) (cadr lambda-exp))
(define (lambda-body       lambda-exp) (cddr lambda-exp))

;; Conditionals

(define (conditional? exp) (form-with-tag? 'cond exp))

(define (clauses cndl) (cdr cndl))
(define (   no-clauses? clauses) (null? clauses))
(define (first-clause   clauses) (car   clauses))
(define ( rest-clauses  clauses) (cdr   clauses))
(define ( else-clause?  clause ) (eq? (predicate clause) 'else))

(define (predicate clause) (car clause))
(define (actions   clause) (cdr clause))
(define (true? x) x)

;; LET expressions

(define (let? exp) (form-with-tag? 'let exp))
(define (let-bindings exp) (cadr exp))
(define (let-body     exp) (cddr exp))

(define let-binding-variable car )		; binding is list: (var val)
(define let-binding-value    cadr)

;; Sequences

(define (sequence? exp) (form-with-tag? 'sequence exp))
(define (sequence-actions seq-exp) (cdr seq-exp))

(define ( last-exp? seq) (null? (cdr seq)))
(define (first-exp  seq) (car seq))
(define ( rest-exps seq) (cdr seq))

;; Procedure applications -- NO-ARGS? and LAST-OPERAND? added
;;                           APPLICATION? changed (from 5.2.1)

(define (no-args? exp)				;; Added for tail recursion
  (if (atom? exp)
      false
      (null? (cdr exp))))

(define (args-application? exp)			;; Changed from 5.2.1
  (if (atom? exp)
      false
      (not (null? (cdr exp)))))

(define (operator app) (car app))
(define (operands app) (cdr app))

(define (last-operand? args)			;; Added for tail recursion
  (null? (cdr args)))

(define (   no-operands? args) (null? args))
(define (first-operand   args) (car   args))
(define ( rest-operands  args) (cdr   args))

;; Compound procedures

(define (make-procedure lambda-exp env)
  (list      'procedure lambda-exp env))

(define (compound-procedure? exp) (form-with-tag? 'procedure exp))

(define (parameters            proc) (cadr (cadr proc)))
(define (procedure-body        proc) (cddr (cadr proc)))
(define (procedure-environment proc) (caddr      proc ))

;; Primitive procedures

(define (make-primitive exp) (list 'primitive exp))

(define (primitive-procedure? exp) (form-with-tag? 'primitive exp))
(define (primitive-id proc) (cadr proc))

;;; APPLYING PRIMITIVE PROCEDURES

;;; The mechanism for applying primitive procedures is somewhat different from
;;; the one given in section 4.1.4 of the text.  The modification is as
;;; suggested in exercise 4.8 of the text.  Instead of representing a primitive
;;; as a list:            (primitive <name-of-primitive>        )
;;; we represent it as:   (primitive <Scheme-procedure-to-apply>)

;;; To apply a primitive procedure, we ask the underlying Scheme system to
;;; perform the application.  (Of course, an implementation on a low-level
;;; machine would perform the application in some other way.)

(define (apply-primitive-procedure p args)
  (apply (primitive-id p)
	 (duckify-args p (reverse args)))) 	; Note: reversal for EC Eval!!

(define (duckify-compound-procedure proc) 	  ; Compound procedures just
  (list 'compound-procedure (parameters     proc) ; aren't very ducky.
	                    (procedure-body proc)
	'*procedure-env*))

(define (duckify-args p args) (if (ducky? p) args (mapcar duckify-arg args)))

(define (duckify-arg arg)
  (cond ((primitive-procedure? arg)
	 (primitive-id         arg))
	((compound-procedure? arg)
	 (amb-error "Grumble... primitive procedures don't like non-prim args"
		    (duckify-compound-procedure arg)) 0)
	(else arg)))


;;; ENVIRONMENTS... lists of FRAMES -- from section 4.1.3

(define the-empty-environment '())

(define (lookup-variable-value var env)
  (let ((b (binding-in-env var env)))
    (cond ((found-binding? b)
	   (binding-value  b))
	  (else (mini-error "Unbound variable" var) 0))))

(define (binding-in-env var env)
  (if (no-more-frames? env)
      no-binding
      (let ((b (binding-in-frame var (first-frame env))))
        (if (found-binding? b)
            b
            (binding-in-env var (rest-frames env))))))

(define (extend-environment variables values  base-env)
  (adjoin-frame (make-frame variables values) base-env))

(define (set-variable-value! var val env)
  (let ((b (binding-in-env var env)))
    (cond ((found-binding? b)
	   (set-binding-value! b val))
	  (else (mini-error "Unbound variable" var) 0))))

(define (define-variable! var val env)
  (let ((b (binding-in-frame var (first-frame env))))
    (if (found-binding? b)
        (set-binding-value! b val)
        (set-first-frame! env
			  (adjoin-binding (make-binding var val)
					  (first-frame env))))))
(define (first-frame env) (car env))
(define (rest-frames env) (cdr env))

(define (no-more-frames? env) (null? env))

(define (adjoin-frame frame env) (cons frame env))

(define (set-first-frame! env new-frame)
  (set-car! env new-frame))

;;; FRAMES... lists of BINDINGS

(define (make-frame variables values)
  (cond ((and (null? variables) (null? values)) '())
        ((null? variables)
         (mini-error "Too many values supplied" values) 0)
        ((null? values)
         (mini-error "Too few values supplied" variables) 0)
        (else
         (cons (make-binding (car variables)
                             (car values))
               (make-frame (cdr variables)
                           (cdr values))))))

(define (adjoin-binding binding frame)
  (cons binding frame))

(define (binding-in-frame var frame)
  (define (scan bindings)
    (cond ((null? bindings) no-binding)
	  ((same-variable? var (binding-variable (car bindings)))
	   (car bindings))
	  (else (scan (cdr bindings)))))
  (scan frame))


;;; BINDINGS... pairs of (VAR . VALUE)

(define (found-binding? b)
  (not (eq? b no-binding)))

(define no-binding false)

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding) (car binding))
(define (binding-value    binding) (cdr binding))

(define (set-binding-value! binding value)
  (set-cdr! binding value))
