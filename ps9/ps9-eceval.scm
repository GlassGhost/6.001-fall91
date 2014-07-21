;;; -*- Mode:Scheme; Base:10 -*- PS9-ECEVAL.SCM

;;		     MASSACHUSETTS INSTITUTE OF TECHNOLOGY
;;	   Department of Electrical Engineering and Computer Science
;;	   6.001---Structure and Interpretation of Computer Programs
;;			     Fall Semester, 1991
;;
;;				 Problem Set 9

;;;; 6.001 Explicit-Control Register-Machine Evaluator

;;; ENVIRONMENT INITIALIZATION...the ENVIRONMENT data abstraction appears in
;;;  the file PS9-SYNTAX.SCM

;;; We initialize the global environment by snarfing many primitives from the
;;; underlying Scheme system.  This is different from the book's treatment of
;;; primitives. If you want more primitives in your evaluator, just modify the
;;; list PRIMITIVE-PROCEDURE-NAMES to include their names or just use
;;; EXTEND-PRIMITIVES to gobble primitive from the underlying Scheme system.

(define goosey-primitives		; Cannot tolerate non-primitive args
  '(+ - * / < > =))
(define ducky-primitives		; Args needn't be primitive data
  '(car cdr cons eq? atom? null?
    clear-graphics))
  
(define (primitive-procedure-names)(append ducky-primitives goosey-primitives))

(define (ducky? p) (not (memq (primitive-id p)
			      (mapcar eval-in-initial-env goosey-primitives))))

(define (setup-environment)  
  (let ((initial-env
	  (extend-environment (primitive-procedure-names)
			      (mapcar (lambda (pname)
					(make-primitive
					  (eval-in-initial-env pname)))
				      (primitive-procedure-names))
			      the-empty-environment)))
    (define (extend-primitives procedure-names)
      (cond ((null? procedure-names) '---done---)
            (else (define-variable! (car procedure-names)
		    (make-primitive
		     (eval-in-initial-env (car procedure-names)))
                    initial-env)
                  (extend-primitives (cdr procedure-names)))))
    (define-variable! 'nil    nil   initial-env)
    (define-variable! 't (not nil)  initial-env)
    (define-variable! 'true   true  initial-env)
    (define-variable! 'false  false initial-env)
    (define-variable! 'extend-primitives
      (make-primitive extend-primitives)
      initial-env)
    initial-env))

(define (eval-in-initial-env form)
  (eval form user-initial-environment))

(define (mapcar proc lst)
  (if (null? lst)
      '()
      (cons (proc (car lst))
	    (mapcar proc (cdr lst)))))

(define the-global-environment (setup-environment))

;;; From 5.2.1

(define (make-bindings proc args)
  (extend-binding-environment (parameters proc)
                              args
                              (procedure-environment proc)))

(define (extend-binding-environment vars args env)
  (extend-environment vars (reverse args) env))

;;; From 5.3.6

(define (user-print object)
  (cond ((compound-procedure? object)
         (print (list 'compound-procedure
                      (parameters object)
                      (procedure-body object)
                      '<procedure-env>)))
        ((compiled-procedure? object)
         (print '<compiled-procedure>))
        (else (print object))))

;; The EC-EVAL machine with stuff from 5.3.6 for compiler-eceval interface

(define-machine explicit-control-evaluator
  (registers exp env val continue fun argl unev
	     arg1 arg2)		;;;arg registers for open-coding primitives
  (controller
read-eval-print-loop
  (perform (initialize-stack))
  (perform (initialize-ops-counter))                   ;;; 
  (perform (newline))(perform (newline))
  (perform (princ "EC-EVAL==> "))
  (assign exp (read-from-keyboard))
  (branch (exit-on? (fetch exp)) ec-eval-exit)
  (assign env the-global-environment)
  (assign continue print-result)
  (goto eval-dispatch)
print-result
  (perform (user-print (fetch val)))
  (goto read-eval-print-loop)

unknown-procedure-type-error
  (assign val '>>>Unknown-procedure-type-error)
  (goto signal-error)
unknown-expression-type-error
  (assign val '>>>Unknown-expression-type-error)
signal-error
  (perform (newline))
  (perform (user-print (fetch val)))		;; Err mesg in VAL
  (perform (user-print (fetch exp)))		;; Irritant in EXP
  (goto read-eval-print-loop)

external-entry
   (perform (initialize-stack))
   (assign env the-global-environment)
   (assign continue print-result)
   (save continue)
   (goto (fetch val))

eval-dispatch
  (branch (self-evaluating?  (fetch exp)) ev-self-eval)
  (branch (quoted?           (fetch exp)) ev-quote)
  (branch (variable?         (fetch exp)) ev-variable)
  (branch (definition?       (fetch exp)) ev-definition)
  (branch (assignment?       (fetch exp)) ev-assignment)
  (branch (lambda?           (fetch exp)) ev-lambda)
  (branch (conditional?      (fetch exp)) ev-cond)
  (branch (no-args?          (fetch exp)) ev-no-args)
  (branch (args-application? (fetch exp)) ev-application)
  (goto unknown-expression-type-error)
ev-self-eval
  (assign val (fetch exp))
  (goto (fetch continue))
ev-quote
  (assign val (text-of-quotation (fetch exp)))
  (goto (fetch continue))
ev-variable
  (assign val (lookup-variable-value (fetch exp) (fetch env)))
  (goto (fetch continue))
ev-lambda
  (assign val (make-procedure (fetch exp) (fetch env)))
  (goto (fetch continue))
ev-no-args
  (assign exp (operator (fetch exp)))
  (save continue)
  (assign continue setup-no-arg-apply)
  (goto eval-dispatch)

setup-no-arg-apply
  (assign fun (fetch val))
  (assign argl '())
  (goto apply-dispatch)
ev-application
  (assign unev (operands (fetch exp)))
  (assign exp (operator (fetch exp)))
  (save continue)
  (save env)
  (save unev)
  (assign continue eval-args)
  (goto eval-dispatch)
eval-args
  (restore unev)
  (restore env)
  (assign fun (fetch val))
  (save fun)
  (assign argl '())
  (goto eval-arg-loop)

eval-arg-loop
  (save argl)
  (assign exp (first-operand (fetch unev)))
  (branch (last-operand? (fetch unev)) eval-last-arg)
  (save env)
  (save unev)
  (assign continue accumulate-arg)
  (goto eval-dispatch)
accumulate-arg
  (restore unev)
  (restore env)
  (restore argl)
  (assign argl (cons (fetch val) (fetch argl)))
  (assign unev (rest-operands (fetch unev)))
  (goto eval-arg-loop)
eval-last-arg
  (assign continue accumulate-last-arg)
  (goto eval-dispatch)
accumulate-last-arg
  (restore argl)
  (assign argl (cons (fetch val) (fetch argl)))
  (restore fun)
  (goto apply-dispatch)

apply-dispatch
  (branch (primitive-procedure? (fetch fun)) primitive-apply)
  (branch ( compound-procedure? (fetch fun))  compound-apply)
  (branch ( compiled-procedure? (fetch fun))  compiled-apply)
  (assign exp (fetch fun))		;; Place irritant in EXP
  (goto unknown-procedure-type-error)
compiled-apply
   (assign val (compiled-procedure-entry (fetch fun)))
   (goto (fetch val))
primitive-apply
  (assign val
          (apply-primitive-procedure (fetch fun)
                                     (fetch argl)))
  (restore continue)
  (goto (fetch continue))
compound-apply
  (assign env (make-bindings (fetch fun) (fetch argl)))
  (assign unev (procedure-body (fetch fun)))
  (goto eval-sequence)
eval-sequence
  (assign exp (first-exp (fetch unev)))
  (branch (last-exp? (fetch unev)) last-exp)
  (save unev)
  (save env)
  (assign continue eval-sequence-continue)
  (goto eval-dispatch)
eval-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (rest-exps (fetch unev)))
  (goto eval-sequence)
last-exp
  (restore continue)
  (goto eval-dispatch)

ev-cond
  (save continue)
  (assign continue evcond-decide)
  (assign unev (clauses (fetch exp)))
evcond-pred
  (branch (no-clauses? (fetch unev)) evcond-return-nil)
  (assign exp (first-clause (fetch unev)))
  (branch (else-clause? (fetch exp)) evcond-else-clause)
  (save env)
  (save unev)
  (assign exp (predicate (fetch exp)))
  (goto eval-dispatch)

evcond-return-nil
  (restore continue)
  (assign val nil)
  (goto (fetch continue))
evcond-decide
  (restore unev)
  (restore env)
  (branch (true? (fetch val)) evcond-true-predicate)
  (assign unev (rest-clauses (fetch unev)))
  (goto evcond-pred)
evcond-true-predicate
  (assign exp (first-clause (fetch unev)))
evcond-else-clause
  (assign unev (actions (fetch exp)))
  (goto eval-sequence)
ev-assignment
  (assign unev (assignment-variable (fetch exp)))
  (save unev)
  (assign exp (assignment-value (fetch exp)))
  (save env)
  (save continue)
  (assign continue ev-assignment-1)
  (goto eval-dispatch)
ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (set-variable-value! (fetch unev) (fetch val) (fetch env)))
  (goto (fetch continue))
ev-definition
  (assign unev (definition-variable (fetch exp)))
  (save unev)
  (assign exp (definition-value (fetch exp)))
  (save env)
  (save continue)
  (assign continue ev-definition-1)
  (goto eval-dispatch)
ev-definition-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform
   (define-variable! (fetch unev) (fetch val) (fetch env)))
  (assign val (fetch unev)) 
  (goto (fetch continue))

ec-eval-exit
  (assign val (exit (fetch exp)))	;; Leave exit message in VAL
  ;; end of controller of explicit control evaluator
  ))

(define (exit-on? exp)
  (member exp the-many-names-of-exit))

(define the-many-names-of-exit
  '( bye   quit   exit   punt   game-over   fini   toast   i-am-outta-here
    (bye) (quit) (exit) (punt) (game-over) (fini) (toast) (i-am-outta-here)))

(define (exit exp)
  (if (atom? exp) exp (car exp)))

;;; A short procedure, for convenience

(define (go)
  (reset-magic-recontinue-hook)		;; If we bomb out, we bomb back here.
  (start explicit-control-evaluator))

;;; From 5.3.6 for compiler interface

(define (make-compiled-procedure entry env)
  (list 'compiled-procedure entry env))

(define (compiled-procedure? proc) (form-with-tag? 'compiled-procedure proc))

(define (compiled-procedure-entry c-proc) (cadr  c-proc))
(define (compiled-procedure-env   c-proc) (caddr c-proc))

(define (setup-compile-and-go)	    ; Called only after MAGIC-RECONTINUE setup
  ;; Installed below...             ;  see RESET-MAGIC-RECONTINUE-HOOK below.
  (error "SETUP-COMPILE-AND-GO not yet installed"))

(define compile-and-go
  (let ()	
    ;; REAL-COMPILE-AND-GO is from the book... the rest of this page is
    ;; all chicanery to make MAGIC-RECONTINUE happy. You only need to
    ;; understand that COMPILE-AND-GO below will eventually be SET!'d
    ;; to the definition of REAL-COMPILE-AND-GO and that this will make
    ;; it behave as in the book.
    (define (real-compile-and-go expression)
      (remote-assign
       explicit-control-evaluator
       'val
       (build-instruction-list explicit-control-evaluator
			       (compile expression)))
      (eval '(goto external-entry)
	    explicit-control-evaluator))

    ;; When FORCEd, this promise installs the book's COMPILE-AND-GO
    (let ((*compile-and-go-promise*
	   (delay (let () ;; (LET ()...) is a highly portable (SEQUENCE... )
		    (set! compile-and-go real-compile-and-go)
		    real-compile-and-go))))
      ;; Install SETUP-COMPILE-AND-GO to be a procedure to force this promise
      (set! setup-compile-and-go (lambda () (force *compile-and-go-promise*))))

    ;; Finally, make the initial definition of COMPILE-AND-GO be a shameless
    ;; shenanigan to force a call to (GO) to setup MAGIC-RECONTINUE.
    (lambda (expression)
      (princ
       "
       -----------------------------------------------------------------------
        Grumble... the EC Eval has not yet been initialized by a call to (GO)
        ...I'm issuing a call to (GO) now.

        [By the way, this grumble could be avoided if you always call (GO) at
         least once before you ever try to call COMPILE-AND-GO.]
       -----------------------------------------------------------------------

       *** Type QUIT now, then type (RETRY)
       ")

      ;; Set up RETRY so you don't have to retype your COMPILE-AND-GO call.
      (set! retry (lambda ()
		    (newline)
		    (princ "Retrying: (COMPILE-AND-GO ")
		    (princ expression)
		    (princ ")")
		    (set! retry (lambda () (go))) ;; No fair doing it twice
		    (compile-and-go expression)))

      ;; Finally, invoke (GO) so it can setup MAGIC-RECONTINUE, and hence setup
      ;;  COMPILE-AND-GO too.
      ;;
      ;; NB: It's crucial that this call to (GO) be in tail recursive position
      ;;     so the continuation captured by RESET-MAGIC-RECONTINUE-HOOK will
      ;;     have no pending operations.
      (go))))

(define (retry) (go))			; Safe default

;;;
;;; Unabashed hacks... you are expected not to understand this page.
;;;

;; USER-SAYS-YES?

(define (user-says-yes?) (member (read-from-keyboard) the-many-names-of-yes))
(define the-many-names-of-yes
  '(y yes yup ya yea uh-huh yep yepper ok sure shir why-not? ja affirmative
    t true not-nil non-negative 1 oui 42 37 I-guess-so okey-dokey bully!
    what-have-I-got-to-lose? so-long-as-its-free so-long-as-its-tax-free
    ok-but-dont-put-me-on-your-mailing-list ok-but-dont-quote-me-on-that))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SURGEON GENERAL'S WARNING: Don't look! You don't want to know.  -B.Bitdiddle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All you should care about is that this lets you fix your typos without
;; crashing EC Eval into the underlying Scheme debugger.

;; MINI-ERROR--- a magic way to give you the option of staying in the EC REPL
;;
;; OBSCURE NOTE: This should not be invoked in a tail-recursive position since
;; that would allow an interpreter to drop the environment before we have a
;; chance to invoke ERROR so the user may not be able to probe the detailed
;; circumstances surrounding the generation of the irritant. This is why all
;; our calls to MINI-ERROR are followed by a mysterious 0.

(define (mini-error string irritant)
  (user-display-message (list 'OOPS! string irritant))
  (newline)
  (princ "Wanna fall through to the real debugger? (y/n): ")
  (cond ((user-says-yes?) (error string irritant))
	(else (newline) (newline)
	      (princ "OK, continuing with the EC Eval REPL...")
	      (magic-recontinue))))

;; RESET-MAGIC-RECONTINUE-HOOK--- sets up MAGIC-RECONTINUE to be a thunk that
;; 1) will allow MINI-ERROR to bypass silly typo errors without falling through
;; to the underlying Scheme debugger, and 2) also lets you back into the ECEval
;; loop without having to retype COMPILE-AND-GO etc.  It achieves this by cap-
;; turing the Scheme continuation pending just before we dive into the EC REPL.
;;
;; Having gotten a handle on this, we are then equipped to set MAGIC-RECONTINUE
;; to be a thunk that magically teleports us back to the EC REPL at precisely
;; that state in the evaluation process where we were about to reset the
;; machine performance metrics and read in a new expression and EC-EVAL it. One
;; subtle point is to notice that this Scheme continuation that we had captured
;; not only contains control information about how to proceed with the REPL,
;; it also contains a pointer to the environment frame that we were in at the
;; previous losing call into the REPL.
;;
;; The overall effect, therefore, is to setup MAGIC-RECONTINUE to completely
;; throw out of whatever control state (i.e. pending operations) it was in when
;; invoked and to magically make it look like we have turned the clock back to
;; just before we fed the EC REPL the bogus expression, from which point
;; we start running the clock forward again with the same machine state that
;; the bad input started with.
;;
;; The whole point of this is to enable MINI-ERROR to invoke MAGIC-RECONTINUE
;; to back up to the point just before we had read in the offending expression
;; (the one whose evaluation spawned, as a subtask, the evaluation of the
;; IRRITANT expression) then start the REPL going again as if the last
;; READ-FROM-KEYBOARD has not yet occurred. This gives you the opportunity to
;; reconsider your input rather than just crapping out into the underlying
;; Scheme debugger and losing all your neat COMPILE-AND-GO state that had been
;; loaded into the machine.

(define (magic-recontinue) (go))	; Safe default... updated below.

(define (reset-magic-recontinue-hook)
  (let ((call-with-reentrant-exit call-with-current-continuation))
    (call-with-reentrant-exit
     (lambda (recontinue)
       (set! magic-recontinue
	     (lambda ()
	       (recontinue "If at first you don't succeed...")))
       (setup-compile-and-go)		;; MAGIC-RECONTINUE is installed now
       "...get out of Dodge." ))))	;;  so let COMPILE-AND-GO work too.

;;;
;;; Miscellany
;;;

;; A familiar useful procedure (from the Adventure game)

(define (user-display-message list-of-stuff)
  (newline)
  (for-each (lambda (s) (princ s) (princ " "))
	    list-of-stuff))
