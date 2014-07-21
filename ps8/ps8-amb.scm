;;; This is the file PS8-AMB.SCM   -- Fall 91

;;; It contains the metacircular evaluator, as described in section 4.1 of the
;;; text, with a few minor modifications and arranged to allow the use of the 
;;; non-deterministic choice operator, AMB.

;;; You should just load this file into Scheme without editing it.  The new
;;; procedures that you will need to modify in order to do the problem set have
;;; been copied into a separate file for your convenience.

;;; EVALUATOR INITIALIZATION

;;; To start the metacircular evaluator, call INITIALIZE-EVALUATOR.  This
;;; initializes the global environment and starts the DRIVER-LOOP.  Use
;;; INITIALIZE-EVALUATOR instead of DRIVER-LOOP if you want to erase any
;;; definitions you have accumulated and start fresh with a clean global
;;; environment. Use DRIVER-LOOP if you want to keep your global environment
;;; but reset the history. Use MAGIC-RECONTINUE to keep your old environment
;;; and old history and old FAIL state last time you exited (or bombed out).

(define the-global-environment 'To-be-setup)

(define (global-environment-initialized?)
  (not (eq? the-global-environment 'To-be-setup)))

(define (initialize-evaluator)
  (set! the-global-environment (setup-environment))
  (driver-loop))

;;; THE DRIVER-LOOP

;;; The driver loop reads an expression, evaluates it in the global
;;; environment, and prints the result.  note that the driver uses a prompt of
;;; "AMB-EVAL==>> " to help you avoid confusing typing to the metacircular
;;; evaluator with typing to the underlying Scheme interpreter.  (Also, we have
;;; called our evaluator procedures AMB-EVAL and AMB-APPLY to help you avoid
;;; confusing them with Scheme's EVAL and APPLY.)

;;; When/If your interaction with the evaluator bombs out in an error, you
;;; should restart it by calling DRIVER-LOOP again.

(define (driver-loop)
  (cond ((not (global-environment-initialized?))
	 ;; Show a little mercy on those who forget to INITIALIZE-EVALUATOR...
	 (initialize-evaluator))
	(else (reset-history! *the-history*)
	      (newline)
	      (princ "Starting a new problem")
	      (values-loop driver-loop))))

;;; To get the same sort of behavior at top level that we see within an
;;; expression, we essentially have to treat the successive forms typed at the
;;; REPL as if they were successive elements of a list-of-values,  except that
;;; we must leave out the "control" forms: (FAIL) (NEXT) and (DONE).  this
;;; means that a top-level sequence such as:
;;;
;;;   (define x (amb 1 2))
;;;   (define y (amb 3 4))
;;;   (list x y)   ==> (1 3)
;;;   (next)       ==> (1 4)
;;;   (next)       ==> (2 3)
;;;   (next)       ==> (2 4)
;;;
;;; should work something like:
;;;
;;;  (list (amb 1 2) (amb 3 4)) ==> (1 3)
;;;  (next)                     ==> (1 4)
;;;  (next)                     ==> (2 3)
;;;  (next)                     ==> (2 4)
;;;
;;; Unfortunately, this does not quite work out because the second example
;;; creates an ambiguous list while the first example creates two ambiguous
;;; defines. This means that in the first example, (NEXT) will back up to the
;;; defines and explore the options within those so the value returned by
;;; (NEXT) will always be the value of some DEFINE form, which is always
;;; just the name of the thing defined. For that reason, whenever we back up to
;;; some previous AMB expression, we will re-evaluate every expression which
;;; appeared after it and display these re-evaluations along with the new value
;;; produced by the call to NEXT. to make this possible, we maintain a global
;;; HISTORY...

;;; The HISTORY keeps track of all non-"control" form expressions read in by
;;; the AMBScheme REPL and stores them in the order in which they were
;;; presented to ensure that textually identical expressions are represented as
;;; separate entries in the history, we treat each expression read in as a
;;; separate EVENT whose EVENT-EXPR is the expression read in.  The point is
;;; for expressions which may happen to be EQ? to be stored in the history as 
;;; events which are *not* EQ?.

;;; THE HISTORY

(define *the-history* (list '*history*))  ; Tagged list of time-ordered events

(define (events         history) (    cdr  history     ))
(define (reset-history! history) (set-cdr! history '() ))
(define (extend-history! event history)
  (cond ((non-recording-event? event)	; e.g., Don't record user-load calls
	 false)				;       or show-history calls
	(else (set-cdr! (last-pair history)
			(list event))
	      true)))

(define (non-recording-event? event)
  (let ((expr (event-expr event)))
    (if (application? expr)
	(memq (operator expr) *non-recording-operators*)
	false)))

(define *non-recording-operators* '(show-history user-load load))

(define   first-event   car  )
(define    rest-events  cdr  )
(define no-more-events? null?)

;;; EVENTS

(define (make-event exp) (list exp))
(define (event-expr event) (car event))	; Events are one-element lists.

(define (show-history)			; useful user-level inspection utility
  (for-each (lambda (event)
	      (newline)
	      (princ ": ") (princ (event-expr event)))
	    (events *the-history*))
  '---thats-all-folks---)

;;;
;;; THE *REAL* AMBScheme REPL (Read-Eval-Print Loop)
;;;

(define (values-loop fail)
  (reset-magic-recontinue-hook)		; Get a handle on what we're about
  (newline)(newline)			; to do and on the FAIL we're using.
  (princ "AMB-EVAL==>> ")
  (let ((event (make-event (read-from-keyboard))))
    (if (exit-on? (event-expr event))
	(exit     (event-expr event))
	(amb-eval (event-expr event)
		  the-global-environment
		  (lambda (value expr-fail)
		    (user-print value)
		    (let ((now+future (memq event (events *the-history*))))
		      (cond (now+future
			     ;; Work our way back up to the last read in expr
			     (back-to-the-future (rest-events now+future)
						 expr-fail))
			    (else
			     (extend-history! event *the-history*)
			     (values-loop expr-fail)))))
		  fail))))

(define (exit-on? exp)
  (member exp the-many-names-of-exit))

(define the-many-names-of-exit
  '( bye   quit   exit   punt   game-over   fini   toast   i-am-outta-here
    (bye) (quit) (exit) (punt) (game-over) (fini) (toast) (i-am-outta-here)))

(define (exit exp)
  (if (atom? exp) exp (car exp)))

;; *THE-HISTORY* --- WHAT IT IS AND HOW IT IS USED TO MAKE AMB AND NEXT WORK
;;
;; ****************************************************************************
;; * You can just punt this long comment if you're not in the mood for some
;; * seriously intense hacking, above and beyond what is necessary to receive
;; * full credit on this assignment. But at least glance at BACK-TO-THE-FUTURE.
;; ****************************************************************************
;;
;; So you're not content to extend and use the AMBScheme system without fully
;; penetrating into its deepest implementation subtleties? So you won't let
;; yourself be dissuaded from pounding your noggin against the micro-details
;; despite our assurance that such a low level, detailed understanding of how
;; this complicated system works is not necessary to complete the problem set
;; with flying colors. Well then, you're a die-hard hacker, tried and true, and
;; you've come to the right place 'cause here's the place in the code where we
;; divulge the innermost secrets of The Way of the AMBScheme. This is pretty
;; heavy stuff so the overworked or the faint at heart should just gloss over
;; this and let it go. Don't say I didn't warn you....       --Ben Bitdiddle.
;;
;; So...  what does BACK-TO-THE-FUTURE really do?  That's a very subtle
;; question since this is where the mysterious AMB-induced continuation hacking
;; interacts with the HISTORY the we accumulate to make it possible for (NEXT)
;; to make our AMBScheme evaluator fall back to previous points in the
;; evaluation process then work back up to present.  In short, this is the very
;; heart of the whole AMB/NEXT evaluation mechanism.
;;
;; Let's think about it in terms of this image of falling back and working up.
;;
;; First, if an avalanche of failures has made us backtrack deep into the past,
;; then we have to work our way back to where the present meets the future
;; (i.e., to where we are waiting for a fresh user input).  We do this by
;; successively re-evaluating each event which followed the current place in
;; the past where we have fallen back to, accruing a new scaffolding of failure
;; continuations which can be invoked to backtrack to the place even deeper in
;; the past.
;;
;; To see how this scaffolding gets set up, you should consider EVAL-AMB and
;; what it does if some future event which we must evaluate involves a call to
;; AMB.  Specifically, look closely at how EVAL-AMP folds GOT-ONE into its
;; failure continuation and postpones using the originally given FAIL until all
;; operands have been exhausted.
;;
;; Now consider what havoc ensues if, in the process of building back up to the
;; present, some future event itself now becomes exhausted and fails...  we
;; avalanche down again then try to dig ourselves out again.  We either
;; eventually get all the way back out to the future/present or we completely
;; bottom out by "Starting a new problem".

(define (back-to-the-future future-events fail-for-exhausted-future)
  (if (no-more-events? future-events)		; All the way back?
      (values-loop fail-for-exhausted-future)
      (amb-eval (event-expr (first-event future-events))
		the-global-environment
		(lambda (val expr-fail)
		  (newline)
		  (princ ": ")
		  (princ (event-expr (first-event future-events)))
		  (princ " --> ")
		  (user-princ val)
		  (back-to-the-future (rest-events future-events) expr-fail))
		fail-for-exhausted-future)))

;;; Our REPL will use a special PRINT, which avoids printing the environment
;;; part of a compound procedure, since the latter is a very long (or even
;;; circular) list. Same for PRINC.

(define (user-show object show)
  (cond ((compound-procedure? object)
         (show (duckify-compound-procedure object)))
        (else (show object))))

(define (user-print object) (user-show object print))
(define (user-princ object) (user-show object princ))
(define (user-prin1 object) (user-show object prin1))

;;; We also need a loader for files of user code... like PS8-LANG.SCM

(define (user-load filename)
  (define win? true)
  (define *FAILED!* '==========================FAILED=========================)
  (define (fail!) (set! win? false))
  (let ((port (open-input-file filename)))
    (define (loop)
      (let ((exp (read port)))
	(cond ((eof-object? exp) (close-input-port port) win?)
	      (else
	       (amb-eval exp
			 the-global-environment
			 (lambda (val fail) (user-print       val) (loop))
			 (lambda () (fail!) (user-print *FAILED!*) (loop)))))))
    (loop)))

;;; ENVIRONMENT INITIALIZATION...the ENVIRONMENT data abstraction appears below

;;; We initialize the global environment by snarfing many primitives from the
;;; underlying Scheme system.  This is different from the book's treatment of
;;; primitives.  (See the section "APPLYING PRIMITIVE PROCEDURES" below.) If
;;; you want more primitives in your evaluator, just modify the list
;;; PRIMITIVE-PROCEDURE-NAMES to include their names or just use
;;; EXTEND-PRIMITIVES to gobble primitive from the underlying Scheme system.

(define goosey-primitives		; Not friendly toward non-primitives
  '(+ - * / = < > <= >= 1+ -1+ quotient remainder sqrt expt floor ceiling abs
    truncate round gcd exp log cos sin tan atan max min odd? even? zero?
    negative? positive?   mapcar for-each  apply user-load))
(define ducky-primitives		; Not perturbed by non-primitive args
  '(cons list length reverse append list-ref list-tail set-car! set-cdr!
    car cdr caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr 
    eq? eqv? equal? null? integer? number? symbol? atom? pair? list? string?
    assq assv assoc memq memv member last-pair true? not
    read eval print princ prin1 newline user-print user-princ user-prin1
    read-from-keyboard show-history))
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
    (define-variable! 'true   true  initial-env)
    (define-variable! 'false  false initial-env)
    (define-variable! 'fail '*fail* initial-env)
    (define-variable! 'next '*next* initial-env)
    (define-variable! 'done '*done* initial-env)
    (define-variable! 'load (make-primitive user-load) initial-env) ;For bozos
    (define-variable! 'extend-primitives (make-primitive extend-primitives)
                                         initial-env)
    initial-env))

;;; THE CORE OF THE EVALUATOR -- modified from section 4.1.1
;;;
;;; FAIL is a procedure of zero arguments.
;;; GOT-ONE is a procedure of two args, the value and the fail continuation

(define (amb-eval exp env got-one fail)
  (cond ((self-evaluating? exp)
         (got-one exp fail))
        ((quoted? exp)
         (got-one (text-of-quotation exp) fail))
        ((variable? exp)
         (got-one (lookup-variable-value exp env) fail))
        ((definition? exp)
         (eval-definition exp env got-one fail))
        ((assignment? exp)
         (eval-assignment exp env got-one fail))
        ((temporary-assignment? exp)
         (eval-temporary-assignment exp env got-one fail))
        ((lambda? exp)
         (got-one (make-procedure exp env) fail))
        ((conditional? exp)
         (eval-cond (clauses exp) env got-one fail))
        ((let? exp)
         (eval-let exp env got-one fail))
        ((amb? exp)
         (eval-amb (operands exp) env got-one fail))
        ((all-values? exp)
         (eval-all-values (operands exp) env got-one fail))
        ((application? exp)
         (amb-eval (operator exp) 
                   env 
                   (lambda (proc fail-rator)
                     (list-of-values (operands exp) 
                                     env
                                     (lambda (args fail-rands)
                                       (amb-apply proc args got-one fail-rands))
                                     fail-rator)) 
                   fail))
        (else (amb-error "Unknown expression type -- AMB-EVAL" exp) 0)))

(define (amb-apply procedure arguments got-one fail)
  (cond ((primitive-procedure? procedure)
         (got-one (apply-primitive-procedure procedure arguments)
                  fail))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment (parameters procedure)
                                            arguments
                                            (procedure-environment procedure))
                        got-one
                        fail))
        ((fail? procedure)
         (fail))
        ((next? procedure)
         (fail))
        ((done? procedure)
         (driver-loop))
        (else
         (amb-error "Unknown procedure type -- AMB-APPLY" procedure) 0)))

(define (eval-amb exps env got-one fail)
  (if (no-operands? exps)
      (fail)
      (amb-eval (first-operand exps) 
                env
                got-one			; Succeed
                (lambda ()		; New fail
                  (eval-amb (rest-operands exps) env got-one fail)))))

(define (eval-cond clist env got-one fail)
  (cond ((no-clauses? clist)
         (got-one false fail))
        ((else-clause? (first-clause clist))
         (eval-sequence (actions (first-clause clist)) env got-one fail))
        (else
         (amb-eval (predicate (first-clause clist)) 
                   env
                   (lambda (pred fail-pred)
                     (if (true? pred)
                         (eval-sequence (actions (first-clause clist))
                                        env
                                        got-one
                                        fail-pred)
                         (eval-cond (rest-clauses clist) env got-one fail-pred)))
                   fail))))

(define (eval-sequence exps env got-one fail)
  (cond ((last-exp? exps)
         (amb-eval (first-exp exps) env got-one fail))
        (else
         (amb-eval (first-exp exps) 
                   env
                   (lambda (value fail-1)
                     (eval-sequence (rest-exps exps) env got-one fail-1))
                   fail))))

(define (list-of-values exps env got-one fail)
  (cond ((no-operands? exps) (got-one '() fail))
        (else
         (amb-eval (first-operand exps) 
                   env
                   (lambda (arg fail-1)
                     (list-of-values (rest-operands exps)
                                     env 
                                     (lambda (args fail-rest)
                                       (got-one (cons arg args) fail-rest))
                                     fail-1))
                   fail))))

(define (eval-assignment exp env got-one fail)
  (amb-eval (assignment-value exp)
            env
            (lambda (value fail-val)
              (set-variable-value! (assignment-variable exp) value env)
              (got-one (list 'Done-- 'new 'value '= value) fail-val))
            fail))

(define (eval-temporary-assignment exp env got-one fail)
  (let ((oldvalue (lookup-variable-value (assignment-variable exp) env)))
    (amb-eval (assignment-value exp) 
              env
              (lambda (value fail-val)
                (set-variable-value! (assignment-variable exp) value env)
                (got-one (list 'Temporarily-Done-- 'new 'value '= value)
                         (lambda ()
                           (set-variable-value! (assignment-variable exp)
                                                oldvalue
                                                env)
                           (fail-val))))
              fail)))

;; Instead of the temporary-assignment and all-values, we should define some
;; more general machinery to let the user-level code get hold of what to do
;; when all of some set of AMB's is exhausted.

(define (eval-all-values seq env got-one fail)
  (cond ((last-exp? seq)
	 (let ((result '()))
	   (amb-eval (first-exp seq)
		     env
		     (lambda (value fail-1)
		       (set! result (cons value result))
		       (fail-1))
		     (lambda ()
		       (got-one (reverse result) fail)))))
	(else (amb-error "Too many forms in ALL-VALUES" seq) 0)))

(define (eval-let exp env got-one fail)
  (amb-eval (cons (cons 'lambda
			(cons (mapcar let-binding-variable (let-bindings exp))
			      (let-body exp)))
                  (mapcar let-binding-value (let-bindings exp)))
            env
            got-one
            fail))

(define (eval-definition exp env got-one fail)
  (amb-eval (definition-value exp) 
            env
            (lambda (value fail-val)
              (define-variable! (definition-variable exp) value env)
              (got-one (definition-variable exp) fail-val))
            fail))


;;; THE SYNTAX OF THE LANGUAGE -- from section 4.1.2
;;;
;;; Slightly modified to abstract out common structure of special forms

;; Self-evaluating entities

(define (self-evaluating? exp)
  (or (number? exp)
      (string? exp)))	; (Filenames in USER-LOAD are strings)

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

;; Assignments--- SET! and SET!-AMB

(define (assignment? exp) (form-with-tag? 'set! exp))

(define (temporary-assignment? exp) (form-with-tag? 'set!-amb exp))

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

;; AMB expressions

(define (amb? exp) (form-with-tag? 'amb exp))

;; ALL-VALUES forms

(define (all-values? exp) (form-with-tag? 'all-values exp))

;; Sequences

(define ( last-exp? seq) (null? (cdr seq)))
(define (first-exp  seq) (car seq))
(define ( rest-exps seq) (cdr seq))

;; Procedure applications

(define (application? exp) (not (atom? exp)))

(define (operator app) (car app))
(define (operands app) (cdr app))

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
;;; as a list:             (primitive <name-of-primitive>        )
;;; we represent it as:    (primitive <Scheme-procedure-to-apply>)

;;; To apply a primitive procedure, we ask the underlying Scheme system to
;;; perform the application.  (Of course, an implementation on a low-level
;;; machine would perform the application in some other way.)

(define (apply-primitive-procedure p args)        ; OOPS! Primitives choke on
  (apply (primitive-id p) (duckify-args p args))) ; non-primitive proc args.

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

(define (fail? proc) (eq? proc '*fail*))
(define (next? proc) (eq? proc '*next*))
(define (done? proc) (eq? proc '*done*))


;;; ENVIRONMENTS... lists of FRAMES -- from section 4.1.3

(define the-empty-environment '())

(define (lookup-variable-value var env)
  (let ((b (binding-in-env var env)))
    (cond ((found-binding? b)
	   (binding-value  b))
	  (else (amb-error "Unbound variable" var) 0))))

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
	  (else (amb-error "Unbound variable" var) 0))))

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
         (amb-error "Too many values supplied" values) 0)
        ((null? values)
         (amb-error "Too few values supplied" variables) 0)
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


;;;
;;; Miscellany
;;;

;; LAST-PAIR

(define (last-pair lst)
  (cond ((atom? lst)
	 (error "Tried to find pair of atom -- LAST-PAIR" lst))
	((null? (cdr lst)) lst)
	(else (last-pair (cdr lst)))))

;; Some familiar useful procedures (from the Adventure game)

(define (user-display-message list-of-stuff)
  (newline)
  (for-each (lambda (s) (princ s) (princ " "))
	    list-of-stuff))

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
;; crashing AMBScheme into the underlying Scheme debugger and losing all your
;; ducky AMB state [the stuff the (NEXT)s are made of].

;; AMB-ERROR--- a magic way to give you the option of staying in DRIVER-LOOP
;;
;; OBSCURE NOTE: This should not be invoked in a tail-recursive position since
;; that would allow an interpreter to drop the environment before we have a
;; chance to invoke ERROR so the user may not be able to probe the
;; circumstances surrounding the generation of the irritant. This is why all
;; our calls to AMB-ERROR are followed by a mysterious 0.

(define (amb-error string irritant)
  (user-display-message (list 'OOPS! string irritant))
  (newline)
  (princ "Wanna fall through to the real debugger? (y/n): ")
  (cond ((user-says-yes?) (error string irritant))
	(else (newline) (newline)
	      (princ "OK, continuing with the driver-loop...")
	      (magic-recontinue))))

;; RESET-MAGIC-RECONTINUE-HOOK--- sets up MAGIC-RECONTINUE to be a thunk that
;; 1) will allow AMB-ERROR to bypass silly typo errors without falling through
;; to the underlying Scheme debugger, and 2) also lets you back into AMBScheme
;; without resetting history & fail. It achieves this by capturing the Scheme
;; continuation pending just before we dive into the REPL with our current FAIL
;;
;; Having gotten a handle on this, we are then equipped to set MAGIC-RECONTINUE
;; to be a thunk that magically teleport us back to the AMBScheme REPL at
;; precisely that state in the evaluation process where we were about to read
;; in a new expression and AMB-EVAL it with respect to the FAIL supplied. The
;; subtle point is to notice that this Scheme continuation that we had captured
;; not only contains the control information about how to proceed with the
;; REPL, it also contains a pointer to the environment frame that maps FAIL to
;; the value it had in the previous losing call into the REPL.
;;
;; The overall effect, therefore, is to setup MAGIC-RECONTINUE to completely
;; throw out of whatever control state (i.e. pending operations) it was in when
;; invoked and to magically make it look like we have turned the clock back to
;; just before we fed the AMBScheme REPL the bogus expression, from which point
;; we start running the clock forward again with the same FAIL continuation as
;; the bad input started with.
;;
;; The whole point of this is to enable AMB-ERROR to invoke MAGIC-RECONTINUE to
;; back up to the point just before we had read in the offending expression
;; (the one whose evaluation spawned, as a subtask, the evaluation of the
;; IRRITANT expression) then start the REPL going again as if the last
;; READ-FROM-KEYBOARD has not yet occurred. This gives you the opportunity to
;; reconsider your input rather than just crapping out into the underlying
;; Scheme debugger and losing all your neat AMB backtracking (NEXT) state that
;; had been accumulated into the FAIL continuation.

(define magic-recontinue driver-loop)	; Safe default... updated below.

(define (reset-magic-recontinue-hook)
  (let ((call-with-reentrant-exit call-with-current-continuation))
    (call-with-reentrant-exit
     (lambda (recontinue)
       (set! magic-recontinue
	     (lambda ()
	       (recontinue "If at first you don't succeed...")))
       "...get out of Dodge."))))

;;;
;;; Portability Support
;;;

;;; Definitions for Chipmunks
;;;--------------------------

(define (eval-in-initial-env form)
  (eval form user-initial-environment))

(define for-each mapc)  ; A rose by this name smells sweeter.


;;; Definitions for MacScheme
;;;--------------------------

;(define (eval-in-initial-env form)
;  (eval form))

;(define read-from-keyboard read)


;;; Definitions for PCScheme
;;;-------------------------

;(define (eval-in-initial-env form)
;  (eval form user-initial-environment))

;(define read-from-keyboard read)

;(define mapcar map)


;;; Definitions for PseudoScheme
;;;-----------------------------

;(define (eval-in-initial-env form)
;  (eval form user-initial-environment))

;(define read-from-keyboard read)

;(define (atom? object) (not (pair? object)))

;(define (reset-magic-recontinue-hook)
;  "Alas...PseudoScheme lacks reentrant continuations"
;  (set! magic-recontinue
;        (lambda ()
;	  (newline)
;	  (princ "---------------------------------------------------------")
;          (newline)
;	  (princ "Sorry, friend: PseudoScheme lacks reentrant continuations")
;          (newline)
;	  (princ "...you'll have to retrace your steps manually. No HISTORY.")
;	  (newline)
;	  (princ "---------------------------------------------------------")
;	  (driver-loop)))
;  "...so MAGIC-RECONTINUE will just start a new problem each time it's called")


;;; Definitions for Yale's T Scheme
;;;--------------------------------

;(define (eval-in-initial-env form)
;  (eval form user-initial-environment))

;(define list-ref  nth)
;(define list-tail nthcdr)
;(define last-pair lastcdr)
;(define mapcar    map)
;(define for-each  walk)

;(define read-from-keyboard read)
;(define eof-object? eof?)
;(define (open-input-file filename) (open filename '(in)))
;(define (close-input-port port) (close port))
