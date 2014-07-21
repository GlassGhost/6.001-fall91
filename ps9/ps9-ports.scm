;;; -*- Mode:Scheme; Base:10 -*- PS9-PORTS.SCM

;;		     MASSACHUSETTS INSTITUTE OF TECHNOLOGY
;;	   Department of Electrical Engineering and Computer Science
;;	   6.001---Structure and Interpretation of Computer Programs
;;			     Fall Semester, 1991
;;
;;				 Problem Set 9

;;;
;;; Portability hacks for sundry Schemes
;;;

;; MacScheme
;;-----------
;;
;; You lose. Your Scheme does not have first class environments so you cannot
;; use our register machine simulator. If you want to hack the PS8 AMB-EVAL to
;; handle IF and to understand LAMBDA expressions with dotted parameter lists
;; [e.g., (lambda (x . args) ...)] then you could use it as a Scheme with first
;; class environments, so you could load all the ps9 stuff into it (using
;; USER-LOAD). This would consume a LOT of memory and would add yet one more
;; layer of interpretation to the system so unless you have a supercomputer
;; with a gigabyte of main memory, you should probably just give up and do
;; this problem set on a Chipmunk or on Athena's 6.001 Scheme.
;;
;; For the undaunted... send email to bitdiddl@athena.mit.edu. You cannot win.

;(define (make-environment . clauses)
;  (error "...You lose! MacScheme does no have first-class environments..."))

;(macro define-machine
;  (lambda (instance)   ; instance ::= (DEFINE-MACHINE name
;    (let ((name (cadr   instance)) ;     (REGISTERS r...)
;          (regs (caddr  instance)) ;     (CONTROLLER c...))
;	  (ctrl (cadddr instance)))
;      ;; (DEFINE-MACHINE name (REGISTERS r...) (CONTROLLER c...))
;      ;; -->
;      ;; (DEFINE name (BUILD-MODEL '(r...) '(c...)))
;     `(DEFINE ,name (BUILD-MODEL ',(cdr regs) ',(cdr ctrl))))))

;(define (eval-in-initial-env form)
;  (eval form))

;(define read-from-keyboard read)

;(define (unsyntax x)
;  (if (or (symbol? x) (number? x) (list? x)) `',x x))

;(define (clear-graphics) 'no-graphics)

;(disable-regsim-stack-graphics!)

;(define (make-new-symbol name) (gensym (symbol->string name)))


;; TI's PC Scheme
;;----------------
;;
;; Note first that this problem set has many layers of interpretation: Scheme
;; running regsim running Scheme code. Needless to say, you will need LOTS of
;; heap storage to run this. Probably the best thing for you to do is to give
;; up and do this problem set either on a Chipmunk or on Athena's 6.001 Scheme.
;;
;; For the undauted...

;(macro make-environment
;  (lambda (instance)   ; instance ::= (MAKE-ENVIRONMENT defn defn...)
;    (let ((defns (cdr instance)))
;      ;; (MAKE-ENVIRONMENT defn defn...)
;      ;; -->
;      ;; (LET () defn defn... (THE-ENVIRONMENT))
;     `(LET () ,@defns (THE-ENVIRONMENT)))))

;(macro define-machine
;  (lambda (instance)   ; instance ::= (DEFINE-MACHINE name
;    (let ((name (cadr   instance)) ;     (REGISTERS r...)
;          (regs (caddr  instance)) ;     (CONTROLLER c...))
;	  (ctrl (cadddr instance)))
;      ;; (DEFINE-MACHINE name (REGISTERS r...) (CONTROLLER c...))
;      ;; -->
;      ;; (DEFINE name (BUILD-MODEL '(r...) '(c...)))
;     `(DEFINE ,name (BUILD-MODEL ',(cdr regs) ',(cdr ctrl))))))

;(define (eval-in-initial-env form)
;  (eval form user-initial-environment))

;(define read-from-keyboard read)

;(define mapcar map)

;(define (unsyntax x)
;  (if (or (symbol? x) (number? x) (list? x)) `',x x))

;(define (clear-graphics) 'no-graphics)

;(disable-regsim-stack-graphics!)

;(define (make-new-symbol name) (gensym (symbol->string name)))




;; Yale's T Scheme
;;----------------

;(define-syntax (make-environment . defns)
; `(LET ((new-locale (MAKE-EMPTY-LOCALE (GENERATE-SYMBOL 'environment))))
;    (EVAL (LET () ,@defns) new-locale)
;    new-locale))

;(define-syntax (define-machine name regs ctrl)
;  ;; (DEFINE-MACHINE name (REGISTERS r...) (CONTROLLER c...))
;  ;; -->
;  ;; (DEFINE name (BUILD-MODEL '(r...) '(c...)))
; `(DEFINE ,name (BUILD-MODEL ',(cdr regs) ',(cdr ctrl))))

;(define (eval-in-initial-env form)
;  (eval form user-initial-environment))

;(define mapcar    map)
;(define for-each  walk)

;(define read-from-keyboard read)

;(define (unsyntax x)
;  (if (or (symbol? x) (number? x) (list? x)) `',x x))

;(define (clear-graphics) 'no-graphics)

;(disable-regsim-stack-graphics!)

;(define (make-new-symbol name) (generate-symbol name))

;; PseudoScheme
;;-------------

;; You lose. Your Scheme does not have first class environments so you cannot
;; use our register machine simulator. If you want to hack the PS8 AMB-EVAL to
;; handle IF and to understand LAMBDA expressions with dotted parameter lists
;; [e.g., (lambda (x . args) ...)] then you could use it as a Scheme with first
;; class environments, so you could load all the ps9 stuff into it (using
;; USER-LOAD). This would consume a LOT of memory and would add yet one more
;; layer of interpretation to the system so unless you have a supercomputer
;; with a gigabyte of main memory, you should probably just give up and do
;; this problem set on a Chipmunk or on Athena's 6.001 Scheme.
;;
;; For the undaunted... send email to bitdiddl@athena.mit.edu. You cannot win.

;(define (make-environment . clauses)
;  (error "...You lose! PseudoScheme does no have first-class environments..."))

;(define-macro (define-machine name registers controller)
;  `(DEFINE ,name (BUILD-MODEL ',(cdr registers) ',(cdr controller))))

;(define read-from-keyboard read)

;(define (make-new-symbol name) (gensym (symbol->string name)))

;(define (unsyntax x)
;  (if (or (symbol? x) (number? x) (list? x)) `',x x))

;(define (clear-graphics) 'no-graphics)

;(disable-regsim-stack-graphics!)

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
;	  (go)))
;  (setup-compile-and-go)
;  "...so MAGIC-RECONTINUE will just start a new problem each time it's called")
