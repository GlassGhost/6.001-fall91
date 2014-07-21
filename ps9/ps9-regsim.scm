;;; -*- Mode:Scheme; Base:10 -*- PS9-REGSIM.SCM

;;		     MASSACHUSETTS INSTITUTE OF TECHNOLOGY
;;	   Department of Electrical Engineering and Computer Science
;;	   6.001---Structure and Interpretation of Computer Programs
;;			     Fall Semester, 1991
;;
;;				 Problem Set 9

;;;;  6.001 Register Machine Simulator
;;;;  Changes from the version in the book are indicated by ";;; ***"

;;;;  This code is provided for your information only.
;;;;  You should not need to modify it.

(define (build-model registers controller)
  (let ((machine (make-new-machine)))
    (set-up-registers machine registers)
    (set-up-controller machine controller)
    machine))

(define (set-up-registers machine registers)
  (for-each (lambda (register-name)
	      (make-machine-register machine register-name))
	    registers))

(define (for-each proc l)
  (cond ((null? l) 'done)
	(else (proc (car l))
	      (for-each proc (cdr l)))))

(define (set-up-controller machine controller)
  (build-instruction-list machine (cons '*start* controller)))

(define (build-instruction-list machine op-list)
  (if (null? op-list)
      '()
      (let ((rest-of-instructions
             (build-instruction-list machine (cdr op-list))))
        (cond ((label? (car op-list))
	       (declare-label machine
			      (car op-list)
			      rest-of-instructions)
	       rest-of-instructions)
	      (else (cons (make-machine-instruction machine (car op-list))
			  rest-of-instructions))))))

(define (label? expression)
  (symbol? expression))

(define (make-machine-register machine name)
  (let ((defined-registers (remote-get machine '*registers*)))
    (cond ((memq name defined-registers)
	   (error "Multiply-defined register" name))
	  (else (remote-define machine name (make-register name))
		(remote-set machine
			    '*registers*
			    (cons name defined-registers))))))

;;; Register Model
;;; Modified by DPC to trace register assignments

(define (make-register name)
  (define contents nil)
  (define trace false)                                        ;;; ***
  (define (get) contents)
  (define (set value)
    (if trace						      ;;; ***
	(pp (list 'reg-trace '= name 'old-value '= contents   ;;; ***
				     'new-value '= value))    ;;; ***
	'not-traced)
    (set! contents value)
    value)
  (define (dispatch message)
    (cond ((eq? message 'get) (get))	  
          ((eq? message 'set) set)
	  ((eq? message 'trace-on)  (set! trace true )  true) ;;; *** 
	  ((eq? message 'trace-off) (set! trace false) false) ;;; ***
          (else
	   (error "Unknown request -- REGISTER" name message))))
  dispatch)

(define (get-contents register)
  (register 'get))

(define (set-contents register value)
  ((register 'set) value))

(define (declare-label machine label labeled-entry)
  (let ((defined-labels (remote-get machine '*labels*)))
    (cond ((memq label defined-labels)
	   (error "Multiply-defined label" label))
	  (else
	   (remote-define machine label labeled-entry)
	   (remote-set machine
		       '*labels*
		       (cons label defined-labels))))))


;;; Stack Model -- monitored stack
;;; Modified by DPC as marked to show stack dynamics graphically

(define *regsim-stack-graphics?* true)

(define (disable-regsim-stack-graphics!)
  (set! *regsim-stack-graphics?* false) 'regsim-stack-graphics-disabled)
(define (enable-regsim-stack-graphics!)
  (set! *regsim-stack-graphics?*  true) 'regsim-stack-graphics-enabled )

(define (make-stack)
  (define s '())
  (define number-pushes 0)
  (define max-depth 0)
  (define stack-ops 0)                                  ;;; ***
  (define (push x)
    (set! s (cons x s))
    (set! number-pushes (1+ number-pushes))
    (set! max-depth (max (length s) max-depth))
    (set! stack-ops (1+ stack-ops))                     ;;; ***
    (show-stack))                                       ;;; ***
  (define (pop)
    (if (null? s)
        (error "Empty stack -- POP")
        (let ((top (car s)))
	  (set! s (cdr s))
	  (set! stack-ops (1+ stack-ops))               ;;; ***
	  (show-stack)                                  ;;; ***
	  top)))    
  (define (show-stack)                                  ;;; ***
    (if *regsim-stack-graphics?*                        ;;; ***
	(draw-point (screen-map stack-ops)		;;; ***
		    (screen-map (* 3 (length s))))      ;;; ***
	'disabled)                                      ;;; ***
    'done)                                              ;;; ***
  (define (screen-map x) (- (remainder x 320) 160))     ;;; ***
  (define (initialize)
    (set! stack-ops 0)                                  ;;; ***
    (set! s '())
    (set! number-pushes 0)
    (set! max-depth 0))
  (define (print-statistics)
    (print (list 'total-pushes  '= number-pushes
                 'maximum-depth '= max-depth)))
  (define (dispatch message)
    (cond ((eq? message 'push) push)
          ((eq? message 'pop) (pop))
          ((eq? message 'initialize) (initialize) 'done)
          ((eq? message 'print-statistics) (print-statistics) 'done)
          (else (error "Unknown request -- STACK" message))))
  dispatch)

(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;;; Name-Value association

(define (remote-get machine variable)
  (eval variable machine))

(define (remote-set machine variable value)
  (eval (list 'set! variable (list 'quote value))
        machine))

(define (remote-define machine variable value)
  (eval (list 'define variable (list 'quote value))
        machine))

(define (make-machine-instruction machine op)
  (eval (list 'lambda '() (verify-machine-instruction op machine)) machine))

;;; Monitored stack machine maker.
;;; Changed by DPC as marked to count operations and enable instruction
;;; traces.

(define (make-new-machine)
  (make-environment
   (define machine-ops 0)                                ;;; ***
   (define trace-flag false)
   (define *labels* '())
   (define *registers* '())                              ;;; Paranoia
   (define *the-stack* (make-stack))
   (define (initialize-ops-counter)                      ;;; ***
     (print (list 'machine-ops '= machine-ops))          ;;; ***     
     (set! machine-ops 0)                                ;;; ***
     'ops-counter-initialized)                           ;;; ***
   (define (initialize-stack)
     (*the-stack* 'print-statistics)
     (*the-stack* 'initialize)
     'stack-initialized)
   (define fetch get-contents)
   (define *program-counter* '())
   (define (execute code-sequence)
     (set! *program-counter* code-sequence)
     (cond ((null? *program-counter*)
	    'done)
	   (else (set! machine-ops (1+ machine-ops))     ;;; ***
		 (if trace-flag                          ;;; Print out code
		     (pp (caddr	                         ;;; 'as-code
			   (unsyntax                     ;;; ***
			     (car *program-counter*))))  ;;; ***
		     'not-traced)                        ;;; ***
		 ((car *program-counter*)))))            ;;; ***
   (define (normal-next-instruction)
     (execute (cdr *program-counter*)))

   (define (assign register value)
     (set-contents register value)
     (normal-next-instruction))
   
   (define (save reg)
     (push *the-stack* (get-contents reg))
     (normal-next-instruction))

   (define (restore reg)
     (set-contents reg (pop *the-stack*))
     (normal-next-instruction))

   (define (goto new-sequence)
     (execute new-sequence))

   (define (branch predicate alternate-next)
     (if predicate
	 (goto alternate-next)
	 (normal-next-instruction)))

   (define (perform operation)
     (normal-next-instruction))

   ;; end of make-new-machine
   ))



;;; Rest of simulator interface

(define (remote-fetch machine register-name)
  (get-contents (remote-get machine register-name)))

(define (remote-assign machine register-name value)
  (set-contents (remote-get machine register-name) value)
  'done)

(define (start machine)
  (eval '(goto *start*) machine))

;;; Added by DPC to trace instructions and register assignments

(define (remote-trace-on machine)                      ;;; ***
  (eval '(set! trace-flag true ) machine))             ;;; ***

(define (remote-trace-off machine)                     ;;; ***
  (eval '(set! trace-flag false) machine))             ;;; ***
  
(define (remote-trace-reg-on machine reg-name)         ;;; ***
  ((remote-get machine reg-name) 'trace-on))           ;;; ***

(define (remote-trace-reg-off machine reg-name)        ;;; ***
  ((remote-get machine reg-name) 'trace-off))          ;;; ***

(define (initialize-stack machine)                     ;;; ***
  (eval '(initialize-stack) machine))                  ;;; ***

(define (initialize-ops-counter machine)               ;;; ***
  (eval '(initialize-ops-counter) machine))            ;;; ***

;;;
;;; Code commissioned by the MIT Chapter of the ACM Society for
;;;  Proper Register Machine Instructions.   --Ben Bitdiddle
;;;

(define (verify-machine-instruction mach-instr machine)
  (cond ((valid-machine-instruction? mach-instr machine)
	 mach-instr)
	(else
	 (newline)(newline)
	 (princ *invalid-machine-instruction-grammar-error-message*)
	 (princ mach-instr)
	 (error "Invalid Machine Instruction [See *scheme* buffer for details]"
		mach-instr))))

(define *invalid-machine-instruction-grammar-error-message*
  "-----------------Invalid machine instruction encountered-----------------

Machine instructions must match one of the following patterns:

     (ASSIGN <register-name> <register-source>)
     (PERFORM (<function> <arg>...))
     (BRANCH (<test-function> <arg>...) <label>)
     (GOTO        <label> )
     (GOTO (FETCH <register-name>))
     (SAVE    <register-name>)
     (RESTORE <register-name>)
where
     <register-source> can be a CONSTANT, (FETCH <register-name>), or
                                (<function> <arg>...)
                 <arg> can be a CONSTANT or (FETCH <register-name>)

INVALID MACHINE INSTRUCTION:
    ")

(define (valid-machine-instruction? mach-instr machine)
  (cond ((assign?  mach-instr) (valid-assign?  mach-instr machine))
	((perform? mach-instr) (valid-perform? mach-instr machine))
	((branch?  mach-instr) (valid-branch?  mach-instr machine))
	((goto?    mach-instr) (valid-goto?    mach-instr machine))
	((save?    mach-instr) (valid-save?    mach-instr machine))
	((restore? mach-instr) (valid-restore? mach-instr machine))
	(else false)))

(define (assign?  mach-instr) (form-with-tag? 'assign  mach-instr))
(define (perform? mach-instr) (form-with-tag? 'perform mach-instr))
(define (branch?  mach-instr) (form-with-tag? 'branch  mach-instr))
(define (goto?    mach-instr) (form-with-tag? 'goto    mach-instr))
(define (save?    mach-instr) (form-with-tag? 'save    mach-instr))
(define (restore? mach-instr) (form-with-tag? 'restore mach-instr))

(define (valid-assign? mach-instr machine) ; Avoid CONJUNCTION/DISJUNCTION
  (if (= (length mach-instr) 3)		   ; since not meta-circular w/ ECEval
      (if (register-name?   (cadr  mach-instr) machine)
	  (register-source? (caddr mach-instr) machine)
	  false)
      false))

(define (valid-perform? mach-instr machine)
  (if (= (length mach-instr) 2)
      (reg-funcall? (cadr mach-instr) machine)
      false))

(define (valid-branch? mach-instr machine)
  (if (= (length mach-instr) 3)
      (if (reg-funcall? (cadr mach-instr) machine)
	  (label? (caddr mach-instr))
	  false)
      false))

(define (valid-goto? mach-instr machine)
  (if (= (length mach-instr) 2)
      (if (label? (cadr mach-instr))
	  true
	  (fetch-register-name? (cadr mach-instr) machine))
      false))

(define (valid-save?    mach-instr machine)
  (valid-save/restore?  mach-instr machine))
(define (valid-restore? mach-instr machine)
  (valid-save/restore?  mach-instr machine))

(define (valid-save/restore? mach-instr machine)
  (if (= (length mach-instr) 2)
      (register-name? (cadr mach-instr) machine)
      false))



(define (register-name? x machine)
  (memq x (remote-get machine '*registers*)))

(define (register-source? x machine)
  (cond ((constant? x) true)
	((fetch-register-name? x machine) true)
	((reg-funcall? x machine) true)
	(else false)))

(define (constant? x)
  (or (self-evaluating? x) (quoted? x) (label? x)))

(define (reg-funcall? x machine)
  (if (pair? x)
      (if (reg-function? (car x) machine)
	  (reg-args?     (cdr x) machine)
	  false)
      false))

(define (reg-function? x machine) (symbol? x))		; Wimpy. Want BOUND?.

(define (reg-arg? x machine)
  (cond ((constant? x) true)
	((fetch-register-name? x machine) true)
	(else false)))

(define (reg-args? x machine)
  (cond ((null? x) true)
	((reg-arg?  (car x) machine)
	 (reg-args? (cdr x) machine))
	(else false)))

(define (fetch-register-name? x machine)
  (if (fetch? x)
      (register-name? (cadr x) machine)
      false))

(define (fetch? x)
  (if (form-with-tag? 'fetch x)
      (= (length x) 2)
      false))
