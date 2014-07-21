;; This patch by-passes a bug in the compiled run-time definition of TAIL on
;; Chipmunks... <<Vive la type-checking!>> If anyone gets around to fixing the
;; runtime system to avoid using a compiled TAIL, or if someone fixes the
;; compiler to not punt type checking, then this patch will become obsolete.

;; NB: Must (ENABLE-LANGUAGE-FEATURES) before loading this file since it uses
;;     the procedure DELAYED?

(define delayed? delayed?)

(if (eq? (object-type tail) 'COMPILED-PROCEDURE) ; TAIL loses?
    (set! tail (let ((native-tail tail))
		 (define (stream? x)
		   (conjunction (pair?         x )
				(delayed? (cdr x))))
		 (lambda (stream)
		   (if (stream? stream)
		       (native-tail stream)
		       (error "Argument to TAIL was not a STREAM:" stream))))))

'FINI  ; Signal that file is done loading.

	    