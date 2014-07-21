;;; Magic... we need to import these procedures from the underlying MIT Scheme
;;;          into the Chipmunk student environment. This must be done after
;;;          having done ENABLE-LANGUAGE-FEATURES in a *separate* file then
;;;          loading this file. Consult an MIT Scheme guru for details.

(define eof-object? eof-object?)

(define  open-input-file  open-input-file)
(define close-input-port close-input-port)

(define string? string?)			; filenames are strings

; for the uber-hack of avoiding crapping out into the Scheme debugger
(define call-with-current-continuation call-with-current-continuation)
