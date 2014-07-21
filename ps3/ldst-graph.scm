;;; Fall 91
;;; This is a patch file for hacking graphics pictures.	      Ziggy@lcs.mit.edu
;;;
;;; The student manual advertises a procedure LOAD/STORE-GRAPHICS which are
;;;  actually installed under the names LOAD/STORE-PICTURE.

(define  load-graphics  load-picture)
(define store-graphics store-picture)
