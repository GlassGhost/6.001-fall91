;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  6.001 Fall 1991 -- Problem Set 9 -- Answer File -- PS9-ANSWER.SCM ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; The code for Problem Set 9 is in the following files (in alphabetic order)
;;;
;;; - PS9-LOAD.SCM   -- Load the whole deal in just the right order
;;; - PS9-MAGIC.SCM  -- Magic DEFINE-MACHINE macro and imported incantations.
;;;
;;; - PS9-COMP.SCM   -- The compiler
;;; - PS9-OPEN.SCM   -- Compiler modification for open-coding primitives
;;; - PS9-ECEVAL.SCM -- The explicit control evaluator
;;; - PS9-REGSIM.SCM -- The register machine simulator
;;; - PS9-SYNTAX.SCM -- Ducky data abstractions for Scheme syntax
;;;

(define (balanced-binary-tree depth)
  (if (<= depth 1)
      (cons 'a 'a)
      (let ((branch (balanced-binary-tree (- depth 1))))
	(cons branch branch))))

;;;
;;; Your answers stuff starts here...
;;;

;;;; Compiler modifications for open-coded primitives with two operands

;;; For two operands we compile code that evaluates the first operand and
;;; places the result in ARG1, and code that evaluates the second operand
;;; and places the result in ARG2.  We then paste together both instruction
;;; sequences, along with the instruction:
;;;      (ASSIGN <target> (<op> (FETCH ARG1) (FETCH ARG2)))

;;; We'll leave this procedure for you to complete.  The tricky part is
;;; to realize that certain registers have to be preserved when the
;;; instruction sequences are glued together.  You should use the
;;; the PRESERVING operation (see text, page 447--448) to accomplish this.

(define (open-code-binary-primitive-application app c-t-env target cont)
  (error "You haven't defined OPEN-BINARY-PRIMITIVE-APPLICATION yet" app)
  )

