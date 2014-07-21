;;; -*- Mode:Scheme; Base:10 -*- PS9-MAGIC.SCM

;;		     MASSACHUSETTS INSTITUTE OF TECHNOLOGY
;;	   Department of Electrical Engineering and Computer Science
;;	   6.001---Structure and Interpretation of Computer Programs
;;			     Fall Semester, 1991
;;
;;				 Problem Set 9

;;; Magic to make this code work on both Chipmunks and Athena.

;;; Syntactic sugar for DEFINE-MACHINE
;;; Ask Jinx@ai.mit.edu if you really need to know what this does.
;;; DO NOT expect to understand this. 

(let ((st (if (lexical-unreferenceable? (the-environment) 'student-package)
	      system-global-syntax-table
	      (access *student-syntax-table* student-package))))
  ;; (DEFINE-MACHINE name (REGISTERS r...) (CONTROLLER c...))
  ;; -->
  ;; (DEFINE name (BUILD-MODEL '(r...) '(c...)))
  (syntax-table-define
   st
   'DEFINE-MACHINE
   (macro (name registers controller)
     `(DEFINE ,name
	(BUILD-MODEL ',(cdr registers) ',(cdr controller))))))


;;; More magic code... imports definitions into student namespace

(define unsyntax unsyntax)

(define string? string?)

(define call-with-current-continuation call-with-current-continuation)
