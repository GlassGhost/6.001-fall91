;;; -*- Scheme -*-  PS9-LOAD.SCM

;;		     MASSACHUSETTS INSTITUTE OF TECHNOLOGY
;;	   Department of Electrical Engineering and Computer Science
;;	   6.001---Structure and Interpretation of Computer Programs
;;			     Fall Semester, 1991
;;
;;				 Problem Set 9

;;;
;;; Stuff to load for PS9: the right stuff to load and the right way to load it
;;;

;; Get magic to work right

(enable-language-features)
(load "psets:ps9-magic.scm")
(disable-language-features)

;; Get the rest of the PS stuff

(load "psets:ps9-syntax")
(load "psets:ps9-regsim")
(load "psets:ps9-eceval")
(load "psets:ps9-comp")
(load "psets:ps9-open")
