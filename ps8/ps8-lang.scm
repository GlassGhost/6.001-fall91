;;; This is the file PS8-LANG.SCM   -- Fall 91

;;; Language generation and parsing

;;; First, we define some example words from a variety of interesting
;;; grammatical categories.

(define (noun)
  (amb 'student 'professor 'dog 'cat 'class))

(define (verb)
  (amb 'studies 'lectures 'eats 'sleeps))

(define (adj)
  (amb 'brilliant 'tired))

(define (adverb)
  (amb 'quickly 'delightedly 'slothfully))

(define (article)
  (amb 'the 'a))

(define (prep)
  (amb 'for 'to 'in 'by))


;;; We parse a sentence (list of words) by recursively looking for constituents
;;; of the current grammatical phrase, starting with sentence.  Alternate
;;; language structures are searched for using AMB.  Words are matched using
;;; CHECK-WORD, and the output is built up by BUILD.

;;; We assume that each parsing fragment task as input a partial sentence, and
;;; returns as output a combination of the parsed structure and the remaining
;;; portion of the sentence that is left to parse

(define make-partial-parse cons)

(define parsing       car)
(define rest-sentence cdr)


(define (parse s)
  (let ((n (parse-noun-phrase s)))
    (if n
	(let ((v (parse-verb-phrase (rest-sentence n))))
	  (if v
	      (if (null? (rest-sentence v))   ; nothing left to parse
		  (build 'sentence
			 (parsing n)
			 (parsing v))
		  (fail))
	      (fail)))
	(fail))))

;; Cannot write this because it is left-recursive.  I.e., to parse a
;; noun-phrase, we have to start by parsing a noun-phrase, and this leads to
;; infinite descent.  We use the usual parsing technique of look-ahead; see if
;; the prepositional phrase is there before we try to combine it.
;;-----------------------------------------------------------------------------
;;(define (parse-noun-phrase)
;;  (amb (parse-np3)
;;       (build 'noun-phrase (parse-noun-phrase) (parse-prepositional-phrase))))
;;------------------------------------------------------------------------------
;; Instead, we parse the np3 first, and then check to see if there is a
;; prepositional phrase to follow:

(define (parse-noun-phrase s)
  (define (add-pps np)
    (amb np
         (let ((pp (parse-prepositional-phrase (rest-sentence np))))
           (add-pps (make-partial-parse (build 'noun-phrase
					       (parsing np)
					       (parsing pp))
					(rest-sentence pp))))))
  (add-pps (parse-np3 s)))

(define (parse-np3 s)
  (let ((art (parse-article s)))
    (if art
	(let ((n (parse-np2 (rest-sentence art))))
	  (if n
	      (make-partial-parse 
	       (build 'np3 (parsing art) (parsing n))
	       (rest-sentence n))
	      (fail)))
	(fail))))
	  
(define (parse-np2 s)
  (amb (parse-np s)
       (let ((ad (parse-adjective s)))
	 (if ad
	     (let ((n (parse-np2 (rest-sentence ad))))
	       (if n
		   (make-partial-parse 
		    (build 'np2 (parsing ad) (parsing n))
		    (rest-sentence n))
		   (fail)))
	     (fail)))))


(define (parse-adjective s)
  (check-word 'adjective adj s))


(define (parse-np s)
  (amb (check-word 'noun noun s)
       (let ((n (check-word 'noun noun s)))
	 (if n
	     (let ((nn (parse-np (rest-sentence n))))
	       (if nn
		   (make-partial-parse 
		    (build 'np (parsing n) (parsing nn))
		    (rest-sentence nn))
		   (fail)))
	     (fail)))))


(define (parse-article s)
  (check-word 'article article s))


(define (parse-prepositional-phrase s)
  (let ((p (check-word 'prep prep s)))
    (if p
	(let ((n (parse-np3 (rest-sentence p))))
	  (if n
	      (make-partial-parse (build 'pp (parsing p) (parsing n))
				  (rest-sentence n))
	      (fail)))
	(fail))))

(define (parse-verb-phrase s)
  (define (add-pps vp)
    (amb vp
         (let ((pp (parse-prepositional-phrase (rest-sentence vp))))
           (add-pps (make-partial-parse (build 'verb-phrase
					       (parsing vp)
					       (parsing pp))
					(rest-sentence pp))))))
  (add-pps
   (amb (check-word 'verb verb s)
	(let ((v (check-word 'verb verb s)))
	  (if v
	      (let ((ad (check-word 'adverb adverb (rest-sentence v))))
		(if ad
		    (make-partial-parse (build 'vp
					       (parsing v)
					       (parsing ad))
					(rest-sentence ad))
		    (fail)))
	      (fail))))))



(define (check-word part-of-speech generator s)
  (cond ((null? s) (fail))
	((equal? (car s) (generator))
	 (make-partial-parse (build-prim part-of-speech (car s))
			     (rest-sentence s)))
	(else (fail))))

(define build      list)
(define build-prim list)
