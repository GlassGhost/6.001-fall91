;;;This is the code for the advisor problem set

;;;Top-level procedure
(define (see-advisor name)
  (print (list 'hi name))
  (print '(i am your freshman advisor))
  (print '(what are your plans for the semester))
  (advisor-driver-loop name))

;;;Driver loop
(define (advisor-driver-loop name)
  (newline)
  (princ '**)
  (let ((user-response (read-from-keyboard)))
    (cond ((equal? user-response '(goodbye))
	   (print (list 'goodbye name))
	   (print '(have a good semester!)))
	  (else (print (reply-to user-response))
		(advisor-driver-loop name)))))

;;;Select method for generating the reply
(define (reply-to input)
  (cond ((generate-match-response conventional-wisdom input))
        ((with-odds 1 2) (reflect-input input))
        (else (pick-random general-advice))))

;;;!!!!!You will need to rewrite this next procedure so that it calls
;;;the matcher.

(define (generate-match-response entries input) false)

(define (match-to-table-entry entry input)
  (let ((match-result (match (entry-pattern entry) input)))
    (if (good-match? match-result)
	(instantiate (entry-skeleton entry) match-result)
	'failed)))

;;;Repeat the user's reponse, after changing first-person words
;;;to second person
(define (reflect-input input)
  (append (pick-random beginnings) (change-person input)))

(define (change-person phrase)
  (sublist '((i you) (me you) (am are) (my your))
	   phrase))

 ;;;Tables are represented as lists of entries
(define (make-entry pattern skeleton)
  (list pattern skeleton))

(define (entry-pattern  x) (car  x))
(define (entry-skeleton x) (cadr x))

;;;The advisor uses the following table

(define conventional-wisdom
  (list
   (make-entry
    '(* 6:001 *)
    '(6:001 is too much work for freshmen -- wait until next year))
   (make-entry
    '(* 8:01 *)
    '(students really enjoy 8:01))
   (make-entry
    '(* seminar *)
    '(i hear that snorkeling in Boston Harbor is a really exciting seminar))
   (make-entry
    '(* to take * next *)
    '(too bad -- (: 1) is not offered next (: 2)))
   (make-entry
    '(* double major in * and *)
    '((: 1) is fascinating and you can make a living doing it if (: 2) does not work out))
   (make-entry
    '(* double major *)
    '(doing a double major is a lot of work))
   ))

(define general-advice
  '((make sure to take some humanities)
    (have you considered a context subject)
    (mit has a lot of interesting departments)
    (make sure to get time to explore the Boston area)
    (how about a freshman seminar)))

;;;tack on a random one of these when reflecting the user's input
(define beginnings
  '((you say)
    (why do you say)
    (i am glad to hear that)
    ()))

;;;Utility procedures

(define (pick-random list)
  (list-ref list (random (length list))))

(define (substitute replacements item)
  (cond ((null? replacements) item)
	((eq? item (caar replacements)) (cadar replacements))
	(else (substitute (cdr replacements) item))))


(define (sublist replacements list)
  (mapcar (lambda (elt) (substitute replacements elt)) list))

(define (with-odds n1 n2) (< (random n2) n1))

;;;pattern matcher

(define (match pat exp)
  (cond ((and (null? pat) (null? exp)) '())
	((null? pat) 'failed)
	((start-arbitrary-segment? pat) (match-arbitrary-segment pat exp))
	((null? exp) 'failed)
	((equal? (car pat) (car exp)) (match (cdr pat) (cdr exp)))
	(else 'failed)))


(define (match-arbitrary-segment pat exp)
  (if (null? exp)
      (match-rest-pat pat exp)
      (let ((first-try (match-rest-rest pat exp)))
	(if (good-match? first-try)
	    first-try
	    (let ((second-try (match-rest-exp pat exp)))
	      (if (good-match? second-try)
		  second-try
		  (match-rest-pat pat exp)))))))

(define (match-rest-pat pat exp)
  (let ((result (match (cdr pat) exp)))
    (if (good-match? result)
	(extend-dictionary '() result)
	'failed)))

(define (match-rest-exp pat exp)
  (let ((result (match pat (cdr exp))))
    (if (good-match? result)
	(extend-first-entry (car exp) result)
	'failed)))

(define (match-rest-rest pat exp)
  (let ((result (match (cdr pat) (cdr exp))))
    (if (good-match? result)
	(extend-dictionary (list (car exp)) result)
	'failed)))

(define (good-match? m) (not (eq? m 'failed)))

(define (start-arbitrary-segment? pat) (eq? (car pat) '*))

(define (extend-dictionary new-entry matches)
  (cons  new-entry  matches))

(define (extend-first-entry extension matches)
  (cons (cons extension (car matches)) (cdr matches)))

