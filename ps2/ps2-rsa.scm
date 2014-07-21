;;; This is the code for problem set 2

;;; This is the answer to Ex. 1.11 for you to work on Ex. 1.13

(define (square x) (* x x))			; Generally useful

(define (new-exp b n)
  (define (iter-exp multiplier product exponent)
    (cond ((zero? exponent) product)
          ((even? exponent)
           (iter-exp (square multiplier) product (/ exponent 2)))
          (else (iter-exp multiplier
                          (* product multiplier)
                          (- exponent 1)))))
  (iter-exp b 1 n))

;;; Procedures needed to implement fermat's test of primality

(define big-random
  (lambda (n)
    (random (min n (expt 10 10)))))

(define fermat-test
  (lambda (n)
    (let ((a (big-random n)))
      (= (expmod-3 a n n) a))))

(define fast-prime?
  (lambda (n times)
    (cond ((zero? times) true)
	  ((fermat-test n)
	   (fast-prime? n (-1+ times)))
	  (else false))))

(define timed-exp
  (lambda (f b e m)
    (let ((start-time (runtime)))
      (f b e m)
      (print (- (runtime) start-time)))))

;;; Louis Reasoner's 3 versions of expmod

(define expmod-1
  (lambda (b e m)
    (if (zero? e)
	1
	(remainder (* b (expmod-1 b (-1+ e) m))
		   m))))

(define expmod-2
  (lambda (b e m)
    (define exp-helper
      (lambda (times)
	(cond ((zero? times) 1)
	      ((even? times)
	       (square (exp-helper (/ times 2))))
	      (else (* b (exp-helper (-1+ times)))))))
    (remainder (exp-helper e) m)))

(define expmod-3
  (lambda (b e m)
    (cond ((zero? e) 1)
	  ((even? e)
	   (remainder (square (expmod-3 b (/ e 2) m))
		      m))
	  (else
	   (remainder (* b (expmod-3 b (-1+ e) m))
		      m)))))

;;; CIA's key selection procedures

(define select-prime
  (lambda ()
    (let ((n (random 20)))			; Upper bound = 20
      (if (<= n 1)				; Lower bound =  1
	  (select-prime)
	  (let ((p (+ (square n) n 41)))
	    (if (fast-prime? p 2)
		p
		(select-prime)))))))

(define select-keys
  (lambda ()
    (newline)
    (princ "Please wait for your daily lottery numbers........")
    (let ((start-time (runtime)))
      (let ((p (select-prime))
	    (q (select-prime)))
	(if (not (= (gcd p q) 1))
	    (select-keys)
	    (sequence
	      (newline)
	      (princ "Your first  public key is.....")
	      (princ (* p q))
	      (newline)
	      (princ "Your second public key is.....")
	      (princ (select-e (* (- p 1) (- q 1))))
	      (newline)
	      (princ "Elapsed time is...............")
	      (princ (- (runtime) start-time))))))))

(define select-e
  (lambda (m)
    (let ((e (random 1000)))
      (if (= (gcd e m) 1)
	  e
	  (select-e m)))))

(define gcd
  (lambda (a b)
    (if (zero? b)
	a
	(gcd b (remainder a b)))))

;;; Encryption and Decryption procedures (really simple)

(define encrypt
  (lambda (message e n)
    (expmod-3 message e n)))

(define decrypt
  (lambda (message d n)
    (expmod-3 message d n)))

;;; Part of Artur O. Grossberg and Baruch Schtraktur's Code-cracker

(define improve (lambda (guess) (-1+ guess)))

;;; The rest is for you to complete.
