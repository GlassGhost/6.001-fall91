;;; This is the code for Problem Set 7  ``The Amazing Owl'' -- Fall 1991

;;; Utility procedures for manipulating streams

(define (square x) (* x x))			; Sec.1.1.4 of SICP

(define (map proc s)				; Sec.3.4.2
  (if (empty-stream? s)
      the-empty-stream
      (cons-stream (proc (head s))
		   (map proc (tail s)))))

(define (filter pred s)				; Sec.3.4.2
  (cond ((empty-stream? s)
	 the-empty-stream)
	((pred (head s))
	 (cons-stream (head s) (filter pred (tail s))))
	(else (filter pred (tail s)))))

(define (add-streams s1 s2)			; Sec.3.4.4
  (cond ((empty-stream? s1) s2)
	((empty-stream? s2) s1)
	(else
	 (cons-stream (+ (head s1) (head s2))
		      (add-streams (tail s1) (tail s2))))))

(define (scale-stream constant s)		; Sec.3.4.4
  (map (lambda (x) (* x constant)) s))

(define (nth-tail n st)
  (if (= (floor n) 0)			; ROUND would be nicer but it has a bug
      st				; in Chipmunk Scheme and on Athena
      (nth-tail (-1+ n) (tail st))))

(define print-stream
  (let ()
    (define (loop rest)
      (cond ((empty-stream? rest)
	     (princ ")"))
	    (else
	     (princ (head rest))
	     (princ " ")
	     (loop (tail rest)))))
    (lambda (s)
      (newline)
      (princ "([STREAM] ")
      (loop s))))

(define (plot-stream s max-y num-vals)		; Useful for debugging
  (define (sign x) (if (< x 0) -1 1))
  (define hp-screen-width 200)
  (define hp-screen-height 180)
  (define x-scale (* 2 (/ hp-screen-width num-vals)))
  (define y-scale (/ hp-screen-height max-y))
  (define (screen-x-point x)
    (round (- (* x x-scale)
	      hp-screen-width)))
  (define (screen-y-point y)
    (let ((intended-y (round (* y-scale y))))
      (if (> (abs  intended-y) hp-screen-height)
	  (* (sign intended-y) hp-screen-height)
	  intended-y)))
  (define (iter s count)
    (cond ((> count num-vals)
	   'done)
	  (else (draw-line-to (screen-x-point count)
			      (screen-y-point (head s)))
		(iter (tail s) (1+ count)))))
  (position-pen (screen-x-point 0)
		(screen-y-point (head s)))
  (iter (tail s) 1))


;;; Angles in radians

(define pi (* 4 (atan 1 1)))

(define (degrees->radians deg)
  (/ (* pi deg) 180))


;;; Distance and Angle trigonometry

(define (compute-distance-and-angle time-shift lsig rsig v b)
   (let ((theta (atan (- (* v time-shift))
                      (sqrt (- (square (* 2 b))
                               (square (* v time-shift))))))
         (dist (* (/ (* v time-shift) 2)
		  (/ (- lsig rsig)
		     (- (* 2 (sqrt (* lsig rsig)))
			(+ lsig rsig))))))
    (list dist theta)))


;;; Global MEAN and DEVIATION variables

(define mean 0) ;the derivative of a bounded signal has zero average value
(define dev 20) ;who knows what evil lurks....

;;; Global signal variables

(define signal-velocity  1)
(define baseline        10)

;;; Procedures for generating test signals

(define (make-counter)				; Useful clock...
  (let ((x 0))
    (lambda ()
      (set! x (1+ x))         
      x)))

(define (compute-signal-factors d theta v b signal-strength onset)
  (let ((dist-left  (sqrt (+ (square d) (square b) (* -2 d b (sin theta)))))
	(dist-right (sqrt (+ (square d) (square b) (*  2 d b (sin theta)))))
	(lazy-onset (+ (random 10) onset)))
    (newline)
    (princ "distances are = ")
    (princ dist-left)
    (princ " ")
    (princ dist-right)
    (let ((time-left  (+ lazy-onset (round (/ dist-left  v))))
	  (time-right (+ lazy-onset (round (/ dist-right v)))))
      (newline)
      (princ "time onsets are = ")
      (princ time-left)
      (princ " ")
      (princ time-right)
      (let ((strength-left  (/ signal-strength (square dist-left)))
	    (strength-right (/ signal-strength (square dist-right))))
	(newline)
	(princ "signals are = ")
	(princ strength-left)
	(princ " ")
	(princ strength-right)
	(make-signal dist-left  time-left  strength-left
		     dist-right time-right strength-right)))))

(define (make-signal left-dist  left-time  left-strength
		    right-dist right-time right-strength)
  (list (list  left-dist  left-time  left-strength)
	(list right-dist right-time right-strength)))

(define signal-left-datum   first)
(define signal-right-datum second)

(define datum-dist   first)
(define datum-time   second)
(define datum-signal third)


;;; Single event signal...

(define (make-audio-signal d theta v b noise-bound signal-strength)
  ;; generate two signals, with noise, based on a distance d away, 
  ;; at angle theta, with baseline b
  (newline)
  (princ "Making a signal...")
  (let ((signal (compute-signal-factors d theta v b signal-strength 10)))
    (let (( left-datum (signal-left-datum  signal))
	  (right-datum (signal-right-datum signal)))
      (define (gen-stream counter datum)
	(cons-stream (if (> (counter) (datum-time datum))
			 (+ (random noise-bound) (datum-signal datum))
			 (random noise-bound))
		     (gen-stream counter datum)))
      (let (( left-counter (make-counter))
	    (right-counter (make-counter)))
	(cons (gen-stream  left-counter  left-datum)
	      (gen-stream right-counter right-datum))))))

;; procedure for making a single source test signal

(define (make-test-signal noise)
  (make-audio-signal 70 (degrees->radians 45)
		     signal-velocity baseline noise 20000000))

;;; Multiple event signals...

(define signal-duration 3)

(define (make-audio-signal-multiple d-theta-onset-list v b
				    noise-bound signal-strength)
  ;; generate two signals, with noise, based on a distance d away, 
  ;; at angle theta, with baseline b
  (newline)
  (princ "Making a multi-signal...")
  (let ((data (mapcar (lambda (dto) (compute-signal-factors (dto-d     dto)
							    (dto-theta dto)
							    v b signal-strength
							    (dto-onset dto)))
		      d-theta-onset-list)))
    (let (( left-data (mapcar signal-left-datum  data))
	  (right-data (mapcar signal-right-datum data)))
      (define (gen-stream counter data-list)
	(cons-stream (let ((signals
			     (filter-list
			       (lambda (x) (not (eq? x 'quiet)))
			       (mapcar (let ((time (counter)))
					 (lambda (datum)
					   (if (and (>= time (datum-time datum))
						    (<  time
							(+ (datum-time datum)
							   signal-duration)))
					       (datum-signal datum)
					       'quiet)))
				       data-list))))
		       (if (not (null? signals))
			   (+ (random noise-bound) (first signals))
			   (random noise-bound)))
                     (gen-stream counter data-list)))
      (let (( left-counter (make-counter))
	    (right-counter (make-counter)))
	(cons (gen-stream  left-counter  left-data)
	      (gen-stream right-counter right-data))))))


(define (filter-list pred lst)
   (cond ((null? lst)
          '())
         ((pred (car lst))
          (cons (car lst) (filter-list pred (cdr lst))))
         (else (filter-list pred (cdr lst)))))


;; procedure for making a multiple source test signal

(define (make-test-signal-multi noise)
   (make-audio-signal-multiple d-theta-onset-values
			       signal-velocity baseline noise 20000000))


(define make-dto  list)
(define dto-d     first)
(define dto-theta second)
(define dto-onset third)

(define d-theta-onset-values
  (let ((angle (degrees->radians 45)))
    (list (make-dto 70 angle 10)
	  (make-dto 50 angle 90)
	  (make-dto 20 angle 180))))



