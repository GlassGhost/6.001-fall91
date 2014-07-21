
;;; templates to be filled in

;; Problem 11 a.

(define (set-up-sums st n)
  (let ((start (first-n st n)))
    (let ((sum   (car start))
          (front (cdr start))
          (back st))
     (define running-sum "...?...")		; To be completed by you
     running-sum)))

;; Problem 11 b.

(define (set-up-squares st n)
  (set-up-sums "...?..." n))			; To be completed by you


;; Problem 14

(define (glued-data st n)
  (combine-streams "...?..."			; To be completed by you
		   (nth-tail (round (/ n 2)) st)
		   (combine-streams "...?..."	; To be completed by you
				    (mean-st st n)
				    ( dev-st st n))))
