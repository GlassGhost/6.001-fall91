
(define (p1 x y)
  (+ (p2 x y) (p3 x y)))

(define (p3 a b)
  (+ (p2 a) (p2 b)))

(define (p2 z w)
  (* z w))