(define (sumsq w h)
  (define (sq x) (* x x))
  (+ (sq w) (sq h)))
(sumsq 3 4)